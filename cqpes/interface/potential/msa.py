import glob
import os
from typing import Literal

import numpy as np
from ase.units import Hartree

from cqpes.interface.potential import CQPESBasePot
from cqpes.pipeline.prepare.msa import v_calc_p
from cqpes.types.data import CQPESData
from cqpes.types.train import TrainConfig
from cqpes.utils.msa import load_msa_so
from cqpes.utils.workspace import ExperimentWorkspace


class CQPESMSAPot(CQPESBasePot):
    def __init__(
        self,
        workdir: str,
        force_mode: Literal["analytical", "numerical"] = "analytical",
    ) -> None:
        # lazy import
        from cqpes._env import _setup_tensorflow

        _setup_tensorflow()

        # attributes
        self.workspace = ExperimentWorkspace.from_existing(workdir)
        self.force_mode = force_mode

        # load phys
        self._load_physics()

        # load descriptor
        self._mount_msa()

        # build network
        self._build_network()

        # cache
        self._num_atoms = None
        self._num_carts = None
        self._r_i = None
        self._r_j = None
        self._num_pairs = None

        self._drdx_buffer = None

        self._V_p_scale = (self.V_max - self.V_min) / (self.p_max - self.p_min)

    def _load_physics(
        self,
    ) -> None:
        import h5py

        config_path = os.path.join(self.workspace.path, "train.json")

        if not os.path.exists(config_path):
            raise FileNotFoundError(
                f"[FATAL] train.json not found in {self.workspace.path}"
            )

        self.config = TrainConfig.from_json(config_path)

        model_path = os.path.join(
            self.workspace.get_subpath("export"),
            "model.h5",
        )

        if not os.path.exists(model_path):
            raise FileNotFoundError(
                f"[FATAL] Integrated model.h5 missing: {model_path}"
            )

        try:
            with h5py.File(model_path, "r") as f:
                if "phys_metadata" not in f:
                    raise KeyError(
                        f"Legacy model detected! 'phys_metadata' not found in {model_path}. "
                        f"Please re-run 'cqpes export -t h5'."
                    )

                g = f["phys_metadata"]

                # --- A. 从 Attributes 读取标量 (Metadata) ---
                self.alpha = float(g.attrs["alpha"])  # type: ignore
                self.ref_energy = float(g.attrs["ref_energy"])  # type: ignore
                self.V_min = float(g.attrs["V_min"])  # type: ignore
                self.V_max = float(g.attrs["V_max"])  # type: ignore

                # 兼容性处理
                self.decay_kernel = g.attrs.get("decay_kernel", "morse")

                # --- B. 从 Dataset 读取数组 (Heavy Data) ---
                # 核心细节：我们在 export 时存的是 full vector (例如 83 维)
                # 这里读取时立刻切掉 index 0，对齐 NN 输入 (82 维)
                self.p_min = np.asarray(g["p_min"][1:])  # type: ignore
                self.p_max = np.asarray(g["p_max"][1:])  # type: ignore

        except Exception as e:
            raise RuntimeError(
                f"[ERROR] Failed to extract physical artifacts from HDF5: {e}"
            )

    def _mount_msa(
        self,
    ) -> None:
        so_files = glob.glob(os.path.join(self.workspace.path, "*.so"))

        if not so_files:
            raise FileNotFoundError(
                f"[FATAL] No MSA engine (.so) found in {self.workspace.path}"
            )

        msa_path = so_files[0]

        self.msa = load_msa_so(msa_path)

    def _build_network(
        self,
    ) -> None:
        import tensorflow as tf

        from cqpes.utils.model import build_network

        export_model_path = os.path.join(
            self.workspace.get_subpath("export"),
            "model.h5",
        )

        if not os.path.exists(export_model_path):
            raise FileNotFoundError(
                f"\n  [{'FATAL':^10}] Production model missing: "
                f"{os.path.basename(export_model_path)}\n"
                f"  [{'ACTION':^10}] Run: cqpes export -t h5 {self.workspace.path}"
            )

        input_dim = len(self.p_min)
        model = build_network(self.config, input_dim=input_dim)
        model.load_weights(export_model_path)

        self.net = model

        @tf.function(
            input_signature=[
                tf.TensorSpec(
                    shape=[None, input_dim],
                    dtype=tf.float64,
                )
            ],
            jit_compile=True,
        )
        def _get_grad(
            X: tf.Tensor,
        ) -> tf.Tensor:
            with tf.GradientTape() as tape:
                tape.watch(X)

                y = self.net(X, training=False)

            return tf.convert_to_tensor(
                tape.gradient(
                    y,
                    X,
                    unconnected_gradients=tf.UnconnectedGradients.ZERO,
                )
            )

        self._tf_grad = _get_grad

    def get_energy(
        self,
        xyz: np.ndarray,
        return_au: bool = False,
    ) -> np.ndarray:
        # (num_configs, num_atoms, 3)
        xyz_arr = self._standardize_coordinates(xyz)

        # 1. calculate p
        p_raw = v_calc_p(
            xyz_list=xyz_arr,
            alpha=self.alpha,
            basis=self.msa.basis,
        )

        p_feat = p_raw[:, 1:]

        # 2. p -> X in [-1, 1]
        x_scaled = CQPESData.rescale(p_feat, self.p_min, self.p_max)

        # 3. X -> y
        y_scaled = self.net(x_scaled, training=False).numpy()

        # 4. y in [-1, 1] -> V
        V_eV = CQPESData.unscale(y_scaled, self.V_min, self.V_max).flatten()

        # 5. to Hartree
        results = ((V_eV / Hartree) + self.ref_energy) if return_au else V_eV

        if results.size == 1 and not isinstance(xyz, list):
            return results.item()  # type: ignore

        return results

    def get_forces_analytical(
        self,
        xyz: np.ndarray,
    ) -> np.ndarray:
        # (num_configs, num_atoms, 3)
        xyz_arr = self._standardize_coordinates(xyz)
        num_configs = xyz_arr.shape[0]
        num_atoms = xyz_arr.shape[1]
        num_carts = 3 * num_atoms

        if self._num_atoms is None:
            self._num_atoms = num_atoms
            self._num_carts = num_carts

            self._r_i, self._r_j = np.triu_indices(self._num_atoms, k=1)
            self._num_pairs = len(self._r_i)

            self._drdx_buffer = np.zeros(
                (self._num_carts, self._num_pairs),
                dtype=np.float64,
                order="F",
            )

        assert (
            (self._num_atoms is not None)
            and (self._num_carts is not None)
            and (self._r_i is not None)
            and (self._r_j is not None)
            and (self._num_pairs is not None)
            and (self._drdx_buffer is not None)
        )

        # 1. calculate p & network input X
        p_raw = v_calc_p(
            xyz_list=xyz_arr,
            alpha=self.alpha,
            basis=self.msa.basis,
        )

        # p_feat = p_raw[:, 1:]
        X_scaled = CQPESData.rescale(p_raw[:, 1:], self.p_min, self.p_max)

        # network gradient
        grad_raw = self._tf_grad(X_scaled).numpy()  # type: ignore

        # scaling factor
        dV_dp_batch = grad_raw * self._V_p_scale

        results_forces = np.zeros((num_configs, num_atoms, 3), dtype=np.float64)

        for i in range(num_configs):
            pos = xyz_arr[i]
            dV_dp = dV_dp_batch[i]

            diff = pos[self._r_i] - pos[self._r_j]
            dist = np.linalg.norm(diff, axis=1)
            unit_vecs = diff / dist[:, np.newaxis]

            self._drdx_buffer.fill(0.0)

            for k in range(self._num_pairs):
                r_idx, c_idx = self._r_i[k], self._r_j[k]
                uv = unit_vecs[k]
                self._drdx_buffer[3 * r_idx : 3 * r_idx + 3, k] = uv
                self._drdx_buffer[3 * c_idx : 3 * c_idx + 3, k] = -uv

            x_morse = np.exp(-dist / self.alpha)
            msa_mono = self.msa.basis.evmono(x_morse)
            msa_poly = p_raw[i]

            forces_flat = np.zeros(self._num_carts)

            for j in range(self._num_carts):
                # Fortran 1-based indexing
                dp_dr_j = self.msa.gradient.dbemsav(
                    self._drdx_buffer,
                    msa_mono,
                    msa_poly,
                    j + 1,
                )

                forces_flat[j] = -np.dot(dV_dp, dp_dr_j[1:])

            results_forces[i] = forces_flat.reshape(self._num_atoms, 3)

        if num_configs == 1 and not isinstance(xyz, list):
            return results_forces[0]

        return results_forces
