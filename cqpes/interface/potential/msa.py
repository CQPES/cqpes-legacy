import glob
import os
from typing import Literal

import numpy as np
import tensorflow as tf
from ase.units import Hartree
from cqpes.interface.potential import CQPESBasePot
from cqpes.pipeline.prepare import v_calc_p
from cqpes.types.data import CQPESData
from cqpes.types.train import TrainConfig
from cqpes.utils.model import build_network
from cqpes.utils.msa import load_msa_so


class CQPESMSAPot(CQPESBasePot):
    def __init__(
        self,
        workdir: str,
        force_mode: Literal["analytical", "numerical"] = "analytical",
    ) -> None:
        self.workdir = os.path.abspath(workdir)
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
        config_path = os.path.join(self.workdir, "train.json")

        if not os.path.exists(config_path):
            raise FileNotFoundError(
                f"[FATAL] train.json not found in {self.workdir}"
            )

        self.config = TrainConfig.from_json(config_path)

        try:
            self.p_min = np.load(os.path.join(self.workdir, "p_min.npy"))[1:]
            self.p_max = np.load(os.path.join(self.workdir, "p_max.npy"))[1:]

            self.V_min = np.load(os.path.join(self.workdir, "V_min.npy")).item()
            self.V_max = np.load(os.path.join(self.workdir, "V_max.npy")).item()

            self.alpha = np.load(os.path.join(self.workdir, "alpha.npy")).item()

            self.ref_energy = np.load(
                os.path.join(self.workdir, "ref_energy.npy")
            ).item()

        except Exception as e:
            raise RuntimeError(
                f"[ERROR] Failed to load physical artifacts: {e}"
            )

    def _mount_msa(
        self,
    ) -> None:
        so_files = glob.glob(os.path.join(self.workdir, "*.so"))

        if not so_files:
            raise FileNotFoundError(
                f"[FATAL] No MSA engine (.so) found in {self.workdir}"
            )

        msa_path = so_files[0]

        self.msa = load_msa_so(msa_path)

    def _build_network(
        self,
    ) -> None:
        export_model_path = os.path.join(self.workdir, "export", "model.h5")

        if not os.path.exists(export_model_path):
            raise FileNotFoundError(
                f"\n  [{'FATAL':^10}] Production model missing: "
                f"{os.path.basename(export_model_path)}\n"
                f"  [{'ACTION':^10}] Run: cqpes export -t h5 {self.workdir}"
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
            return results.item()

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

        grad_raw = self._tf_grad(
            X=tf.convert_to_tensor(X_scaled, dtype=tf.float64)
        ).numpy()

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
