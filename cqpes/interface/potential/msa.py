import glob
import os
from typing import List, Literal, Union

import numpy as np
import tensorflow as tf
from ase import Atoms
from ase.units import Hartree
from cqpes.interface.potential import CQPESBasePot
from cqpes.pipeline.prepare import v_calc_p
from cqpes.types.data import CQPESData
from cqpes.types.train import TrainConfig
from cqpes.utils.model import build_network
from cqpes.utils.msa import load_msa_so
from scipy.spatial.distance import pdist


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

        self._mount_msa()
        self._build_network()

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

    def _mount_msa(self) -> None:
        so_files = glob.glob(os.path.join(self.workdir, "*.so"))

        if not so_files:
            raise FileNotFoundError(
                f"[FATAL] No MSA engine (.so) found in {self.workdir}"
            )

        msa_path = so_files[0]
        self.msa = load_msa_so(msa_path)

    def _build_network(self) -> None:
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

    def get_energy(
        self,
        xyz: Union[np.ndarray, List[Atoms]],
        return_au: bool = False,
    ) -> np.ndarray:
        # (N_configs, N_atoms, 3)
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
        y_scaled = self.net.predict(
            x_scaled,
            batch_size=4096,
            verbose=0,
        )

        # 4. y in [-1, 1] -> V
        V_eV = CQPESData.unscale(y_scaled, self.V_min, self.V_max)

        results = V_eV.flatten()

        # 5. to Hartree
        if return_au:
            results = (results / Hartree) + self.ref_energy

        if results.size == 1 and not isinstance(xyz, list):
            return results.item()

        return results

    def get_forces_analytical(
        self,
        xyz: Union[np.ndarray, List[Atoms], Atoms],
    ) -> np.ndarray:
        # (N_configs, N_atoms, 3)
        xyz_arr = self._standardize_coordinates(xyz)
        n_configs = xyz_arr.shape[0]
        n_atoms = xyz_arr.shape[1]
        n_cart = 3 * n_atoms

        # 1. calculate p
        p_raw = v_calc_p(
            xyz_list=xyz_arr,
            alpha=self.alpha,
            basis=self.msa.basis,
        )

        p_feat = p_raw[:, 1:]

        # 2. prepare tensor and track gradients for the entire batch
        X_numpy = CQPESData.rescale(p_feat, self.p_min, self.p_max)
        X_tensor = tf.convert_to_tensor(X_numpy, dtype=tf.float64)

        with tf.GradientTape() as tape:
            tape.watch(X_tensor)
            y = self.net(X_tensor)

        grad_raw = tape.gradient(y, X_tensor)

        if grad_raw is None:
            raise RuntimeError(
                "[FATAL] Gradient disconnected. "
                "Ensure network topology is differentiable."
            )

        V_scale = (self.V_max - self.V_min) / 2.0
        p_scale = 2.0 / (self.p_max - self.p_min)

        # (N_configs, N_features)
        dV_dp_batch = tf.convert_to_tensor(grad_raw).numpy() * V_scale * p_scale

        results_forces = np.zeros((n_configs, n_atoms, 3), dtype=np.float64)

        for i in range(n_configs):
            pos = xyz_arr[i]
            dV_dp = dV_dp_batch[i]

            # 1. xyz -> dist vec
            r_vec = pdist(pos)
            n_bonds = len(r_vec)
            msa_drdx = np.zeros((n_cart, n_bonds), dtype=np.float64, order="F")

            k = 0

            for row in range(n_atoms - 1):
                for col in range(row + 1, n_atoms):
                    r = r_vec[k]
                    if r > 1e-12:
                        unit_vec = (pos[row] - pos[col]) / r
                        msa_drdx[3 * row : 3 * row + 3, k] = unit_vec
                        msa_drdx[3 * col : 3 * col + 3, k] = -unit_vec

                    k += 1

            # 2. dist vec -> morse -> mono -> poly
            x_morse = np.exp(-r_vec / self.alpha).ravel().astype(np.float64)
            msa_mono = self.msa.basis.evmono(x_morse)
            msa_poly = self.msa.basis.evpoly(msa_mono)

            # 3. dbemsav
            forces_flat = np.zeros(n_cart)

            for j in range(n_cart):
                # fortran 1-based indexing
                dp_dr_j = self.msa.gradient.dbemsav(
                    msa_drdx,
                    msa_mono,
                    msa_poly,
                    j + 1,
                )

                forces_flat[j] = -np.dot(dV_dp, dp_dr_j[1:])

            results_forces[i] = forces_flat.reshape(n_atoms, 3)

        if n_configs == 1 and not isinstance(xyz, list):
            return results_forces[0]

        return results_forces
