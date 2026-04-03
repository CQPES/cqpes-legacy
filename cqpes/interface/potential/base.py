from abc import ABC, abstractmethod
from typing import Literal, Optional

import numpy as np
from ase.units import Bohr, Hartree

import cqpes  # # noqa: F401
from cqpes.utils.workspace import ExperimentWorkspace


class CQPESBasePot(ABC):
    def __init__(
        self,
        workdir: str,
        force_mode: Literal["analytical", "numerical"] = "analytical",
    ) -> None:
        self.workspace = ExperimentWorkspace.from_existing(workdir)
        self.force_mode = force_mode

    def _standardize_coordinates(
        self,
        xyz: np.ndarray,
    ) -> np.ndarray:
        arr = np.asarray(xyz, dtype=np.float64)

        if arr.ndim == 2:
            return arr[np.newaxis, :, :]
        elif arr.ndim == 3:
            return arr
        else:
            raise ValueError(
                f"[FATAL] Invalid coordinate shape {arr.shape}. "
                f"Expected (N, 3) or (M, N, 3)."
            )

    @abstractmethod
    def get_energy(
        self,
        xyz: np.ndarray,
        return_au: bool = False,
    ) -> np.ndarray:
        pass

    def get_forces(
        self,
        xyz: np.ndarray,
        return_au: bool = False,
        force_mode: Optional[str] = None,
        **kwargs,
    ) -> np.ndarray:
        target_mode = (force_mode or self.force_mode).lower()

        if target_mode == "analytical":
            forces = self.get_forces_analytical(xyz)
        else:
            forces = self.get_forces_numerical(xyz, **kwargs)

        if return_au:
            forces *= Bohr / Hartree

        return forces

    def get_forces_numerical(
        self,
        xyz: np.ndarray,
        delta: float = 0.01,
    ) -> np.ndarray:
        # (N_configs, N_atoms, 3)
        xyz_arr = self._standardize_coordinates(xyz)
        n_configs, n_atoms = xyz_arr.shape[0], xyz_arr.shape[1]

        n_displacements = 2 * 3 * n_atoms
        total_evals = n_configs * n_displacements
        displaced_batch = np.zeros((total_evals, n_atoms, 3), dtype=np.float64)

        idx = 0

        for c in range(n_configs):
            pos_ref = xyz_arr[c]
            for i in range(n_atoms):
                for j in range(3):
                    for d in [delta, -delta]:
                        displaced_batch[idx] = pos_ref.copy()
                        displaced_batch[idx, i, j] += d
                        idx += 1

        all_energies = self.get_energy(displaced_batch)

        # (N_configs, N_atoms, 3_cartesian, 2_directions)
        all_energies = all_energies.reshape(n_configs, n_atoms, 3, 2)

        # e_plus is index 0, e_minus is index 1 in the last dimension
        results_forces = -(all_energies[..., 0] - all_energies[..., 1]) / (
            2 * delta
        )

        if n_configs == 1 and not isinstance(xyz, list):
            return results_forces[0]

        return results_forces

    @abstractmethod
    def get_forces_analytical(
        self,
        xyz: np.ndarray,
    ) -> np.ndarray:
        pass

    def check_forces(
        self,
        xyz: np.ndarray,
        delta: float = 0.01,
        atol: float = 1.0e-05,
        rtol: float = 1.0e-03,
        verbose: bool = True,
    ) -> bool:
        f_ana = self.get_forces(xyz, force_mode="analytical")
        f_num = self.get_forces(xyz, force_mode="numerical", delta=delta)

        is_consistent = np.allclose(f_ana, f_num, atol=atol, rtol=rtol)

        if verbose:
            max_diff = np.max(np.abs(f_ana - f_num))
            status = "PASSED" if is_consistent else "FAILED"
            print(
                f"[CHECK_FORCES] Status: {status}\n"
                f"               MaxDiff: {max_diff:.4e}\n"
                f"               delta: {delta}"
            )

        return is_consistent
