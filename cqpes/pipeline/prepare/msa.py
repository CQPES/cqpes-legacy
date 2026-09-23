import os
import shutil
from types import ModuleType
from typing import List, cast

import numpy as np
from ase import Atoms
from ase.io import read
from ase.units import Hartree
from scipy.spatial import distance

from cqpes.types import CQPESData, PrepareConfig, PrepareSummary
from cqpes.utils.msa import load_msa_so

from . import hint_force_file


def v_calc_morse(
    xyz_list: np.ndarray,
    alpha: float,
) -> np.ndarray:
    r_list = np.array([distance.pdist(xyz) for xyz in xyz_list])

    return np.exp(-1.0 * r_list / alpha)


def v_calc_p(
    xyz_list: np.ndarray,
    alpha: float,
    basis: ModuleType,
) -> np.ndarray:
    morse_list = v_calc_morse(xyz_list, alpha)

    mono_list = np.apply_along_axis(
        func1d=basis.evmono,
        axis=1,
        arr=morse_list,
    )

    poly_list = np.apply_along_axis(
        func1d=basis.evpoly,
        axis=1,
        arr=mono_list,
    )

    return poly_list


def build_drdx(
    pos: np.ndarray,
    r_i: np.ndarray,
    r_j: np.ndarray,
) -> np.ndarray:
    """d(r_pair)/d(xyz) as a Fortran-ordered (3*Natoms, N_pairs) matrix."""
    n_atoms = pos.shape[0]
    n_carts = 3 * n_atoms
    n_pairs = len(r_i)

    diff = pos[r_i] - pos[r_j]
    dist = np.linalg.norm(diff, axis=1)
    unit_vecs = diff / dist[:, np.newaxis]

    drdx = np.zeros((n_carts, n_pairs), dtype=np.float64, order="F")

    for k in range(n_pairs):
        r_idx, c_idx = r_i[k], r_j[k]
        uv = unit_vecs[k]
        drdx[3 * r_idx : 3 * r_idx + 3, k] = uv
        drdx[3 * c_idx : 3 * c_idx + 3, k] = -uv

    return drdx


def v_calc_dp(
    xyz_list: np.ndarray,
    alpha: float,
    gradient: ModuleType,
    p_list: np.ndarray,
    mono_list: np.ndarray,
) -> np.ndarray:
    """d(p)/d(xyz) with shape (N, 3*Natoms, Npip), via MSA dbemsav."""
    n_configs, n_atoms, _ = xyz_list.shape
    n_carts = 3 * n_atoms

    r_i, r_j = np.triu_indices(n_atoms, k=1)

    dp_list = np.zeros(
        (n_configs, n_carts, p_list.shape[1]),
        dtype=np.float64,
    )

    for i in range(n_configs):
        drdx = build_drdx(xyz_list[i], r_i, r_j)

        for j in range(n_carts):
            # Fortran 1-based cartesian indexing
            dp_list[i, j] = gradient.dbemsav(
                drdx,
                mono_list[i],
                p_list[i],
                j + 1,
            )

    return dp_list


def v_calc_V(
    energy_list: np.ndarray,
    ref_energy: float,
) -> np.ndarray:
    V_list = (energy_list - ref_energy) * Hartree

    return V_list


def load_force_file(
    force_path: str,
    n_samples: int,
    n_atoms: int,
) -> np.ndarray:
    force_list = np.atleast_2d(np.loadtxt(force_path))

    expected = (n_samples, 3 * n_atoms)

    if force_list.shape != expected:
        raise ValueError(
            f"Force file '{force_path}' has shape {force_list.shape}, "
            f"but expected {expected} (eV/Angstrom, atom-major)."
        )

    return force_list.reshape(n_samples, n_atoms, 3)


def run_prepare_msa(
    config: PrepareConfig,
    msa_path: str,
) -> PrepareSummary:
    hint_force_file(config)

    # load msa so
    msa = load_msa_so(msa_path)
    basis = msa.basis

    # load xyz
    mol_list = cast(List[Atoms], read(config.xyz, index=":"))
    xyz_list = np.array([mol.get_positions() for mol in mol_list])

    # parse energy
    energy_list = np.loadtxt(config.energy)

    if len(xyz_list) != len(energy_list):
        raise ValueError(
            f"Dimension mismatch: xyz has {len(xyz_list)} frames, "
            f"but energy has {len(energy_list)} entries."
        )

    # morse -> mono -> poly
    morse_list = v_calc_morse(xyz_list, config.alpha)

    mono_list = np.apply_along_axis(
        func1d=basis.evmono,
        axis=1,
        arr=morse_list,
    )

    p_list = np.apply_along_axis(
        func1d=basis.evpoly,
        axis=1,
        arr=mono_list,
    )

    # d(p)/d(xyz), only needed for force-aided training
    dp_list = None

    if config.force is not None:
        dp_list = v_calc_dp(
            xyz_list=xyz_list,
            alpha=config.alpha,
            gradient=msa.gradient,
            p_list=p_list,
            mono_list=mono_list,
        )

    # ref energy
    if config.ref_energy is not None:
        ref_energy = config.ref_energy
    else:
        ref_energy = float(energy_list.min())

    V_list = v_calc_V(energy_list, ref_energy)

    # forces in eV/Angstrom
    F_list = None

    if config.force is not None:
        F_list = load_force_file(config.force, len(xyz_list), xyz_list.shape[1])

    # dataset
    cqpes_data = CQPESData(
        xyz=xyz_list,
        alpha=config.alpha,
        p=p_list,
        V=V_list,
        ref_energy=ref_energy,
        F=F_list,
        dp=dp_list,
    )

    output_path = cqpes_data.to_dir(config.output)

    assert cqpes_data.V_min is not None and cqpes_data.V_max is not None

    msa_filename = os.path.basename(msa_path)
    archived_msa = os.path.join(output_path, msa_filename)
    shutil.copy2(msa_path, archived_msa)

    return PrepareSummary(
        n_samples=cqpes_data.n_samples,
        n_atoms=xyz_list.shape[1],
        alpha=config.alpha,
        n_pip=p_list.shape[1],
        ref_energy=ref_energy,
        v_range=(cqpes_data.V_min, cqpes_data.V_max),
        output_dir=output_path,
    )
