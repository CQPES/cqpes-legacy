import os
import shutil
from types import ModuleType
from typing import List, cast

import numpy as np
from ase import Atoms
from ase.io import read
from scipy import constants as C
from scipy.spatial import distance

from cqpes.types import CQPESData, PrepareConfig, PrepareSummary
from cqpes.utils.msa import load_msa_so


def v_calc_p(
    xyz_list: np.ndarray,
    alpha: float,
    basis: ModuleType,
) -> np.ndarray:
    def _calc_morse(
        r: np.ndarray,
        alpha: float,
    ) -> np.ndarray:
        return np.exp(-1.0 * r / alpha)

    r_list = np.array([distance.pdist(xyz) for xyz in xyz_list])

    morse_list = np.apply_along_axis(
        func1d=_calc_morse,
        axis=1,
        arr=r_list,
        alpha=alpha,
    )

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


def v_calc_V(
    energy_list: np.ndarray,
    ref_energy: float,
) -> np.ndarray:
    V_list = (energy_list - ref_energy) * C.physical_constants[
        "Hartree energy in eV"
    ][0]

    return V_list


def scale(
    t: np.ndarray,
    t_min: np.ndarray,
    t_max: np.ndarray,
) -> np.ndarray:
    t_scaled = np.nan_to_num(2 * (t - t_min) / (t_max - t_min) - 1)

    return t_scaled


def run_prepare(
    msa_path: str,
    config: PrepareConfig,
) -> PrepareSummary:
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

    # p
    p_list = v_calc_p(
        xyz_list=xyz_list,
        alpha=config.alpha,
        basis=basis,
    )

    # ref energy
    if config.ref_energy is not None:
        ref_energy = config.ref_energy
    else:
        ref_energy = float(energy_list.min())

    V_list = v_calc_V(energy_list, ref_energy)

    # dataset
    cqpes_data = CQPESData(
        xyz=xyz_list,
        alpha=config.alpha,
        p=p_list,
        V=V_list,
        ref_energy=ref_energy,
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
