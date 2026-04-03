import gzip
import os
import shutil
from typing import List, cast

import jax
import numpy as np
from ase import Atoms
from ase.io import read
from ase.units import Hartree
from jax import numpy as jnp
from jaxpip.descriptor import PolynomialDescriptor

import cqpes  # noqa: F401
from cqpes.types import CQPESData, PrepareConfig, PrepareSummary


def v_calc_V(
    energy_list: np.ndarray,
    ref_energy: float,
) -> np.ndarray:
    V_list = (energy_list - ref_energy) * Hartree

    return V_list


def run_prepare_jaxpip(
    config: PrepareConfig,
    basis_file: str,
) -> PrepareSummary:
    # load jaxpip basis
    descriptor = PolynomialDescriptor.from_file(
        basis_file=basis_file,
        alpha=config.alpha,
        decay_kernel="morse",  # TODO: Support reciprocal
        dtype=jnp.float64,
    )

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
    p_list = jax.vmap(descriptor)(jnp.asarray(xyz_list))
    p_list = np.asarray(p_list)

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

    basis_filename = os.path.basename(basis_file)

    if basis_filename.endswith(".gz"):
        target_basis_name = basis_filename
    else:
        target_basis_name = basis_filename + ".gz"

    archived_basis = os.path.join(output_path, target_basis_name)

    if not basis_file.endswith(".gz"):
        with open(basis_file, "rb") as f_in:
            with gzip.open(archived_basis, "wb") as f_out:
                shutil.copyfileobj(f_in, f_out)
    else:
        shutil.copy2(basis_file, archived_basis)

    return PrepareSummary(
        n_samples=cqpes_data.n_samples,
        n_atoms=xyz_list.shape[1],
        alpha=config.alpha,
        n_pip=p_list.shape[1],
        ref_energy=ref_energy,
        v_range=(cqpes_data.V_min, cqpes_data.V_max),
        output_dir=output_path,
    )
