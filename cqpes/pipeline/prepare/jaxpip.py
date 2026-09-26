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

from cqpes.types import CQPESData, PrepareConfig, PrepareSummary
from jaxpip.descriptor import PolynomialDescriptor

from . import hint_force_file
from .extxyz import load_extxyz_dataset


def v_calc_V(
    energy_list: np.ndarray,
    ref_energy: float,
) -> np.ndarray:
    V_list = (energy_list - ref_energy) * Hartree

    return V_list


def v_calc_dp(
    xyz_list: np.ndarray,
    descriptor,
) -> np.ndarray:
    """d(p)/d(xyz) with shape (N, 3*Natoms, Npip) via forward-mode AD,
    same layout as the MSA dbemsav path."""
    jac_fn = jax.jacfwd(descriptor)

    # jacfwd output is output-first: (N, Npip, Natoms, 3)
    dp_list = jax.lax.map(jac_fn, jnp.asarray(xyz_list))

    n_configs, n_atoms, _ = xyz_list.shape

    dp_list = np.transpose(np.asarray(dp_list), (0, 2, 3, 1))

    return dp_list.reshape(n_configs, 3 * n_atoms, -1)


def run_prepare_jaxpip(
    config: PrepareConfig,
    basis_file: str,
) -> PrepareSummary:
    from cqpes._env import _setup_jax

    hint_force_file(config)

    _setup_jax()

    # load jaxpip basis
    descriptor = PolynomialDescriptor.from_file(
        basis_file=basis_file,
        alpha=config.alpha,
        decay_kernel="morse",  # TODO: Support reciprocal
        dtype=jnp.float64,
    )

    if config.use_extxyz:
        # extxyz energies are V (eV, taken as-is) - no reference shift;
        # the stored reference stays 0.0 (Hartree) for export/inference
        xyz_list, V_list, F_list, _ = load_extxyz_dataset(
            config.xyz, want_forces=config.force is not None
        )
        ref_energy = 0.0
    else:
        # legacy: xyz + absolute electronic energies in Hartree
        mol_list = cast(List[Atoms], read(config.xyz, index=":"))
        xyz_list = np.array([mol.get_positions() for mol in mol_list])

        energy_list = np.loadtxt(config.energy)

        if len(xyz_list) != len(energy_list):
            raise ValueError(
                f"Dimension mismatch: xyz has {len(xyz_list)} frames, "
                f"but energy has {len(energy_list)} entries."
            )

        if config.ref_energy is not None:
            ref_energy = config.ref_energy
        else:
            ref_energy = float(energy_list.min())

        V_list = v_calc_V(energy_list, ref_energy)

        F_list = None

    # p
    p_list = np.asarray(jax.lax.map(descriptor, jnp.asarray(xyz_list)))

    # d(p)/d(xyz), only needed for force-aided training
    dp_list = None

    if config.force is not None:
        dp_list = v_calc_dp(xyz_list, descriptor)

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
