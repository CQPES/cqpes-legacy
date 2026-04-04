import os
import sys
from typing import List, Literal, cast

import numpy as np
from ase import Atoms
from ase.io import read, write

from cqpes import CQPESPot


def run_predict(
    workdir_path: str,
    xyz_path: str,
    output_path: str,
    return_au: bool = False,
    calc_forces: bool = True,
    force_mode: Literal["analytical", "numerical"] = "analytical",
) -> None:
    try:
        pot = CQPESPot(workdir_path, force_mode=force_mode)

        print(f"  [{'READ':^10}] Target: {os.path.basename(xyz_path)}")

        mol_list = cast(List[Atoms], read(xyz_path, index=":"))
        num_frames = len(mol_list)
        xyz_batch = np.array([mol.get_positions() for mol in mol_list])

        print(f"  [{'SAMPLES':^10}] Loaded {num_frames} frames.")
        print(f"  [{'COMPUTE':^10}] Neural Network forward passing (Energy)...")

        energies = pot.get_energy(xyz_batch, return_au=return_au)

        if np.isscalar(energies):
            energies = [energies]

        if calc_forces:
            print(
                f"  [{'COMPUTE':^10}] Engine resolving forces ({force_mode})..."
            )

            forces_batch = pot.get_forces(xyz_batch, return_au=return_au)

            if forces_batch.ndim == 2:
                forces_batch = np.expand_dims(forces_batch, axis=0)

        if return_au:
            print(
                f"  [{'SCALE':^10}] "
                "Converting to Atomic Units (Bohr, Hartree)..."
            )
            unit_e, unit_c, unit_f = "Hartree", "Bohr", "Hartree/Bohr"
        else:
            unit_e, unit_c, unit_f = "eV", "Angstrom", "eV/Angstrom"

        print(f"  [{'PACK':^10}] Injecting properties into ExtXYZ format...")

        for i, atoms in enumerate(mol_list):
            # to au
            atoms.info["energy"] = energies[i]
            atoms.info["energy_unit"] = unit_e
            atoms.info["length_unit"] = unit_c

            if calc_forces:
                atoms.arrays["forces"] = forces_batch[i]  # type: ignore
                atoms.info["force_unit"] = unit_f

        write(output_path, mol_list, format="extxyz")

        print(
            f"  [{'DONE':^10}] ExtXYZ trajectory saved to "
            f"{os.path.basename(output_path)}"
        )

    except Exception as e:
        print(f"\n[ERROR] Prediction failed: {e}", file=sys.stderr)
        sys.exit(1)
