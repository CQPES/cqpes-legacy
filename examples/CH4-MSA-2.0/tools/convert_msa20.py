"""Convert the MSA-2.0 geom.inp dataset (energy + gradients) to CQPES rawdata.

MSA-2.0 `geom.inp` format (per the MSA Tutorial):
  line 1        : number of atoms
  line 2        : ab initio energy in Hartree
  next N lines  : <element> x y z gx gy gz
                  coordinates in ANGSTROM, gradients dE/dxyz in HARTREE/BOHR
  atom order    : permutation group order, for CH4 A4B -> [H, H, H, H, C]

CQPES rawdata convention:
  - coordinates in Angstrom            -> <name>.xyz
  - energies in Hartree                -> <name>_energy.dat
  - FORCES (F = -grad) in eV/Angstrom  -> <name>_force.dat  (N x 3*Natoms)

The unit trap: MSA stores GRADIENTS in Hartree/Bohr, NOT forces in
eV/Angstrom. The conversion applied here is

  F[eV/A] = -grad[Hartree/Bohr] * Hartree / Bohr

(this script verifies the parsed data: per-frame sum of gradients ~ 0,
consistent element symbols, and reports the converted force RMS).

Usage:
  python3 convert_msa20.py --geom ../PyMSA-Builder/MSA-2.0/geom.inp \
      --out-dir ../rawdata --name CH4
"""

import argparse
import os
import sys

import numpy as np
from ase.units import Bohr, Hartree

# Hartree/Bohr -> eV/Angstrom
HA_BOHR_TO_EV_A = Hartree / Bohr


def parse_geom_inp(path):
    lines = open(path).read().split("\n")

    energies, xyz, grads, symbols = [], [], [], None
    i = 0

    while i < len(lines):
        if not lines[i].strip():
            i += 1
            continue

        n_atoms = int(lines[i])
        i += 1

        energies.append(float(lines[i]))
        i += 1

        rows, syms = [], []

        for k in range(n_atoms):
            parts = lines[i + k].split()
            syms.append(parts[0])
            rows.append([float(v) for v in parts[1:7]])

        i += n_atoms

        rows = np.array(rows)
        xyz.append(rows[:, :3])
        grads.append(rows[:, 3:6])

        if symbols is None:
            symbols = syms
        elif syms != symbols:
            raise ValueError(f"Atom symbols changed at frame {len(energies)}: {syms}")

    return np.array(energies), np.array(xyz), np.array(grads), symbols


def sanity_checks(xyz, grads, symbols):
    n_frames, n_atoms, _ = xyz.shape

    if sorted(symbols) != sorted(["H"] * (n_atoms - 1) + ["C"]):
        raise ValueError(f"Unexpected composition: {symbols}")

    # permutation-group order for A4B: the four H's first, then C
    if symbols[-1] != "C" or set(symbols[:-1]) != {"H"}:
        raise ValueError(
            f"Atom order {symbols} does not match the A4B convention "
            "[H, H, H, H, C] of the MOL_4_1_4 basis."
        )

    # gradients of a translationally invariant energy must sum to ~0
    resid = np.abs(grads.sum(axis=1)).max()

    if resid > 1.0e-3:
        raise ValueError(
            f"Sum of gradients per frame reaches {resid:.2e} - data does not "
            "look like translational-invariant gradients."
        )

    print(f"  [  CHECK  ] {n_frames} frames, {n_atoms} atoms, symbols {''.join(symbols)}")
    print(
        f"  [  CHECK  ] grad sum per frame max |.| = {resid:.2e} "
        "(translational invariance OK)"
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--geom", required=True, help="MSA-2.0 geom.inp file")
    parser.add_argument("--out-dir", required=True)
    parser.add_argument("--name", default="CH4")
    args = parser.parse_args()

    energies, xyz, grads, symbols = parse_geom_inp(args.geom)

    sanity_checks(xyz, grads, symbols)

    n_frames, n_atoms, _ = xyz.shape

    # the unit trap, applied exactly once:
    #   forces = -gradients, Hartree/Bohr -> eV/Angstrom
    forces = -grads * HA_BOHR_TO_EV_A

    print(f"  [  CHECK  ] energy span: {energies.min():.6f} .. {energies.max():.6f} Hartree")
    print(f"  [  CHECK  ] force RMS after conversion: {np.sqrt((forces**2).mean()):.4f} eV/A")

    os.makedirs(args.out_dir, exist_ok=True)

    xyz_path = os.path.join(args.out_dir, f"{args.name}.xyz")

    with open(xyz_path, "w") as f:
        for frame in range(n_frames):
            f.write(f"{n_atoms}\n\n")

            for sym, pos in zip(symbols, xyz[frame]):
                f.write(
                    f"{sym:<2} {pos[0]:>18.10f} "
                    f"{pos[1]:>18.10f} {pos[2]:>18.10f}\n"
                )

    energy_path = os.path.join(args.out_dir, f"{args.name}_energy.dat")
    np.savetxt(energy_path, energies, fmt="%.12f")

    force_path = os.path.join(args.out_dir, f"{args.name}_force.dat")
    np.savetxt(force_path, forces.reshape(n_frames, 3 * n_atoms), fmt="%.12f")

    print(f"  [   DONE  ] {n_frames} frames -> {xyz_path}")
    print(f"              energies (Hartree):   {energy_path}")
    print(f"              forces (eV/A):        {force_path}")


if __name__ == "__main__":
    sys.exit(main())
