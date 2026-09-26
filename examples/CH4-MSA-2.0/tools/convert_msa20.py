"""Convert the MSA-2.0 geom.inp dataset (energy + gradients) to extxyz.

MSA-2.0 `geom.inp` format (per the MSA Tutorial):
  line 1        : number of atoms
  line 2        : ab initio energy in Hartree
  next N lines  : <element> x y z gx gy gz
                  coordinates in ANGSTROM, gradients dE/dxyz in HARTREE/BOHR
  atom order    : permutation group order, for CH4 A4B -> [H, H, H, H, C]

Produces a single `extxyz` trajectory - the format `cqpes prepare`
expects for force-aided fitting (point 'xyz', 'energy' and 'force' at
it in prepare.json) - with ASE-unit conventions:

  energy = -<- absolute energy converted to eV (kept as-is: the PES
            reproduces these values verbatim, no reference is subtracted)
  forces = -grad, converted Hartree/Bohr -> eV/Angstrom:

    F[eV/A] = -grad[Hartree/Bohr] * Hartree / Bohr

(this script verifies the parsed data: per-frame sum of gradients ~ 0,
consistent element symbols, and reports the converted force RMS).

Usage:
  python3 convert_msa20.py \
      --geom ../CH4-with-forces/PyMSA-Builder/MSA-2.0/geom.inp \
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

    # the unit traps, applied exactly once:
    #   energy: Hartree -> eV (kept verbatim - this is what the PES fits)
    #   forces = -gradients, Hartree/Bohr -> eV/Angstrom
    energies_ev = energies * Hartree
    forces = -grads * HA_BOHR_TO_EV_A

    print(f"  [  CHECK  ] energy range: {energies_ev.min():.6f} .. {energies_ev.max():.6f} eV")
    print(f"  [  CHECK  ] force RMS after conversion: {np.sqrt((forces**2).mean()):.4f} eV/A")

    os.makedirs(args.out_dir, exist_ok=True)

    extxyz_path = os.path.join(args.out_dir, f"{args.name}.extxyz")

    with open(extxyz_path, "w") as f:
        for frame in range(n_frames):
            f.write(f"{n_atoms}\n")
            f.write(
                f'Properties=species:S:1:pos:R:3:forces:R:3 '
                f'energy={energies_ev[frame]:.12f} pbc="F F F"\n'
            )

            for sym, pos, force in zip(symbols, xyz[frame], forces[frame]):
                f.write(
                    f"{sym:<2} "
                    f"{pos[0]:>18.12f} {pos[1]:>18.12f} {pos[2]:>18.12f} "
                    f"{force[0]:>18.12f} {force[1]:>18.12f} {force[2]:>18.12f}\n"
                )

    print(f"  [   DONE  ] {n_frames} frames -> {extxyz_path}")


if __name__ == "__main__":
    sys.exit(main())
