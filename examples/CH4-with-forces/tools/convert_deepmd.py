"""Convert a DeePMD-kit dataset (set.000 npys) to a single extxyz file.

Produces one `extxyz` trajectory carrying coordinates (Angstrom),
energies (eV) and forces (eV/Angstrom) - the format `cqpes prepare`
expects for force-aided fitting (point 'xyz', 'energy' and 'force' at
it in prepare.json).

The box/virial entries of DeePMD datasets are ignored (CQPES is
non-periodic only).

Atom order matters: the output order must match the convention the PIP
basis was built with (for MSA A(n)B(m): species blocks, A atoms first).
Use --permute "4,0,1,2,3" to reorder, e.g. to move the unique B atom to
the front. The permutation is applied to coordinates and forces alike.

Usage:
  python3 convert_deepmd.py \
      --deepmd-dir ../00.data \
      --out-dir ../rawdata \
      --name CH4
"""

import argparse
import glob
import os
import sys

import numpy as np


def load_split(split_dir):
    set_dirs = sorted(glob.glob(os.path.join(split_dir, "set.*")))

    if not set_dirs:
        raise FileNotFoundError(f"No set.* directories in {split_dir}")

    coord, energy, force = [], [], []

    for set_dir in set_dirs:
        coord.append(np.load(os.path.join(set_dir, "coord.npy")))
        energy.append(np.load(os.path.join(set_dir, "energy.npy")))
        force.append(np.load(os.path.join(set_dir, "force.npy")))

    return (
        np.concatenate(coord, axis=0),
        np.concatenate(energy, axis=0).ravel(),
        np.concatenate(force, axis=0),
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--deepmd-dir", required=True)
    parser.add_argument("--out-dir", required=True)
    parser.add_argument("--name", default="MOL")
    parser.add_argument(
        "--permute",
        default=None,
        help="Comma-separated atom index permutation, e.g. '4,0,1,2,3'",
    )
    args = parser.parse_args()

    perm = None

    if args.permute is not None:
        perm = np.array([int(i) for i in args.permute.split(",")])

    all_coord, all_energy, all_force = [], [], []

    for split in ["training_data", "validation_data"]:
        split_dir = os.path.join(args.deepmd_dir, split)

        if not os.path.isdir(split_dir):
            continue

        coord, energy, force = load_split(split_dir)

        all_coord.append(coord)
        all_energy.append(energy)
        all_force.append(force)

        print(f"  [{split:<16}] {len(coord)} frames")

    coords = np.concatenate(all_coord, axis=0)
    energies = np.concatenate(all_energy, axis=0)
    forces = np.concatenate(all_force, axis=0)

    n_frames, n_rows = coords.shape[0], coords.shape[1]

    if n_rows % 3 != 0:
        raise ValueError(f"coord rows {n_rows} not divisible by 3")

    n_atoms = n_rows // 3

    coords = coords.reshape(n_frames, n_atoms, 3)
    forces = forces.reshape(n_frames, n_atoms, 3)

    train_dir = os.path.join(args.deepmd_dir, "training_data")

    with open(os.path.join(train_dir, "type_map.raw")) as f:
        symbols = f.read().split()

    with open(os.path.join(train_dir, "type.raw")) as f:
        type_ids = [int(i) for i in f.read().split()]

    if perm is not None:
        if sorted(perm.tolist()) != list(range(n_atoms)):
            raise ValueError(f"Invalid permutation: {args.permute}")

        coords = coords[:, perm, :]
        forces = forces[:, perm, :]
        type_ids = [type_ids[i] for i in perm]

        print(f"  [PERMUTE] applied {args.permute}")

    atom_syms = [symbols[t] for t in type_ids]

    if len(atom_syms) != n_atoms:
        raise ValueError(
            f"type.raw has {len(atom_syms)} entries, coords have {n_atoms} atoms"
        )

    os.makedirs(args.out_dir, exist_ok=True)

    extxyz_path = os.path.join(args.out_dir, f"{args.name}.extxyz")

    with open(extxyz_path, "w") as f:
        for frame in range(n_frames):
            f.write(f"{n_atoms}\n")
            f.write(
                f'Properties=species:S:1:pos:R:3:forces:R:3 '
                f'energy={energies[frame]:.12f} pbc="F F F"\n'
            )

            for sym, pos, force in zip(atom_syms, coords[frame], forces[frame]):
                f.write(
                    f"{sym:<2} "
                    f"{pos[0]:>18.12f} {pos[1]:>18.12f} {pos[2]:>18.12f} "
                    f"{force[0]:>18.12f} {force[1]:>18.12f} {force[2]:>18.12f}\n"
                )

    print(f"  [  CHECK  ] energy range: {energies.min():.6f} .. {energies.max():.6f} eV")
    print(f"  [  CHECK  ] force RMS: {np.sqrt((forces**2).mean()):.4f} eV/A")
    print(f"  [   DONE  ] {n_frames} frames -> {extxyz_path}")


if __name__ == "__main__":
    sys.exit(main())
