import numpy as np
from ase.io import read
from scipy.spatial import distance

# a vacuum box must exceed twice the molecular diameter by this much to be
# treated as non-periodic (minimum-image distances stay uncorrupted)
VACUUM_MARGIN = 2.0  # Angstrom


def _get_energy(frame) -> float:
    """Energy attached by ASE's extxyz reader (SinglePointCalculator) with
    a fallback to atoms.info for hand-written files."""
    try:
        return float(frame.get_potential_energy())
    except Exception:
        return frame.info.get("energy", None)


def _get_forces(frame):
    try:
        return frame.get_forces()
    except Exception:
        return frame.arrays.get("forces", None)


def _check_pbc(frame, index: int, path: str) -> None:
    """CQPES fits intramolecular distances only - genuinely periodic
    structures cannot be represented. Vacuum boxes (cell much larger than
    the molecule) are accepted with a warning."""
    if not frame.pbc.any():
        return

    pos = frame.get_positions()
    diam = float(distance.pdist(pos).max()) if len(pos) > 1 else 0.0

    lengths = frame.cell.lengths()

    if lengths.min() >= 2.0 * diam + VACUUM_MARGIN:
        print(
            f"  [{'  WARN':^10}] Frame {index}: vacuum box detected "
            f"(cell {lengths.min():.2f} A vs molecule {diam:.2f} A) - "
            f"treating as non-periodic."
        )
        return

    raise ValueError(
        f"[extxyz] Frame {index} of '{path}' looks genuinely periodic "
        f"(cell {lengths.min():.2f} A vs molecular diameter {diam:.2f} A). "
        f"CQPES supports non-periodic systems only."
    )


def load_extxyz_dataset(path: str, want_forces: bool):
    """Extract a dataset from an extxyz file.

    Energies are taken AS-IS (V, in eV per the ASE convention) - no
    reference is subtracted; forces come back in eV/Angstrom.

    Returns (xyz (N, Natoms, 3), V (N,), forces (N, Natoms, 3) or None,
    symbols). Raises when required properties are missing or inconsistent
    across frames.
    """
    frames = read(path, index=":")

    if len(frames) == 0:
        raise ValueError(f"[extxyz] No frames found in '{path}'.")

    xyz_list, v_list, force_list = [], [], []
    symbols = None

    for i, frame in enumerate(frames):
        _check_pbc(frame, i, path)

        energy = _get_energy(frame)

        if energy is None:
            raise ValueError(
                f"[extxyz] Frame {i} of '{path}' carries no energy."
            )

        forces = _get_forces(frame)

        if want_forces and forces is None:
            raise ValueError(
                f"[extxyz] Frame {i} of '{path}' carries no forces "
                f"(required for force-aided fitting)."
            )

        syms = frame.get_chemical_symbols()

        if symbols is None:
            symbols = syms
        elif syms != symbols:
            raise ValueError(
                f"[extxyz] Atom symbols changed at frame {i} of '{path}'."
            )

        xyz_list.append(frame.get_positions())
        v_list.append(energy)

        if want_forces:
            force_list.append(forces)

    return (
        np.array(xyz_list),
        np.array(v_list),
        np.array(force_list) if want_forces else None,
        symbols,
    )
