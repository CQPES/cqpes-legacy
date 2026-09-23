import os

from cqpes.types import PrepareConfig


def hint_force_file(config: PrepareConfig) -> None:
    """Non-blocking hint printed when the rawdata directory contains files
    matching '*force*' but prepare.json has no "force" entry."""
    if config.force is not None:
        return

    raw_dir = os.path.dirname(config.xyz)

    candidates = sorted(
        f
        for f in os.listdir(raw_dir)
        if "force" in f.lower() and os.path.isfile(os.path.join(raw_dir, f))
    )

    if not candidates:
        return

    print(
        f"  [{'HINT':^10}] File(s) matching '*force*' found next to the "
        f"input xyz: {', '.join(candidates)}"
    )
    print(
        f"  {'':^12} To enable force-aided training, add a \"force\" "
        f"entry (eV/Angstrom) to prepare.json."
    )
