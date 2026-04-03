import glob
import os
import re
from typing import Tuple

from natsort import natsorted


def find_best_checkpoint(
    workdir: str,
) -> Tuple[str, str]:
    ckpt_dir = os.path.join(workdir, "ckpt")
    h5_files = natsorted(glob.glob(os.path.join(ckpt_dir, "*.h5")))

    if not h5_files:
        raise FileNotFoundError(f"No weights found in {ckpt_dir}")

    best_path = h5_files[-1]

    match = re.search(
        r"epoch_(\d+)_val_mse_([\d.e-]+)",
        os.path.basename(best_path),
    )

    label = match.group(0) if match else "epoch_unknown"

    return best_path, label
