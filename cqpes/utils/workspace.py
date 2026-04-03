import glob
import json
import os
import shutil
from dataclasses import asdict
from datetime import datetime

from cqpes.types.train import TrainConfig


class ExperimentWorkspace:
    def __init__(
        self,
        path: str,
    ) -> None:
        self.path = os.path.abspath(path)

    @classmethod
    def create(
        cls,
        base_workdir: str,
    ) -> "ExperimentWorkspace":
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        path = f"{base_workdir}_{timestamp}"
        os.makedirs(path, exist_ok=True)

        return cls(path)

    @classmethod
    def from_existing(
        cls,
        path: str,
    ) -> "ExperimentWorkspace":
        if not os.path.exists(path):
            raise FileNotFoundError(
                f"[FATAL] Workspace directory not found: {path}"
            )

        return cls(path)

    def backup_artifacts(
        self,
        data_dir: str,
        config: TrainConfig,
    ) -> None:
        exts = ["*.so", "*.json.gz", "*.npy"]

        for ext in exts:
            for f in glob.glob(os.path.join(data_dir, ext)):
                shutil.copy2(f, self.path)

        weight_script = os.path.abspath("weighting.py")

        if os.path.exists(weight_script):
            shutil.copy2(weight_script, self.path)

        config_path = os.path.join(self.path, "train.json")

        with open(config_path, "w") as f:
            json.dump(asdict(config), f, indent=4)

    def get_subpath(self, subname: str) -> str:
        full_path = os.path.join(self.path, subname)
        os.makedirs(full_path, exist_ok=True)
        return full_path

    def __repr__(self) -> str:
        return f"ExperimentWorkspace(path='{self.path}')"
