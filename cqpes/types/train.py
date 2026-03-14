import json
import os
from dataclasses import dataclass, field, fields
from typing import List, Literal


@dataclass
class NetworkConfig:
    layers: List[int] = field(default_factory=lambda: [20, 20])
    activation: Literal["tanh", "isru", "sigmoid"] = "tanh"


@dataclass
class FitConfig:
    lr: float = 0.001
    epoch: int = 1000
    batch_size: int = -1  # full batch


@dataclass
class LMConfig:
    adaptive_scaling: bool = True
    fletcher: bool = False
    solve_method: Literal["solve", "lstsq"] = "solve"
    jacobian_max_num_rows: int = 500


@dataclass
class TrainConfig:
    data: str = field(metadata={"must_exist": True})
    workdir: str = field(metadata={"must_not_exist": False})
    network: NetworkConfig = field(default_factory=NetworkConfig)
    fit: FitConfig = field(default_factory=FitConfig)
    lm: LMConfig = field(default_factory=LMConfig)
    split: List[float] = field(default_factory=lambda: [0.9, 0.05, 0.05])

    def __post_init__(self) -> None:
        for f in fields(self):
            val = getattr(self, f.name)

            if not isinstance(val, str):
                continue

            # to abs path
            if ("must_exist" in f.metadata) or ("must_not_exist" in f.metadata):
                abs_val = os.path.abspath(val)
                setattr(self, f.name, abs_val)

                # check data
                if f.metadata.get("must_exist") and not os.path.exists(abs_val):
                    raise FileNotFoundError(
                        f"[{f.name}] Data directory '{abs_val}' not found. "
                        f"Please run 'cqpes prepare' first."
                    )

                # check workdir
                if f.metadata.get("must_not_exist") and os.path.exists(abs_val):
                    raise FileExistsError(
                        f"[{f.name}] Work directory '{abs_val}' already exists."
                    )

    @classmethod
    def from_json(
        cls,
        config_json: str,
    ) -> "TrainConfig":
        if not os.path.exists(config_json):
            raise FileNotFoundError(f"Config file not found: {config_json}")

        with open(config_json, "r") as f:
            d = json.load(f)

        return cls(
            data=d["data"],
            workdir=d["workdir"],
            network=NetworkConfig(**d.get("network", {})),
            fit=FitConfig(**d.get("fit", {})),
            lm=LMConfig(**d.get("lm", {})),
            split=d.get("split", [0.9, 0.05, 0.05]),
        )
