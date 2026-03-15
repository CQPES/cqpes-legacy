import json
import os
import warnings
from dataclasses import dataclass, field, fields
from typing import Optional, Tuple


@dataclass
class PrepareConfig:
    # path
    xyz: str = field(metadata={"must_exist": True})
    energy: str = field(metadata={"must_exist": True})
    output: str = field(metadata={"must_not_exist": True})

    # phys
    alpha: float
    ref_energy: Optional[float] = None

    def __post_init__(
        self,
    ) -> None:
        for f in fields(self):
            val = getattr(self, f.name)

            if val is None:
                continue

            # to abs path
            if ("must_exist" in f.metadata) or ("must_not_exist" in f.metadata):
                val = os.path.abspath(val)
                setattr(self, f.name, val)

            # check xyz and energy
            if f.metadata.get("must_exist") and not os.path.exists(val):
                raise FileNotFoundError(
                    f"[{f.name}] Input file '{val}' not found."
                )

            # check output
            if f.metadata.get("must_not_exist") and os.path.exists(val):
                raise FileExistsError(
                    f"[{f.name}] Output directory '{val}' already exists."
                )

        # check if reference energy exists
        if self.ref_energy is None:
            warnings.warn("Use minimum energy as reference energy.")

        # check if alpha > 0
        if self.alpha <= 0:
            raise ValueError(f"alpha must be greater than 0, got {self.alpha}.")

    @classmethod
    def from_json(
        cls,
        config_json: str,
    ) -> "PrepareConfig":
        if not os.path.exists(config_json):
            raise FileNotFoundError(f"Config file '{config_json}' not found.")

        with open(config_json, "r") as f:
            config_dict = json.load(f)

        return cls(**config_dict)


@dataclass(frozen=True)
class PrepareSummary:
    n_samples: int
    n_atoms: int
    alpha: float
    n_pip: int
    ref_energy: float
    v_range: Tuple[float, float]
    output_dir: str

    def log(self) -> None:
        from cqpes.utils.logger import print_header

        WIDTH = 80

        print_header("PREPARE SUMMARY")

        print(f"  {'Samples:':<16} {self.n_samples}")
        print(f"  {'Atoms:':<16} {self.n_atoms}")
        print(f"  {'Morse Alpha:':<16} {self.alpha:<16.4f}")
        print(f"  {'PIP Dimension:':<16} {self.n_pip}")

        print(f"  {'-' * (WIDTH - 4)}")

        print(f"  {'Ref Energy:':<16} {self.ref_energy:<16.8f} Hartree")
        print(f"  {'V_min (eV):':<16} {self.v_range[0]:<16.4f}")
        print(f"  {'V_max (eV):':<16} {self.v_range[1]:<16.4f}")

        print(f"  {'-' * (WIDTH - 4)}")

        print(f"  {'Output Dir:':<16}")
        print(f"    {self.output_dir}")

        print("=" * WIDTH)
