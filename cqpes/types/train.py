import json
import os
from dataclasses import dataclass, field, fields
from typing import Any, Dict, List, Literal, Type, TypeVar

import numpy as np

T = TypeVar("T")


def _filter_kwargs(
    cls: Type[T],
    kwargs: Dict[str, Any],
) -> Dict[str, Any]:
    valid_keys = {f.name for f in fields(cls)}  # type: ignore
    return {k: v for k, v in kwargs.items() if k in valid_keys}


@dataclass
class NetworkConfig:
    layers: List[int] = field(default_factory=lambda: [20, 20])
    activation: Literal["tanh", "isru", "sigmoid"] = "tanh"


@dataclass
class FitConfig:
    lr: float = 0.001
    epoch: int = 1000
    batch_size: int = -1  # full batch
    # relative weight of the force term: MSE(E) + force_weight * MSE(F)
    # ignored when the dataset has no force data
    force_weight: float = 1.0


@dataclass
class LMConfig:
    """Legacy LM engine ("lm" key) -- routes to the upstream
    tf_levenberg_marquardt package, byte-identical to main-branch behavior."""

    adaptive_scaling: bool = True
    fletcher: bool = False
    solve_method: Literal["solve", "lstsq"] = "solve"
    jacobian_max_num_rows: int = 500


@dataclass
class RNCLMConfig:
    """New LM engine ("rnc-lm" key) -- routes to the vendored cqpes.lm fork
    (trust-ratio step control + geodesic curvature, RNC-LM arXiv:2607.07623).
    Defaults = validated production configuration (devlog §12/§14).
    Escape hatches: rnc_order=1 disables the curvature term;
    lambda_min <= 1e-10 disables the damping floor."""

    adaptive_scaling: bool = True
    fletcher: bool = False
    solve_method: Literal["solve", "lstsq"] = "solve"
    jacobian_max_num_rows: int = 500
    step_control: Literal["classic", "trust_ratio"] = "trust_ratio"
    eta_acc: float = 0.1  # acceptance threshold for the trust ratio rho
    max_t_retries: int = 3  # curve-length backtracking trials before lambda grows
    # geodesic-acceleration order: 1 = straight-line update,
    # 2 = adds the second-order correction c2 = G^-1 (-J^T R2) (RNC-LM Eq. 22)
    rnc_order: int = 2
    fd_h_rel: float = 1e-5  # relative FD step for R2 (h = fd_h_rel * |theta|/|d1|)
    # soft geodesic trust gate: cap ||d2/2|| at alpha_ga * ||d1|| (<=0 disables;
    # 1.0 = no gate). Guards against null-space amplification of c2 at tiny
    # lambda in sparsely sampled directions (Runge-type overshoot).
    alpha_ga: float = 0.75
    lambda_min: float = 1e-8  # floor for the lambda/3 branch (Eq. 40);
    # validated sweet spot on H3S sweep {1e-10..1e-7} (devlog §14)


@dataclass
class TrainConfig:
    data: str = field(metadata={"must_exist": True})
    workdir: str = field(metadata={"must_not_exist": False})
    network: NetworkConfig = field(default_factory=NetworkConfig)
    fit: FitConfig = field(default_factory=FitConfig)
    lm: LMConfig | None = field(default_factory=LMConfig)
    # new optimizer engine; mutually exclusive with `lm` at the JSON level
    # (JSON key: "rnc-lm")
    rnc_lm: RNCLMConfig | None = None
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

        if len(self.split) != 3:
            raise ValueError(
                f"split must contain exactly 3 elements "
                f"(Train, Valid, Test), got {len(self.split)}"
            )

        if not np.allclose(sum(self.split), 1.0):
            raise ValueError(
                f"split fractions must sum to 1.0, got {sum(self.split)}"
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

        lm_raw = d.get("lm")
        rnc_raw = d.get("rnc-lm")
        # empty dicts carry no information: treat them as absent so
        # placeholder keys never trigger the mutual-exclusion error
        if isinstance(lm_raw, dict) and not lm_raw:
            lm_raw = None
        if isinstance(rnc_raw, dict) and not rnc_raw:
            rnc_raw = None

        if lm_raw is not None and rnc_raw is not None:
            raise ValueError(
                'Config cannot specify both "lm" and "rnc-lm": '
                "the two LM engines are mutually exclusive. Pick one."
            )

        rnc_lm = (
            RNCLMConfig(**_filter_kwargs(RNCLMConfig, rnc_raw))
            if rnc_raw is not None
            else None
        )

        if lm_raw is not None:
            lm = LMConfig(**_filter_kwargs(LMConfig, lm_raw))
        elif rnc_raw is None:
            lm = LMConfig()  # backward-compatible default when neither key given
        else:
            lm = None  # "rnc-lm" present: the legacy engine is not in play

        return cls(
            data=d["data"],
            workdir=d["workdir"],
            network=NetworkConfig(
                **_filter_kwargs(NetworkConfig, d.get("network", {}))
            ),
            fit=FitConfig(**_filter_kwargs(FitConfig, d.get("fit", {}))),
            lm=lm,
            rnc_lm=rnc_lm,
            split=d.get("split", [0.9, 0.05, 0.05]),
        )
