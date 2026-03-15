import os
from dataclasses import dataclass
from typing import Any, Optional, Union

import numpy as np


@dataclass
class CQPESData:
    xyz: np.ndarray  # (N, Natoms, 3)
    alpha: float
    p: np.ndarray  # (N, Npip)
    V: np.ndarray  # (N,)
    ref_energy: float

    p_min: Optional[np.ndarray] = None
    p_max: Optional[np.ndarray] = None
    V_min: Optional[float] = None
    V_max: Optional[float] = None

    def __post_init__(self) -> None:
        n_samples = self.p.shape[0]
        if self.V.shape[0] != n_samples:
            raise ValueError(
                f"V size ({self.V.shape[0]}) mismatch with "
                f"p size ({n_samples})."
            )
        if self.xyz.size > 0 and self.xyz.shape[0] != n_samples:
            raise ValueError(
                f"xyz size ({self.xyz.shape[0]}) mismatch with "
                f"p size ({n_samples})."
            )

        if self.p_min is None:
            self.p_min = self.p.min(axis=0)

        if self.p_max is None:
            self.p_max = self.p.max(axis=0)

        if self.V_min is None:
            self.V_min = float(self.V.min())

        if self.V_max is None:
            self.V_max = float(self.V.max())

    @staticmethod
    def rescale(
        t: np.ndarray,
        t_min: Optional[Union[np.ndarray, float]],
        t_max: Optional[Union[np.ndarray, float]],
    ) -> np.ndarray:
        assert t_min is not None and t_max is not None

        return np.nan_to_num(2 * (t - t_min) / (t_max - t_min) - 1)

    @staticmethod
    def unscale(
        scaled_t: np.ndarray,
        t_min: Optional[Union[np.ndarray, float]],
        t_max: Optional[Union[np.ndarray, float]],
    ) -> np.ndarray:
        assert t_min is not None and t_max is not None

        return t_min + (scaled_t + 1) * (t_max - t_min) / 2

    @property
    def X(self) -> np.ndarray:
        return self.rescale(self.p, self.p_min, self.p_max)

    @property
    def y(self) -> np.ndarray:
        return self.rescale(self.V, self.V_min, self.V_max)

    @property
    def n_samples(self) -> int:
        return self.p.shape[0]

    @classmethod
    def from_dir(
        cls,
        path: str,
    ) -> "CQPESData":
        def _load(name: str):
            f_path = os.path.join(path, name)

            if not os.path.exists(f_path):
                raise FileNotFoundError(f"Missing: {f_path}")

            return np.load(f_path)

        return cls(
            xyz=_load("xyz.npy"),
            alpha=_load("alpha.npy"),
            p=_load("p.npy"),
            V=_load("V.npy"),
            ref_energy=float(_load("ref_energy.npy")),
            p_min=_load("p_min.npy"),
            p_max=_load("p_max.npy"),
            V_min=float(_load("V_min.npy")),
            V_max=float(_load("V_max.npy")),
        )

    def to_dir(
        self,
        path: str,
    ) -> str:
        if not os.path.exists(path):
            os.makedirs(path, exist_ok=True)

        payload = {
            "xyz.npy": self.xyz,
            "alpha.npy": self.alpha,
            "p.npy": self.p,
            "V.npy": self.V,
            "ref_energy.npy": np.array(self.ref_energy),
            "p_min.npy": self.p_min,
            "p_max.npy": self.p_max,
            "V_min.npy": np.array(self.V_min),
            "V_max.npy": np.array(self.V_max),
        }

        for filename, data in payload.items():
            np.save(os.path.join(path, filename), data)

        return os.path.abspath(path)

    def __getitem__(
        self,
        index: Any,
    ) -> "CQPESData":
        return CQPESData(
            xyz=self.xyz[index],
            alpha=self.alpha,
            p=self.p[index],
            V=self.V[index],
            ref_energy=self.ref_energy,
            p_min=self.p_min,
            p_max=self.p_max,
            V_min=self.V_min,
            V_max=self.V_max,
        )
