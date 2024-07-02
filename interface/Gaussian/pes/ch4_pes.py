import os

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"

import sys

sys.path.append(os.path.dirname(__file__))

import numpy as np

np.seterr(divide='ignore', invalid='ignore')

import json

import tensorflow as tf
from gau_pes import BasePES
from msa import basis
from scipy import constants as C
from scipy.spatial import distance

_NUM_ATOMS = 5


class CH4PES(BasePES):
    def __init__(self) -> None:
        _dirname = os.path.dirname(__file__)

        with open(os.path.join(_dirname, "param.json")) as f:
            config = json.load(f)

        # load reference energy
        self.ref_energy = np.load(
            os.path.join(config["data"], "ref_energy.npy")
        )

        # load scaler for p
        self.p_min = np.load(os.path.join(config["data"], "p_min.npy"))
        self.p_max = np.load(os.path.join(config["data"], "p_max.npy"))

        # load scaler for V
        self.V_min = np.load(os.path.join(config["data"], "V_min.npy"))
        self.V_max = np.load(os.path.join(config["data"], "V_max.npy"))

        # load model
        self.model = tf.keras.models.load_model(config["model"])

    @staticmethod
    def scale(t, t_min, t_max):
        t_scaled = 2 * (t - t_min) / (t_max - t_min) - 1
        np.nan_to_num(t_scaled, copy=False)
        return t_scaled

    @staticmethod
    def inverse_scale(t_scaled, t_min, t_max):
        t = (t_scaled + 1) * (t_max - t_min) / 2 + t_min
        return t

    @staticmethod
    def morse(r: np.array, alpha=1.0) -> np.array:
        return np.exp(-1.0 * r / alpha)

    def calc_energy(
        self,
        coords: np.array
    ) -> float:
        """Calculate the potential energy of CH4 system.

        Order of atoms: C H H H H
        """
        self._check_coords(_NUM_ATOMS, coords)

        r = distance.pdist(coords)
        m = self.morse(r)
        mono = basis.evmono(m)
        poly = basis.evpoly(mono)

        X = self.scale(
            poly,
            self.p_min,
            self.p_max,
        ).reshape((1, -1))[:, 1:]

        V = self.inverse_scale(
            self.model(X).numpy(),
            self.V_min,
            self.V_max,
        ).flatten()[0]

        energy = V / C.physical_constants["Hartree energy in eV"][0] \
            + self.ref_energy

        return energy


if __name__ == "__main__":
    from gau_pes import GauDriver

    driver = GauDriver()
    pes = CH4PES()

    driver.write(
        energy=pes.calc_energy(driver.coords),
        gradients=(pes.calc_gradients(driver.coords)
                   if driver.derivs in [1, 2] else None),
        force_constants=(pes.calc_force_constants(
            driver.coords) if driver.derivs == 2 else None),
    )
