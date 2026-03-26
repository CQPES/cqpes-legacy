import libch4
import numpy as np
from gau_pes import BasePES

_NUM_ATOMS = 5


class CH4PES(BasePES):
    def __init__(self) -> None:
        libch4.init()

    def calc_energy(
        self,
        coords: np.array,
    ) -> float:
        """Calculate potential energy of CH4 system"""
        self._check_coords(_NUM_ATOMS, coords)

        return libch4.calc_energy(coords)


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
