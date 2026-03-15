from typing import Literal

from ase.calculators.calculator import Calculator, all_changes

from cqpes.interface.potential import CQPESPot


class CQPESCalculator(Calculator):
    implemented_properties = ["energy", "forces"]

    def __init__(
        self,
        workdir: str,
        force_mode: Literal["analytical", "numerical"] = "analytical",
        **kwargs,
    ) -> None:
        Calculator.__init__(self, **kwargs)

        self.pot = CQPESPot(
            workdir=workdir,
            force_mode=force_mode,
            **kwargs,
        )

    def calculate(
        self,
        atoms=None,
        properties=["energy"],
        system_changes=all_changes,
    ) -> None:
        Calculator.calculate(self, atoms, properties, system_changes)

        # get pos
        pos = self.atoms.get_positions()  # type: ignore

        # calculate energy in eV
        energy_eV = self.pot.get_energy([pos])
        self.results["energy"] = energy_eV

        # calculate forces in eV/Ang
        if "forces" in properties:
            self.results["forces"] = self.pot.get_forces(self.atoms)  # type: ignore
