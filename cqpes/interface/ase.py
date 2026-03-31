from typing import List, Literal

from ase.calculators.calculator import Calculator, all_changes
from cqpes.interface.potential import CQPESPot


class CQPESCalculator(Calculator):
    implemented_properties = ["energy", "forces"]

    default_parameters = {
        "force_mode": "analytical",
        "delta": 0.01,
    }

    def __init__(
        self,
        workdir: str,
        force_mode: Literal["analytical", "numerical"] = "analytical",
        delta: float = 0.01,  # Angstrom
        **kwargs,
    ) -> None:
        super().__init__(**kwargs)

        if self.parameters is None:
            self.parameters = {}

        self.parameters.update(
            {
                "workdir": workdir,
                "force_mode": force_mode,
                "delta": delta,
            }
        )

        self.pot = CQPESPot(
            workdir=workdir,
            force_mode=force_mode,
        )

    def calculate(
        self,
        atoms=None,
        properties: List[str] = ["energy"],
        system_changes: List[str] = all_changes,
    ) -> None:
        super().calculate(atoms, properties, system_changes)

        self.results["energy"] = self.pot.get_energy(
            self.atoms,  # type: ignore
            return_au=False,
        )

        if "forces" in properties:
            self.results["forces"] = self.pot.get_forces(
                self.atoms,  # type: ignore
                force_mode=self.parameters["force_mode"],
                delta=self.parameters["delta"],
                return_au=False,
            )
