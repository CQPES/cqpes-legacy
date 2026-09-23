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

        positions = self.atoms.get_positions()  # type: ignore

        if "forces" in properties:
            # single fused pass on backends that support it
            energy, forces = self.pot.get_energy_and_forces(
                positions,
                force_mode=self.parameters["force_mode"],
                delta=self.parameters["delta"],
            )

            self.results["energy"] = energy
            self.results["forces"] = forces
        else:
            self.results["energy"] = self.pot.get_energy(
                positions,
                return_au=False,
            )
