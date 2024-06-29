from dataclasses import dataclass
from typing import List

import numpy as np


@dataclass
class XYZMol:
    num_atoms: int
    title: str
    atom_name: List[str]
    xyz: np.array  # Angstrom

    def __str__(self) -> str:
        xyz_contents = []
        xyz_contents.append(f"{self.num_atoms}")
        xyz_contents.append(f"{self.title}")

        for i in range(self.num_atoms):
            xyz_contents.append(
                f"{self.atom_name[i]:4s}"
                f"{self.xyz[i][0]:13.8f}"
                f"{self.xyz[i][1]:13.8f}"
                f"{self.xyz[i][2]:13.8f}"
            )

        return "\n".join(xyz_contents)

    @staticmethod
    def from_file(
        xyz_file: str,
    ) -> "XYZMol":
        contents = open(xyz_file, "r").readlines()
        contents = [line.rstrip() for line in contents]

        num_atoms = int(contents.pop(0))
        title = contents.pop(0)
        atom_name = []
        xyz = []

        for _ in range(num_atoms):
            line = contents.pop(0)
            arr = line.split()
            atom_name.append(arr[0])
            atom_xyz = [float(x) for x in arr[1:4]]
            xyz.append(atom_xyz)

        xyz = np.array(xyz)

        xyz_mol = XYZMol(
            num_atoms=num_atoms,
            title=title,
            atom_name=atom_name,
            xyz=xyz,
        )

        return xyz_mol

    @staticmethod
    def from_traj(
        xyz_file: str,
    ) -> List["XYZMol"]:
        mol_list = []
        with open(xyz_file, "r") as f:
            while line := f.readline():
                num_atoms = int(line)
                atom_name = []
                xyz = []

                # title line
                title = f.readline()

                for _ in range(num_atoms):
                    line = f.readline()
                    arr = line.split()

                    atom_name.append(arr[0])
                    xyz.append([float(x) for x in arr[1: 4]])

                xyz = np.array(xyz)

                mol_list.append(XYZMol(
                    num_atoms=num_atoms,
                    title=title,
                    atom_name=atom_name,
                    xyz=xyz,
                ))

        return mol_list
