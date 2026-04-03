import glob
import os
from typing import Literal

import equinox as eqx
import jax
import numpy as np
from ase.units import Hartree
from jax import numpy as jnp
from jaxpip.model import PolynomialNeuralNetwork

import cqpes  # noqa: F401
from cqpes.interface.potential import CQPESBasePot
from cqpes.utils.workspace import ExperimentWorkspace


class CQPESJaxPIPPot(CQPESBasePot):
    def __init__(
        self,
        workdir: str,
        force_mode: Literal["analytical", "numerical"] = "analytical",
    ) -> None:
        self.workspace = ExperimentWorkspace.from_existing(workdir)
        self.force_mode = force_mode

        jaxpip_network = self._build_network()

        @eqx.filter_jit
        def wrapper_energy_batch(xyz):
            return jax.vmap(jaxpip_network.get_energy)(xyz)

        @eqx.filter_jit
        def wrapper_energy_and_forces_batch(xyz):
            return jax.vmap(jaxpip_network.get_energy_and_forces)(xyz)

        self._jit_energy_batch = wrapper_energy_batch
        self._jit_energy_and_forces_batch = wrapper_energy_and_forces_batch

    def _build_network(
        self,
    ) -> PolynomialNeuralNetwork:
        # find basis json.gz
        json_gz_files = glob.glob(
            os.path.join(
                self.workspace.path,
                "*.json.gz",
            )
        )

        if len(json_gz_files) == 0:
            raise RuntimeError("Error: No JaxPIP basis json found")
        elif len(json_gz_files) == 1:
            basis_file = json_gz_files[0]
        elif len(json_gz_files) > 1:
            raise RuntimeError(
                f"Error: Multiple JaxPIP basis json.gz found: {json_gz_files}"
            )

        # find network eqx
        export_dir = self.workspace.get_subpath("export")
        eqx_files = glob.glob(os.path.join(export_dir, "*.eqx"))

        if len(eqx_files) == 0:
            raise RuntimeError("Error: No JaxPIP network eqx")
        elif len(eqx_files) == 1:
            model_file = eqx_files[0]
        elif len(eqx_files) > 1:
            raise RuntimeError(f"Error: Multiple eqx found: {eqx_files}")

        # phys const
        self.ref_energy = np.load(
            os.path.join(
                self.workspace.path,
                "ref_energy.npy",
            )
        ).item()

        jaxpip_network = PolynomialNeuralNetwork.from_file(
            basis_file=basis_file,
            model_file=model_file,
        )

        return jaxpip_network

    def get_energy(
        self,
        xyz: np.ndarray,
        return_au: bool = False,
    ) -> np.ndarray:
        xyz_arr = self._standardize_coordinates(xyz)
        xyz_jx = jnp.asarray(xyz_arr)

        results = self._jit_energy_batch(xyz_jx)

        if return_au:
            results = (results / Hartree) + self.ref_energy

        if results.size == 1 and not isinstance(xyz, list):
            return results.item()

        return np.asarray(results)

    def get_forces_analytical(
        self,
        xyz: np.ndarray,
    ) -> np.ndarray:
        xyz_arr = self._standardize_coordinates(xyz)
        xyz_jx = jnp.asarray(xyz_arr)

        _, forces_jx = self._jit_energy_and_forces_batch(xyz_jx)

        forces_np = np.asarray(forces_jx)

        if len(xyz_arr) == 1 and not isinstance(xyz, list):
            return forces_np[0]

        return forces_np
