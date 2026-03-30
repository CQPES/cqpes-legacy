import glob
import json
import os
from typing import List, Literal, Union

import jax
import numpy as np
from ase import Atoms
from ase.units import Hartree

jax.config.update("jax_enable_x64", True)

import equinox as eqx
from cqpes.interface.potential import CQPESBasePot
from jax import numpy as jnp
from jaxpip.descriptor import PolynomialDescriptor
from jaxpip.model import FeatureScaler, PolynomialNeuralNetwork


class CQPESJaxPIPPot(CQPESBasePot):
    def __init__(
        self,
        workdir: str,
        force_mode: Literal["analytical", "numerical"] = "analytical",
    ) -> None:
        self.workdir = os.path.abspath(workdir)
        self.force_mode = force_mode

        jaxpip_network = self._build_network(self.workdir)

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
        workdir: str,
    ) -> PolynomialNeuralNetwork:
        # find basis json.gz

        json_gz_files = glob.glob(os.path.join(workdir, "*.json.gz"))

        if len(json_gz_files) == 1:
            basis_file = json_gz_files[0]
        else:
            raise RuntimeError(
                f"Error: Multiple json.gz found: {json_gz_files}"
            )

        alpha = np.load(os.path.join(workdir, "alpha.npy")).item()

        self.ref_energy = np.load(
            os.path.join(workdir, "ref_energy.npy")
        ).item()

        with open(os.path.join(workdir, "export", "model_info.json")) as f:
            model_info = json.load(f)

        descriptor = PolynomialDescriptor.from_file(
            basis_file=basis_file,
            alpha=alpha,
            dtype=jnp.float64,
        )

        jaxpip_network = PolynomialNeuralNetwork(
            descriptor=descriptor,
            hidden_layers=model_info["hidden_layers"],
            key=jax.random.PRNGKey(1919810),
            activation=model_info["activation"],
        )

        jaxpip_network = eqx.tree_deserialise_leaves(
            os.path.join(workdir, "export", "model.eqx"),
            jaxpip_network,
        )

        return jaxpip_network

    def get_energy(
        self,
        xyz: Union[np.ndarray, List[Atoms]],
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
        xyz: Union[np.ndarray, List[Atoms]],
    ) -> np.ndarray:
        xyz_arr = self._standardize_coordinates(xyz)
        xyz_jx = jnp.asarray(xyz_arr)

        _, forces_jx = self._jit_energy_and_forces_batch(xyz_jx)

        forces_np = np.asarray(forces_jx)

        if len(xyz_arr) == 1 and not isinstance(xyz, list):
            return forces_np[0]

        return forces_np
