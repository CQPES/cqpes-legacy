# autopep8: off

import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__), os.pardir))

# autopep8: on

import argparse
import json
import warnings

import numpy as np
from msa import basis
from scipy import constants as C
from scipy.spatial import distance

from libmol import XYZMol

HEADER = r"""
                      _____ ____  _____  ______  _____                          
                     / ____/ __ \|  __ \|  ____|/ ____|                         
                    | |   | |  | | |__) | |__  | (___                           
                    | |   | |  | |  ___/|  __|  \___ \                          
                    | |___| |__| | |    | |____ ____) |                         
                     \_____\___\_\_|    |______|_____/                          
"""


def _parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-c",
        "--config",
        default="prepare.json",
        type=str,
        help="Path to configuration file",
    )

    args = parser.parse_args()

    return args


def _check_config(config):
    # xyz
    if not os.path.exists(config["xyz"]):
        raise FileNotFoundError(
            f"Rawdata xyz file {config['xyz']} not found",
        )

    # energy
    if not config["energy"]:
        warnings.warn(
            "Energy file not specified, fall back to extracting energy from "
            "the comment line in xyz file, make sure there is only one "
            "floating number in the comment line and it is the energy of "
            "current configuration in Hartree."
        )
    else:
        if not os.path.exists(config["energy"]):
            raise FileNotFoundError(
                f"Energy file {config['energy']} not found",
            )

    # ref_energy
    if not config["ref_energy"]:
        warnings.warn(
            "Use minimum energy as reference energy.",
        )

    # alpha
    if config["alpha"] < 0:
        raise ValueError(
            "alpha must be greater than 0.",
        )

    # output
    if os.path.exists(config["output"]):
        raise FileExistsError(
            f"Output directory {config['output']} already exists."
        )


def _print_header():
    print(f"{HEADER:^80}")
    print(f"{'CQPES: ChongQing Potential Energy Surface (legacy)':^80}")
    print()
    print(f"{'PREPARE':^80}")
    print("=" * 80)


def v_calc_p(
    xyz_list: np.array,
    alpha: float,
) -> np.array:
    def _calc_morse(
        r: np.array,
        alpha: float,
    ) -> np.array:
        return np.exp(-1.0 * r / alpha)

    r_list = np.array([distance.pdist(xyz) for xyz in xyz_list])

    morse_list = np.apply_along_axis(
        func1d=_calc_morse,
        axis=1,
        arr=r_list,
        alpha=alpha,
    )

    mono_list = np.apply_along_axis(
        func1d=basis.evmono,
        axis=1,
        arr=morse_list,
    )

    poly_list = np.apply_along_axis(
        func1d=basis.evpoly,
        axis=1,
        arr=mono_list,
    )

    return poly_list


def v_calc_V(
    energy_list: np.array,
    ref_energy: float,
) -> np.array:
    V_list = (energy_list - ref_energy) \
        * C.physical_constants["Hartree energy in eV"][0]

    return V_list


def scale(
    t: np.array,
    t_min: np.array,
    t_max: np.array,
):
    t_scaled = 2 * (t - t_min) / (t_max - t_min) - 1
    np.nan_to_num(t_scaled, copy=False)
    return t_scaled


if __name__ == "__main__":
    _print_header()

    args = _parse_args()

    with open(args.config, "r") as f:
        config = json.load(f)

    _check_config(config)

    # load rawdata
    mol_list = XYZMol.from_traj(config["xyz"])

    if config["energy"]:
        energy_list = np.loadtxt(config["energy"])
    else:
        energy_list = np.array([float(mol.title) for mol in mol_list])

    print(f"    - Input xyz file: {os.path.abspath(config['xyz'])}")
    print(f"    - Input energy file: {os.path.abspath(config['energy'])}")

    print("-" * 80)
    print(f"    - Number of molecules: {len(mol_list)}")
    print(f"    - Number of energy: {len(energy_list)}")

    if len(mol_list) != len(energy_list):
        raise ValueError(
            "Number of molecules and energy does not match!"
        )

    # calculate pip
    p_list = v_calc_p(
        np.array([mol.xyz for mol in mol_list]),
        alpha=config["alpha"],
    )

    p_min = p_list.min(axis=0)
    p_max = p_list.max(axis=0)

    X_list = scale(p_list, p_min, p_max)

    print("-" * 80)
    print(f"    - Number of atoms: {mol_list[0].num_atoms}")
    print(f"    - Length of PIP: {len(p_list[0])}")

    # convert energy to eV
    if not config['ref_energy']:
        ref_energy = energy_list.min()
    else:
        ref_energy = config['ref_energy']

    V_list = v_calc_V(energy_list, ref_energy)
    V_min = V_list.min()
    V_max = V_list.max()

    y_list = scale(V_list, V_min, V_max)

    print(f"    - Reference energy: {ref_energy:.8f} (Hartree)")
    print(f"    - Potential energy range: {V_min:.2f} ~ {V_max:.2f} (eV)")

    # write output
    os.makedirs(config['output'])

    print("-" * 80)
    print(f"    - Output directory: {os.path.abspath(config['output'])}")
    print("=" * 80)

    # save PIP
    np.save(os.path.join(config["output"], "p.npy"), p_list)
    np.save(os.path.join(config["output"], "p_min.npy"), p_min)
    np.save(os.path.join(config["output"], "p_max.npy"), p_max)

    # save energy
    np.save(os.path.join(config["output"], "ref_energy.npy"), ref_energy)
    np.save(os.path.join(config["output"], "V.npy"), V_list)
    np.save(os.path.join(config["output"], "V_min.npy"), V_min)
    np.save(os.path.join(config["output"], "V_max.npy"), V_max)

    # save training data
    np.save(os.path.join(config["output"], "X.npy"), X_list)
    np.save(os.path.join(config["output"], "y.npy"), y_list)
