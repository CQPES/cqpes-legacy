import os
import sys
import time

from ase import Atoms
from ase.io import write
from ase.md.velocitydistribution import (MaxwellBoltzmannDistribution,
                                         Stationary, ZeroRotation)
from ase.md.verlet import VelocityVerlet
from ase.units import fs

from cqpes import CQPESCalculator


# ANSI Color Codes for Terminal Aesthetics
class Color:
    CYAN = "\033[96m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    RESET = "\033[0m"
    BOLD = "\033[1m"


def get_ch4_for_md():
    """返回平衡态的 CH4 构型 (Global Minimum)"""
    return Atoms(
        symbols=["C", "H", "H", "H", "H"],
        positions=[
            [0.00000000, 0.00000000, 0.00000000],
            [1.08594333, 0.00000000, 0.00000000],
            [-0.36198111, 0.51191859, -0.88666901],
            [-0.36198111, -1.02383719, 0.00000000],
            [-0.36198111, 0.51191859, 0.88666901],
        ],
        masses=[12.011, 1.008, 1.008, 1.008, 1.008],
    )


def run_nve(workdir: str):
    atoms = get_ch4_for_md()

    calc = CQPESCalculator(workdir=workdir, force_mode="analytical")
    atoms.calc = calc

    print(f"\n{Color.BOLD}{' CQPES NVE DYNAMICS TEST ':=^60}{Color.RESET}")
    print(f"Working Directory: {workdir}")

    MaxwellBoltzmannDistribution(atoms, temperature_K=300)
    Stationary(atoms)
    ZeroRotation(atoms)

    dt = 0.5 * fs
    dyn = VelocityVerlet(atoms, dt, logfile=None)

    traj_path = os.path.abspath(__file__).replace(".py", ".xyz")
    open(traj_path, "w").close()

    def write_traj():
        write(traj_path, atoms, format="xyz", append=True)

    dyn.attach(write_traj, interval=10)

    print(
        f"{Color.CYAN}[TRAJ]{Color.RESET} Appending coordinates to {os.path.basename(traj_path)}"
    )
    print(
        f"{Color.CYAN}[INIT]{Color.RESET} T = 300 K, dt = 0.5 fs, Steps = 1000 (0.5 ps)"
    )
    print(f"{'-' * 65}")
    print(
        f"{'Step':>6} | {'Time(fs)':>10} | {'Ep(eV)':>12} | {'Ek(eV)':>10} | {'Etot(eV)':>13}"
    )
    print(f"{'-' * 65}")

    def print_dyn():
        step = dyn.get_number_of_steps()
        if step % 50 == 0:
            ep = atoms.get_potential_energy().item()
            ek = atoms.get_kinetic_energy()
            etot = ep + ek
            t = step * 0.5
            print(
                f"{step:6d} | {t:10.1f} | {ep:12.6f} | {ek:10.6f} | {etot:13.6f}"
            )

    dyn.attach(print_dyn, interval=50)

    t0 = time.perf_counter()
    dyn.run(1000)
    t1 = time.perf_counter()

    print(f"{'-' * 65}")
    print(
        f"{Color.GREEN}[DONE]{Color.RESET} 1000 steps completed in {t1 - t0:.2f}s."
    )
    print(f"{Color.BOLD}{'=' * 60}{Color.RESET}\n")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"Usage: python {sys.argv[0]} <workdir_path>")
        sys.exit(1)

    run_nve(sys.argv[1])
