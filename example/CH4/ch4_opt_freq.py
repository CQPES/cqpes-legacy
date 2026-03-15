import sys
import time

import numpy as np
from ase import Atoms
from ase.optimize import BFGS
from ase.vibrations import Vibrations

from cqpes import CQPESCalculator


# ANSI Color Codes for Terminal Aesthetics
class Color:
    CYAN = "\033[96m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    RESET = "\033[0m"
    BOLD = "\033[1m"


def get_initial_ch4():
    """Return an initial unstable CH4 configuration."""
    return Atoms(
        "CH4",
        positions=[
            [0.00000000, 0.00000000, 0.00000000],
            [1.08594333, 0.00000000, 0.00000000],
            [-0.36198111, 0.51191859, -0.88666901],
            [-0.36198111, -1.02383719, 0.00000000],
            [-0.36198111, 0.51191859, 0.88666901],
        ],
        masses=[12.011, 1.008, 1.008, 1.008, 1.008],
    )


def run_full_task(workdir: str, mode: str):
    """Run a complete optimization + vibrational analysis workflow."""
    atoms = get_initial_ch4()
    calc = CQPESCalculator(workdir=workdir, force_mode=mode)  # type: ignore
    atoms.calc = calc

    # 1. Geometry Optimization
    t_start = time.perf_counter()
    # Keep quiet, logs handled by the external benchmark logic
    opt = BFGS(atoms, logfile=None)
    opt.run(fmax=0.001)
    opt_time = time.perf_counter() - t_start

    # Capture optimized energy and forces for comparison
    final_energy = atoms.get_potential_energy()
    final_forces = atoms.get_forces()
    final_pos = atoms.get_positions().copy()

    # 2. Vibrational Analysis
    t_start = time.perf_counter()
    vib_name = f"ch4_vib_{mode}"
    vib = Vibrations(atoms, name=vib_name, delta=0.01)
    vib.run()
    freqs = vib.get_frequencies()
    vib_time = time.perf_counter() - t_start
    vib.clean()  # Clean up .dat files in the working directory

    return {
        "pos": final_pos,
        "energy": final_energy,
        "forces": final_forces,
        "freqs": freqs,
        "opt_time": opt_time,
        "vib_time": vib_time,
    }


if __name__ == "__main__":
    # 1. CLI Argument Handling
    if len(sys.argv) < 2:
        print(f"Usage: python {sys.argv[0]} <workdir_path>")
        sys.exit(1)

    workdir = sys.argv[1]
    print(f"\n{Color.BOLD}{' CQPES CROSS-MODE BENCHMARK ':=^60}{Color.RESET}")
    print(f"Working Directory: {workdir}")
    print(f"Timestamp: {time.strftime('%Y-%m-%d %H:%M:%S')}")

    # 2. Execute both modes sequentially
    print(f"\n{Color.CYAN}[EXEC]{Color.RESET} Running Analytical Mode...")
    res_ana = run_full_task(workdir, "analytical")

    print(f"{Color.CYAN}[EXEC]{Color.RESET} Running Numerical Mode...")
    res_num = run_full_task(workdir, "numerical")

    # 3. Results Summary and Comparison
    print(f"\n{Color.BOLD}{' COMPARISON REPORT ':=^60}{Color.RESET}")

    # (A) Performance Comparison
    print(f"\n1. PERFORMANCE (Elapsed Time)")
    print(
        f"   {'Step':<12} | {'Analytical':>12} | {'Numerical':>12} | {'Speedup':>10}"
    )
    print(f"   {'-' * 12}-+-{'-' * 12}-+-{'-' * 12}-+-{'-' * 10}")
    for step in ["opt_time", "vib_time"]:
        t_a = res_ana[step]
        t_n = res_num[step]
        speedup = t_n / t_a if t_a > 0 else 0
        print(f"   {step:<12} | {t_a:11.4f}s | {t_n:11.4f}s | {speedup:9.2f}x")

    t_total_a = res_ana["opt_time"] + res_ana["vib_time"]
    t_total_n = res_num["opt_time"] + res_num["vib_time"]
    print(
        f"   {'TOTAL':<12}{Color.RESET} | {t_total_a:11.4f}s | {t_total_n:11.4f}s | {t_total_n / t_total_a:9.2f}x"
    )

    # (B) Precision: Minimum Geometry & Energy Comparison
    pos_diff = np.max(np.abs(res_ana["pos"] - res_num["pos"]))
    force_diff = np.max(np.abs(res_ana["forces"] - res_num["forces"]))
    e_diff = abs(res_ana["energy"] - res_num["energy"]).item()

    print(f"\n2. PRECISION (Minimum Geometry & Energy)")
    print(f"   Max Position Diff:  {pos_diff:.6e} Angstrom")
    print(f"   Max Force Diff:     {force_diff:.6e} eV/Ang")
    print(f"   Energy Difference:  {e_diff:.6e} eV")

    # (C) Vibration Comparison (Selected Fundamental Modes)
    print(f"\n3. VIBRATIONS (Frequencies in cm^-1)")
    print(
        f"   {'Mode':<8} | {'Analytical':>12} | {'Numerical':>12} | {'Diff':>10}"
    )
    print(f"   {'-' * 8}-+-{'-' * 12}-+-{'-' * 12}-+-{'-' * 10}")

    num_modes = len(res_ana["freqs"])
    for i in range(num_modes):
        f_a = res_ana["freqs"][i].real
        f_n = res_num["freqs"][i].real
        print(
            f"   #{i + 1:02d}      | {f_a:12.2f} | {f_n:12.2f} | {abs(f_a - f_n):10.2f}"
        )

    print(f"\n{Color.BOLD}{' BENCHMARK COMPLETED ':=^60}{Color.RESET}\n")
