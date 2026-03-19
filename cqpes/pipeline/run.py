import os
import sys
from datetime import datetime

import tensorflow as tf
from ase import Atoms, units
from ase.io import read, write
from ase.md.langevin import Langevin
from ase.md.logger import MDLogger
from ase.md.velocitydistribution import MaxwellBoltzmannDistribution
from ase.md.verlet import VelocityVerlet
from ase.optimize import BFGS
from ase.vibrations import Vibrations
from sella import Sella
from tqdm import tqdm

from cqpes.interface.ase import CQPESCalculator
from cqpes.utils.logger import print_header

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"

tf.config.set_visible_devices([], "GPU")
tf.keras.backend.set_floatx("float64")


def run_task(args) -> None:
    if (args.opt and args.md) or (args.freq and args.md):
        raise RuntimeError(
            "Only opt and freq tasks can be run sequentially.\n"
            "MD tasks are supposed to run individually."
        )

    # 1. read xyz
    data = read(args.xyz)
    atoms: Atoms = data[-1] if isinstance(data, list) else data

    # 2. setup calculator
    calc = CQPESCalculator(workdir=args.workdir, force_mode="analytical")
    atoms.calc = calc

    # 3. tasks
    output_basename = args.output.replace(".xyz", "")
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    traj_file = None

    # (1) opt
    if args.opt:
        print_header("GEOMETRY OPTIMIZATION")

        if args.opt == "min":
            print(
                f"  [{'OPT':^10}] Mode: Minimization (BFGS) | fmax: {args.fmax}"
            )
            dyn = BFGS(atoms)
        else:
            print(f"  [{'TS':^10}] Mode: TS Search (Sella) | fmax: {args.fmax}")
            dyn = Sella(atoms)

        traj_file = f"{output_basename}_opt_{timestamp}.xyz"
        dyn.attach(lambda: write(traj_file, atoms, append=True))
        dyn.run(fmax=args.fmax, steps=args.steps)

    # (2) freq
    if args.freq:
        if args.opt:
            print_header("VIBRATIONAL ANALYSIS", no_header=True)
        else:
            print_header("VIBRATIONAL ANALYSIS")

        atoms.calc.parameters["force_mode"] = args.freq
        atoms.calc.pot.force_mode = args.freq

        vib_name = f"vib_{args.freq[:3]}"
        vib = Vibrations(atoms, name=vib_name, delta=args.freq_delta)

        print(
            f"  [{'FREQ':^10}] Mode: {args.freq.upper()} | "
            f"delta: {args.freq_delta} A"
        )

        vib.clean()
        vib.run()

        print(f"\n  [{'RESULT':^10}] Frequency Summary ({args.freq}):")

        vib.summary()

        atoms.calc.parameters["force_mode"] = "analytical"
        atoms.calc.pot.force_mode = "analytical"

    # (3) md
    if args.md:
        print_header("MOLECULAR DYNAMICS")

        MaxwellBoltzmannDistribution(atoms, temperature_K=args.temp)

        print(
            f"  [{'MD':^10}] Distribution: Maxwell-Boltzmann | "
            f"Temp: {args.temp} K"
        )

        dt_ase = args.dt * units.fs

        if args.md == "nve":
            # nve
            print(
                f"  [{'NVE':^10}] Ensemble: NVE (Velocity Verlet) | "
                f"dt: {args.dt} fs"
            )

            dyn_md = VelocityVerlet(
                atoms,
                timestep=dt_ase,
            )
        else:
            # nvt
            print(
                f"  [{'NVT':^10}] Ensemble: NVT (Langevin) | "
                f"Target: {args.temp} K"
            )

            dyn_md = Langevin(
                atoms,
                timestep=dt_ase,
                temperature_K=args.temp,
                friction=0.01 / units.fs,
            )

        traj_file = f"{output_basename}_md_{args.md}_{timestamp}.xyz"
        log_file = f"{output_basename}_md_{args.md}_{timestamp}.log"

        print(f"  [{'EXEC':^10}] Running {args.steps} MD steps...")

        # 1. traj
        dyn_md.attach(lambda: write(traj_file, atoms, append=True))

        # 2. logger
        logger = MDLogger(
            dyn_md,
            atoms,
            log_file,
            header=True,
            stress=False,
            peratom=False,
            mode="w",
        )

        dyn_md.attach(logger, interval=1)

        # 2. pbar
        pbar = tqdm(
            total=args.steps,
            desc=f"  [{'PROGRESS':^10}]",
            ncols=85,
            unit="step",
            file=sys.stdout,
            bar_format=(
                "{desc} {percentage:3.0f}% |{bar}| "
                "{n_fmt}/{total_fmt}[{elapsed}<{remaining}]"
            ),
        )

        md_state = {"is_step_zero": True}

        def update_pbar():
            if md_state["is_step_zero"]:
                md_state["is_step_zero"] = False
            else:
                pbar.update(1)

        dyn_md.attach(update_pbar)

        dyn_md.run(args.steps)

        pbar.close()

    if traj_file:
        print(f"  [{'SAVED':^10}] Trajectory saved to: {traj_file}")

        if args.md:
            print(f"  [{'SAVED':^10}] MD log saved to: {log_file}")

    print(f"  [{'DONE':^10}] Process completed")
