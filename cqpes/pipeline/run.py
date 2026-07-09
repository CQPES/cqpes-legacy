import sys
from datetime import datetime
from typing import List, Optional

from ase import Atoms, units
from ase.constraints import FixCom
from ase.io import read, write
from ase.md.langevin import Langevin
from ase.md.logger import MDLogger
from ase.md.velocitydistribution import MaxwellBoltzmannDistribution
from ase.md.verlet import VelocityVerlet
from ase.optimize import BFGS
from ase.vibrations import Vibrations
from tqdm import tqdm

from cqpes.interface.ase import CQPESCalculator
from cqpes.utils.logger import print_header


def _snapshot(atoms: Atoms) -> Atoms:
    frame = atoms.copy()
    try:
        frame.info["energy"] = float(atoms.get_potential_energy())
    except Exception:
        pass
    return frame


def run_task(
    workdir: str,
    xyz: str,
    opt: Optional[str] = None,
    fmax: Optional[float] = None,
    freq: Optional[str] = None,
    freq_delta: float = 0.01,
    md: Optional[str] = None,
    temp: float = 300.0,
    dt: float = 1.0,
    steps: int = 1000,
    output: str = "run_out.xyz",
    irc: bool = False,
) -> None:
    if opt == "min" and irc:
        raise RuntimeError(
            "IRC requires a TS geometry and is incompatible with --opt min."
        )

    if md and (opt or freq or irc):
        raise RuntimeError(
            "MD tasks must run individually and cannot be combined with "
            "--opt, --freq, or --irc."
        )

    if not (opt or freq or md or irc):
        raise RuntimeError(
            "No task specified. Use one of --opt, --freq, --irc, or --md."
        )

    # fmax default: IRC uses sella's recommended 0.05, others use 1e-05
    if fmax is None:
        fmax = 0.05 if irc else 1.0e-05

    # 1. read xyz
    data = read(xyz)
    atoms: Atoms = data[-1] if isinstance(data, list) else data

    # 2. setup calculator
    calc = CQPESCalculator(workdir=workdir, force_mode="analytical")
    atoms.calc = calc

    # 3. tasks
    output_basename = output.replace(".xyz", "")
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    saved_files: List[str] = []

    # (1) opt
    if opt:
        print_header("GEOMETRY OPTIMIZATION")

        if opt == "min":
            print(f"  [{'OPT':^10}] Mode: Minimization (BFGS) | fmax: {fmax}")
            dyn = BFGS(atoms)
        else:
            try:
                from sella import Sella
            except ImportError:
                print("\n[ERROR] 'sella' is required for TS search")
                print("Please install it via: pip install sella\n")
                sys.exit(1)

            print(f"  [{'TS':^10}] Mode: TS Search (Sella) | fmax: {fmax}")
            dyn = Sella(atoms)

        traj_opt = f"{output_basename}_opt_{timestamp}.xyz"
        dyn.attach(lambda: write(traj_opt, atoms, append=True))  # type: ignore
        dyn.run(fmax=fmax, steps=steps)
        saved_files.append(traj_opt)

    # (2) freq
    if freq:
        if opt:
            print_header("VIBRATIONAL ANALYSIS", no_header=True)
        else:
            print_header("VIBRATIONAL ANALYSIS")

        atoms.calc.parameters["force_mode"] = freq  # type: ignore
        atoms.calc.pot.force_mode = freq  # type: ignore

        vib_name = f"vib_{freq[:3]}"
        vib = Vibrations(atoms, name=vib_name, delta=freq_delta)

        print(f"  [{'FREQ':^10}] Mode: {freq.upper()} | delta: {freq_delta} A")

        vib.clean()
        vib.run()

        print(f"\n  [{'RESULT':^10}] Frequency Summary ({freq}):")

        vib.summary()

        atoms.calc.parameters["force_mode"] = "analytical"  # type: ignore
        atoms.calc.pot.force_mode = "analytical"  # type: ignore

    # (3) irc
    if irc:
        if opt:
            print_header("INTRINSIC REACTION COORDINATE", no_header=True)
        else:
            print_header("INTRINSIC REACTION COORDINATE")

        try:
            from sella import IRC
        except ImportError:
            print("\n[ERROR] 'sella' is required for IRC")
            print("Please install it via: pip install sella\n")
            sys.exit(1)

        print(
            f"  [{'IRC':^10}] Mode: Intrinsic Reaction Coordinate | fmax: {fmax}"
        )

        irc_dyn = IRC(atoms, logfile=None)  # type: ignore

        frames: List[Atoms] = []
        irc_dyn.attach(lambda: frames.append(_snapshot(atoms)))

        print(f"  [{'FWD':^10}] Stepping forward (toward product)...")
        irc_dyn.run(fmax=fmax, steps=steps, direction="forward")
        n_fwd = len(frames)

        # Each direction needs its own step budget and its own initial
        # observer call (which captures the TS frame). Resetting nsteps
        # makes ASE's Dynamics.irun treat reverse as a fresh run.
        irc_dyn.nsteps = 0

        print(f"  [{'REV':^10}] Stepping reverse (toward reactant)...")
        irc_dyn.run(fmax=fmax, steps=steps, direction="reverse")

        fwd_frames = frames[:n_fwd]
        rev_frames = frames[n_fwd:]

        # Assemble reactant -> TS -> product.
        # forward frames begin with the TS; reverse frames (after the
        # nsteps reset) also begin with the TS, so we drop one copy.
        full_path = list(reversed(rev_frames))
        if fwd_frames:
            full_path += fwd_frames[1:]

        traj_irc = f"{output_basename}_irc_{timestamp}.xyz"
        write(traj_irc, full_path, format="extxyz")
        saved_files.append(traj_irc)

        n_fwd_steps = max(len(fwd_frames) - 1, 0)
        n_rev_steps = max(len(rev_frames) - 1, 0)
        print(
            f"  [{'IRC':^10}] Forward steps: {n_fwd_steps} | "
            f"Reverse steps: {n_rev_steps}"
        )

    # (4) md
    if md:
        print_header("MOLECULAR DYNAMICS")

        MaxwellBoltzmannDistribution(atoms, temperature_K=temp)

        print(
            f"  [{'MD':^10}] Distribution: Maxwell-Boltzmann | Temp: {temp} K"
        )

        dt_ase = dt * units.fs

        if md == "nve":
            print(
                f"  [{'NVE':^10}] Ensemble: NVE (Velocity Verlet) | dt: {dt} fs"
            )

            dyn_md = VelocityVerlet(
                atoms,
                timestep=dt_ase,
            )
        else:
            print(
                f"  [{'NVT':^10}] Ensemble: NVT (Langevin) | Target: {temp} K"
            )

            atoms.set_constraint(FixCom())

            dyn_md = Langevin(
                atoms,
                timestep=dt_ase,
                temperature_K=temp,
                friction=0.01 / units.fs,
                fixcm=False,
            )

        traj_md = f"{output_basename}_md_{md}_{timestamp}.xyz"
        log_md = f"{output_basename}_md_{md}_{timestamp}.log"

        print(f"  [{'EXEC':^10}] Running {steps} MD steps...")

        # 1. traj
        dyn_md.attach(lambda: write(traj_md, atoms, append=True))

        # 2. logger
        logger = MDLogger(
            dyn_md,
            atoms,
            log_md,
            header=True,
            stress=False,
            peratom=False,
            mode="w",
        )

        dyn_md.attach(logger, interval=1)

        # 3. pbar
        pbar = tqdm(
            total=steps,
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

        dyn_md.run(steps)

        pbar.close()

        saved_files.append(traj_md)
        saved_files.append(log_md)

    # summary
    for f in saved_files:
        print(f"  [{'SAVED':^10}] {f}")

    print(f"  [{'DONE':^10}] Process completed")
