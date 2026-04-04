import argparse
import sys
import warnings
from importlib.metadata import PackageNotFoundError, version
from typing import Literal

import argcomplete

from cqpes.utils.logger import custom_warning, print_header

warnings.showwarning = custom_warning

warnings.filterwarnings("ignore", message=".*HDF5 file.*")
warnings.filterwarnings("ignore", message=".*native Keras format.*")


def get_version() -> str:
    try:
        return version("cqpes")
    except PackageNotFoundError:
        return "unknown-dev"


def fuzzy_choice(choices):
    def wrapper(value):
        matches = [c for c in choices if c.startswith(value.lower())]

        if len(matches) == 1:
            return matches[0]

        elif len(matches) > 1:
            import argparse

            raise argparse.ArgumentTypeError(
                f"ambiguous value '{value}': could match {', '.join(matches)}"
            )
        else:
            import argparse

            raise argparse.ArgumentTypeError(
                f"invalid choice '{value}' (choose from {', '.join(choices)})"
            )

    return wrapper


def main() -> None:
    current_version = get_version()

    parser = argparse.ArgumentParser(
        prog="cqpes",
        description="CQPES: GPU-Aided Potential Energy Surface Development Toolkit",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )

    parser.add_argument(
        "-v",
        "--version",
        action="version",
        version=f"%(prog)s {current_version}",
    )

    subparsers = parser.add_subparsers(
        dest="command",
        required=True,
        help="Sub-commands",
    )

    # 1. prepare
    prepare_parser = subparsers.add_parser(
        "prepare",
        help="Prepare dataset from raw files",
    )

    prepare_parser.add_argument(
        "config",
        help="Path to prepare.json",
    )

    prepare_parser.add_argument(
        "--msa",
        help="Path to compiled MSA .so module",
    )

    prepare_parser.add_argument(
        "--jaxpip",
        help="Path to JaxPIP basis file",
    )

    # 2. train
    train_parser = subparsers.add_parser(
        "train",
        help="Train PIP-NN model via Keras & TensorFlow",
    )

    train_parser.add_argument(
        "config",
        help="Path to train.json",
    )

    # 3. test
    test_parser = subparsers.add_parser(
        "test",
        help="Evaluate model performance and plot errors",
    )

    test_parser.add_argument(
        "workdir",
        help="Path to the workdir",
    )

    # 4. export
    export_parser = subparsers.add_parser(
        "export",
        help="Export trained model for dynamics interfaces",
    )

    export_parser.add_argument(
        "workdir",
        help="Path to the workdir",
    )

    export_parser.add_argument(
        "-t",
        "--type",
        choices=["h5", "potfit", "jaxpip"],
        default="h5",
        help="Export format",
    )

    # 5. predict
    predict_parser = subparsers.add_parser(
        "predict",
        help="Predict energies for a given XYZ trajectory",
    )

    predict_parser.add_argument(
        "workdir",
        help="Path to the experiment workdir",
    )

    predict_parser.add_argument(
        "xyz",
        help="Path to the target .xyz file",
    )

    predict_parser.add_argument(
        "--no-forces",
        action="store_false",
        default=True,
        dest="calc_forces",
        help="Disable force calculation (Energy only)",
    )

    predict_parser.add_argument(
        "--force-mode",
        default="analytical",
        type=fuzzy_choice(["analytical", "numerical"]),
        help="Force calculation mode (default: analytical)",
    )

    predict_parser.add_argument(
        "-o",
        "--output",
        default="predicted.xyz",
        help="Output file path (ExtXYZ format)",
    )

    predict_parser.add_argument(
        "--au",
        action="store_true",
        default=False,
        dest="return_au",
        help=(
            "Output in Atomic Units "
            "(Energy: Hartree, Coords: Bohr, Forces: Hartree/Bohr)"
        ),
    )

    # 6. run tasks
    run_parser = subparsers.add_parser(
        "run",
        help="Run tasks (Opt, TS, Freq, MD) using trained PES via ASE",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )

    run_parser.add_argument(
        "workdir",
        help="Path to the model directory",
    )

    run_parser.add_argument(
        "xyz",
        help="Path to the initial .xyz file",
    )

    task_group = run_parser.add_argument_group("task selection")
    ctrl_group = run_parser.add_argument_group("control parameters")

    # (1) opt
    task_group.add_argument(
        "--opt",
        nargs="?",
        const="min",
        choices=["min", "ts"],
        help="Optimization mode: 'min' (minimization) or 'ts' (transition state)",
    )

    ctrl_group.add_argument(
        "--fmax",
        type=float,
        default=1.0e-05,
        help="Force threshold (eV/Ang) for Opt & TS",
    )

    # (2) freq
    task_group.add_argument(
        "--freq",
        nargs="?",
        const="analytical",
        type=fuzzy_choice(["analytical", "numerical"]),
        help="Frequency analysis: 'analytical' or 'numerical'",
    )

    ctrl_group.add_argument(
        "--freq-delta",
        type=float,
        default=0.01,
        help=(
            "Displacement (Ang) for frequency analysis.\n"
            "Gaussian-recommended value. (https://gaussian.com/freq)"
        ),
    )

    # (3) md
    task_group.add_argument(
        "--md",
        nargs="?",
        const="nve",
        choices=["nve", "nvt"],
        help="Molecular Dynamics mode",
    )

    ctrl_group.add_argument(
        "--temp",
        type=float,
        default=300.0,
        help="Temperature in K for MD",
    )

    ctrl_group.add_argument(
        "--dt",
        type=float,
        default=1.0,
        help="Time step in fs for MD",
    )

    # general
    ctrl_group.add_argument(
        "--steps",
        type=int,
        default=1000,
        help="Max steps for Opt, TS, or MD",
    )

    ctrl_group.add_argument(
        "-o",
        "--output",
        default="run_out.xyz",
        help="Output trajectory path: default is run_out.xyz",
    )

    argcomplete.autocomplete(parser)
    args = parser.parse_args()

    if args.command == "prepare":
        if (args.msa is not None) and (args.jaxpip is not None):
            raise RuntimeError(
                "Conflict detected: Cannot specify both '--msa' and '--jaxpip' "
                "simultaneously. Please choose one backend."
            )

        if args.msa is not None:
            _run_prepare_msa(
                args.config,
                msa_path=args.msa,
            )
        elif args.jaxpip is not None:
            _run_prepare_jaxpip(
                args.config,
                basis_file=args.jaxpip,
            )
    elif args.command == "train":
        _run_train(args.config)
    elif args.command == "test":
        _run_test(args.workdir)
    elif args.command == "export":
        _run_export(
            args.workdir,
            export_type=args.type,
        )
    elif args.command == "predict":
        _run_predict(
            args.workdir,
            args.xyz,
            output_path=args.output,
            return_au=args.return_au,
            calc_forces=args.calc_forces,
            force_mode=args.force_mode,
        )
    elif args.command == "run":
        _run_tasks(args)


def _run_prepare_msa(
    config_path: str,
    msa_path: str,
) -> None:
    from cqpes.pipeline.prepare.msa import run_prepare_msa
    from cqpes.types.prepare import PrepareConfig
    from cqpes.utils.logger import print_header

    try:
        print_header("PREPARING DATASET")

        config = PrepareConfig.from_json(config_path)
        summary = run_prepare_msa(config, msa_path)

        summary.log()
    except Exception as e:
        print(f"\n[ERROR] Preparation failed: {e}", file=sys.stderr)
        sys.exit(1)


def _run_prepare_jaxpip(
    config_path: str,
    basis_file: str,
) -> None:
    from cqpes.pipeline.prepare.jaxpip import run_prepare_jaxpip
    from cqpes.types.prepare import PrepareConfig
    from cqpes.utils.logger import print_header

    try:
        print_header("PREPARING DATASET")

        config = PrepareConfig.from_json(config_path)
        summary = run_prepare_jaxpip(config, basis_file)

        summary.log()
    except Exception as e:
        print(f"\n[ERROR] Preparation failed: {e}", file=sys.stderr)
        sys.exit(1)


def _run_train(
    config_path: str,
) -> None:
    from cqpes.pipeline.train import run_train
    from cqpes.types.train import TrainConfig

    try:
        print_header("TRAINING PIP-NN")

        config = TrainConfig.from_json(config_path)
        run_train(config)

    except Exception as e:
        print(f"\n[ERROR] Training failed: {e}", file=sys.stderr)
        sys.exit(1)


def _run_export(
    workdir_path: str,
    export_type: str,
) -> None:
    from cqpes.pipeline.export import run_export

    try:
        print_header("EXPORTING MODEL")
        run_export(workdir_path, export_type)  # type: ignore
    except Exception as e:
        print(f"\n[ERROR] Export failed: {e}", file=sys.stderr)
        sys.exit(1)


def _run_test(
    workdir_path: str,
) -> None:
    from cqpes.pipeline.test import run_test
    from cqpes.utils.logger import print_header

    try:
        print_header("MODEL EVALUATION")
        run_test(workdir_path)
    except Exception as e:
        print(f"\n[ERROR] Evaluation failed: {e}", file=sys.stderr)
        sys.exit(1)


def _run_predict(
    workdir_path: str,
    xyz_path: str,
    output_path: str,
    return_au: bool,
    calc_forces: bool,
    force_mode: Literal["analytical", "numerical"],
) -> None:
    from cqpes.pipeline.predict import run_predict

    try:
        print_header("POTENTIAL PREDICTION")
        run_predict(
            workdir_path,
            xyz_path,
            output_path,
            return_au,
            calc_forces,
            force_mode,
        )
    except Exception as e:
        print(f"\n[ERROR] Prediction failed: {e}", file=sys.stderr)
        sys.exit(1)


def _run_tasks(
    args,
) -> None:
    from cqpes.pipeline.run import run_task

    try:
        run_task(args)
    except Exception as e:
        print(f"\n[ERROR] Task excution failed: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
