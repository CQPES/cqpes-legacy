import argparse
import os
import sys
import warnings

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"

try:
    import absl.logging

    absl.logging.set_verbosity(absl.logging.ERROR)

    import tensorflow as tf

    tf.get_logger().setLevel("ERROR")
except ImportError:
    pass

import argcomplete

from cqpes.utils.logger import custom_warning, print_header

warnings.showwarning = custom_warning

warnings.filterwarnings("ignore", message=".*HDF5 file.*")
warnings.filterwarnings("ignore", message=".*native Keras format.*")


def main() -> None:
    parser = argparse.ArgumentParser(
        prog="cqpes",
        description="CQPES: GPU-Aided Potential Energy Surface Development Toolkit",
    )

    subparsers = parser.add_subparsers(
        dest="command", required=True, help="Sub-commands"
    )

    prepare_parser = subparsers.add_parser(
        "prepare", help="Prepare dataset from raw files"
    )

    prepare_parser.add_argument(
        "--msa",
        required=True,
        help="Path to compiled MSA .so module",
    )

    prepare_parser.add_argument("config", help="Path to prepare.json")

    train_parser = subparsers.add_parser(
        "train", help="Train PIP-NN model via TensorFlow"
    )

    train_parser.add_argument("config", help="Path to train.json")

    test_parser = subparsers.add_parser(
        "test", help="Evaluate model performance and plot errors"
    )

    test_parser.add_argument("workdir", help="Path to the experiment workdir")

    export_parser = subparsers.add_parser(
        "export", help="Export trained model for dynamics interfaces"
    )

    export_parser.add_argument(
        "-t",
        "--type",
        choices=["h5", "potfit"],
        default="h5",
        help="Export format",
    )

    export_parser.add_argument("workdir", help="Path to the experiment workdir")

    predict_parser = subparsers.add_parser(
        "predict", help="Predict energies for a given XYZ trajectory"
    )

    predict_parser.add_argument(
        "workdir", help="Path to the experiment workdir"
    )

    predict_parser.add_argument(
        "--no-forces",
        action="store_false",
        dest="calc_forces",
        help="Disable force calculation (Energy only)",
    )

    predict_parser.add_argument(
        "--mode",
        choices=["analytical", "numerical"],
        default="analytical",
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
        dest="return_au",
        help=(
            "Output in Atomic Units "
            "(Energy: Hartree, Coords: Bohr, Forces: Hartree/Bohr)"
        ),
    )

    predict_parser.add_argument("xyz", help="Path to the target .xyz file")

    argcomplete.autocomplete(parser)

    args = parser.parse_args()

    if args.command == "prepare":
        _run_prepare(args.msa, args.config)
    elif args.command == "train":
        _run_train(args.config)
    elif args.command == "test":
        _run_test(args.workdir)
    elif args.command == "export":
        _run_export(args.type, args.workdir)
    elif args.command == "predict":
        _run_predict(args.workdir, args.xyz, args.output, args.return_au)


def _run_prepare(
    msa_path: str,
    config_path: str,
) -> None:
    from cqpes.pipeline.prepare import run_prepare
    from cqpes.types.prepare import PrepareConfig

    try:
        config = PrepareConfig.from_json(config_path)
        summary = run_prepare(msa_path, config)
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
    export_type: str,
    workdir_path: str,
) -> None:
    from cqpes.pipeline.export import run_export

    try:
        print_header("EXPORTING MODEL")
        run_export(export_type, workdir_path)  # type: ignore
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
) -> None:
    from cqpes.pipeline.predict import run_predict

    try:
        print_header("POTENTIAL PREDICTION")
        run_predict(workdir_path, xyz_path, output_path, return_au)
    except Exception as e:
        print(f"\n[ERROR] Prediction failed: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
