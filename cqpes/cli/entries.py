import argparse
import argcomplete
import sys

from cqpes.utils.logger import print_header


def main():
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

    export_parser.add_argument("config", help="Path to export.json")

    export_parser.add_argument(
        "-t",
        "--type",
        choices=["h5", "potfit"],
        default="h5",
        help="Export format",
    )

    argcomplete.autocomplete(parser)

    args = parser.parse_args()

    if args.command == "prepare":
        _run_prepare(args.config)
    elif args.command == "train":
        _run_train(args.config)
    elif args.command == "test":
        _run_test(args.workdir)
    elif args.command == "export":
        _run_export(args.config, args.type)


def _run_prepare(config_path: str):
    from cqpes.module.prepare import run_prepare
    from cqpes.types.prepare import PrepareConfig

    try:
        config = PrepareConfig.from_json(config_path)
        summary = run_prepare(config)
        summary.log()
    except Exception as e:
        print(f"\n[ERROR] Preparation failed: {e}", file=sys.stderr)
        sys.exit(1)


def _run_train(config_path: str):
    from cqpes.module.train import run_train
    from cqpes.types.train import TrainConfig

    try:
        print_header("TRAINING PIP-NN")

        config = TrainConfig.from_json(config_path)
        run_train(config)

    except Exception as e:
        print(f"\n[ERROR] Training failed: {e}", file=sys.stderr)
        sys.exit(1)


def _run_export(workdir_path: str, export_type: str):
    from cqpes.module.export import run_export

    try:
        print_header("EXPORTING MODEL")
        run_export(workdir_path, export_type)
    except Exception as e:
        print(f"\n[ERROR] Export failed: {e}", file=sys.stderr)
        sys.exit(1)


def _run_test(workdir_path: str):
    from cqpes.module.test import run_test
    from cqpes.utils.logger import print_header

    try:
        print_header("MODEL EVALUATION")
        run_test(workdir_path)
    except Exception as e:
        print(f"\n[ERROR] Evaluation failed: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
