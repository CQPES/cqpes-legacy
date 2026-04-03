import os
from typing import Dict

import numpy as np
import pandas as pd
import scienceplots  # noqa: F401
import tf_levenberg_marquardt as lm
from matplotlib import pyplot as plt
from sklearn.metrics import mean_absolute_error, mean_squared_error

import cqpes  # noqa: F401
from cqpes.types import CQPESData, TrainConfig
from cqpes.utils.model import build_network
from cqpes.utils.train import find_best_checkpoint
from cqpes.utils.workspace import ExperimentWorkspace


def run_test(
    workdir_path: str,
) -> None:
    # 1. existing workspace
    workspace = ExperimentWorkspace.from_existing(workdir_path)
    eval_dir = workspace.get_subpath("eval")

    # 2. load metadata
    train_config = TrainConfig.from_json(
        os.path.join(
            workspace.path,
            "train.json",
        )
    )

    phys_dict = {
        k: np.load(os.path.join(workspace.path, f"{k}.npy"))
        for k in ["p_min", "p_max", "V_min", "V_max"]
    }

    subset_idx_map = {
        name: np.loadtxt(
            os.path.join(workspace.path, f"{name.lower()}_idx.txt"),
            dtype=np.int32,
        )
        for name in ["Train", "Valid", "Test"]
    }

    # 3. find best checkpoint
    best_ckpt_path, label = find_best_checkpoint(workspace.path)

    print(f"  [{'WORKDIR':^10}] {workspace.path}")
    print(f"  [{'MODEL':^10}] Target: {os.path.basename(best_ckpt_path)}")

    # 4. build network
    input_dim = len(phys_dict["p_min"]) - 1
    model = build_network(train_config, input_dim=input_dim)
    model_wrapper = lm.model.ModelWrapper(model)  # type: ignore
    model_wrapper.build(input_shape=(1, input_dim))
    model_wrapper.load_weights(best_ckpt_path)

    # 5. estimate error
    dataset = CQPESData.from_dir(train_config.data)
    X_scaled, V_true = dataset.X[:, 1:], dataset.V.reshape((-1, 1))
    y = model_wrapper(X_scaled, training=False).numpy()
    V_pred = CQPESData.unscale(y, phys_dict["V_min"], phys_dict["V_max"])

    errors_meV = (V_pred - V_true) * 1.0e03

    file_prefix = f"{os.path.basename(workspace.path)}_{label}"

    _export_metrics(V_true, V_pred, subset_idx_map, eval_dir, file_prefix)
    _plot_diagnostics(V_true, errors_meV, subset_idx_map, eval_dir, file_prefix)


def _export_metrics(
    V_true: np.ndarray,
    V_pred: np.ndarray,
    subset_idx_map: Dict[str, np.ndarray],
    output_dir: str,
    file_prefix: str,
) -> None:
    stats = []
    eval_indices = {**subset_idx_map, "Total": np.arange(len(V_true))}

    for name, idx in eval_indices.items():
        y_t, y_p = V_true[idx] * 1.0e03, V_pred[idx] * 1.0e03

        stats.append(
            {
                "Set": name,
                "MAE (meV)": mean_absolute_error(y_t, y_p),
                "RMSE (meV)": np.sqrt(mean_squared_error(y_t, y_p)),
                "MaxErr (meV)": np.abs(y_t - y_p).max(),
            }
        )

    df = pd.DataFrame(stats)
    csv_path = os.path.join(output_dir, f"{file_prefix}_metrics.csv")
    df.to_csv(csv_path, index=False)

    print(f"  [{'METRICS':^10}] Stats saved to: {csv_path}")
    print("\n" + df.to_string(index=False) + "\n")


def _plot_error_scatter(
    V_true,
    errors_meV,
    subset_idx_map: Dict[str, np.ndarray],
    output_dir: str,
    file_prefix: str,
) -> None:
    plot_path = os.path.join(output_dir, f"{file_prefix}_scatter.png")

    print(f"  [{'PLOT':^10}] Generating scatter plot...")

    colors = {"Train": "b", "Valid": "g", "Test": "r"}

    with plt.style.context(["science", "no-latex"]):
        fig, ax = plt.subplots(figsize=(8, 6), dpi=300)

        for name, idx in subset_idx_map.items():
            ax.scatter(
                V_true[idx],
                errors_meV[idx],
                c=colors[name],
                alpha=0.5,
                label=name,
                s=1.0,
            )

        ax.axhline(0, color="#c0392b", linestyle="--", linewidth=1.5)
        ax.set_xlabel(r"$\mathrm{Ab \ Initio \ Energy \ (eV)}$")
        ax.set_ylabel(r"$\mathrm{Error \ (meV)}$")

        ax.legend(loc="upper right", frameon=True)

        plt.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)

        print(f"  [{'SAVE':^10}] Scatter plot saved as: {plot_path}")


def _plot_error_dist(
    errors_meV: np.ndarray,
    output_dir: str,
    file_prefix: str,
) -> None:
    plot_path = os.path.join(output_dir, f"{file_prefix}_hist.png")

    print(f"  [{'PLOT':^10}] Generating histogram...")

    abs_err = np.abs(errors_meV).flatten()
    upper_bound = np.percentile(abs_err, 99.5)
    max_err = np.ceil(upper_bound)
    bin_width = 0.2 if max_err < 10 else 0.5
    edges = np.arange(0.0, max_err + bin_width, bin_width)

    with plt.style.context(["science", "no-latex"]):
        fig, ax = plt.subplots(figsize=(7, 5), dpi=300)

        weights = np.ones_like(abs_err) / len(abs_err)

        ax.hist(
            abs_err,
            bins=edges,  # type: ignore
            weights=weights,
            color="b",
            edgecolor="white",
            linewidth=0.8,
            rwidth=0.9,
        )

        ax.set_xlabel("Fitting Error (meV)", fontsize=12, fontweight="bold")
        ax.set_ylabel("Distribution", fontsize=12, fontweight="bold")

        mae = np.mean(abs_err).item()

        ax.axvline(
            mae,
            color="#e74c3c",
            linestyle="-",
            linewidth=1.5,
            label=f"MAE: {mae:.2f}",
        )

        plt.legend()

        plt.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)

        print(f"  [{'SAVE':^10}] Histogram saved as: {plot_path}")


def _plot_diagnostics(
    V_true: np.ndarray,
    errors_meV: np.ndarray,
    subset_idx_map: Dict[str, np.ndarray],
    output_dir: str,
    file_prefix: str,
) -> None:
    _plot_error_scatter(
        V_true=V_true,
        errors_meV=errors_meV,
        subset_idx_map=subset_idx_map,
        output_dir=output_dir,
        file_prefix=file_prefix,
    )

    _plot_error_dist(
        errors_meV=errors_meV,
        output_dir=output_dir,
        file_prefix=file_prefix,
    )
