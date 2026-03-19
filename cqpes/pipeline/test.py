import glob
import os
import re

import numpy as np
import pandas as pd
import scienceplots  # noqa: F401
import tensorflow as tf
import tf_levenberg_marquardt as lm
from matplotlib import pyplot as plt
from natsort import natsorted
from sklearn.metrics import mean_absolute_error, mean_squared_error

from cqpes.types import CQPESData, TrainConfig
from cqpes.utils.model import build_network

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"

tf.config.set_visible_devices([], "GPU")
tf.keras.backend.set_floatx("float64")


def _isru(x):
    return x / tf.sqrt(tf.square(x) + 1.0)


tf.keras.utils.get_custom_objects().update({"isru": _isru})


def run_test(workdir_path: str):
    workdir = os.path.abspath(workdir_path)

    # load config
    train_config = TrainConfig.from_json(os.path.join(workdir, "train.json"))

    # load dataset
    dataset = CQPESData.from_dir(train_config.data)
    X_raw = dataset.X[:, 1:]
    V_true = dataset.V.reshape(-1, 1)

    params = {
        k: np.load(os.path.join(workdir, f"{k}.npy"))
        for k in ["p_min", "p_max", "V_min", "V_max"]
    }

    indices = {
        "Train": np.loadtxt(
            os.path.join(workdir, "train_idx.txt"), dtype=np.int32
        ),
        "Valid": np.loadtxt(
            os.path.join(workdir, "valid_idx.txt"), dtype=np.int32
        ),
        "Test": np.loadtxt(
            os.path.join(workdir, "test_idx.txt"), dtype=np.int32
        ),
    }

    # find checkpoint
    ckpt_dir = os.path.join(workdir, "ckpt")
    h5_files = natsorted(glob.glob(os.path.join(ckpt_dir, "*.h5")))

    if not h5_files:
        raise FileNotFoundError(f"[FATAL] No checkpoints found in {ckpt_dir}")

    best_ckpt_path = h5_files[-1]
    best_ckpt_name = os.path.basename(best_ckpt_path)

    match = re.search(r"epoch-[\w\d]+", best_ckpt_name)
    epoch_label = match.group(0) if match else "epoch-unknown"

    folder_name = os.path.basename(os.path.normpath(workdir))
    file_prefix = f"{folder_name}_{epoch_label}"

    print(f"  [{'WORKDIR':^10}] {workdir}")
    print(f"  [{'MODEL':^10}] Target: {best_ckpt_name}")

    # 4. build model
    model = build_network(train_config, input_dim=X_raw.shape[1])
    model_wrapper = lm.model.ModelWrapper(model)
    model_wrapper.build(input_shape=(1, X_raw.shape[1]))
    model_wrapper.load_weights(best_ckpt_path)

    # errors
    V_pred_norm = model_wrapper.predict(X_raw, batch_size=4096)
    V_pred = CQPESData.unscale(V_pred_norm, params["V_min"], params["V_max"])
    errors_meV = (V_pred - V_true) * 1000.0

    # 5. summary
    _export_metrics(V_true, V_pred, indices, file_prefix)
    _plot_error_scatter(V_true, errors_meV, indices, file_prefix)
    _plot_error_dist(errors_meV, file_prefix)


def _export_metrics(V_true, V_pred, indices: dict, file_prefix: str) -> None:
    csv_filename = f"{file_prefix}_metrics.csv"
    stats = []

    eval_indices = {**indices, "Total": np.arange(len(V_true))}

    for name, idx in eval_indices.items():
        y_t, y_p = V_true[idx] * 1000.0, V_pred[idx] * 1000.0
        stats.append(
            {
                "Set": name,
                "MAE (meV)": mean_absolute_error(y_t, y_p),
                "RMSE (meV)": np.sqrt(mean_squared_error(y_t, y_p)),
                "MaxErr (meV)": np.abs(y_t - y_p).max(),
            }
        )

    df = pd.DataFrame(stats)
    df.to_csv(csv_filename, index=False)
    print(f"  [{'METRICS':^10}] Stats saved to: {csv_filename}")
    print("\n" + df.to_string(index=False) + "\n")


def _plot_error_scatter(
    V_true, errors_meV, indices: dict, file_prefix: str
) -> None:
    plot_filename = f"{file_prefix}_scatter.png"
    print(f"  [{'PLOT':^10}] Generating scatter plot...")

    colors = {"Train": "b", "Valid": "g", "Test": "r"}

    with plt.style.context(["science", "no-latex"]):
        fig, ax = plt.subplots(figsize=(8, 6), dpi=300)

        for name, idx in indices.items():
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

        plt.savefig(plot_filename, bbox_inches="tight")
        plt.close(fig)

        print(f"  [{'SAVE':^10}] Scatter plot saved as: {plot_filename}")


def _plot_error_dist(errors_meV, file_prefix: str) -> None:
    plot_filename = f"{file_prefix}_hist.png"
    print(f"  [{'PLOT':^10}] Generating histogram...")

    abs_err = np.abs(errors_meV)
    max_err = np.ceil(abs_err.max() * 2) / 2
    bin_width = 0.5 if max_err < 25 else max_err / 50
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

        plt.savefig(plot_filename, bbox_inches="tight")
        plt.close(fig)

        print(f"  [{'SAVE':^10}] Histogram saved as: {plot_filename}")
