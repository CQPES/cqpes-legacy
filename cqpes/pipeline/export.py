import glob
import os
import sys
from typing import Literal

import numpy as np
import tensorflow as tf
import tf_levenberg_marquardt as lm
from natsort import natsorted

from cqpes.types import TrainConfig
from cqpes.utils.model import build_network

tf.keras.backend.set_floatx("float64")


def _isru(x):
    return x / tf.sqrt(tf.square(x) + 1.0)


tf.keras.utils.get_custom_objects().update({"isru": _isru})


_ACT_MAP = {
    "linear": 0,
    "tanh": 1,
    "sigmoid": 2,
    "isru": 3,
}


def _model2potfit(
    model: tf.keras.Model,
    output_dir: str,
    p_min: np.ndarray,
    p_max: np.ndarray,
    V_min: np.ndarray,
    V_max: np.ndarray,
) -> tuple[str, str]:
    weights_file = os.path.join(output_dir, "weights.txt")
    biases_file = os.path.join(output_dir, "biases.txt")

    # weights.txt
    with open(weights_file, "w") as f:
        print(
            f"{model.input_shape[1]:8d}"
            f"{(len(model.layers) - 1):8d}"
            f"{model.output_shape[1]:8d}",
            file=f,
        )

        for idx, layer in enumerate(model.layers):
            if idx == (len(model.layers) - 1):
                print("", file=f)

                break

            units = layer.get_config()["units"]

            print(f"{units:8d}", end="", file=f)

        act_name = model.layers[0].get_config()["activation"]
        act_id = _ACT_MAP.get(act_name)

        if act_id is None:
            raise ValueError(
                f"Activation '{act_name}' not supported for txt export."
            )

        print(f"{act_id:8d}{model.count_params():8d}", file=f)

        pdela = 0.5 * (
            np.concatenate((p_max[1:], V_max.reshape((1,))))
            - np.concatenate((p_min[1:], V_min.reshape((1,))))
        )

        pavga = 0.5 * (
            np.concatenate((p_max[1:], V_max.reshape((1,))))
            + np.concatenate((p_min[1:], V_min.reshape((1,))))
        )

        for x in pdela:
            print(f"{x:20.16f}\t", end="", file=f)

        print("", file=f)

        for x in pavga:
            print(f"{x:20.16f}\t", end="", file=f)

        print("", file=f)

        for layer in model.layers:
            W, _ = layer.get_weights()

            for w in W.T.flatten():
                print(f"{w:40.20f}", file=f)

    # bias.txt
    with open(biases_file, "w") as f:
        for layer in model.layers:
            _, b = layer.get_weights()

            for x in b:
                print(f"{x:40.20f}", file=f)

    return weights_file, biases_file


def run_export(
    export_type: Literal["h5", "potfit"],
    workdir_path: str,
) -> None:
    workdir = os.path.abspath(workdir_path)

    # 1. workdir
    config_path = os.path.join(workdir, "train.json")
    ckpt_dir = os.path.join(workdir, "ckpt")

    # 2. load train config
    train_config = TrainConfig.from_json(config_path)

    # 3. load scale factor
    try:
        p_min = np.load(os.path.join(workdir, "p_min.npy"))
        p_max = np.load(os.path.join(workdir, "p_max.npy"))
        V_min = np.load(os.path.join(workdir, "V_min.npy"))
        V_max = np.load(os.path.join(workdir, "V_max.npy"))
    except FileNotFoundError as e:
        print(
            f"\n[ERROR] Physical parameters missing in workdir: {e}",
            file=sys.stderr,
        )
        print("Did you forget to save .npy files during 'train'?")
        return

    # 4. auto detect checkpoint
    h5_files = natsorted(glob.glob(os.path.join(ckpt_dir, "*.h5")))
    best_ckpt = h5_files[-1]

    # 5. build model
    input_dim = len(p_min) - 1
    model = build_network(train_config, input_dim=input_dim)
    model_wrapper = lm.model.ModelWrapper(model)
    model_wrapper.build(input_shape=(1, input_dim))
    model_wrapper.load_weights(best_ckpt)

    # 6. export
    export_dir = os.path.join(workdir, "export")
    os.makedirs(export_dir, exist_ok=True)

    if export_type == "h5":
        h5_path = os.path.join(export_dir, "model.h5")
        model_wrapper.model.save(h5_path)
        print(
            f"  [{'H5':^10}] Saved pure Keras model to: "
            f"{os.path.basename(h5_path)}"
        )
    elif export_type == "potfit":
        weights_path, biases_path = _model2potfit(
            model_wrapper.model, export_dir, p_min, p_max, V_min, V_max
        )
        print(
            f"  [{'POTFIT':^10}] Saved potfit model to "
            f"{os.path.basename(weights_path)} and "
            f"{os.path.basename(biases_path)}"
        )

    print(f"  [{'DONE':^10}] Results saved to {export_dir}")
