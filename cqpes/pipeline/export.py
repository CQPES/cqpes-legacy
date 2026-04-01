import glob
import json
import os
import sys
from typing import Literal

import cqpes  # noqa: F401
import numpy as np
import tensorflow as tf
import tf_levenberg_marquardt as lm
from cqpes.types import TrainConfig
from cqpes.utils.model import build_network
from natsort import natsorted

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
    V_min: float,
    V_max: float,
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
            np.concatenate((p_max[1:], [V_max]))
            - np.concatenate((p_min[1:], [V_min]))
        )

        pavga = 0.5 * (
            np.concatenate((p_max[1:], [V_max]))
            + np.concatenate((p_min[1:], [V_min]))
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


def _model2keras(
    model: tf.keras.Model,
    output_dir: str,
) -> str:
    h5_path = os.path.join(output_dir, "model.h5")
    model.save(h5_path)

    return h5_path


def _model2jaxpip(
    model: tf.keras.Model,
    basis_file: str,
    alpha: float,
    output_dir: str,
    p_min: np.ndarray,
    p_max: np.ndarray,
    V_min: float,
    V_max: float,
) -> str:

    import jax

    jax.config.update("jax_enable_x64", True)

    import equinox as eqx
    from jax import numpy as jnp
    from jaxpip.descriptor import PolynomialDescriptor
    from jaxpip.model import FeatureScaler, PolynomialNeuralNetwork

    descriptor = PolynomialDescriptor.from_file(
        basis_file=basis_file,
        alpha=alpha,
        dtype=jnp.float64,
    )

    hidden_units = [layer.units for layer in model.layers[:-1]]

    skeleton = PolynomialNeuralNetwork(
        descriptor=descriptor,
        hidden_layers=hidden_units,
        key=jax.random.PRNGKey(0),
        activation=model.layers[0].get_config()["activation"],
    )

    new_model = skeleton

    linear_indices = [
        i
        for i, layer in enumerate(new_model.layers.layers)
        if isinstance(layer, eqx.nn.Linear)
    ]

    for k_idx, eqx_idx in enumerate(linear_indices):
        W_keras, b_keras = model.layers[k_idx].get_weights()

        # NOTE: Transpose required
        W_jax = jnp.array(W_keras.T, dtype=jnp.float64)
        b_jax = jnp.array(b_keras, dtype=jnp.float64)

        new_model = eqx.tree_at(
            where=lambda m, idx=eqx_idx: (
                m.layers.layers[idx].weight,
                m.layers.layers[idx].bias,
            ),
            pytree=new_model,
            replace=(W_jax, b_jax),
        )

    new_scaler = FeatureScaler(
        p_min=jnp.array(p_min[1:], dtype=jnp.float64),
        p_max=jnp.array(p_max[1:], dtype=jnp.float64),
        V_min=jnp.array(V_min, dtype=jnp.float64),
        V_max=jnp.array(V_max, dtype=jnp.float64),
    )

    new_model = eqx.tree_at(
        where=lambda m: m.scaler,
        pytree=new_model,
        replace=new_scaler,
    )

    eqx_path = os.path.join(output_dir, "jaxpip_network.eqx")
    new_model.save(eqx_path)

    return eqx_path


def run_export(
    workdir_path: str,
    export_type: Literal["h5", "potfit", "jaxpip"],
) -> None:
    workdir = os.path.abspath(workdir_path)

    # 1. workdir
    config_path = os.path.join(workdir, "train.json")
    ckpt_dir = os.path.join(workdir, "ckpt")

    # 2. load train config
    train_config = TrainConfig.from_json(config_path)

    # find jaxpip basis
    if export_type == "jaxpip":
        json_files = glob.glob(os.path.join(train_config.data, "*.json.gz"))

        if len(json_files) == 0:
            raise RuntimeError("Error: No JaxPIP basis json found")
        elif len(json_files) == 1:
            basis_file = json_files[0]
        elif len(json_files) > 1:
            raise RuntimeError(
                f"Error: Multiple JaxPIP basis json found: {json_files}"
            )

    # 3. load morse range parameter &  scale factor
    alpha = np.load(os.path.join(workdir, "alpha.npy")).item()

    phys_dict = {}

    try:
        phys_dict["p_min"] = np.load(os.path.join(workdir, "p_min.npy"))
        phys_dict["p_max"] = np.load(os.path.join(workdir, "p_max.npy"))
        phys_dict["V_min"] = np.load(os.path.join(workdir, "V_min.npy")).item()
        phys_dict["V_max"] = np.load(os.path.join(workdir, "V_max.npy")).item()
    except FileNotFoundError as e:
        print(
            f"\n[ERROR] Physical parameters missing in workdir: {e}",
            file=sys.stderr,
        )

        print("Did you forget to save .npy files during 'train'?")

        return

    # 4. auto detect checkpoint
    h5_files = natsorted(glob.glob(os.path.join(ckpt_dir, "*.weights.h5")))

    if not h5_files:
        print(
            f"\n[ERROR] No checkpoints found in {ckpt_dir}",
            file=sys.stderr,
        )
        print(
            "Did you start training?",
            file=sys.stderr,
        )
        return

    best_ckpt = h5_files[-1]

    # 5. build model
    input_dim = len(phys_dict["p_min"]) - 1
    model = build_network(train_config, input_dim=input_dim)
    model_wrapper = lm.model.ModelWrapper(model)  # type: ignore
    model_wrapper.build(input_shape=(1, input_dim))
    model_wrapper.load_weights(best_ckpt)

    # 6. export
    export_dir = os.path.join(workdir, "export")
    os.makedirs(export_dir, exist_ok=True)

    if export_type == "h5":
        model_h5 = _model2keras(model, export_dir)

        print(
            f"  [{'H5':^10}] Saved pure Keras model to: "
            f"{os.path.basename(model_h5)}"
        )
    elif export_type == "potfit":
        weights_path, biases_path = _model2potfit(
            model_wrapper.model,
            export_dir,
            **phys_dict,
        )

        print(
            f"  [{'POTFIT':^10}] Saved potfit model to "
            f"{os.path.basename(weights_path)} and "
            f"{os.path.basename(biases_path)}"
        )
    elif export_type == "jaxpip":
        model_eqx = _model2jaxpip(
            model,
            basis_file=basis_file,
            alpha=alpha,
            output_dir=export_dir,
            **phys_dict,
        )

        print(
            f"  [{'JAXPIP':^10}] Saved pure Equinox model to: "
            f"{os.path.basename(model_eqx)}"
        )

    meta_path = os.path.join(export_dir, "model_info.json")

    hidden_units = [layer.units for layer in model.layers[:-1]]

    with open(meta_path, "w") as f:
        json.dump(
            {
                "hidden_layers": hidden_units,
                "activation": model.layers[0].get_config()["activation"],
                "alpha": alpha,
                "feature_dim": model.input_shape[1],
            },
            f,
            indent=4,
        )

    print(f"  [{'DONE':^10}] Results saved to {export_dir}")
