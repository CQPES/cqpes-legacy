import os

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"

import levenberg_marquardt as lm
import numpy as np
import tensorflow as tf

tf.keras.backend.set_floatx("float64")

import argparse
import json

_ACT_MAP = {
    "tanh": 1,
    "sigmoid": 2,
    "linear": 0,
}


def _parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-c",
        "--config",
        default="export_model.json",
        type=str,
        help="Path to configuration file",
    )

    parser.add_argument(
        "-t",
        "--type",
        type=str,
        help="Model format (h5, potfit)",
        required=True,
    )

    args = parser.parse_args()

    return args


def load_scale(config):
    # load train config
    with open(config["train_config"]) as f:
        train_config = json.load(f)

    # load scale
    p_min = np.load(os.path.join(train_config["data"], "p_min.npy"))
    p_max = np.load(os.path.join(train_config["data"], "p_max.npy"))
    V_min = np.load(os.path.join(train_config["data"], "V_min.npy"))
    V_max = np.load(os.path.join(train_config["data"], "V_max.npy"))

    return (p_min, p_max, V_min, V_max)


def load_model(config):
    # load train config
    with open(config["train_config"]) as f:
        train_config = json.load(f)

    # build model
    model = tf.keras.Sequential()

    # add layers
    model.add(
        tf.keras.Input(
            shape=(len(p_min) - 1,),
        ),
    )

    for num_units in train_config["network"]["layers"]:
        model.add(
            tf.keras.layers.Dense(
                units=num_units,
                activation=train_config["network"]["activation"],
            ),
        )

    model.add(
        tf.keras.layers.Dense(
            units=1,
            activation="linear"
        )
    )

    # add wrapper
    model_wrapper = lm.ModelWrapper(model)
    model_wrapper.build(input_shape=(1, len(p_min) - 1))

    # load weights
    model_wrapper.load_weights(config["ckpt"])
    model_wrapper.model.compile()

    return model_wrapper


def model2potfit(
    model,
    output,
):
    weights_file = os.path.join(output, "weights.txt")
    biases_file = os.path.join(output, "biases.txt")

    # weights.txt
    with open(weights_file, "w") as f:
        # input shape, number of hidden layers, & output shape
        print(
            f"{model.input_shape[1]:8d}"
            f"{(len(model.layers) - 1):8d}"
            f"{model.output_shape[1]:8d}",
            file=f,
        )

        # shape of each hidden layer
        for (idx, layer) in enumerate(model.layers):
            if idx == (len(model.layers) - 1):
                print("", file=f)
                break

            units = layer.get_config()["units"]
            print(f"{units:8d}", end="", file=f)

        # type of activation functions & number of parameters
        print(
            f"{_ACT_MAP[model.layers[0].get_config()['activation']]:8d}"
            f"{model.count_params():8d}",
            file=f,
        )

        # scale
        pdela = 0.5 * (np.concatenate(
            (p_max[1:], V_max.reshape((1,))),
        ) - np.concatenate(
            (p_min[1:], V_min.reshape((1,))),
        ))

        pavga = 0.5 * (np.concatenate(
            (p_max[1:], V_max.reshape((1,))),
        ) + np.concatenate(
            (p_min[1:], V_min.reshape((1,))),
        ))

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


if __name__ == "__main__":
    args = _parse_args()

    if args.type not in ["h5", "potfit"]:
        print(
            f"Invalid model type {args.type}, only supports `h5` or `potfit`"
        )
        exit()

    # load config
    with open(args.config) as f:
        config = json.load(f)

    # load scale
    p_min, p_max, V_min, V_max = load_scale(config)

    # load model
    model_wrapper = load_model(config)

    # export model
    print(f"Exporting model in {args.type} format...")

    if args.type == "h5":
        h5_file = os.path.join(config["output"], "model.h5")
        model_wrapper.model.save(h5_file)

        print(f"h5 file saved to {h5_file}")
    elif args.type == "potfit":
        weights_file, biases_file = model2potfit(
            model_wrapper.model,
            config["output"],
        )

        print(f"weights file saved to {weights_file}")
        print(f"biases  file saved to {biases_file}")
    else:
        pass

    print("Model exported successfully!")
