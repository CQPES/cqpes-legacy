import os

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"

import levenberg_marquardt as lm
import tensorflow as tf

tf.keras.backend.set_floatx("float64")

import argparse
import json


def _parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-c",
        "--config",
        default="export_model.json",
        type=str,
        help="Path to configuration file",
    )

    args = parser.parse_args()

    return args


if __name__ == "__main__":
    args = _parse_args()

    with open(args.config) as f:
        config = json.load(f)

    with open(config["train_config"]) as f:
        train_config = json.load(f)

    # build model
    model = tf.keras.Sequential()

    model.add(
        tf.keras.Input(
            shape=(config["input"],)
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

    model_wrapper = lm.ModelWrapper(model)
    model_wrapper.build(input_shape=(1, config["input"]))

    # load weights
    model_wrapper.load_weights(config["ckpt"])
    model_wrapper.model.compile()

    # export
    model_wrapper.model.save(config["output"])
