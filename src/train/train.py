# autopep8: off

import os

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"

import sys

sys.path.append(os.path.join(os.path.dirname(__file__), os.pardir))

# autopep8: on

import argparse
import datetime
import inspect
import json

import levenberg_marquardt as lm
import numpy as np
import tensorflow as tf
from sklearn.model_selection import train_test_split
from tensorflow.keras.callbacks import ModelCheckpoint, TensorBoard

tf.keras.backend.set_floatx("float64")


from weighting import weighting

HEADER = r"""
                      _____ ____  _____  ______  _____                          
                     / ____/ __ \|  __ \|  ____|/ ____|                         
                    | |   | |  | | |__) | |__  | (___                           
                    | |   | |  | |  ___/|  __|  \___ \                          
                    | |___| |__| | |    | |____ ____) |                         
                     \_____\___\_\_|    |______|_____/                          
"""


def _parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-c",
        "--config",
        default="prepare.json",
        type=str,
        help="Path to configuration file",
    )

    args = parser.parse_args()

    return args


def _check_config(config):
    # data
    if not os.path.exists(config["data"]):
        raise FileNotFoundError(
            f"Data directory {config['data']} not found."
        )

def _print_header():
    print(f"{HEADER:^80}")
    print(f"{'CQPES: ChongQing Potential Energy Surface (legacy)':^80}")
    print()
    print(f"{'TRAIN':^80}")
    print("=" * 80)


if __name__ == "__main__":
    _print_header()

    args = _parse_args()

    with open(args.config) as f:
        config = json.load(f)

    _check_config(config)

    print(f"    - Data directory: {os.path.abspath(config['data'])}")

    # load data
    X = np.load(os.path.join(config["data"], "X.npy"))[:, 1:]
    y = np.load(os.path.join(config["data"], "y.npy"))
    V = np.load(os.path.join(config["data"], "V.npy"))

    print("-" * 80)

    print(f"    - Subset train: valid: test = {': '.join([f'{x:.2f}' for x in config['split']])}")
    
    print("-" * 80)

    workdir = os.path.abspath(f"{config['workdir']}-{datetime.datetime.now()}")
    os.makedirs(workdir)

    print(f"    - Workdir: {workdir}")

    index = np.arange(len(X)).astype(np.int32)
    train_idx, valid_idx = train_test_split(index, test_size=(1 - config['split'][0]))
    valid_idx, test_idx = train_test_split(valid_idx, test_size=(config['split'][2] / (1 - config['split'][0])))

    np.savetxt(os.path.join(workdir, "train_idx.txt"), train_idx, fmt="%d")
    np.savetxt(os.path.join(workdir, "valid_idx.txt"), valid_idx, fmt="%d")
    np.savetxt(os.path.join(workdir, "test_idx.txt"), test_idx, fmt="%d")

    print("-" * 80)

    print("    - Weighting function:\n")
    weighting_src = inspect.getsource(weighting)
    for line in weighting_src.split("\n"):
        print(f"    {line}")

    print("-" * 80)

    # assign weights
    weights = np.array([weighting(v) for v in V])

    print("    - Network Structure:\n")

    # build model
    model = tf.keras.Sequential()
    model.add(
        tf.keras.Input(
            shape=(len(X[0]),)
        ),
    )

    for num_units in config["network"]["layers"]:
        model.add(
            tf.keras.layers.Dense(
                units=num_units,
                activation=config["network"]["activation"],
            ),
        )

    model.add(
        tf.keras.layers.Dense(
            units=1,
            activation="linear"
        )
    )

    model.summary()

    print()
    print("-" * 80)

    # load fit parameters
    print(f"    - Learning rate: {config['fit']['lr']}")
    print(f"    - Epoch: {config['fit']['epoch']}")
    print(f"    - Batch size: {config['fit']['batch_size']}")

    print("-" * 80)

    # levenberg-marquardt
    print("    - Levenberg-Marquardt Optimizer")
    print(f"    - Adaptive scaling: {config['lm']['adaptive_scaling']}")
    print(f"    - Fletcher: {config['lm']['fletcher']}")
    print(f"    - Solve method: {config['lm']['solve_method']}")
    print(
        f"    - Jacobian max num rows: {config['lm']['jacobian_max_num_rows']}"
    )

    model_wrapper = lm.ModelWrapper(model)

    model_wrapper.compile(
        optimizer=tf.keras.optimizers.SGD(learning_rate=config['fit']['lr']),
        loss=lm.MeanSquaredError(),
        damping_algorithm=lm.DampingAlgorithm(
            adaptive_scaling=config['lm']['adaptive_scaling'],
            fletcher=config['lm']['solve_method'],
        ),
        solve_method=config['lm']['solve_method'],
        jacobian_max_num_rows=config['lm']['jacobian_max_num_rows'],
        metrics=[
            tf.keras.metrics.MeanSquaredError(),
        ],
        weighted_metrics=[
            tf.keras.metrics.MeanSquaredError(),
        ],
    )

    ckpt_dir = os.path.join(workdir, "ckpt")
    os.makedirs(ckpt_dir)

    log_dir = os.path.join(workdir, "log")
    os.makedirs(log_dir)

    ckpt = ModelCheckpoint(
        filepath=os.path.join(ckpt_dir, "model-epoch-{epoch:04d}-val-mse-{val_weighted_mean_squared_error:.5e}.h5"),
        monitor="val_weighted_mean_squared_error",
        save_best_only=True,
        save_weights_only=True,
    )

    tensorboard = TensorBoard(log_dir)

    # train
    if config['fit']['batch_size'] == -1:
        batch_size = len(train_idx)
    else:
        batch_size = config['fit']['batch_size']

    model_wrapper.fit(
        X[train_idx],
        y[train_idx],
        batch_size=batch_size,
        epochs=config['fit']['epoch'],
        sample_weight=weights[train_idx],
        validation_data=(
            X[valid_idx],
            y[valid_idx],
            weights[valid_idx],
        ),
        callbacks=[
            tensorboard,
            ckpt,
        ]
    )
