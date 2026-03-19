import datetime
import glob
import importlib.util
import inspect
import json
import os
import shutil
from dataclasses import asdict
from typing import Callable

import numpy as np
import tensorflow as tf
import tf_levenberg_marquardt as lm
from sklearn.model_selection import train_test_split
from tensorflow.keras.callbacks import ModelCheckpoint  # type: ignore
from tensorflow.keras.callbacks import TensorBoard

from cqpes.types import CQPESData, TrainConfig
from cqpes.utils.model import build_network

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "2"
tf.keras.backend.set_floatx("float64")


def _isru(x):
    return x / tf.sqrt(tf.square(x) + 1.0)


tf.keras.utils.get_custom_objects().update({"isru": _isru})


def _load_weighting_func() -> Callable:
    def default_weighting(
        v: float,
    ) -> float:
        return 1.0

    weight_path = os.path.abspath("weighting.py")

    if not os.path.exists(weight_path):
        return default_weighting

    spec = importlib.util.spec_from_file_location(
        "weighting_module", weight_path
    )

    if spec and spec.loader:
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)

        if hasattr(module, "weighting"):
            return module.weighting
        else:
            raise AttributeError(
                f"[FATAL] Found custom weighting file at '{weight_path}', "
                "but it does NOT define the required 'weighting' function.\n"
                "Please define 'def weighting(v):' "
                "or remove the file to use defaults."
            )

    raise ImportError(f"[FATAL] Failed to load module from '{weight_path}'.")


def run_train(
    config: TrainConfig,
) -> None:
    WIDTH = 80

    # 1. workdir
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    run_workdir = f"{config.workdir}_{timestamp}"
    os.makedirs(run_workdir, exist_ok=True)

    print(f"  [{'WORKDIR':^10}] {run_workdir}")

    config_snapshot = os.path.join(run_workdir, "train.json")

    with open(config_snapshot, "w") as f:
        json.dump(asdict(config), f, indent=4)

    msa_files = glob.glob(os.path.join(config.data, "*.so"))

    for msa_file in msa_files:
        shutil.copy2(
            src=msa_file,
            dst=os.path.join(run_workdir, os.path.basename(msa_file)),
        )

    weight_src = os.path.abspath("weighting.py")

    if os.path.exists(weight_src):
        shutil.copy(weight_src, os.path.join(run_workdir, "weighting.py"))

    # 2. load data & split
    print(f"  [{'LOAD':^10}] Loading CQPES dataset from {config.data}...")

    dataset = CQPESData.from_dir(config.data)

    X = dataset.X[:, 1:]
    y = dataset.y
    V = dataset.V

    artifacts = [
        "alpha.npy",
        "ref_energy.npy",
        "p_min.npy",
        "p_max.npy",
        "V_min.npy",
        "V_max.npy",
    ]

    for item in artifacts:
        src = os.path.join(config.data, item)
        if os.path.exists(src):
            shutil.copy2(src, run_workdir)

    index = np.arange(len(X)).astype(np.int32)
    r_train, r_val, r_test = config.split

    train_idx, valid_idx = train_test_split(index, test_size=(1 - r_train))
    valid_idx, test_idx = train_test_split(
        valid_idx, test_size=(r_test / (1 - r_train))
    )

    np.savetxt(
        os.path.join(run_workdir, "train_idx.txt"),
        train_idx,
        fmt="%d",
    )

    np.savetxt(
        os.path.join(run_workdir, "valid_idx.txt"),
        valid_idx,
        fmt="%d",
    )

    np.savetxt(
        os.path.join(run_workdir, "test_idx.txt"),
        test_idx,
        fmt="%d",
    )

    print(
        f"  [{'SPLIT':^10}] Train: {len(train_idx)} | "
        f"Valid: {len(valid_idx)} | "
        f"Test: {len(test_idx)}"
    )

    # 3. weighting
    weighting_func = _load_weighting_func()

    print(f"  [{'WEIGHT':^10}] Active weighting function:")
    print("\n")

    for line in inspect.getsource(weighting_func).strip().split("\n"):
        print(f"             {line}")

    print("\n")

    # for better performance
    weights = np.fromiter(
        (weighting_func(v) for v in V),
        dtype=np.float64,
        count=len(V),
    )

    # 4. build nn
    print(f"  [{'NETWORK':^10}] Constructing PIP-NN with LM Optimizer...")

    model = build_network(config, input_dim=X.shape[1])

    # 5. lm optimizer
    model_wrapper = lm.model.ModelWrapper(model)
    model_wrapper.compile(
        optimizer=tf.keras.optimizers.SGD(learning_rate=config.fit.lr),
        loss=lm.loss.MeanSquaredError(),
        damping_algorithm=lm.damping.DampingAlgorithm(
            adaptive_scaling=config.lm.adaptive_scaling,
            fletcher=config.lm.fletcher,
        ),
        solve_method=config.lm.solve_method,
        jacobian_max_num_rows=config.lm.jacobian_max_num_rows,
        metrics=[tf.keras.metrics.MeanSquaredError(name="mse")],
        weighted_metrics=[tf.keras.metrics.MeanSquaredError(name="wmse")],
    )

    # 6. config callbacks
    ckpt_dir = os.path.join(run_workdir, "ckpt")
    log_dir = os.path.join(run_workdir, "log")
    os.makedirs(ckpt_dir, exist_ok=True)
    os.makedirs(log_dir, exist_ok=True)

    ckpt = ModelCheckpoint(
        filepath=os.path.join(
            ckpt_dir,
            "model_epoch_{epoch:04d}_val_mse_{val_wmse:.5e}.h5",
        ),
        monitor="val_wmse",
        save_best_only=True,
        save_weights_only=True,
    )

    tensorboard = TensorBoard(log_dir)

    # 7. start
    batch_size = (
        len(train_idx) if config.fit.batch_size == -1 else config.fit.batch_size
    )

    print(
        f"  [{'TRAIN':^10}] Starting LM training loop | "
        f"Epochs: {config.fit.epoch} | "
        f"Batch Size: {batch_size}"
    )

    print("-" * WIDTH)

    model_wrapper.fit(
        X[train_idx],
        y[train_idx],
        batch_size=batch_size,
        epochs=config.fit.epoch,
        sample_weight=weights[train_idx],
        validation_data=(X[valid_idx], y[valid_idx], weights[valid_idx]),
        callbacks=[tensorboard, ckpt],
        verbose=2,  # type: ignore
    )
