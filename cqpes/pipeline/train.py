import importlib.util
import inspect
import os
from typing import Callable, Dict, List

import numpy as np
from sklearn.model_selection import train_test_split

from cqpes.types import CQPESData, TrainConfig
from cqpes.utils.workspace import ExperimentWorkspace


def _load_weighting_func() -> Callable:
    def default_weighting(
        v: float,
    ) -> float:
        return 1.0

    weight_path = os.path.abspath("weighting.py")

    if not os.path.exists(weight_path):
        return default_weighting

    spec = importlib.util.spec_from_file_location(
        name="weighting_module",
        location=weight_path,
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


def _split_dataset(
    indices: np.ndarray,
    split_ratio: List[float],
) -> Dict[str, np.ndarray]:
    r_train, r_valid, r_test = split_ratio

    train_idx, valid_idx = train_test_split(indices, test_size=(1 - r_train))

    valid_idx, test_idx = train_test_split(
        valid_idx, test_size=(r_test / (1 - r_train))
    )

    return {
        "train": train_idx,
        "valid": valid_idx,
        "test": test_idx,
    }


def _save_indices(
    subset_idx_map: Dict[str, np.ndarray],
    workdir: str,
) -> None:
    for key, val in subset_idx_map.items():
        np.savetxt(
            os.path.join(workdir, f"{key}_idx.txt"),
            val,
            fmt="%d",
        )


def run_train(
    config: TrainConfig,
) -> None:
    # lazy import
    from cqpes._env import _setup_tensorflow

    _setup_tensorflow(use_gpu=True)

    import tensorflow as tf
    import tf_levenberg_marquardt as lm
    from tensorflow.keras.callbacks import ModelCheckpoint  # type: ignore
    from tensorflow.keras.callbacks import TensorBoard  # type: ignore

    from cqpes.utils.model import build_network

    # 1. create context
    workspace = ExperimentWorkspace.create(config.workdir)

    print(f"  [{'WORKDIR':^10}] {workspace.path}")

    weighting_func = _load_weighting_func()

    # 2. backup
    workspace.backup_artifacts(config.data, config)

    # 3. data load & split
    print(f"  [{'LOAD':^10}] Loading dataset from {config.data}...")

    dataset = CQPESData.from_dir(config.data)
    X, y, V = dataset.X[:, 1:], dataset.y, dataset.V

    indices = np.arange(len(X))
    subset_idx_map = _split_dataset(indices, config.split)
    _save_indices(subset_idx_map, workspace.path)

    # 4. weighting
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

    # 5. build network
    print(f"  [{'NETWORK':^10}] Constructing PIP-NN with LM Optimizer...")

    model = build_network(config, input_dim=X.shape[1])

    # lm optimizer
    model_wrapper = lm.model.ModelWrapper(model)  # type: ignore

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
    ckpt_dir = workspace.get_subpath("ckpt")
    log_dir = workspace.get_subpath("log")

    ckpt = ModelCheckpoint(
        filepath=os.path.join(
            ckpt_dir,
            "model_epoch_{epoch:04d}_val_mse_{val_wmse:.5e}.weights.h5",
        ),
        monitor="val_wmse",
        save_best_only=True,
        save_weights_only=True,
    )

    tensorboard = TensorBoard(log_dir)

    # 7. start
    batch_size = (
        len(subset_idx_map["train"])
        if config.fit.batch_size == -1
        else config.fit.batch_size
    )

    print(
        f"  [{'TRAIN':^10}] Starting LM training loop | "
        f"Epochs: {config.fit.epoch} | "
        f"Batch Size: {batch_size}"
    )

    print("-" * 80)

    model_wrapper.fit(
        X[subset_idx_map["train"]],
        y[subset_idx_map["train"]],
        batch_size=batch_size,
        epochs=config.fit.epoch,
        sample_weight=weights[subset_idx_map["train"]],
        validation_data=(
            X[subset_idx_map["valid"]],
            y[subset_idx_map["valid"]],
            weights[subset_idx_map["valid"]],
        ),
        callbacks=[tensorboard, ckpt],
        verbose=2,  # type: ignore
    )
