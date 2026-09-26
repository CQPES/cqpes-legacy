import importlib.util
import inspect
import os
from typing import Callable, Dict, List
from typing import Optional

import numpy as np
from sklearn.model_selection import train_test_split

from cqpes.types import CQPESData, TrainConfig
from cqpes.utils.workspace import ExperimentWorkspace


def _load_weighting_func(
    weighting_path: Optional[str],
) -> Callable:
    def default_weighting(
        v: float,
    ) -> float:
        return 1.0

    if weighting_path is None:
        print(
            f"  [{'WEIGHT':^10}] Uniform weighting (no --weighting specified)"
        )
        print()

        return default_weighting

    weight_path = os.path.abspath(weighting_path)

    if not os.path.exists(weight_path):
        raise FileNotFoundError(
            f"[FATAL] Weighting file not found: {weight_path}"
        )

    spec = importlib.util.spec_from_file_location(
        name="weighting_module",
        location=weight_path,
    )

    if spec and spec.loader:
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)

        if hasattr(module, "weighting"):
            func = module.weighting
        else:
            raise AttributeError(
                f"[FATAL] Found custom weighting file at '{weight_path}', "
                "but it does NOT define the required 'weighting' function.\n"
                "Please define 'def weighting(v):' "
                "or remove the file to use defaults."
            )

        print(f"  [{'WEIGHT':^10}] Active weighting function:")
        print()

        for line in inspect.getsource(func).strip().split("\n"):
            print(f"             {line}")

        print()

        return func

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
    weighting: Optional[str] = None,
) -> None:
    # lazy import
    from cqpes._env import _setup_tensorflow

    _setup_tensorflow(use_gpu=True)

    import tensorflow as tf
    import tf_levenberg_marquardt as lm
    from tensorflow.keras.callbacks import ModelCheckpoint  # type: ignore
    from tensorflow.keras.callbacks import TensorBoard  # type: ignore

    from cqpes.utils.model import (
        PhysicalResidualMetric,
        PIPNNForceModel,
        build_network,
    )

    # 1. create context
    workspace = ExperimentWorkspace.create(config.workdir)

    print(f"  [{'WORKDIR':^10}] {workspace.path}")

    weighting_func = _load_weighting_func(weighting)

    # 2. backup
    workspace.backup_artifacts(config.data, config, weighting=weighting)

    # 3. data load & split
    print(f"  [{'LOAD':^10}] Loading dataset from {config.data}...")

    dataset = CQPESData.from_dir(config.data)
    X, y, V = dataset.X[:, 1:], dataset.y, dataset.V

    n_samples = len(X)

    indices = np.arange(n_samples)
    subset_idx_map = _split_dataset(indices, config.split)
    _save_indices(subset_idx_map, workspace.path)

    # 3.5 force-aided fitting context
    # residual layout: [y | sqrt(force_weight) * F / f_scale] per sample.
    # Both blocks are brought to O(1) (energy via min-max scaling, forces
    # via their own RMS), so force_weight is a dataset-independent relative
    # weight:  loss = MSE(dy) + force_weight * MSE(F / f_scale)
    # F/f_scale is assembled inside PIPNNForceModel by folding 1/f_scale
    # into v_p_scale; dp/dxyz is the constant packed input.
    has_forces = dataset.F is not None

    if has_forces:
        if dataset.dp is None:
            raise RuntimeError(
                f"[FATAL] Dataset has forces but no 'dp.npy' in {config.data}. "
                "Please re-run 'cqpes prepare' with the 'force' entry set."
            )

        force_weight = config.fit.force_weight

        # RMS of the ab initio forces; also the conversion between the
        # model's normalized force block and physical eV/Angstrom
        f_scale = float(np.sqrt(np.mean(np.square(dataset.F))))

        if f_scale < 1.0e-12:
            f_scale = 1.0

        n_cart = dataset.F.shape[1] * 3

        fit_x = np.concatenate(
            [
                X,
                dataset.dp[:, :, 1:].reshape((n_samples, -1)),
            ],
            axis=1,
        )

        fit_y = np.concatenate(
            [
                y.reshape((-1, 1)),
                np.sqrt(force_weight)
                * dataset.F.reshape((n_samples, -1))
                / f_scale,
            ],
            axis=1,
        )

        print(
            f"  [{'FORCE':^10}] Force-aided fitting enabled | "
            f"force_weight: {force_weight} | "
            f"force RMS: {f_scale:.4f} eV/A | "
            f"residuals per sample: {1 + n_cart}"
        )
    else:
        force_weight = config.fit.force_weight
        fit_x, fit_y = X, y

    # 4. weighting
    weights = np.fromiter(
        (weighting_func(v) for v in V),
        dtype=np.float64,
        count=len(V),
    )

    # 5. build network
    if has_forces:
        print(f"  [{'NETWORK':^10}] Constructing PIP-NN (forces + energy) with LM Optimizer...")

        v_p_scale = (dataset.V_max - dataset.V_min) / (
            dataset.p_max[1:] - dataset.p_min[1:]
        )

        model = PIPNNForceModel(
            network=build_network(config, input_dim=X.shape[1]),
            n_cart=n_cart,
            v_p_scale=v_p_scale / f_scale,
            force_weight=force_weight,
        )

        model.build(input_shape=(None, fit_x.shape[1]))
    else:
        print(f"  [{'NETWORK':^10}] Constructing PIP-NN with LM Optimizer...")

        model = build_network(config, input_dim=X.shape[1])

    # lm optimizer
    model_wrapper = lm.model.ModelWrapper(model)  # type: ignore

    # physical-unit progress metrics (the LM loss itself stays in scaled
    # units): dV = dy * s_v, and the force block is sqrt(w) * F / f_scale
    s_v = (dataset.V_max - dataset.V_min) / 2.0

    e_unit = s_v * 1.0e3  # meV per scaled unit

    metrics = [
        tf.keras.metrics.MeanSquaredError(name="mse"),
        PhysicalResidualMetric((0, 1), e_unit, "E_MAE(meV)"),
        PhysicalResidualMetric((0, 1), e_unit, "E_RMSE(meV)", reduction="rmse"),
    ]

    if has_forces:
        f_unit = f_scale / np.sqrt(force_weight) * 1.0e3  # meV/A per scaled unit

        metrics += [
            PhysicalResidualMetric((1, None), f_unit, "F_MAE(meV/A)"),
            PhysicalResidualMetric(
                (1, None), f_unit, "F_RMSE(meV/A)", reduction="rmse"
            ),
        ]

    model_wrapper.compile(
        optimizer=tf.keras.optimizers.SGD(learning_rate=config.fit.lr),
        loss=lm.loss.MeanSquaredError(),
        damping_algorithm=lm.damping.DampingAlgorithm(
            adaptive_scaling=config.lm.adaptive_scaling,
            fletcher=config.lm.fletcher,
        ),
        solve_method=config.lm.solve_method,
        jacobian_max_num_rows=config.lm.jacobian_max_num_rows,
        metrics=metrics,
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
        fit_x[subset_idx_map["train"]],
        fit_y[subset_idx_map["train"]],
        batch_size=batch_size,
        epochs=config.fit.epoch,
        sample_weight=weights[subset_idx_map["train"]],
        validation_data=(
            fit_x[subset_idx_map["valid"]],
            fit_y[subset_idx_map["valid"]],
            weights[subset_idx_map["valid"]],
        ),
        callbacks=[tensorboard, ckpt],
        verbose=2,  # type: ignore
    )
