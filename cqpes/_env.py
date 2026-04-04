import os
import sys

__all__ = [
    "_setup_tensorflow",
    "_setup_jax",
]


def _setup_tensorflow(
    use_gpu: bool = False,
) -> None:
    # env
    os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"

    if "TF_USE_LEGACY_KERAS" not in os.environ:
        os.environ["TF_USE_LEGACY_KERAS"] = "1"

    if not use_gpu:
        os.environ["CUDA_VISIBLE_DEVICES"] = "-1"

    # keras
    try:
        import tf_keras

        sys.modules["keras"] = tf_keras
        sys.modules["tensorflow.keras"] = tf_keras
    except ImportError:
        pass

    # tensorflow
    import tensorflow as tf

    tf.get_logger().setLevel("ERROR")

    if not use_gpu:
        try:
            tf.config.set_visible_devices([], "GPU")
        except Exception:
            pass

    # precision
    if tf.keras.backend.floatx() != "float64":
        tf.keras.backend.set_floatx("float64")

    # register isru
    def _isru(x):
        return x / tf.sqrt(tf.square(x) + 1.0)

    if "isru" not in tf.keras.utils.get_custom_objects():
        tf.keras.utils.get_custom_objects().update({"isru": _isru})


def _setup_jax() -> None:
    if "XLA_PYTHON_CLIENT_PREALLOCATE" not in os.environ:
        os.environ["XLA_PYTHON_CLIENT_PREALLOCATE"] = "false"

    import jax

    # jax precision
    if not jax.config.read("jax_enable_x64"):
        jax.config.update("jax_enable_x64", True)
