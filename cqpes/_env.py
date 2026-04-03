import os
import sys


def _setup_tensorflow() -> None:
    # keras
    try:
        import tf_keras

        sys.modules["keras"] = tf_keras
        sys.modules["tensorflow.keras"] = tf_keras
    except ImportError:
        pass

    if "TF_USE_LEGACY_KERAS" not in os.environ:
        os.environ["TF_USE_LEGACY_KERAS"] = "1"

    # tensorflow
    import tensorflow as tf

    # precision
    if tf.keras.backend.floatx() != "float64":
        tf.keras.backend.set_floatx("float64")

    # register isru
    def _isru(x):
        return x / tf.sqrt(tf.square(x) + 1.0)

    if "isru" not in tf.keras.utils.get_custom_objects():
        tf.keras.utils.get_custom_objects().update({"isru": _isru})


def _setup_jax() -> None:
    import jax

    # jax precision
    if not jax.config.read("jax_enable_x64"):
        jax.config.update("jax_enable_x64", True)


_setup_tensorflow()
_setup_jax()
