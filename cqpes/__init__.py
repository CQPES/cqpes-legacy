import os

os.environ["TF_USE_LEGACY_KERAS"] = "1"


import tensorflow as tf

if tf.keras.backend.floatx() != "float64":
    tf.keras.backend.set_floatx("float64")


def _isru(x):
    return x / tf.sqrt(tf.square(x) + 1.0)


if "isru" not in tf.keras.utils.get_custom_objects():
    tf.keras.utils.get_custom_objects().update({"isru": _isru})

from .interface import CQPESCalculator, CQPESPot  # noqa: E402
from .types import CQPESData  # noqa: E402

__all__ = [
    "CQPESData",
    "CQPESPot",
    "CQPESCalculator",
]
