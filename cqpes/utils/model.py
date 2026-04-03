import tensorflow as tf

import cqpes  # noqa: F401
from cqpes.types.train import TrainConfig


def build_network(
    config: TrainConfig,
    input_dim: int,
) -> tf.keras.Model:
    model = tf.keras.Sequential()
    model.add(tf.keras.Input(shape=(input_dim,)))

    for num_units in config.network.layers:
        model.add(
            tf.keras.layers.Dense(
                units=num_units,
                activation=config.network.activation,
            )
        )

    model.add(tf.keras.layers.Dense(units=1, activation="linear"))

    return model
