import tensorflow as tf

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


def _activation_grad(activation, h):
    """d(act)/dx recovered from the activation output h itself."""
    name = getattr(activation, "__name__", "")

    if name == "tanh":
        return 1.0 - h * h

    if name == "sigmoid":
        return h * (1.0 - h)

    if name in ("isru", "_isru"):
        return (1.0 - h * h) ** 2

    raise ValueError(
        f"Activation '{name}' does not support analytic input gradients."
    )


class PhysicalResidualMetric(tf.keras.metrics.Metric):
    """Physical-unit error of one residual block, for the training log.

    `span` selects the block of the packed [y | F_norm] target vector
    (energy block: (0, 1), force block: (1, None)); `unit` converts scaled
    residual units to the display unit (meV, or meV/Angstrom); `reduction`
    is 'mae' or 'rmse' (computed over all residuals of the epoch).
    """

    def __init__(
        self,
        span,
        unit: float,
        name: str,
        reduction: str = "mae",
        **kwargs,
    ):
        super().__init__(name=name, **kwargs)

        if reduction not in ("mae", "rmse"):
            raise ValueError(f"Unknown reduction '{reduction}'.")

        self._span = span
        self._unit = tf.constant(
            float(unit),
            dtype=tf.keras.backend.floatx(),
        )
        self._squared = reduction == "rmse"

        self.total = self.add_weight(name="total", initializer="zeros")
        self.count = self.add_weight(name="count", initializer="zeros")

    def update_state(self, y_true, y_pred, sample_weight=None):
        s, e = self._span

        err = (y_true[:, s:e] - y_pred[:, s:e]) * self._unit

        err = tf.square(err) if self._squared else tf.abs(err)

        self.total.assign_add(tf.reduce_sum(err))
        self.count.assign_add(tf.cast(tf.size(err), err.dtype))

    def result(self):
        if self._squared:
            return tf.sqrt(self.total / self.count)

        return self.total / self.count


def forward_collect(
    net: tf.keras.Model,
    X: tf.Tensor,
):
    """Run a Sequential Dense net layer-by-layer, keeping hidden activations."""
    acts = []
    a = X

    for layer in net.layers:
        a = layer(a)
        acts.append(a)

    return acts


def input_grad_from_activations(
    net: tf.keras.Model,
    acts,
) -> tf.Tensor:
    """dy/dX as an explicit matmul chain: W1 diag(s1') W2 diag(s2') ... W_out.

    Pure differentiable ops (no tape), so an outer tape.jacobian stays a
    single-level differentiation. Requires >= 1 hidden layer.
    """
    kernels = [layer.kernel for layer in net.layers]

    g = tf.broadcast_to(
        tf.squeeze(kernels[-1], axis=-1),
        tf.shape(acts[-2]),
    )

    for i in range(len(kernels) - 2, -1, -1):
        g = g * _activation_grad(net.layers[i].activation, acts[i])
        g = g @ tf.transpose(kernels[i])

    return g


def analytic_input_grad(
    net: tf.keras.Model,
    X: tf.Tensor,
) -> tf.Tensor:
    return input_grad_from_activations(net, forward_collect(net, X))


class PIPNNForceModel(tf.keras.Model):
    """PIP-NN with an analytical force head for force-aided LM fitting.

    Input is packed as [X | flatten(dp/dxyz)], output as
    [y | sqrt(force_weight) * F_norm], where F_norm = -dV/dxyz expressed in
    whatever units `v_p_scale` encodes:

        dV/dxyz = (dV/dy) * (dy/dX) * (dX/dp) * (dp/dxyz)

    with dV/dy and dX/dp folded into the constant vector `v_p_scale`
    (shape (n_feat,), = (V_max - V_min) / (p_max - p_min)). Callers pass
    v_p_scale raw for physical eV/Angstrom forces, or divided by a norm
    (e.g. the force RMS) for normalized force residuals matching the
    training targets; dy/dX is the analytic matmul chain and dp/dxyz is
    per-sample constant input.
    """

    def __init__(
        self,
        network: tf.keras.Model,
        n_cart: int,
        v_p_scale,
        force_weight: float = 1.0,
    ) -> None:
        super().__init__()

        if len(network.layers) < 2:
            raise ValueError(
                "Force head requires at least one hidden layer."
            )

        self.network = network
        self.n_feat = int(network.inputs[0].shape[-1])
        self.n_cart = int(n_cart)
        self.force_weight = float(force_weight)

        self.v_p_scale = tf.constant(
            v_p_scale,
            dtype=network.dtype,
        )

    def call(self, inputs, training=None, mask=None):
        X = inputs[:, : self.n_feat]
        dpdx = tf.reshape(
            inputs[:, self.n_feat :],
            (-1, self.n_cart, self.n_feat),
        )

        acts = forward_collect(self.network, X)
        y = acts[-1]

        g = input_grad_from_activations(self.network, acts)

        forces = -tf.einsum("ni,nki->nk", g * self.v_p_scale, dpdx)

        force_scale = tf.sqrt(
            tf.constant(self.force_weight, dtype=inputs.dtype)
        )

        return tf.concat([y, force_scale * forces], axis=1)
