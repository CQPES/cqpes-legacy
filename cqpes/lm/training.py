import keras
import tensorflow as tf
from tensorflow import Tensor
from tensorflow.python.keras.engine import compile_utils, data_adapter

from .damping import DampingAlgorithm
from .loss import Loss, MeanSquaredError


class Trainer:
    """Levenberg-Marquardt training algorithm."""

    def __init__(
        self,
        model: keras.Model,
        optimizer=keras.optimizers.SGD(learning_rate=1.0),
        loss: Loss = MeanSquaredError(),
        damping_algorithm=DampingAlgorithm(),
        attempts_per_step: int = 10,
        solve_method: str = 'qr',
        jacobian_max_num_rows: int = 100,
        experimental_use_pfor: bool = True,
        step_control: str = 'classic',
        eta_acc: float = 0.1,
        max_t_retries: int = 3,
        beta_min: float = 0.3,
        beta_max: float = 0.5,
    ):
        """Initializes `Trainer` instance.

        Args:
          model: It is the Model to be trained, expected to inherit from keras.Model and
            already built.
          optimizer: (Optional) Performs the update of the model trainable variables.
            When keras.optimizers.SGD is used, it is equivalent to the operation
            `w = w - learning_rate * updates`, where updates is the step computed using
            the Levenberg-Marquardt algorithm.
          loss: (Optional) An object which inherits from keras.losses.Loss and has an
            additional function to compute residuals.
          damping_algorithm: (Optional) Class implementing the damping algorithm to
            use during training.
          attempts_per_step: Integer (Optional) During the train step when new model
            variables are computed, the new loss is evaluated and compared with the old
            loss value. If new_loss < loss, then the new variables are accepted;
            otherwise, the old variables are restored and new ones are computed using
            a different damping factor. This argument represents the maximum number of
            attempts, after which the step is taken.
          solve_method: (Optional) Possible values are:
            'qr': Uses QR decomposition which is robust but slower.
            'cholesky': Uses Cholesky decomposition which is fast but may fail when the
              Hessian approximation is ill-conditioned.
            'solve': Uses tf.linalg.solve. It seems a compromise in terms of speed and
              robustness.
          jacobian_max_num_rows: Integer (Optional) When the number of residuals is
            greater than the number of variables (overdetermined), the Hessian
            approximation is computed by slicing the input and accumulating the result
            of each computation. This reduces memory usage and increases speed. The
            input is sliced into blocks of size less than or equal to
            jacobian_max_num_rows.
          experimental_use_pfor: (Optional) If True, vectorizes the Jacobian
            computation. Else falls back to a sequential while_loop. Vectorization can
            sometimes fail or lead to excessive memory usage. This option can be used
            to disable vectorization in such cases.
          step_control: (Optional) Step acceptance controller:
            'classic': original strict-decrease with damping retry (upstream behavior).
            'trust_ratio': trust-ratio acceptance (Eq. 34) with curve-length
              backtracking along the fixed LM direction (Eq. 37) before any damping
              change; damping update follows Eq. (40) of RNC-LM (arXiv:2607.07623).
          eta_acc: (Optional) Acceptance threshold for the trust ratio rho in
            'trust_ratio' mode.
          max_t_retries: (Optional) Maximum number of curve-length (t) backtracking
            trials per constructed direction before the damping factor is increased
            and the direction is recomputed.
          beta_min, beta_max: (Optional) Bounds for the interpolated t contraction,
            t_{j+1} in [beta_min * t_j, beta_max * t_j] (Eq. 38).
        """
        if step_control not in ('classic', 'trust_ratio'):
            raise ValueError(
                "Invalid step_control, expected 'classic' or 'trust_ratio'."
            )
        if not model.built:
            raise ValueError(
                'Trainer model has not yet been built. Build the model first by calling'
                ' `build()` or `fit()` with some data, or specify an `input_shape` '
                'argument in the first layer(s) for automatic build.'
            )

        self.model = model
        self.loss = loss
        self.optimizer = optimizer
        self.damping_algorithm = damping_algorithm
        self.attempts_per_step = attempts_per_step
        self.jacobian_max_num_rows = jacobian_max_num_rows
        self.experimental_use_pfor = experimental_use_pfor
        self.step_control = step_control
        self.eta_acc = eta_acc
        self.max_t_retries = max_t_retries
        self.beta_min = beta_min
        self.beta_max = beta_max
        # Accepted curve length of the last train step ('trust_ratio' mode only);
        # exposed for logging.
        self.last_t_acc = None

        # Define and select linear system equation solver.
        def qr(matrix: Tensor, rhs: Tensor) -> Tensor:
            q, r = tf.linalg.qr(matrix, full_matrices=True)
            y = tf.linalg.matmul(q, rhs, transpose_a=True)
            return tf.linalg.triangular_solve(r, y, lower=False)

        def cholesky(matrix: Tensor, rhs: Tensor) -> Tensor:
            chol = tf.linalg.cholesky(matrix)
            return tf.linalg.cholesky_solve(chol, rhs)

        def solve(matrix: Tensor, rhs: Tensor) -> Tensor:
            return tf.linalg.solve(matrix, rhs)

        if solve_method == 'qr':
            self.solve_function = qr
        elif solve_method == 'cholesky':
            self.solve_function = cholesky
        elif solve_method == 'solve':
            self.solve_function = solve
        else:
            raise ValueError('Invalid solve_method.')

        # Keep track of the current damping_factor.
        self.damping_factor = tf.Variable(
            self.damping_algorithm.starting_value,
            trainable=False,
            dtype=self.model.dtype,
        )

        # Used to backup and restore model variables.
        self._backup_variables = []

        # Since training updates are computed with shape (num_variables, 1),
        # self._splits and self._shapes are needed to split and reshape the updates so
        # that they can be applied to the model trainable_variables.
        self._splits = []
        self._shapes = []

        for variable in self.model.trainable_variables:
            variable_shape = tf.shape(variable)
            variable_size = tf.reduce_prod(variable_shape)
            backup_variable = tf.Variable(
                tf.zeros_like(variable),
                trainable=False,
            )

            self._backup_variables.append(backup_variable)
            self._splits.append(variable_size)
            self._shapes.append(variable_shape)

        self._num_variables = tf.reduce_sum(self._splits).numpy().item()
        self._num_outputs = None

    @tf.function
    def _compute_jacobian(self, inputs: Tensor, targets: Tensor):
        with tf.GradientTape(persistent=True) as tape:
            outputs = self.model(inputs, training=True)
            targets, outputs, _ = compile_utils.match_dtype_and_rank(
                targets, outputs, None
            )
            residuals = self.loss.residuals(targets, outputs)

        jacobians = tape.jacobian(
            residuals,
            self.model.trainable_variables,
            experimental_use_pfor=self.experimental_use_pfor,
            unconnected_gradients=tf.UnconnectedGradients.ZERO,
        )

        del tape

        assert jacobians

        num_residuals = tf.size(residuals)
        jacobians = [tf.reshape(j, (num_residuals, -1)) for j in jacobians]
        jacobian = tf.concat(jacobians, axis=1)
        residuals = tf.reshape(residuals, (num_residuals, -1))

        return jacobian, residuals, outputs

    def _init_gauss_newton_overdetermined(self, inputs: Tensor, targets: Tensor):
        # Perform the following computation:
        # J, residuals, outputs = self._compute_jacobian(inputs, targets)
        # JJ = tf.linalg.matmul(J, J, transpose_a=True)
        # rhs = tf.linalg.matmul(J, residuals, transpose_a=True)
        #
        # But reduce memory usage by slicing the inputs so that the Jacobian matrix
        # will have maximum shape (jacobian_max_num_rows, num_variables) instead of
        # (batch_size, num_variables).
        assert self._num_outputs
        slice_size = self.jacobian_max_num_rows // self._num_outputs
        batch_size = tf.shape(inputs)[0]
        num_slices = batch_size // slice_size
        remainder = batch_size % slice_size

        JJ = tf.zeros(
            [self._num_variables, self._num_variables], dtype=self.model.dtype
        )

        rhs = tf.zeros([self._num_variables, 1], dtype=self.model.dtype)

        outputs_array = tf.TensorArray(self.model.dtype, size=0, dynamic_size=True)

        for i in tf.range(num_slices):
            tf.autograph.experimental.set_loop_options(
                shape_invariants=[(rhs, tf.TensorShape((self._num_variables, None)))]
            )

            _inputs = inputs[i * slice_size : (i + 1) * slice_size]
            _targets = targets[i * slice_size : (i + 1) * slice_size]

            J, residuals, _outputs = self._compute_jacobian(_inputs, _targets)

            outputs_array = outputs_array.write(i, _outputs)

            JJ += tf.linalg.matmul(J, J, transpose_a=True)
            rhs += tf.linalg.matmul(J, residuals, transpose_a=True)

        if remainder > 0:
            _inputs = inputs[num_slices * slice_size :]
            _targets = targets[num_slices * slice_size :]

            J, residuals, _outputs = self._compute_jacobian(_inputs, _targets)

            if num_slices > 0:
                outputs = tf.concat([outputs_array.concat(), _outputs], axis=0)
            else:
                outputs = _outputs

            JJ += tf.linalg.matmul(J, J, transpose_a=True)
            rhs += tf.linalg.matmul(J, residuals, transpose_a=True)
        else:
            outputs = outputs_array.concat()

        return 0.0, JJ, rhs, outputs

    def _init_gauss_newton_underdetermined(self, inputs, targets):
        J, residuals, outputs = self._compute_jacobian(inputs, targets)
        JJ = tf.linalg.matmul(J, J, transpose_b=True)
        rhs = residuals
        return J, JJ, rhs, outputs

    def _compute_gauss_newton_overdetermined(
        self, J: Tensor, JJ: Tensor, rhs: Tensor
    ) -> Tensor:
        updates = self.solve_function(JJ, rhs)
        return updates

    def _compute_gauss_newton_underdetermined(
        self, J: Tensor, JJ: Tensor, rhs: Tensor
    ) -> Tensor:
        updates = self.solve_function(JJ, rhs)
        updates = tf.linalg.matmul(J, updates, transpose_a=True)
        return updates

    def _train_step(
        self,
        inputs: Tensor,
        targets: Tensor,
        init_gauss_newton,
        compute_gauss_newton,
    ):
        # J: Jacobian matrix not used in the overdetermined case.
        # JJ: Gauss-Newton Hessian approximation
        # rhs: gradient when overdetermined, residuals when underdetermined.
        # outputs: prediction of the model for the current inputs.
        J, JJ, rhs, outputs = init_gauss_newton(inputs, targets)

        # Perform normalization for numerical stability.
        batch_size = tf.shape(inputs)[0]
        normalization_factor = 1.0 / tf.dtypes.cast(batch_size, dtype=self.model.dtype)

        JJ *= normalization_factor
        rhs *= normalization_factor

        # Compute the current loss value.
        loss = self.loss(targets, outputs)

        stop_training = False
        attempt = 0
        damping_factor = self.damping_algorithm.init_step(self.damping_factor, loss)

        attempts = tf.constant(self.attempts_per_step, dtype=tf.int32)

        while tf.constant(True, dtype=tf.bool):
            update_computed = False
            try:
                # Apply the damping to the Gauss-Newton Hessian approximation.
                JJ_damped = self.damping_algorithm.apply(damping_factor, JJ)

                # Compute the updates:
                # overdetermined: updates = (J'*J + damping)^-1*J'*residuals
                # underdetermined: updates = J'*(J*J' + damping)^-1*residuals
                updates = compute_gauss_newton(J, JJ_damped, rhs)
            except Exception as e:
                print(f'An error occurred: {e}')
            else:
                if tf.reduce_all(tf.math.is_finite(updates)):
                    update_computed = True
                    # Split and reshape the updates
                    updates = tf.split(tf.squeeze(updates, axis=-1), self._splits)
                    updates = [
                        tf.reshape(update, shape)
                        for update, shape in zip(updates, self._shapes)
                    ]

                    # Apply the updates to the model trainable_variables.
                    self.optimizer.apply_gradients(
                        zip(updates, self.model.trainable_variables)
                    )

            if attempt < attempts:
                attempt += 1

                if update_computed:
                    # Compute the new loss value.
                    outputs = self.model(inputs, training=False)
                    new_loss = self.loss(targets, outputs)

                    if new_loss < loss:
                        # Accept the new model variables and backup them.
                        loss = new_loss
                        damping_factor = self.damping_algorithm.decrease(
                            damping_factor, loss
                        )
                        self.backup_variables()
                        break

                    # Restore the old variables and try a new damping_factor.
                    self.restore_variables()

                damping_factor = self.damping_algorithm.increase(damping_factor, loss)

                stop_training = self.damping_algorithm.stop_training(
                    damping_factor, loss
                )
                if stop_training:
                    break
            else:
                break

        # Update the damping_factor for the next train_step.
        self.damping_factor.assign(damping_factor)
        return loss, outputs, attempt, stop_training

    def _train_step_tr(
        self,
        inputs: Tensor,
        targets: Tensor,
        init_gauss_newton,
        compute_gauss_newton,
        overdetermined: bool,
    ):
        """Train step with trust-ratio damping control and curve-length backtracking.

        Follows Sec. 4.4 of RNC-LM (arXiv:2607.07623) adapted to K = 1: the damped
        LM direction is realized as a curve theta(t) = theta_p + t * (-lr * updates),
        t in (0, 1]. Any trial that actually decreases the loss is accepted; on
        rejection the curve length is first shortened by quadratic interpolation
        (Eq. 37-38), reusing the same linear solve, and only when the curve is
        exhausted is the damping factor increased and the direction recomputed.
        The accepted trust ratio rho (Eq. 34) drives the damping update (Eq. 40):
        rho < 0.25 -> 2*lam, rho > 0.75 -> lam/3. The K = 1 'classic' path is
        unchanged.
        """
        # J: Jacobian matrix (only used in the underdetermined case).
        # JJ: Gauss-Newton Hessian approximation, rhs: normalized J'r.
        J, JJ, rhs, outputs = init_gauss_newton(inputs, targets)

        # Perform normalization for numerical stability.
        batch_size = tf.shape(inputs)[0]
        normalization_factor = 1.0 / tf.dtypes.cast(batch_size, dtype=self.model.dtype)

        JJ *= normalization_factor
        rhs *= normalization_factor

        # Learning rate scales the applied displacement per unit t:
        # trial step = -lr * t * updates, i.e. curve length t of the configured step.
        lr = float(self.optimizer.learning_rate)
        lr2 = lr * lr

        # Compute the current loss value.
        loss = self.loss(targets, outputs)

        # Backup the current variables before any trial: restore_variables()
        # must never fall back to the zero-initialized backup of __init__
        # when the first trials of a step are rejected.
        self.backup_variables()

        stop_training = False
        attempts = tf.constant(self.attempts_per_step, dtype=tf.int32)

        damping_factor = self.damping_algorithm.init_step(self.damping_factor, loss)
        min_value = self.damping_algorithm.min_value
        max_value = self.damping_algorithm.max_value

        # Damping-matrix contributions shared by all trials of this step:
        # G = JJ + lam_eff * D with D = I (or diag(JJ) with Fletcher scaling).
        if self.damping_algorithm.fletcher:
            updates_d_updates = lambda u: tf.reduce_sum(  # noqa: E731
                u * u * tf.linalg.diag_part(JJ)
            )
        else:
            updates_d_updates = lambda u: tf.reduce_sum(u * u)  # noqa: E731

        attempt = 0
        t = 1.0
        t_retries = 0
        accepted = False
        self.last_t_acc = None

        while tf.constant(True, dtype=tf.bool):
            update_computed = False
            try:
                # Apply the damping to the Gauss-Newton Hessian approximation.
                JJ_damped = self.damping_algorithm.apply(damping_factor, JJ)

                # Compute the LM direction: (JJ + lam*D) * updates = rhs.
                updates = compute_gauss_newton(J, JJ_damped, rhs)
            except Exception as e:
                print(f'An error occurred: {e}')
            else:
                if tf.reduce_all(tf.math.is_finite(updates)):
                    update_computed = True

            if update_computed:
                # sigma = <updates, G updates> = -phi'(0) / lr^2 of the applied curve.
                if overdetermined:
                    # The solve gives G * updates == rhs, so sigma = updates' rhs.
                    sigma = tf.reduce_sum(updates * rhs)
                else:
                    Ju = tf.linalg.matmul(J, updates)
                    sigma = normalization_factor * tf.reduce_sum(Ju * Ju)

                if not (sigma > 0.0):
                    update_computed = False

            if not update_computed:
                # Linear system failed or direction is not a descent direction:
                # regularize harder and rebuild the direction.
                if attempt >= attempts:
                    break
                damping_factor = self.damping_algorithm.increase(damping_factor, loss)
                if self.damping_algorithm.stop_training(damping_factor, loss):
                    stop_training = True
                    break
                t = 1.0
                t_retries = 0
                continue

            attempt += 1

            # Trial of curve length t: apply -lr * t * updates to the variables.
            trial_updates = tf.split(tf.squeeze(updates, axis=-1) * t, self._splits)
            trial_updates = [
                tf.reshape(update, shape)
                for update, shape in zip(trial_updates, self._shapes)
            ]
            self.optimizer.apply_gradients(
                zip(trial_updates, self.model.trainable_variables)
            )

            outputs = self.model(inputs, training=False)
            new_loss = self.loss(targets, outputs)

            # Predicted reduction of the linear model along the applied curve (Eq. 34).
            u_dd = updates_d_updates(updates)
            scaler = 1.0
            if self.damping_algorithm.adaptive_scaling:
                scaler = tf.math.reduce_max(tf.linalg.diag_part(JJ))
            lam_eff = scaler * damping_factor

            delta_c = loss - new_loss
            delta_m = lr2 * (
                (t - 0.5 * t * t) * sigma + 0.5 * lam_eff * t * t * u_dd
            )

            # K=1 hybrid acceptance: any real decrease is accepted. The strict
            # rho gate of RNC-LM (rho > eta_acc) is only meaningful with K >= 2
            # curves, where the prediction becomes accurate; a straight-line
            # update structurally under-delivers in the ill-conditioned regime,
            # so rho is used solely to drive the damping update (Eq. 40).
            if delta_c > 0.0:
                # Accept the trial point and backup the accepted variables.
                rho_acc = delta_c / delta_m if delta_m > 0.0 else -1.0
                loss = new_loss
                self.last_t_acc = float(t)
                self.backup_variables()
                accepted = True

                # Damping update (Eq. 40), referenced to the FULL step: when the
                # accepted point came after backtracking (t < 1), the full step
                # already proved untrustworthy, so lambda must not relax --
                # otherwise lambda collapses to its floor while t crawls at its
                # own floor (v2 crawl regime, see exp/docs/devlog.md).
                if t < 1.0:
                    damping_factor = tf.minimum(
                        tf.scalar_mul(2.0, damping_factor), max_value
                    )
                elif rho_acc < 0.25:
                    damping_factor = tf.minimum(
                        tf.scalar_mul(2.0, damping_factor), max_value
                    )
                elif rho_acc > 0.75:
                    damping_factor = tf.maximum(damping_factor / 3.0, min_value)
                break

            # Trial rejected: restore the old variables.
            self.restore_variables()

            if t_retries < self.max_t_retries and attempt < attempts:
                # Quadratic interpolation along the current curve (Eq. 36-37):
                # phi'(0) = -lr^2 * sigma, trial phi(t) = new_loss.
                denom = 2.0 * ((new_loss - loss) + lr2 * sigma * t)
                if denom > 0.0:
                    t_next = lr2 * sigma * t * t / denom
                else:
                    t_next = 0.5 * t
                t = min(max(t_next, self.beta_min * t), self.beta_max * t)
                t_retries += 1
                continue

            # Curve exhausted without an acceptable point: increase damping,
            # recompute the direction and restart from the full curve length.
            if attempt >= attempts:
                break
            damping_factor = self.damping_algorithm.increase(damping_factor, loss)
            if self.damping_algorithm.stop_training(damping_factor, loss):
                stop_training = True
                break
            t = 1.0
            t_retries = 0

        if not accepted:
            stop_training = stop_training or self.damping_algorithm.stop_training(
                damping_factor, loss
            )

        # Update the damping_factor for the next train_step.
        self.damping_factor.assign(damping_factor)
        return loss, outputs, attempt, stop_training

    def _compute_num_outputs(self, inputs: Tensor, targets: Tensor) -> Tensor:
        input_shape = inputs.shape[1:]  # Exclude batch dimension
        target_shape = targets.shape[1:]

        # Create input and targets with batch size set to 0
        _inputs = tf.zeros(shape=(0, *input_shape), dtype=inputs.dtype)
        _targets = tf.zeros(shape=(0, *target_shape), dtype=targets.dtype)

        # Pass symbolic inputs through the model
        outputs = self.model(_inputs)

        # Use Keras operations directly
        residuals = self.loss.residuals(_targets, outputs)

        # Return the total number of outputs
        return tf.reduce_prod(residuals.shape[1:])

    def reset_damping_factor(self):
        self.damping_factor.assign(self.damping_algorithm.starting_value)

    def backup_variables(self):
        zip_args = (self.model.trainable_variables, self._backup_variables)
        for variable, backup in zip(*zip_args):
            backup.assign(variable)

    def restore_variables(self):
        zip_args = (self.model.trainable_variables, self._backup_variables)
        for variable, backup in zip(*zip_args):
            variable.assign(backup)

    def train_step(self, inputs: Tensor, targets: Tensor):
        if self._num_outputs is None:
            self._num_outputs = self._compute_num_outputs(inputs, targets)

        batch_size = tf.shape(inputs)[0]
        num_residuals = batch_size * self._num_outputs
        overdetermined = num_residuals >= self._num_variables

        if overdetermined:
            init_gauss_newton = self._init_gauss_newton_overdetermined
            compute_gauss_newton = self._compute_gauss_newton_overdetermined
        else:
            init_gauss_newton = self._init_gauss_newton_underdetermined
            compute_gauss_newton = self._compute_gauss_newton_underdetermined

        if self.step_control == 'trust_ratio':
            loss, outputs, attempts, stop_training = self._train_step_tr(
                inputs,
                targets,
                init_gauss_newton,
                compute_gauss_newton,
                overdetermined,
            )
        else:
            loss, outputs, attempts, stop_training = self._train_step(
                inputs,
                targets,
                init_gauss_newton,
                compute_gauss_newton,
            )

        return loss, outputs, attempts, stop_training

    def fit(self, dataset, epochs: int = 1, metrics=None):
        """Trains self.model on the dataset for a fixed number of epochs.

        Arguments:
            dataset: A tf.data dataset, must return a tuple (inputs, targets).
            epochs: Integer. Number of epochs to train the model.
            metrics: List of metrics to be evaluated during training.
        """
        self.backup_variables()
        steps = dataset.cardinality().numpy().item()
        stop_training = False

        if metrics is None:
            metrics = []

        pl = keras.callbacks.ProgbarLogger()

        pl.set_params(
            {
                'verbose': 1,
                'epochs': epochs,
                'steps': steps,
                'stateful_metrics': ['damping_factor', 'attempts'],
            }
        )

        pl.on_train_begin()

        for epoch in range(epochs):
            if stop_training:
                break

            # Reset metrics.
            for m in metrics:
                m.reset_states()

            pl.on_epoch_begin(epoch)

            iterator = iter(dataset)

            for step in range(steps):
                if stop_training:
                    break

                pl.on_train_batch_begin(step)

                data = next(iterator)

                data = data_adapter.expand_1d(data)
                inputs, targets, sample_weight = data_adapter.unpack_x_y_sample_weight(
                    data
                )

                loss, outputs, attempts, stop_training = self.train_step(
                    inputs, targets
                )

                # Update metrics.
                for m in metrics:
                    m.update_state(targets, outputs)

                logs = {
                    'damping_factor': self.damping_factor,
                    'attempts': attempts,
                    'loss': loss,
                }
                logs.update({m.name: m.result() for m in metrics})

                pl.on_train_batch_end(step, logs)

            pl.on_epoch_end(epoch)

        pl.on_train_end()
