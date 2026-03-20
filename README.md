# CQPES (ChongQing Potential Energy Surface)

[![DOI](https://img.shields.io/badge/DOI-10.3390/chemistry7060201-B31B1B)](https://doi.org/10.3390/chemistry7060201)
[![PyPI version](https://img.shields.io/pypi/v/cqpes)](https://pypi.org/project/cqpes/)
[![License](https://img.shields.io/pypi/l/cqpes)](https://github.com/cqpes/cqpes-legacy/blob/main/LICENSE)
[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/CQPES/cqpes-legacy)

This package is a Python implementation of potential energy surface (PES) fitting with permutational invariant polynomials neural network (PIP-NN).

Paper: [CQPES: A GPU-Aided Software Package for Developing Full-Dimensional Accurate Potential Energy Surfaces by Permutation-Invariant-Polynomial Neural Network](https://doi.org/10.3390/chemistry7060201)

If CQPES helps your work, please cite correctly.

> Li, J.; Song, K.; Li, J. CQPES: A GPU-Aided Software Package for Developing Full-Dimensional Accurate Potential Energy Surfaces by Permutation-Invariant-Polynomial Neural Network. _Chemistry (Basel)_ **2025**, _7_ (6), 201. https://doi.org/10.3390/chemistry7060201.

## Installation

We provide pre-packaged installers including all dependencies (TensorFlow, CUDA, etc.). This is the most stable way to use CQPES.

### Step 1: Deploy the Environment

Download the installer from [Releases](https://github.com/cqpes/cqpes-legacy/releases).

Run the installer:

```bash
# cpu version
$ bash ./cqpes-cpu-2.0.2-Linux-x86_64.sh
# cuda version
$ cat cqpes-cu120-2.0.2-Linux-x86_64.sh.part* > cqpes-cu120-2.0.2-Linux-x86_64.sh
$ bash ./cqpes-cu120-2.0.2-Linux-x86_64.sh
```

### Step 2: Install CQPES

Activate your environment first:

```
$ conda activate /path/to/cqpes-env
```

For standard users, simply install via `pip`:

```bash
(cqpes-env)$ pip install cqpes
```

For beginners and developers, or offline environments, clone the repository and install from source:

```bash

(cqpes-env)$ git clone https://github.com/CQPES/cqpes-legacy.git
(cqpes-env)$ cd cqpes-legacy
# For users
(cqpes-env)$ pip install .
# For developers
(cqpes-env)$ pip install -e .
```

### Step 3: Verify the installation

```bash
(cqpes-env)$ cqpes -h
usage: cqpes [-h] [-v] {prepare,train,test,export,predict,run} ...

CQPES: GPU-Aided Potential Energy Surface Development Toolkit

positional arguments:
  {prepare,train,test,export,predict,run}
                        Sub-commands
    prepare             Prepare dataset from raw files
    train               Train PIP-NN model via Keras & TensorFlow
    test                Evaluate model performance and plot errors
    export              Export trained model for dynamics interfaces
    predict             Predict energies for a given XYZ trajectory
    run                 Run tasks (Opt, TS, Freq, MD) using trained PES via ASE

options:
  -h, --help            show this help message and exit
  -v, --version         show program's version number and exit
```

## The CQPES Workflow

CQPES provides a unified Command Line Interface (CLI) for the entire PES development lifecycle: from data preparation and training, to model evaluation and dynamic simulations.

You can try the following steps in directory `example/CH4`!

### Step 1. Generate PIP Basis

We provide a wrapper for MSA-2.0 to generate the PIP basis. Clone the builder repository:

```bash
(cqpes-env)$ git clone https://github.com/CQPES/PyMSA-Builder.git
(cqpes-env)$ cd PyMSA-Builder
(cqpes-env)$ python3 build.py
```

Follow the interactive prompts to configure your molecular system (e.g., `4 1` for an A<sub>4</sub>B system like CH<sub>4</sub>). Once built, copy the generated .so library into your working directory.

### Step 2: Prepare Dataset

Organize your raw structural data in standard xyz format and your corresponding energies (in Hartree) in a .dat file. Pack them into efficient NumPy arrays using the prepare command:

```bash
(cqpes-env)$ cqpes prepare config/prepare.json --msa msa.cpython-310-x86_64-linux-gnu.so
```

This handles the Morse-like variable transformations and structural unpacking automatically based on your JSON configuration.

### Step 3: Model Training

Train the PIP-NN model using Levenberg-Marquardt (LM) or other optimizers defined in your configuration file:

```bash
(cqpes-env)$ cqpes train config/train.json
```

The trained models and training logs will be saved in a timestamped output path for model, e.g., `model_20260315_123751`.

### Step 4. Model Evaluation

Evaluate the accuracy of your trained PES against the test set:

```bash
(cqpes-env)$ cqpes test model_20260315_123751/
```

This will automatically compute MAE, MSE, and RMSE for your training, validation, and test subsets. Additionally, fitting error scatter plots and histograms will be generated and saved in the model path for visual diagnostics.

### Step 5. Model Export

CQPES can be exported in 2 formats:

- Standard Keras `h5` format, required by `predict` and `run` commands
- Legacy `potfit` plain text format, compatible with fortran interface for software like Polyrate, VENUS96C, or Caracal.

```bash
(cqpes-env)$ cqpes export -t h5 model_20260315_123751/
(cqpes-env)$ cqpes export -t potfit model_20260315_123751/
```

The exported files will be stored in model path.

### Step 6. Property Prediction

Quickly predict energies and forces (analytical and numerical) for a given .xyz trajectory using a trained model.

```bash
(cqpes-env)$ cqpes predict model_20260315_123751 new_trajectory.xyz --output predictions.xyz
```

### Step 7. Running Simulations

CQPES is fully integrated with the Atomic Simulation Environment (ASE). You can perform high-level computational chemistry tasks directly via the CLI:

```bash
# Geometry optimization followed by analytical frequency analysis
(cqpes-env)$ cqpes run model_20260315_123751 ch4.xyz --opt min --freq

# Transition State (TS) search (requires Sella)
(cqpes-env)$ cqpes run model_20260315_123751 guess_ts.xyz --opt ts --freq

# Molecular Dynamics (NVT ensemble at 300K)
(cqpes-env)$ cqpes run model_20260315_123751 input.xyz --md nvt --temp 300.0 --dt 1.0 --steps 5000 -o md.xyz
```

## Supported Interfaces & Interoperability

### Python

```python
from cqpes import CQPESPot, CQPESCalculator
```

### Fortran

[`example/CH4/interface/Fortran`](https://github.com/CQPES/cqpes-legacy/tree/main/example/CH4/interface/Fortran)

### Gaussian

[`example/CH4/interface/Gaussian`](https://github.com/CQPES/cqpes-legacy/tree/main/example/CH4/interface/Gaussian)

### Polyrate

[CQPES Legacy + Polyrate](https://github.com/CQPES/cqpes-legacy-polyrate)

### VENUS96

[`example/CH4/interface/VENUS96C`](https://github.com/CQPES/cqpes-legacy/tree/main/example/CH4/interface/VENUS96C)

### Caracal

## Reference

- (1) Xie, Z.; Bowman, J. M. Permutationally Invariant Polynomial Basis for Molecular Energy Surface Fitting via Monomial Symmetrization. _J. Chem. Theory Comput._ **2010**, _6_ (1), 26–34. https://doi.org/10.1021/ct9004917.
- (2) Nandi, A.; Qu, C.; Bowman, J. M. Using Gradients in Permutationally Invariant Polynomial Potential Fitting: A Demonstration for CH4 Using as Few as 100 Configurations. _J. Chem. Theory Comput._ **2019**, _15_ (5), 2826–2835. https://doi.org/10.1021/acs.jctc.9b00043.
- (3) Jiang, B.; Guo, H. Permutation Invariant Polynomial Neural Network Approach to Fitting Potential Energy Surfaces. _J. Chem. Phys._ **2013**, _139_ (5). https://doi.org/10.1063/1.4817187.
- (4) Li, J.; Jiang, B.; Guo, H. Permutation Invariant Polynomial Neural Network Approach to Fitting Potential Energy Surfaces. II. Four-Atom Systems. _J. Chem. Phys._ **2013**, _139_ (20). https://doi.org/10.1063/1.4832697.
- (5) Li, J.; Song, K.; Li, J. CQPES: A GPU-Aided Software Package for Developing Full-Dimensional Accurate Potential Energy Surfaces by Permutation-Invariant-Polynomial Neural Network. _Chemistry (Basel)_ **2025**, _7_ (6), 201. https://doi.org/10.3390/chemistry7060201.
