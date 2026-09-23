# CH4 Force-Aided Fitting Example

PIP-NN training with energies **and** forces (gradients) for CH<sub>4</sub>,
demonstrating the `force` workflow of CQPES.

## Dataset

`00.data/` contains a DeePMD-kit style dataset (VASP PBE data, non-periodic,
from an `OUTCAR` — not checked into the repository): 160 training frames +
40 validation frames, energies in eV, forces in eV/Angstrom, atom order
`[H, H, H, H, C]`.

The JaxPIP basis for this system is included at
`rawdata/MOL_4_1_4.json.gz`.

The converter packs both splits into the CQPES rawdata format:

```bash
$ python3 tools/convert_deepmd.py \
      --deepmd-dir 00.data \
      --out-dir rawdata \
      --name CH4
```

- `rawdata/CH4.xyz` — coordinates (Angstrom)
- `rawdata/CH4_energy.dat` — total energies (Hartree)
- `rawdata/CH4_force.dat` — forces (eV/Angstrom, N x 3*Natoms, atom-major)

> **Atom order must match your PIP basis.** This dataset is already in the
> `[H, H, H, H, C]` order expected by an MSA basis built as `A(4)B(1)` with
> A = H (e.g. `MOL_4_1_4` from PyMSA-Builder with input `4 1`). If your
> basis expects the unique atom first (e.g. built as `1 4`), pass
> `--permute "4,0,1,2,3"` to the converter — it permutes coordinates and
> forces consistently.

## Workflow

Both backends support force-aided training: `prepare` stores the PIP
Jacobian `dp/dxyz` alongside the forces (`dp.npy` in the dataset) —
computed with `dbemsav` for the `MSA` backend and with forward-mode
automatic differentiation for the `JaxPIP` backend (the two agree to
machine precision):

```bash
# 1. dataset (energies + forces + dp/dxyz)
#    MSA backend: needs a compiled MSA .so, built via PyMSA-Builder
#    (https://github.com/CQPES/PyMSA-Builder, input "4 1" for CH4)
$ cqpes prepare config/prepare.json --msa path/to/msa.cpython-*.so
#    or, backend-free:
$ cqpes prepare config/prepare.json --jaxpip rawdata/MOL_4_1_4.json.gz

# 2. LM training on [y | F] residuals
$ cqpes train config/train.json

# 3. evaluation (reports energy AND force metrics)
$ cqpes test model_<timestamp>/
```

In `config/train.json`, `fit.force_weight` is the relative weight of the
force term in the loss:

```
loss = MSE(E) + force_weight * MSE(F)
```

To train energies only, remove the `force` entry from `prepare.json` and
re-run prepare (any dataset without `force.npy`/`dp.npy` automatically
falls back to energy-only training).

## Notes

- Training runs entirely in FP64 (as does the rest of CQPES).
- Export / predict / run work exactly as with an energy-only model: the
  trained artifact is a plain MLP, and analytical forces at inference are
  assembled from the same chain (`dV/dy · dy/dX · dX/dp · dp/dxyz`).
