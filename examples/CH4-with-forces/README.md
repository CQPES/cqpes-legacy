# CH4 Force-Aided Fitting Example

PIP-NN training with energies **and** forces (gradients) for CH<sub>4</sub>,
demonstrating the `force` workflow of CQPES.

## Dataset

`00.data/` contains a DeePMD-kit style dataset (VASP PBE data, non-periodic,
from an `OUTCAR` — not checked into the repository): 160 training frames +
40 validation frames, energies in eV, forces in eV/Angstrom, atom order
`[H, H, H, H, C]`.

The converter packs both splits into a single extxyz file - the format
`cqpes prepare` expects for force-aided fitting:

```bash
$ python3 tools/convert_deepmd.py \
      --deepmd-dir 00.data \
      --out-dir rawdata \
      --name CH4
```

- `rawdata/CH4.extxyz` — coordinates (Angstrom), energies (eV),
  forces (eV/Angstrom) in one self-describing file

The JaxPIP basis for this system is included at
`rawdata/MOL_4_1_4.json.gz`.

> **Atom order must match your PIP basis.** This dataset is already in the
> `[H, H, H, H, C]` order expected by an MSA basis built as `A(4)B(1)` with
> A = H (e.g. `MOL_4_1_4` from PyMSA-Builder with input `4 1`). If your
> basis expects the unique atom first (e.g. built as `1 4`), pass
> `--permute "4,0,1,2,3"` to the converter — it permutes coordinates and
> forces consistently.

Alternatively, `dpdata` converts DeePMD data to extxyz directly:

```python
import dpdata
from ase.io import write
frames = [dpdata.LabeledSystem(d, fmt="deepmd/npy").to_ase_structure()
          for d in ["00.data/training_data", "00.data/validation_data"]]
write("rawdata/CH4.extxyz", [a for split in frames for a in split])
```

## Workflow

```bash
# 1. dataset (energies + forces + dp/dxyz)
#    MSA backend: needs a compiled MSA .so, built via PyMSA-Builder
#    (https://github.com/CQPES/PyMSA-Builder, input "4 1" for CH4)
$ cqpes prepare config/prepare.json --msa path/to/msa.cpython-*.so
#    or, backend-free:
$ cqpes prepare config/prepare.json --jaxpip rawdata/MOL_4_1_4.json.gz

# 2. LM training on [y | F/F_rms] residuals
$ cqpes train config/train.json

# 3. evaluation (reports energy AND force metrics)
$ cqpes test model_<timestamp>/
```

In `config/prepare.json`, `xyz`, `energy` and `force` all point at the same
extxyz file - coordinates, energies and forces are extracted from it
(ASE conventions: eV, eV/Angstrom; energies are used as-is).

In `config/train.json`, `fit.force_weight` is the relative weight of the
force term in the loss. Both residual blocks are normalized to O(1)
first — energies via min-max scaling to `[-1, 1]`, forces by their
dataset RMS (DeePMD-style `sigma_F`) — so

```
loss = MSE(ΔE / s_E) + force_weight · MSE(ΔF / F_rms)
```

with `s_E = (V_max - V_min) / 2`. `force_weight: 1` balances the two
relative errors and is dataset-independent; the "physical" balance
where a 1 meV/Å force error counts as a 1 meV energy error corresponds
to `force_weight = (F_rms / s_E)²` (≈ 96 for this dataset).

To train energies only, drop the `force` entry and point `xyz` and
`energy` at the same extxyz file (energies used as-is, in eV), or use the
legacy route: separate `xyz` plus a plain-text file of absolute energies
in Hartree.

## Notes

- Training runs entirely in FP64 (as does the rest of CQPES).
- Export / predict / run work exactly as with an energy-only model: the
  trained artifact is a plain MLP, and analytical forces at inference are
  assembled from the same chain (`dV/dy · dy/dX · dX/dp · dp/dxyz`).
