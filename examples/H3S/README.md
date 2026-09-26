# H + H<sub>2</sub>S

Here is an example case for the reactive system H + H<sub>2</sub>S published in _J. Chem. Phys._ **2016**.

<div align="center">
  <img src="./pes_h+h2s.jpg" width="400">
</div>

## Data

`rawdata/` ships both input formats (33,435 frames, atom order `[H, H, H, S]`, matching the `MOL_3_1_3` basis built as `3 1`):

| file | content |
|---|---|
| `H3S.xyz` + `H3S_CCSD-T.dat` | legacy route: coordinates (Angstrom) + absolute electronic energies (**Hartree**) |
| `H3S.extxyz` | extxyz route: single self-describing file, energies in **eV**, used as-is |

Note that `config/prepare.json` sets an explicit `ref_energy` (-399.45864 Hartree) rather than the dataset minimum, so the legacy route gives `V` in the range -0.61 … 14.81 eV. The extxyz route ignores `ref_energy` by design and uses the file energies verbatim.

The extxyz file is regenerated from the legacy pair with:

```python
from ase.io import read, write
from ase.calculators.singlepoint import SinglePointCalculator
import numpy as np

frames = read("rawdata/H3S.xyz", index=":")
E_ev = np.loadtxt("rawdata/H3S_CCSD-T.dat").ravel() * 27.211386245988  # Hartree -> eV
for mol, e in zip(frames, E_ev):
    mol.calc = SinglePointCalculator(mol, energy=float(e))
write("rawdata/H3S.extxyz", frames)
```

## Workflow

Both routes are energy-only and produce equivalent datasets (`p` and coordinates
are bitwise identical; `V` differs by exactly the constant reference offset).

**Legacy route** (`config/prepare.json`, V = E − E_ref with explicit E_ref):

```bash
$ cqpes prepare config/prepare.json --jaxpip MOL_3_1_3.json.gz
$ cqpes train config/train.json          # data: "./data"
$ cqpes test model_<timestamp>/
```

**extxyz route** (`config/prepare_extxyz.json`, energies used as-is in eV):

```bash
$ cqpes prepare config/prepare_extxyz.json --jaxpip MOL_3_1_3.json.gz
$ cqpes train config/train_extxyz.json   # data: "./data_extxyz"
$ cqpes test model_<timestamp>/
```

For the `MSA` backend instead of `JaxPIP`, build the matching .so via
[PyMSA-Builder](https://github.com/CQPES/PyMSA-Builder) (input `3 1` for this
basis) and pass `--msa path/to/msa.cpython-*.so`.

## References

- (1) Lu, D.; Li, J. Full-Dimensional Global Potential Energy Surfaces Describing Abstraction and Exchange for the H + H2S Reaction. _J. Chem. Phys._ **2016**, _145_ (1), 014303. https://doi.org/10.1063/1.4954765.
- (2) Lu, D.; Qi, J.; Yang, M.; Behler, J.; Song, H.; Li, J. Mode Specific Dynamics in the H2 + SH → H + H2S Reaction. _Phys. Chem. Chem. Phys._ **2016**, _18_ (42), 29113–29121. https://doi.org/10.1039/c6cp05780b.
