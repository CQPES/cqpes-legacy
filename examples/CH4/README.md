# CH<sub>4</sub>

Here is the example case for Methane as presented in the CQPES paper (_MDPI Chemistry_ **2025**).

The conformations and corresponding electronic energies at the CCSD(T)/cc-pVQZ level are obtained from the VIB5 dataset.

## Data

`rawdata/` ships both input formats (97,217 frames, atom order `[C, H, H, H, H]`, matching the `MOL_1_4_4` basis built as `1 4`):

| file | content |
|---|---|
| `CH4.xyz` + `CH4_CCSD-T.dat` | legacy route: coordinates (Angstrom) + absolute electronic energies (**Hartree**) |
| `CH4.extxyz` | extxyz route: single self-describing file, energies in **eV**, used as-is |

The extxyz file is regenerated from the legacy pair with:

```python
from ase.io import read, write
from ase.calculators.singlepoint import SinglePointCalculator
import numpy as np

frames = read("rawdata/CH4.xyz", index=":")
E_ev = np.loadtxt("rawdata/CH4_CCSD-T.dat").ravel() * 27.211386245988  # Hartree -> eV
for mol, e in zip(frames, E_ev):
    mol.calc = SinglePointCalculator(mol, energy=float(e))
write("rawdata/CH4.extxyz", frames)
```

## Workflow

Both routes are energy-only and produce equivalent datasets (`p` and coordinates
are bitwise identical; `V` differs by exactly the constant reference offset).

**Legacy route** (`config/prepare.json`, V = E − E_ref with E_ref = dataset minimum):

```bash
$ cqpes prepare config/prepare.json --jaxpip MOL_1_4_4.json.gz
$ cqpes train config/train.json          # data: "data"
$ cqpes test model_<timestamp>/
```

**extxyz route** (`config/prepare_extxyz.json`, energies used as-is in eV):

```bash
$ cqpes prepare config/prepare_extxyz.json --jaxpip MOL_1_4_4.json.gz
$ cqpes train config/train_extxyz.json   # data: "data_extxyz"
$ cqpes test model_<timestamp>/
```

For the `MSA` backend instead of `JaxPIP`, build the matching .so via
[PyMSA-Builder](https://github.com/CQPES/PyMSA-Builder) (input `1 4` for this
basis) and pass `--msa path/to/msa.cpython-*.so`.

## References

- (1) Zhang, L.; Zhang, S.; Owens, A.; Yurchenko, S. N.; Dral, P. O. VIB5 Database with Accurate Ab Initio Quantum Chemical Molecular Potential Energy Surfaces. _Sci. Data_ **2022**, _9_ (1), 84. https://doi.org/10.1038/s41597-022-01185-w.
- (2) Li, J.; Song, K.; Li, J. CQPES: A GPU-Aided Software Package for Developing Full-Dimensional Accurate Potential Energy Surfaces by Permutation-Invariant-Polynomial Neural Network. _Chemistry (Basel)_ **2025**, _7_ (6), 201. https://doi.org/10.3390/chemistry7060201.
