# CH4 (MSA-2.0 geom.inp) Force-Aided Fitting Example

Force-aided PIP-NN fitting for CH<sub>4</sub> from the **MSA-2.0 example
dataset** (`geom.inp`, 1000 CCSD(T)-style geometries with energies and
gradients, shipped with
[PyMSA-Builder](https://github.com/CQPES/PyMSA-Builder) / MSA-2.0).

## ⚠️ Units — read before touching the data

The MSA-2.0 data file mixes three different unit systems on one line, and
the third column group is a **gradient**, not a force:

| quantity | unit in `geom.inp` | unit in CQPES |
|---|---|---|
| coordinates | Angstrom | Angstrom |
| energy | **Hartree** | Hartree (input), eV (internal) |
| **gradient** dE/dxyz | **Hartree/Bohr** | **force** −dE/dxyz in **eV/Angstrom** |

The conversion (applied exactly once, in `tools/convert_msa20.py`):

```
F[eV/A] = -grad[Hartree/Bohr] * Hartree / Bohr
```

Two more traps this dataset ships with, both handled by the converter's
sanity checks:

- **Atom order**: `geom.inp` follows the permutation-group order A4B —
  `[H, H, H, H, C]` — which matches the `MOL_4_1_4` basis used here.
  Do NOT mix with datasets ordered `[C, H, H, H, H]` (see the plain CH4
  example) without permuting.
- Morse alpha: MSA's `param.inp` uses `a = 2.0 bohr`; CQPES uses its own
  `alpha = 1.0 Angstrom` (the PIP basis structure is alpha-independent,
  alpha only enters the Morse variable `exp(-r/alpha)`).

## Conversion

```bash
$ python3 tools/convert_msa20.py \
      --geom ../CH4-with-forces/PyMSA-Builder/MSA-2.0/geom.inp \
      --out-dir rawdata \
      --name CH4
```

Outputs (also checked into `rawdata/` for convenience):
`CH4.xyz` (Angstrom), `CH4_energy.dat` (Hartree), `CH4_force.dat`
(eV/Angstrom, N x 3*Natoms, atom-major), plus the JaxPIP basis
`MOL_4_1_4.json.gz`.

## Workflow

```bash
# 1. dataset (energies + forces + dp/dxyz), backend-free
$ cqpes prepare config/prepare.json --jaxpip rawdata/MOL_4_1_4.json.gz

# 2. LM training on [y | F/F_rms] residuals
$ cqpes train config/train.json

# 3. evaluation (energy AND force metrics)
$ cqpes test model_<timestamp>/
```

`fit.force_weight: 1` balances the two O(1)-normalized residual blocks
(energies min-max scaled, forces divided by their dataset RMS, here
1.41 eV/A). See the main README for the `force_weight` semantics.

## Reference

Nandi, A.; Qu, C.; Bowman, J. M. Using Gradients in Permutationally
Invariant Polynomial Potential Fitting: A Demonstration for CH4 Using as
Few as 100 Configurations. _J. Chem. Theory Comput._ **2019**, _15_ (5),
2826–2835.
