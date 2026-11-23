# OrcaFlex Model Library

Each subdirectory is one model: a `spec.yml` (the high-level, human-authored
source of truth consumed by the modular generator) and, for models extracted
from existing OrcaFlex files, `modular/` and `monolithic/` reference outputs.

**Claim status** — what each model is *proven* to do — lives in
[`../../MODEL_CLAIM_REGISTRY.yaml`](../../MODEL_CLAIM_REGISTRY.yaml)
(attested claims, enforced by tests) and
[`../../MODEL_CLAIM_INVENTORY.yaml`](../../MODEL_CLAIM_INVENTORY.yaml)
(pending entries). Directory presence alone implies **no** claim: L1 =
validator-clean YAML generation, L2 = loads in licensed OrcaFlex, L3 =
statics/dynamics run without errors.

## Naming

- `a*`–`e*` prefixes follow the Orcina example lettering for models extracted
  from the standard OrcaFlex example set.
- `mj*` — M-shaped jumper models reconstructed from published literature
  (digitalmodel [#722](https://github.com/vamseeachanta/digitalmodel/issues/722)).

## Literature-derived models

Geometry provenance, stated-vs-inferred confidence flags, and published
dynamic responses (the L2/L3 validation targets) for the `mj*` family are
documented in the
[M-shaped jumper literature geometry survey](../../subsea/jumper/m_shaped/LITERATURE_GEOMETRY_SURVEY.md).

| Model | Source | Scale | Validation targets |
|-------|--------|-------|--------------------|
| `mj01_exxonmobil_m_jumper_viv` | Wang OMAE2013 / Zheng OMAE2015 / Igeh UiS 2017 | model (1:~4.5 of 10″ FS) | measured modes 0.863/2.149/2.194/2.542 Hz; VIV response 0.05–1.24 m/s |
| `mj02_zhu2022_m_jumper_fullscale` | Zhu et al. 2022, *Processes* 10:2133 | full scale (OD 270 mm) | out-of-plane f₁ ≈ 1.464 Hz; slug-FIV lock-in at 2.5 Hz |
| `mj03_li2024_m_jumper_lab` | Li et al. 2024, *JMSE* 12:1261 | lab (OD 60 mm) | 13 tabulated modes, f₁ = 8.520 Hz |

L1 regression for the `mj*` family:
`tests/solvers/orcaflex/modular_generator/test_m_jumper_library_specs.py`.
