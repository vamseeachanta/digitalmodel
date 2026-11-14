# Roll damping database

Public tables behind the viscous roll damping calculator
(`digitalmodel.hydrodynamics.roll_damping_components`) and its benchmark
checks. Load them with `digitalmodel.hydrodynamics.roll_damping_benchmarks`.

| File | Holds | Rows |
|---|---|---|
| `benchmark_cases.csv` | Measured or fitted roll damping of barge-like hulls from the literature, one row per case and amplitude, with page and verification level | see `manifest.yaml` |
| `sources.csv` | Citation and link for each `source_id` | see `manifest.yaml` |
| `acceptance_criteria.yaml` | Three-colour benchmark bands (green, amber, red) per quantity, each with its rationale | 8 quantities |
| `method_matrix.csv` | Recommended estimation route, cross-check and screening damping per scenario | see `manifest.yaml` |
| `wiki_tables.csv` | Pointers to the copyrighted paper tables (C1, C2, Table 1) held in the private llm-wiki; no values | see `manifest.yaml` |

## Rules

- Every value carries its source page and a verification level: full text read,
  derived from a published fit or table, abstract only. Snippet-only values are
  not entered.
- Measured decay damping is **total** damping and includes the wave (radiation)
  part. The calculator's eddy and friction parts are viscous only; compare like
  with like, and state the basis in every comparison.
- Copyrighted coefficient tables stay in the private llm-wiki. This directory
  holds only the `wiki_path` to each.
- `examples/hydrodynamics/roll_damping/generic_roll_damping_study.py`
  computes the generic roll damping report's numbers and verdicts. From public
  data alone (these tables and the repository code, with no private wiki
  reachable) it grades every literature benchmark and 8 of the 12 reproduction
  items of Chakrabarti (2001). The four Fig. 4 container-ship items need the
  Q6 coefficient table held in the private wiki and are reported as not graded
  without it. The illustrative hull studies (sensitivity, amplitude and sweep
  results) need private source metadata to resolve their citations and stop
  with a citation error without it. `test_public_data_reproduction_scope`
  pins this split.
- Add a benchmark row when a new test or CFD result is read in full; update
  `manifest.yaml` (`scripts/roll_damping/build_manifest.py`) in the same change.

## Independent synthetic barge check

`test_study_generic_barge_base_case` checks an illustrative calculation, not a
measured benchmark or a worked example from a publication. The rectangular hull
has length 100.0 m, beam 30.0 m, draught 5.0 m, KG 7.0 m and block coefficient
1.0, with sharp corners, zero speed, no bilge keels and roll amplitude 10.0 deg.
These are synthetic study inputs. The mass radius of gyration is assumed as
0.35 times the beam and added roll inertia as 0.25 times structural roll inertia;
neither value is established by measurements. Water density is 1025.0 kg/m³,
kinematic viscosity is 1.19e-6 m²/s and gravity is 9.80665 m/s². Wave and lift
damping are set to zero, so the result is a viscous component estimate rather
than total decay damping. No claim of measured agreement follows from this test.

The hydrostatic and inertia calculation is independent of the study helpers:

```text
V = L B T; m = rho V; GM = T/2 + B²/(12 T) - KG
C = m g GM; I = m (0.35 B)²; A = 0.25 I
omega_n = sqrt(C/(I+A)); T_n = 2 pi/omega_n; B_critical = 2 sqrt(C (I+A))
```

The empirical basis is `source_id: chakrabarti2001` in `sources.csv`, equations
8–10 (skin friction) and 35 (sharp-cornered eddy damping). The test evaluates the
following scalar arithmetic without calling the component or conversion helpers;
the input frequency is the independently calculated natural frequency:

```text
OG = T - KG; phi = pi/18; H = B/(2 T)
S = L (1.7 T + B); r_f = (1.032 S/L - 2 OG)/pi
Re = r_f² phi² omega_n/nu
B_F = 0.787 rho S r_f² sqrt(omega_n nu) (1 + 0.00814 Re^0.386)
B_E = (2/pi) rho L T^4 (H² + 1 - OG/T) (H² + (1 - OG/T)²) phi omega_n
damping_pct = 100 (B_F + B_E)/B_critical
```

| Quantity | Independently calculated value | Units |
|---|---:|---|
| Displacement volume | 15000.0 | m³ |
| Displacement mass | 15375000.0 | kg |
| GM | 10.5 | m |
| Roll stiffness C | 1583161059.4 | N m/rad |
| Structural roll inertia I | 1695093750.0 | kg m² |
| Added roll inertia A | 423773437.5 | kg m² |
| Natural period | 7.3 | s |
| Friction damping B_F | 2414406.6 | N m s/rad |
| Eddy damping B_E | 701319554.9 | N m s/rad |
| Critical damping | 3663063210.6 | N m s/rad |
| Viscous damping | 19.2 | % critical |

Table 1. Synthetic base-case arithmetic; displayed values are rounded and test
comparisons use the unrounded calculations.

The period and combined percentage are checked alongside both inertias,
stiffness and the two damping terms. The calculation checks arithmetic under
the stated assumptions; empirical validity for an actual hull is not established.
