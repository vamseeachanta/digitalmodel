# Roll damping database

Public tables behind the viscous roll damping calculator
(`digitalmodel.hydrodynamics.roll_damping_components`) and its benchmark
checks. Load them with `digitalmodel.hydrodynamics.roll_damping_benchmarks`.

| File | Holds | Rows |
|---|---|---|
| `benchmark_cases.csv` | Measured or fitted roll damping of barge-like hulls from the literature, one row per case and amplitude, with page and verification level | see `manifest.yaml` |
| `sources.csv` | Citation and link for each `source_id` | see `manifest.yaml` |
| `acceptance_criteria.yaml` | Three-colour benchmark bands (green, amber, red) per quantity, each with its rationale | 7 quantities |
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
  regenerates every calculated number and verdict of the generic roll damping
  report from these tables.
- Add a benchmark row when a new test or CFD result is read in full; update
  `manifest.yaml` (`scripts/roll_damping/build_manifest.py`) in the same change.
