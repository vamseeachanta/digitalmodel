# OrcaWave Diffraction Analysis

**ABOUTME**: Canonical spec-to-run workflow for Orcina OrcaWave panel-method diffraction analysis — high-level `spec.yml` in, validated hydrodynamic results out, with direct integration to OrcaFlex.

See also: [OrcaFlex & OrcaWave Manuals and References](../orcaflex/MANUALS_AND_REFERENCES.md) for official documentation links, YAML format reference, and API resources.

---

## Quick start

The canonical workflow starts from a solver-agnostic high-level spec. A complete
runnable example ships with the code at
[`examples/hydrodynamics/diffraction/unit_box_rao/`](../../../examples/hydrodynamics/diffraction/unit_box_rao/)
(spec + mesh + README) — copy that directory, swap the mesh, edit the spec.

```bash
# 1. Validate the spec (any host, no license)
uv run diffraction validate-spec examples/hydrodynamics/diffraction/unit_box_rao/spec.yml

# 2. Generate a self-contained solver package without solving (any host)
uv run diffraction run-orcawave examples/hydrodynamics/diffraction/unit_box_rao/spec.yml \
    --dry-run -o output/

# 3. Full solve with automatic result validation (licensed Windows host)
uv run diffraction run-orcawave examples/hydrodynamics/diffraction/unit_box_rao/spec.yml \
    -o output/
```

All commands live on the `diffraction` console script (Click CLI at
`src/digitalmodel/hydrodynamics/diffraction/cli.py`). `python -m digitalmodel
diffraction ...` works once the routing fix in
[#713](https://github.com/vamseeachanta/digitalmodel/issues/713) is merged;
until then use the console script form shown above.

---

## Command surface

| Command | Purpose |
| --- | --- |
| `diffraction validate-spec <spec.yml>` | Schema-validate a canonical spec |
| `diffraction convert-spec <spec.yml> [--solver orcawave\|aqwa\|all] [--format single\|modular] [-o DIR]` | Generate solver-native input from a spec (no run) |
| `diffraction run-orcawave <spec.yml> [-o DIR] [--dry-run] [--validate/--no-validate] [--strict-validation]` | Prepare and execute an OrcaWave run |
| `diffraction resolve --outcome <name> --mesh <file> --water-depth <d> --out <spec.yml> [--loa --beam --draft --displacement --hull-id --inertia-mode]` | Inverse entry point: outcome + barebones data → complete spec + assumption ledger |
| `diffraction run-aqwa` / `batch-aqwa` / `batch-orcawave` | AQWA runs and multi-spec batch processing |
| `diffraction validate-geometry` | Mesh quality checks |
| `diffraction mesh-build` | Mesh construction/export helpers |
| `diffraction convert-orcawave` / `convert-aqwa` | Solver results → OrcaFlex vessel type |
| `diffraction compare` / `plot-raos` | Cross-solver comparison and RAO plotting |

Run any command with `--help` for the full option list — the CLI is the source
of truth, not this table.

### Canonical spec shape

`spec.yml` is solver-agnostic (schema: `hydrodynamics/diffraction/input_schemas.py`,
Pydantic-validated). Top-level sections: `vessel` (geometry + inertia),
`environment`, `frequencies`, `wave_headings`, optional `solver_options`,
`outputs`, `metadata`. See the
[example spec](../../../examples/hydrodynamics/diffraction/unit_box_rao/spec.yml)
for a minimal complete instance, and `L00_validation_wamit/*/spec.yml` in this
directory for the WAMIT validation suite.

---

## Mesh paths and output layout

**Resolution**: `vessel.geometry.mesh_file` is resolved relative to the
directory containing the spec file. Keep meshes next to the spec with a
relative reference so the pair stays portable.

**Packaging**: `run-orcawave` (including `--dry-run`) writes a self-contained
package — the generated OrcaWave input references the mesh relatively and the
mesh is copied alongside, so the output directory can be moved to a licensed
machine and solved there:

```
output/
├── <VesselName>.yml        # OrcaWave-native input, relative BodyMeshFileName
├── <mesh file>             # copied next to the input
└── modular/                # per-section files (01_general ... master.yml) for review
```

A licensed solve additionally produces:

```
output/
├── <VesselName>.owr        # OrcaWave results
└── <VesselName>_data.dat   # hydrodynamic data export
```

`.xlsx` export is deliberately deferred (decision D2,
[#611](https://github.com/vamseeachanta/digitalmodel/issues/611)).
Note: `convert-spec` currently writes the input `.yml` only and does not copy
meshes — self-contained `convert-spec` packages are tracked in
[#605](https://github.com/vamseeachanta/digitalmodel/issues/605); prefer
`run-orcawave --dry-run` when you need a runnable package.

---

## Execution behavior: licensed vs unlicensed hosts

The runner (`hydrodynamics/diffraction/orcawave_runner.py`) tries, in order:

1. **OrcFxAPI Python binding** (licensed Windows host): `Diffraction(...).Calculate()`,
   saving `.owr` + `_data.dat`.
2. **Subprocess**: `OrcaWave.exe <input.yml>` if an executable is found
   (explicit `--executable`, `ORCAWAVE_PATH` env var, standard install paths,
   or `PATH`).
3. **Dry-run fallback**: with `--dry-run`, or when no solver is available,
   files are generated and validated but no solve happens — the run reports
   `DRY_RUN`, not an error.

After a successful solve, the output validator runs automatically
(`--validate`, on by default; wired in
[#625](https://github.com/vamseeachanta/digitalmodel/issues/625)): reciprocity,
energy, asymptote, and completeness checks produce a `PASS`/`WARNING`/`FAIL`
verdict in the run summary. `--strict-validation` turns a `FAIL`/`ERROR`
verdict into a failed run.

Every value the resolver or backend assumed on your behalf (defaults, database
lookups) is recorded in an **assumption ledger**
(`assumption_ledger.py`, [#624](https://github.com/vamseeachanta/digitalmodel/issues/624))
so the output report surfaces what was not user-supplied.

---

## Semantic-equivalence claim boundary

The current OrcaWave workflow should be described conservatively:
- near-equivalent for key engineering inputs and tested round-trip pathways
- not guaranteed 100% semantically equivalent across every strict OrcaWave YAML field

Practical interpretation:
- preserved and regression-tested fields include frequency values, heading values, COG, inertia tensor, fixed DOFs / constraints, and representative solver options
- accepted normalization differences include unit/representation changes such as kg/m^3 <-> t/m^3, rad/s <-> period(s), bool <-> Yes/No, and Infinity <-> infinite
- some strict OrcaWave fields are intentionally classified rather than preserved literally:
  - `output_only`
  - `gui_only`
  - `internal_default_only`
  - `known_non_configurable_in_spec`
  - `solver_mode_significant`
  - `physics_significant`
  - `representation_normalization_only`

This means the repo is strong for engineering round-trip fidelity, but it does not claim universal identity across every strict OrcaWave YAML key or solver-internal default.

## Named multi-body benchmark (#2458)

`multibody_fpso_turret_v1` promotes the FPSO hull plus turret fixture as a named OrcaWave benchmark under `tests/hydrodynamics/diffraction/fixtures/benchmarks/multibody_fpso_turret_v1/`. The benchmark manifest records the claim boundary as near-equivalent for key engineering inputs and tested round-trip pathways, not strict identity across every native OrcaWave YAML field.

The benchmark is a bridge candidate for future OrcaWave -> OrcaFlex handoff validation. It extends the delivered foundations from #1605, #1592, and #1768 with a stable multi-body case that preserves body count, body identity, fixed DOFs, connection parent, body position, attitude, mass, COG, and radii-of-gyration-derived inertia through the OrcaWave forward and reverse path.

---

## Integration with OrcaFlex

Convert solver results into an OrcaFlex vessel type:

```bash
uv run diffraction convert-orcawave <results> -o vessel_type/
```

then load the generated vessel type YAML in OrcaFlex:

```python
import OrcFxAPI

model = OrcFxAPI.Model()
vessel = model['Vessel1']
vessel.VesselType = 'my_vessel_vessel_type.yml'
```

---

## Examples and benchmarks in this directory

- **`L00_validation_wamit/`** — 13 canonical-spec WAMIT validation cases (all pass schema → generation audit; see [`../SPEC_AUDIT_REPORT.md`](../SPEC_AUDIT_REPORT.md))
- **`examples/L01_default_vessel/` … `L06`** — Orcina-provided OrcaWave-native examples; `L01` includes the licensed smoke-test path ([#468](https://github.com/vamseeachanta/digitalmodel/issues/468)) and `API_EXECUTION_SUMMARY.md` (16-case success; 180-case timeout under investigation in [#714](https://github.com/vamseeachanta/digitalmodel/issues/714))
- **`L01_aqwa_benchmark/`** — AQWA cross-validation studies
- **`LICENSED_E2E_ACCEPTANCE.md`** — acceptance contract for the licensed end-to-end run ([#610](https://github.com/vamseeachanta/digitalmodel/issues/610))

---

## Workstream status and related issues

The output-driven diffraction domain epic is
[#622](https://github.com/vamseeachanta/digitalmodel/issues/622). Key open
threads relevant to this workflow:

- [#610](https://github.com/vamseeachanta/digitalmodel/issues/610) — licensed end-to-end acceptance run from the canonical spec (the workflow's acceptance gate)
- [#500](https://github.com/vamseeachanta/digitalmodel/issues/500) — hard-fail mesh preflight in spec conversion
- [#605](https://github.com/vamseeachanta/digitalmodel/issues/605) — self-contained `convert-spec` packages
- [#606](https://github.com/vamseeachanta/digitalmodel/issues/606) / [#608](https://github.com/vamseeachanta/digitalmodel/issues/608) — mesh pipeline integration and quality gates before solve
- [#607](https://github.com/vamseeachanta/digitalmodel/issues/607) — planned given-mesh workflow (largely covered today by `diffraction resolve`)
- [#613](https://github.com/vamseeachanta/digitalmodel/issues/613) — planned `orcawave-doctor` environment/license diagnostic

## Legacy workflow (historical)

Earlier documentation described a 5-phase vessel-name-driven orchestrator
(`orchestrator.py --vessel sea_cypress`) and a `modules/` package layout. That
layout no longer exists; the orchestrator survives at
`src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` for existing
vessel configs but is **not** the canonical path — new work should start from a
`spec.yml` and the `diffraction` CLI as described above.

---

## Prerequisites

1. **Any host (dry-run, spec authoring, validation)** — `uv sync` only.
2. **Licensed solve** — Windows host with OrcaWave installed and either the
   OrcFxAPI Python binding (preferred) or `OrcaWave.exe` reachable via
   `ORCAWAVE_PATH`/`PATH`.

```bash
# Verify solver availability on a licensed host
python -c "import OrcFxAPI; print(OrcFxAPI.Version())"
where OrcaWave
```

---

## API Reference

For detailed OrcaWave Python API documentation, see:
https://www.orcina.com/webhelp/OrcFxAPI/Redirector.htm?Pythonreference,Diffraction.htm

## External Resources

- [OrcaWave Manual](https://www.orcina.com/webhelp/OrcaWave/Default.htm)
- [OrcFxAPI Reference](https://www.orcina.com/webhelp/OrcFxAPI/)
- [OrcaPySM1](https://github.com/praveen-kch/OrcaPySM1) — Python scripts for OrcaWave automation
- [orcawave](https://github.com/Exor8129/orcawave) — Additional OrcaWave utilities

---

**Last Updated**: 2026-06-12
**Version**: Canonical spec-to-run workflow (issue #614)
