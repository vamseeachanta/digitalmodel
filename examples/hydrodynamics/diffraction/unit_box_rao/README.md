# Canonical diffraction example — unit box RAOs

The smallest complete, runnable example of the canonical diffraction workflow:
a high-level `spec.yml` plus its mesh, converted to an OrcaWave model and
solved end-to-end with one command. Use it as the starting point for any new
diffraction analysis — copy the directory, swap the mesh, edit the spec.

| File | Purpose |
| --- | --- |
| `spec.yml` | Canonical high-level diffraction spec (solver-agnostic) |
| `unit_box.gdf` | 5-panel WAMIT GDF unit box, referenced relatively from the spec |

The case is deliberately tiny — 3 frequencies x 3 headings on 5 panels — so a
licensed solve completes in seconds.

## Run it

On any host (no OrcaWave license required) — generate the solver package only:

```bash
uv run diffraction run-orcawave examples/hydrodynamics/diffraction/unit_box_rao/spec.yml \
    --dry-run -o output/
```

On a licensed Windows host (OrcFxAPI or OrcaWave installed) — full solve plus
automatic result validation:

```bash
uv run diffraction run-orcawave examples/hydrodynamics/diffraction/unit_box_rao/spec.yml \
    -o output/
```

> Use the `diffraction` console script as shown. `python -m digitalmodel
> diffraction ...` is not currently routed to this CLI (see issue #713).

## Expected output

Dry run (`--dry-run`):

```
output/
├── UnitBoxRAO.yml      # OrcaWave-native input, BodyMeshFileName: unit_box.gdf (relative)
├── unit_box.gdf        # mesh copied alongside — the directory is self-contained
└── modular/            # 9 per-section files (01_general.yml ... master.yml) for review
```

The run reports `[DRY_RUN]` and validation `SKIPPED` (no results without a solve).

Licensed solve additionally produces:

```
output/
├── UnitBoxRAO.owr           # OrcaWave results file
└── UnitBoxRAO_data.dat      # hydrodynamic data export
```

with a validation verdict (`PASS`/`WARNING`/`FAIL`) from the output validator
(reciprocity, energy, asymptote, and completeness checks) printed in the run
summary. `.xlsx` export is deferred by design (decision D2, issue #611).

## Convert only (no run)

To generate the OrcaWave input without invoking the runner:

```bash
uv run diffraction convert-spec examples/hydrodynamics/diffraction/unit_box_rao/spec.yml \
    --solver orcawave -o output/
```

Note: `convert-spec` currently writes the `.yml` only and does not copy the
mesh (issue #605); prefer `run-orcawave --dry-run` for a self-contained package.

## Regression guard

`tests/hydrodynamics/diffraction/test_canonical_example.py` dry-runs this spec
in CI on unlicensed hosts, so the example cannot silently rot. The licensed
end-to-end acceptance path is tracked in issue #610.
