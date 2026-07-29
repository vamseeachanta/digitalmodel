# Issue #1903 — generator/artifact head-alignment report

Scope: all 18 `build_*.py` scripts under `scripts/capabilities/`,
`scripts/corrosion/`, and `scripts/production_chemistry/`.

| Generator | Output page | Changed? | What changed |
|---|---|---:|---|
| `scripts/capabilities/build_anchor_holding_explorer.py` | `docs/api/structural/anchor-holding-explorer.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc}`. |
| `scripts/capabilities/build_capabilities_inventory.py` | N/A — writes `docs/capability-map/capabilities-inventory.json` and `docs/capability-map/capabilities-ia-spec-1444.md` | no | No HTML output or HTML template exists. |
| `scripts/capabilities/build_casing_design_explorer.py` | `docs/api/well/casing-design-explorer.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc;--ok:#1f9d57;--bad:#b91c1c}`. |
| `scripts/capabilities/build_cathodic_protection_explorer.py` | `docs/api/structural/cathodic-protection-explorer.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc;--ok:#1f9d57;--teal2:#12A6B0}`. |
| `scripts/capabilities/build_cfd_runtime_estimator.py` | `docs/api/cfd/cfd-runtime-estimator.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--ok:#1f9d57;--bad:#c0392b}`. |
| `scripts/capabilities/build_dynacard_example_library.py` | N/A — writes `src/digitalmodel/marine_ops/artificial_lift/dynacard/data/example_cards.json` | no | No HTML output or HTML template exists. |
| `scripts/capabilities/build_dynacard_troubleshooting.py` | `docs/api/artificial-lift/dynacard-troubleshooting.html` | no | Already aligned: `data-theme`, `../_assets/brand.css`, and the page-local reduced `:root` block match the committed page. |
| `scripts/capabilities/build_field_economics_explorer.py` | `docs/api/structural/field-economics-explorer.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc;--ok:#1f9d57;--bad:#c0392b}`. |
| `scripts/capabilities/build_ipr_explorer.py` | `docs/api/structural/ipr-explorer.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc}`. |
| `scripts/capabilities/build_onepagers.py` | `docs/api/capabilities/api/*.html` (36 pages) | yes | Changed `_API_TEMPLATE` only: added `data-theme="light"` and the exact `../../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc}`. The PDF `_TEMPLATE` was left untouched. |
| `scripts/capabilities/build_pore_pressure_explorer.py` | `docs/api/structural/pore-pressure-explorer.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc;--ok:#1f9d57;--bad:#c0392b;--mw:#7c3aed}`. |
| `scripts/capabilities/build_ship_resistance_explorer.py` | `docs/api/structural/ship-resistance-explorer.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc}`. |
| `scripts/capabilities/build_sloshing_cfd_showcase.py` | `docs/api/cfd/sloshing-cfd-study.html` | no | Skipped: the committed page has a `brand.css` link but no inline `:root` block, so its head differs from the reduced-`:root` pattern in scope. |
| `scripts/capabilities/build_sloshing_explorer.py` | `docs/api/structural/sloshing-explorer.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc;--ok:#1f9d57;--bad:#c0392b;--roll:#7c3aed;--c0:#0B3D91;--c1:#0f8a7e;--c2:#b8860b;--c3:#c0392b}`. |
| `scripts/capabilities/build_viv_explorer.py` | `docs/api/structural/viv-explorer.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc;--ok:#1f9d57;--warn:#b7791f;--bad:#c0392b}`. |
| `scripts/capabilities/build_wall_thickness_explorer.py` | `docs/api/structural/wall-thickness-explorer.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc;--ok:#1f9d57;--bad:#c0392b;--warn:#b7791f}`. |
| `scripts/corrosion/build_galvanic_explorer.py` | `docs/api/corrosion/galvanic-compatibility-explorer.html` | yes | Added `data-theme="light"` and the exact `../_assets/brand.css` link; reduced the inline block to `:root{--soft:#f4f8fc}`. |
| `scripts/production_chemistry/build_scale_si_explorer.py` | `docs/api/production/scale-si-explorer.html` | no | Skipped: the committed page has a `brand.css` link but no inline `:root` block, so its head differs from the reduced-`:root` pattern in scope. |

## SKIPPED / UNCERTAIN

- `build_capabilities_inventory.py` was not touched because it emits JSON and
  Markdown, not HTML.
- `build_dynacard_example_library.py` was not touched because it emits JSON,
  not HTML.
- `build_dynacard_troubleshooting.py` was not touched because its template head
  already matches its committed page.
- `build_sloshing_cfd_showcase.py` was not touched because its committed page
  has no inline `:root` block. Removing the generator's bespoke full block
  would be a different head transformation from the supplied reduced-block
  pattern.
- `build_scale_si_explorer.py` was not touched for the same reason: its
  committed page has no inline `:root` block.
- `build_onepagers.py::_TEMPLATE` was not touched because it renders PDF
  one-pagers; only `_API_TEMPLATE` renders the committed HTML pages in scope.

No identified committed HTML output lacked a `brand.css` link. In particular,
both different-pattern pages listed above contain
`../_assets/brand.css`; their uncertainty is the absent inline `:root`, not the
brand link.

## Verification performed

- Baseline static comparison against `HEAD`: 13 of 14 comparable generator
  heads drifted; `build_dynacard_troubleshooting.py` was the one match.
- Post-edit static comparison: 14 of 14 comparable heads match their committed
  pages for the exact `<html>` tag, `brand.css` link, and inline `:root` block.
- `build_onepagers.py::_API_TEMPLATE`: 36 of 36 committed API-page heads match.
- All 18 enumerated Python scripts parse successfully with `ast.parse`.
- The changed-script manifest is exactly the expected 13 files.
- `git diff --check` passes, and `git diff -- docs/` is empty.

The generators were not executed, per the task constraint. Full regeneration
and no-op diff verification therefore remains for an environment where
`uv run` is available.
