# Exit handoff — production casing design capability (2026-07-02)

Session scope: turn `productioncasingdesignconsiderations.pdf` (B. Hansen, Devon
Energy, EPA Hydraulic Fracturing Technical Workshop — Session 2: Well Design,
Aug 2011; public EPA material) into a full-ecosystem capability: algorithms +
tests, capabilities explorer, durable workflow, wiki notes, session worklog.
The source folder has since moved from `Temp - Oil&Gas/` to the ace
`og-general-reference/` share.

## Delivered (all MERGED)

| Artifact | Where | Reference |
|---|---|---|
| Design-check layer: load cases (tubing leak, frac screen-out, mud/mix-water/pore external, full-evacuation collapse), DF engine (burst 1.25 / 1.1 <5 ksi SICP, collapse 1.1, tension 1.4, compression 1.2, VME 1.25 — configurable), API rounding, `max_frac_surface_pressure` + ballooning/cooling adders, NACE MR0175 sour screen, connection classes | `src/digitalmodel/well/tubulars/casing_design.py` (+42 tests) | #1290 → PR #1300 |
| Explorer (8 products, Plotly depth profiles, Barlow golden panel, NACE + connections tables) + frozen-JSON drift guard (27 tests) + "Well construction — casing & tubulars" capabilities section + one-pager PDFs + API envelope | `docs/api/well/casing-design-explorer.html`, `scripts/capabilities/build_casing_design_explorer.py`, `build_onepagers.py` specs `sec-well`/`casing-design` | #1290 → PR #1300 |
| Durable workflow: engine basename `casing_design` → `run_casing_design` adapter; registry id `casing-design`; example input = same demo well as the explorer; fail-closed test assertions | `well/workflow.py`, `docs/registry/workflows.yaml`, `examples/workflows/casing-design/input.yml` | #1305 → PR #1306 |
| Wiki notes: concept + source pages, indexed | `llm-wiki/wikis/drilling-engineering/wiki/{concepts,sources}/` | llm-wiki PR #817 |
| Session work review (live link) | <https://vamseeachanta.github.io/workspace-hub/casing-design-worklog.html> | workspace-hub `build_pages.py` HTML_PAGES |

Live: <https://vamseeachanta.github.io/digitalmodel/well/casing-design-explorer.html>
and the capabilities page section.

## Verification

- Golden numbers pinned in tests: 5-1/2" 23# P110 Barlow burst **14,520 psi**
  (deck worked example, API-rounded), 7" 26# P110 published ratings (9,960 /
  6,230 psi / 830 klbf), max frac surface pressure 11,616 psi at DF 1.25,
  sour screen 0.85 psia partial pressure with the 175/225 °F grade windows.
- `tests/well/tubulars` 137 passed; `tests/workflows/test_durable_workflows.py`
  127 passed / 2 runtime-skipped. Explorer build byte-reproducible; drift-guard
  test re-runs every published case against the live checker.
- CI on both PRs: all touched shards green (`marine-ops-other` owns
  `tests/well/`; `tests-workflows` green). The 4 failing shards
  (infrastructure-other, misc, orcaflex-solver, specialized) are the standing
  baseline-red set (same failures on merged PR #1298).

## Gotchas recorded (also in auto-memory)

- `from digitalmodel.well.tubulars import casing` yields the **function** (the
  package `__init__` shadows the submodule) — import names from
  `digitalmodel.well.tubulars.casing` directly.
- assetutilities `ApplicationManager` merges the custom input YAML only under
  pytest (`inputfile=` param) or CLI `sys.argv[1]`; a plain scripted
  `engine(inputfile=...)` silently runs on the base config alone.
- Registry rows are fail-closed in `test_durable_workflows.py` — every new
  workflow needs a per-id value-assertion block or the test raises.
- `build_onepagers.py` CLI filter is positional spec ids (not `--only`).

## Open / candidate next steps (not started)

- Biaxial collapse derating under axial tension (API 5C3 reduced-yield) — the
  known physics gap noted in #1290; the API-ellipse envelope in
  `design_envelope.py` is the composition point.
- Tubing-design counterpart to the casing layer (same deck covers tubing).
- Baseline-red cleanup of the 4 standing CI shards (every dm PR shows red).
