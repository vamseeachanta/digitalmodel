# Session handoff — dynacard capability (rounds 1 + 2), 2026-07-01 → 2026-07-02

## What shipped (all MERGED and LIVE)

| Round | Issue → PR | Delivered |
|---|---|---|
| R1 | #1257 → PR #1262 | 7 curated troubleshooting use cases, "Artificial lift" capabilities section, one-pagers + workflow-API envelopes, 16-test drift guard |
| R1 side-fix | PR #1265 | Unbroke main's Quality Gates (riser_database abs-path exemptions) |
| R2 | #1288 → PR #1298 | 336-card example library + loader, 28-entry troubleshooting guide, POC settings & alarms engine, explorer v2 with dropdown library browser + settings section |

**Live:**
- Explorer: <https://vamseeachanta.github.io/digitalmodel/artificial-lift/dynacard-troubleshooting.html>
- Capabilities section: <https://vamseeachanta.github.io/digitalmodel/capabilities/#artificial-lift>

## Module surface added

- `dynacard/example_cards.py` + `dynacard/data/example_cards.json` — 336 cards:
  88 `synthetic-verified` (5 seeds + severity params per mode, each verified to
  re-classify to its own mode), 3 `field-measured` (anonymized field-well-A/B
  surface cards with SPM/stroke), 245 `field-archive-digitized` (normalized
  pump-card shapes; phenomena BUTTERFLY / INCOMPLETE_FILLAGE /
  FILLAGE_COLLAPSE / FULL_CARD).
- `dynacard/troubleshooting.py` — `TROUBLESHOOTING_GUIDE` (28 phenomena:
  18 classifier modes + 5 archive labels + FLUMPING, DEVIATED_WELL_FRICTION,
  HIGH_FLUID_LEVEL, HEAVY_OIL, BARREL_HOLE_SPLIT), `guide_for()`.
- `dynacard/poc_settings.py` — `recommend_setpoints` / `evaluate_alarms`
  (two-tier load recipes, span + card-area malfunctions, pump-off strokes,
  fillage deadband, VSD limits) plus `automatic_idle_time`,
  `estimate_production_bpd`, `separator_capacity_bpd`, `rods_parted`.
  Golden tests pin the published worked examples
  (`tests/.../dynacard/test_poc_settings.py`).

## Build scripts (scripts/capabilities/)

- `build_dynacard_troubleshooting.py` — the explorer page (use cases +
  library browser + settings section). Refuses to publish any use case whose
  diagnosis drifts from its mode.
- `build_dynacard_example_library.py` — assembles the library JSON.
  Reproducible **without** the private share: synthetic recomputed, field
  entries preserved from the committed JSON.
- `extract_dynacard_archive_pngs.py` — PNG digitizer (red-marker pixel
  clustering → normalized shapes), optional re-run.
- `build_onepagers.py <spec-ids>` — scoped PDF/API regeneration.

## De-identification (hard gate — keep it)

No operator / field / well / personnel / vendor identifiers are committed
anywhere. Field-source locations live ONLY in two **uncommitted** side files
next to the scripts on the build machine:
`scripts/capabilities/_measured_cards_manifest.json` (archive root + per-card
source paths) and `_digitized_cards_stage.json` (staged digitized cards).
Without them the library build preserves the committed field entries. Do not
git-add these files; do not name the archive path in the repo.

## Verification state at exit

- `pytest tests/marine_ops/artificial_lift/` — 739 passed, 1 skipped.
- Drift guards: 7 use cases + 88 synthetic library cards re-classified in CI;
  guide coverage asserted against `PumpDiagnostics.FAILURE_MODES`.
- PR #1298 domain-gate failures (misc / specialized / infrastructure-other /
  orcaflex-solver) are the pre-existing baseline-red set of **#1277** —
  identical on main; all artificial-lift shards green.

## Open threads

- **#1263** — classifier fails on Gibbs-solver-transformed cards
  (NORMAL→WORN_BARREL end-to-end; only 7/18 modes round-trip through the
  registered workflow path). The published page avoids that path.
- **#1299** — 12-idea automation backlog from the training-doc mining
  (pumped-off-vs-gassy transfer-point discriminator, runtime/cycle KPIs,
  automated TV/SV valve checks, gas-handling advisor, load-triggered VSD
  governor, wave-echo diagnostics, …) with suggested sequencing.
- **#1277** — baseline-red CI shards (owned elsewhere).

## Gotchas for the next session

- The ML classifier's contract is **pump (downhole) cards in oilfield units**;
  never feed it normalized shapes (`ExampleCard.as_card_data()` docstring).
- P1 corner-detection fillage is not a discriminator on synthetic shapes —
  publish load-based metrics instead (deliberate, see build script docstring).
- Digitized cards render as **dots** (marker order from nearest-neighbour
  chaining is best-effort; `meta.chain_q` flags dubious chains).
- Shared-clone quirks on this machine: `git -c core.fsmonitor=false` for
  checkout/pull; drop `uv.lock` from diffs (`git checkout origin/main -- uv.lock`)
  after any `uv run`.
