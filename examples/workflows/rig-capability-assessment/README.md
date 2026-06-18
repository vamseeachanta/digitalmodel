# Rig Capability Assessment (stages 1–4)

Screen candidate onshore drilling rigs against a well/field program and rank the
survivors on a weighted capability fit. Rule-based, deterministic, public-data —
no network, no licensed solver, no downhole physics.

Issue: digitalmodel#821. Full design + public data registry:
`deckhand/docs/deckhand/workflows/rig-capability-assessment.md`.

## Run

```bash
uv run python -m digitalmodel examples/workflows/rig-capability-assessment/input.yml
```

Writes `results/rig_capability_summary.json` with, per rig: qualified (pass/fail),
the specific unmet hard-gates (fail reasons), the weighted fit score, and the
per-dimension score breakdown — plus the derived required capability and the
ranking.

## What it does

1. **Required capability** — derived from the `well_design` envelope (heaviest
   string → hookload via a 1.25 design factor; mud pressure; pad mode; automation
   / data interop), floored at the publicly-stated "super-spec" threshold (H&P
   FY2025 10-K: AC, ≥1,500 HP, ≥750,000 lb hookload, 7,500 psi, walking). Override
   with an explicit `required_capability:` block.
2. **Rig ingest** — each candidate references a bundled public rig-CLASS default
   (`hp_flexrig`, `precision_st1500`, `pten_apex`, `nabors_pace_x800`) and may add
   per-rig `overrides:`.
3. **Hard-gate screen** — pass/fail with explicit reasons. (The example's Nabors
   PACE-X800 fails the default `walking` gate because it uses an X-Y skid — a
   deliberate demonstration.)
4. **Weighted fit scoring** — ranks survivors on capability margin, pad mobility,
   automation/digital, power/emissions, and geothermal HT readiness.

## Caveats

- Rig-class defaults are **marketing-grade, not contractual** — binding capability
  needs the per-rig IADC daily drilling report / spec sheet.
- `ht_track_record` is a coarse 0–1 prior (no contractor publishes it) — override
  with real EGS high-temperature evidence per engagement.
- KPI baselining (ROP/cost/NPT/uptime) and any paid rig-level feed (Enverus,
  S&P Petrodata) are **out of scope** here — future issues.
