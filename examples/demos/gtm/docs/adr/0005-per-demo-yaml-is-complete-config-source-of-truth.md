---
status: accepted
---

# Each demo's per-demo yaml is the COMPLETE reviewable source of truth for ALL config

Each demo's `inputs/demo_0N_*.yml` is the **COMPLETE reviewable source of truth for ALL
config**: the sweep axes (sizes / codes / pressures), the site/environment (water depth),
the material (grade, SMYS, SMTS, corrosion allowance), the code factors (safety class,
find-min bounds/tolerance), the report thresholds (fixed design pressure), the fundamental
physical constants (seawater density, gravity, steel density), the catalog locations
(pipelines.json / design_codes.json), and the artifact paths (results/output roots).

**No config is hardcoded in the demo.** The loader resolves the yaml into a typed config
object and the demo threads that object through every consumer — `run_parametric_sweep`,
`find_min_wall_thickness`, `run_single_analysis`, `build_chart_1..5`, `build_summary_table`,
`build_report` (report narrative), and the inline JSON-save block in `main()` (there is no
separate `save_results_json` function; the metadata block is built from the resolved config).
A client reviewing or editing the single yaml sees and controls the entire deliverable;
editing any value changes the computed results, the saved JSON metadata, AND the report
narrative live — compute, data, metadata, and report prose are all config-driven.

**Per-demo, not a shared library.** The complete config lives in each demo's own yaml (and
its own loader/dataclass), not in a cross-demo shared config library. Each demo is an
independently reviewable industry deliverable; coupling them through a shared schema/library
would re-bury config and couple unrelated demos. (User decision 2026-06-01.)

This **supersedes** the ADR-0002 / ADR-0004 Phase-1 compromise that "locked constants stay
in code." That compromise was a deliberately bounded blast radius for the first integration
subissue; the constants are now fully migrated into the yaml and wired, so the loud-refuse
"edited-but-unwired constant" guard (`_assert_constants_match_phase1`) is removed.

## Consequences

- The default run still reproduces the frozen golden exactly, because the committed baseline
  yaml encodes today's exact values; the ADR-0002 golden test continues to hold.
- The demo carries no config defaults that diverge from the yaml. The only in-code fallbacks
  are: the committed-baseline catalog/artifact PATHS (so the demo still runs if the yaml is
  unreadable), and a static PIPE_SIZES OD geometry reference table that the yaml *selects
  from* by size label (the geometry catalog, like pipelines.json, is reference data, not a
  swept parameter).
- An edited-config regression test asserts that changing a yaml constant (e.g. water depth)
  propagates into every result and diverges from the golden, while the baseline yaml still
  reproduces the golden — proving the wiring is live, not cosmetic.
- Editing the yaml is now the supported way to produce a variant run; there is no second
  source of truth to keep in sync.
