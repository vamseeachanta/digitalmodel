# Exit handoff — capabilities-page expansion + live-explorer program (2026-07-04)

## What this session delivered

Started from "review the capabilities page, restore the dropped wall-thickness &
cathodic-protection sections, explore further capabilities, file issues." It grew into a full
build-out of `docs/api/capabilities/` (the public capabilities page).

### 1. Restore + expand the page (13 → 22 sections) — all MERGED, live
- **#1389** — restored **Wall thickness — sizing & code checks** and **Cathodic protection**
  sections (they were never-added, not dropped: the page was born in #1149 from live-dashboard
  seeds only). Anchoring issue **#1380**.
- **#1394** — Naval architecture + Geotechnical sections.
- **#1396** — VIV, Fatigue, Production, Drilling, Field-development sections (folded into one
  integration PR — they share nav/validation-table/footer lines). Backlog epic **#1391**
  (children #1381–#1387), each an adversarially-verified content spec.

### 2. Broken-main fix — MERGED
- **#1408** — `tests/docs/test_digitalmodel_routing_contract.py` was RED on main: `motion_forecast`
  (#1358) and `residual` (#1374) domains were added to `src/digitalmodel/` by other lanes without
  their operator-map + routing-registry rows. Added both to
  `docs/maps/digitalmodel-operator-map.md` and `docs/registry/module-routing.yaml`
  (`package_domains() == operator_map_domains() == registry_domains()` = 62). This also unblocked
  the contract-gate failure that PR **#1403** (drilling-riser explorer, other lane) had inherited.

### 3. Live interactive explorers — 8 total, all MERGED, all engine-driven (not mocked)
Each is a committed `scripts/capabilities/build_*_explorer.py` that runs the real digitalmodel
engine over a parameter sweep, embeds the results as JSON in a self-contained interactive HTML
(`docs/api/structural/*-explorer.html`, slider/selector-driven, no external assets), and wires the
section's card to it. All rendered + screenshot-verified.

| PR | Section | Explorer | Engine |
|----|---------|----------|--------|
| #1400 | Wall thickness | multi-code utilization | `WallThicknessAnalyzer` (9 codes) |
| #1401 | Cathodic protection | sacrificial-anode by coating/life | DNV-RP-B401 `marine_structure_current_demand` |
| #1411 | (both above) | 1-page PDFs + workflow-API pages | `build_onepagers.py` SPECS |
| #1414 | Naval architecture | ship resistance & powering | Holtrop-Mennen `total_resistance` |
| #1415 | Geotechnical | drag-anchor holding | DNV-RP-E302 `drag_anchor_capacity` |
| #1417 | Production | well IPR (Vogel) | `VogelIpr.flow_rate` |
| #1419 | VIV | reduced-velocity lock-in | DNV-RP-C205 `VIVScreening` |
| #1420 | Field development | NPV vs oil price | `evaluate_economics` (DCF) |
| #1421 | Drilling | pore-pressure / mud-weight | Eaton + Bowers `*_pore_pressure` |

## Repo state at exit
- **All of the above PRs are MERGED** to `origin/main`. Verified: all 8 explorer HTML files and
  all 8 card links are present on `origin/main`; page is 22 sections / 22 nav.
- **No dirty working state, no dangling worktrees, no local servers/processes.** Work was done in
  a scratch sparse-clone; the main `/mnt/local-analysis/digitalmodel` checkout was not modified.
- **No external actions** (no emails/messages/publishes) taken.

## Open / next items
- **PR #1403** (drilling-riser operability explorer) is another lane's, still open — my #1408
  unblocked its contract-gate failure; it needs a CI re-run then can land. It shares
  `build_onepagers.py` with any future explorer PDFs (coordinate).
- **Explorer 1-page PDFs + workflow-API pages** exist only for wall-thickness & CP (#1411). The 6
  new-section explorers (#1414–#1421) are live-page only; adding their PDFs is a clean follow-on via
  the same `build_onepagers.py` SPECS pattern (do it after #1403 lands to avoid the shared-file merge).
- **Fatigue** is the one epic-#1391 section without a live explorer (S-N / SCF engine not built this
  phase).

## Environment lessons (see memory `project_dm_capabilities_page_expansion`)
- `import digitalmodel` can BLOCK for minutes (up to ~400s) on an OrcaFlex-license retry — builds
  look hung but aren't. Pure-stdlib submodules (e.g. `well/drilling/pore_pressure.py`) can be
  direct-file-loaded to bypass.
- Some engines run ~0.6–1 s/call (citation/edition validation) — keep sweeps to a few hundred points.
- JS `String(2.0)` = `"2"` ≠ Python `str(2.0)` = `"2.0"` — use `pi.toFixed(1)` for float-keyed lookups.
- Parallel subagent probes collide on shared scratch filenames and CPU-contend the slow imports —
  serialize the python probes.
- Engine venv: `/mnt/local-analysis/digitalmodel/.venv/bin/python` + `warnings.filterwarnings('ignore')`.
