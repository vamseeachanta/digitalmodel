# Plan: digitalmodel #578 — W2W motion-compensated gangway operability module

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/578
**Status:** plan-review
**Tier:** T2 (composes existing infrastructure; well-scoped operability surface)

## Context
- **Standards / publisher / revision.** DNV-ST-0358 (2024-02) — Certification of offshore gangways for personnel transfer. Adjacent: DNV-RP-H103 (already in wiki) for marine-operations weather criteria.
- **Why it matters.** Walk-to-Work (W2W) gangway operability is a primary FOWT O&M cost driver. `grep -rilE 'gangway|walk.to.work|w2w|motion.compensat' src/digitalmodel/` returns zero hits. `src/digitalmodel/orcaflex/weather_window.py` already produces operability tables from Hs/Tp limits but does not model gangway-specific motion limits (vessel + floater RAOs, relative motion at landing point, gangway slewing range).
- **Dependencies on other issues.** Hard dependency on #574 for DNV-ST-0358 wiki resolver page. Soft dependency on the existing `weather_window.py` and `hydrodynamics/rao_analysis/` packages (no API change required there).

## Scope
- New package `src/digitalmodel/marine_ops/w2w_gangway/`:
  - `gangway_models.py` — Pydantic models for `GangwayEnvelope` (slew range degrees, telescope min/max m, heave-comp range m, max landing-point velocity m/s, max landing-point acceleration m/s²), `LandingPoint` (xyz on receiving structure), `SOVConfig` (Service Operation Vessel main particulars + RAO reference).
  - `relative_motion.py` — `compute_relative_rao(sov_rao, floater_rao, landing_point) -> RelativeMotionRAO` combining vessel + floater motions at the landing point in 6 DOF.
  - `operability.py` — `gangway_operability_table(scatter_diagram, gangway, relative_rao) -> OperabilityTable`. Returns Hs/Tp band crossings plus persistence stats. Wraps `weather_window.py` so gangway operability composes with the existing operability surface.
  - `__init__.py`.
- `tests/marine_ops/w2w_gangway/test_relative_motion.py` — analytic regression: zero-floater-motion case reduces to vessel-only RAO.
- `tests/marine_ops/w2w_gangway/test_operability.py` — fixture: SOV + spar FOWT scenario (uses K01 spar RAO + a generic SOV RAO from a public reference like Damen FCS 7011) reproduces a hand-calc operability number within ±2%.
- `tests/marine_ops/w2w_gangway/fixtures/sov_spar_scenario.yml` — fixture data.
- `docs/domains/articles/w2w_gangway_operability.md` — explainer, Hs/Tp band table example.
- **Non-goals.** No structural design of the gangway (DNV-ST-0358 §5; this PR is operability-only). No collision risk between SOV and floater. No motion-prediction filter design (gangway controller territory). No crew-comfort criterion (ISO 2631-1 — separate follow-up).

## Deliverables
- `src/digitalmodel/marine_ops/w2w_gangway/__init__.py`
- `src/digitalmodel/marine_ops/w2w_gangway/gangway_models.py`
- `src/digitalmodel/marine_ops/w2w_gangway/relative_motion.py`
- `src/digitalmodel/marine_ops/w2w_gangway/operability.py`
- `tests/marine_ops/w2w_gangway/test_relative_motion.py`
- `tests/marine_ops/w2w_gangway/test_operability.py`
- `tests/marine_ops/w2w_gangway/fixtures/sov_spar_scenario.yml`
- `docs/domains/articles/w2w_gangway_operability.md`

## Approach
1. **Survey existing infrastructure.** `ls src/digitalmodel/hydrodynamics/rao_analysis/` and `head src/digitalmodel/orcaflex/weather_window.py` to confirm the RAO container API and the operability-table return type. Smoke check: `cd digitalmodel && uv run python -c "from digitalmodel.orcaflex.weather_window import OperabilityTable; print(OperabilityTable.__doc__)"`.
2. **Build models.** `gangway_models.py` first (no dependencies). Smoke check: `cd digitalmodel && uv run python -c "from digitalmodel.marine_ops.w2w_gangway.gangway_models import GangwayEnvelope; print('ok')"`.
3. **Relative-motion combiner.** Implement using existing RAO arithmetic. Smoke check: `cd digitalmodel && uv run pytest tests/marine_ops/w2w_gangway/test_relative_motion.py::test_zero_floater_motion_reduces_to_vessel -q`.
4. **Operability table.** Compose with `weather_window.OperabilityTable`. Smoke check: `cd digitalmodel && uv run pytest tests/marine_ops/w2w_gangway/test_operability.py -q`.
5. **Citation.** Emit `Citation(code_id="dnv-st-0358", ...)`. Smoke check: `cd digitalmodel && uv run pytest tests/marine_ops/w2w_gangway/ -q -k citation`.
6. **Doc note.** Write the explainer.

## Open questions
- **SOV reference.** Default SOV RAO source: a public Damen FCS 7011 / Ulstein SX195 reference dataset, or a parametrised generic? Default: generic parametric for v1; document the data-source path. **Needs user input.**
- **Motion-criterion authority.** DNV-ST-0358 §3 cites a `0.5 m/s lateral velocity at landing` heuristic; recent revisions tighten to 0.3 m/s for stepped landing. Confirm which to bake into the default `GangwayEnvelope`.
- **Persistence stats.** Reuse the existing `weather_window.persistence_*` functions or duplicate? Default: reuse — module is a thin operability layer.
- **Multi-DOF criterion combination.** ST-0358 mixes velocity, acceleration, and excursion criteria. Default: AND-combination (most conservative); flag OR-combination as caller-toggleable.

## Acceptance Criteria
- [ ] Module produces gangway-operability tables consistent with DNV-ST-0358 motion criteria (test fixture replicates a published or hand-calc Hs/Tp band).
- [ ] Reuses existing `wave_spectra` + `rao_analysis` infrastructure (no duplication; verified by `grep -c "from digitalmodel.hydrodynamics" src/digitalmodel/marine_ops/w2w_gangway/*.py >= 2`).
- [ ] Citation to DNV-ST-0358 emits and resolves via the wiki page from #574.
- [ ] Pytest fixture covers one SOV + spar FOWT scenario; full module suite green: `uv run pytest tests/marine_ops/w2w_gangway/ -q`.
- [ ] No regression in `tests/orcaflex/test_weather_window.py`.
