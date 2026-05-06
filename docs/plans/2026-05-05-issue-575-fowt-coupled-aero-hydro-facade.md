# Plan: digitalmodel #575 — FOWT coupled aero-hydro response Python facade

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/575
**Status:** plan-review
**Tier:** T3 (new module crossing aero, hydro, mooring, dynamic-cable subsystems; non-trivial coupling)

## Context
- **Standards / publishers / revisions.** IEC 61400-3-2 Ed.1 (FOWT design + DLC matrix), DNV-RP-0286 (2019-05) coupled-analysis methodology, DNV-ST-0119 (2021-06) for global structural acceptance. All three need the wiki resolver pages from #574 to land first.
- **Why it matters.** Coupled aero-hydro response is the FOWT-defining calculation. Today's repo has the K01 5MW spar OrcaFlex example at `docs/domains/orcaflex/examples/modular/K01/K01 5MW spar FOWT/` plus reusable hydrodynamic infrastructure (`src/digitalmodel/hydrodynamics/{aqwa,capytaine,bemrosetta,wave_spectra.py,rao_analysis}`) and the mooring pilot at `src/digitalmodel/orcaflex/mooring_design.py`, but no Python facade orchestrates them with an IEC DLC matrix. Every new FOWT project re-wires the OrcaFlex YAML by hand.
- **Dependencies on other issues.** **Hard dependency on #574** for citation emission. Soft dependency on #576 (watch-circle vs cable curvature is a downstream consumer of this facade's mooring outputs).

## Scope
- New package `src/digitalmodel/marine_ops/marine_engineering/fowt_coupled/` containing:
  - `models.py` — Pydantic models for `RotorNacelleAssembly` (rotor diameter, hub height, rated thrust curve, RNA mass + CoG), `Tower`, `FloaterPlatform` (variant: spar / semi-sub / TLP, drawing on `field_development.concept_selection.HostType`), `DynamicCable` (top hang-off, sag bend, touchdown), `CoupledFOWTConfig` (composes the above + mooring + metocean).
  - `coupled_response.py` — `compute_coupled_response(config, dlc)` entry point that wires turbine thrust / RNA loads onto the floater RAO + mooring restoring + dynamic-cable boundary; returns time-domain or frequency-domain results.
  - `dlc_runner.py` — `IEC_61400_3_2_DLC` enum (DLC 1.1, 1.2, 1.3, 1.6, 6.1, 6.2, 6.3 minimum) plus `DLCMatrix.run(config)` orchestrator that emits one result per case.
  - `solvers/orcaflex_backend.py` — wraps OrcFxAPI for time-domain integration (graceful skip when `OrcFxAPI` is unavailable, mirroring the `if not _ORCFX_AVAILABLE` pattern already used elsewhere in `src/digitalmodel/orcaflex/`).
  - `solvers/openfast_backend.py` — **stub-only** in this PR (raises `NotImplementedError`), to fence the architecture decision in Open Questions below.
  - `__init__.py` — public API surface.
- `tests/marine_ops/marine_engineering/fowt_coupled/test_dlc_runner.py` — fixtures cover one DLC 1.x (power-production) and one DLC 6.x (parked) on the K01 spar config.
- `tests/marine_ops/marine_engineering/fowt_coupled/test_coupled_response.py` — analytic regression: at zero wave / steady wind, mean offset matches the static thrust × mooring stiffness reciprocal within tolerance.
- **Non-goals.** No solver kernel rewrite — this PR is a thin facade over OrcFxAPI plus the existing hydro modules. No OpenFAST integration (deferred to follow-up). No certification-grade fatigue (DNV-ST-0119 §7) — frequency-domain RAO + DLC enumeration only.

## Deliverables
- `src/digitalmodel/marine_ops/marine_engineering/fowt_coupled/__init__.py` — public API
- `src/digitalmodel/marine_ops/marine_engineering/fowt_coupled/models.py` — Pydantic config models
- `src/digitalmodel/marine_ops/marine_engineering/fowt_coupled/coupled_response.py` — coupling orchestrator
- `src/digitalmodel/marine_ops/marine_engineering/fowt_coupled/dlc_runner.py` — IEC DLC matrix
- `src/digitalmodel/marine_ops/marine_engineering/fowt_coupled/solvers/__init__.py`
- `src/digitalmodel/marine_ops/marine_engineering/fowt_coupled/solvers/orcaflex_backend.py`
- `src/digitalmodel/marine_ops/marine_engineering/fowt_coupled/solvers/openfast_backend.py` (stub)
- `tests/marine_ops/marine_engineering/fowt_coupled/test_dlc_runner.py`
- `tests/marine_ops/marine_engineering/fowt_coupled/test_coupled_response.py`
- `tests/marine_ops/marine_engineering/fowt_coupled/fixtures/k01_5mw_spar.yml` — derived from the existing K01 example
- `docs/domains/articles/fowt_coupled_response.md` — short explainer linking back to mapping doc

## Approach
1. **Spec phase.** Run `/gsd:spec-phase` against this plan to lock the OpenFAST-vs-OrcaFlex scope (issue body explicitly requests this). Smoke check: `cat docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md && ls docs/domains/orcaflex/examples/modular/K01/`.
2. **Models + DLC enum.** Create `models.py` and `dlc_runner.py` with no solver wiring. Smoke check: `cd digitalmodel && uv run python -c "from digitalmodel.marine_ops.marine_engineering.fowt_coupled.dlc_runner import IEC_61400_3_2_DLC; print(list(IEC_61400_3_2_DLC))"`.
3. **OrcaFlex backend.** Wrap K01 spar YAML wiring as a programmatic builder. Smoke check: `cd digitalmodel && uv run pytest tests/marine_ops/marine_engineering/fowt_coupled/test_dlc_runner.py::test_dlc_1_1_smoke -q` (gracefully skips if no OrcFxAPI license is present).
4. **Citation emission.** Wire `Citation(code_id="iec-61400-3-2", ...)` and `Citation(code_id="dnv-rp-0286", ...)` calls into `coupled_response.py` per `.claude/rules/calc-citation-contract.md`. Smoke check: `cd digitalmodel && uv run pytest tests/marine_ops/marine_engineering/fowt_coupled/ -q -k citation`.
5. **Analytic regression.** Add the static-thrust mean-offset test. Smoke check: `cd digitalmodel && uv run pytest tests/marine_ops/marine_engineering/fowt_coupled/test_coupled_response.py::test_static_offset_matches_mooring_stiffness -q`.
6. **Doc note.** Write `docs/domains/articles/fowt_coupled_response.md` summarising entry points, DLC coverage, and the deferred OpenFAST hook.

## Open questions
- **OpenFAST vs OrcaFlex-only.** Issue body says "Optional: thin OpenFAST link if licensing permits." Recommend OrcaFlex-only for v1 to avoid the OpenFAST Python binding tax (`pyOpenFAST` / `openfast_io`); preserve `openfast_backend.py` as a stub. **Needs user input.**
- **Time-domain vs frequency-domain default.** DNV-RP-0286 §5 expects nonlinear time-domain for power-production DLCs. v1 should expose both but default to frequency-domain for unit-test speed; full TD becomes opt-in. Confirm with user.
- **K01 reference — which subset.** Reference example has multiple sub-cases. Default: pick the steady-wind case for the regression baseline; loop in `/gsd:spec-phase` outputs.

## Acceptance Criteria
- [ ] Module enumerates IEC 61400-3-2 DLCs as a Python `Enum` covering at least DLC 1.1, 1.2, 1.6, 6.1, 6.2, 6.3.
- [ ] K01 5MW spar reference run reproducible from a Python entrypoint that takes `fixtures/k01_5mw_spar.yml`.
- [ ] Citations to `iec-61400-3-2` and `dnv-rp-0286` emit through `digitalmodel.citations` and resolve against the wiki pages from #574.
- [ ] Pytest fixtures cover one DLC 1.x and one DLC 6.x case; `uv run pytest tests/marine_ops/marine_engineering/fowt_coupled/ -q` is green (with OrcFxAPI-dependent tests skipping cleanly when the license is absent).
- [ ] OpenFAST backend exists as a stub that raises `NotImplementedError` with a clear message pointing to the follow-up issue.
