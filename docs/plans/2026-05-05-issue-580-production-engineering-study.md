# Plan: digitalmodel #580 — Production engineering study (literature, methods, implementation)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/580
**Status:** plan-review
**Tier:** T3 (multi-phase study deliverable; library extension of an existing module)

## Context
- **Standard / publisher / revision.** Not a single standard — this is a literature-and-methods consolidation across IPR (Vogel 1968, Fetkovich 1973), VLP (Hagedorn-Brown 1965, Beggs-Brill 1973, Gray), nodal analysis (Brown's *Technology of Artificial Lift*), and multiphase flow correlations. Reference texts: Brill & Mukherjee *Multiphase Flow in Wells*, Economides et al. *Petroleum Production Systems*.
- **Why it matters.** `src/digitalmodel/production_engineering/` already ships `vlp_correlations.py` (Hagedorn-Brown + Beggs-Brill), `ipr_models.py` (Vogel + Fetkovich + Linear PI + Composite), and `nodal_solver.py`. The gap WRK-5066 calls out is **(a) explicit method-selection rationale**, **(b) validation against published worked examples**, and **(c) a literature-survey artifact** that cites the canonical papers per acceptance criterion in the issue body. Today the modules implement formulas but do not surface a "why we picked Beggs-Brill over Hagedorn-Brown for this regime" decision artifact, and there is no single reference-cross-walk doc.
- **Dependencies on other issues.** None — self-contained within `production_engineering/`. Soft alignment with #581 (structural side of platform engineering) only at the boundary of "platform-mounted production system."

## Scope
- **Phase A — Literature gathering** (doc-only):
  - `docs/domains/production/production-engineering-literature.md` — ≥10 references with: citation, equation lineage, parameter definitions, applicability ranges, and the digitalmodel module that implements (or should implement) each method.
- **Phase B — Method assessment / selection** (doc + small code):
  - `docs/domains/production/production-engineering-method-selection.md` — comparison tables: Beggs-Brill vs Hagedorn-Brown vs Gray (VLP); Vogel vs Fetkovich vs composite (IPR). Recommends primary correlation per flow regime with rationale.
  - `src/digitalmodel/production_engineering/method_selector.py` — programmatic `select_vlp_correlation(regime, fluid_props) -> VLPCorrelation` and `select_ipr_model(reservoir_state, fluid_props) -> IPRModel`. Pure decision logic; does not re-implement the correlations.
- **Phase C — Implementation gaps + worked examples**:
  - `src/digitalmodel/production_engineering/vlp_correlations.py` — add **Gray correlation** (vertical gas-condensate well; gap today). Validate Beggs-Brill + Hagedorn-Brown against the Brill & Mukherjee textbook example pressure-traverse data (±5%).
  - `src/digitalmodel/production_engineering/ipr_models.py` — validate Vogel against the original 1968 JPT paper example (±2%); validate Fetkovich against an SPE 4529 isochronal-test example.
  - `src/digitalmodel/production_engineering/nodal_solver.py` — add a worked-example test reproducing one published nodal analysis case (suggest Economides Ch. 2 Example 2-9).
  - `tests/production_engineering/test_vlp_validation.py`, `tests/production_engineering/test_ipr_validation.py`, `tests/production_engineering/test_nodal_validation.py` — eight or more validation tests against published examples.
  - `tests/production_engineering/fixtures/` — published-example input data.
- **Non-goals.** No reservoir simulation. No artificial-lift (gas lift, ESP, rod pump) sizing — those are separate WRK threads. No multilateral / horizontal-well pressure drop (Joshi/Babu-Odeh territory; follow-up). No transient analysis. No PVT property prediction (Standing, Glaso etc. — that is a separate `reservoir/` module).

## Deliverables
- `docs/domains/production/production-engineering-literature.md` — literature survey (≥10 refs)
- `docs/domains/production/production-engineering-method-selection.md` — method-selection rationale
- `src/digitalmodel/production_engineering/method_selector.py` — selection logic
- `src/digitalmodel/production_engineering/vlp_correlations.py` — extended with Gray correlation
- `src/digitalmodel/production_engineering/ipr_models.py` — validation hooks added
- `src/digitalmodel/production_engineering/nodal_solver.py` — worked-example hook
- `tests/production_engineering/test_vlp_validation.py` — Brill & Mukherjee + Hagedorn-Brown examples
- `tests/production_engineering/test_ipr_validation.py` — Vogel 1968 + Fetkovich SPE 4529
- `tests/production_engineering/test_nodal_validation.py` — Economides Ch. 2 example
- `tests/production_engineering/test_method_selector.py` — selector decision tests
- `tests/production_engineering/fixtures/` — input data files (CSV/YAML)
- `examples/production_engineering/nodal_worked_example.py` — runnable demo with plot output

## Approach
1. **Phase A literature doc.** Compile ≥10 references with full bibliographic detail per the issue acceptance criteria. Smoke check: `wc -l docs/domains/production/production-engineering-literature.md` and `grep -cE '^\d+\.|^\-\s' $_` (≥10).
2. **Phase B method selection.** Comparison tables + selector module. Smoke check: `cd digitalmodel && uv run python -c "from digitalmodel.production_engineering.method_selector import select_vlp_correlation; print(select_vlp_correlation.__doc__)"`.
3. **Gray correlation.** Implement following the same dataclass pattern as Hagedorn-Brown / Beggs-Brill in `vlp_correlations.py`. Smoke check: `cd digitalmodel && uv run pytest tests/production_engineering/test_vlp_validation.py::test_gray_correlation_smoke -q`.
4. **Validation tests.** Wire each published example as a fixture; assert ±2–5% per acceptance bands. Smoke check: `cd digitalmodel && uv run pytest tests/production_engineering/ -q`.
5. **Worked-example demo.** Add `examples/production_engineering/nodal_worked_example.py` producing the Economides nodal-analysis plot. Smoke check: `cd digitalmodel && uv run python examples/production_engineering/nodal_worked_example.py --output /tmp/nodal.png && ls -la /tmp/nodal.png`.

## Open questions
- **Gray correlation revision.** Original 1974 Texaco internal report vs the GLR-corrected version in Beggs's *Production Optimization*. Default: GLR-corrected. **Needs user input** if there's a contractual preference.
- **Validation tolerance.** Issue acceptance demands "validated against published examples" but doesn't specify a tolerance. Default: ±2% for closed-form (IPR), ±5% for empirical (VLP). Confirm.
- **Plot output format.** The issue mentions "nodal analysis plots" — Matplotlib PNG vs HTML interactive (Plotly)? Default: matplotlib PNG to stay dependency-light; flag Plotly as follow-up.
- **Child WRKs.** Issue body lists "Literature gathering / Method assessment / Python implementation" as TBD child WRKs. Recommend they remain phases inside this single PR rather than separate issues, since the work is small and tightly coupled.

## Acceptance Criteria
- [ ] `production-engineering-literature.md` cites ≥10 references including Beggs & Brill (1973), Hagedorn-Brown (1965), Vogel (1968), Fetkovich (1973), Brown's Artificial Lift, Brill & Mukherjee, Economides — explicitly per the issue body.
- [ ] `production-engineering-method-selection.md` recommends a primary correlation per flow regime with documented rationale (Beggs-Brill vs Hagedorn-Brown vs Gray; Vogel vs Fetkovich).
- [ ] Gray correlation lands in `vlp_correlations.py` with at least one published-example validation test passing within ±5%.
- [ ] ≥8 unit tests across VLP / IPR / nodal modules validate against published examples.
- [ ] `examples/production_engineering/nodal_worked_example.py` runs and produces a nodal analysis plot.
