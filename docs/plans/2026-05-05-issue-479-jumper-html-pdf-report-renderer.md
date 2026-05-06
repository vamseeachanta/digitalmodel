# Plan: digitalmodel #479 — HTML/PDF report renderer for jumper installation analysis

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/479
**Status:** plan-review
**Tier:** T3 (module-feature)
**Parent:** #471

## Context

`src/digitalmodel/solvers/orcaflex/reporting/renderers/jumper.py` is a 20-line stub that subclasses `BaseRenderer` and only customizes the `dynamic-results` section with a passthrough lambda. The base renderer (`base.py`) wires the canonical 17-section layout used by `installation.py`, `mooring.py`, etc. — these inject section overrides via the same `get_section_config` pattern. The jumper renderer must extend that pattern to deliver the 11-section professional report (Cover, Executive Summary, Pipe Properties, Geometry, Weight Tally, Rigging, Crane Configuration, OrcaFlex Sections, COG per KIT, Go/No-Go, Calc References) the issue specifies.

`run_jumper_analysis()` in `marine_ops/installation/jumper_lift.py` already produces every numeric block the report needs (kit weights, COG, 27 OrcaFlex sections, sling stiffness, spreader bar). `evaluate_go_no_go` in `marine_ops/installation/go_no_go.py` produces 12-criterion `GoNoGoDecision` records ready to render. `jumper_installation.py` PipelineConfig already exposes `generate_report: bool = True` — the wiring point is `_generate_report` (currently a placeholder).

`docs/marketing/PORTFOLIO_CAPABILITIES.md` is the visual reference for client-facing layout.

## Plan

1. **Inventory existing renderer machinery.** Read `solvers/orcaflex/reporting/renderers/{base,installation,mooring,jumper}.py`, `section_builders.py`, and `models.py` (`OrcaFlexAnalysisReport`). Capture: section-config schema, builder signature `(report, include_plotlyjs) -> str`, how `installation.py` injects extra HTML for `geometry-stinger` and `dc-overbend`. The new `JumperRenderer` will follow that exact contract — no new framework, just additional injection points and one extra renderer entry path for jumper-only sections.

2. **Author `JumperReportData` adapter.** New module `src/digitalmodel/marine_ops/installation/jumper_report.py`:
   - `@dataclass JumperReportData(jumper_name, jumper_analysis, go_no_go, sections, generated_at)`
   - `from_pipeline_output(po: PipelineOutput) -> JumperReportData` — flattens `run_jumper_analysis()` dict + `GoNoGoDecision` into render-ready fields
   - `as_orcaflex_report(self) -> OrcaFlexAnalysisReport` — bridge into the existing renderer, populating only the fields the 11 jumper sections need
   This keeps `JumperRenderer` thin; data-shaping lives next to the source modules.

3. **Extend `JumperRenderer`.** Edit `src/digitalmodel/solvers/orcaflex/reporting/renderers/jumper.py`:
   - Override `get_section_config` to inject jumper-specific builders for `geometry` (A-G segments + bend arcs + clamp locations table), `static-results` (Weight Tally + Rigging), `design-checks` (Go/No-Go criteria table), and add a new `cog-per-kit` section.
   - New private builders `_build_jumper_geometry`, `_build_jumper_weight_tally`, `_build_jumper_rigging`, `_build_jumper_crane_config`, `_build_jumper_orcaflex_sections`, `_build_jumper_cog_per_kit`, `_build_jumper_go_no_go` — each emits a `<section id="...">` with a Plotly chart where it adds insight (weight breakdown pie, section-length bar chart, sling stiffness curve).
   - Plotly via the `include_plotlyjs="cdn"` channel already used by base; no new deps.

4. **PDF path.** Add `to_pdf(html_str, out_path)` helper in `solvers/orcaflex/reporting/__init__.py` (or extend the existing render helper). Use **weasyprint** as primary (already an indirect dep of several reporting paths — confirm via `uv pip list | grep weasyprint`). If weasyprint absent, fall back to running Chrome headless via `python -m playwright` as a tier-2; expose a `pdf_engine: str = "auto"` arg. Add `weasyprint>=62` to `[project.optional-dependencies].reporting` in `pyproject.toml` if missing.

5. **Wire into pipeline.** Edit `marine_ops/installation/jumper_installation.py` `_generate_report` (currently placeholder around line 200-250) to instantiate `JumperReportData.from_pipeline_output(po)`, build `OrcaFlexAnalysisReport`, run `JumperRenderer().render(...)`, write `<output_dir>/<jumper_name>_report.html` and (if PDF requested) `<jumper_name>_report.pdf`. Return paths in `PipelineOutput.report_path` and `output_files`.

6. **Tests.** New `tests/solvers/orcaflex/reporting/test_jumper_renderer.py`:
   - `test_jumper_renderer_emits_all_11_sections` — assert every required section ID present in HTML
   - `test_jumper_renderer_includes_go_no_go_table` — render with mocked `GoNoGoDecision`, assert PASS/FAIL labels in HTML
   - `test_jumper_pipeline_writes_html` — full pipeline against `ballymore_mf_plet/spec.yml` (skip PDF on environments without weasyprint via `pytest.importorskip`)

7. **Smoke check.** `uv run python -m digitalmodel.marine_ops.installation.jumper_installation docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml --output-dir /tmp/jumper-report` produces `/tmp/jumper-report/ballymore_mf_plet_jumper_report.html`. Open in browser, verify Plotly charts render, no broken section anchors.

## Acceptance Criteria

- [ ] `JumperRenderer` overrides `get_section_config` and emits jumper-specific HTML for all 11 sections listed in the issue
- [ ] HTML output includes Plotly weight-breakdown pie + section-length bar + Go/No-Go status table
- [ ] PDF output produced via weasyprint primary path; missing-engine fallback raises a clear error
- [ ] `jumper_installation.py` pipeline produces `<jumper>_report.html` (and PDF when engine present) end-to-end
- [ ] `tests/solvers/orcaflex/reporting/test_jumper_renderer.py` adds ≥ 3 tests, all green
- [ ] Existing `tests/solvers/orcaflex/reporting/test_jumper_fixture_*` tests still pass

## Open Questions

- Brand styling: should the cover page consume `docs/marketing/PORTFOLIO_CAPABILITIES.md` palette directly, or a separate `assets/jumper_report.css`? Default to inline CSS in renderer pending design input.
