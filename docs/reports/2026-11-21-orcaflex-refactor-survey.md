# OrcaFlex code survey and first bounded refactor

Refs #1824 (deepen engineering workflow modules around bounded seams). Related:
workspace-hub#2694 (canonical authority for duplicate modules). Base commit:
`299c8954f` on `main`, 21 Nov 2026. Line numbers below refer to that commit.

Scope: `src/digitalmodel/orcaflex/` (33 files) and `src/digitalmodel/solvers/orcaflex/`
(352 files), searched for OrcFxAPI use, model builders, post-processing and
report generation. No licensed solver was run. Findings marked *not established*
were not verified against an OrcaFlex installation.

## 1. Byte-identical duplicates

| Copy A | Copy B | Lines | Importers |
|---|---|---|---|
| `solvers/orcaflex/orcaflex_post_process.py` | `solvers/orcaflex/post_process/orcaflex_post_process.py` | 168 | none by module path; `_compat.py:162` maps the flat name to the `post_process` package |
| `solvers/orcaflex/post_process/postProcessPlotting.py:11` | `solvers/orcaflex/post_results/postProcessPlotting.py:11` | 62 | none in `src/`, `tests/`, `scripts/`, `examples/` |

`git diff --no-index` reports no difference for either pair.

## 2. Divergent versions of one module

| Older | Newer | Who imports which |
|---|---|---|
| `opp_time_series.py` (502 lines) | `opp_time_series_v2.py` (424) | older: `opp.py:29`, `opp_summary.py:15`, `workflows/orcaflex_reproduce_results.py:15`; newer: no importer |
| `orcaflex_optimized_parallel.py` (451) | `orcaflex_optimized_parallel_v2.py` (496) | newer: `comprehensive_benchmark.py:169`, `deploy_production_optimization.py:22`; older: no importer |
| `opp_summary.py` (286) | `opp_summary_superseded.py` (316) | `opp.py:24` imports `opp_summary`; the superseded file has no importer |

Both parallel modules define `run_optimized_parallel_analysis` (`:374` and `:423`).
The `_v2` and `_superseded` names are the suffix pattern that workspace-hub#2694 Phase C
intends to warn on. Which version of each pair carries the intended behaviour is not
established; the diff stats are 396+/473- and 326+/282-.

## 3. Dead or unrunnable code

- `solvers/orcaflex/OrcaFlexAnalysis.py:16-20` imports `common.*`, `custom.post_results.*`
  and `dataManager.post_results.*`. None of these packages exists in the repository, so
  the script cannot be imported. Lines 27 and 31 build Windows-separator relative paths.
- `solvers/orcaflex/post_results/` holds `temp.py` (5 lines),
  `DataFrame_to_image2_NotWorking.py` and a checked-in `Output.txt`. None has an importer.
- `orcaflex/reporting/sections/code_check.py:8` imports OrcFxAPI and never uses it.

## 4. Callers that must know internal file layout

- `orcaflex/reporting/sections/qa_summary.py:6` resolves `Path(__file__).parents[7]`.
  From `src/digitalmodel/orcaflex/reporting/sections/` that is two directories *above*
  the repository root, so `docs/domains/orcaflex/examples/qa` is never found inside
  the checkout and the section always reports "No QA results found". The sibling facade
  `orcaflex/qa.py:11` uses `parents[3]`, which is the repository root. Defect; not fixed
  here because the fix changes report output.
- `orcaflex/qa.py:9-26` loads `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py`
  by file path; library code depends on a documentation directory.
- `orcaflex/reporting/builder.py:60-66` falls back to a hard-coded Windows installation
  directory for OrcFxAPI 11.6 when `ORCAFLEX_API_PATH` is unset.
- `_compat.py:58,162` and `engine.py:240` route the flat name `orcaflex_post_process` to a
  package, so the engine basename and the module path are maintained separately.

## 5. Shallow duplicated helpers

- Two report stacks query the same model in different ways.
  `orcaflex/reporting/sections/*.py` select objects with `isinstance(o, OrcFxAPI.Line)`
  (ten comprehensions in five files at base, e.g. `model_summary.py:24-27`,
  `static_config.py:17,43`, `time_series.py:10-11`). `solvers/orcaflex/reporting/
  extractors/aggregator.py:215` selects with `obj.type == ofx.ObjectType.Line`. Which
  form the installed OrcFxAPI supports for every object type is *not established* here.
  The refactor below puts the section form in one function, so a later correction is
  a one-place change.
- `_render_table` is defined twice (`model_summary.py:5`, `static_config.py:5`); the
  Plotly `<div>`/`<script>` snippet is written twice (`range_graphs.py:50-53`,
  `time_series.py:30-33`).
- `_safe_attr` is defined four times in `solvers/orcaflex/reporting/extractors/`
  (`aggregator.py:295`, `boundary_conditions_extractor.py:93`, `loads_extractor.py:63`,
  `materials_extractor.py:110`) and `_safe_float` twice (`:101`, `:118`). Identical
  bodies. Candidate follow-up of the same shape as this PR.
- The section HTML does not escape object names (e.g. `static_config.py:31`);
  `solvers/orcaflex/reporting/section_builders/utils.py:3` does (`html.escape`).
- Test fakes are duplicated: `tests/orcaflex/reporting/test_orcaflex_reporting.py:9-60`
  redefines the fake OrcFxAPI classes already in `tests/orcaflex/conftest.py:18-128`.

## 6. Related to the catenary cluster of workspace-hub#2694

`orcaflex/mooring_design.py:256` (`solve_catenary`) is an inline single-segment catenary
solver exported from `orcaflex/__init__.py:44`. Whether it agrees with the canonical
catenary module named in that epic is not established here; it is listed for that
epic's Phase A inventory, not changed.

## 7. Refactor selected

**Choice.** One module, `orcaflex/reporting/sections/_common.py`, owns object selection
by OrcFxAPI type (`objects_of_type`, `lines`, `vessels`, `buoys`) and the two shared
HTML fragments (`kv_table`, `plotly_chart`), plus `attr_text`. Five sections call it:
model summary, static configuration, time series, range graphs and mooring loads.

**Why this one.** It removes ten copies of the selection rule and four copies of the
HTML fragments across five callers; the five sections shrink by 21 lines net and the
new module is about 100 lines. It has an
existing fake-OrcFxAPI fixture set, so behaviour can be pinned exactly without a
licence. The other candidates are either deletions with no caller (sections 1-3), which
need an owner decision on which version is canonical, or behaviour changes (section 4).

**Compatibility.** No module moved. The old private helpers `_render_table` (in
`model_summary` and `static_config`) and `_safe_get` (in `model_summary`) still resolve
through a module `__getattr__` that returns the shared function and emits
`DeprecationWarning`. OrcFxAPI is still imported at call time, so the package imports
without OrcaFlex and a missing OrcFxAPI still raises `ImportError` from each section.

**Behaviour.** Unchanged by construction and by test: the characterisation tests
compare 25 section outputs (five model variants by five sections) and one full report
byte-for-byte with HTML captured from the base commit.

## 8. Follow-ups proposed (not in this PR)

1. Fix `qa_summary.py:6` (`parents[7]` to `parents[5]`), with a test that resolves the
   QA directory inside the checkout.
2. Consolidate `_safe_attr`/`_safe_float` in `solvers/orcaflex/reporting/extractors/`.
3. Owner decision per pair in sections 1-2, then delete or shim the non-canonical copy.
4. Escape object names in the section HTML.
5. Establish on a licensed host which object-selection form OrcFxAPI supports, then
   correct `_common.objects_of_type` once.
