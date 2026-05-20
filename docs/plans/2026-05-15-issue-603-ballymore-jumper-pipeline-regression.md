# Plan: digitalmodel #603 - Add regression coverage for Ballymore spec-to-modular pipeline

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/603
**Status:** plan-review
**Tier:** T2
**Date:** 2026-05-15
**Parent/story:** #471
**Related:** #478, #506, #601, #602, #604

## Scope

Add regression tests that lock the intended Ballymore path:

```text
Ballymore domain spec.yml -> jumper calculations -> generator-ready ProjectInputSpec
-> ModularModelGenerator -> master.yml/includes
```

This is a test-focused issue. It should execute after #601 and #602, or be folded into #478 if the
approved integration issue is implemented as one combined pass.

## Resource Intelligence Summary

### Live issue state

Verified from GitHub issue #603 on 2026-05-15:

- State: open
- Labels: `cat:engineering`
- Acceptance criteria require both Ballymore specs, 27-section assertions, parseable generated YAML,
  no OrcFxAPI requirement, and a negative test for the current failure mode.

### Sources consulted

- `AGENTS.md` - repository test command convention is `PYTHONPATH=src uv run python -m pytest`.
- `docs/plans/2026-05-05-issue-478-orcaflex-modular-generator-integration.md` - broad integration
  issue this test issue may be folded into.
- `docs/plans/2026-05-15-issue-601-jumper-line-section-yaml-output.md` - local draft plan created in
  this workspace for preserving line-section YAML; it will not be visible on GitHub `main` until the
  planning artifacts are committed/pushed.
- `docs/plans/2026-05-15-issue-602-ballymore-jumper-projectinputspec-conversion.md` - local draft
  plan created in this workspace for the conversion module; it will not be visible on GitHub `main`
  until committed/pushed.
- `src/digitalmodel/marine_ops/installation/jumper_installation.py:198-209` - current key mismatch
  prevents `line_sections.yml` from being written.
- `src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py:82-118` - current
  `ProjectInputSpec` failure mode when no model type is defined.
- `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:140-180` - generator output
  contract.
- `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py` - existing
  semantic proof pattern for parseable generated jumper generic output.
- `tests/marine_ops/installation/test_jumper_lift.py` - existing jumper calculation test family.
- `docs/plans/README.md` - absent in this repository; follow existing standalone plan-file
  convention.

### Reproduction Evidence

Command run on 2026-05-15 America/Chicago from `/mnt/local-analysis/workspace-hub/digitalmodel`:

```bash
uv run python - <<'PY'
from pathlib import Path
import shutil
import yaml
from pydantic import ValidationError
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.marine_ops.installation.jumper_installation import run_pipeline

spec_paths = [
    Path('docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml'),
    Path('docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml'),
]

for spec_path in spec_paths:
    data = yaml.safe_load(spec_path.read_text())
    try:
        ProjectInputSpec(**data)
        print(f'{spec_path}: ProjectInputSpec PASS')
    except ValidationError as exc:
        first_error = exc.errors()[0]
        print(f'{spec_path}: ProjectInputSpec FAIL: {first_error["msg"]}')

    out = Path('/tmp/jumper-plan-repro') / spec_path.parent.name
    if out.exists():
        shutil.rmtree(out)
    result = run_pipeline(str(spec_path), output_dir=str(out), generate_report=False, run_go_no_go=False)
    print(f'{spec_path}: pipeline_output_files={sorted(Path(p).name for p in result.output_files)}')
    print(f'{spec_path}: line_sections_yaml_len={len(result.line_sections_yaml)}')
    print(f'{spec_path}: analysis_section_count={len(result.jumper_analysis.get("orcaflex_sections", []))}')
    print(f'{spec_path}: has_line_sections_file={(out / "line_sections.yml").exists()}')
PY
```

Tail of output:

```text
docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml: ProjectInputSpec FAIL: Value error, Either 'pipeline', 'riser', 'mooring', or 'generic' must be defined in spec
docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml: pipeline_output_files=['analysis_summary.json']
docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml: line_sections_yaml_len=0
docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml: analysis_section_count=27
docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml: has_line_sections_file=False
docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml: ProjectInputSpec FAIL: Value error, Either 'pipeline', 'riser', 'mooring', or 'generic' must be defined in spec
docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml: pipeline_output_files=['analysis_summary.json']
docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml: line_sections_yaml_len=0
docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml: analysis_section_count=27
docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml: has_line_sections_file=False
```

## Deliverable

A focused regression test module, implemented only after #601 and #602 have landed or in the same
integration branch after those code slices, proves the two Ballymore specs travel through the
generator-ready pipeline without a hard licensed OrcaFlex dependency and that the current
missing-model-type failure mode is explicitly covered.

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-05-15-issue-603-ballymore-jumper-pipeline-regression.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist |
| Review artifacts | `scripts/review/results/2026-05-15-plan-603-{claude,codex,gemini}.md` |
| Regression tests | `tests/solvers/orcaflex/modular_generator/test_ballymore_jumper_pipeline.py` |
| Possible shared fixtures | `tests/solvers/orcaflex/modular_generator/conftest.py` |

## Preconditions

- #601 should be implemented before closing this issue if tests assert `line_sections.yml` is written
  through `run_pipeline()`.
- #602 should be implemented before closing this issue if tests assert `build_modular_spec()` and
  `ModularModelGenerator` output.
- Local draft plan files for #601/#602 are convenience context only. If another executor cannot
  retrieve them from `main`, use GitHub issues #601/#602 and the import contract below as the
  authoritative prerequisites.
- Implementation mode: dependent-only. A standalone #603 branch must start by verifying #601 and #602
  are merged into the target branch. If they are not merged, stop and leave #603 blocked rather than
  committing always-on tests that intentionally fail.
- The expected #602 API contract for this test issue, as specified by the current #602 plan, is:
  `from digitalmodel.marine_ops.installation.jumper_to_modular_spec import build_modular_spec` and
  `build_modular_spec(spec_path: str | Path) -> dict`, plus
  `build_conversion_provenance(spec_path: str | Path, modular_data: dict) -> dict` for
  baseline-geometry assertions. This contract must be present in the approved and merged #602 work
  before #603 is implemented. If the merged #602 code exposes different public helpers or provenance
  keys, stop and update this #603 plan/review before adding always-on tests; do not add adapter
  guesses in the tests.
- If #478 is executed as an integrated pass, this issue can be closed by the #478 implementation only
  if all tests listed here are present and passing.

## Plan

0. Before writing tests, verify the target branch contains the merged #601/#602 code and the expected
   #602 public helpers. If not, stop with a blocked status and do not add failing CI tests.
1. After prerequisites are present, add the test module first, with explicit dependency imports:
   - Keep imports of `build_modular_spec` local to tests that need it so the no-license subprocess can
     install import guards before any conversion/generator modules load.
   - If `build_modular_spec` is missing, tests should fail normally rather than silently skip.
   - License-dependent tests are forbidden in this module.
2. Parametrize both specs:
   - `docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml`
   - `docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml`
3. Test calculation output:
   - Load the domain spec with `stage_1_load_spec(str(spec_path))` from
     `digitalmodel.marine_ops.installation.jumper_installation`.
   - Run `stage_2_calculate(spec)` from the same module.
   - Assert `orcaflex_sections` count is 27 for each spec and save the ordered list of
     `(line_type, length_m)` pairs for comparison with the converted model.
4. Test conversion output from #602:
   - Import `build_modular_spec` from
     `digitalmodel.marine_ops.installation.jumper_to_modular_spec`.
   - Call `build_modular_spec(spec_path: str | Path) -> dict`.
   - Validate with `ProjectInputSpec(**data)`.
   - Assert `spec.is_generic()` is true.
   - Assert `data["generic"]["lines"]` contains a single main jumper line named exactly `JumperLine`
     with a `properties` key named exactly
     `LineType, Length, TargetSegmentLength, PreBendCurvaturex, PreBendCurvaturey`.
   - Assert that table has exactly 27 rows; auxiliary connector/attachment objects must not be mixed
     into this 27-row count.
   - Assert the table's first two columns match the ordered calculation output line types and lengths
     within `1e-6 m`; this prevents a converter from emitting 27 placeholder rows.
   - Assert bend rows have nonzero `PreBendCurvaturey`, non-bend straight rows have zero curvature,
     and every first-column line type resolves to `data["generic"]["line_types"]`.
5. Test generation output:
   - Generate into `tmp_path`.
   - Assert `master.yml`, `inputs/parameters.yml`, and includes `01_general.yml`,
     `03_environment.yml`, and `20_generic_objects.yml` exist, matching the working generic jumper
     template's generator output set.
   - Parse `master.yml` and assert its include list names those same three include files; existence
     without a `master.yml` reference is not sufficient.
   - Parse every generated `*.yml` with `yaml.safe_load()`.
   - Inspect `includes/20_generic_objects.yml` and assert it contains a `Lines` object named
     `JumperLine` and the same 27-row compact line table.
   - Inspect `includes/03_environment.yml` and assert environment water depth/density come from the
     source spec.
   - Assert generated metadata/project text only where `metadata.name`,
     `metadata.description`, or `metadata.project` is emitted by the generator; use those exact source
     values as expectations.
   - Assert every `LineType` reference used by a generated line table resolves to a generated
     `LineTypes` entry.
   - Use the compact OrcaFlex key shape required by #602 for the main jumper line: inspect keys in
     each `Lines` entry that start with `LineType,`, select the key containing
     `PreBendCurvaturex`, and collect the first column from each row. If additional valid OrcaFlex
     line tables are emitted later, they need separate tests and must not dilute the 27-row main-line
     assertion.
6. Add a negative test documenting the current failure mode:
   - Direct `ProjectInputSpec(**yaml.safe_load(ballymore_spec))` raises a validation error containing
     "Either 'pipeline', 'riser', 'mooring', or 'generic' must be defined".
   - Preferred #602 behavior is non-mutating conversion, so this negative test should remain while the
     source Ballymore files are domain specs.
   - If an approved later change intentionally converts the source specs in place, replace this with a
     negative test that removes `generic`/all model-type fields from generated data and asserts the
     same missing-model-type validation error. Do not simply delete the negative coverage.
7. Add an explicit no-hard-dependency regression:
   - This test must run in a fresh Python subprocess launched by pytest with `PYTHONPATH=src`; do not
     implement it as an in-process fallback, because earlier tests in the same pytest process may have
     already imported generator modules.
   - At the top of the subprocess code, before importing any `digitalmodel` module, install a
     `sys.meta_path` finder that records and rejects any fullname equal to `OrcFxAPI` or starting with
     `OrcFxAPI.` by raising `ImportError("OrcFxAPI blocked for no-license regression")`. Also wrap
     `builtins.__import__` for belt-and-suspenders coverage. The test asserts both guards were active
     before the first `digitalmodel` import. Import attempts may be recorded as diagnostics; the
     failure condition is that the Ballymore conversion/generation path cannot complete when OrcFxAPI
     raises `ImportError`.
   - In that same fresh subprocess, loop over both Ballymore domain specs, run
     `stage_1_load_spec(str(spec_path))`, run `stage_2_calculate(spec)`, call `build_modular_spec()`,
     validate `ProjectInputSpec`, and generate with
     `ModularModelGenerator.from_spec(...).generate(Path(os.environ["BALLYMORE_TEST_TMP"]) /
     spec_path.parent.name)`.
   - Launch the subprocess with `cwd` set to the repository root, environment copied from
     `os.environ`, `PYTHONPATH` prepended with `src`, and `BALLYMORE_TEST_TMP` set to the pytest
     `tmp_path` string so the child process has a deterministic output root.
   - Assert the subprocess exits 0 and its stdout records both spec names. This proves the Linux code
     path for both Ballymore variants has no hard dependency on the licensed OrcaFlex Python API. Do
     not claim this proves Windows solver fidelity; licensed load/export remains #604.
8. Include the verification command in the test module docstring or closeout comment:

   ```bash
   PYTHONPATH=src uv run python -m pytest tests/solvers/orcaflex/modular_generator/test_ballymore_jumper_pipeline.py -q
   ```

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Add | `tests/solvers/orcaflex/modular_generator/test_ballymore_jumper_pipeline.py` | Regression coverage for the full Ballymore spec-to-generator path |
| Modify optional | `tests/solvers/orcaflex/modular_generator/conftest.py` | Shared spec path fixture only if it removes duplication |
| Modify optional | `tests/marine_ops/installation/conftest.py` | Only if calculation fixtures belong in the existing marine-ops test area |

## TDD Test List

| Test | Dependency | Expected GREEN |
|---|---|---|
| `test_ballymore_specs_calculate_27_orcaflex_sections` | current code | both specs report 27 sections |
| `test_ballymore_specs_direct_projectinputspec_failure_is_documented` | current code | direct domain-spec validation fails with the known model-type error |
| `test_build_modular_spec_validates_for_ballymore_specs` | #602 | converted dict validates as `ProjectInputSpec` |
| `test_converted_line_table_matches_calculation_sections` | #602 | line type and length columns match `stage_2_calculate()` output order |
| `test_generated_ballymore_model_tree_parses` | #602 | all generated YAML files parse |
| `test_generated_ballymore_model_contains_jumperline_and_environment` | #602 | generated `20_generic_objects.yml` contains `JumperLine`, 27 rows, and source water data |
| `test_generated_ballymore_line_type_references_resolve` | #602 | all line references resolve |
| `test_generated_ballymore_model_tree_has_expected_includes` | #602 | `01_general.yml`, `03_environment.yml`, and `20_generic_objects.yml` are included and present |
| `test_pipeline_line_sections_file_exists_for_ballymore_specs` | #601 | `run_pipeline(..., generate_report=False, run_go_no_go=False)` writes `line_sections.yml`; parsed YAML has root `line_sections` and 27 entries, proving the `orcaflex_sections_yaml` source key is wired |
| `test_ballymore_generation_does_not_require_orcfxapi` | #602 | conversion/generation works when `OrcFxAPI` imports raise `ImportError` |
| `test_plet_plem_baseline_geometry_status_is_explicit` | #602 | PLET-PLEM baseline-geometry status is asserted while source notes remain |

## Acceptance Criteria

- [ ] Test loads both Ballymore specs from `docs/domains/orcaflex/subsea/jumper/installation/`.
- [ ] Test asserts jumper calculation emits 27 sections for each spec.
- [ ] Test asserts converted line table line types and lengths match calculation output order, not
  merely row count.
- [ ] Test asserts generated model directory contains `master.yml`, `inputs/parameters.yml`, and
  includes `01_general.yml`, `03_environment.yml`, and `20_generic_objects.yml`.
- [ ] Test parses all generated YAML with `yaml.safe_load()`.
- [ ] Test asserts generated `20_generic_objects.yml` contains `JumperLine`, 27 compact line rows,
  and resolvable line-type references.
- [ ] Test asserts generated `03_environment.yml` contains source water depth/density.
- [ ] Test verifies no hard OrcFxAPI dependency is required on Linux by installing an import blocker
  before local conversion/generator imports and requiring the full path to complete.
- [ ] Negative test documents current direct-domain-spec failure mode or guards against missing
  model-type data.
- [ ] Test asserts PLET-PLEM baseline-geometry status remains explicit while source notes remain.
- [ ] CI command is documented in the test module docstring or issue closeout.

## Risks and Constraints

- This issue is test-only by intent. If it discovers code is still missing, implement the code under
  #601/#602/#478 rather than expanding this issue without updating its scope.
- Do not merge these tests into always-on CI before #601 and #602 land. Concrete execution paths:
  1. standalone #603: first verify #601/#602 are merged into the target branch, then add tests;
  2. integrated #478/#601/#602/#603 branch: commit implementation first, then commit #603 tests in the
     same branch before opening/merging the combined PR. Do not add temporary `xfail`/skip markers for
     missing `build_modular_spec`; missing prerequisites mean #603 execution has not started.
- Tests must not depend on output paths under `/tmp` except through `tmp_path` fixtures.
- `test_plet_plem_baseline_geometry_status_is_explicit` must call #602's
  `build_conversion_provenance()` and assert MF-PLET reports `baseline_geometry=False`, PLET-PLEM
  reports `baseline_geometry=True` while its raw source notes remain, and that generated metadata or
  provenance exposes that status.
- Required #602 provenance schema for this test:
  `provenance["config_name"]`, `provenance["source_spec"]`, `provenance["section_count"]`,
  `provenance["baseline_geometry"]`, `provenance["baseline_geometry_source"]`, and
  `provenance["assumptions"]`. `baseline_geometry_source` must be `"none"` for MF-PLET and either
  `"raw_source_notes"` or a future explicit field path for PLET-PLEM.

## Adversarial Review Summary

| Provider | Verdict | Artifact | Notes |
|---|---|---|---|
| Codex | APPROVE | `scripts/review/results/2026-05-15-plan-603-codex.md` | Final bounded rerun found no findings or blockers |
| Gemini | UNAVAILABLE | `scripts/review/results/2026-05-15-plan-603-gemini.md` | Gemini quota exhausted; stderr saved in `.err` artifact |

**Overall result:** PLAN-REVIEW - no MAJOR blockers remain in latest available review evidence;
Gemini was unavailable due quota. Await user approval before implementation.
