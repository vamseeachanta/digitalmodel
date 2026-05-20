# Plan: digitalmodel #601 - Preserve generated jumper line-section YAML in pipeline outputs

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/601
**Status:** plan-review
**Tier:** T2
**Date:** 2026-05-15
**Parent/story:** #471
**Related:** #478, #506, #602, #603

## Scope

Fix the existing jumper-installation pipeline so the 27 line sections already computed by
`jumper_lift.py` are written to `line_sections.yml`. This issue does not claim to generate a
full OrcaFlex model and must not broaden into `ProjectInputSpec` conversion; that is #602/#478.

## Resource Intelligence Summary

### Live issue state

Verified from GitHub issue #601 on 2026-05-15:

- State: open
- Labels: `cat:engineering`
- Acceptance criteria require both Ballymore specs to write parseable `line_sections.yml`
  containing exactly 27 entries under `line_sections`.

### Sources consulted

- `AGENTS.md` - repository test command convention is `PYTHONPATH=src uv run python -m pytest`.
- `docs/plans/2026-05-05-issue-478-orcaflex-modular-generator-integration.md` - broader
  generator integration plan; this plan is narrower.
- `docs/plans/2026-05-05-issue-506-passing-ship-jumper-installation-schemas.md` - adjacent
  schema-validation plan; not required for this key-mismatch fix.
- `src/digitalmodel/marine_ops/installation/jumper_lift.py:1138-1156` - computes
  `orcaflex_sections` and returns `orcaflex_sections_yaml`.
- `src/digitalmodel/marine_ops/installation/jumper_lift.py:1003-1032` - `generate_orcaflex_line_sections_yaml()`
  emits YAML with the root key `line_sections:`.
- `src/digitalmodel/marine_ops/installation/jumper_installation.py:198-209` - current
  `stage_4_generate_yaml()` reads `orcaflex_yaml`, not `orcaflex_sections_yaml`.
- `src/digitalmodel/marine_ops/installation/jumper_installation.py:230-235` - writes
  `line_sections.yml` only when `PipelineOutput.line_sections_yaml` is non-empty.
- `src/digitalmodel/marine_ops/installation/jumper_installation.py:290-346` - `run_pipeline()`
  wires the stage output into `stage_5_write_outputs()`.
- `docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml`
- `docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml`
- `tests/marine_ops/installation/` - existing home for jumper installation tests; this plan uses
  that path instead of the issue's seed path under `tests/solvers/orcaflex/modular_generator/`
  because #601 is a `marine_ops.installation` pipeline key-contract bug, not a generator behavior
  regression.
- `docs/plans/README.md` - absent in this repository; follow the existing standalone plan-file
  convention used by the current `docs/plans/*.md` files.

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

`run_pipeline(..., run_go_no_go=False)` writes a parseable `line_sections.yml` for both Ballymore
jumper specs, with exactly 27 entries under `line_sections`, without changing the scope or shape of
full model generation.

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-05-15-issue-601-jumper-line-section-yaml-output.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist |
| Review artifacts | `scripts/review/results/2026-05-15-plan-601-{claude,codex,gemini}.md` |
| Source fix | `src/digitalmodel/marine_ops/installation/jumper_installation.py` |
| Focused tests | `tests/marine_ops/installation/test_jumper_pipeline_line_sections.py` |

## Plan

1. Add tests before editing production code. The consumer/pipeline tests are expected to be RED at
   HEAD; the producer-side test is a GREEN guard proving the calculation stage already emits the
   intended key.
   - `test_stage_2_calculate_emits_orcaflex_sections_yaml_key` loading one Ballymore spec and
     asserting `stage_2_calculate()` returns a non-empty `orcaflex_sections_yaml`, contains 27
     `orcaflex_sections`, and does not rely on a producer-side `orcaflex_yaml` key.
   - `test_stage_4_generate_yaml_uses_orcaflex_sections_yaml_key` with a minimal dict containing
     `orcaflex_sections_yaml`.
   - `test_stage_4_generate_yaml_accepts_legacy_orcaflex_yaml_key` to cover the planned compatibility
     fallback.
   - Parametrized `test_run_pipeline_writes_line_sections_file_for_ballymore_specs` for both
     Ballymore specs, calling `run_pipeline(..., generate_report=False, run_go_no_go=False)`,
     asserting `line_sections.yml` exists, `yaml.safe_load()` succeeds, and
     `len(data["line_sections"]) == 27`.
2. Make the smallest production change in `stage_4_generate_yaml()`:
   - Prefer `results.get("orcaflex_sections_yaml")`.
   - Fall back to `results.get("orcaflex_yaml", "")` only to avoid breaking any undocumented caller
     still using the old key.
3. Keep `stage_5_write_outputs()` unchanged unless tests reveal the YAML string needs trailing
   newline normalization.
4. Run the focused test module:

   ```bash
   PYTHONPATH=src uv run python -m pytest tests/marine_ops/installation/test_jumper_pipeline_line_sections.py -q
   ```

5. Run a manual smoke matching the issue acceptance criteria:

   ```bash
   PYTHONPATH=src uv run python - <<'PY'
   from pathlib import Path
   import shutil
   import yaml
   from digitalmodel.marine_ops.installation.jumper_installation import run_pipeline

   for spec in [
       'docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml',
       'docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml',
   ]:
       out = Path('/tmp/jumper-line-sections') / Path(spec).parent.name
       if out.exists():
           shutil.rmtree(out)
       run_pipeline(spec, output_dir=str(out), generate_report=False, run_go_no_go=False)
       data = yaml.safe_load((out / 'line_sections.yml').read_text())
       print(spec, len(data['line_sections']))
   PY
   ```

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/marine_ops/installation/jumper_installation.py` | Use the actual result key emitted by `jumper_lift.py` |
| Add | `tests/marine_ops/installation/test_jumper_pipeline_line_sections.py` | Lock the key contract and Ballymore pipeline output |

## TDD Test List

| Test | Expected RED | Expected GREEN |
|---|---|---|
| `test_stage_2_calculate_emits_orcaflex_sections_yaml_key` | expected GREEN at HEAD; guard test | producer emits `orcaflex_sections_yaml` and 27 sections |
| `test_stage_4_generate_yaml_uses_orcaflex_sections_yaml_key` | returns empty string | returns the YAML value |
| `test_stage_4_generate_yaml_accepts_legacy_orcaflex_yaml_key` | expected GREEN at HEAD; fallback guard | old callers still work |
| `test_run_pipeline_writes_line_sections_file_for_ballymore_specs` | no `line_sections.yml` | file exists with 27 `line_sections` |

## Acceptance Criteria

- [ ] Both Ballymore specs write `line_sections.yml` through `run_pipeline()`.
- [ ] `line_sections.yml` parses with `yaml.safe_load()`.
- [ ] Parsed `line_sections` count is exactly 27 for each spec.
- [ ] Tests lock the `orcaflex_sections_yaml` key contract so the drift cannot recur.
- [ ] No implementation in this issue claims the generated YAML is a full OrcaFlex model.

## Risks and Constraints

- The pipeline prints status messages during tests; use `capsys` or assert files rather than brittle
  stdout text.
- The fix must not mask #602 by renaming this line-section YAML as a full `ProjectInputSpec`.
- The PLET-PLEM spec currently documents that some geometry values are MF-PLET baseline values; this
  issue only preserves current computed sections and does not validate workbook-derived geometry.

## Adversarial Review Summary

| Provider | Verdict | Artifact | Notes |
|---|---|---|---|
| Claude | UNAVAILABLE | `scripts/review/results/2026-05-15-plan-601-claude.md` | CLI timed out before a usable review |
| Codex | MINOR | `scripts/review/results/2026-05-15-plan-601-codex.md` | Initial TDD/fallback concerns addressed in this plan |
| Gemini | MINOR | `scripts/review/results/2026-05-15-plan-601-gemini.md` | Root-key and test-kwargs concerns addressed in this plan |

**Overall result:** PLAN-REVIEW - no MAJOR blockers remain in latest available reviews; await user
approval before implementation.
