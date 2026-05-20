# Plan: digitalmodel #604 - Validate generated Ballymore jumper model on licensed OrcaFlex runner

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/604
**Status:** plan-review
**Tier:** T2
**Date:** 2026-05-15
**Parent/story:** #471
**Related:** #478, #602, #603, workspace-hub#2708

## Scope

Run the generated Ballymore jumper model on a licensed OrcaFlex environment, preferably
`licensed-win-1`, and record load/export/statics evidence. This is an operational validation issue;
it should not start until #602/#603 produce generated `master.yml` and regression tests on Linux.

Linux-only validation remains limited to schema validation and deterministic YAML generation.

## Resource Intelligence Summary

### Live issue state

Verified from GitHub issue #604 on 2026-05-15:

- State: open
- Labels: `cat:engineering`
- Acceptance criteria require generating both Ballymore models, loading `master.yml` with OrcFxAPI on
  licensed Windows, exporting `.dat` or `.sim` where appropriate, and writing a validation report.

### Related queue issue

Fetched workspace-hub#2708 on 2026-05-15:

- `workspace-hub#2708` is open and scoped to validating generic OrcaFlex solver-queue dispatch on
  `licensed-win-1`.
- Its acceptance criteria are queue-specific: submit one OrcaFlex job, prove `queue/completed/`
  contains `result.yaml` and `.sim`, and document the smoke run.
- This issue is domain-specific for Ballymore jumper generated models and should reuse queue proof
  from #2708 if available rather than duplicating queue infrastructure hardening.

### Sources consulted

- `AGENTS.md` - repository test command convention is `PYTHONPATH=src uv run python -m pytest`.
- `docs/plans/2026-05-15-issue-602-ballymore-jumper-projectinputspec-conversion.md` - local draft
  generator-ready model precursor created in this workspace; it will not be visible on GitHub `main`
  until committed/pushed.
- `docs/plans/2026-05-15-issue-603-ballymore-jumper-pipeline-regression.md` - local draft Linux
  regression precursor created in this workspace; it will not be visible on GitHub `main` until
  committed/pushed.
- `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:140-180` - generator writes
  `master.yml`, includes, and `inputs/parameters.yml`.
- `docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml` - reference generic jumper
  template; Linux generator proof succeeds, but it is not a licensed OrcaFlex load proof.
- `docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml`
- `docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml`
- `docs/reports/` - local checkout report root for validation evidence; create it if absent on the
  execution checkout or branch.
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

ref = Path('docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml')
ref_spec = ProjectInputSpec(**yaml.safe_load(ref.read_text()))
print(f'{ref}: ProjectInputSpec PASS generic={ref_spec.is_generic()} line_types={len(ref_spec.generic.line_types)} lines={len(ref_spec.generic.lines)}')
out = Path('/tmp/jumper-reference-template-generate')
if out.exists():
    shutil.rmtree(out)
ModularModelGenerator.from_spec(ref_spec).generate(out)
includes = sorted(p.name for p in (out / 'includes').glob('*.yml'))
print(f'{ref}: generate PASS master_exists={(out / "master.yml").exists()} includes={includes}')
PY
```

Tail of output:

```text
docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml: ProjectInputSpec FAIL: Value error, Either 'pipeline', 'riser', 'mooring', or 'generic' must be defined in spec
docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml: ProjectInputSpec FAIL: Value error, Either 'pipeline', 'riser', 'mooring', or 'generic' must be defined in spec
docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml: ProjectInputSpec PASS generic=True line_types=4 lines=3
docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml: generate PASS master_exists=True includes=['01_general.yml', '03_environment.yml', '20_generic_objects.yml']
```

Licensed OrcaFlex reproduction proof is N/A at planning time because the current Linux environment
reports OrcFxAPI/license unavailable and #602 has not yet generated Ballymore `master.yml` artifacts.
This issue exists to capture that licensed proof after prerequisites land.

## Deliverable

A validation report records, for both generated Ballymore jumper models, whether licensed OrcaFlex can
load `master.yml`, export `.dat` or `.sim`, and optionally calculate statics. Failures must include
the exact OrcaFlex error and a follow-up fix issue.

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-05-15-issue-604-ballymore-licensed-orcaflex-validation.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist |
| Review artifacts | `scripts/review/results/2026-05-15-plan-604-{claude,codex,gemini}.md` |
| Generated model root | `output/ballymore_mf_plet/` and `output/ballymore_plet_plem/` or run-specific temp path |
| Validation report | `docs/reports/2026-05-15-ballymore-jumper-orcaflex-validation.md` or dated closeout path |
| Validation helper | `scripts/orcaflex/validate_ballymore_jumper_models.py` |
| Report-schema test | `tests/solvers/orcaflex/modular_generator/test_ballymore_validation_report.py` |

## Preconditions

- #602 has produced generator-ready Ballymore `master.yml` and includes for both specs.
- The exact #602 generation API is available:
  `from digitalmodel.marine_ops.installation.jumper_to_modular_spec import build_modular_spec` and
  `build_modular_spec(spec_path: str | Path) -> dict`. If #602 lands a different public API, update
  this plan before execution or consume already-generated `master.yml` artifact paths directly.
- #603 has locked Linux regression coverage and no OrcFxAPI dependency.
- Either `licensed-win-1` has direct repo checkout access at `D:/workspace-hub/digitalmodel` or the
  generated model package is transferred with all includes intact.
- If using the git-backed solver queue, workspace-hub#2708 should have proven baseline OrcaFlex queue
  dispatch first.

## Plan

1. Add a tiny report validator/test before the licensed run:
   - The report must include repo commit SHA, generation command, generated model paths, host name,
     OrcaFlex/OrcFxAPI version if available, load status per model, export paths, statics status, and
     follow-up issue links for any failure.
   - Implement the report writer/parser in `scripts/orcaflex/validate_ballymore_jumper_models.py`
     using simple structured records, for example `ValidationReport` plus one `ValidationRow` per
     model, and have the test module import those helpers to validate required keys.
   - Keep this as local validation/reporting coverage; do not mock OrcFxAPI as proof of licensed
     behavior.
2. Generate both Ballymore models from repository code after #602 lands:

   ```bash
   PYTHONPATH=src uv run python - <<'PY'
   from pathlib import Path
   import shutil
   from digitalmodel.marine_ops.installation.jumper_to_modular_spec import build_modular_spec
   from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
   from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

   for spec_path in [
       Path('docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml'),
       Path('docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml'),
   ]:
       data = build_modular_spec(spec_path)
       spec = ProjectInputSpec(**data)
       model_key = spec_path.parent.name
       out = Path('output') / model_key
       if out.exists():
           shutil.rmtree(out)
       ModularModelGenerator.from_spec(spec).generate(out)
       print(out / 'master.yml')
   PY
   ```

3. Transfer or sync generated outputs to licensed Windows if not generated there.
4. On licensed Windows, run a direct load/export smoke through
   `scripts/orcaflex/validate_ballymore_jumper_models.py`. If the helper cannot be used on the target
   host, use the same helper body via a valid PowerShell here-string piped to Python. The fallback
   body must attempt both models independently, capture success/failure per model, write or print a
   report row for each model, and exit nonzero only after both rows have been collected:

   ```powershell
   @'
   from pathlib import Path
   import json
   import os
   import platform
   import subprocess
   import sys
   import traceback
   import yaml
   import OrcFxAPI

   repo = Path('D:/workspace-hub/digitalmodel')
   os.chdir(repo)
   commit = subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip()
   host = platform.node()
   orcfxapi_version = getattr(OrcFxAPI, '__version__', 'unknown')
   required_includes = {'01_general.yml', '03_environment.yml', '20_generic_objects.yml'}
   failures = 0
   for master in [
       repo / 'output/ballymore_mf_plet/master.yml',
       repo / 'output/ballymore_plet_plem/master.yml',
   ]:
       row = {
           'commit': commit,
           'host': host,
           'python': sys.executable,
           'orcfxapi_version': orcfxapi_version,
           'master': str(master),
           'status': 'not-run',
           'objects': None,
           'dat': None,
           'error': None,
       }
       try:
           master_data = yaml.safe_load(master.read_text())
           include_names = {Path(item).name for item in master_data.get('Include', [])}
           missing = sorted(required_includes - include_names)
           if missing:
               raise RuntimeError(f'missing required includes in master.yml: {missing}')
           for include_ref in master_data.get('Include', []):
               include_path = master.parent / include_ref
               if not include_path.exists():
                   raise RuntimeError(f'missing include file referenced by master.yml: {include_ref}')
           if not (master.parent / 'inputs' / 'parameters.yml').exists():
               raise RuntimeError('missing inputs/parameters.yml')
           model = OrcFxAPI.Model()
           model.LoadData(str(master))
           row['objects'] = len(model.objects)
           dat_path = master.with_suffix('.dat')
           model.SaveData(str(dat_path))
           row['dat'] = str(dat_path)
           row['status'] = 'load-export-pass'
       except Exception as exc:
           failures += 1
           row['status'] = 'load-export-fail'
           row['error'] = f'{type(exc).__name__}: {exc}'
           row['traceback_tail'] = traceback.format_exc().splitlines()[-6:]
       print(json.dumps(row, sort_keys=True))
   raise SystemExit(1 if failures else 0)
   '@ | python -
   ```

5. Record source-data provenance before interpreting a successful load:
   - MF-PLET can be reported as a generated Ballymore MF-PLET load/export proof.
   - PLET-PLEM must be marked as "load proof for current baseline-geometry placeholder" while its
     spec still says `Segment lengths need verification` and `Using MF-PLET baseline until workbook
     is converted`.
   - The report must not claim PLET-PLEM domain-geometry validation until workbook-derived geometry
     has landed in the source spec.
6. If load/export succeeds and statics is in scope for the generated model, run `CalculateStatics()`
   separately and record convergence status. Statics is in scope only when the helper is invoked with
   a `--run-statics` flag or `BALLYMORE_RUN_STATICS=1`; default validation is load/export only. Do
   not hide a load failure behind a statics attempt.
   A `.dat` export comes from `SaveData(...)`. A `.sim` artifact requires an actual dynamic run
   (`RunSimulation()` followed by `SaveSimulation(...)`) or a solver-queue run that records the `.sim`
   output; do not call a `.dat` export a `.sim` proof.
7. If using the solver queue after workspace-hub#2708:
   - Submit the generated `.dat` or model package as a queue job.
   - Record `queue/pending`, `queue/completed` or `queue/failed`, `result.yaml`, and `.sim` artifact
     paths.
8. Write the validation report under `docs/reports/` or
   `docs/domains/orcaflex/subsea/jumper/installation/validation/`.
   - The helper must write a durable JSON report and a concise Markdown rendering from the same data.
     Required JSON shape:
     `{"schema_version": 1, "commit": str, "generation_command": str, "host": str, "cwd": str,
     "python": str, "orcfxapi_version": str, "status": "validation-passed|blocked|failed",
     "package_manifest": [ManifestEntry, ...], "rows": [ValidationRow, ...]}`. Each manifest entry
     must include `model_key`, `path`, `sha256`, and `bytes` for the exact generated `master.yml`, all
     files referenced by `master.yml` `Include`, and `inputs/parameters.yml`. Each row must include
     `model_key`, `master`, `status`, `objects`, `dat`, `sim`, `statics_status`,
     `source_data_status`, `error`, and `followup_issue`.
   - The PowerShell fallback may print JSON lines with the same row keys if it cannot write the final
     repo report directly, but those rows must be copied into the committed JSON/Markdown report
     before closeout.
9. If either model fails to load/export/statics:
   - Capture exact OrcaFlex error text.
   - Identify failing include/object name if possible.
   - File a focused fix issue before closing #604.
   - Keep #604 open or explicitly mark it blocked; do not close #604 as validation-passed until both
     models load/export successfully. A failure report plus follow-up issue is useful evidence, but
     it is not successful validation.

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Add | `docs/reports/YYYY-MM-DD-ballymore-jumper-orcaflex-validation.md` | Durable licensed validation evidence |
| Add | `docs/reports/YYYY-MM-DD-ballymore-jumper-orcaflex-validation.json` | Structured report consumed by tests and rendered to Markdown |
| Add | `scripts/orcaflex/validate_ballymore_jumper_models.py` | Repeatable direct OrcFxAPI load/export helper plus structured report writer/parser |
| Add | `tests/solvers/orcaflex/modular_generator/test_ballymore_validation_report.py` | Report shape and required-field coverage |

## TDD Test List

| Test | Expected RED | Expected GREEN |
|---|---|---|
| `test_ballymore_validation_report_requires_commit_host_and_model_rows` | report parser/helper absent | report contains required metadata and one row per model |
| `test_ballymore_validation_report_requires_failure_followup_links` | missing failure follow-up allowed | failed model rows require a follow-up issue URL |
| `test_validate_script_lists_both_expected_master_paths` | helper absent | helper enumerates both Ballymore `master.yml` paths |
| `test_validate_script_checks_package_integrity_before_load` | missing includes can be misdiagnosed | helper verifies `master.yml` includes, include files, and `inputs/parameters.yml` before `LoadData` |
| `test_validate_script_attempts_all_models_on_failure` | helper can abort on first exception | mocked helper records rows for both models even if first load fails |
| `test_validate_script_records_load_export_status_and_errors` | helper can list paths only | mocked OrcFxAPI records `LoadData`, `SaveData`, `.dat` path, object count, exact error text, and nonzero exit on failure |
| `test_validate_script_writes_report_rows` | helper can print ad hoc output only | helper writes required report schema or structured rows consumed by the report validator |
| `test_validate_script_records_runtime_environment` | Windows proof can be unreproducible | report records cwd, Python executable, host, commit SHA, and OrcFxAPI version/import status |
| `test_validation_report_records_package_manifest_hashes` | loaded package identity can drift | report records SHA-256 and byte counts for each loaded `master.yml`, include, and parameters file |
| `test_validation_report_requires_success_before_closeout` | failure capture can be treated as pass | report status distinguishes validation-passed from blocked/failure-documented |
| `test_validation_report_marks_plet_plem_placeholder_geometry` | provenance can be overstated | PLET-PLEM row records baseline-geometry status until workbook geometry lands |
| `test_validation_report_distinguishes_dat_from_sim` | export semantics can be ambiguous | `.sim` rows require `RunSimulation()`/queue evidence |

## Acceptance Criteria

- [ ] Ballymore MF-PLET model YAML is generated from repo code.
- [ ] Ballymore PLET-PLEM model YAML is generated from repo code.
- [ ] Evidence is captured for both generated `master.yml` files. Exact load/export failure text and
  a linked follow-up issue are required for failed rows, but failure evidence does not count as
  validation-passed completion.
- [ ] Successful #604 completion requires both generated `master.yml` files to load/export with
  OrcFxAPI on licensed Windows.
- [ ] The committed report records a package manifest with SHA-256 hashes and byte counts for the
  exact generated files loaded on Windows.
- [ ] Package integrity is checked before `LoadData`: `master.yml` includes the expected files, each
  include exists, and `inputs/parameters.yml` exists.
- [ ] Successful load exports `.dat` through `SaveData(...)`; `.sim` is recorded only if
  `RunSimulation()`/`SaveSimulation(...)` or the solver queue actually produced a `.sim` artifact.
- [ ] The validation helper or fallback attempts both models independently and records one structured
  row per model even when one load/export fails.
- [ ] Runtime provenance records licensed host, working directory, Python executable, commit SHA, and
  OrcFxAPI version/import status.
- [ ] If `CalculateStatics()` is attempted, convergence status and basic object counts are recorded.
- [ ] If load/statics fails, a fix issue is filed with the failing include/object name when known.
- [ ] #604 closes as `validation-passed` only when both generated models load/export successfully; if
  a failure is captured, the evidence/report requirement may be satisfied but #604 remains
  open/blocked with the focused follow-up issue linked.
- [ ] Report records source-data status per model and explicitly labels PLET-PLEM as baseline geometry
  until workbook-derived segment lengths are implemented.
- [ ] A concise validation report is committed under `docs/reports/` or the jumper validation docs
  path.

## Risks and Constraints

- This cannot be fully verified on the current Linux host; treat Linux generator proof as necessary
  but insufficient.
- Queue validation in workspace-hub#2708 and domain validation here are separate. Do not block this
  issue on queue use if direct licensed Windows load/export gives better domain evidence.
- Generated output paths under `output/` are normally runtime artifacts. Commit the validation report,
  not large generated model outputs, unless a separate artifact-retention plan approves them.

## Adversarial Review Summary

| Provider | Verdict | Artifact | Notes |
|---|---|---|---|
| Codex | MINOR | `scripts/review/results/2026-05-15-plan-604-codex.md` | Final minor JSON/report-path notes patched after review |
| Gemini | UNAVAILABLE | `scripts/review/results/2026-05-15-plan-604-gemini.md` | Gemini quota exhausted; stderr saved in `.err` artifact |

**Overall result:** PLAN-REVIEW - no MAJOR blockers remain in latest available review evidence;
Gemini was unavailable due quota. Await user approval before implementation.
