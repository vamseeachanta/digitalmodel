# Plan: digitalmodel #704 — Repair `tests-orcaflex` baseline failures

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/704
**Parent:** https://github.com/vamseeachanta/digitalmodel/issues/700
**Status:** draft
**Tier:** T2 (focused baseline repair across tests plus two OrcaFlex modules)
**Client:** N/A
**Project:** N/A
**Lane:** codex

## Resource Intelligence Summary

This issue will repair the `tests-orcaflex` shard selected by OrcaFlex-scoped
CI routing. It will not change domain routing, workflow orchestration, or the
`tests-orcaflex-solver` shard; those are covered by
https://github.com/vamseeachanta/digitalmodel/issues/703,
https://github.com/vamseeachanta/digitalmodel/issues/705, and
https://github.com/vamseeachanta/digitalmodel/issues/706.

Evidence gathered on 2026-06-12 from branch
`chore/704-orcaflex-baseline-plan` at
`41289638e85572cb9397bd3c052418888ac8d492`:

- `tests/orcaflex/test_mooring_design.py` exercises `solve_catenary()` and
  `MooringLineDesign.estimate_catenary()`.
- `src/digitalmodel/orcaflex/mooring_design.py` currently uses a direct
  Newton-like pretension iteration that evaluates `math.cosh(V / H)` without a
  safe bound, so low `H` iterates can overflow.
- `test_catenary_grounded_length` currently asks the no-pretension API to infer
  seabed contact from only water depth, line length, and submerged weight.
  Without anchor radius or top tension, that geometry is underdetermined; the
  current no-pretension formula is an all-suspended estimate by construction.
- `tests/orcaflex/test_installation_analysis.py::TestDAF::test_daf_increases_with_heave`
  compares two cases that both clamp to the current DAF cap of `5.0`.
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator` imports through the
  compatibility alias `digitalmodel.agents.orcaflex`, but the current
  implementation lives under `digitalmodel.workflows.agents.orcaflex`.
  The import alias works; the stale part is the expected generator contract.
- `src/digitalmodel/workflows/agents/orcaflex/generators/base_files.py`
  generates a 13-file modular wrapper/data-file set. The tests expect an older
  9-file set with methods such as `generate_var_data()` and `generate_vessel()`.
- This checkout does not contain `docs/plans/README.md` or
  `docs/plans/_template-issue-plan.md`; the plan will follow the existing
  `digitalmodel` standalone plan-file pattern under `docs/plans/`.

### Reproduction Proofs

Focused command:

```bash
uv run --no-sources --with-editable . python -m pytest \
  tests/orcaflex/test_mooring_design.py::TestCatenary::test_catenary_with_pretension \
  tests/orcaflex/test_mooring_design.py::TestCatenary::test_catenary_grounded_length \
  tests/orcaflex/test_installation_analysis.py::TestDAF::test_daf_increases_with_heave \
  tests/test_orcaflex_agent.py::TestBaseFileGenerator \
  -q -p no:randomly -p no:sugar --tb=short
```

Result:

```text
8 failed, 1 passed in 2.11s

OverflowError: math range error
assert result.grounded_length > 0  # current value 0.0
assert 5.0 > 5.0                  # both DAF cases hit the cap
AttributeError: 'BaseFileGenerator' object has no attribute 'generate_var_data'
AttributeError: 'BaseFileGenerator' object has no attribute 'generate_vessel'
AttributeError: 'tuple' object has no attribute 'exists'
AssertionError: assert len(files) == 9  # current generator returns 13 files
```

Full shard command from the issue:

```bash
uv run --no-sources --with-editable . python -m pytest \
  tests/orcaflex/ tests/test_orcaflex_agent.py \
  -q -p no:randomly -p no:sugar --tb=short
```

Result:

```text
9 failed, 276 passed, 1 skipped in 3.17s

Additional full-shard failure:
tests/orcaflex/test_mooring_design.py::TestMooringLineDesign::test_estimate_catenary
  OverflowError: math range error
```

The reproduction created a local `.venv` and changed `uv.lock`; both were
removed/restored after evidence capture so the planning worktree stays clean.

## Artifact Map

- `src/digitalmodel/orcaflex/mooring_design.py` — catenary pretension solver
  to make numerically stable.
- `tests/orcaflex/test_mooring_design.py` — catenary and mooring-line design
  tests to harden.
- `src/digitalmodel/orcaflex/installation_analysis.py` — DAF model to preserve
  or revise depending on test clarification.
- `tests/orcaflex/test_installation_analysis.py` — DAF tests to correct for cap
  behavior.
- `src/digitalmodel/workflows/agents/orcaflex/generators/base_files.py` —
  current generator contract.
- `src/digitalmodel/workflows/agents/orcaflex/commands/generate.py` — CLI help
  text that may need to align with the current 13-file contract.
- `tests/test_orcaflex_agent.py` — stale BaseFileGenerator expectations to
  update to the current modular file contract.

## Deliverable

The implementation will make the issue command pass:

```bash
uv run --no-sources --with-editable . python -m pytest tests/orcaflex/ tests/test_orcaflex_agent.py -q -p no:randomly -p no:sugar --tb=short
```

Any test expectation changes will document the intended behavior explicitly so
future CI repairs do not reintroduce ambiguous physics or stale generator
contracts.

## Proposed Design

### Catenary

The implementation will keep the existing `solve_catenary()` signature. It will
separate two behaviors:

- No pretension: continue treating the available inputs as an all-suspended
  preliminary catenary estimate. Because no anchor radius or fairlead tension is
  provided, the solver will not infer grounded length from line length alone.
- Pretension provided: solve for horizontal tension with a bounded bracketing
  method over `0 < H < T`, where `T` is top pretension. The solver will evaluate
  the catenary depth residual safely, avoid `math.cosh()` overflow, and compute
  suspended length from `V / w` so excess line becomes grounded length.

Planned test changes:

- Add/keep a red test proving the pretension case does not overflow and returns
  positive tensions.
- Change the grounded-length test to use a pretension case, because grounded
  length is determinate there.
- Keep or add a test documenting that the no-pretension estimate may use the
  full line as suspended when no anchor radius/tension is supplied.
- Include `MooringLineDesign.estimate_catenary()` in the focused test set,
  because the full shard showed it fails through the same pretension path.

### DAF

The current DAF model clamps output to `5.0`. The failing monotonic test uses
inputs that saturate both heave cases, so `daf2 > daf1` cannot hold. The
implementation will not remove the cap just to satisfy monotonicity. It will:

- change the monotonic test to use unsaturated parameters where increased heave
  produces a higher DAF below the cap;
- add or keep a cap test documenting that high dynamic loading clamps to `5.0`;
- only change `DAFInput.calculate_daf()` if the new tests reveal an actual model
  defect beyond test-input saturation.

### BaseFileGenerator

The current generator has already moved to a modular 13-file contract:

- `01_general.yml`
- `02_environment.yml`
- `04_vessel.yml` plus `_04_vessel_data.yml`
- `05_vessel_inst.yml` plus `_05_vessel_inst_data.yml`
- `06_line_types.yml`
- `07_lines.yml` plus `_07_lines_data.yml`
- `08_buoys.yml` plus `_08_buoys_data.yml`
- `09_groups.yml`
- `<project>_base.yml`

The implementation will update `tests/test_orcaflex_agent.py` to this current
contract instead of restoring the older 9-file API. The test suite will verify
the public methods that actually exist:

- `generate_environment()` for `02_environment.yml`
- `generate_vessel_type()` for the wrapper/data pair
- `generate_vessel_instance()` for the wrapper/data pair
- `generate_line_types()`
- `generate_lines()` returning a wrapper/data pair
- `generate_buoys()` returning a wrapper/data pair
- `generate_all()` returning 13 files

The `test_generate_general_file` expectation will be corrected so it checks
general analysis settings in `01_general.yml`; `WaveType: JONSWAP` belongs to
environment wave templates, not the general file.

If `src/digitalmodel/workflows/agents/orcaflex/commands/generate.py` still
documents old filenames in CLI help text, the implementation will update that
help text to the 13-file contract. It will not add legacy methods unless a
review finds external callers depending on them.

## TDD Test List

The implementation will make or add these failing tests first, then implement
the minimum code/test-contract corrections:

- `tests/orcaflex/test_mooring_design.py::TestCatenary::test_catenary_with_pretension`
- `tests/orcaflex/test_mooring_design.py::TestCatenary::test_catenary_grounded_length`
- `tests/orcaflex/test_mooring_design.py::TestMooringLineDesign::test_estimate_catenary`
- New/updated no-pretension behavior test documenting the all-suspended
  estimate.
- `tests/orcaflex/test_installation_analysis.py::TestDAF::test_daf_increases_with_heave`
- New/updated DAF cap test documenting the `5.0` ceiling.
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_general_file`
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_environment_file`
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_vessel_type_files`
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_vessel_instance_files`
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_line_type_and_line_files`
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_all_files`

## Implementation Steps

1. Update the tests first and verify the intended red failures still reproduce.
2. Replace the pretension catenary Newton-like iteration with a bounded solver
   that cannot evaluate unbounded `cosh()` arguments.
3. Adjust grounded-length tests to use a determinate pretension case and
   document no-pretension behavior.
4. Correct DAF tests to distinguish monotonic unsaturated behavior from capped
   saturated behavior.
5. Update BaseFileGenerator tests to the current 13-file modular contract and
   update CLI help text if it still lists stale filenames.
6. Run the focused test command and then the full issue shard command.
7. Run syntax/format checks on changed Python files.
8. Run the available legal scan gate before code-stage review. This repo does
   not currently contain `scripts/legal/legal-sanity-scan.sh`; if a repo-local
   wrapper is still absent during implementation closeout, run the
   workspace-level scanner from `/mnt/local-analysis/workspace-hub` and report
   the exact command and target checkout used.

## Acceptance Criteria

- `tests/orcaflex/test_mooring_design.py::TestCatenary::test_catenary_with_pretension`
  passes without `math.cosh` overflow.
- `tests/orcaflex/test_mooring_design.py::TestCatenary::test_catenary_grounded_length`
  passes with grounded behavior tied to a determinate pretension case.
- `tests/orcaflex/test_mooring_design.py::TestMooringLineDesign::test_estimate_catenary`
  passes.
- `tests/orcaflex/test_installation_analysis.py::TestDAF::test_daf_increases_with_heave`
  passes with unsaturated monotonic inputs, and capped behavior is documented.
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator` reflects the current
  13-file modular BaseFileGenerator contract.
- The command from [#704](https://github.com/vamseeachanta/digitalmodel/issues/704)
  passes locally:

```bash
uv run --no-sources --with-editable . python -m pytest tests/orcaflex/ tests/test_orcaflex_agent.py -q -p no:randomly -p no:sugar --tb=short
```

## Verification Commands

After implementation and before code review, run:

```bash
uv run --no-sources --with-editable . python -m pytest tests/orcaflex/test_mooring_design.py tests/orcaflex/test_installation_analysis.py tests/test_orcaflex_agent.py -q -p no:randomly -p no:sugar --tb=short
uv run --no-sources --with-editable . python -m pytest tests/orcaflex/ tests/test_orcaflex_agent.py -q -p no:randomly -p no:sugar --tb=short
uv run --no-sources python -m py_compile src/digitalmodel/orcaflex/mooring_design.py src/digitalmodel/orcaflex/installation_analysis.py src/digitalmodel/workflows/agents/orcaflex/generators/base_files.py
if [ -x scripts/legal/legal-sanity-scan.sh ]; then scripts/legal/legal-sanity-scan.sh --diff-only; else (cd /mnt/local-analysis/workspace-hub && scripts/legal/legal-sanity-scan.sh --repo=digitalmodel --diff-only); fi
```

If `uv` mutates `uv.lock` or creates `.venv`, the implementation closeout will
restore/remove that residue before reporting completion.

## Risks

- The catenary function lacks anchor-radius input, so no-pretension grounded
  behavior is underdetermined. This plan avoids inventing hidden physics and
  ties grounded assertions to pretension-driven suspended length.
- A bounded pretension solver must handle invalid or physically impossible
  pretension inputs explicitly rather than looping or overflowing.
- The DAF model's cap can mask monotonic trends. Tests must cover both
  unsaturated monotonicity and saturated cap behavior.
- Updating tests to the 13-file generator contract may expose stale CLI help or
  external documentation. This issue should update adjacent CLI help text only;
  broader docs belong in a follow-up if found.

## Adversarial Review Plan

This draft must be reviewed before it is surfaced as approval-ready. Reviewers
should look for physics ambiguity, test-only papering over real model defects,
stale generator API compatibility risk, legal-scan executability, and scope
creep outside the `tests-orcaflex` shard.

Planned review artifacts:

- `scripts/review/results/2026-06-12-plan-704-claude.md`
- `scripts/review/results/2026-06-12-plan-704-codex.md`
- `scripts/review/results/2026-06-12-plan-704-gemini.md`

If any provider returns `MAJOR`, the plan will stay in draft/needs-revision
state and implementation will remain blocked.

## Approval Gate

Implementation must not start until the issue has user-provided
`status:plan-approved` evidence and `.planning/plan-approved/704.md` exists for
the reviewed plan SHA or an issue comment that names that SHA. This plan does
not self-approve and does not authorize code edits.
