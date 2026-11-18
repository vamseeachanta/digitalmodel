# Coverage gate for CI changes

Run the old and proposed CI commands against the same base SHA, once per Python version, extras set and obligation. Keep application and test sources identical for a selection change. Use fresh evidence directories. Record any CI configuration overlay in the PR so reviewers can reproduce a dirty worktree.

For example, from the base checkout with the gate available, run this comparison of a harness expansion in the active test environment.

```sh
base=$(git rev-parse HEAD)
gate=scripts/ci/coverage_gate.py
mkdir -p evidence
printf '%s\n' '["py312-test"]' > evidence/envs.json
printf '%s\n' '["scripts/ci/detect_touched_domains.py"]' > evidence/modules.json

python "$gate" collect --env py312-test --out evidence/before --obligation pre-merge -- \
  tests/scripts/test_select_family_tests.py --cov=scripts/ci -q
python "$gate" collect --env py312-test --out evidence/after --obligation pre-merge -- \
  tests/scripts/test_select_family_tests.py tests/scripts/test_closure_selection.py --cov=scripts/ci -q
python "$gate" compare --kind consolidation --commit "$base" \
  --before evidence/before --after evidence/after --report evidence/report.md \
  --required-envs evidence/envs.json --required-modules evidence/modules.json \
  --touched-modules evidence/modules.json
```

Substitute the actual pytest commands and coverage file keys for the fix. List every required environment and module even if both commands could omit it. Each environment directory contains `junit.xml`, `coverage.json` and `env.json` with the base commit, clean-worktree flag, pytest command, Python version, obligation, pytest exit code and completion flag. Collection uses pytest-cov when installed. For coverage.py alone, set the source in the coverage configuration and omit pytest-cov options. Collect preserves the pytest exit code for a completed run. An interrupted run, absent artifact or unavailable collector fails.

Compare the pre-merge and daily obligations separately. Use fresh before/after roots with `--obligation daily` for the full nightly suite. For a pre-merge closure audit supply three JSON string lists together. `--closure` contains required pytest node IDs, `--pr-path-nodes` contains nodes selected on the proposed PR path, and `--nightly-path-nodes` contains nodes selected nightly. Include dependents, family, engine and the structure check when building the closure.

- **Consolidation** checks per-environment node identities, outcomes, skip reasons and executed-line inclusion for each touched module.
- **Removal** also requires `--mapping mapping.json`, a reviewed JSON object mapping each removed ID to a replacement that passed in the same environment and obligation.
- **Selection** compares shard names. Run `python "$gate" compare --kind selection --before before.json --after after.json --closure closure.json --commit "$base" --report selection.md`. All three inputs are JSON string lists. Before and after can also be directories containing `selected-shards.json`. The report identifies missing closure shards and shards dropped outside the closure. Attach the run evidence separately for each obligation.

The Markdown verdict starts with `Coverage gate at <sha8>: PASS` or `Coverage gate at <sha8>: FAIL (<reasons>)`. Each environment row gives before/after node counts, after outcomes, losses, new failures, changed skip reasons and lost line counts by module. Details name affected IDs or files and count omitted entries. Compare exits **0** for PASS, **1** for a failed evidence check, and **2** for invalid input or usage. Both nonzero codes fail the gate.

Review still covers weaker assertions in tests that run and lost branches on lines that remain covered.
