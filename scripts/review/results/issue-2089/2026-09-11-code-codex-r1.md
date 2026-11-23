# Independent Codex Milestone 1 artifact review

Date: 2026-09-11
Issue: https://github.com/vamseeachanta/digitalmodel/issues/2089
Reviewer: Codex, independent read-only defect hunt.

## Verdict

REQUEST_CHANGES — one MINOR reproduced runtime-profile consistency defect.
No licensed-execution, authentication or native-parser bypass was established.
This verdict applies only to the reviewed synthetic Milestone 1 implementation;
it does not qualify Milestone 2 or authorize a live capture.

## Finding M1 — work/temp path aliases evade the distinct-directory contract

Location: `src/digitalmodel/solvers/ghs/contracts.py::runtime_record` and `path_text`.

The profile rejects identical work/temp strings after `ntpath.normcase`, but does
not normalize dot segments before comparing them. Public `prepare_canary` accepts:

```text
work_dir = C:/Synthetic Work
temp_dir = C:/Synthetic Work/.
result = prepared packet returned
```

These normalize to the same Windows path under `ntpath.normpath`. A trailing-dot
variant is also accepted; that variant was a lexical probe, not a live Windows
filesystem experiment. Alternate slash spelling alone correctly rejects.

Impact: the prepared packet can encode a profile that contradicts the planned
separate work/temp-directory requirement. Hashing preserves the contradictory
profile rather than rejecting it. The defect cannot currently launch GHS because
the live API unconditionally refuses execution; severity remains MINOR.

Requested correction: reject ambiguous dot/trailing-dot/trailing-space segments
or use a reviewed Windows canonical path policy before the distinct-directory
comparison. Add public preparation regressions for equivalent paths. Do not
advertise this lexical check as full filesystem identity/ACL qualification.

## Verified scope and negative conclusions

- Live `run_approved_capture` unconditionally raises; neither receipt preview nor
  a synthetic process rc=0 produces licensed or qualified completion.
- Receipt preview explicitly leaves provenance, ACL checks and nonce reservation
  unverified and returns launch_allowed=false. Actual nonce consumption and
  Windows containment are deferred live controls, not missing synthetic claims.
- All three normalized depths and ten quantities are required. Exact independent
  box expectations and the absolute tolerance boundary tests pass.
- Staging substitution, event/byte overflow, stream exception and missing/duplicate
  terminal-event tests pass. Trusted injected generators are not sandboxed, and
  synthetic events do not demonstrate wall-clock termination.
- A synthetic normalized comparison can be submitted under a different prepared
  case with asserted reference hashes. It remains synthetic_normalized,
  comparison_passed_unreviewed and licensed_execution_verified=false, with evidence
  truth explicitly excluded. This is not native provenance verification; future
  licensed integration must bind actual receipts/artifacts to the executed packet.
- Saved example has30 passing comparisons, capture_unqualified and no licensed
  execution. Packet, report bytes, capture-reference, policy-reference and all
  recorded implementation hashes independently match retained artifacts.
- The guide/report accurately exclude native geometry qualification, guessed
  parsing, actual process launch and engineering approval.

## Independent validation

Initial attempt with the generic workspace venv failed collection because its
OpenFOAM parent-package dependency loguru was absent. This was an environment
mismatch; no source change was made. The supplied isolated dependency command then
passed under the repository's normal pytest configuration:

```text
PYTHONDONTWRITEBYTECODE=1 PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 PYTHONPATH=src
/usr/local/bin/uv run --no-project --python 3.11
  --with pytest==8.4.2 --with numpy==2.4.4 --with scipy==1.17.1
  --with pyyaml==6.0.3 --with loguru==0.7.3
  python -B -m pytest tests/solvers/ghs -q -p no:cacheprovider
75 passed in 4.27s
```

Focused public API probes:
- dot-segment work/temp alias: ACCEPTED (finding M1);
- trailing-dot variant: ACCEPTED (lexical evidence only);
- alternate-separator exact alias: REJECTED;
- different-case synthetic assertion comparison: explicitly unreviewed/unlicensed.

The saved replay script was inspected, not rerun, because its main function writes
artifacts. Its output and reference hashes were checked read-only instead.
No solver, browser, company share or key file was accessed by this review.

## Inspected artifact hashes

- `src/digitalmodel/solvers/ghs/__init__.py`: `92694625a8108a6bfd920e8842cebfc1805f0b018eba06943aea11f79cabb355`
- `src/digitalmodel/solvers/ghs/_canonical.py`: `b5888a05501700f1a452ee6a8235c024c241c20e5615472340bf5955ea16794c`
- `src/digitalmodel/solvers/ghs/comparison.py`: `b193f80ea8476815a81499ff9372902d7a3721e540cbf4b8346ae7cc020e0676`
- `src/digitalmodel/solvers/ghs/contracts.py`: `398b1f248b12e8f188e8bafe434dbd2065d0c3471093d17edbc103ac25b16a70`
- `src/digitalmodel/solvers/ghs/runner.py`: `349fb4aa1a0e42ba01606d1361d1afb697a6f57bc6a3093ddfd35bcc273155b7`
- `tests/solvers/ghs/test_boundaries.py`: `827198bbdc3d5f253996cb84cc161e1dbe4455146b3239cf367d26588d41d585`
- `tests/solvers/ghs/test_comparison.py`: `0f893bfa68544681bb62aed7c6fa3cce59d8c551bd3c92be106baf85ed8c571f`
- `tests/solvers/ghs/test_contracts.py`: `76e78fb3230fd1aa0c0dc82ffd15c547569e93e574cd9964965910b64717f33d`
- `tests/solvers/ghs/test_runner.py`: `3de585da6ba96a64b657d0250be35997237498d75e4d670dbb04c6c31409fabc`
- `docs/plans/2026-09-11-issue-2089-ghs-canary.md`: `bbd0bc72328bb62b3f311ade8b476a47235c0b2ce318fd3dc46393fccab43a6c`
- `docs/domains/ghs/neutral-hydrostatics-canary.md`: `b615fa0682cad22c87f1649ac43d85f49e5b1bc7646c443eee631a9218f8387f`
- `scripts/review/results/issue-2089/replay-synthetic-example.py`: `6f9f4f9d9da8751c03281002e18a2411866988c251578efd9710aae072cfc946`
- `scripts/review/results/issue-2089/synthetic-example-result.json`: `5a6c548a7f08a69a70b4351af56245c51669752dde2bd6e51175ca14b2fc4f55`
- `docs/reports/2026-09-11-ghs-m1-synthetic.html`: `e7adfe1f37c8fe79a44edb8c09cd147c41de7fb08e59aae54eedd31677abb9d8`

## Handoff

Only this review file was written. No code, tests or example artifacts were edited
and no commit was made. Main owns cleanup auditing and disposition. After the
bounded alias correction, main should retain tests-first evidence and independently
recheck the affected profile contract without treating this review as live approval.
