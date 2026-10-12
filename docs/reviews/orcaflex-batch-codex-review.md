# OrcaFlex batch reliability — independent Codex code review

Verdict: APPROVE for the bounded repair under
[2051](https://github.com/vamseeachanta/digitalmodel/issues/2051) and
[1564](https://github.com/vamseeachanta/digitalmodel/issues/1564).

The review examined the staged code diff, packaged base config, focused
regressions, central run contract, executor, and native-proof JSON. No
implementation changes were made by this review. Tests and licensed solves
were not rerun; the main session reported 54 passing tests and one skip.

## Defect-hunting results

- The new OrcaFlexLoader subclasses SafeLoader and changes only construction
  of null-tagged scalars. Blank scalars remain empty strings; explicit null
  tokens remain None. Quoted text retains the safe loader's string behavior.
  Constructor registration is confined to the subclass. Tests distinguish
  blanks, tilde/null tokens, quoted text, zero and false, and verify that
  global yaml.safe_load behavior remains unchanged.
- The canonical dumper emits empty text as quoted empty text and None as
  tilde. This addresses the measured empty-script/default distinction;
  arbitrary lossless conversion is not claimed. UTF-8 output and BOM-aware
  input are explicit; legacy encodings remain outside qualification.
- Packaged defaults mirror the router. The genuine engine-configuration test
  verifies user overrides survive without mocking ApplicationManager.
- Failed/missing executor results and empty batches become FAIL after
  diagnostic files are written. Explicit mock success becomes SKIPPED.
  CLI tests select the worktree source and assert fresh summary, sidecar,
  and exit status through the existing central contract.
- PASS continues to consume the existing executor's success status. It is
  not a new authenticated receipt or an engineering-adequacy assertion.

## Disposition of the other-provider findings

The reported missing-licence-probe concern is not supported by the complete
source. router refuses at orcaflex_run_batch.py:74 when real mode cannot
obtain licence availability; only the later completed path assigns
solver_available. The existing test_license_absent_and_mock_false_fails_fast
asserts this refusal. A successful earlier probe does not reserve a licence;
later worker failures still produce FAIL.

The reported missing model/status-key concern is not a reachable external
input defect: _manifest_rows constructs both fields for every row before
_set_run_verdict consumes them. Missing executor results become failed rows.

Explicit UTF-8 is deliberate for the qualified native input path, not an
accidental locale substitution. Native proof limits explicitly exclude
unqualified legacy encodings.

The main session reported the subsequent Claude review as UNAVAILABLE
(exit 1, no output). This Codex approval does not imply provider consensus.

## Native evidence binding and limits

Read docs/reports/orcaflex-batch-native-proof.json: CLI exit zero, one real
case completed, no failures, DLL 11.6c, saved-simulation readback and four
finite samples. Independently recomputed SHA-256 for the workflow, YAML
utilities and packaged config; all three match that proof's source hashes.

The proof covers one generic line in the local interactive context. It does
not qualify remote dispatch, scheduled-task execution, arbitrary model
corpora, engineering adequacy, or worker-thread enforcement. The recorded
worker count is one; solver threads retain the existing executor default.

No material blocker was found in this bounded diff. Deployment and broader
execution-contract acceptance remain separate workstreams.
