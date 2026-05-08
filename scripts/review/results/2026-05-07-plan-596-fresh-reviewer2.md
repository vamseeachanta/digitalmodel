# Fresh review artifact: digitalmodel #596 updated plan — reviewer 2

Verdict: MAJOR

Findings:
- [severity: high] B1528 follow-up requirement is still not approval-safe / machine-verifiable — The plan requires a follow-up issue before closeout, but the draft config example uses placeholder prose in `follow_up_issue_or_permanent_justification` and required metadata only checks field presence, not a real issue ID/URL or permanent-justification standard. An executor could satisfy the checker with placeholder text and close #596 without creating the actual relocation/classification follow-up. Required change: tighten schema/gates so the exception must contain a concrete issue URL/ID or defined permanent-justification form, and add test/verification coverage rejecting placeholder prose before closeout.
- [severity: medium] Exact B1528 safety verification is incomplete; moored-current surface is omitted — The verification command includes yaw and time-trace tests but omits `tests/naval_architecture/test_b1528_sirocco_moored_current.py`, even though moored-current outputs are part of the same exception set and are linked from docs/source. Required change: add the moored-current test or explicitly justify exclusion.

Approval readiness: not ready until follow-up-link gate and B1528 verification are tightened.
