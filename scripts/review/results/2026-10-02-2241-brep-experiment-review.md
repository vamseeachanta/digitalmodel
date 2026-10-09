# Issue 2241 item 2 — rejected candidate review

The owner instructed continuation after the published r3 plan approval checkpoint.
The one-candidate experiment followed that plan's failure path. No qualified
repair, default migration or item-2 completion is claimed.

## Code and artifact review

- Independent Codex r1: MINOR. Published fine-keel counterexamples needed a
  committed reproducer; the geometry JSON needed interior/boundary distinction;
  native tests needed the slow marker.
- Main-session corrections: committed `keel_counterexample`, reran all six
  geometry diagnostics, verified the four fine-keel results against independent
  evidence within 1e-10 m, refreshed JSON, and marked native tests slow.
- Independent Codex r2: APPROVE. Verified those corrections, unchanged defaults,
  explicit unqualified opt-in semantics, hidden/time-bounded subprocesses and
  targeted worker failure handling. Six diagnostic tests passed; the reviewer
  consumed no native assessments. A source audit separately checked native
  per-metric status semantics and shared-edge/pole-bound behavior.
- Claude: UNAVAILABLE. Read-only source review timed out at 240 seconds without
  output. Authentication independently reported logged in via subscription.
  One smaller inline source review also timed out at 180 seconds without output.
- Gemini: UNAVAILABLE. Exit 41, authentication not configured. No repeated retry.

This is one-provider approval, not cross-provider consensus. The PR must remain
draft until the required second-provider code/artifact review can be completed.

## Revised plan review

Codex: APPROVE for owner consideration of the diagnostic-only graph study in
`docs/plans/2026-10-02-issue-2241-brep-boundary-study.html`. It preserves the
inherited numerical contracts, geometry-first ordering, one-candidate stop rule
and explicit native-call budget. Claude/Gemini are unavailable as above, so the
plan is not fully reviewed and not authorized for implementation. The original
experiment's authorization does not carry over to that new candidate.

## Evidence

- RED before fitting edits: rounded default failed exact native-valid assertion
  with `geometric_singularity_nonintegrable`; transom with
  `quadrature_unconverged`. Both used the existing exporter/screen route.
- Native diagnostic budget: ten allocated slots; six native assessments and
  four geometry rejections before assessment. No refinement/control acceptance
  calls followed the decisive geometry failures; budget limit twenty.
- Full hull-library and diffraction quality-gate suite: 678 passed, 42 skipped,
  9 xfailed in 345.44 seconds. Six new xfails document rejected geometry; the
  original three remain. A later worker-exit regression and endpoint-extension
  test are covered by the separate focused run, not counted in that full run.
- Black 24.10 and Ruff pass the seven changed Python files. All new/modified
  production modules are below 400 lines and functions below 50 lines.
- Verified-target full legal scan passed with no blocking findings. The explicit
  repository-root route was used because the scanner's known `--repo` resolver
  defect is tracked separately. No scanner implementation was changed here.
- HTML parsed and relative links resolved. Committed JSON rejects NaN literals,
  retains unavailable native values as null and includes synthetic inputs only.

Reusable reproduction/boundary findings were added to
`.claude/rules/geometry-validation.md`.
