# Uniform local-loss study: source and implementation qualification audit

Study: [issue 2287](https://github.com/vamseeachanta/digitalmodel/issues/2287).
Dates are UTC; the host local calendar was still 2026-10-05.
Base revision: `2a52374d401e2f445d4baf435322f2cf18346c98`.
Specification: [four-size plan](../../plans/2026-10-06-issue-2287-uniform-local-loss-screening.md).

The requested minimum acceptable wall versus axial length and circumferential
width is not established by the inspected implementation. The authorized
computation is an implementation diagnostic only. No baseline threshold is
promoted as an allowable-wall screening result.

| Evidence | Observation | Qualification consequence |
|---|---|---|
| Canonical coordinator and architecture record | L1/L2 stage dictionaries can be retained together | Reuse coordinator; do not extend frozen legacy engine |
| `assessment/level1_screener.py` | Code-required thickness comparison, independent of length and width | Not complete Part 5 Level 1 |
| `assessment/level2_engine.py` | Reference wall is code minimum, simplified Folias; width omitted | Not qualified Part 5 Level 2 envelope |
| Same LML engine | Rows below 90% nominal counted times minimum spacing; not bounding span | Requested extent and inferred extent must be recorded separately; shallow loss discontinuity |
| `circumferential_defect.py` | Separate longitudinal polynomial and axial net-section screen | Not a full L1/L2 width-dependent substitute |
| Existing `api579_lta.yaml` and legacy input anchors | Mt at lambda=1 is 1.199; at lambda=2 is 1.618 | Conflicts with square-root-of-polynomial implementation; normative form unresolved |
| Existing validation records | Goldens reproduce assumed formulas, not an independently sourced 2021 Part 5 example | Passing tests establish implementation consistency only |
| `assessment/level3_escalation.py` | Handoff/interface only | No numerical Level 3 result exists here |

Table 1. Existing-method coverage and limits on use.

The ASME [publisher catalog](https://www.asme.org/codes-standards/find-codes-standards/fitness-for-service)
identifies FFS-1 2021. Private wiki metadata inspected on 2026-10-06 identifies
older 2007 extracted content and a 2016 licensed original, but reports no 2021
original on disk. Catalog identities `api-standards` and
`ace-codes-standards-library` are preserved. Exact edition-matched clause access,
errata, source digest and implementation-use rights were not established.
Older extracts and self-consistency anchors cannot resolve the 2021 equation
or establish applicability. Licensed source material is not copied here.

Constructive work in this slice adds finite-input checks, physical cell-center
coordinates, requested/derived length comparison, explicit width/load omission,
separate raw verdict/applicability/qualification and source hashes. Production
equations remain unchanged because a source-backed correction is not established.

The bounded study uses four geometry-only precedents with new synthetic common
grade and pressure assumptions. Each of 784 cases retains Level 1 and Level 2
raw results; `allowable_remaining_wall_in` is always null with a reason.
`INAPPLICABLE` means the existing algorithm raises its lambda limit; the absence
of that flag does not establish full standard applicability. Remaining cases
are `UNQUALIFIED`, even where raw verdict is `ACCEPT`. Width-invariant values
are a diagnosed missing physical dependency, not four accepted width curves.
Named unverified applicability checks include Rt, minimum remaining-wall floor,
Lmsd, adjacent-flaw spacing and routing. Every diagnostic records forced LML
routing and no sound-wall region in its grid. Intact controls are marked
separately. The polynomial discrepancy specifically concerns square-root versus
direct-polynomial evaluation; neither is selected as normative here.

Reproduce using the repository environment with source on PYTHONPATH:

```powershell
uv run python -m digitalmodel.asset_integrity.uniform_loss_diagnostic --output <task-local-output.json>
uv run python -m pytest tests/asset_integrity/test_uniform_loss_diagnostic.py -q
```

Runtime/library versions, host and module SHA-256 values are written into the
diagnostic output. Numerical files remain task-local; the common repository
retains the method and qualification record. Later qualified reusable results
belong in private digitalmodel-data with stable IDs, manifest revision/digest,
criteria and qualified intended use; no storage migration occurs here.
Hashes cover all digitalmodel Python source bytes, including imported helpers;
CRLF/LF checkout differences deliberately change these raw-byte digests.
No external configuration is loaded by the fixed diagnostic study. Hostname
metadata remains in task-local execution evidence. Run/test results and the
diagnostic digest are recorded in the issue-2287 session handoff.

Discovery uses the existing `docs/registry/module-routing.yaml` asset_integrity
row. Private wiki `data/query_sources.json` remains the query-surface authority.
The hub report-artifact-index document is a proposal, not an installed viewer
registry. No competing report index or raw report copy is added.

Next checkpoint: authorized exact 2021 Part 5 source audit and independent
worked-example validation; then test and correct supported physics, qualify
applicability and inversion, and produce separate L1/L2 screening slices. The
Level 3 pilot resource proposal in the specification requires actual solver,
host/license/material/criterion qualification and compute approval before runs.
