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

## Edition-specific continuation: exact records and clause requests

| Existing record | Observed source coverage | Missing evidence |
|---|---|---|
| Standards spine `api-579-1` | 21 pages: one metadata header plus 20 pages explicitly dated 2007; revision array includes 2021 | No 2021 clause extraction identified |
| Standards spine `api-579-1-asme-ffs-1` | One joint-standard metadata page | No exact 2021 technical source/digest/access record |
| Standards record `api-std-579-asme-ffs-1` | 2016 catalog copy recorded as encrypted-metadata-only; parse status provisional-unverified; historical appendix fragments separately described | No 2021 Part 5 technical source; older fragments are not edition substitutes |
| Transfer ledger `2016-API579-PART5` | Status done, but doc_path empty, doc_paths empty and modules empty | No resolvable source or computational qualification despite processing status |
| Catalog identities `api-standards`, `ace-codes-standards-library` | Known publisher/library source identities | Edition-specific authorized-access and permitted-use evidence, source digest and errata |

Table 2. Source records consulted after the initial preparation PR.

The source owner's prior audit records an unsupported security handler for the
2016 licensed original and stops extraction after that failure. No decryption,
parser bypass, raw copy or extraction was attempted by this study. The local
master document index named in the hub registry is absent; the mounted-source
registry points at Linux-owned indexes rather than an accessible Windows
original. ACE_SHARE_ROOT is unset in this execution environment. These are
coverage limits, not proof that an owned 2021 source does not exist elsewhere.

The existing strict SSH route probe to the declared Linux alias stopped on
missing trusted host-key verification. No host key, credential, authentication,
network or security setting was changed. Source retrieval is therefore referred
through the parent to the established standards coordinator; peer catalog
conversion and private source records remain with that owner.

Exact source requests are as follows. The locators shown are claims made by
existing implementation comments, not verified clause identifiers for 2021.

| Implementation claim / requested item | Source-backed correction candidate | Required independent regression |
|---|---|---|
| Part 5 cylinder Folias, claimed Table 5.2 | Resolve direct polynomial versus square root; no equation change until original is read | Source-issued Mt values at several lambda values, range endpoints and worked example; compare both existing helpers |
| Part 5 RSF, claimed Eq. 5.13; definitions of Tc, tmm and FCA | Replace code-minimum reference only if verified sound-region/future-wall definitions require it | Separate remote wall, pressure-required wall and FCA; benchmark independent source case |
| Part 5 L1/L2 procedures, claimed sections 5.4 and 5.4.2.2 | Implement complete level-specific length/width, minimum-wall/Rt, Lmsd, weld/spacing gates and pressure/rerating logic | Valid and inapplicable boundary cases with explicit reason codes; independently checked pressure criterion |
| Part 5 Level 2 profile/area procedure | Verify whether uniform LTA lets levels coincide; do not invent level differentiation | Rectangular uniform profile and nonuniform reference example, axial/circumferential checks and grid convergence |
| Flaw characterization / extent convention | Correct row-count segmentation and width/orientation semantics only within verified geometry definition | Exact edge extent, shallow loss, separated-row gap, wrapped circumferential patch and width response |

Table 3. Conditional correction/test plan; no candidate is normative acceptance.

The arithmetic conflict is independently reproducible from existing code:

| lambda (1) | circumferential helper Mt (1) | square of returned Mt (1) | legacy fixture Mt (1) | canonical simplified helper Mt (1) |
|---|---|---|---|---|
| 1.000 | 1.095171 | 1.199399 | 1.199 | 1.216553 |
| 2.000 | 1.272045 | 1.618099 | 1.618 | 1.708801 |

Table 4. Algorithm/fixture disagreement; source qualification is unresolved.

The square of the circumferential helper agrees with legacy fixture rounding;
the canonical helper differs from both. This establishes inconsistent
implementation evidence, not which 2021 equation is correct. Existing
hand-reproduced goldens cannot adjudicate it. No production numerical formula
is changed in response to this comparison.

Further code audit: `digitalmodel.codes.API_579` labels results 2021, but a
label is not evidence of edition-matched source verification. The frozen legacy
LML code uses remote-region average minus outside-flaw FCA as tc, unlike the
canonical code-minimum reference. It also uses a 0.050 in floor in its local
applicability branch; this conflicts with other candidate floor claims and
requires source verification rather than selecting a preferred threshold.

Legacy `LMLMAWPrEvaluation` leaves Mt/RSFa unset on some short/long-flaw branches
and omits the exact lambda=20 branch. Its Level 2 helper sorts profile readings
and associates a j-reading mean with a (j+1)-spacing length. These are source-code
observations, not proof of the normative profile method. This legacy path is
frozen and is not selected as a fallback. A qualified canonical implementation
will require endpoint/tie tests, physical contiguous-area/profile handling and
source-backed definitions; preserving the frozen code does not qualify it.

Publisher errata discovery follows API's
[addenda and errata instructions](https://www.api.org/products-and-services/standards/addenda-and-errata)
through the publication's amendment record. The publisher announcement locator
in existing metadata was inaccessible through the web tool on this check;
the available publisher catalog confirms edition metadata, not Part 5 equations
or the applicable errata set. No conclusion of "no errata" is drawn.
