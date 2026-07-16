# Plan for #1588: Reconcile DNV-RP-C203 basis before publishing weld-fatigue results

> **Status:** blocked-draft / needs qualified source decision
> **Complexity:** T3
> **Date:** 2026-07-14
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1588
> **Client:** N/A
> **Lane:** lane:codex
> **Structural review artifact:** `scripts/review/results/2026-07-16-plan-1588-claude.md`
> **Implementation-basis review:** pending after qualified Decisions 1–4
> **Human report:** `docs/plans/2026-07-14-issue-1588-standards-inventory.html`

---

## Current gate

Planning and resource intelligence will remain open. No calculation constant, curve behavior, test oracle, generated result, or acceptance statement will change until a qualified reviewer with lawful access records the accepted DNV edition/amendment and exact locators. Issue #818 will remain implementation-blocked because its class-F seawater-with-cathodic-protection example crosses the unresolved curve branch.

The current structural review will terminate by recording its verdict at the path above,
keeping `status:needs-plan`, changing no approval label, and requiring a fresh T3 review
after qualified Decisions 1–4 amend the plan. A structural verdict will cover only
governance, ordering, source boundaries, and fail-closed architecture; it will not approve
numeric test anchors or migration behavior.

## Resource Intelligence Summary

### Issue and related work

- [digitalmodel #1588](https://github.com/vamseeachanta/digitalmodel/issues/1588) is OPEN with `status:needs-plan`; it requires source reconciliation rather than assuming which implementation is correct.
- [digitalmodel #818](https://github.com/vamseeachanta/digitalmodel/issues/818) is OPEN with `status:plan-approved`, but its discovery-first checkpoint pauses implementation until #1588 resolves the calculation basis.
- The standards-derived path is `run_from_config()` → `weld_fatigue_quickcheck()` → `thickness_correction()` → `get_sn_curve()` → pyLife `WoehlerCurve.cycles()` → `miner_damage()` → design-life scaling → life factor and screening verdict.

### Standards and methods in scope

| Item | Role in reached path | Current publisher identity | Local possession signal | Planning disposition |
|---|---|---|---|---|
| DNV-RP-C203, *Fatigue design of offshore steel structures* | Direct encoded basis claimed for curves, environment adjustment and thickness correction | Edition 2024-10, amended 2025-10 | Filename evidence for 2000, 2005, 2008, 2010 and 2011 only | Normative basis will be verified through an authorized DNV source before implementation |
| BS 7608, *Guide to fatigue design and assessment of steel products* | Supporting reference named in module prose; no BS curve or constant is selected | BS 7608:2014+A1:2015, Current | Filename evidence for 1993, 1993+A1:1995, 2014 and one ambiguous PDF | The generic equivalence statement will be removed or qualified unless a licensed cross-standard mapping supports it |
| Palmgren-Miner cumulative damage | Named engineering method used by `miner_damage()` | No separate standards document is identified by the code | N/A | Its normative locator will be established within the accepted basis or it will be labeled as an uncited engineering method pending a named authority |
| pyLife 2.3.0 `WoehlerCurve` | Numerical implementation dependency | Third-party library, not a design standard | Locked in `uv.lock` | Branch selection, knee anchoring, default scatter and failure-probability behavior will be tested and disclosed |

Official metadata sources: [DNV catalog and authorized access route](https://www.dnv.com/energy/standards-guidelines/dnv-rp-c203-fatigue-design-of-offshore-steel-structures/), [BSI current product record](https://knowledge.bsigroup.com/products/guide-to-fatigue-design-and-assessment-of-steel-products), and [BSI chronology/access page](https://landingpage.bsigroup.com/LandingPage/Undated?UPI=000000000000295202). Public publisher pages establish identity and currentness only; they do not validate constants, formulas, locators, applicability, or cross-standard equivalence.

### Local holdings and rights boundary

The drive index returns DNV C203 holdings for 2000/2005/2008/2011 and BS 7608 holdings for 1993 and 1993+A1:1995. Direct filename inspection additionally finds DNV 2010 and BS 7608:2014. No reachable filename establishes possession of DNV 2016, 2021, 2024-10, or the 2025-10 amendment. The ACE knowledge index is current at 12 days; the O&G standards inventory is 198 days stale and the master document index is 88 days stale. Public planning artifacts will record only de-identified edition/year possession signals; infrastructure roots, private/client paths, and document contents will remain excluded.

These paths demonstrate possession signals only. No licensed PDF body, OCR output, table, or extracted full text was opened during planning. Possession, an index entry, a private-wiki label, or a stale ledger status will not be treated as authority to reproduce standards text/tables or publish encoded constants.

### Provenance and registry conflicts

- `llm-wiki/wikis/engineering-standards/wiki/standards/dnv-rp-c203.md` identifies revision `2024-10` but points only to a local 2011 source. It does not record the 2025-10 amendment and lacks the required `doc_key`.
- `llm-wiki/wikis/engineering/wiki/standards/dnv-rp-c203.md` is a legacy technical summary without `code_id`, `publisher`, or `revision`; it cannot resolve a fail-closed calculation citation.
- BS resolver pages mix 1993, 1993+A1 and 2014 identities, and duplicate `code_id` usage creates ambiguity.
- `workspace-hub/data/design-codes/code-registry.yaml` records `our_edition: 2016-04` and `latest_known_edition: 2024-07`; both conflict with live possession evidence and the publisher's 2024-10/2025-10 identity.
- `workspace-hub/data/document-index/standards-transfer-ledger.yaml` marks BS-7608 `done` while leaving `doc_path` and `doc_paths` empty. No DNV-RP-C203 ledger entry was found.
- The calculation-citation contract requires a fail-closed `Citation` sidecar, a standards-page target outside `sources/`, and `source_sibling: generic` for this public reusable calculation.
- The calculation code self-declares DNV-RP-C203 (2021), Tables 2-1/2-2 and Sec. 2.4, while no 2021 possession signal is established and publisher-current identity is 2024-10 amended 2025-10. Confirming or rejecting each asserted locator will be the primary qualified reconciliation task.

### Reached calculation dependency closure

| Reached value or behavior | Code evidence | Selected example impact | Unresolved verification |
|---|---|---|---|
| Class F | `examples/manufacturing/weld_fatigue.yml:17`; quick-check normalization | Direct | Real-joint applicability is not established; example must remain synthetic |
| Claimed edition and curve tables | `fatigue/sn_curves.py:2,4,6` claims DNV-RP-C203 (2021), Tables 2-1/2-2 | Direct provenance claim | Qualified reviewer must confirm or reject edition and each table locator |
| Claimed thickness section/table | `weld_fatigue_quickcheck.py:62,64` claims Sec. 2.4 and Table 2-1 last column | Direct provenance claim | Qualified reviewer must confirm or reject both locators and applicability |
| First slope `k_1=3.0` and second slope `k_2=5.0` | `fatigue/sn_curves.py:31` | Direct, on opposite sides of the knee | Exact accepted-edition values and applicability |
| Seawater-CP `log_a1=11.455` | `fatigue/sn_curves.py:53,126-129` | Direct | Exact accepted-edition table locator and value |
| Stored seawater-CP `log_a2=15.091` | `fatigue/sn_curves.py:53` | Not reached | `get_sn_curve()` does not read it; effective low-stress intercept is continuity-derived by pyLife |
| Knee `ND=1e7` | `fatigue/sn_curves.py:21,121` | Direct | Code comment records a possible seawater-CP `1e6` interpretation |
| `SD=10^((log_a1-log10(ND))/k_1)` | `fatigue/sn_curves.py:73,140` | Direct branch selector | No independent seawater-CP anchor |
| pyLife Basquin/Woehler cycles and `< SD` branch rule | pyLife 2.3.0; called by `fatigue/damage.py:35` | Direct | Both selected branches, equality boundary and implicit defaults lack accepted anchors |
| `t_ref=25 mm`, class-F `k=0.25`, and thickness factor | `weld_fatigue_quickcheck.py:62-66,187-203`; `damage.py:91-125` | Direct at 40 mm | Exact edition locator, applicability, and override provenance |
| `D=sum(n_i/N_i)` | `fatigue/damage.py:13-39` | Direct | Selected two-bin seawater-CP result lacks independent source-backed oracle |
| Linear life scaling | `weld_fatigue_quickcheck.py:215-217` | Direct over 25 years | Normative/project basis is not located |
| `allowable_damage=1.0` and inclusive PASS threshold | `weld_fatigue_quickcheck.py:119,219-223` | Direct | No DFF, consequence class, project acceptance basis, or exact locator |

`damage.design_life_check()` and the stored seawater `log_a2` are not reached. `DNV_CURVES` is built at import but `get_sn_curve()` reads the raw dictionaries directly. BS 7608 does not supply a selected constant in this path.

## Proposed deliverable

After the source gate is resolved, the implementation will reconcile the reached DNV-RP-C203 behavior to one accepted edition plus amendment, emit fail-closed citation sidecars for every standards-derived value or formula, preserve licensed-content boundaries, add independent numerical anchors, and label outputs as screening results unless qualified engineering acceptance is recorded.

## Files that may change after approval

| Action | Path | Purpose |
|---|---|---|
| Modify | `src/digitalmodel/fatigue/sn_curves.py` | Reconcile only source-verified curve/environment/knee behavior and attach cited values |
| Modify | `src/digitalmodel/fatigue/damage.py` | Attach cited method/formula evidence if the accepted basis supplies it |
| Modify | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | Emit citations, qualified screening semantics, accepted revision/amendment, and provenance |
| Modify | `src/digitalmodel/citations/` | Add a DNV C203 citation registry entry without licensed table reproduction |
| Create | `docs/evidence/standards/issue-1588-qualified-basis.json` | Record non-reproducing qualified decision metadata, exact locators, access attestation, reviewer role, and immutable approval binding |
| Create/modify | `tests/fatigue/` and `tests/citations/` | Add independent anchors and fail-closed provenance tests before implementation |
| Update | `examples/manufacturing/` | Regenerate only after behavior and provenance are accepted |
| Coordinate separately | workspace-hub #3533/#3538 and llm-wiki #837/#839/#840 | Land public registry, citation, page/source identity, private authority evidence, and mechanically blocked resolver contracts under their own approvals |

The exact file list will be narrowed after the qualified comparison. This draft will not authorize broad curve-library rewrites or silent changes to other fatigue workflows.

## TDD test list

Tests will be written before implementation and will include:

1. Citation resolution will enforce an in-repo minimum floor regardless of external contract state: it will deny missing page/source identity, absent external contract, blocked resolver, wrong revision/amendment, wrong publisher, wrong source sibling, or missing exact locator. Landed #3538/#839 contracts may strengthen but will not weaken this floor.
2. After Decision 2, independent source-backed class-F seawater-CP anchors will cover one stress above the accepted knee, one below it, and the knee equality boundary; the structural review will not approve these values.
3. A source-backed test will establish whether the second branch uses an explicit intercept or continuity at the accepted knee; a declared-but-unused constant will fail coverage.
4. Thickness cases at 25, 40 and 60 mm will validate the accepted reference, exponent, boundary and applicability.
5. An independent two-bin Miner calculation will validate per-bin cycles, per-bin damage, total block damage, 25-year scaling and life factor without copying generated output as the oracle.
6. Acceptance tests will distinguish an engine screening verdict from project/design approval and will fail closed when the allowable-damage/DFF basis is absent.
7. Migration tests will enumerate affected examples and committed artifacts if behavior changes; no artifact will be silently regenerated.
8. Existing consumers will be regression-tested, including any module that imports `get_sn_curve()`, `miner_damage()` or `thickness_correction()`.

## Acceptance criteria

- [ ] A qualified reviewer with lawful DNV access records the exact accepted edition, amendment state, clause/table locators, applicability decision, and rights basis without committing licensed body text.
- [ ] The reached dependency closure above is reconciled line by line; every standards-derived numeric value/formula either has a valid `Citation`/`CitedValue` sidecar or is removed from the claimed standards basis.
- [ ] The DNV resolver identity is unambiguous and records current edition plus amendment; the calculation resolver passes fail-closed validation.
- [ ] BS 7608 is either mapped through an authorized, qualified comparison or removed/qualified as a non-implemented supporting reference.
- [ ] Independent anchors cover both curve branches, knee semantics, thickness correction, Miner accumulation, life scaling and acceptance semantics.
- [ ] A change-impact inventory identifies every test, example, report and caller affected by any behavior change.
- [ ] Generated outputs remain explicitly labeled `screening`; no synthetic result implies real-joint or project acceptance.
- [ ] Legal/security scan and T3 plan/code cross-review gates pass before closeout.
- [ ] #1588, #818 and any downstream marketing issue remain open until their separate acceptance boundaries are satisfied.

## Decisions required before implementation and resolver activation

1. **Authorized DNV source:** identify a repository-owner-authorized structural/fatigue engineer competent to interpret the selected basis and holding lawful active access to edition 2024-10 and the 2025-10 amendment, or explicitly select a different project-governing edition with rationale. The reviewer will create `docs/evidence/standards/issue-1588-qualified-basis.json` containing only publisher, edition, amendment, exact non-quoting locators, applicability decisions, reviewer GitHub login/role, `lawful_access_attested=true`, decision time, and no licensed text/table. A repository-owner GitHub comment will bind that file's commit, path, blob, and SHA-256 before implementation.
2. **Technical basis:** record the accepted seawater-CP knee, both branch definitions/intercepts, slopes, environmental applicability, thickness correction, Miner treatment and allowable-damage/DFF basis with exact locators.
3. **BS claim:** decide whether BS 7608 will remain a scoped cross-reference requiring a qualified mapping, or be removed from this DNV implementation's compliance prose.
4. **Migration policy:** decide whether a source-backed behavior change will invalidate and regenerate the existing synthetic example and downstream proof, or whether a versioned legacy mode is required.

## Adversarial review and dependency gates

T3 review may assess this plan's structure before the four qualified decisions are recorded; those missing decisions will remain explicit implementation blockers rather than creating a cycle with #837. Review prompts will be defect-seeking and will challenge licensed-source authority, hidden constants, branch coverage, acceptance semantics, resolver ambiguity, transitive impact, and migration.

The pre-decision structural review will explicitly exclude numeric anchor design in tests
2–5 and migration behavior until Decisions 2 and 4 are recorded. Its only terminal output
will be the committed review artifact; it will not move the issue beyond
`status:needs-plan` or authorize implementation.

The dependency order will be: (A) #3533 publishes public-safe schema/pending records; (B) #839/#840/#3538 establish page/source, private-evidence, and citation contracts; (C) #837 Stage A installs mechanically blocked identities; (D) the qualified decisions above supply exact source and locators; (E) the private authority record and exactly one #837 resolver activate under separate approval; (F) #1588 calculation implementation begins. Resolver activation is therefore a pre-implementation gate, not a prerequisite for reviewing this plan.

As verified on 2026-07-16, #3533, #3538, #837, #839, and #840 are all OPEN at
`status:needs-plan`; none will be treated as landed by implication.

Any qualified-basis addition will amend this plan and receive fresh adversarial review before user approval or implementation. This issue will remain at `status:needs-plan` during the current review cycle; neither `status:plan-review` nor `status:plan-approved` will be self-applied.

## Risks and boundaries

- Public catalog metadata cannot validate numeric implementation details.
- Local possession is not reuse or publication authority.
- The existing `status:plan-approved` on #818 does not supersede fresh discovery that its numerical basis is unresolved.
- Existing tests largely mirror encoded behavior; they are not independent standards verification.
- A generic `PASS` can be misread as design acceptance; implementation will separate screening state from qualified acceptance.
- Unrelated standards returned by broad token search, including DNV-RP-F112 and DNV-RP-F204, are excluded unless the accepted calculation path explicitly invokes them.
