# Riser standards citation audit

The seven-input inventory records 152 literal citation occurrences, three edition-label differences and 62 occurrences without a local year. No normative calculation error or divergent threshold is established. Existing formulae, thresholds, labels and register entries are retained; unresolved editions require document-holder review before selecting a correction.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-11-16 | Record the bounded citation evidence and unresolved applicability to support review. |

## Evidence and coverage

[riser-citation-audit.json](riser-citation-audit.json) contains exact source anchors, literal spellings, editions, clauses, register keys, intended-use status and finding dispositions. IDs A–G identify inputs below; occurrence suffixes distinguish mentions. Lines locate the recorded base; tests resolve anchors anywhere in the current file. Digests are provenance only and are not test assertions. Base: `c73997e92c145f1a11c0c3b7e9dfcc678a7eab11`, inspected 2026-11-16. Each digest is SHA-256 of that base file; all seven inputs equalled the base when inventoried.

| Input | Repository path | Base SHA-256 |
|---|---|---|
| A | `src/digitalmodel/codes.py` | `339657665d6afa81bbde24243914f5146696f7e10e980e69d7af853cbec2cc54` |
| B | `docs/domains/codes-register.md` | `82f3ee8d175bf2336d06bd86f3dedf0d9840b24b5a9c3fa2813b174f1c4b64bc` |
| C | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py` | `a6fbddd502b5fdc04978e7bf1356f1079c2a37e15f37c87cd99cddba8ec668ef` |
| D | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py` | `a9882b2be352da7cc873d5e4a9d0edfcec914205572842067696d8395325121f` |
| E | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py` | `6c84e135b478e4f44a9cd73e6195628bc81bd9e6410191306578ef2027a7e76d` |
| F | `src/digitalmodel/structural/analysis/wall_thickness_codes/manifest.yaml` | `272f217d597eb0f9000a8a2795ac41c7b2c44023dc894a9f2031c9b336d9aed2` |
| G | `src/digitalmodel/structural/fatigue/worked_examples.py` | `b169385389ec5db52a255c1b078f4d55b3174b71128b8d63bd9cb4de8aa48e4a` |

Register coverage is 28/36 unique designations (77.8%). Composite register labels contribute their separate named designations; Eurocode 3 maps to EN 1993-1-1. Explicit publisher/spelling aliases map DNVGL to DNV, hyphenated API and ASME labels to spaced labels, and abbreviated RP/STD or ST/OS labels to their full designations. These aliases preserve RP versus STD and OS versus ST and do not establish edition equivalence. The eight unmatched designations are API RP 2SK, DNV CN 30.1, DNV-OS-E301, DNV-OS-F201, DNV-RP-C203, DNV-RP-E301, ISO 13628-7 and OTC 20228. The methodology paper is retained in the denominator as an unmatched reference rather than silently excluded.

## VERSION CONFLICT

| ID | Observed evidence | Action and rationale |
|---|---|---|
| V01 | F201: A014 / E004 state 2021; G022 states 2018. | Deferred: compare the strength strategy and fatigue-factor applicability against their stated editions; unlike methods do not alone prove a calculation conflict. |
| V02 | F101: A015 states 2021; G014 states 2017. | Deferred: establish the fatigue example's governing edition before reconciling its label with the strength register. |
| V03 | C203: B023 records a 2016 VIV reference; G004 and other C203 entries cite 2011 curves. | Deferred: the VIV implementation is outside the fixed inputs; this is a documented reference difference, not a verified implementation conflict. |

Three differences remain unresolved. Historical OS-F201 (2010) mentions are not counted as ST-F201 conflicts. RP 2RD retains the register's unverified 2013 label: a holder of the earlier RP document must compare the strategy's checks and allowable-factor basis before resolving the edition. The existing register's publisher retrieval notes are historical evidence only; no publisher source was fetched for this audit.

## IMPLAUSIBLE CLAUSE

| ID | Screening evidence | Action and rationale |
|---|---|---|
| C01 | Manifest ASME S403.2 family; F201 Sec. 5 and Cl. 5.4.4; fatigue Table, Fig. and Cl. locators. The JSON clauses map retains continuation-line locators. | No implausible numeric pattern established; normative correctness is NOT_EVALUATED. Section and subclause syntax is plausible internally but does not establish clause existence, title or applicability. |

## MISSING VERSION

| ID | Coverage: occurrences with no local year | Action and rationale |
|---|---|---|
| M01 | A: 2; illustrative ASME label and IACS UR W11. | Deferred: an illustrative label is not an edition selection; W11 requires the applicable revision. |
| M02 | B: 8; abbreviated references and unresolved register rows. | Deferred: repeated narrative labels do not establish an implemented edition; retain the documented uncertainties. |
| M03 | C: 11; RP 2RD strategy and RP 1111 analogies. | Deferred: obtain edition-specific RP method evidence; the register's RP year remains unverified. |
| M04 | D: 9; STD 2RD strategy. | Deferred: confirm implemented method applicability before inheriting the register year. |
| M05 | E: 12; F201 labels and F101 analogies. | Deferred: surrounding 2021 and historical 2010 mentions do not qualify every method. |
| M06 | F: 1; ASME description without its own year. | Deferred: primary metadata states 2019 elsewhere; confirm that inheritance is intended for the described check. |
| M07 | G: 19; fatigue, mooring and methodology references. | Deferred: obtain applicable document editions and method/condition evidence; do not infer them from other references in the example. |

Each listed occurrence has a finding ID, deferred action and rationale in JSON. Null means no year at the literal occurrence, not necessarily an absent year elsewhere in its module. All 62 are retained without invented years.

## DIVERGENT THRESHOLD

| ID | Source symbols and comparison conditions | Action and rationale |
|---|---|---|
| T01 | C `ApiRp2rdStrategy.C_A` / operating WSD versus D `ApiStd2rdStrategy.COMBINED_DESIGN_FACTOR` / SLS/ULS internal LRFD Method 1; both dimensionless. | NON_COMPARABLE: designation, parameter, condition and method differ; no conflicting same-standard allowable is established. |
| T02 | E `DEFAULT_FACTORS` / `gamma_SC` for pressure containment versus G `scr_touchdown` / `dff` for high-class fatigue; both dimensionless. | NON_COMPARABLE: edition labels, parameter, limit state, condition and method differ. Document review is required before qualifying either factor. |

Comparisons carry designation, edition, parameter, units, limit state, condition and method. Unknown editions are explicitly unverified and do not qualify a comparison. No licensed numerical table or factor value is reproduced.

## Limits and validation

The inventory is a manually reviewed lexical slice of the seven files, including their register/documentation references to other modules; those other modules were not inspected. The tests cover the declared designation and locator vocabulary, not arbitrary abbreviations or semantic correctness. The report does not establish full-repository coverage; transfer-ledger coverage is not established. The original report destination, complete sweep and conflict-resolution acceptance remain follow-up under #272.

Offline validation checks the exact input manifest, occurrence counts, aliases, live anchors, clause locator sets, register membership, recomputed coverage and dispositions. Synthetic negative cases reject missing inputs/occurrences/clauses, stale anchors, missing actions, conflated identifiers, false denominators unlike units/conditions and unverified editions. The existing register assertions are unchanged. Second-provider artifact review remains pending; no approval is recorded here.
