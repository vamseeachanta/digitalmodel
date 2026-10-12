# Mooring-chain degradation FFS

Public-source research screen for issue #2199; not an implementation.

Single source: research note on the issue, sources checked 2026-10-11.

Research only; no code. All sources were checked on 2026-10-11. "Opened" means I read the page or the document text myself. "Search snippet" means I only saw a search-engine summary of the page and the claim is **unverified** until someone opens the source. No edition years are given for standards except where the opened source states one. Nothing here reproduces normative text from a paid standard.

## 1. Bottom line

- The issue's screen is buildable from open sources for **fatigue with corrosion and mean load** (a CC BY data set of 154 full-scale tests with a published corrosion-grade scale exists) and for **wear** (a regulator-published analytical method exists).
- It is **not** buildable from open sources for **pitted-link residual strength**: the joint industry project that defined the pit classes validated its method against break tests, but the break-load numbers and the pit-class boundaries are in conference papers and proprietary reports I could not open. The honest design is a cross-section-loss lower-tier estimate plus an "outside published bounds, escalate" flag, exactly as the issue already says.
- Discard thresholds are a licensed-text matter. One open regulator-commissioned report describes the industry inspection practice as a retirement criterion of limiting strength reduction to 10 % of minimum breaking strength; that is enough to document a default, and the value must stay an input.

## 2. Damage mechanisms that drive the work

| Mechanism | What the public record says | Source |
|---|---|---|
| General (uniform) corrosion | Design practice handles corrosion "in a simplified manner by a reduction of the cross section area of the chains". | Lone et al., preprint, p. 1-2 (opened) [S1] |
| Pitting, including microbiologically influenced corrosion | Forensic study of severely pitted chain recovered from two floating production units in warm waters: pit depth growth up to about 10 times the uniform rate; residual strength in destructive tests at worst 25 % below as-new breaking load; strength loss did not track area loss simply and depends on pit geometry and position relative to high-stress zones. | Fontaine et al., repository record of OTC paper, DOI 10.4043/25234-ms (record opened, paper not) [S2] |
| Interlink and fairlead wear | A regulator-published method covers adhesion, corrosion, surface fatigue and erosion wear between adjacent links, with a condensed practitioner version in an appendix. | HSE RR1092 summary via FABIG page (summary opened; the report PDF itself returned an error for me) [S3] |
| Unexpected mechanisms found in service | "microbial action, excessive wear at the touchdown point [the thrash zone], interaction between different material types, out-of-plane bending of chain". | BSEE-commissioned study, Executive Summary p. i (opened) [S4] |
| Fatigue, with corrosion and mean-load dependence | Pits and roughness reduce fatigue capacity by more than the simplified cross-section correction; capacity depends strongly on mean load. | [S1] p. 2 (opened) |
| Connectors | A disproportionate share of failures is in connectors (the study cites a 2013 conference review). | [S4] (opened) |

Failure statistics (trade-press report of two 2013 conference papers; the papers themselves were not opened): 21 permanent-mooring failures 2001-2011, nine of them multiple-line; chain the largest share of incidents (47 %); 12 of 23 incidents in the first five years of service. [S5]

## 3. Governing standards and recommended practices (designations only)

| Document | What it covers, per the opened source | Status of my check |
|---|---|---|
| API RP 2SK, API RP 2I, API RP 2SM | Stationkeeping design, in-service inspection of mooring hardware, synthetic rope moorings. The BSEE study says these are the documents incorporated by reference in US regulation, and that API RP 2I "is the only mooring inspection standard that gives strong and explicit criteria and/or guidance" but was developed primarily for mobile-unit moorings and in-air inspection. | [S4] opened |
| API RP 2MIM | Mooring integrity management for permanent floating production systems. The 2015 BSEE study recorded it as planned; search snippets say it has since been published and describe it as an assessment (fitness-for-service) document rather than a design document. | Publication and scope: **unverified** (search snippet only) |
| DNV-RP-E308 | "provides guidance on the application of mooring integrity management for existing and new assets"; covers design, manufacturing, installation and operation, including life extension and relocation. The landing page states edition 2021-10. | DNV landing page opened [S6] |
| DNV-OS-E301 | Position mooring; the stress-based S-N approach for chain is attributed to it in [S1] ("a stress-based assessment ... is prescribed by DNVGL-OS-E301, whereas 'T-N' curves ... [are] required by API-RP-2SK"). | Scope page returned an error; characterisation taken from [S1] (opened) |
| ISO 19901-7 | Stationkeeping systems for floating offshore structures and mobile offshore units; a UK regulator note says its principles can be used in the renewable sector. | [S7] opened; ISO page returned an error |
| IACS Recommendation 38 | Survey of offshore mooring chain cable in use; "Major class societies, in general, also follow" it for in-service inspection. | [S4] opened; the recommendation itself not opened |
| IACS UR W22, ISO 20438 | Manufacture, grades, dimensions and tolerances of offshore mooring chain (needed for as-new geometry and MBL). | **Unverified** (search snippet only) |
| Energy Institute guidance on management of mooring integrity, part 1 (chain systems) | Industry guidance document. | **Unverified** (page returned access denied) |

## 4. Assessment methods and levels in use

1. **Screening:** compare measured bar diameter and interlink dimensions with the discard threshold; reduce break strength by the lost cross-section. This is the practice the codes embody; the pitting joint industry project was started because codes "were formulated based on uniform corrosion, and not highly localized material loss" (search snippet of the project page; **unverified**, the page returned an error for me).
2. **Analytical:** wear-rate prediction [S3]; S-N or T-N fatigue with corroded diameter [S1]; the extended S-N form with intercept as a function of mean load and corrosion grade [S1].
3. **Numerical:** non-linear finite element residual-strength analysis of a scanned or idealised degraded link, correlated to break tests; a non-linear multiaxial finite element fatigue method that "correlated well with fatigue test data, while the linear FEA method used in industry did not" (abstract summary of Farrow, Potts and Washington, opened on a public database record) [S8]. A response-surface tool built on that analysis is the commercial state of the art (search snippet; **unverified**).

## 5. Input data and where it comes from

- Inspection is mostly visual, by diver or remotely operated vehicle. "The standard requirement of dimensional measurements can be extremely difficult underwater"; three-dimensional scanning equipment exists but is "not in common use". In-service inspection "is not good at finding smaller scale defects such as fatigue cracks in chain". Cleaning marine growth to measure a link increases corrosion afterwards. [S4] pp. i-iii (opened)
- Special periodic inspection at about five-year intervals with measurements at the fairlead region, splash zone, touch-down region and connectors. [S4] (opened)
- **Published measurement uncertainty for underwater chain diameter readings: not found in any source I opened.** The screen should carry measurement uncertainty as an explicit input and report sensitivity to it.
- Corrosion grade for fatigue is assigned by expert visual judgement on a 1 to 7 scale; the authors state the "inevitable subjectivity involved" and that scan-based grading is still being developed. [S1] section 2.2 (opened)

## 6. Acceptance criteria in outline

- Retirement criterion in the inspection practice: "limiting the strength reduction to 10% minimum breaking strength"; the same source records that, with a sound design check, a component may be accepted for continued service beyond that criterion. [S4] code-review table (opened). This is the documented default for the screen; it stays an input.
- Cracks in chain: the inspection practice "gives actual discard criteria for certain types of defects (e.g., cracks on chain ...)". [S4] (opened). Consistent with the issue's "visible cracks: REPLACE or Level 3" flag.
- Design corrosion allowance: a search snippet attributes an average rate of 0.4 mm/year to the stationkeeping practice and reports that observed long-term rates on highly corroded links were significantly higher. **Unverified** (snippet only); do not hard-code.

## 7. Published validation and benchmark material

| Candidate for golden tests | Content | Usable? |
|---|---|---|
| Lone et al. preprint, licence CC BY 4.0 [S1] | 154 full-scale seawater fatigue tests on studless chain (67 new, 87 used from eight installations), grades R3 to R5, 70 to 171 mm, mean load 6 to 20 % of MBL, corrosion grades 1 to 7, service life 5 to 20 years; Table 1 gives nominal diameter and MBL per batch; tables of fitted coefficients and design-curve parameters. | **Yes** for the fatigue tier and for nominal MBL per diameter and grade. The batch table is in the paper; individual test points are plotted, not tabulated (the page says data download "is not yet available"). |
| Corrosion-grade scale, same source, Table 2 | Seven verbal grades with pit depths (for example grade 2: scattered pitting less than 1 mm deep; grade 7: severe widespread pitting 3-6 mm and deeper). | **Yes** as a documented, cited classification; it is a different scale from the small / medium / mega pit classes named in this issue. |
| Forensic study [S2] | Worst-case 25 % strength loss from break tests of pitted links. | Bound only; no per-link data in the record. |
| Chain residual-strength joint industry project conference papers | Break-test correlation, uniform and mega-pit parametric results. | **Not open**: papers and pages not retrievable; treat class boundaries and strength factors as inputs. |
| HSE RR1092 wear method [S3] | Analytical wear method with calibration. | Likely yes, but the report text must be opened first (returned an error from this host). |
| Seawater corrosion of rope and chain joint industry project | Regulator record says results "are proprietary until 3 years after JIP termination" and that a draft final report on corrosion and wear of chain links was under review. | [S9] opened; check whether a final public report now exists. |

## 8. Who buys the work, and when

- **Life extension:** "many of the installed mooring systems are reaching their original design life, and the operators lack the data to be able to demonstrate that the fatigue loading is as originally expected". [S4] p. iii (opened)
- **Inspection findings and post-event checks:** five-yearly special surveys, and inspections after large storms or dropped objects. [S4] (opened)
- **Undetected line failure:** "a number of cases in recent years when inspections have revealed a failed line with no information as to when the line actually failed". [S4] p. ii (opened)
- **Regulatory gap closure:** the same study names the absence of a required mooring integrity management plan as "a major gap in the U.S. regulations". [S4] (opened)
- **Floating wind and marine energy:** UK regulators expect duty holders to use oil and gas mooring practice as the starting point and require third-party verification of the mooring system. [S7] (opened). This is a growing buyer group with thinner in-house mooring expertise.

## 9. Fit assessment for a small analysis-led consultancy

- **Can offer now with open methods:** corroded-diameter strength and fatigue screening; mean-load and corrosion-grade corrected fatigue life with the published model [S1]; wear projection after [S3] is opened and checked; a clear input-uncertainty statement, which the public record shows is the weak point of every inspection data set.
- **Needs licensed standards or proprietary data:** the numerical discard thresholds and inspection intervals (licensed practices); pit-class boundaries and pitted-link strength factors (proprietary project results); as-new dimensional tolerances (licensed or unverified here).
- **Differentiator:** a transparent, reproducible screen with every threshold as a cited input and an explicit escalation boundary, sold as an independent second opinion on life extension or on an inspection finding. The established player owns the validated pitted-link finite element method; competing on that head-on is not realistic without break-test data.
- **Verdict: good fit for fatigue and wear tiers; partial fit for pitting (screen and escalate only).**

## 10. Consequences for this issue's acceptance criteria

- "Golden tests against published chain corrosion / MBL data": use the batch table and design-curve tables in [S1] for fatigue; use nominal diameter-to-MBL pairs from the same table as a sanity check on the as-new strength formula. No open per-link corroded break-load data set was found.
- "Pitting: classification into the small / medium / mega classes": no open definition of the class boundaries was found. Either obtain the conference papers through a licensed route or implement the openly published 1-7 grade scale and keep the three-class scheme as a configurable mapping.

## Sources

- [S1] Lone, Maincon, Gabrielsen, Sauder, Larsen, Leira, "Analysis of S-N data for new and corroded mooring chains at varying mean load levels using a hierarchical model", preprint, CC BY 4.0. https://engrxiv.org/preprint/view/2377 (page and PDF text opened)
- [S2] Fontaine, Rosen, Potts, Ma, Melchers, "SCORCH JIP - Feedback on MIC and pitting corrosion from field recovered mooring chain links", repository record. https://scholars.lib.ntu.edu.tw/handle/123456789/582634 (record opened)
- [S3] "HSE RR1092: Practical method for calculating mooring chain wear for floating offshore installations", summary page. https://www.fabig.com/external-publications/hse-rr1092/ (opened); programme summary "HSE RR1090" https://www.fabig.com/external-publications/hse-rr1090/ (opened)
- [S4] "Study on Mooring System Integrity Management for Floating Structures", final report to the US offshore safety regulator, dated August 4, 2015. https://bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program//730-aa.pdf (text opened)
- [S5] "Mooring line failures attract industry attention", trade press, 16 July 2013. https://www.oedigital.com/component/k2/item/3477-mooring-line-failures-attract-industry-attention (opened; secondary report of two conference papers)
- [S6] "DNV-RP-E308 Mooring integrity management", landing page. https://www.dnv.com/energy/standards-guidelines/dnv-rp-e308-mooring-integrity-management/ (opened)
- [S7] "Regulatory expectations on moorings for floating wind and marine devices", August 2017. https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/640962/Regulatory_expectations_on_mooring_devices_from_HSE_and_MCA.PDF (text opened)
- [S8] Farrow, Potts, Washington, "Investigations Into Fatigue Performance of Offshore Mooring Chains", database record. https://tethys-engineering.pnnl.gov/publications/investigations-fatigue-performance-offshore-mooring-chains (opened)
- [S9] "TAP-641 - Seawater Corrosion of Rope & Chain (SCORCh)", regulator research record. https://www.bsee.gov/research-record/tap-641-seawater-corrosion-rope-chain-scorch (opened)

## Could not verify

- The regulator's own report pages and PDFs for RR444 and RR1090-RR1098, and every page on the pitting project owner's site, returned "not found" from this host; claims that rest on them are marked above.
- Current publication status and scope wording of API RP 2MIM; IACS UR W22 and ISO 20438 content; the Energy Institute guidance; the 0.4 mm/year allowance.
- Any published accuracy figure for underwater chain dimensional measurement.
