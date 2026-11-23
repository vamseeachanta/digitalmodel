# Fixed-platform damaged and corroded member FFS

Public-source research screen for issue #2198; not an implementation.

research note on the issue, sources checked 2026-10-11

Research only; no code. All sources were checked on 2026-10-11. "Opened" means the page or document text was read directly; "search snippet" means only a search-engine summary was seen and the claim is **unverified**. Standards are cited by designation; no edition is given unless an opened source states it. This screen is shallower than the three priority screens posted on 2026-10-11 (mooring chain, flexible pipe, hull wastage); section 9 lists what a reviewer should open next.

## 1. Bottom line

- Demand is real and documented by the regulator: one hurricane season left 116 fixed platforms destroyed and 163 with major damage, and the damage list reads like this issue's scope (dents, bows, holes, severed members) [S1].
- **Public test series for dented tubulars exist, but the data tables are in journal and conference papers, not in open reports.** Bibliographic records confirm test counts, D/t values and dent depths for the main series [S2, S3, S4]; the specimen-level numbers needed for golden tests must be taken from the papers through a licensed route, or from one open doctoral thesis [S5].
- No opened source states the capacity-reduction formulas used in the design codes. The issue's own knowledge note is right that the formulas must come from citable papers; the codes' text is licensed.

## 2. Damage mechanisms

Regulator-commissioned hurricane assessment [S1, section 7.1, opened]:
- Braces: "Buckles, dents, holes, cracks, tears, out-of-plane bowing, severed members."
- Legs: buckles, dents, holes, cracks, tears, severed legs.
- Joints: cracks at the weld and into chord or brace, punch-through and pull-out of braces.
- Cause: "primarily due to strength overload".
- "Major damage below water included separated or torn members, cracked members, members with numerous holes, dents or a combination of these, cracked welds, missing members or buckled members" (section 5).

Dropped objects and vessel collision are the usual sources of dent damage outside storms (search snippet of two research-database abstracts; **unverified**). Patch corrosion of tubulars, including multiple patches, was the subject of two regulator-funded joint industry projects [S6, S7, opened].

## 3. Governing standards and recommended practices (designations only)

| Document | Role | Status of check |
|---|---|---|
| API RP 2SIM | Structural integrity management of existing fixed offshore structures; described in press coverage as "a continuous process used for demonstrating the fitness-for-purpose of an offshore structure from installation through to decommissioning". | Search snippet; **unverified**. A regulator workshop file lists it alongside API RP 2RIM and API RP 2MIM [S8, opened, headings only]. |
| API RP 2A | Design of fixed platforms; the hurricane study ties platform performance to its design practices and notes that most destroyed platforms were of 1960s-1970s design, before industry guidance matured [S1]. | [S1] opened |
| ISO 19902, NORSOK N-004, NORSOK N-006 | Named in this issue as holding damaged-member provisions. | **Not opened**; no claim made here about their content. |

## 4. Assessment methods and levels in use

1. **Screening:** classify damage (dent depth, out-of-straightness, wall loss, flooded, severed, cracked) and compare with ranges covered by test-calibrated formulas.
2. **Analytical:** semi-empirical residual-strength equations for dented and bowed tubes. The lineage visible in open records: a 1984 formulation for damaged bracing members (cited in later papers; search snippet), a 1985 method giving "parametric diagrams of residual strength against dent depth and D/t" (search snippet), a doctoral thesis that builds moment-curvature relations for dented sections and a residual-strength formulation "based on the Perry formula" [S5, abstract opened], and modified equations that "reasonably assess the residual strength of the dented braces subjected to combined flexural and axial load" [S3, record opened].
3. **Numerical:** non-linear finite element analysis of the damaged member, and system pushover for reserve strength when a primary member is severed or several are damaged (the issue already routes this to the Level 3 chain).

## 5. Input data

Underwater inspection findings (dent depth and length, bow, wall thickness in corroded patches, flooded-member detection, crack indications), member geometry and material, and loads from the structural model. The hurricane study notes how uneven reported damage data is: minor and moderate damage reports were "sometimes vague and open to interpretation", while major damage reports were more descriptive [S1, section 5]. No published measurement-uncertainty figures for underwater dent or bow measurement were found in the opened sources.

## 6. Published validation and benchmark material

| Series | What the opened record states | Access |
|---|---|---|
| Landet and Lotsberg, journal paper, 1992 [S2] | 35 ultimate-load tests: 4 undented, 18 with dent depth 10 % of diameter, 13 with 20 %; D/t of 31 and 47; axial force combined with moment, rotation measured. | Record opened; paper not open. Best single candidate for the issue's golden tests and applicability flags (D/t 31-47, dent depth up to 20 % of diameter). |
| Ricles, Lamport, Gillum, conference paper, 1992 [S3] | Six dented braces in three pairs (axial, and combined axial plus bending); "pronounced" loss of residual strength under axial load. | Record opened; D/t and dent depths not in the abstract. |
| Later grout-repair tests by the same group | 13 tests including undamaged, dented (10 % of diameter) and repaired specimens; grout arrested dent growth and restored capacity. | Search snippet; **unverified**. |
| Doctoral thesis, University of Glasgow, 1987 [S5] | Lateral impact tests and residual strength tests under axial compression with hydrostatic pressure; design equations for extent of damage and residual strength. Full text is downloadable from the university repository (18 MB); copyright stays with the author. | Abstract page opened; full text not read. The only open full-text data source found. |
| Dented high-strength line pipe under axial compression, journal paper, 2015 [S9] | Existing semi-empirical equations for low-strength steel are reasonable for shallow dents and "considerably underestimate" residual capacity for deep dents in X80 pipe. Repository manuscript under CC BY-NC-ND. | Record opened. Useful as a material-grade applicability flag. |
| Patch-corroded tubulars, two regulator-funded projects [S6, S7] | Axial tests and analytical models for single and multiple corrosion patches; models "fairly successful in predicting the capacity of the corroded members tested". | Records opened; final reports are not linked from the records. |

## 7. Who buys the work, and when

- **Post-storm assessment:** about 3,000 platforms were exposed in one season; 116 destroyed and 163 with major damage; "Generally these types of damage required some sort of above or below water repair or similar mitigation" [S1].
- **Ageing fleet:** most destroyed platforms were older-vintage designs [S1].
- **Routine underwater inspection findings and collision or dropped-object events** under a structural integrity management programme (search snippets on API RP 2SIM; **unverified**).

## 8. Fit assessment for a small analysis-led consultancy

- **Can offer now:** corroded-member re-check by thickness reduction in the existing member and joint checks (pure code reuse); a dented or bowed member screen once the formulas are taken from the cited papers; routing of flooded, severed and cracked members to the numerical tier.
- **Needs licensed standards or papers:** the code formulas and their limits of validity; the specimen tables for golden tests.
- **Differentiator:** fast, documented triage between "accept", "repair" and "needs pushover" after an inspection campaign or storm, with explicit applicability flags. Established integrity contractors already own the post-storm assessment relationship with operators, so entry is more likely as a specialist subcontractor or for smaller operators.
- **Verdict: reasonable fit, medium data risk.** The corroded-member branch is low risk; the dented-member branch depends on acquiring two or three papers.

## 9. What a reviewer should open next

1. The two 1992 test papers [S2, S3] for specimen tables, and the thesis [S5] for open data.
2. The regulator's research-report series on dented and grout-filled tubulars (referenced in the bibliographies of the papers above; not found online on 2026-10-11).
3. The publisher scope pages for API RP 2SIM and ISO 19902 to confirm the damage-evaluation wording.

## Sources

- [S1] "Assessment of Fixed Offshore Platform Performance in Hurricanes Katrina and Rita", final report to the United States offshore regulator, May 2007. https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program/578aa.pdf (executive summary, sections 5 and 7 opened)
- [S2] Landet and Lotsberg, "Laboratory Testing of Ultimate Capacity of Dented Tubular Members", Journal of Structural Engineering 118(4), 1992, bibliographic record. https://trid.trb.org/View/440978 (opened)
- [S3] Ricles, Lamport, Gillum, "Residual Strength of Damaged Offshore Steel Tubular Bracing", conference paper OTC 6938, 1992, bibliographic record. https://trid.trb.org/View/441898 (opened)
- [S4] Search results listing further records in the same database (combined bending and dent damage; collapse and residual strength of damaged tubular members): not opened.
- [S5] Cho, "Design Approximations for Offshore Tubulars Against Collisions", PhD thesis, 1987. https://theses.gla.ac.uk/77600/ (abstract page opened)
- [S6] "TAP-101 Residual Strength of Offshore Structures After Damage", regulator research record. https://www.bsee.gov/research-record/tap-101-residual-strength-offshore-structures-after-damage (opened)
- [S7] "TAP-277 Structural Integrity Assessment and Repair of Corrosion-Damaged Offshore Platform Tubulars", regulator research record. https://www.bsee.gov/research-record/tap-277-structural-integrity-assessment-and-repair-corrosion-damaged-offshore (opened)
- [S8] Regulator workshop slides on a floater evaluation and assessment team. https://www.BSEE.gov/sites/bsee.gov/files/feat-for-bsee.pdf (text partly extractable; only headings read)
- [S9] Zeinoddini, Ezzati, Parke, "Plastic buckling, wrinkling and collapse behaviour of dented X80 steel line pipes under axial compression", 2015, repository record. https://openresearch.surrey.ac.uk/esploro/outputs/journalArticle/Plastic-buckling-wrinkling-and-collapse-behaviour/99516279702346 (opened)

## Could not verify

- Any content of API RP 2SIM, ISO 19902, NORSOK N-004 or NORSOK N-006 (not opened).
- UK regulator reports on ageing and life extension and on dented tubulars (pages returned "not found" on 2026-10-11).
- Specimen-level data for every test series except what the bibliographic records state.
- The later hurricane assessment report for the 2008 season (file too large to fetch on 2026-10-11).
