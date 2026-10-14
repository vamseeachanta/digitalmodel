# Hull-structure wastage FFS

Public-source research screen for [issue #2202](https://github.com/vamseeachanta/digitalmodel/issues/2202); not an implementation.

Single source: research note on the issue, sources checked 2026-10-11.

Research only; no code. All sources were checked on 2026-10-11. "Opened" means the page or the document text was read directly; "search snippet" means only a search-engine summary was seen and the claim is **unverified**. Rule and recommendation designations are given without edition years unless the opened copy states one. The common structural rules are openly published; only short defining sentences are quoted.

## 1. Bottom line

- The renewal logic this issue needs is **fully open** and simple enough to hand-check: one renewal-thickness formula per element, a fixed 0.5 mm reserve band, and three hull-girder zone checks. Golden tests on a synthetic thickness-measurement report are realistic.
- **Three scope corrections** came out of reading the rule text itself (section 3): the renewal chapter does not define "substantial corrosion" as 75 % of a margin; it does not contain pitting, grooving or edge-corrosion limits; and the recommendation number the issue cites for pitting intensity appears to be the container-ship survey guideline, while a flag-state instruction attributes the pitting intensity diagram to an IMO resolution. The screen should therefore carry **two rule families** (ships built to the common structural rules, and everything else on percentage diminution) and say which one it applied.
- No published numeric accuracy for ultrasonic gauging was found; measurement uncertainty has to be an explicit input.

## 2. Damage mechanisms

General corrosion, pitting, grooving next to welds, and edge corrosion at free edges and openings. A flag-state instruction gives working definitions: groove corrosion is "typically local material loss adjacent to weld joints along abutting stiffeners and at stiffener or plate butts or seams"; edge corrosion is "local corrosion at the free edges of plates, stiffeners, primary support members and around openings" [S2]. The rules split corrosion into "local and global" allowance and require renewal "if either the local or global corrosion allowance is exceeded" [S1, Pt 1 Ch 13 Sec 1, 1.2].

## 3. What the governing texts actually say

### 3.1 Common structural rules for bulk carriers and oil tankers, Part 1 Chapter 13 (class-society mirror; page footer reads 01 JUL 2025) [S1, opened]

- Local renewal: "Steel renewal is required if the measured thickness, tm in mm, is less than the renewal thickness, tren", with tren equal to as-built thickness minus the corrosion addition minus any voluntary addition (Sec 2, 2.1.1).
- Reserve thickness: "taken equal to 0.5 mm" (Sec 2 symbols; also Pt 1 Ch 3).
- The band between tren and tren plus the reserve thickness is where "Alternative solutions may be adopted" under the survey requirements (Sec 2, 2.1.3). This is the band the survey requirements treat as needing closer follow-up; the chapter itself does not use the words "substantial corrosion".
- Repair thickness: not less than as-built minus voluntary addition (Sec 2, 2.1.2).
- Global (hull girder), evaluated "during special surveys, for ships over 10 years of age" (Sec 2, 2.2.1):
  - deck and bottom zones: current section modulus at deck and at bottom "not to be less than 90% of the section modulus" with gross offered thickness, or alternatively the deck-zone and bottom-zone sectional areas not less than 90 %;
  - neutral-axis zone: current area not less than the area with gross offered thickness minus half the corrosion addition;
  - short cut: if every contributing item has lost less than 10 % (deck and bottom) or half the corrosion addition (neutral-axis zone), the check is satisfied without calculation (Sec 2, 2.2.2).
- Zone membership (which strakes and stiffeners count as deck, bottom and neutral-axis zones) is listed in Sec 2, 1.2 and differs between bulk carriers and oil tankers.
- Re-assessment of yielding, buckling and fatigue against new-building requirements "is not required during the operational life of ships, provided that the measured thickness ... remain greater than the renewal thickness" (Sec 1, 1.2.2). The issue's proposed buckling re-run on thinned panels is therefore an added-value tier above the rule, not a rule requirement.
- A text search of this copy found no pitting, grooving, edge-corrosion or "substantial corrosion" provisions anywhere in the chapter; hull survey requirements are delegated to UR Z10.2, UR Z10.4 and UR Z10.5 (Sec 1, 1.1.4 and 2.1.1).

### 3.2 Ships not built to those rules

- Substantial corrosion as "wastage in excess of 75% of allowable margins, but within acceptable limits" is attributed by a search result to IACS UR Z7 (hull classification surveys). **Unverified**: the mirror refused the connection.
- A flag-state instruction states the same idea from the other side: "Substantial corrosion means corrosion where less than 25% of the corrosion margin remains, but thickness is still greater than that for excessive corrosion", with the corrosion margin defined as original minus minimum allowable thickness [S2, opened].
- The same instruction publishes percentage diminution limits for steel vessels outside class (for example 30 % for shell plating and 25 % for longitudinals and stiffeners), "based on the installed plating thickness not the minimum calculated plate thickness", and tells surveyors to use the class society's own limits for classed designs [S2]. These are usable as a documented, citable default set for the non-rule-family branch.

### 3.3 Hull-girder limit for oil tankers in service

IMO resolution MSC.108(73), adopted 5 December 2000, urges governments to ensure the actual hull-girder section modulus of oil tankers "is not less than 90% of the required section modulus for new buildings specified in IACS' Unified Requirements S7 or S11, whichever is the greater" (for tankers of 130 m and above constructed on or after 1 July 2002, per the recital) [S3, opened]. Note the reference value differs from 3.1: required new-building modulus here, gross offered modulus there.

### 3.4 Pitting, grooving, edge corrosion

- The flag-state instruction reproduces a pitting intensity diagram "taken from IMO Resolution A.1049(27)" and states, "based on IACS Common Structural Rules": scattered pitting with intensity below 20 % and remaining thickness in pits not less than 70 % of original (or 3.0 mm if greater) may be repaired by welding; intensity below 5 % needs cleaning and coating only; grooving up to 15 % of stiffener height with at least 75 % of original thickness; edge corrosion up to 25 % of a flat-bar height or flange length [S2, opened]. It also gives a minimum edge thickness figure of "30% of the original thickness", which reads like a slip for 70 % and should be checked against the primary text before use.
- Because these limits are not in the Chapter 13 copy opened (3.1), their primary home is the survey requirements or an earlier rule edition. **Unverified** which.
- IACS Recommendation 84 is listed by a bookseller as "Container Ships. Guidelines for Surveys, Assessment and Repair of Hull Structures" (search snippet; **unverified**, and whether it contains pitting intensity diagrams was not checked). The issue's "pitting intensity classification per IACS Rec. 84 practice" should be re-pointed or confirmed.

## 4. Assessment methods and levels in use

1. **Screening:** per-reading comparison with renewal thickness and the follow-up band; the 10 % / half-corrosion-addition short cut for the hull girder [S1].
2. **Analytical:** hull-girder section modulus and zone areas from measured thicknesses [S1, S3]; panel buckling with reduced thickness; incremental-iterative (Smith-type) ultimate hull-girder strength.
3. **Numerical:** non-linear finite element collapse analysis; a public ship-structure research report couples a three-dimensional hydrodynamic model with a finite element model of the degraded structure and checks buckling modes "calculated according to IACS Common Structural Rules", so that the result "can be provided to a ship owner and operator to make operational and repair decisions" [S5, opened abstract].

## 5. Input data and where it comes from

- Ultrasonic thickness measurement by an approved service supplier, witnessed by the surveyor; IACS Recommendation 77 (the opened page states Rev.4, October 2017) is the surveyor's guideline for controlling the process, read with procedural requirement PR 19 [S6, landing page opened; document not opened].
- Report contents required by one regulator: reading locations, original thickness, allowable diminution, measured thickness, recorded diminution, items needing repair [S2]. That list is a ready schema for the screen's input file.
- Sampling: transverse sections "chosen where the largest reductions in thickness are suspected"; where class or IACS guidance is not followed, a grid no coarser than frame spacing up to 600 mm, tightened to 300-400 mm where excessive corrosion is indicated [S2].
- Plans on board are to show "both the as-built and renewal thickness" and any voluntary addition for each element [S1, Sec 1, 1.3.1]; without these the renewal thickness cannot be computed, which is the main data-availability risk for older or non-rule ships.
- **Measurement uncertainty:** an industry forum report found accuracy "can vary significantly depending on equipment and operator technique" and that more readings improve the representative-thickness estimate, but the abstract gives no numbers [S7, record opened]. Heavy scale "can affect both the conduct and accuracy of thickness measurements" [S2]. No published numeric tolerance was found.

## 6. Acceptance criteria in outline

ACCEPT when all readings exceed renewal thickness plus reserve and the hull-girder checks pass; MONITOR in the reserve band (or beyond 75 % of margin for the percentage family); RENEW below renewal thickness or when a zone or modulus limit fails. All numeric limits above are rule values from openly published texts and can be documented defaults; corrosion additions per compartment come from the rule's own table and must be inputs taken from the ship's plans, not inferred.

## 7. Published validation and benchmark material

| Candidate | Content | Usable? |
|---|---|---|
| Rule formulas in [S1] | Closed-form; a synthetic section can be hand-checked for each branch, including the short cut. | **Yes**, meets the issue's golden-test acceptance line. |
| Kuznecovs, Ringsberg, Johnson, Yamada, journal paper, CC BY [S4] | Double-hull tanker, non-linear finite element versus Smith method, intact and collision-damaged, new and corroded (the corroded case is assumed to represent 25 years of ageing, with corrosion margins estimated from the common structural rules). Good agreement between both methods and the rule formula for the intact non-corroded hull in vertical bending; "The differences between the two methods were larger for the corroded and damaged ship hull structure". | **Yes** as an external benchmark for the hull-girder tier and as a caution on Smith-method accuracy for corroded sections. |
| Ship-structure research report SSC-474, public release [S5] | Worked assessment process for a corrosion-degraded ship. | Useful as method reference; only the front matter and abstract were read. |
| Paper on pitting wastage and hull-girder ultimate strength using an international committee benchmark bulk carrier | Search snippet only. | **Unverified**. |

## 8. Who buys the work, and when

- **Class renewal surveys:** hull-girder evaluation at special surveys for ships over 10 years old [S1]; thickness measurement at each 10-year renewal survey for domestic commercial vessels in one jurisdiction, with owners to be advised when substantial corrosion or excessive diminution is found [S2].
- **Charter-market condition rating:** a class society announced on 27 January 2026 that its condition-assessment thickness analysis for tankers built to the common structural rules "moved away from percentage-based acceptance criteria" to fixed thickness values aligned with the original design requirements, citing benefits to owners planning maintenance and to charterers comparing ratings [S8, opened]. This confirms both the two-family split in section 1 and a live commercial driver.
- **Repair-or-operate decisions on aged ships:** the stated purpose of [S5].
- **Statutory enhanced survey programme for bulk carriers and oil tankers:** the IMO resolution in [S3] sits under it.

## 9. Fit assessment for a small analysis-led consultancy

- **Can offer now with open methods:** the complete screen (local, global, short cut), a steel-renewal quantity estimate from a thickness-measurement report, panel buckling and hull-girder ultimate strength as an optional higher tier, sensitivity to gauging uncertainty.
- **Needs licensed or owner data:** individual class societies' percentage diminution tables and condition-rating criteria; the ship's approved plans with as-built, renewal and voluntary-addition thicknesses; survey requirement texts for pitting and grooving limits if the open copies cannot be confirmed.
- **Differentiator:** speed and transparency before dry-dock (renewal tonnage forecast from the previous survey's readings plus a corrosion-rate projection), and an independent check for owners, buyers and insurers. Class societies own the formal verdict; this is decision support around it, not a substitute.
- **Verdict: strong fit. Lowest data and licensing risk of the offering screens reviewed on 2026-10-11, and the closest reuse of existing hull-girder and panel-buckling code.**

## 10. Consequences for this issue's objective

- Replace "substantial-corrosion band (75 % of the allowable margin), with the reserve-thickness convention" by an explicit rule-family switch: reserve band of 0.5 mm for ships built to the common structural rules; 75 %-of-margin band for others.
- Keep both hull-girder references (90 % of gross offered modulus or zone areas; 90 % of required new-building modulus for the IMO oil-tanker recommendation) and report which was used.
- Confirm the pitting reference before coding the intensity classes.

## Sources (all checked 2026-10-11)

- [S1] "Common Structural Rules for Bulk Carriers and Oil Tankers", class-society mirror NR606 (landing page states "Jan 2024 with corrigenda Jan 2025"; page footers in the file read 01 JUL 2025). Landing page https://marine-offshore.bureauveritas.com/common-structural-rules-bulk-carriers-and-oil-tankers ; file https://rulesexplorer-docs.bureauveritas.com/documents/nr606/jan2025/606-NR_2025-01.pdf (Part 1 Chapter 13 and Chapter 3 symbols opened)
- [S2] "Ultrasonic Thickness Measurement of Metallic Hulls", instruction to surveyors DCV-ITS-021 (05/2026), Australian maritime regulator. https://www.amsa.gov.au/sites/default/files/dcv-its-021-utm-for-metallic-vessels.pdf (opened)
- [S3] IMO Resolution MSC.108(73), adopted 5 December 2000. https://wwwcdn.imo.org/localresources/en/KnowledgeCentre/IndexofIMOResolutions/MSCResolutions/MSC.108(73).pdf (opened)
- [S4] Kuznecovs, Ringsberg, Johnson, Yamada, "Ultimate limit state analysis of a double-hull tanker subjected to biaxial bending in intact and collision-damaged conditions", Ocean Engineering 209 (2020) 107519, CC BY. https://research.chalmers.se/publication/517350/file/517350_Fulltext.pdf (abstract and corrosion-model section opened)
- [S5] "Structural Assessment of Aged Ships", Ship Structure Committee report SSC-474, 2018, approved for public release. https://www.iims.org.uk/wp-content/uploads/2018/07/Structural-Assessment-of-Aged-Ships.pdf (front matter and abstract opened)
- [S6] IACS Recommendation 77, "Guidelines for the Surveyor on how to Control the Thickness Measurement Process", landing page. https://iacs.org.uk/resolutions/recommendations/61-80/rec-77-rev4-cln (opened)
- [S7] "Gauging Accuracy and Gauging Guidelines", tanker structure forum report, 1986, bibliographic record. https://trid.trb.org/View/394311 (opened)
- [S8] Class-society press release on condition-assessment thickness analysis for tankers built to the common structural rules, 27 January 2026. https://www.lr.org/en/knowledge/press-room/press-listing/press-release/2026/lr-enhances-cap-thickness-analysis-to-improve-confidence-in-tanker-structural-assessments/ (opened)

## Could not verify

- The text of IACS UR Z7 and UR Z10.x (substantial-corrosion definitions, pitting and grooving limits, thickness-measurement tables): mirrors refused the connection; the official site's landing pages do not show the content.
- Whether [S1] is identical to the association's own current consolidated edition (a class-society mirror was opened, not the association's file).
- IACS Recommendation 84 content; IMO Resolution A.1049(27) itself; published corrosion-rate statistics by compartment (not searched in depth); any numeric ultrasonic gauging accuracy.
