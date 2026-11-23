# Worn and corroded casing remaining collapse and burst

Public-source research screen for issue [#2201](https://github.com/vamseeachanta/digitalmodel/issues/2201); not an implementation.

Single source: research note on the issue, sources checked 2026-10-11.

First-person wording of the research note was restated in the third person with no change of content.

Research only; no code. All sources were checked on 2026-10-11. "Opened" means the page or document text was read for this screen; "search snippet" means only a search-engine summary was seen and the claim is **unverified**. Standards are cited by designation; an edition appears only where an opened source states it. This screen is shallower than the three priority screens posted on 2026-10-11; section 9 lists what a reviewer should open next.

## 1. Bottom line

- The calculation in this issue (reduced wall into the standard performance-property formulas) is the industry's baseline practice, and published work says it is **conservative for collapse and not reliable for a local groove**: a uniform-wall reduction is "very conservative" (thesis abstract, search snippet), while standard burst and collapse models were "not suitable" for a single crescent groove (thesis abstract, search snippet). Both are **unverified** here because the repositories refused the connection. The screen should present the uniform-wall result as a lower-tier estimate and flag local grooves for a higher tier.
- The golden-test source named in the acceptance criteria (the technical report's own example properties) is a **licensed document**; no open copy was found. Open alternatives exist for the unworn baseline (the formulas' regime structure is described in open papers) but not, from the sources that could be opened, for worn-pipe test data.
- A regulator safety alert documents the failure mode exactly: casing "extensively worn during operations, allowing failure at pressures significantly below rated burst" [S1].

## 2. Damage mechanisms

- **Drilling wear:** internal wear from friction between rotating drill-string tool joints and the casing, worse in deep and directional wells with high side loads at doglegs; it "can lead to a considerable decrease in casing resistance" [S2, abstract and introduction opened].
- **Corrosion:** a regulator investigation summary describes loss of an outer conductor exposing surface casing "to severe corrosion, which then collapsed" (search snippet; **unverified**).
- **Collapse regimes:** governed by slenderness D/t; yield collapse for thick walls, plastic collapse for intermediate, elastic collapse for thin walls, plus a transition regime in the standard formulation [S2, section 2, opened].

## 3. Governing standards and technical reports (designations only)

| Document | Role | Status of check |
|---|---|---|
| API TR 5C3 | Collapse-strength models for unworn pipe; four collapse regimes. | Described so in [S2] (opened), which cites a 2008 edition. Publisher page not opened. |
| ISO/TR 10400 | Equations and templates for casing, tubing, drill pipe and line pipe properties, including axial strength, internal pressure resistance and collapse resistance; a later edition has replaced the first. | Search snippet of standards-catalogue pages; **unverified** (the publisher page returned access denied). |
| API 7CW | Test procedure addressing worn tubes. | Described so in [S2] (opened), which cites a 2015 edition. |
| NORSOK D-010 | Well integrity; the Norwegian regulator's well-status survey refers to it. | [S3] opened (reference only). |

## 4. Assessment methods and levels in use

1. **Screening:** remaining wall as a percentage; linear de-rating of burst with wall.
2. **Analytical:** standard formulas with reduced wall and measured ovality (this issue); published empirical corrections for a crescent groove. One model built from 135 finite element geometries reports a 5.2 % average difference against six collapse tests; a doctoral study reports a mean test-to-prediction ratio of 1.003 with standard deviation 0.077 across 36 collapse tests for a stress-concentration approach (both search snippets; **unverified**).
3. **Numerical:** elastoplastic finite element collapse of the worn cross-section; an open paper finds "a nonlinear decay of the collapse resistance" for wear between 3 % and 18 % of wall on a T-95 tube, with the model validated only for the perfect tube [S2].

## 5. Input data

- Multi-finger caliper logs measure inner radius, not wall thickness; ultrasonic and electromagnetic tools estimate wall. Vendor-stated radial accuracy of about 0.02-0.03 inch and resolution of a few thousandths of an inch for calipers (search snippets of vendor sheets and a practitioner forum; **unverified**, and not independent measurements).
- Operational indicators the regulator recommends watching: "rig alignment, monitoring of ditch magnet recoveries, and frequent examination of the drilling wear bushing" [S1].
- Because caliper accuracy is the same order as the wear depths that matter on thin strings, the screen needs measurement uncertainty as an explicit input.

## 6. Acceptance criteria in outline

Design-factor comparison of de-rated collapse, burst and axial ratings with the well's load cases; the operator's well-barrier policy sets the factors. Nothing normative was opened; factors stay as inputs.

## 7. Published validation and benchmark material

| Candidate | Content | Status |
|---|---|---|
| Example properties in the licensed technical report | Named in this issue's acceptance line. | Licensed; not open. |
| Open paper [S2] | One tube grade, wear 3-18 %, finite element only. | Opened; no physical tests. |
| Collapse test sets used in the two model papers (6 and 36 tests) | Physical collapse tests on worn pipe. | Search snippets; **unverified**; papers not opened. |
| Experimental and numerical studies cited in [S2] (Kuriyama et al., Sakakibara et al., and a thesis by Moreira Junior) | Worn-pipe collapse tests at several D/t. | Cited in [S2]; not opened. |

## 8. Who buys the work, and when

- **Well-barrier status and plugging decisions:** the Norwegian regulator's 2024 survey covered 488 temporarily abandoned or inactive wells; wells with one failed barrier (and the second degraded or intact) rose from 17 in 2022 to 33 in 2024, and operators must act on them [S3, opened].
- **During drilling,** when extended operations run inside a casing string and wear is suspected, before the next pressure test [S1].
- **Incident response** after a casing failure [S1].
- **Life extension and re-use** (slot recovery, return to production or injection): close to 200 of the surveyed wells are planned for that [S3].

## 9. Fit assessment and next reading

- **Can offer now:** the uniform-wall recalculation with ovality, axial load and temperature de-rating, and clear applicability flags; sensitivity to log uncertainty.
- **Needs licensed standards or proprietary data:** the technical report itself (formula constants and example properties); worn-pipe test data; vendor log interpretation.
- **Differentiator:** limited. Commercial casing-design and wear software already does this for operators, and service companies bundle it with the logs. An independent check for smaller operators or for plugging decisions is the plausible niche.
- **Verdict: weak-to-moderate fit; build only the baseline recalculation unless a client asks for more.**
- **A reviewer should open next:** the two model papers with physical test sets; the publisher scope pages; the regulator's full investigation report behind [S1].

## Sources

- [S1] United States offshore regulator, Safety Alert No. 226, "Pressure Rise to Surface after Cementing Leads to Burst Casing and Loss of Control", 28 January 2005. https://www.bsee.gov/sites/bsee.gov/files/safety-alerts/safety/safety-alert-no-226.pdf (opened)
- [S2] Silva, Lima Junior, Gouveia, "Elastoplastic numerical evaluation on the collapse pressure of a worn casing tube", Revista de Engenharia e Tecnologia 11(3), 2019. https://revistas.uepg.br/index.php/ret/article/download/13194/209209211470 (opened)
- [S3] Norwegian offshore safety regulator, "Knowledge acquisition for temporarily plugged and abandoned wells", 12 November 2024. https://www.havtil.no/en/explore-technical-subjects2/technical-competence/news/2024/knowledge-acquisition-for-temporarily-plugged-and-abandoned-wells/ (opened)

## Could not verify

- Every thesis and model paper named in section 4 (repositories unreachable or redirecting to an empty page).
- Scope wording of API TR 5C3, ISO/TR 10400 and API 7CW from their publishers.
- Any independent, published accuracy figure for casing inspection logs.
