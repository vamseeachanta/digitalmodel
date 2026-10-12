# Unbonded flexible pipe integrity FFS

Public-source research screen for issue #2200; not an implementation.

Single source: research note on the issue, sources checked 2026-10-11.

Research only; no code. All sources were checked on 2026-10-11. "Opened" means I read the page or the document text myself; "search snippet" means I only saw a search-engine summary and the claim is **unverified**. Standards are cited by designation; an edition is given only where the opened source states it. No normative text from a paid standard is reproduced.

## 1. Bottom line

- The **failure-mode taxonomy with event counts is public** and can be reproduced with citation: an industry joint project's refreshed data set of 556 categorised damage and failure events, presented at a regulator seminar [S2], plus a regulator-commissioned state-of-the-art report with six anonymised serious-incident narratives [S1]. That satisfies the "published failure-mode taxonomy reproduced with citations" and "validation against a published case history" acceptance lines.
- Two of the quantitative items in the issue have **no public basis**: an "allowable fraction" of broken armour wires, and a corrosion-fatigue life reduction factor. The regulator report says S-N data in representative annulus environments is mostly proprietary and that "No general prediction model for CO2 corrosion in flexible pipe annulus is available in public domain" [S1]. Both must be user inputs with an explicit "no public default" status, or the screen will imply precision it does not have.
- The realistic product is a **risk-ranking and evidence-to-verdict screen** (MONITOR / REPAIR / REPLACE / ESCALATE), not a remaining-life calculator.

## 2. Damage mechanisms and their published frequency

Counts from the refreshed industry data set (556 events; 424 damage without failure, 122 leaks, 10 ruptures; that is 76.3 %, 21.9 % and 1.8 %) [S2, slide 9, opened]:

| Grouped cause | Events | Share |
|---|---|---|
| Outer sheath damage / annulus flooded | 215 | 38.7 % |
| Ancillary equipment | 64 | 11.5 % |
| Carcass failure, multilayer pressure-sheath collapse | 36 | 6.5 % |
| Pressure sheath end-fitting pull-out | 33 | 5.9 % |
| Pressure sheath ageing | 31 | 5.6 % |
| Vent system anomalies / blockage | 28 | 5.0 % |
| Corrosion of armours | 27 | 4.9 % |
| Other | 24 | 4.3 % |
| Tensile armour birdcaging | 18 | 3.2 % |
| End-fitting leak / failure | 17 | 3.1 % |
| Overbend / pressure armour unlock | 15 | 2.7 % |
| Pressure sheath fatigue / fracture / microleaks | 13 | 2.3 % |
| Global pipe defect, wax blockage | 11 | 2.0 % |
| Smooth-bore liner collapse | 9 | 1.6 % |
| Carcass tearing / pull-out | 9 | 1.6 % |
| Tensile armour wire breakage | 6 | 1.1 % |

The source's slide text is a flattened table; I checked that these sixteen rows sum to 556. The per-row split between risers and flowlines, and between damage, leak and rupture, is in the slide but the text extraction is ambiguous for some rows, so it is not repeated here. The same source notes that sheath damage and annulus flooding events recorded no failures in themselves; ruptures sit in the armour-corrosion and armour-breakage rows (4 riser ruptures under corrosion of armours is legible).

Regulator-commissioned report, one national shelf [S1, opened]:
- "Updated Norwegian statistics for 2010-13 show at least 1.5% probability of failure per riser per operational year."
- "Historically, more than 25% of NCS risers have been replaced and only few risers have met their originally documented service life."
- The incident database held 85 reported incidents as of October 2013, 60 of them in the "Major" category.
- Earlier joint-project figures: about 600 events categorised from the 1970s onward against roughly 16,000 km of pipe manufactured [S2, slide 2, opened].

How the modes end, in the report's own words [S1, section 4.6]: "Failure modes caused by full rupture of armour wires carrying the tensile load may lead to full opening of the bore to the environment." Pressure-sheath cracks give leaks that are limited by intact carcass and pressure armour and may be caught by vent monitoring first. Carcass collapse or pull-out first shows as flow restriction.

## 3. Governing standards and recommended practices (designations only)

| Document | What it covers, per the opened source |
|---|---|
| API Spec 17J | Specification for unbonded flexible pipe [S1 section 2.4; S3] |
| API RP 17B | Recommended practice for flexible pipe [S1; S3]. The UK regulator's page lists it as recognised good practice and gives the edition it had in mind at the time (3rd edition, March 2002) [S3]. |
| ISO 13628-2 and ISO 13628-11 | The API documents "reformatted and issued as ISO documents" [S1 section 2.4] |
| API 17TR2 | Ageing of PA11 pressure sheaths; contains the service-life model and acceptance criteria in terms of corrected inherent viscosity. The report records field cases on both sides of the model and inter-laboratory disagreement [S1 sections 2.4, 5.2, 5.4]. |
| API 17TR1 | Evaluation standard for high-temperature pressure-sheath polymers [S1] |
| API Spec 17L1 / API RP 17L2 | Flexible pipe ancillary equipment [S1] |
| NORSOK M-506 | CO2 corrosion rate model; its pH module "does not cover confined environments" such as a flooded annulus [S1 section 5.4] |
| NACE MR0175 / ISO 15156 | Sour-service qualification of armour wire [S1] |
| DNV-RP-O501 | Erosion guidance used for carcass erosion [S1] |
| Industry joint-project guidance on flexible pipe integrity management, and an industry-institute guidance on life extension of unbonded flexible pipe systems | Risk-based integrity management good practice and a life-extension assessment flow. The flow is shown in [S2, slide 5]: known current integrity, future threats, risk acceptance, repair or further assessment, re-assessment interval, with sacrificial dissection as a confidence step. The full guidance documents returned access errors and are **unverified** beyond that slide. |

## 4. Assessment methods and levels in use

1. **Screening / risk rating:** "Perform risk rating of the individual riser to enable selection of appropriate IM activities" [S1, executive summary]. The life-extension flow is "semi-qualitative risk-based" and asks how the assessed pipe differs from pipes with recorded failures [S2, slides 7-8].
2. **Analytical:** polymer ageing by the API 17TR2 model with coupon data; annulus corrosion by general CO2 models with the caveat above; erosion by rigid-pipe models [S1 section 5.4].
3. **Numerical:** global time-domain riser analysis feeding a local cross-section model to get wire stress histories for rainflow fatigue; non-linear finite element models for damage cases such as corroded or broken wires and carcass pull-out [S1 section 5.4.2]. An open conference paper builds closed-form stress-concentration expressions for pipes with broken tensile wires from 47 finite element cases [S4, abstract opened; values not in the abstract].

Stated limits of the fatigue method [S1 section 5.4.1 and 4.4]: most S-N results in representative environments "are proprietary to the individual companies or to closed groups of companies"; tests are too short to develop corrosion roughness; "corrosion pits/grooves as small as a fraction of 1mm leads to significant reduction (as much as one order of magnitude) of the fatigue life".

## 5. Input data and where it comes from

| Evidence | Method | Published capability or limit [S1 section 5.2 unless noted] |
|---|---|---|
| Outer sheath breach, location | General visual inspection by diver or vehicle; annulus vacuum or pressure test | Annulus testing gives "Reliable detection of breaches in the external sheath in the upper part of the riser down to well below the sea level", free-volume estimate and vent impedance. Results from different providers on the same riser have been inconsistent. The joint-project presentation questions whether initiating defects are really identifiable by general visual inspection [S2, slide 13]. |
| Annulus condition | Vent gas sampling; continuous vent-rate monitoring | Detects incipient pressure-sheath leaks and abrupt vent blockage; needs riser-specific calibration and control-room criteria. |
| Armour wire corrosion | Eddy current, radiography | "no available methods have either demonstrated or proven capabilities for detection and quantification of corrosion damage on tensile armour wires". |
| Armour wire breaks | Strain gauges on wires, magnetic stress measurement, acoustic emission, embedded optical fibres | Available and field-tested; require qualification per pipe configuration. |
| Carcass condition | Internal camera and laser scanning in shutdown | Carcass pitch measured "with a resolution of 0.1mm". |
| Pressure-sheath ageing | Flow-exposed coupons, temperature history | Coupons usually sit at the cold end, so a temperature correction is needed. |
| Operating history | Pressure, temperature, depressurisation rate, vessel motion records | Needed to show operation inside the design envelope. |

About forty inspection and monitoring methods were peer reviewed and categorised by the joint project [S2, slide 3].

## 6. Acceptance criteria in outline

- Polymer: API 17TR2 acceptance in terms of corrected inherent viscosity [S1].
- Fatigue: design practice applies a safety factor of 10 on armour fatigue life, compared with about 2 on polymer ageing life (the report's Figure 4-13 caption) [S1 section 4.6].
- Armour damage: no generic allowable. The report's worked logic for a breach found above the waterline is to "determine the maximum corrosion damage that can be allowed and still maintain adequate margins against rupture and fatigue", then set inspection detection and sizing requirements from that, then qualify the inspection method; and separately to "determine by calculation/modeling the consequence of one or several wire ruptures" to set response procedures [S1 section 5.5]. This is a case-specific analysis, not a look-up.
- Annulus corrosion rate in a confined, oxygen-free flooded annulus: laboratory rates "well below 0.01mm/year" and an industry consensus that it is negligible; no models for oxygen ingress, recurrent condensation, or breaches in the splash zone [S1 section 5.4.1].

## 7. Published validation and case-history material

- **Six anonymised serious-incident narratives** [S1 section 3.4]: for example, a blocked or unconnected annulus vent, an undiscovered outer-sheath breach at the waterline in a guide tube, heavy wire corrosion, wire failure and parting of the riser with gas release (three variants); repeated trawl impact on a flowline with sheath breach, clamp repair, and brittle failure of plastically deformed high-strength wires; carcass pull-out at the topside end fitting from polymer shrinkage in high-temperature service. Each maps cleanly to an evidence-to-verdict path and can serve as the validation record: the screen should return REPLACE or ESCALATE on each narrative's input pattern. The report concludes that all but one "could have been avoided with better insight in the possible failure mechanisms and careful inspection, monitoring and testing".
- **Event-frequency table** [S2] for the taxonomy and for prior likelihood ranking.
- **Round robins or public benchmark calculations:** none found. The report records that reputable laboratories could not reproduce each other's polymer-ageing results [S1 section 5.4.1].

## 8. Who buys the work, and when

- **Regulatory duty:** in the UK, operators must keep pipelines "maintained in an efficient state, in efficient working order and in good repair", and the safety-case verification scheme must cover all hydrocarbon risers: "There should be independent and competent assurance on ongoing suitability of flexible hydrocarbon risers." The regulator's inspectors had found "a lack of knowledge and use of recognised good practice" after failures between 2002 and 2007 [S3, opened].
- **Regulator pressure elsewhere:** the Norwegian regulator is quoted as saying flexible risers are "an area where the industry still has a clear and pressing potential for improvement" and asking for integrity management with continuous monitoring and documented operating history [S1 section 1.1].
- **Life extension:** only a few risers reached their documented life [S1]; an industry-institute life-extension guidance exists for exactly this decision [S2].
- **Inspection finding or incident response:** a breach "could have occurred as much as 2 years before the time of detection" and the question is whether the riser can stay in service after repair [S1 section 5.5].
- **Continuing events:** since the joint project's 2017 publication, further corrosion failures, replacements driven by fatigue findings, ancillary failures and end-fitting leaks [S2, slide 4].

## 9. Fit assessment for a small analysis-led consultancy

- **Can offer now with open methods:** the failure-mode-to-evidence-to-verdict screen; risk ranking of a riser population against the published event frequencies; global riser dynamics and fatigue-loading reassessment from measured vessel motion (existing riser analysis capability); an independent review of an operator's integrity strategy against the public good-practice items (vent function, breach location, operating envelope).
- **Needs licensed standards or proprietary data:** polymer-ageing acceptance values (licensed technical report); armour-wire S-N curves in annulus environments (proprietary); local cross-section stress models with manufacturer geometry (data sheets owned by the manufacturer and operator); any quantitative remaining-capacity claim after wire breaks.
- **Differentiator:** independence from manufacturers and inspection vendors, and a transparent screen that states what is not knowable from public models. The regulator-commissioned report asks operators to "Ensure access to relevant flexible riser expertise"; the market is small and expert-led, with a few specialist firms already established.
- **Verdict: moderate fit as a screening and independent-review offering; poor fit for quantitative remaining-life work without manufacturer data.**

## 10. Consequences for this issue's objective

- "Armour-wire remaining capacity from wire-break count against a documented allowable fraction": no documented public allowable exists. Implement as proportional loss of intact-wire area with a mandatory user-supplied stress-concentration or load-sharing factor, flagged ESCALATE by default, and cite [S1 section 5.5] and [S4] for why.
- "Corrosion-fatigue life reduction factor": implement as a user input; the only public statement is the order-of-magnitude warning quoted above, which is not a design value.
- Add "annulus vent blocked or not connected" as a first-class input: it is 5 % of events on its own and is the initiating condition in three of the six serious-incident narratives.

## Sources

- [S1] "Un-bonded Flexible Risers - Recent Field Experience and Actions for Increased Robustness", report for the Norwegian offshore safety regulator, document 0389-26583-U-0032, revision 5, dated 31.12.2013, classification "Open". https://www.havtil.no/contentassets/c2a5bd00e8214411ad5c4966009d6ade/un-bonded-flexible-risers--recent-field-experience-and-actions--for-increased-robustness.pdf (text opened)
- [S2] "Flexible Pipe Integrity; post-Sureflex & introducing the draft EI life extension guidance", presentation hosted by the Norwegian regulator (undated in the file). https://www.havtil.no/contentassets/8357c28fc9b543419c58ec2b0eec8307/11---flexible-pipe-integrity-post-sureflex-introducing-the-draft-ei-life-extension-guidance---wood.pdf (text opened)
- [S3] "Unbonded flexible pipe inspection issues and recommendations", UK regulator, issued 19 February 2008, page last updated 26 March 2025. https://www.hse.gov.uk/pipelines/resources/flexiblepipe.htm (opened)
- [S4] dos Santos and de Sousa, "Assessment of flexible pipes with damaged tensile armor wires using finite element models and symbolic regression", conference paper record. https://publicacoes.softaliza.com.br/cilamce2020/article/view/6610 (abstract opened)
- [S5] "Lessons From the Sureflex JIP: Latest Industry Data and Flexible Pipe Integrity Management", technical-society magazine article, 16 December 2024. https://jpt.spe.org/lessons-from-the-sureflex-jip-latest-industry-data-and-flexible-pipe-integrity-management (opened; states that the full joint-project report, revision 05 dated December 2023, is an open industry publication with 14 conclusions, but lists none of them)

## Could not verify

- The open joint-project report itself (revision 05, December 2023): its host page returned access denied from this host. It is the first document a reviewer should open; its statistics may supersede the 556-event table.
- The industry-institute life-extension guidance and its mechanism-by-mechanism tables (access denied).
- Current editions and scope wording of API RP 17B and API Spec 17J from the publisher (not opened; the report [S1] said new editions were in final review in late 2013).
- Any published measurement uncertainty for annulus free-volume tests (the report only says providers disagree).
