# FFS Offering Catalog — Industry Lookup Tables (2026-09-25)

**Epic:** #1057 Phase 4 · **Data:** `src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml` (single source; this page mirrors it)
**Companion notes:** `ffs-readiness-review-2026-09-25.md`, `level3-and-part9-program-2026-09-25.md`, plan `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md`

## How to read these tables

- **Tier**: T1 field screen (Level 1 or a code screening rule, inspector-facing verdict); T2 office Level 2 (closed-form, engineer-facing report); T3 Level 3 (numerical: FEA, fracture mechanics, collapse).
- **Status**: `live` (engine, tests, validation record, registered workflow, Deckhand route) · `validated` (engine + tests + validation record, no workflow) · `engine` (engine + tests only) · `planned` (issue filed) · `none` (roadmap candidate, no issue yet).
- Codes are cited as publisher identifiers only; clause text lives in llm-wiki, never here.
- API 579-1 parts: 3 brittle fracture · 4 general metal loss · 5 local metal loss · 6 pitting · 7 hydrogen damage · 8 shell distortion · 9 crack-like flaws · 10 creep · 11 fire damage · 12 dents and gouges · 13 laminations · 14 fatigue (2016+). API RP 571 supplies the damage-mechanism taxonomy (60+ mechanisms) that routes a finding to a part.

## 1. Pipelines (onshore and offshore transmission, gathering, hazardous liquid)

Inspection and integrity frameworks: ASME B31.8S (22 root causes in 9 threat categories), API RP 1160, API RP 1176 (cracking). Defect taxonomy cross-checked against the Pipeline Defect Assessment Manual (PDAM): defect-free pipe, corrosion, gouges, plain and kinked dents, dents on welds, dent-gouge, manufacturing defects, girth and seam weld defects, cracking, environmental cracking, defect interaction, fittings, leak-vs-rupture.

| Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|
| General / local metal loss (external, internal corrosion) | ASME B31G, DNV-RP-F101, API 579 Pt 4/5 | corroded_pipe, rstreng_2d, dnv_rp_f101, ffs coordinator | T1, T2 | validated (#2181 wires the workflow) |
| River-bottom profile metal loss | RSTRENG, DNV-RP-F101 | rstreng_2d | T2 | validated |
| Circumferential metal loss, net section | API 579 Pt 5, DNV-RP-F101 combined loading | circumferential_defect | T2 | validated (F101 factor stubbed, #1146) |
| Interacting defect colonies | DNV-RP-F101, API 579 Pt 4 | dnv_rp_f101 | T2 | engine (#1094 finding 1) |
| Pitting | API 579 Pt 6 | pitting | T1, T2 | engine (#2182) |
| Plain dents, dents on welds | API 579 Pt 12, PDAM, B31.8 App R | dent_assessment | T1, T2 | engine (#2182) |
| Dent-gouge, gouges | API 579 Pt 12, PDAM | dent_assessment | T1, T2 | engine; gouge-only path missing |
| Crack-like flaws (SCC, seam, girth, fatigue, hook cracks) | API 579 Pt 9, BS 7910, API RP 1176 | crack_fad → #2175, #2176 | T1, T2, T3 | engine |
| Crack growth, remaining life, leak-before-break | BS 7910 Cl 8, API 579 Pt 9 | fatigue/crack_growth → #2177 | T2 | engine |
| Laminations, inclusions | API 579 Pt 13 | — | T1, T2 | none |
| Wrinkles, buckles, ovality, ripples | API 579 Pt 8 | — | T2, T3 | none |
| Manufacturing defects in pipe body | PDAM | — | T2 | none |
| Composite repair behind a REPAIR verdict | ASME PCC-2, ISO 24817 | composite_repair | T2 | engine (#2185) |
| Re-inspection interval, remaining life | B31.8S, API RP 1160 | inspection_planning | T1 | validated (#2180 routes it) |
| Subsea: free spans (VIV fatigue) | DNV-RP-F105 | fatigue atlas | T2 | live |
| Subsea: upheaval / lateral buckling | DNV-RP-F110 | base configs | T2 | engine, not an FFS verdict |
| Subsea: on-bottom stability | DNV-RP-F109 | on_bottom_stability | T1 | live |

## 2. Refining and petrochemical fixed equipment

Inspection codes: API 510 (vessels), API 570 (piping), API 653 (tanks); risk: API RP 580/581; damage taxonomy: API RP 571.

| Asset | Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|---|
| Pressure vessel | General / local metal loss, re-rate | API 579 Pt 4/5, API 510 | ffs coordinator (ASME VIII t_min) | T1, T2 | validated |
| Pressure vessel | Pitting | Pt 6 | pitting | T1, T2 | engine |
| Pressure vessel | Hydrogen blisters, HIC, SOHIC | Pt 7 | — | T1, T2 | planned (#1274) |
| Pressure vessel | Brittle fracture susceptibility (MAT) | Pt 3 | — | T1, T2 | planned (#1274) |
| Pressure vessel | Weld misalignment, out-of-roundness, bulges | Pt 8 | — | T2, T3 | none |
| Pressure vessel | Crack-like flaws | Pt 9, BS 7910 | crack_fad → #2175 | T2, T3 | engine |
| Pressure vessel | Creep, creep-fatigue | Pt 10 | — | T2, T3 | planned (#1274) |
| Pressure vessel | Fire damage | Pt 11 | — | T1, T2 | none |
| Pressure vessel | Fatigue screen and initiation | Pt 14 | structural/fatigue S-N library | T1, T2 | engine, Part 14 not wired |
| Process piping | CUI, injection points, deadlegs, soil-to-air, mixing points, erosion thinning | API 570, Pt 4/5 | ffs coordinator | T1, T2 | validated |
| Process piping | Pitting | Pt 6 | pitting | T1, T2 | engine |
| Process piping | Cracks at branch / weldolet welds | Pt 9, BS 7910 | crack_fad → #2175; FE benchmark #2157 | T2, T3 | engine |
| Process piping | Composite repair | PCC-2, ISO 24817 | composite_repair | T2 | engine |
| Storage tank | Bottom corrosion (critical zone, underside, topside) | API 653, Pt 4/5/6 | ffs coordinator, pitting | T1, T2 | engine, no tank-specific t_min |
| Storage tank | Shell course thinning, fill-height re-rate | API 653, Pt 4/5 | ffs coordinator | T1, T2 | engine, API 650 method not wired |
| Storage tank | Settlement: planar tilt, out-of-plane, edge, bottom | API 653 Annex B | — | T1 | planned (#2172) |
| Storage tank | Settlement beyond Annex B: strain limit, buckling, collapse | API 579 Annex B1 / 2D | — | T3 | planned (#2174, #2173, #2171) |
| Storage tank | Shell distortion (buckling, flat spots, peaking) | Pt 8, API 653 | — | T2, T3 | none |
| Storage tank | Floating roof, seals, appurtenances | API 653 | — | T1 | none |
| Any | Damage-mechanism identification and routing | API RP 571 | — | T1 | none (crosswalk table is a catalog deliverable) |
| Any | Risk ranking, inspection interval | API RP 580/581, API 510/570/653 | rbi_screening, inspection_planning | T1 | engine / validated |

## 3. Fixed offshore platforms

Frameworks: API RP 2SIM, ISO 19902, NORSOK N-006. Typical findings: member dents and bows, corrosion wastage, joint cracks, missing or flooded members, scour, subsidence, marine growth.

| Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|
| Member dents, bows, holes (impact, dropped objects) | API RP 2SIM, ISO 19902 | jacket_topside (design checks only) | T2, T3 | none |
| Corrosion wastage (splash zone, atmospheric) | API RP 2SIM, API RP 2A | jacket_topside, thickness-reduced re-check | T1, T2 | engine |
| Fatigue cracks at tubular joints | API RP 2SIM, BS 7910, DNV-RP-C203 | crack_fad, S-N library | T2, T3 | engine |
| Missing or severed members, flooded members, pushover / RSR | API RP 2SIM, ISO 19902 | — | T3 | none (needs #2173) |
| Scour, subsidence, marine growth beyond design | API RP 2SIM | geotechnical/scour | T1 | none as FFS |
| Structural health monitoring alerts | API RP 2SIM, DNV-ST-0126 | offshore_resilience/structural_health | T1 | engine |

## 4. Floating production systems, moorings and risers

Frameworks: API RP 2MIM, API RP 2FSIM, DNV-OS-E301. Mooring chain pitting classes per industry practice: small (<5 mm dia, <2 mm deep), medium (5–20 mm, 2–10 mm), mega (>20 mm, >10 mm).

| Asset | Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|---|
| Mooring chain | General corrosion (diameter loss) | API RP 2MIM, DNV-OS-E301 | mooring_resilience screening factors | T1, T2 | none |
| Mooring chain | Pitting classes, discard criteria | API RP 2MIM, Chain FEARS JIP | — | T1, T2 | none |
| Mooring chain | Interlink / fairlead wear | API RP 2MIM, DNV-OS-E301 | — | T1 | none |
| Mooring chain | Fatigue (T-T, OPB) | API RP 2MIM, DNV-OS-E301, DNV-RP-C203 | mooring_fatigue, S-N library | T2 | engine |
| Mooring chain | Link ovalization, loose studs, cracks | API RP 2MIM | — | T1 | none |
| Steel risers (SCR, TTR) | Metal loss, pitting, cracks | API RP 2RD, DNV-ST-F201, API 579, BS 7910 | ffs coordinator, crack_fad | T1, T2 | engine |
| Steel risers | Fatigue (wave, VIV) | DNV-ST-F201, DNV-RP-C203 | riser_fatigue | T2 | live |
| Drilling riser joints | Metal loss, weld flaws, collapse-limited depth | B31G, DNV-RP-F101, BS 7910 | riser_joint_ffs on real C-scans | T1, T2 | engine (#2183) |
| Flexible pipe | Outer sheath damage, annulus flooding | API RP 17B | — | T1 | none |
| Flexible pipe | Tensile armour corrosion / rupture, end-fitting fatigue | API RP 17B | — | T2 | none |
| Flexible pipe | Carcass collapse, pressure sheath creep / rupture | API RP 17B | — | T2 | none |

## 5. Wells

| Asset | Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|---|
| Casing and tubing | Wear and corrosion; remaining collapse / burst with ovality | API TR 5C3 / ISO 10400 | — | T2 | none |

## 6. Offshore wind support structures

| Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|
| Circumferential weld fatigue cracks | DNV-ST-0126, DNV-RP-C203, BS 7910 | S-N library, crack_fad | T2, T3 | engine |
| Corrosion pit to crack transition | DNV-RP-C203, BS 7910 | pitting, crack_growth | T2 | engine |
| Grouted connection slippage / cracking | DNV-ST-0126 | — | T2 | none |
| Bolted flange preload loss | DNV-ST-0126 | — | T1 | none |
| Scour | DNV-ST-0126 | geotechnical/scour | T1 | engine |

## 7. Ships and floating hulls (class rules)

Frameworks: IACS CSR Ch 13 (renewal criteria: wastage allowance, substantial corrosion above 75 % of margin, hull girder section modulus not below 90 % deck/bottom or 85 % neutral-axis zone), IACS Rec. 84.

| Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|
| Thickness diminution vs renewal thickness | IACS CSR Ch 13 | — | T1 | none |
| Buckling of thinned plates / panels | IACS CSR, DNV-RP-C201 | plate / panel buckling | T2 | live |
| Pitting intensity on plating | IACS Rec. 84 | pitting | T1 | engine |
| Hull girder section modulus loss | IACS CSR Ch 13 | hull_girder_screening | T2 | engine |
| Fatigue cracks at details | IACS CSR, BS 7910 | S-N library, crack_fad | T2 | engine |

## 8. Power boilers and high-temperature pressure parts

| Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|
| Tube / header wall thinning | API 579 Pt 4/5, NBIC | ffs coordinator | T1, T2 | validated |
| Creep damage, remaining life (Larson-Miller, Omega) | Pt 10 | — | T2, T3 | planned (#1274) |
| Thermal fatigue, creep-fatigue | Pt 14, Pt 10 | — | T2 | none |
| Crack-like flaws in headers, nozzles | Pt 9, BS 7910 | crack_fad | T2, T3 | engine |

## Coverage summary

| Status | Rows |
|---|---|
| live | 4 |
| validated | 8 |
| engine | 30 |
| planned | 6 |
| none | 23 |

The `none` rows are the offering roadmap beyond Phase 4: fixed-platform damaged members, mooring chain degradation, flexible pipe, casing wear, ship wastage, API 579 Parts 8/11/13/14, and the API RP 571 crosswalk. Each is filed as an issue from the program plan.

## Sources (public overviews consulted 2026-09-25)

- API 579-1/ASME FFS-1 2021 parts and Part 14 fatigue: [ANSI blog](https://blog.ansi.org/ansi/fitness-for-service-api-579-asme-ffs-1-2021/), [CADE overview](https://cadeengineering.com/asme-api-579-1-asme-ffs-1-new-edition-2021/), [ASME PVP 2016 Part 14 summary](https://asmedigitalcollection.asme.org/PVP/proceedings-abstract/PVP2016/50350/V01AT01A002/285035)
- API RP 571 scope: [Inspectioneering](https://inspectioneering.com/tag/api+rp+571), [API RP 571 3rd edition release](https://inspectioneering.com/news/2020-04-01/9137/api-publishes-new-edition-of-rp-571---damage-mechanisms-affecting-fixed-equipmen)
- PDAM defect list: [Penspen PDAM overview](https://penspen.com/wp-content/uploads/2014/09/pdam-overview.pdf), [Penspen PDAM](https://www.penspen.com/our-services/digital-solutions-and-tools/pdam/)
- ASME B31.8S threats: [ASME B31.8S](https://www.asme.org/codes-standards/find-codes-standards/b31-8s-managing-system-integrity-gas-pipelines), [49 CFR 192.917](https://www.ecfr.gov/current/title-49/subtitle-B/chapter-I/subchapter-D/part-192/subpart-O/section-192.917)
- API 570 damage locations: [API 570 overview](https://ifluids.com/standard/api-570-piping-inspection-code/), [InServe API 570 guide](https://inservemechanical.com/services/asset-management/api-570-process-piping-inspection/)
- API 653 damage types and settlement: [API 653 5th ed public announcement](https://www.api.org/~/media/files/publications/whats%20new/653_e5%20pa.pdf), [settlement calculation guide](https://armorshieldlining.com/feeds/blog/api-653-settlement-calculation), [CommTank](https://www.commtank.com/tank-articles/what-happens-during-an-api-653-tank-inspection/)
- API RP 2SIM scope: [OTC-20675](https://onepetro.org/OTCONF/proceedings-abstract/10OTC/10OTC/OTC-20675-MS/36386), [GlobalSpec](https://standards.globalspec.com/std/14364931/API%20RP%202SIM), [BSEE 2SIM briefing](https://www.bsee.gov/sites/bsee.gov/files/bsee-interim-document/reports/7-structures-hugh-westlake.pdf)
- API RP 2MIM and chain pitting classes: [BSEE TAP 730](https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program/730-aa.pdf), [GlobalSpec](https://standards.globalspec.com/std/13448200/api-rp-2mim)
- Flexible pipe failure modes: [API RP 17B public announcement](https://www.api.org/~/media/files/publications/whats%20new/17b%20e5%20pa.pdf), [flexible riser integrity review](https://www.osti.gov/etdeweb/servlets/purl/21157881)
- Offshore wind substructure failure modes: [ORE Catapult substructure report](https://cms.ore.catapult.org.uk/wp-content/uploads/2018/01/Offshore-wind-farm-substructure-monitoring-and-inspection-report-.pdf)
- Ship renewal criteria: [IACS CSR Ch 13 (ClassNK copy)](https://www.classnk.com/hp/pdf/rules/amendments/e-Amendments/06.03.20/CSR-B/17_Chapter13.pdf), [IACS Rec. 84](https://iacs.s3.af-south-1.amazonaws.com/wp-content/uploads/2022/05/20152627/rec84rev1.pdf)
- Boiler creep FFS: [Springer life assessment chapter](https://link.springer.com/chapter/10.1007/978-981-92-0998-9_21), [ASME Power Boilers guide ch. 14](https://asmedigitalcollection.asme.org/ebooks/book/chapter-pdf/2797412/859674_ch14.pdf)
- Casing wear / collapse: [API 5C3 vs ISO 10400 reliability](https://www.researchgate.net/publication/327701418_Reliability_Analysis_of_Vertical_Well_Casing_Comparison_of_API_5C3_and_ISO_10400), [casing defects with wear and corrosion](https://www.researchgate.net/publication/292187078_Evaluation_of_Casing_Integrity_Defects_Considering_Wear_and_Corrosion_-_Application_to_Casing_Design)
