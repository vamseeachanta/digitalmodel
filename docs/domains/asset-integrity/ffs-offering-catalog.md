<!-- GENERATED FILE, do not edit. Source: src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml and ffs_design_screen_catalog.yml. Regenerate: python -m digitalmodel.asset_integrity.offering_catalog render -->
# FFS Offering Catalog — Industry Lookup Tables (data 2026-09-25)

**Epic:** #1057 Phase 4 · **Data:** `src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml` (FFS verdicts) and `ffs_design_screen_catalog.yml` (design screens, owner decision D5); this page is rendered from both by `python -m digitalmodel.asset_integrity.offering_catalog render` and `tests/asset_integrity/test_offering_catalog_render.py` fails on drift
**Companion notes:** `ffs-readiness-review-2026-09-25.md` and `level3-and-part9-program-2026-09-25.md` (PR #2186), plan `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md`

## How to read these tables

- **Tier**: T1 field screen (Level 1 / code screening rule); T2 office Level 2 (closed-form); T3 Level 3 (numerical).
- **Status**: `live` (engine, tests, validation record, registered durable workflow with example) · `workflow` (registered durable workflow with example, no validation record) · `validated` (engine + tests + validation record, no registered workflow) · `engine` (engine + tests only) · `planned` (issue filed) · `none` (roadmap candidate, no issue yet). A row is never stronger than the strongest engine it depends on; `none` rows list no engines.
- Codes are cited as publisher identifiers only. Clause text, tables, figures and licensed numeric thresholds stay out of this repo; thresholds are user inputs whose public defaults are recorded on the implementing issue.
- API 579-1 parts: 3 brittle fracture · 4 general metal loss · 5 local metal loss · 6 pitting · 7 hydrogen blisters / HIC / SOHIC · 8 weld misalignment and shell distortion · 9 crack-like flaws · 10 creep · 11 fire damage · 12 dents and gouges · 13 laminations · 14 fatigue (2016+). API RP 571 supplies the damage-mechanism taxonomy that routes a finding to a part (`ffs_damage_mechanism_crosswalk.yml`).

## 1. Pipelines (onshore and offshore transmission, gathering, hazardous liquid)

Frameworks: ASME B31.8S (22 root causes in 9 threat categories), API RP 1160, API RP 1176 (cracking), NACE SP0502 / SP0206 (direct assessment). Subsea design screens (free span, on-bottom stability, global buckling) are in the design-screen section, not here. Defect taxonomy cross-checked against PDAM: defect-free pipe, corrosion, gouges, plain and kinked dents, dents on welds, dent-gouge, manufacturing defects, girth and seam weld defects, cracking, environmental cracking, defect interaction, fittings, leak-vs-rupture.

| Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|
| General / local metal loss (external, internal corrosion) | ASME B31G, DNV-RP-F101, API 579 Pt 4/5 | corroded-pipe, rstreng-2d, dnv-f101, ffs-metal-loss | T1, T2 | validated |
| River-bottom profile metal loss | ASME B31G, DNV-RP-F101 | rstreng-2d | T2 | validated |
| Circumferential metal loss / net-section | API 579 Pt 5, DNV-RP-F101 | circumferential | T2 | validated (F101 combined-loading factor stubbed (#1146)) |
| Interacting defect colonies | DNV-RP-F101, API 579 Pt 4 | dnv-f101 | T2 | engine (#1094 finding 1) |
| Pitting | API 579 Pt 6 | pitting | T1, T2 | engine |
| Plain dents / dent on weld | API 579 Pt 12, PDAM, ASME B31.8 App R | dents | T1, T2 | engine |
| Dent-gouge / gouges | API 579 Pt 12, PDAM | dents | T1, T2 | engine (gouge-only PDAM path not implemented) |
| Crack-like flaws (SCC, seam weld, girth weld, fatigue cracks, hook cracks) | API 579 Pt 9, BS 7910, API RP 1176 | crack-fad, part9-level2 (#2175), part9-level1 (#2176) | T1, T2, T3 | engine |
| Fatigue crack growth / remaining life / leak-before-break | BS 7910, API 579 Pt 9 | crack-growth | T2 | engine (#2177) |
| Laminations / inclusions | API 579 Pt 13 | parts-8-11-13-14 (#2203) | T1, T2 | planned |
| Wrinkles, buckles, ovality, ripples | API 579 Pt 8 | parts-8-11-13-14 (#2203) | T2, T3 | planned |
| Manufacturing defects in pipe body | PDAM | — | T2 | none |
| Geohazard / ground-movement strain demand | API RP 1133 | — | T2 | none |
| Direct-assessment programme support (ECDA / ICDA region and indication ranking) | NACE SP0502, NACE SP0206 | — | T1 | none |
| Composite repair selection behind a REPAIR verdict | ASME PCC-2, ISO 24817 | composite-repair | T2 | engine |
| Re-inspection interval / remaining life | ASME B31.8S, API RP 1160 | inspection-planning | T1 | workflow |

## 2. Refining and petrochemical fixed equipment

Inspection codes: API 510 (vessels), API 570 (piping), API 653 (tanks), API 660 / TEMA (exchangers), API 560 / RP 573 (fired heaters); risk: API RP 580/581; damage taxonomy: API RP 571 (crosswalk data in ffs_damage_mechanism_crosswalk.yml).

| Asset | Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|---|
| Pressure vessel (API 510 -> API 579) | General / local metal loss | API 579 Pt 4/5, API 510 | ffs-metal-loss | T1, T2 | validated (L1 t_min includes ASME VIII) |
| Pressure vessel (API 510 -> API 579) | Pitting | API 579 Pt 6 | pitting | T1, T2 | engine |
| Pressure vessel (API 510 -> API 579) | Hydrogen blisters / HIC / SOHIC | API 579 Pt 7 | parts-3-7-10 (#1274) | T1, T2 | planned |
| Pressure vessel (API 510 -> API 579) | Brittle fracture susceptibility (MAT) | API 579 Pt 3 | parts-3-7-10 (#1274) | T1, T2 | planned |
| Pressure vessel (API 510 -> API 579) | Weld misalignment, out-of-roundness, bulges | API 579 Pt 8 | parts-8-11-13-14 (#2203) | T2, T3 | planned |
| Pressure vessel (API 510 -> API 579) | Crack-like flaws (weld cracks, SCC, fatigue) | API 579 Pt 9, BS 7910 | crack-fad, part9-level2 (#2175) | T2, T3 | engine |
| Pressure vessel (API 510 -> API 579) | Creep / creep-fatigue | API 579 Pt 10 | parts-3-7-10 (#1274) | T2, T3 | planned |
| Pressure vessel (API 510 -> API 579) | Fire damage | API 579 Pt 11 | parts-8-11-13-14 (#2203) | T1, T2 | planned |
| Pressure vessel (API 510 -> API 579) | Fatigue (crack initiation, cyclic-service screen) | API 579 Pt 14 | parts-8-11-13-14 (#2203), sn-fatigue | T1, T2 | planned (S-N library exists; Part 14 screening not wired) |
| Pressure vessel (API 510 -> API 579) | Re-rate / MAWP reduction | API 579 Pt 4/5, API 510 | ffs-metal-loss | T2 | validated |
| Process piping (API 570 -> API 579) | CUI, injection point, deadleg, soil-to-air, mixing point, erosion thinning | API 570, API 579 Pt 4/5 | ffs-metal-loss | T1, T2 | validated |
| Process piping (API 570 -> API 579) | Pitting | API 579 Pt 6 | pitting | T1, T2 | engine |
| Process piping (API 570 -> API 579) | Crack-like flaws at branch / weldolet welds | API 579 Pt 9, BS 7910 | crack-fad, part9-level2 (#2175) | T2, T3 | engine (weldolet FE benchmark in plan #2157 / PR #2194) |
| Process piping (API 570 -> API 579) | Composite repair | ASME PCC-2, ISO 24817 | composite-repair | T2 | engine |
| Heat exchangers and fired heaters | Exchanger tube bundle thinning / pitting / tube plugging limits | API 660, TEMA, API 579 Pt 4/5/6 | — | T1, T2 | none |
| Heat exchangers and fired heaters | Fired-heater tube creep, bulging, carburization | API 560, API RP 573, API 579 Pt 10/8 | — | T2, T3 | none |
| Atmospheric storage tank (API 653 -> API 579) | Bottom plate underside / topside corrosion, critical zone | API 653, API 579 Pt 4/5/6 | ffs-metal-loss, pitting | T1, T2 | engine (no tank-specific t_min rules) |
| Atmospheric storage tank (API 653 -> API 579) | Shell course thinning / fill-height re-rate | API 653, API 579 Pt 4/5 | ffs-metal-loss | T1, T2 | engine (API 650 one-foot method not wired) |
| Atmospheric storage tank (API 653 -> API 579) | Settlement (planar tilt, out-of-plane differential, edge, bottom) | API 653 | tank-settlement-screen (#2172) | T1 | planned (#2172) |
| Atmospheric storage tank (API 653 -> API 579) | Settlement beyond Annex B limits (strain limit, buckling, collapse) | API 579 | tank-level3 (#2174), calculix-nonlinear (#2173), annex-f-material (#2171) | T3 | planned (#2174; Annex B1 (2007) / Annex 2D (2016+)) |
| Atmospheric storage tank (API 653 -> API 579) | Shell distortion (buckling, flat spots, peaking, out-of-roundness) | API 579 Pt 8, API 653 | parts-8-11-13-14 (#2203) | T2, T3 | planned |
| Atmospheric storage tank (API 653 -> API 579) | Floating roof / seal / appurtenance damage | API 653 | — | T1 | none |
| Any fixed equipment | Damage-mechanism identification and routing to an API 579 part | API RP 571 | offering-catalog (#2197) | T1 | planned (571 -> API 579 part crosswalk data is #2197) |
| Any fixed equipment | Risk ranking / inspection interval | API RP 580, API RP 581, API 510, API 570, API 653 | rbi, inspection-planning | T1 | engine |

## 3. Fixed offshore platforms (jackets, topsides, conductors, caissons)

Frameworks: API RP 2SIM, ISO 19902, NORSOK N-006. Typical findings: member dents and bows, corrosion wastage, joint cracks, missing or flooded members, scour, subsidence, marine growth, conductor and caisson wall loss.

| Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|
| Member dents / bows / holes (boat impact, dropped objects) | API RP 2SIM, ISO 19902 | platform-damaged-member (#2198), jacket-member-joint | T2, T3 | planned (design checks exist; damaged-member capacity does not) |
| Corrosion wastage (splash zone, atmospheric) | API RP 2SIM, API RP 2A | jacket-member-joint | T1, T2 | engine (thickness-reduced re-check possible) |
| Fatigue cracks at tubular joints | API RP 2SIM, BS 7910, DNV-RP-C203 | crack-fad, sn-fatigue | T2, T3 | engine |
| Missing / severed / flooded members (pushover, RSR) | API RP 2SIM, ISO 19902 | platform-damaged-member (#2198), calculix-nonlinear (#2173) | T3 | planned |
| Scour, subsidence, marine growth beyond design | API RP 2SIM | scour | T1 | engine (scour capacity only; no SIM verdict) |
| Structural health monitoring alerts | API RP 2SIM, DNV-ST-0126 | structural-health | T1 | engine |
| Conductors, caissons, boat landings, risers guards: conductor / caisson wall loss, guide wear, fatigue | API RP 2SIM, API RP 17G, DNV-RP-C203 | — | T1, T2 | none |

## 4. Floating production systems (FPSO, semi, TLP, spar) and their moorings and risers

Frameworks: API RP 2MIM, API RP 2FSIM, API RP 2I, DNV-OS-E301, DNV-OS-E303. Chain pitting is classed small / medium / mega in industry practice (Chain FEARS); class boundaries are inputs on #2199 with their public source.

| Asset | Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|---|
| Mooring chain, wire and fibre rope, connectors | General corrosion (uniform diameter loss) against discard criterion | API RP 2MIM, DNV-OS-E301 | mooring-chain-degradation (#2199), mooring-screen | T1, T2 | planned |
| Mooring chain, wire and fibre rope, connectors | Pitting classes (small / medium / mega) and discard guidance | API RP 2MIM, Chain FEARS | mooring-chain-degradation (#2199) | T1, T2 | planned |
| Mooring chain, wire and fibre rope, connectors | Wear at interlink / fairlead | API RP 2MIM, API RP 2I, DNV-OS-E301 | mooring-chain-degradation (#2199) | T1 | planned |
| Mooring chain, wire and fibre rope, connectors | Chain fatigue (tension-tension, out-of-plane bending) | API RP 2MIM, DNV-OS-E301, DNV-RP-C203 | mooring-fatigue, sn-fatigue | T2 | engine |
| Mooring chain, wire and fibre rope, connectors | Link ovalization / loose studs / cracks | API RP 2MIM, API RP 2I | mooring-chain-degradation (#2199) | T1 | planned |
| Mooring chain, wire and fibre rope, connectors | Wire rope broken wires / corrosion / diameter loss | API RP 2I | — | T1 | none |
| Mooring chain, wire and fibre rope, connectors | Fibre rope creep, abrasion, particle ingress | DNV-OS-E303 | synthetic-rope-fatigue | T1, T2 | engine (fatigue only; no condition-based discard) |
| Mooring chain, wire and fibre rope, connectors | Anchor holding capacity after seabed change / drag | DNV-OS-E301 | — | T2 | none |
| Steel catenary / top-tensioned production risers | Metal loss, pitting, cracks | API STD 2RD, DNV-ST-F201, API 579 Pt 4/5/6/9, BS 7910 | ffs-metal-loss, crack-fad | T1, T2 | engine |
| Steel catenary / top-tensioned production risers | Fatigue (wave, VIV) | DNV-ST-F201, DNV-RP-C203 | riser-fatigue | T2 | workflow |
| Drilling / completion riser joints and wellhead fatigue | Metal loss / weld flaws / collapse-limited depth | ASME B31G, DNV-RP-F101, BS 7910 | riser-joint-ffs | T1, T2 | engine (real C-scan fixtures; workflow #2183) |
| Drilling / completion riser joints and wellhead fatigue | Wellhead / conductor fatigue from riser loads | API RP 17G, DNV-RP-C203 | — | T2 | none |
| Unbonded flexible pipe | Outer sheath damage / annulus flooding | API RP 17B | flexible-pipe-screen (#2200) | T1 | planned |
| Unbonded flexible pipe | Tensile armour wire corrosion / rupture (end-fitting fatigue) | API RP 17B | flexible-pipe-screen (#2200) | T2 | planned |
| Unbonded flexible pipe | Carcass collapse, pressure sheath creep / rupture | API RP 17B | flexible-pipe-screen (#2200) | T2 | planned |

## 5. Well tubulars

| Asset | Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|---|
| Casing and tubing | Casing wear (drillstring contact) and corrosion, remaining collapse / burst with ovality | API TR 5C3 / ISO 10400 | casing-capacity (#2201) | T2 | planned |

## 6. Offshore wind support structures

| Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|
| Circumferential weld fatigue cracks | DNV-ST-0126, DNV-RP-C203, BS 7910 | sn-fatigue, crack-fad | T2, T3 | engine |
| Corrosion pitting to crack transition | DNV-RP-C203, BS 7910 | pitting, crack-growth | T2 | engine |
| Grouted connection slippage / cracking | DNV-ST-0126 | — | T2 | none |
| Bolted flange preload loss | DNV-ST-0126 | — | T1 | none |
| Scour | DNV-ST-0126 | scour | T1 | engine |
| Lifetime extension assessment (remaining fatigue life, load re-evaluation) | DNV-ST-0262, DNV-RP-C203 | — | T2 | none (S-N library exists but no lifetime-extension engine) |

## 7. Ships and floating hulls (class rules)

Frameworks: IACS CSR Ch 13 renewal criteria (wastage allowance, substantial-corrosion band, hull girder section modulus limits; values are inputs on #2202), IACS Rec. 84.

| Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|
| Thickness diminution vs renewal thickness (wastage allowance, substantial-corrosion band) | IACS CSR Ch 13 | hull-wastage (#2202) | T1 | planned |
| Buckling of thinned plates / panels | IACS CSR Ch 13, DNV-RP-C201 | plate-panel-buckling | T2 | workflow (plate metal-loss FFS on the capabilities page) |
| Pitting intensity on plating | IACS Rec. 84 | pitting | T1 | engine |
| Hull girder section modulus loss against class limits | IACS CSR Ch 13 | hull-girder, hull-wastage (#2202) | T2 | engine |
| Fatigue cracks at details | IACS CSR Ch 13, BS 7910 | sn-fatigue, crack-fad | T2 | engine |

## 8. Power boilers and high-temperature pressure parts

| Defect / mechanism | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|
| Tube / header wall thinning (erosion, fireside corrosion) | API 579 Pt 4/5, NBIC | ffs-metal-loss | T1, T2 | engine (2026-06-27 record covers pipe/vessel t_min, not ASME I / NBIC tube rules) |
| Creep damage / remaining life (Larson-Miller, Omega) | API 579 Pt 10 | parts-3-7-10 (#1274) | T2, T3 | planned |
| Thermal fatigue / creep-fatigue | API 579 Pt 14/10 | parts-8-11-13-14 (#2203) | T2 | planned |
| Crack-like flaws in headers / nozzles | API 579 Pt 9, BS 7910 | crack-fad | T2, T3 | engine |

## 9. Subsea pipeline / flowline design screens reusable in an integrity review

Data: `ffs_design_screen_catalog.yml` (owner decision D5, 2026-09-25). These are registered design-check workflows a pipeline integrity review reuses; they never produce a fitness-for-service verdict and are not counted in the coverage summary below.

| Screen | Governing codes | Engine(s) | Tier | Status |
|---|---|---|---|---|
| Free spans (VIV onset screening) | DNV-RP-F105 | free-span | T2 | workflow |
| Upheaval / lateral buckling | DNV-RP-F110 | — | T2 | none |
| On-bottom stability | DNV-RP-F109 | on-bottom-stability | T1 | workflow |

## Coverage summary

| Status | Rows |
|---|---|
| live | 0 |
| workflow | 3 |
| validated | 6 |
| engine | 31 |
| planned | 25 |
| none | 13 |
| total | 78 |

No row is `live` today: the 3 `workflow` rows lack validation records and the 6 `validated` rows lack a registered workflow. The 13 `none` rows are the roadmap beyond the filed issues: **pipelines-midstream**: manufacturing defects in pipe body, geohazard / ground-movement strain demand, direct-assessment programme support (ECDA / ICDA region and indication ranking); **downstream-fixed-equipment**: exchanger tube bundle thinning / pitting / tube plugging limits, fired-heater tube creep, bulging, carburization, floating roof / seal / appurtenance damage; **upstream-offshore-fixed**: conductor / caisson wall loss, guide wear, fatigue; **upstream-offshore-floating**: wire rope broken wires / corrosion / diameter loss, anchor holding capacity after seabed change / drag, wellhead / conductor fatigue from riser loads; **offshore-wind**: grouted connection slippage / cracking, bolted flange preload loss, lifetime extension assessment (remaining fatigue life, load re-evaluation).

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
