# Mooring industry practice survey

Research note on the issue, sources checked 2026-10-11.

Five passages flagged by the repository writing-register rule are paraphrased; facts, numbers, source ids and locators are unchanged. The research note’s exact wording for those entries is retained in the companion source list files.

- **Read** = the page was opened and the statement was taken from it. **Located, not read** (section 12) = the document exists at the URL given but could not be opened as text (PDF-only, paywall, "not found" or access refused). Nothing in the conclusions rests on a located-not-read item. Statements seen only in a search-result summary are tagged *(unverified)*.
- Vendor rows record published properties with the page they came from; no brochure prose is copied. Most numeric tables (break load, proof load, mass per metre) sit in vendor PDFs or images that could not be read here, and the cells say so.
- No paid-standard text is used. Standards are referred to by number and by what their public catalogue pages say.
- This extends the academic list on #1806; its keys (M1 to M40 there) are referred to as `#1806:Mnn`.

## 1. What practitioners do today (workflows and tool classes)

**1.1 Analysis chain.** The public record shows a three-tier practice:

| Tier | What it is | Who is on record using or offering it |
|---|---|---|
| Quasi-static / frequency-domain screening | Catenary statics, linearised stiffness, frequency-domain motions | Sesam Mimosa, described by DNV as "static and dynamic analysis of moored vessels and mooring systems" [M35]; BV Ariane, "static, time-domain based, multi-body", using catenary line models [M36][M37]; open-source MoorPy (quasi-static, catenary) [M26][M27]; MAP++ (static cable library) [M29]; RAFT with MoorPy for floating wind arrays [M33] |
| Time-domain de-coupled or coupled line dynamics | Lumped-mass or finite-element lines driven by, or coupled to, vessel motion | OrcaFlex (lumped-mass line model) [M9]; Sesam DeepC built on Simo and Riflex for coupled floater, mooring and riser analysis [M35]; MoorDyn (lumped-mass) coupled to OpenFAST, FAST.Farm, WEC-Sim and DualSPHysics [M25] |
| Monitoring-driven assessment | Measured position/motion plus a numerical model in place of load cells | MARIN MoniMoor JIP (2022, three years) [M40]; a physics-based condition monitoring system applied to a pilot floating wind park [M42]; operator-deployed machine-learning predictor on a turret-moored unit [M47] |

**1.2 Where the tiers disagree.** Two verified results put numbers or direction on the gap between tiers:
- In the OC6 Phase IV validation (17 partners, 19 numerical models, 1:43 basin test of a tetrahedral-spar floating wind unit) platform motions were estimated "reasonably", but the authors report "a significant dispersion in the dynamic loading for the upwind mooring line" across participants [M45].
- In the 2025 frequency-domain array model verification, differences in tension against the time-domain reference are attributed to the quasi-static mooring model omitting drag and line dynamics; shared-line tension standard deviations show the largest relative differences [M33].
- A 2025 journal comparison reporting 40 to 50 % higher peak tensions from a fully dynamic line model than from a quasi-static one was seen only in a search summary *(unverified, publisher page refused access; section 12)*.

**1.3 Fatigue.** The verified public material shows the design basis under revision rather than settled:
- BV lists the MoorLife JIP, aimed at reducing uncertainty in chain fatigue life through corrosion-state quantification, variable-amplitude loading and finite-element models with residual stress [M37].
- OTC 2026 carries a ClassNK/University of Tokyo paper on out-of-plane and in-plane bending effects on chain fatigue from scaled tank tests, and a paper on fatigue damage during installation and retrieval of drag anchors (titles only read) [M39].
- Mean-load and corrosion effects on chain fatigue (NTNU/SINTEF work) and a TWI study on grade R4 versus R5 in seawater were located but not readable (section 12), so no finding is drawn from them here.

**1.4 Integrity practice.** The verified direction of travel is from periodic, compliance-driven inspection to risk-based, life-cycle integrity management:
- The 2015 DeepStar study (two years, started December 2013) reported the annual probability of single-line failure at about 20 times what mooring codes typically assume and multiple-line failure at about 400 times; fatigue was the leading mechanism; single-line failures cost roughly USD 3 to 15 million each; the resulting guideline introduced risk-based inspection and was released to API as the basis of a mooring integrity recommended practice [M32].
- DNV-RP-E308 (edition 2021-10) covers integrity management over design, manufacture, installation and operation, including life extension and relocation [M30].
- A July 2026 trade interview with an installation contractor's senior engineer states the shift to risk-based programmes that combine inspection, engineering assessment, monitoring data and remaining-life evaluation, with photogrammetry, subsea measurement tools and digital twins as aids, and the caution that these "do not replace engineering judgment or physical inspection" [M31].
- A 2023 Norwegian regulator audit of two production floaters found no non-conformities but five improvement points, including "low reliability for detection of anchor line rupture" on one unit and no continuous measurement and logging of line stretch on either [M3]. Line-failure detection therefore remains an open item even on a mature shelf.

## 2. Vendor capability matrix (published properties and the page they come from)

| Vendor / product family | What the public page states | Not published on an open HTML page | Source |
|---|---|---|---|
| Drag anchors, STEVPRIS Mk6 | Three fluke-angle settings; uplift at seabed 5 degrees intact and 10 degrees damaged ("case by case"), with tests said to show 20 degrees possible; soil range soft to hard clay; over 8,000 units over about three decades, more than 2,000 of the sixth generation; type-approved | Holding-capacity chart is an image; weights and fluke angles in degrees | [M11] |
| Drag anchors for hard soil, STEVSHARK REX | Four fluke-angle settings; hollow fluke takes ballast up to 60 % of anchor weight; structure designed to "match the strength of R5 + 30% chains"; holding capacity 22 to 47 % above the previous model depending on location; soils include hard clay, cemented carbonate and calcarenite, dense sand | Absolute capacities (vendor calculates per site) | [M13] |
| Vertical load anchor, STEVMANTA | Line arrives at seabed at 45 degrees; capacity "up to 3.5 times the installation load" in vertical mode; single-line (shear pin) or double-line installation; introduced 1996; permanent version uses four wires in place of a shank | Sizes, approvals | [M12] |
| Anchor range overview | Seven anchor products listed, including a gravity-installed plate-type anchor for preset moorings | Any capacity figure | [M10] |
| Chain, Swedish maker | Preferred production range 76 to 165 mm (3 to 6.5 in), "all offshore grades"; studless link 3.35 d wide and 6 d long versus 3.6 d wide for stud link; studless about 9 % lighter than stud link of comparable strength, same break load at same nominal diameter; in-house connectors, test bench, inspection (phased array), RFID traceability | Grade list, break/proof load tables (catalogue PDF) | [M14][M15][M16] |
| Chain, Spanish group | Up to 220 mm; grades R3, R3S, R4, R4S, R5, R5S, R6; three plants with stated ranges (60 to 220 mm, 70 to 120 mm, 40 to 153 mm); 88,000 t/year capacity; an online property calculator; R&D list includes hydrogen-service alloys, monitoring and digital twin, floating wind | Break load tables on an open page | [M17][M18][M19] |
| Chain and accessories, Norwegian stockist | Stud link, studless, adaptors, weak links, swivel forerunners; tables of strength, weight, mechanical properties and minimum mean diameter exist | Table values are images or PDFs | [M20] |
| Polyester rope, Belgian maker (DeepRope) | High-tenacity polyester for deepwater; DNV-approved manufacturer under OS-E303; type approvals from ABS and BV; quayside plant for larger sizes and lengths; in-house stiffness and installation-performance testing | Break load, diameter, length limits | [M21] |
| Polyester and nylon rope, Portuguese/Dutch maker | 12x1 construction named for deepwater and offshore-energy markets; a May 2025 technical note positions nylon for permanent moorings shallower than about 500 m and lists failure modes under study (internal abrasion, marine growth ingress, seabed abrasion, cutting at steel components, long-term polymer degradation) | Break load, stiffness, fatigue data | [M22][M23][M24] |
| Steel wire (spiral strand) | Vendor mooring pages returned "not found" | Everything | section 12 |

**Open substitutes for vendor tables.** Two openly documented property sets can stand in for early-phase work:
- OrcaFlex line-type wizard, chain: mass per length 19.9 d^2 (studless) and 21.9 d^2 (stud link) te/m, axial stiffness 0.854e8 d^2 and 1.01e8 d^2 kN, with d in metres (units are stated on the page for the stud-link values); normal drag coefficient 2.4 / 2.6 on diameter d; axial drag 1.15 / 1.4 on d/pi; normal added mass 1.0, axial 0.5; outer contact diameter 3.35 d / 3.6 d; seabed friction "typically 0.4 to 0.8" [M7]. The contact diameters match the chain maker's published link widths [M15].
- OrcaFlex rope/wire wizard covers nylon, polyester and polypropylene (8-strand) and 6x19 wire with fibre or wire core, as functions of nominal diameter, "based on data from a catalogue published by Marlow Ropes Ltd (1995)", with the advice to use supplier data where available [M8]. The formula sub-pages were not read.
- MoorPy ships "a library of mooring component property and cost coefficients" and reference properties for common line materials and anchor types under a BSD-3-Clause licence [M26][M27]. The coefficient page itself returned "not found", so values are not quoted.

## 3. Software comparison (named in the public record; no ranking implied)

| Tool | Class | Line model and scope, as stated | Licence | Source |
|---|---|---|---|---|
| OrcaFlex | Commercial, time domain | Lumped-mass line of nodes and massless spring segments; line types, attachments (clump weights, buoyancy); chain and rope property wizards | Commercial | [M7][M8][M9] |
| Sesam Mimosa | Commercial | "Mooring system and individual mooring line analysis"; static and dynamic analysis of moored vessels | Commercial | [M35] |
| Sesam DeepC (Simo + Riflex) | Commercial, coupled time domain | Non-linear analysis of single or multi-body floaters with moorings and risers | Commercial | [M35] |
| Ariane (BV), v8.2.8 of July 2026 | Commercial | Static and time-domain multi-body, multi-component lines, drift-load models, thrusters, fenders; side-by-side and jetty mooring | Commercial | [M36] |
| Aqwa | Commercial | Vendor page read gives only a general hydrodynamics statement; no mooring detail could be verified | Commercial | [M48] |
| MoorDyn v2 | Open source | Lumped-mass dynamics, point-mass and rigid-body objects, Morison hydrodynamics; couples to OpenFAST, FAST.Farm, WEC-Sim, DualSPHysics. A 2024 summary of the wind-energy coupling adds synthetic-rope visco-elasticity, non-linear springs, dynamic cables, buoyancy cans, seabed friction and line-loss events | BSD-3-Clause | [M25][M28][M43] |
| MoorPy | Open source | Quasi-static catenary, equilibrium, stiffness matrices by semi-analytic Jacobians; platforms as linear hydrostatics only; theory paper "in progress" | BSD-3-Clause | [M26][M27] |
| MAP++ | Open source | Static cable loads and geometry through an API | Apache 2.0 | [M29] |
| RAFT (with MoorPy) | Open source | Frequency-domain floating wind arrays including shared lines | not read | [M33] |

Unverified here (pages not opened): SIMA as the current front end to Simo/Riflex, MOSES, Flexcom, ProteusDS, Moody, FEAMooring.

## 4. Recent conference and journal work (2022 to 2026)

Summarised from abstracts or full open pages:
1. **OpenFAST floating wind enhancements, OMAE2024 (presentation only)**: mooring module overhaul (rope visco-elasticity, line-loss events, seabed friction), shared-mooring support in the farm tool, large-yaw theory for line-loss cases, and a tight-coupling algorithm reported 10 to 100 times faster [M43].
2. **Sensors and digital twin for mooring integrity, OMAE2025**: argues load cells are costly and that neural-network detectors act only after failure and need extensive simulated training; proposes GPS plus motion-reference units feeding a quasi-static/dynamic solver with material properties updated for corrosion and creep; applied to operational data from a pilot floating wind park; no quantitative validation in the abstract [M42].
3. **Frequency-domain arrays with shared lines, Wind Energy Science 2025**: two 15 MW semi-submersibles 1,600 m apart in 600 m depth with one shared line; reports a "comb-like" tension response from wave phase differences between units [M33].
4. **OC6 Phase IV, Wind Energy Science 2024**: see 1.2 [M45].
5. **Synthetic rope tests for floating wind, PLoS One 2025**: polyester (break strength 1,786 kN) versus nylon (1,167 kN) sub-scale ropes; polyester stiffer and less creep-prone than nylon at 30 to 60 % of break strength; wet yarn-on-yarn abrasion life rises for polyester and falls for nylon. The page shows stiffness without units and reviewers raised concerns, so this is indicative only [M44].
6. **Machine-learning motion and mooring response, preprint 2025**: gradient-boosted regression trained on about 10^6 samples for a turret-moored unit in 400 m; mean errors below 5 % for key mooring parameters and heading within 2.5 degrees; stated as deployed on an operating facility [M47].
7. **Fatigue surrogate for chain, Wind Energy Science preprint 2025 (withdrawn by authors)**: R^2 of 0.928 for hourly damage from five metocean variables; recorded as a direction of work, not a result [M34].

Titles and authors only (abstracts not open): OTC 2025 session with operator papers on a new failure-detection technology for floaters, a chain breakage on a Gulf of Mexico platform, a top-chain replacement campaign, diverless top-chain cutting for decommissioning, a split mooring model test, and a passive constant-tension concept [M38]; OTC 2026 session with papers on drag-anchor installation fatigue, bending effects on chain fatigue, chain drag under mussel fouling, and remotely operated inspection and repair [M39]. The operator authorship shows that failure, detection and replacement case histories are still being reported every year.

Not reached: ISOPE and DOT mooring tracks (no open abstract pages found).

## 5. Benchmarks, code comparisons, round robins since 2018

| Exercise | Headline | Source |
|---|---|---|
| OC6 Phase IV (2024) | 19 models; wide spread in upwind-line dynamic tension, good agreement on keel-line tension | [M45] |
| Array model versus time-domain farm model (2025) | Mean offsets and fairlead tensions agree; quasi-static lines miss drag and dynamics in tension spectra | [M33] |
| Blind focused-wave test with taut-moored bodies (2021, reported under #1800) | Wide scatter even among like methods; mooring load was one of the compared quantities | see #1800 note |
| Open-source versus commercial line dynamics (2016, older than the window) | Fairlead loads from two open modules "largely equivalent" to the commercial reference | *(unverified, search summary; section 12)* |

No multi-participant mooring round robin for oil and gas units (as opposed to floating wind) was found on an open page. That absence is itself a finding: public cross-code evidence is concentrated in the wind-energy collaborations.

## 6. Problem areas and incident-driven changes

- **High-strength chain on mobile units.** A DNV review commissioned by the Norwegian regulator (published December 2022) covers 18 incidents, mostly on that shelf, with hydrogen embrittlement as the most probable leading mechanism and four contributing factors: material susceptibility, chain handling, high tensile load, and hydrogen from cathodic protection. It notes little systematic research and no established best practice [M1][M2]. The report PDF (which names the grades) could not be read.
- **Detection of a failed line** remains weak in practice [M3], and operators are publishing new detection technology [M38].
- **Codes versus observed failure rates.** See the DeepStar figures in 1.4 [M32].
- **Floating wind redundancy.** The Carbon Trust programme's fifth phase (2024) ran a moorings redundancy, reliability and integrity project that "clarifies what redundancy means" for three-cluster systems with one or two legs per cluster [M4]; the following phase (2025) developed an integrity management strategy for commercial-scale farms and studied marine growth prevention [M5]. Both summary PDFs were located but not readable.
- **Guidance documents.** OEUK Mooring Integrity Guidelines Issue 3 (first published November 2008, revised 2013/14) is a priced download [M6]; a 2021 Energy Institute guidance on chain systems exists but its page refused access (section 12).
- **Joint-industry work in progress** (class society list): remote subsea detection of corrosion and fatigue damage in chain towards a digital twin; chain fatigue life uncertainty; nylon rope behaviour and degradation; fatigue-accumulation monitoring; real-time line-health feedback [M37]; monitoring without load cells [M40].

## 7. Openly available data and models

- Line-property coefficient sets: section 2 [M7][M8][M26].
- Reference floating wind designs and shared-mooring cases in a public code organisation (five repositories listed) [M28].
- Validation data from the wind collaborations (basin tests with mooring tensions) [M45]; access conditions were not confirmed.
- No open full-scale chain or rope fatigue data set was found.

## 8. Trends, each tied to a source

| Trend | Evidence |
|---|---|
| Risk-based, life-cycle integrity management replacing fixed-interval inspection | [M30][M31][M32] |
| Monitoring by motion/position plus model, instead of load cells | [M40][M42] |
| Machine-learning surrogates for tension and fatigue, some in operation | [M47]; research stage [M34] |
| Floating wind driving shared lines, shallow-water nylon, redundancy definitions and farm-scale integrity strategy | [M33][M24][M4][M5] |
| Material limits of very high-strength chain under scrutiny | [M1]; hydrogen-service alloy R&D at a chain maker [M19] |
| Open-source line dynamics maturing (visco-elastic rope, line-loss events) | [M25][M43] |

Regulatory status that could not be verified on an open page: a 2024 fourth edition of the API stationkeeping practice, a 2019 API mooring integrity management practice reaffirmed in 2025, and the safety-factor and consequence-class content of the floating wind structure standard *(all unverified; section 12)*. The 2018 press release for the latter confirms only that its scope runs to "mooring and anchors of the complete wind farm" [M41].

## 9. Gap analysis for a small consultancy with strong open analysis capability

| Area | Verdict | Basis |
|---|---|---|
| Concept and screening (catenary statics, stiffness, offsets, array layouts) | **Open methods meet concept and screening needs** | MoorPy, MAP++, RAFT are documented, permissively licensed and verified against time-domain references [M26][M29][M33] |
| Line dynamics for extreme and fatigue tension | **Open methods adequate with care; cross-check needed** | MoorDyn has the features; the public evidence of spread in dynamic tension [M45] means any result should be bracketed by a second model or sensitivity set |
| Component properties (break load, stiffness, fatigue curves, anchor capacity) | **Proprietary data unavoidable for final design** | Vendors publish ranges and ratios, not tables, on open pages (section 2); anchor capacities are calculated by the vendor per site [M13] |
| Code-compliant design reports for class approval | **Licensed tools and paid standards largely unavoidable** | Class and client expectations centre on the commercial packages in section 3; the standards themselves are paid documents |
| Integrity: detection of line failure and tension estimation from motion data | **Differentiated offering plausible** | Regulator finds detection unreliable [M3]; JIPs are still developing this [M37][M40]; a physics-based open stack plus measured motions is exactly the method being published [M42] |
| Second opinion / independent verification of mooring analyses | **Differentiated offering plausible** | Documented code-to-code scatter [M45] and failure rates above code assumptions [M32] make an open, reproducible independent check saleable |
| High-strength chain material assessment (hydrogen embrittlement) | **Not an analysis-led niche** | Needs test data and metallurgy; little systematic research is public [M1] |

## 10. Cross-reference to existing knowledge seeds

The issue points to `knowledge/seeds/mooring/`. That path was not found in this repository through the hosting API on the day, so the cross-reference is given against the #1806 list instead: integrity and reliability (this note sections 1.4 and 6) extend `#1806:M1`; chain fatigue and out-of-plane bending (1.3) extend `#1806:M37` to `M40`; polyester and nylon behaviour (sections 2 and 4) extend `#1806:M18` to `M22`; drag and plate anchors (section 2) extend `#1806:M7` and `M30`, `M31`; line dynamics codes (section 3) extend `#1806:M16`. The seed mapping should be completed by whoever converts this note into `docs/research/mooring-industry-practice.md`.

## Sources read (all checked 2026-10-11)

- [M1] Usability and suitability of high strength mooring chains (project report page). Norwegian Ocean Industry Authority (Havtil), 2022. https://www.havtil.no/en/explore-technical-subjects2/technical-competence/reports-from-projects/2022/usability-and-suitability-of-high-strength-mooring-chains/
- [M2] DNV investigates mooring lines failures. Offshore magazine, 2022. https://www.offshore-mag.com/business-briefs/equipment-engineering/article/14287387/dnv-investigates-mooring-lines-failures
- [M3] Audit report: follow-up of anchoring systems on two production floaters. Havtil, 2023. https://www.havtil.no/en/supervision/audit-reports/2023/aker-bp--skarv-and-alvheim--follow-up-of-anchoring-systems/
- [M4] Floating Wind Joint Industry Programme: Phase V summary report (landing page). Carbon Trust, 2024. https://www.carbontrust.com/our-work-and-impact/guides-reports-and-tools/floating-wind-joint-industry-programme-phase-v-summary-report
- [M5] Floating Wind Stage 3 Phase 1 summary report (landing page). Carbon Trust, 2025. https://www.carbontrust.com/our-work-and-impact/guides-reports-and-tools/floating-wind-stage-3-phase-i-summary-report
- [M6] Mooring Integrity Guidelines, Issue 3 (catalogue page). Offshore Energies UK, undated. https://oeuk.org.uk/product/mooring-integrity-guidelines-issue-3/
- [M7] OrcaFlex help: Chain. Orcina, current web help. https://www.orcina.com/webhelp/OrcaFlex/Content/html/Chain.htm
- [M8] OrcaFlex help: Rope/wire. Orcina. https://www.orcina.com/webhelp/OrcaFlex/Content/html/Ropewire.htm
- [M9] OrcaFlex help: Lines. Orcina. https://www.orcina.com/webhelp/OrcaFlex/Content/html/Lines.htm
- [M10] Anchors (product overview). Delmar Vryhof, 2026. https://delmarvryhof.com/products/anchors/
- [M11] STEVPRIS Mk6 product page. Delmar Systems, 2026. https://delmarsystems.com/products/anchors/stevpris-mk6/
- [M12] STEVMANTA product page. Delmar Systems, 2026. https://delmarsystems.com/products/anchors/stevmanta/
- [M13] STEVSHARK REX product page. Delmar Systems, 2026. https://delmarsystems.com/products/anchors/stevshark-rex/
- [M14] Products. Ramnäs Bruk, 2026. https://ramnas.com/products/
- [M15] Studless mooring chain. Ramnäs Bruk, 2026. https://ramnas.com/products/mooring-chains/studless/
- [M16] Downloads (list of 17 documents). Ramnäs Bruk, 2026. https://ramnas.com/downloads/
- [M17] Home page. Vicinay Marine, 2026. https://www.vicinaymarine.com/
- [M18] Marine and mooring chains manufacture. Vicinay Marine, 2026. https://www.vicinaymarine.com/business-units/marine-and-mooring-chains-manufacture/
- [M19] Key R&D projects. Vicinay Marine, 2026. https://www.vicinaymarine.com/business-units/engineering-and-innovation/research-and-development/key-rd-projects/
- [M20] Chains (product category and table index). Sotra Anchor & Chain, 2026. https://www.sotra.net/products/chains
- [M21] Deepwater mooring (Bexco). Bekaert, 2026. https://ropes.bekaert.com/gb/en/bexco/industries/oil-and-gas/deepwater-mooring
- [M22] Home page. Lankhorst Offshore, 2026. https://www.lankhorstoffshore.com/
- [M23] Deepwater mooring: rope selection. Lankhorst Offshore, 2026. https://www.lankhorstoffshore.com/markets/deepwater-mooring/rope-selection
- [M24] Nylon ropes for floating wind taut and semi-taut moorings: a progress report. Lankhorst Offshore, 2025. https://www.lankhorstoffshore.com/about-us/news-events/nylon-ropes-for-fowt-taut-and-semi-taut-moorings-a-progress-report
- [M25] MoorDyn documentation. MoorDyn project, current. https://moordyn.readthedocs.io/en/latest/
- [M26] MoorPy documentation. United States national laboratory (wind software stack), current. https://moorpy.readthedocs.io/en/latest/
- [M27] MoorPy repository. https://github.com/NREL/MoorPy
- [M28] FloatingArrayDesign organisation (repository list). https://github.com/FloatingArrayDesign
- [M29] MAP++ documentation. https://map-plus-plus.readthedocs.io/en/latest/
- [M30] DNV-RP-E308 Mooring integrity management (catalogue page, edition 2021-10). DNV. https://www.dnv.com/energy/standards-guidelines/dnv-rp-e308-mooring-integrity-management/
- [M31] Mooring integrity requires life-cycle thinking as deepwater projects grow more complex. Offshore magazine, 2026. https://www.offshore-mag.com/subsea/article/55393856/intermoor-mooring-integrity-requires-life-cycle-thinking-as-deepwater-projects-grow-more-complex
- [M32] DeepStar mooring study to form basis of new API RP. Offshore magazine, 2015. https://www.offshore-mag.com/business-briefs/equipment-engineering/article/16758300/deepstar-mooring-study-to-form-basis-of-new-api-rp
- [M33] Hall, Carmo, Lozon: Frequency-domain modeling of floating wind arrays with shared mooring lines. Wind Energy Science 10, 3027-3043, 2025. https://wes.copernicus.org/articles/10/3027/2025/
- [M34] Ludot et al.: Data-driven surrogate models for real-time fatigue monitoring of chain mooring lines in floating wind turbines (preprint, withdrawn). Wind Energy Science Discussions, 2025. https://wes.copernicus.org/preprints/wes-2024-162/
- [M35] Sesam software for mooring and risers. DNV, 2026. https://www.dnv.com/services/sesam-software-for-mooring-and-risers-2310/
- [M36] Ariane: advanced mooring software. Bureau Veritas Marine & Offshore, 2026. https://marine-offshore.bureauveritas.com/node/362
- [M37] Mooring integrity (research and development page). Bureau Veritas Marine & Offshore, 2026. https://marine-offshore.bureauveritas.com/research-development/mooring-integrity
- [M38] Technical programme session: Technological Advancements in Mooring Systems Design, Failure Detections and Repair Case Studies. Offshore Technology Conference, 2025. https://2026.otcnet.org/2025-technical-program/technological-advancements-in-mooring-systems-design-failure-detections-and-repair-case-studies
- [M39] Technical programme session: Advances in Floating Systems, Design, Monitoring, and Integrity Management. Offshore Technology Conference, 2026. https://2027.otcnet.org/2026-technical-program/advances-in-mooring-systems-design-monitoring-and-integrity-management
- [M40] MoniMoor JIP. MARIN, 2022. https://www.marin.nl/en/jips/monimoor
- [M41] Press release on the revised floating wind turbine structure standard and certification guideline. DNV, 2018. https://www.dnv.com/news/2018/dnv-gl-launches-revised-design-standard-and-new-certification-guideline-for-floating-wind-turbines-130237/
- [M42] Advanced Sensors and Digital Twin Technology for Reliable Mooring Integrity Assessment Through Physics Based Models (submission 157554). ASME OMAE2025 session gallery. https://omae.secure-platform.com/a/solicitations/246/sessiongallery/20278/application/157554
- [M43] Recent Floating Wind Modelling Enhancements in OpenFAST (submission 123909). ASME OMAE2024 session gallery. https://omae.secure-platform.com/a/solicitations/214/sessiongallery/15604/application/123909
- [M44] Zeng et al.: Mechanical behavior of synthetic fiber ropes for mooring floating offshore wind turbines. PLoS One 20(2), e0318190, 2025. https://pmc.ncbi.nlm.nih.gov/articles/PMC11801622/
- [M45] Bergua et al.: OC6 project Phase IV: validation of numerical models for novel floating offshore wind support structures. Wind Energy Science 9, 1025-1051, 2024. https://wes.copernicus.org/articles/9/1025/2024/
- [M47] Morris-Thomas, Martens: An application of machine learning to the motion response prediction of floating assets. arXiv 2506.15713, 2025. https://arxiv.org/abs/2506.15713
- [M48] Aqwa product page. Ansys (Synopsys), 2026. https://ansys.synopsys.com/products/structures/ansys-aqwa

## Located, not read (kept out of conclusions)

- UK HSE research reports RR444 (2006) and RR1090 to RR1098 (2017) on floating-production mooring integrity: index pages returned "not found" (https://www.hse.gov.uk/research/rrhtm/rr1090.htm). Titles seen in search summaries only.
- DNV report 2022-1060 for the Norwegian regulator (PDF, 7.4 MB): https://www.havtil.no/contentassets/a55d5b8c269b4d7e9dc164b781ef2022/2022_1060-rapport-usability-and-suitability-of-high-strength-mooring-chains-rev.-1-signed.pdf
- Carbon Trust Phase V collated summary (PDF, 55 pages) and Stage 3 Phase 1 summary (PDF, 82 pages): https://www.carbontrust.com/sites/default/files/2024-03/Collated-Summary-Report.pdf and the link on [M5].
- Energy Institute, Guidance for the management of mooring integrity part 1: chain systems (2021): access refused.
- OTC-25273 (2014 industry survey of failures and pre-emptive replacements) and the consultancy summaries of it: "not found" or paywalled.
- API stationkeeping practice (fourth edition, 2024) and mooring integrity management practice (2019, reaffirmed 2025): retailer listings in search summaries only.
- Floating wind structure standard (2021 edition): consequence classes and factors seen only on third-party slides.
- ABS fibre-rope guidance notes (June 2021) and position-mooring requirements (2022/2023): PDFs.
- Industry drilling-contractor safety alert 24-3 (2024) on a moored semi-submersible losing lines in a storm; United States regulator accident report on a parted line (2021) and review of a tension-loss event (2023); United States safety alerts 259 and 296: PDFs not readable; details in search summaries only.
- Australian regulator information paper on mooring integrity: timed out.
- Chain maker's product catalogue PDF; stockist's strength, weight and mechanical-property tables (PDF/images); anchor manual and brochure (behind a request form); steel-wire rope mooring pages ("not found").
- NTNU doctoral thesis (2023) and SINTEF papers on mean-load and corrosion effects in chain fatigue; TWI report 1135/2020 on high-strength chain in seawater: records opened but no abstract text returned.
- MDPI J. Mar. Sci. Eng. 13(12) 2407 (2025), two-solver comparison including mooring peak tensions: access refused.
- National-laboratory deep-water shared-anchor report (2025): host did not resolve.
- MoorDyn features/theory pages and MoorPy coefficient page: "not found".
- OTC 2025/2026 paper abstracts for [M38][M39] (titles only read).

## Counts and acceptance-criteria status

- Sources read: **47**. Located, not read: **about 27 documents** in 16 groups.
- Acceptance criteria: vendor capability matrix (section 2, with the stated limit that numeric tables are not on open pages); five-plus recent papers summarised (section 4: two OMAE submissions and five journal/preprint items read, plus thirteen OTC papers by title only); software comparison (section 3); cross-reference to seeds (section 10, partial: seed path not found).
- Not done: Bridon-Bekaert wire data; ISOPE and DOT tracks; HSE reports; the integrity JIP known as MIRP in the issue title could not be matched to a public page under that name (the DeepStar study [M32] and the HSE-published JIP in section 12 are the closest public outputs).
