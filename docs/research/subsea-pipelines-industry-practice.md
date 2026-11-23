# Subsea pipelines industry practice survey

Tool landscape, method convergence and divergence, key papers, contractor matrix, open-source landscape, joint-industry projects, gap analysis.

Single source: [research note on the issue](https://github.com/vamseeachanta/digitalmodel/issues/1815#issuecomment-6115103070), sources checked 2026-10-11.

Three passages flagged by the repository's writing-register rule are paraphrased; facts, numbers, source ids and locators are unchanged. The note's exact wording for these passages is retained in the adjacent source list file.

This domain was researched last and is the thinnest of the three notes posted today; the counts and acceptance-criteria status section lists what was not done.

- Every source was checked on **2026-10-11**. Sources are numbered `[Pnn]` and listed in section 11 with title, publisher, year and URL.
- **Read** = the page was opened and the statement taken from it. **Located, not read** (section 12) = found but not openable as text (PDF-only data sheets, paywalls, access refused). Statements seen only in a search-result summary are tagged *(unverified)* and are not used in conclusions.
- Conference papers were read at abstract level from the ASME conference's public session gallery. Vessel figures come from trade-press or shipbuilder pages because the contractors' own pages either refused access or publish data sheets as PDFs; each cell names its source.
- No paid-standard text is used: standards are cited by number, edition date and the scope sentence on their public catalogue page.
- Extends the academic list on #1814 (keys cited as `#1814:Pnn`).

## 1. Tool landscape

| Task | Method in use | Tools named in the public record | Source |
|---|---|---|---|
| Wall thickness, burst, collapse, propagation, combined loading | Limit-state (load and resistance factor) code check | DNV-ST-F101 code-check tool; an early-phase "Pipeline Engineering Tool" | [P8][P16] |
| On-bottom stability, simplified | Absolute static and generalised methods to find minimum weight | StableLines | [P8] |
| On-bottom stability, dynamic | Time-domain lateral displacement with clay pipe-soil models and second-order irregular waves for shallow water | PILSS (developed through a JIP; GUI, command line and API) | [P9] |
| Free spans | Fatigue from wave loading and in-line/cross-flow vortex-induced vibration, plus ultimate checks | FatFree (desktop and API); Vivana for "free-span pipelines" under current | [P8][P34] |
| Global (lateral and upheaval) buckling, early phase | Analytical/probabilistic screening | SimBuck; the SAFEBUCK JIP's BUCKFAST reliability tool, "limited to flat seabeds or simple loading" per a 2024 contractor paper | [P8][P1] |
| Global buckling and walking, detailed | Non-linear finite-element models of the whole flowline on the as-laid seabed profile, with response surfaces and Monte Carlo sampling | General-purpose finite-element codes (the abstracts read do not name the product) | [P1][P3] |
| Corrosion defect assessment | Burst pressure of single, interacting and irregular defects from inspection data | DNV-RP-F101 tool | [P8][P15] |
| Pipelay | Static and dynamic lay analysis with stinger roller contact; automated lay tables | OrcaFlex "supports" feature (cylinders fixed to a vessel or buoy, designed for stinger roller boxes; simple path or explicit geometry) and six pipelay examples including rigid, hinged and articulated stingers and lay-table automation; OFFPIPE named as the "well-established" reference in a 2025 thesis | [P26][P27][P25] |
| Rock-berm and seabed intervention design | Two-dimensional limit equilibrium versus three-dimensional finite elements with consolidation | Not named in abstract | [P5] |

Cathodic protection tools and pre-commissioning practice, both named in the issue, were not reached.

## 2. Method convergence versus divergence

**Converging**
1. **One recommended-practice family frames most public work.** The papers read check against DNV-ST-F101 (edition 2021-08, amended 2021-12) and its companion practices: global buckling RP-F110 (2019-09, amended 2021-09), pipe-soil interaction RP-F114 (2021-05), on-bottom stability RP-F109 (2021-05, amended 2025-09), free spans RP-F105 (2025-12), corroded pipelines RP-F101 (2019-09, amended 2025-09) [P16][P12][P13][P11][P14][P15]. One paper checks strength and fatigue against both API RP 1111 and DNV-ST-F101 [P3].
2. **Controlled lateral buckling is the accepted design philosophy for hot, exposed lines.** The RP's stated aim is controlled buckling of an unburied pipeline with safety maintained [P12]; the JIP behind it promoted pre-installed triggers at intervals [P17][P19]. Trenching and burial as the historical restraint is described as less practical in deep water [P19].
3. **Pipe-soil interaction as a force-displacement response, not a friction coefficient.** This was a core SAFEBUCK output and now has its own recommended practice [P18][P13].

**Diverging or unresolved**
1. **Deterministic versus probabilistic buckling design.** A 2024 contractor paper calls deterministic design potentially "extremely conservative" and proposes finite-element response surfaces with Monte Carlo sampling over pipe-soil, sleeper friction and out-of-straightness, compared with code target failure probabilities by safety class; it also says earlier probabilistic tools ignore seabed imperfections and buckle interaction [P1].
2. **Choice of buckle initiator.** Sleepers alone, sleepers combined with mattresses, and residual curvature are all being reported: combining mattresses at the touchdown points with sleepers cut maximum longitudinal strain and thermal fatigue damage by more than 50 % on very soft clay [P3]; residual curvature, common in shallow water, is being applied to ultra-deep exposed lined flowlines anchored at both ends, with stated concerns about landing on very soft soil and competition with rogue buckles [P2].
3. **On-bottom stability: simplified static versus dynamic, and what the practice leaves out.** The vendor positions dynamic time-domain analysis as giving "more precise lateral displacements" and lower weight or intervention cost than the simplified methods [P9]. A class-led JIP planned to start in the first quarter of 2027 lists scenarios the current guidance "does not adequately cover": cables, shallow-water hydrodynamics, temporary conditions, inclined or rocky seabeds, bundles and piggy-back arrangements, short free spans, and time-dependent effects such as scour and self-burial [P10].
4. **Mobile seabeds.** A 2018 summary of a university-led JIP states that existing guidelines did not explain how to account for seabed mobility, and that its methods were incorporated in a class guideline [P20]. Field evidence in 2024 shows the issue is live: on a 20-inch, 44 km line, spans corrected with grout bags in 2015 to 2016 reappeared elsewhere by 2017; after most of the line was lowered and buried in 2018, surveys from 2019 to 2023 found much of it exposed again with new spans beyond the allowable length [P4].
5. **Installation analysis tools.** Mismatches between two lay-analysis programs, especially for deep-water S-lay, motivated a 2025 thesis that found both agreement and discrepancy in catenary shape, stress and tension and traced them to modelling assumptions and boundary conditions [P25].
6. **Rock-berm design method.** Two-dimensional limit equilibrium versus three-dimensional finite elements with soil consolidation, with the latter reducing rock volume on a shelf project with iceberg scars [P5].
7. **Pressure-containment reduction under compressive force.** A class FAQ interpretation drew industry attention for pipe-in-pipe and buried hot lines; a JIP (phase 1 December 2022 to November 2023, phase 2 planned 2024 to 2025) aims at less conservative reductions and strain limits [P30].

The issue's example (absolute versus dynamic stability methods and their adoption) could not be quantified: no public survey of which method operators accept was found.

## 3. Key state-of-the-art papers (2024 onwards)

All OMAE2024, abstract level:
1. **Probabilistic lateral buckling for hot flowlines with buckle mitigators** (contractor): 5 km global finite-element model on undulating seabed; stochastic axial and lateral pipe-soil response, sleeper friction and horizontal out-of-straightness; outputs include post-buckle limit states, walking rate, end loads and rogue versus engineered buckle locations; no numbers in the abstract [P1].
2. **Residual curvature method for ultra-deep-water flowlines** (contractor with operator): field feedback from exposed mechanically lined flowlines with suction-pile anchors at both ends; measures to keep as-laid lateral offset acceptable; no numbers in the abstract [P2].
3. **Lateral buckling mitigation with combined sleepers and mattresses** (consultant with operator): finite-element sensitivities on single and dual sleepers, friction bounds, berms and sleeper height; more than 50 % reduction in strain and thermal fatigue damage; results insensitive to mattress position, friction, width and height [P3].
4. **Field observation after span rectification by burial** (consultant with operator): see section 2, item 4 [P4].
5. **Optimising subsea rock installation on a soft-clay shelf site**: see section 2, item 6 [P5].
6. Same sessions, titles only: reeling and unreeling a pipe with Lüders bands; bending response, collapse and fabrication analysis of mechanically lined pipe; slugging fatigue of a steel lazy-wave riser [P6][P7]. These show lined-pipe mechanics as a second active theme alongside buckling.

Against the acceptance criterion: five OMAE papers summarised at abstract level. **No ISOPE, OTC, OPT, DOT or pigging-association paper was opened.** A further full-paper source is the 2025 master's thesis on lay-analysis validation [P25].

## 4. Contractor and vessel capability matrix

### Rows sourced to pages read

| Contractor: vessel | Technique | Figures as published | Source and its limits |
|---|---|---|---|
| Saipem: Castorone | S-lay, steep S-lay beyond 2,000 m; designed for a later J-lay tower (triple joints, two stations) for beyond 3,000 m | Length 330 m without ramp; pull capacity 750 t from three 250 t tensioners; 1,500 t holding capacity flooded; pipe up to 60 in; three-section articulated stinger totalling 121 m; DP3 | Trade article, 2013 [P21]; contractor page refused access |
| McDermott: DLV 2000 | Deep-water S-lay with heavy lift | Three 150 t tensioners; 2,200 t main crane; pipe 4.5 to 60 in; 100 m stinger; 500 t / 250 t abandonment and recovery winch; DP3; 184 m length | Trade profile [P22]; contractor page "not found" |
| Heerema: Aegir | J-lay and reel-lay tower with heavy lift | Tower rated 2,000 t for J-lay operations; main crane 4,000 t in the specification table and 5,000 t after a 2021 upgrade; crane work down to 3,500 m; DP3 | Ship designer's reference page [P23]; reel-lay tension and pipe sizes not stated there |
| Subsea7: Seven Vega | Reel-lay | 600 t top tension; 32 m main reel (5,600 t) and 17 m auxiliary reel (1,600 t); up to 3,000 m water depth | Trade article, 2019 [P24]; contractor data sheet is a PDF |

### Unverified or not reached (kept out of conclusions)

| Contractor: vessel | Technique | Figures as published | Source and its limits |
|---|---|---|---|
| Allseas: Solitaire | S-lay | 1,050 t tension from three 350 t tensioners, 2 to 60 in, 300 m length, DP3 *(unverified: official spec sheet is a PDF, figures seen in search summary only)* | section 12 |
| Subsea7: Seven Borealis | S-lay and J-lay | 600 t S-lay, 937 t J-lay *(unverified, PDF data sheet)* | section 12 |
| TechnipFMC reel-lay fleet; Allseas other vessels; Saipem other vessels; line pipe mills | not reached | n/a | section 13 |

The acceptance criterion asks for six contractors: four have verified rows (from secondary sources), one is unverified, one (TechnipFMC) is not covered.

## 5. Open-source landscape

- **Verified open items are few.** Open documentation exists for commercial tools (pipelay supports and worked examples [P26][P27]) but not open code.
- **Probabilistic lateral buckling.** A GPL v3 Python package published by a consultancy, implementing a 2023 congress paper on friction distributions at expected buckles, appeared in search results; its package page failed to load and the repository address returned "not found" *(unverified; section 12)*.
- **General pipeline calculation libraries.** Two Python packages were seen in search results only: one in alpha, one restricted to personal non-commercial use *(unverified)*.
- **Not found:** any open implementation of on-bottom stability, free-span fatigue or defect assessment with published verification; any open pipelay solver; the "FreeCAD pipeline tooling", "BHpipe" and regulator tools named in the issue.
- General-purpose open dynamics and finite-element codes could be applied to lay and buckling problems, but no public pipeline verification of them was found in this search.

## 6. Joint-industry project landscape

| JIP | What the public record states | Output | Source |
|---|---|---|---|
| SAFEBUCK phases 1 and 2 | Lateral buckling of hot on-bottom pipelines: design guideline for single pipe and pipe-in-pipe, predictive models, buckle initiation methods, joint integrity, pipe-soil tests under large cyclic displacement; engineering by a UK consultancy, materials testing by a welding institute, geotechnical testing at a university. A 2010 article cites three full-bore ruptures and one abandonment attributed to design, and a walking pipeline whose end connection ruptured; low-cycle fatigue lives reduced "by a factor of ten or more" in corrosive conditions | Reports and software proprietary to members | [P17][P19] |
| SAFEBUCK phase 3 and GEO (from 2014) | Structural reliability check of the guideline; operating data; a force-resultant plasticity model for lateral pipe-soil response; axial friction review; formalising the guideline into an industry recommended practice | Confidential to members for one year after completion | [P18] |
| Mobile-seabed stability JIP (university-led) | Sedimentation and scour, pipe-fluid and pipe-soil interaction; new stability design methods | Incorporated in a class guideline | [P20] |
| Dynamic lateral stability software JIP | Time-domain stability with clay models | Commercial tool | [P9] |
| On-bottom stability update JIP (planned 2027) | See section 2, item 3 | Recommendations to update RP-F109 | [P10] |
| Compressive true force and pressure containment | See section 2, item 7 | Updated guideline | [P30] |
| Concrete-coated pipe installation criteria (phase 2 announced 2018) | Criteria described as more than 20 years old; phase 2 to test bending into the non-linear strain range; possible relaxation of weather limits | Design guideline | [P31] |
| Hydrogen pipelines (2021 to 2026, 37 partners) | Hydrogen embrittlement and integrity; new and requalified offshore lines | DNV-RP-F123 issued March 2026; phase 3 full-scale tests to follow | [P28] |
| H2S in CO2 streams (from March 2022) | Sulphide stress cracking and corrosion risk at higher H2S limits in carbon-steel CO2 lines | Intended update of RP-F104 | [P29] |

The origin of RP-F110 is described by the class society only as "a joint industry project involving leading oil companies and consultants" [P33]. **HOTPIPE and POSIT, named in the issue, could not be confirmed on any public page opened**, and no public SAFEBUCK deliverable summary was found (consistent with the gap noted on #1814).

## 7. Problem areas and incident-driven changes

- Failures cited by the JIP authors (ruptures, abandonment, walking-induced connection failure) drove the controlled-buckling guideline [P19].
- Span recurrence on mobile seabeds after rectification [P4]; a class society acting as technical expert for a mining authority classed several spans on a 443 km gas trunk line (in service since 1977) as critical, forcing immediate remediation in a protected tidal area [P32].
- A UK regulator guidance document on anchor hazards to pipelines (written after a 2007 anchor strike on a gas trunk line) and the UK loss-of-containment statistics report covering 2001 to 2020 (seventh edition, December 2024) were located but not readable (section 12). **No regulator safety alert specific to subsea pipelines from 2022 to 2025 was verified.**

## 8. Openly available data

None verified beyond the published papers. Project survey data, pipe-soil test data and JIP software are proprietary or member-confidential [P17][P18].

## 9. Trends, each tied to a source

| Trend | Evidence |
|---|---|
| Probabilistic, finite-element-based buckling design replacing deterministic envelopes | [P1] |
| Cheaper buckle initiation (residual curvature in deep water; mattresses with sleepers) | [P2][P3] |
| Mechanically lined pipe mechanics (reeling, collapse, bending) | [P6][P7] |
| Energy-transition service: hydrogen practice issued 2026; CO2 impurity limits under study | [P28][P29] |
| On-bottom stability guidance being widened to cables, shallow water and time-dependent seabed effects | [P10] |
| Design tools exposed through programming interfaces for scripted use | [P8][P9] |
| Sensor technology written into the free-span practice (2025-12 edition lists "application of sensor technology") | [P14] |

Digital twins and machine-learning surrogates specific to pipelines were not verified on a page opened.

## 10. Gap analysis for a small consultancy with strong open analysis capability

| Area | Verdict | Basis |
|---|---|---|
| Code checks (wall thickness, collapse, propagation), simplified stability, span screening, expansion and screening-level buckling | **Open methods adequate in principle, but must be self-built and self-verified** | The methods are closed-form and documented in the texts on #1814; the standards are paid; no verified open implementation was found (section 5) |
| Detailed lateral buckling, walking and upheaval analysis | **Finite-element capability needed; a licensed solver is the norm, proprietary soil data unavoidable** | Whole-line non-linear models with stochastic pipe-soil inputs [P1][P3]; pipe-soil test data and JIP models are member-confidential [P18] |
| Dynamic on-bottom stability | **Licensed tool is the accepted route** | Purpose-built commercial tool from a JIP [P9]; guidance still being extended [P10] |
| Pipelay analysis | **Licensed tools unavoidable for contractor-grade work** | Commercial tools with documented stinger modelling [P26][P27]; even these disagree in deep-water S-lay [P25] |
| Probabilistic design studies (response surfaces, Monte Carlo over soil and out-of-straightness) | **Differentiated offering plausible** | Contractors are publishing the method as new and as a cost reducer [P1]; the statistical layer is open-tool territory even when the solver is licensed |
| Span and exposure trending from repeat survey data on mobile seabeds | **Differentiated offering plausible** | Operators report recurring spans and failed burial [P4][P32]; guidance gaps on time-dependent seabed effects are acknowledged [P10] |
| Independent verification of lay or buckling analyses | **Plausible** | Documented tool-to-tool discrepancies [P25] |
| Hydrogen and CO2 requalification studies | **Standards and test data dominated; limited analysis-led opening** | New practice built on JIP testing [P28][P29] |
| Installation capability data (vessel limits) | **Proprietary; request from contractors** | Public figures are dated or secondary (section 4) |

## Sources read

- [P1] Bai et al.: Probabilistic Approach of Lateral Buckling Analyses for HPHT Pipelines With Buckle Mitigators (submission 126874). ASME OMAE2024 session gallery. https://omae.secure-platform.com/a/solicitations/214/sessiongallery/15119/application/126874
- [P2] Jebali et al.: Lateral Buckling Control With the Residual Curvature Method for Ultra Deepwater Flowlines (submission 121344). ASME OMAE2024. https://omae.secure-platform.com/a/solicitations/214/sessiongallery/15119/application/121344
- [P3] Hayes et al.: Pipeline Lateral Buckling Mitigation With Combined Sleepers and Mattresses (submission 123740). ASME OMAE2024. https://omae.secure-platform.com/a/solicitations/214/sessiongallery/15119/application/123740
- [P4] Teh et al.: Pipeline Field Observation After Span Rectification by Burial (submission 120824). ASME OMAE2024. https://omae.secure-platform.com/a/solicitations/214/sessiongallery/15120/application/120824
- [P5] Xie et al.: Optimizing Subsea Rock Installation for Cost Savings: A Norwegian Shelf Case Study (submission 128176). ASME OMAE2024. https://omae.secure-platform.com/a/solicitations/214/sessiongallery/15752/application/128176
- [P6] Session 04-04-04 Lateral Buckling and Reeling (listing). ASME OMAE2024. https://omae.secure-platform.com/a/solicitations/214/sessiongallery/15119
- [P7] Session 04-04-02 Rigid Pipe Mechanics II (listing). ASME OMAE2024. https://omae.secure-platform.com/a/solicitations/214/sessiongallery/15120
- [P8] Pipeline tools (product overview). DNV, 2026. https://www.dnv.com/energy/services/subsea-facilities/pipeline-tools/
- [P9] Pipeline, umbilical and cable lateral stability software: PILSS. DNV, 2026. https://www.dnv.com/services/pipeline-umbilical-and-cable-lateral-stability-software-pilss-241930/
- [P10] Joint industry project: improving on-bottom design for pipelines, cables and umbilicals. DNV, 2026. https://www.dnv.com/group/joint-industry-projects/improving-on-bottom-design-for-pipelines-cables-and-umbilicals/
- [P11] DNV-RP-F109 On-bottom stability design of submarine pipelines, cables and umbilicals (catalogue page). DNV. https://www.dnv.com/energy/standards-guidelines/dnv-rp-f109-on-bottom-stability-design-of-submarine-pipelines/
- [P12] DNV-RP-F110 Global buckling of submarine pipelines (catalogue page). DNV. https://www.dnv.com/energy/standards-guidelines/dnv-rp-f110-global-buckling-of-submarine-pipelines/
- [P13] DNV-RP-F114 Pipe-soil interaction for submarine pipelines (catalogue page). DNV. https://www.dnv.com/energy/standards-guidelines/dnv-rp-f114-pipe-soil-interaction-for-submarine-pipelines/
- [P14] DNV-RP-F105 Free spanning pipelines (catalogue page). DNV. https://www.dnv.com/energy/standards-guidelines/dnv-rp-f105-free-spanning-pipelines/
- [P15] DNV-RP-F101 Corroded pipelines (catalogue page). DNV. https://www.dnv.com/energy/standards-guidelines/dnv-rp-f101-corroded-pipelines/
- [P16] DNV-ST-F101 Submarine pipeline systems (catalogue page). DNV. https://www.dnv.com/energy/standards-guidelines/dnv-st-f101-submarine-pipeline-systems/
- [P17] TAP-454: research record on safety in the design of hot on-bottom pipelines with lateral buckling (see section 2: controlled buckling of an unburied pipeline with safety maintained [P12]). United States Bureau of Safety and Environmental Enforcement. https://www.bsee.gov/research-record/tap-454-safe-design-hot-bottom-pipelines-lateral-buckling-safebuck
- [P18] TAP-664: Safebuck Phase III and Safebuck Geo JIPs (research record). United States Bureau of Safety and Environmental Enforcement, 2014. https://www.bsee.gov/research-record/tap-664-safebuck-phase-iii-and-safebuck-geo-jips
- [P19] Bruton, Carr: Bucking the trend. Offshore Engineer, 2010. https://www.oedigital.com/amp/news/460109-bucking-the-trend
- [P20] Griffiths et al.: Improved stability design of subsea pipelines on mobile seabeds: learnings from the STABLEpipe JIP. ASME OMAE2018 (repository record). https://research-repository.uwa.edu.au/en/publications/improved-stability-design-of-subsea-pipelines-on-mobile-seabeds-l/
- [P21] Castorone extends the threshold for deepwater, large-diameter pipelay. Offshore magazine, 2013. https://www.offshore-mag.com/vessels/article/16761539/castorone-extends-the-threshold-for-deepwater-large-diameter-pipelay
- [P22] McDermott DLV 2000 (vessel profile). Ship Technology. https://www.ship-technology.com/projects/mcdermott-dlv-2000/
- [P23] Aegir (reference page). Ulstein. https://ulstein.com/references/aegir
- [P24] Reel-lay vessel launch report. Ship Technology, 2019. https://www.ship-technology.com/news/reel-lay-vessel-subsea
- [P25] Tavakoli: Static Analysis for Offshore Pipelay using software OrcaFlex: Development and Validation of a Model (master's thesis record). Università di Bologna, 2025. https://amslaurea.unibo.it/id/eprint/36480/
- [P26] OrcaFlex help: Supports, Data. Orcina. https://www.orcina.com/webhelp/OrcaFlex/Content/html/Supports,Data.htm
- [P27] OrcaFlex examples: Pipelay and recovery. Orcina. https://www.orcina.com/resources/examples/?key=e
- [P28] New DNV recommended practice supports offshore hydrogen pipelines development. World Pipelines, 2026. https://www.worldpipelines.com/regulations-and-standards/26032026/new-dnv-recommended-practice-supports-offshore-hydrogen-pipelines-development/amp/
- [P29] Joint industry project: CO2 Safe and Sour. DNV. https://www.dnv.com/group/joint-industry-projects/hydrogen-sulfide-challenges-in-carbon-dioxide-pipelines-co2-safe-and-sour/
- [P30] Joint industry project: mitigation of pressure containment capacity reduction caused by compressive true force. DNV. https://www.dnv.com/group/joint-industry-projects/mitigation-of-pressure-containment-capacity-reduction-caused-by-compressive-true-force-preshot/
- [P31] Pipeline installation: new JIP to unlock significant cost savings (press release). DNV, 2018. https://www.dnv.com/news/2018/pipeline-installation-dnv-gl-new-jip-to-unlock-significant-cost-savings-118250/
- [P32] Birk: Freespan remediation of a gas trunk line in a tidal national park (abstract). Pipeline Technology Conference, 2020. https://www.pipeline-conference.com/abstracts/norpipe-freespan-remediation-german-wadden-sea
- [P33] Global buckling (training course page). DNV. https://www.dnv.com/training/global-buckling-10054
- [P34] Sesam software for mooring and risers (lists the VIV module for free-span pipelines). DNV. https://www.dnv.com/services/sesam-software-for-mooring-and-risers-2310/

## Located, not read (kept out of conclusions)

- Contractor data sheets (PDF): S-lay vessel specification sheet 2026 (https://www.allseas.com/sites/d10p.allseas.com/files/2026-02/solitaire-vessel-specs-2026_0.pdf); S-lay/J-lay and reel-lay vessel data sheets 2025 on subsea7.com. Contractor web pages for Saipem, TechnipFMC (access refused), Subsea7 (challenge page), McDermott, Heerema, Allseas ("not found").
- Pipeline and riser loss of containment 2001 to 2020, seventh edition, Energy Institute, December 2024: access refused; priced report.
- UK regulator guidance on pipeline anchor hazards (PDF) and a consultancy paper on a 2007 anchor-strike assessment (PDF).
- Probabilistic lateral buckling Python package (package page failed to load; repository "not found") and two general pipeline Python packages: search summaries only.
- Route and span analysis software from a geotechnical contractor (version 3.0 announced 2015): article page returned navigation only.
- Trade article on free-span assessment (Pipeline & Gas Journal, April 2021): preview only.
- Orcina "OrcaFlex for pipelay" brochure and contact-modelling article (PDFs).
- DNV-RP-F104 (CO2 pipelines) and DNV-RP-F123 (hydrogen) catalogue pages; subsea toolkit page.
- Remaining OMAE2024 pipeline abstracts (lined pipe, reeling); all OMAE2025 pipeline sessions (gallery listing did not render).
- University repository records on pipe-soil and stability (two Southampton items, one open-archive item).

## Counts and acceptance-criteria status

- Sources read: **34**. Located, not read: **about 18 documents** in 10 groups.
- Acceptance criteria: five-plus recent OMAE/ISOPE/OPT papers: met for OMAE only (five at abstract level); contractor and vessel matrix: **partly met** (four verified rows from secondary sources, one unverified, TechnipFMC missing); tool capability matrix: **partly met** (OrcaFlex and StableLines verified; SAGE Profile and ABAQUS not verified on a vendor page); JIP landscape: **partly met** (SAFEBUCK documented; HOTPIPE and POSIT not confirmed); convergence and divergence: met.
- Not done: ISOPE, OTC, OPT, DOT and pigging-association proceedings; the four trade journals as systematic sources; line pipe manufacturers; cathodic protection tools; pre-commissioning practice; AutoPIPE, Caesar II, ANSYS, ABAQUS and 3DEXPERIENCE documentation; PipeSoil; published benchmarks or round robins (only the lay-tool comparison in [P25] was found); regulator incident alerts; open data sets.

a second pass on the items in this section is recommended before that conversion.
