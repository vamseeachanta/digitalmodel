> Source: research note on the issue, sources checked 2026-10-11

# Hydrodynamics industry practice survey

Hydrodynamics industry practice survey, 2022 to 2026.

- Every source was checked on **2026-10-11**. Sources are numbered `[Hnn]` and listed in section 11 with title, publisher, year and URL.
- **Read** = the page was opened and the statement taken from it. **Located, not read** (section 12) = found but not openable as text. Statements seen only in a search-result summary are tagged *(unverified)* and are not used in conclusions.
- Most conference papers were read at abstract level (the ASME conference's public session gallery, open-access journals, preprint servers and institutional repositories). Abstracts often give no numbers; where that is the case it is said.
- Extends, without repeating, the academic list on #1799 (keys cited as `#1799:Hnn`) and the benchmark-hull register on #1839 (rows cited as `#1839:On`).
- One naming point for anyone following links: the US wind-energy laboratory's web hosts did not resolve on the day, and a current open-source project page names the laboratory "National Laboratory of the Rockies" [H23]. Older links to its former domain may need updating.

No paid-standard text is used. The vendor matrix is limited to what the vendor pages state.

## 1. Tool landscape (what's used where)

| Task | Tool class | Tools named in the public record | Source |
|---|---|---|---|
| First-order diffraction/radiation (added mass, damping, excitation, motion transfer functions) | Frequency-domain panel method | WAMIT; Wadam (run from HydroD); Aqwa; OrcaWave; HydroStar; DIODORE; open-source NEMOH, Capytaine, HAMS | [H18][H19][H9][H17][H20][H12][H11][H14][H16] |
| Mean and slow-drift loads, sum-frequency loads | Second-order quadratic transfer functions (QTF), Newman-type approximations | WAMIT v6S "complete second-order" in bichromatic, bidirectional waves; Wadam mean drift plus "option to solve the full second-order problem"; OrcaWave reports QTFs; NEMOH v3 QTF module; time-domain use in OpenFAST with full QTFs or the Standing et al. extension of Newman's approximation | [H18][H19][H17][H11][H22] |
| Slender members and viscous effects | Morison strip theory, hybrid with potential flow, tuned drag | Wadam panel plus linearised Morison beam model; OpenFAST potential-flow, strip-theory and hybrid options with added viscous drag; frequency-dependent drag proposals | [H19][H22][H2][H28] |
| Time-domain coupled floater, mooring, riser | Convolution or state-space radiation with line dynamics | OpenFAST; Sesam DeepC on Simo/Riflex; OrcaFlex taking OrcaWave results; QBlade-Ocean; DeepLines Wind; Flexcom (named as participant code) | [H22][H37][H17][H4][H3] |
| Extreme, strongly non-linear events (wave impact, green water, air gap, focused waves) | Navier-Stokes CFD and hybrid methods, model tests | Open-source and commercial CFD named in blind tests; a CFD-based rigid-body solver used with basin data; multi-fidelity screening to cut CFD cost | [H30][H24][H25] |
| Interchange and comparison of coefficient sets | Converters | BEMRosetta reads and writes more than a dozen solver formats | [H21] |

**Who reports using what (verified examples).** An academic three-code comparison for an ultra-deep-water FPSO at 2,100 m used Aqwa, WAMIT and an in-house code [H9]. The 2024 OC6 Phase IV exercise had 19 models from 17 partners, sorted by Morison versus potential-flow hydrodynamics, with some adding second-order sum- and difference-frequency forces [H3]. An operator-affiliated preprint describes a machine-learning motion predictor deployed on an operating turret-moored unit [H29].

## 2. Method convergence vs. divergence

**Converging**
1. **Linear potential flow is a solved, commoditised step.** Open and commercial panel codes are compared on the same cases as a matter of routine: Capytaine against NEMOH, HAMS and WAMIT on a truncated cylinder, a cylinder with heave plate and multiple bodies [H6]; NEMOH v3 against WAMIT, DIODORE and HydroStar on four cases including a semi-submersible [H12]; a CC BY 4.0 data set comparing NEMOH with published WAMIT results for the same semi-submersible reports good first-order agreement [H13].
2. **A common interchange format.** WAMIT-style coefficient files are the de facto exchange format: the time-domain wind code takes them from "a separate frequency-domain panel code such as WAMIT" [H22], the comparison data set converts NEMOH output to that format [H13], and the converter tool lists it for both reading and writing [H21].
3. **Second-order loads are no longer optional for moored floaters.** Full QTF capability is advertised by the commercial codes [H18][H19][H17] and, since January 2023, exists in an open code [H11].
4. **Validation by collaboration.** The wind-energy code comparison series is the main public source of cross-code evidence, now continuing as a new task with a first phase on viscous loads *(phase plan seen in search summary only; the task page returned a site error)*.

**Diverging or unresolved**
1. **Low-frequency response of semi-submersibles.** Mid-fidelity tools "consistently underpredicted" non-linear low-frequency surge and pitch loads and resonant motions; the OC6 Phase I campaign found that models with better load predictions "do not necessarily produce a more accurate motion response in a moored configuration" [H1]. Fixes differ between groups: modified viscous-drag modelling with one coefficient set [H2]; adoption of that method in another code with "significant improvement" in surge and pitch in irregular waves but smaller gains with wind [H4]; non-linear wave kinematics, wave stretching and MacCamy-Fuchs corrections in a strip-theory model [H27]; frequency-band-dependent drag coefficients [H28]. There is no single agreed recipe.
2. **Second-order potential beyond standard perturbation theory.** A 2026 preprint generalises the Pinkster approximation to fully non-linear waves and replaces first-order motions by total motions in the quadratic term, reporting "significant improvements over standard second-order radiation-diffraction theory" for a moored ship [H26]. The open-code second-order solution "deviates more than the first-order results" from the commercial reference [H13].
3. **Extreme-event CFD practice.** In the blind focused-wave study with two taut-moored bodies, "Navier-Stokes and hybrid methods were generally more accurate", all methods predicted heave amplitude reasonably, but solutions varied widely even among similar methods, which the authors read as a "need for standardisation" [H30].
4. **Coupled-code scatter.** A code-to-code comparison on an industry-designed floating wind concept showed that "coupled interactions can produce large differences in motion and load responses between otherwise similar simulation frameworks" [H4]; dynamic tension in the upwind line showed "significant dispersion" across 19 models [H3].
5. **Formulation choices inside panel codes** (source versus direct integral equation, Green-function tabulation density) still matter for thin and slender geometry, which is why a direct formulation and user-set table density were added to an open code in 2024 [H6].

## 3. Key state-of-art papers (2024+)

Conference papers (ASME OMAE, abstract level):
1. **Case studies of panel-method accuracy with an open code, OMAE2024**: four-code benchmark; adds a direct formulation and adjustable Green-function interpolation; abstract gives relative strengths, no numbers [H6].
2. **Recent floating wind modelling enhancements, OMAE2024 (presentation only)**: loads evaluated at the displaced floater position, several wave-stretching methods, fully non-linear hydrostatics, multiple large-volume bodies, large-yaw theory, constrained extreme-wave generation, and a tight-coupling scheme reported 10 to 100 times faster [H8].
3. **Three-code FPSO response study, OMAE2023**: Aqwa, WAMIT and an in-house code at 2,100 m depth, ballast and loaded, five headings, with a planned basin test; abstract reports no quantitative differences [H9].
4. **Second-order difference- and sum-frequency loads in an open potential-flow solver, OMAE2022**: full QTF from a near-field quadratic part and an indirect-method potential part; verified on a bottom-mounted cylinder and floating hemisphere and compared with a commercial code on a wind semi-submersible [H7].
5. **Sensors and digital twin for mooring integrity, OMAE2025**: reported under #1807; relevant here as a use of motion data plus model.

Journal and preprint papers:
6. **OC6 Phase IV, Wind Energy Science 2024**: 1:43 basin test, 19 models; motions and tower-base moment "reasonably" estimated; mooring dynamic load scatter [H3].
7. **Validation of a hydrodynamic module against two basin campaigns and two frameworks, Wind Energy Science 2024**: three floater concepts; the viscous-drag method of [H2] improves irregular-wave surge and pitch; code-to-code differences on an industry-designed concept [H4].
8. **Frequency-domain arrays with shared lines, Wind Energy Science 2025**: strip-theory limits (no radiation or scattering) visible in pitch and roll spectra against the time-domain farm model [H5].
9. **Probabilistic adaptive screening for wave-impact design loads, Ocean Engineering 2026**: most probable maxima within 2 to 15 % of brute-force Monte Carlo using about 1 to 3 % of the high-fidelity simulation time, on non-linear waves, vertical bending, green water and slamming [H25].
10. **Directional focused wave groups on a floating wind unit, preprint 2024**: harmonic separation in basin and CFD; stronger sea states raise odd harmonics in surge and pitch and reveal a pitch sub-harmonic; spreading changes both odd and even super-harmonics [H24].
11. **Motion-explicit loads in fully non-linear waves, preprint 2026**: see 2, item 2 [H26].
12. **Morison-model improvements validated on a 1:70 basin campaign, preprint 2025** [H27] and **frequency-dependent drag, preprint 2025** [H28]: see 2, item 1.
13. **Open-source panel codes for wave-energy geometries, J. Phys. Conf. Ser. 2024**: compares coefficients and cost for three open codes; abstract states aim, not results [H10].
14. Anchors: **OC6 Phase I, 2020** [H1] and **OpenFAST low-frequency improvements, Renewable Energy 2022** [H2].

Against the acceptance criterion: four OMAE papers summarised at abstract level plus ten journal or preprint items. **No ISOPE or OTC hydrodynamics paper could be opened** (no public abstract pages found); Marine Structures, Applied Ocean Research and the ASME journal are not represented by a paper read here.

## 4. Vendor capability matrix

| Tool (vendor) | Version / date on page | First order | Second order | Multi-body | Other stated features | Not stated on the page read | Source |
|---|---|---|---|---|---|---|---|
| WAMIT | Version 7 | Yes; higher-order solution method since v6 | v6S: complete second-order analysis in bichromatic and bidirectional waves; mean drift | "more flexible inputs for multi-body analysis" | Parallel processing; internal tanks coupled with vessel dynamics; CAD link; licensed to "more than 200 industrial and research organizations" | Generalised modes (not on this page) | [H18] |
| Wadam / HydroD (DNV) | not given | 3D radiation-diffraction on a panel model plus linearised Morison on a beam model | Mean drift transfer functions; full second-order option | not stated | Wave-current interaction up to about 3 to 4 knots; automatic load transfer to structural analysis; stability in HydroD | Forward-speed solver | [H19] |
| OrcaWave (Orcina) | current web help | Added mass, damping, load and displacement transfer functions | QTF results | bodies (plural) | Multi-threaded; results import straight into OrcaFlex; Python and MATLAB API; text model files | Irregular-frequency removal, QTF method options (other help pages not read) | [H17] |
| HydroStar (BV) | 8.4.0, November 2025 | 3D diffraction/radiation | not stated on page | Yes, including side-by-side | Forward speed; tank sloshing coupled with ship motion; generalised modes | QTF | [H20] |
| Aqwa (Ansys) | not given | Page read gives only a general statement on hydrodynamic assessment | not stated | not stated | Used as a comparison code in academic work [H9] | Almost everything | [H36] |
| Ariane (BV) | 8.2.8, July 2026 | consumes coefficients; mooring tool | drift-load models | 6-degree-of-freedom multi-body | covered under #1807 | n/a | #1807 note |

Rows are limited to what the page read actually states; an empty cell is not a claim that the capability is absent.

## 5. Open-source landscape (NEMOH, Capytaine)

| Code | Licence | What the project page states | Limits stated | Source |
|---|---|---|---|---|
| NEMOH v3.0 | GPL v3 | Released January 2023; irregular-frequency removal; new linear solvers; QTF module for sum and difference frequencies; developers "believe it is the only open-source BEM software with a second-order module"; verified against three commercial codes on four cases | Second-order agreement weaker than first-order in an independent data set | [H11][H12][H13] |
| Capytaine v3.0.0 (August 2026) | Apache 2.0 since v3 (docs CC BY-SA 4.0) | Python rewrite of NEMOH v2; multiple bodies and generalised degrees of freedom; finite or infinite depth; lid-based irregular-frequency removal; hydrostatics; Kochin function; direct or indirect formulation; optional HAMS Green function; OpenMP; NetCDF output | Forward speed approximate and single-body only; non-neutrally-buoyant bodies partial; no second-order feature listed | [H14][H15] |
| HAMS | Apache 2.0 | Fortran 90 frequency-domain panel code; triangular and quadrilateral panels; OpenMP; semi-submersible example | Linear analysis; no releases tagged | [H16] |
| OpenFAST hydrodynamics module | not read | Potential flow from external coefficients, strip theory, hybrid; convolution or state-space radiation; QTF or Newman-type difference-frequency loads | Internal wave kinematics only to still-water level in the documentation read (stretching added per [H8]) | [H22][H8] |
| WEC-Sim | not stated on page | Time-domain multi-body simulator on MATLAB/Simulink and Simscape Multibody; rigid and flexible bodies with generalised modes | Requires commercial MATLAB toolchain | [H23] |
| BEMRosetta | GPL 3.0 | Viewer and converter for coefficients, case files and meshes across WAMIT, Aqwa, OrcaWave, HAMS, NEMOH, Capytaine, HydroStar, DIODORE and others; mesh healing; hydrostatics | No embedded solver (launches NEMOH and HAMS) | [H21] |
| QBlade-Ocean | not read | Hydrodynamic module validated against two basin campaigns and two frameworks | n/a | [H4] |

Assessment: for first-order work the open stack is at parity for the cases published; for second-order work one open code exists with published verification; forward speed, hydroelastic load transfer and tank coupling remain areas where the commercial pages state capability and the open pages do not.

## 6. Benchmarks, code comparisons and blind tests since 2018

| Exercise | Type | Headline | Source |
|---|---|---|---|
| OC6 Phase I (2020) | Basin validation, component loads | Low-frequency load and motion underprediction traced to non-linear hydrodynamics; better loads do not guarantee better moored motions | [H1] |
| OC6 Phase IV (2024) | Basin validation, 19 models | Good aerodynamics, reasonable motions, scattered upwind-line dynamics | [H3] |
| Blind focused-wave series 2 (2021) | Blind test, low to high fidelity | Wide scatter even among like methods; a mid-fidelity model with CFD-informed viscous terms matched the better Navier-Stokes solvers | [H30] |
| Blind-test catalogue | Five series: fixed FPSO impact, two floating-structure cases, flexible membrane, sloshing | Cases listed; dates and data terms not on the page | [H31] |
| Four-code panel benchmark (2024) | Code-to-code | Formulation matters for thin and slender bodies | [H6] |
| NEMOH v3 verification (2023) and independent comparison (2025) | Code-to-code | First-order good; second-order follows trends with larger deviation | [H12][H13] |
| Three-code FPSO comparison (2023) | Code-to-code plus planned test | No numbers in abstract | [H9] |

See #1839 for geometry and reference coefficient files of the semi-submersible and spar used in several of these (`#1839:O1`, `O2`, `O3`).

## 7. Problem areas and incident-driven changes of practice

- **Air gap and horizontal wave impact on column-stabilised units.** On 30 December 2015 a wave struck the forward box girder of a semi-submersible in survival condition, breaking 17 windows; one person died and four were injured [H32]. The regulator's investigation found "weaknesses in how wave force calculations are used when designing mobile units" and later accepted that the regulations had been "too vague with regard to the applicable calculation methodology" for horizontal wave forces [H32]. Several air-gap calculations existed for the unit and gave different results [H33].
- **Resulting change.** The class society published a new air-gap prediction guideline in June 2016, asked owners of about 100 classed semi-submersibles for updated air-gap documentation, required windows in exposed zones to be removed before winter where a positive air gap could not be shown, and allowed operational limits as an interim measure [H34]. The two guidelines (air gap; horizontal wave impact loads) have since been retired and replaced by recommended practices DNV-RP-0613 and DNV-RP-0614 [H35].
- **Low-frequency response of floating wind semi-submersibles**: section 2, item 1.
- **Reproducibility of CFD for wave impact**: section 2, item 3. Further joint-industry CFD verification work was seen only in a reference list *(unverified)*.

## 8. Openly available data and model-test resources

- Reference coefficient sets and meshes: see #1839. Added here: a CC BY 4.0 set of first- and second-order coefficients for a wind semi-submersible at headings 0 to 180 degrees in a simulation-ready format [H13]; a published verification data set for the open second-order solver (GPL v3) [H12].
- Blind-test case descriptions for focused-wave interaction with fixed and floating bodies [H31]; the published results paper [H30].
- Basin campaigns behind the wind collaborations (1:43 and 1:70 scale) are described in open papers [H3][H27]; the data portals themselves could not be opened, so access terms are unconfirmed (section 12).

## 9. Trends, each tied to a source

| Trend | Evidence |
|---|---|
| Floating wind is setting the validation agenda for semi-submersible hydrodynamics | [H1][H2][H3][H4] |
| Open second-order capability and open-versus-commercial parity checks | [H11][H12][H13][H6] |
| Mid-fidelity models tuned with physics-based viscous corrections rather than moving wholesale to CFD | [H2][H27][H28][H30] |
| Multi-fidelity statistics for extreme non-linear loads | [H25] |
| Machine-learning surrogates moving from papers to an operating asset | [H29] |
| Non-linear extensions of second-order theory | [H26] |
| Faster coupled simulation (tight coupling, 10 to 100 times) | [H8] |
| Class guidance changed by an incident (air gap, wave impact) | [H32][H34][H35] |

## 10. Gap analysis for a small consultancy with strong open analysis capability

| Area | Verdict | Basis |
|---|---|---|
| First-order diffraction/radiation, hydrostatics, motion transfer functions for stationary floaters | **Open methods adequate** | Repeated code-to-code agreement with commercial references [H6][H12][H13]; permissive licences for two of three codes [H14][H16] |
| Difference-frequency (slow-drift) QTFs | **Open methods usable with independent checks** | One open implementation, verified by its authors against three commercial codes [H12], with larger deviation than first order in an independent comparison [H13]; note the GPL v3 licence for anything redistributed |
| Coupled time-domain floater and mooring analysis | **Open methods adequate for floating wind; evidence thinner for oil and gas units** | Open wind codes are the best-validated public tools [H1][H3]; no equivalent public validation for FPSO-type units was found |
| Forward-speed seakeeping, hydroelastic load transfer to structural models, tank sloshing coupling, side-by-side operations | **Licensed tools largely unavoidable** | Stated on commercial pages [H18][H19][H20]; stated as partial or absent on open pages [H15] |
| Extreme events (air gap, wave impact, green water) | **Model tests or CFD needed; proprietary basin data unavoidable for design values** | Incident-driven class requirements [H34][H35]; blind-test scatter [H30] |
| Independent verification of supplied coefficient sets and transfer functions (format conversion, re-run on an open code, documented differences) | **Differentiated offering plausible** | Tooling exists [H21]; public comparisons show where differences arise [H6][H13]; ties directly to the registry in #1839 |
| Calibrated mid-fidelity models for low-frequency response (documented viscous tuning against public basin data) | **Differentiated offering plausible** | The problem is documented and unresolved [H1][H2][H4][H28]; public data and open codes exist |
| Surrogate models for operations (heading, motions, mooring response) | **Plausible, but needs asset data** | Demonstrated by an operator-affiliated team with about 10^6 training samples [H29] |

## Sources read

- [H1] Robertson et al.: OC6 Phase I: Investigating the underprediction of low-frequency hydrodynamic loads and responses of a floating wind turbine. J. Phys.: Conf. Ser. 1618, 032033, IOP, 2020. https://iopscience.iop.org/article/10.1088/1742-6596/1618/3/032033
- [H2] Wang, Robertson, Jonkman, Yu: OC6 phase I: Improvements to the OpenFAST predictions of nonlinear, low-frequency responses of a floating offshore wind turbine platform. Renewable Energy 187, 282-301, Elsevier, 2022 (bibliographic record with abstract). https://ideas.repec.org/a/eee/renene/v187y2022icp282-301.html
- [H3] Bergua et al.: OC6 project Phase IV: validation of numerical models for novel floating offshore wind support structures. Wind Energy Science 9, 1025-1051, Copernicus, 2024. https://wes.copernicus.org/articles/9/1025/2024/
- [H4] Behrens de Luna et al.: Quantifying the impact of modeling fidelity on different substructure concepts for floating offshore wind turbines, Part 1. Wind Energy Science 9, 623-649, 2024. https://wes.copernicus.org/articles/9/623/2024/
- [H5] Hall, Carmo, Lozon: Frequency-domain modeling of floating wind arrays with shared mooring lines. Wind Energy Science 10, 3027-3043, 2025. https://wes.copernicus.org/articles/10/3027/2025/
- [H6] Ancellin, Nguyen: Case Studies of BEM Solver Accuracy With the Open-Source Code Capytaine (submission 127995). ASME OMAE2024 session gallery. https://omae.secure-platform.com/a/solicitations/214/sessiongallery/15622/application/127995
- [H7] Kurnia, Ducrozet, Gilloteaux: Second Order Difference- and Sum-Frequency Wave Loads in the Open-Source Potential Flow Solver NEMOH (submission 79163). ASME OMAE2022 session gallery. https://omae.secure-platform.com/a/solicitations/165/sessiongallery/10383/application/79163
- [H8] Jonkman: Recent Floating Wind Modelling Enhancements in OpenFAST (submission 123909). ASME OMAE2024 session gallery. https://omae.secure-platform.com/a/solicitations/214/sessiongallery/15604/application/123909
- [H9] Sridhar, Sannasiraj, Sundaravadivelu: Hydrodynamic Response Analysis of Ultra-Deep Water FPSO System Using Ansys Aqwa, Wamit, and HydRA (submission 100687). ASME OMAE2023 session gallery. https://omae.secure-platform.com/a/solicitations/190/sessiongallery/13822/application/100687
- [H10] Raghavan, Lavidas, Metrikine: Comparing Open-Source BEM solvers for analysing wave energy converters. J. Phys.: Conf. Ser. 2647, 072002, 2024 (repository record). https://tethys-engineering.pnnl.gov/publications/comparing-open-source-bem-solvers-analysing-wave-energy-converters
- [H11] Release of NEMOH v3.0.0. LHEEA, Centrale Nantes, 2022/2023. https://lheea.ec-nantes.fr/lheea/news-and-events/release-of-nemoh-v3-0-0
- [H12] Kurnia, Ducrozet: NEMOH: Open-source boundary element solver for computation of first- and second-order hydrodynamic loads in the frequency domain (data set). Mendeley Data, 2023. https://data.mendeley.com/datasets/k9v4hjb5xj/1
- [H13] Al-Ghuwaidi, Pribadi: Benchmark study of NEMOH vs WAMIT for the OC4-DeepCwind semi-submersible FOWT (data set, CC BY 4.0). Zenodo, 2025. https://zenodo.org/records/16940342
- [H14] Capytaine documentation, version 3.0.0. 2026. https://capytaine.org/stable/
- [H15] Capytaine documentation: Features. 2026. https://capytaine.org/stable/features.html
- [H16] HAMS repository. https://github.com/YingyiLiu/HAMS
- [H17] OrcaWave help: Introduction. Orcina. https://www.orcina.com/webhelp/OrcaWave/Content/html/Introduction.htm
- [H18] WAMIT home page. WAMIT, Inc. https://www.wamit.com/
- [H19] Frequency domain hydrodynamic analysis of stationary vessels: Wadam. DNV. https://www.dnv.com/services/frequency-domain-hydrodynamic-analysis-of-stationary-vessels-wadam-2412/
- [H20] HydroStar software page. Bureau Veritas Marine & Offshore, 2025. https://marine-offshore.bureauveritas.com/hydrostar-software-powerful-hydrodynamic
- [H21] BEMRosetta repository. https://github.com/BEMRosetta/BEMRosetta
- [H22] OpenFAST documentation: HydroDyn user guide. https://openfast.readthedocs.io/en/main/source/user/hydrodyn/index.html
- [H23] WEC-Sim documentation. https://wec-sim.github.io/WEC-Sim/main/index.html
- [H24] Aliyar et al.: Directional focused wave group response of a Floating Wind Turbine: Harmonic separation in experiment and CFD. arXiv 2412.16718, 2024. https://arxiv.org/abs/2412.16718
- [H25] van Essen, Seyffert: Design loads for wave impacts: introducing the Probabilistic Adaptive Screening (PAS) method. Ocean Engineering 357, 125440, 2026 (preprint page). https://arxiv.org/abs/2511.23156
- [H26] Dermatis et al.: A time-domain approach for motion-explicit evaluation of loads on floating structures in fully nonlinear waves. arXiv 2604.09247, 2026. https://arxiv.org/abs/2604.09247
- [H27] Sarker et al.: Hydrodynamic Modeling Improvements for Floating Offshore Wind Turbines with Validation Results. arXiv 2508.01093, 2025. https://arxiv.org/abs/2508.01093
- [H28] Sakif et al.: Enhanced Hydrodynamic Modeling of Offshore Wind Turbines using Morison's Equation with Frequency-Dependent Coefficients. arXiv 2509.12536, 2025. https://arxiv.org/abs/2509.12536
- [H29] Morris-Thomas, Martens: An application of machine learning to the motion response prediction of floating assets. arXiv 2506.15713, 2025. https://arxiv.org/abs/2506.15713
- [H30] Ransley et al.: Focused wave interactions with floating structures: a blind comparative study. Proc. ICE Engineering and Computational Mechanics 174(1), 46-61, 2021 (repository record). https://researchportal.bath.ac.uk/en/publications/focused-wave-interactions-with-floating-structures-a-blind-compar/
- [H31] CCP-WSI data repository: test cases. https://www.ccp-wsi.ac.uk/data_repository/test_cases
- [H32] Investigation of incident with fatal consequences on a mobile unit (investigation summary page). Havtil, 2016. https://www.havtil.no/en/supervision/investigation-reports/2016/cosl-drilling---coslinnovator---investigation-of-incident-with-fatal-consequences/
- [H33] PSA cites air gap inconsistencies in rig damage study. Offshore magazine, 2016. https://www.offshore-mag.com/vessels/article/16770375/psa-cites-air-gap-inconsistencies-in-rig-damage-study
- [H34] Modifications or operational limitations for a limited number of rigs (press release). DNV, 2016. https://www.dnv.com/news/2016/modifications-or-operational-limitations-for-a-limited-number-of-rigs-77136/
- [H35] Offshore technical guidance documents. DNV, 2026. https://www.dnv.com/maritime/offshore-classification/technical-guidance-otg/
- [H36] Aqwa product page. Ansys (Synopsys), 2026. https://ansys.synopsys.com/products/structures/ansys-aqwa
- [H37] Sesam software for mooring and risers. DNV, 2026. https://www.dnv.com/services/sesam-software-for-mooring-and-risers-2310/

## Located, not read (kept out of conclusions)

- International code-comparison task page for the current phase series (viscous loads 2024, floater structural dynamics 2025 to 2026, farm aerodynamics 2026 to 2027): site error on the day; plan seen in search summary only. https://iea-wind.org/task-56-oc7-project-offshore-code-comparison-collaboration-7/
- US wind laboratory project and publication pages for the OC series and the 1:70 control co-design basin campaign: hosts did not resolve.
- ITTC 2024 Ocean Engineering and Seakeeping committee reports: not searched to a readable page.
- University data set for a first floating-wind comparative study at 1:70 scale: "not found" at the handle returned by search.
- Blind focused-wave series 3 paper (Int. J. Offshore and Polar Engineering 30(1), 2020): repository records only.
- Raghavan et al. 2022 three-code comparison; a university repository study of two open codes against WAMIT with incomplete and overlapped panels: search summaries only.
- Kurnia and Ducrozet journal paper on bi-directional QTFs for wind platforms; companion CFD paper on difference-frequency excitation (Sustainability, 2021).
- DNV-RP-0613 and DNV-RP-0614 texts; ABS air-gap and wave-impact guidance (2020).
- OMAE papers on CFD credibility for green water and breaking-wave impact, and on a joint-industry CFD verification effort: seen in a reference list only.
- MDPI J. Mar. Sci. Eng. 13(12) 2407 (2025) two-solver comparison (deviations "below 1%" for coefficients *(unverified)*): access refused.
- OrcaWave theory/validation pages, HydroD and time-domain seakeeping solver pages, Aqwa technical documentation.
- Any ISOPE or OTC hydrodynamics paper; any paper from Marine Structures, Applied Ocean Research or J. Offshore Mechanics and Arctic Engineering.

## Counts and acceptance-criteria status

- Sources read: **37**. Located, not read: **about 20 documents** in 12 groups.
- Acceptance criteria: five-plus recent OMAE/ISOPE papers summarised: **partly met** (four OMAE at abstract level, no ISOPE; ten further journal or preprint items); vendor capability matrix with five-plus tools: met (six rows, limited to what vendor pages state); open-source landscape: met; convergence and divergence: met.
- Not done: practitioner technical notes from the software vendor and class societies ("open practice" papers and technical bulletins named in the issue) were not found as open pages; no ranking of tools is offered because none can be sourced.
