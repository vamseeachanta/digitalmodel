# Public reference sources for floating offshore wind semi-submersible analysis

Research date: 2026-10-13; public sources only; [issue 2423](https://github.com/vamseeachanta/digitalmodel/issues/2423).

## Scope and questions

1. Which public reports and repositories define the reference turbines and semi-submersible platforms, what do they contain, and what reuse rights do their sources state?
2. Which publicly accessible standards catalogues, previews and open publications identify design load cases? Paid documents are considered by clause number and title only.
3. Which public turbine controller sources are available, and what licence statements accompany them?
4. Which public turbulent-wind generators are available, under which stated licences, and what do they produce?
5. What public evidence supports each identifier in the existing worked-example worksheet, and where does a complete case-definition reference remain unresolved?

This is a source-selection note. It does not issue a design basis, select environmental parameters, qualify a controller, establish equivalence between model implementations, or report calculated engineering results. No model, simulation or solver-result file was opened. The scope is the public documentary evidence needed before those decisions can be made.

## Method

Public pages were searched and opened with the web search/fetch tool. Network access was established at https://www.iea.org/, whose returned page title was **IEA - International Energy Agency**. The earlier request to the laboratory wind homepage failed; that individual failure did not establish loss of network access. The proof page is source [1], retained only as method evidence.

The issue body, every issue comment and its timeline were read through the GitHub CLI. The timeline identified merged [pull request 2436](https://github.com/vamseeachanta/digitalmodel/pull/2436); its description, comments and changed-file list were read. The worksheet was then read directly at `docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md`. Its proposed-case table contains eleven rows, matching the expected count. The repository documentation index and an existing hydrodynamics research note established the placement used here.

Twenty-one distinct external pages or documents were successfully opened, including the network proof. All twenty-one are cited below; the locally read worksheet is the additional source [22]. Six distinct fetch targets failed and are recorded separately. Searches alone do not count as opened sources. Repeated views of the same document do not increase the count. PDF text was read without downloading model archives, installing engineering software or rendering entire reports. Search-result summaries were used for discovery only.

Search terms included: `IEA 15 MW reference wind turbine VolturnUS-S reference platform report github license`; `OC4 DeepCwind semisubmersible definition report load cases free decay`; `NREL 5 MW reference turbine github NREL 5MW license`; `ROSCO controller license DTU basic controller license`; `IEC 61400-3-2 2025 contents design load cases 7.4`; `TurbSim PyConTurb license turbulent wind generator`; `DNV ST 0119 floating wind turbines DNV RP 0286`; `mann turbulence generator license wind`; and `floating wind design load cases parked power production open access`. Domain filters restricted follow-up discovery to the relevant publishers, documentation sites and public repositories.

Quoted extracts are deliberately short. A source number links each finding to the source register; retrieval dates appear at each evidence point. Printed report pages are used below, rather than zero-based PDF page indices. A licence label is recorded as a source statement, without an independent conclusion about legal permission for a particular redistribution. No paid-standard clause text, factors, limits or load tables are reproduced. Proposed interpretations appear only in the inference section.

## Findings

### 1. Reference turbine and platform definitions

The NREL 5-MW definition report covers the baseline turbine's aerodynamic, structural and control characteristics. Its executive summary identifies the coverage as "aerodynamic, structural, and control-system properties". The report also contains input-file appendices and a control-system source-code appendix, listed in its contents. Report reuse rights: to be checked. [2, executive summary and contents](https://docs.nlr.gov/docs/fy09osti/38060.pdf), retrieved 2026-10-13.

The OC4 semi-submersible definition supplies tower, platform, hydrodynamic, mooring and control information; the contents include "Floating Platform Structural Properties" and "Mooring System Properties". Section 1 identifies its turbine as the NREL offshore 5-MW baseline and explains that its tower and controller differ with the supporting structure. Benchmark/report reuse rights: to be checked. [3, contents and section 1](https://docs.nlr.gov/docs/fy14osti/60601.pdf), retrieved 2026-10-13.

OpenFAST's regression repository publishes turbine inputs and example cases, including the identifiers `5MW_Baseline` and `5MW_OC4Semi_WSt_WavesWN`. Its README describes "Input files for test case execution" and "Turbine models used in the regression test cases"; its displayed licence is "Apache-2.0 license". No regression-result files were opened for this survey. [4, README and licence label](https://github.com/OpenFAST/r-test), retrieved 2026-10-13.

The public abstract of the IEA 15-MW turbine report describes a "fixed-bottom monopile support structure" and a "Class IB direct-drive machine". It identifies a separate forthcoming floating-support report. Only the abstract was successfully opened for this turbine report; detailed turbine parameters are not transcribed here. Report reuse rights: to be checked. [5, abstract](https://research-hub.nlr.gov/en/publications/iea-wind-tcp-task-37-definition-of-the-iea-15-megawatt-offshore-r/), retrieved 2026-10-13.

The IEA reference-model repository lists documentation, OpenFAST and HAWC2 inputs, optimization inputs, ontology files and available CAD. The README names "OpenFAST aeroelastic model inputs" and "HAWC2 aeroelastic model inputs"; the displayed licence is "Apache-2.0 license". It links both turbine and floating-platform reports and states that controller-version information is in the controller input header. [6, README and licence label](https://github.com/IEAWindSystems/IEA-15-240-RWT), retrieved 2026-10-13.

The VolturnUS-S report defines the semi-submersible, tower and controller, including its "mooring system". Benchmark/report reuse rights: to be checked. [7, executive summary and contents](https://docs.nlr.gov/docs/fy20osti/76773.pdf), retrieved 2026-10-13.

### 2. Public standards navigation and open load-case discussion

The IEC catalogue identifies IEC 61400-3-2:2025 and describes it as replacing the earlier technical specification: "This first edition cancels and replaces IEC TS 61400-3-2, published in 2019." This is catalogue metadata, not a transcription of a design requirement. [8, catalogue](https://webstore.iec.ch/en/publication/67761), retrieved 2026-10-13.

The IEC preview exposes these clause numbers and titles: 7.4, "Design situations and load cases"; 7.4.2, "Power production (DLC 1.1 to 1.6)"; 7.4.7, "Parked (standstill or idling) (DLC 6.1 to 6.5)"; and 7.5.6, "Simulation requirements". Only contents entries are cited, with no underlying table or clause requirements reproduced. [9, contents, printed pages 2-3](https://cdn.standards.iteh.ai/samples/104665/ec829410245e45188ee32e81bd4e003f/IEC-61400-3-2-2025.pdf), retrieved 2026-10-13.

The DNV catalogue lists DNV-RP-0286, edition 2019-05 amended 2021-10, and includes "design load cases setup" among its listed subjects. Its full-document access route is a subscription. [10, catalogue](https://www.dnv.com/energy/standards-guidelines/dnv-rp-0286-coupled-analysis-of-floating-wind-turbines/), retrieved 2026-10-13.

The publisher's DNV-RP-0286 preview provides section 5, "Load cases set-up and data analysis"; 5.2, "Extreme loads – ultimate limit states"; and 5.3, "Fatigue loads – fatigue limit states". These are contents titles only, with no load combinations or acceptance criteria adopted. [11, contents, printed page 6](https://brandcentral.dnv.com/fr/gallery/10651/others/2dd1ccdad4f9405f9eee1ca4da569a44_hi.pdf), retrieved 2026-10-13.

The DNV-ST-0119 catalogue displays "Edition 2025-12" and the title "Floating wind turbines". No clause number for its load-case table was established from the page opened. [12, catalogue](https://www.dnv.com/energy/standards-guidelines/dnv-st-0119-floating-wind-turbines/), retrieved 2026-10-13.

The open platform report contains Table 12, "IEC Design Load Case Matrix", on printed page 27. Section 4 describes a selected assessment; it is not presented as the complete standard. This note reproduces no environmental numbers from that table, and none were checked against the paid standard. Worked-example reuse rights: to be checked. [7, section 4 and Table 12](https://docs.nlr.gov/docs/fy20osti/76773.pdf), retrieved 2026-10-13.

### 3. Public controller sources and stated licences

ROSCO publishes a Fortran controller and a largely Python toolset for tuning and related tasks. Its README identifies "Generic tuning of NREL's ROSCO controller" and the repository displays "Apache-2.0 license". [13, README](https://github.com/NatLabRockies/ROSCO), retrieved 2026-10-13. The licence file itself begins "Apache License" and "Version 2.0, January 2004". [14, licence heading](https://raw.githubusercontent.com/NatLabRockies/ROSCO/main/LICENSE), retrieved 2026-10-13.

The Basic DTU controller's older public repository describes a pitch-regulated, variable-speed controller and identifies its replacement GitLab location. Its licence statement is: "The Basic DTU Wind Energy controller is distributed under the GNU General Public License v3.0." The replacement location could not be opened, so this finding applies to the older repository, not an inspected current release. [15, README, licence section](https://github.com/DTUWindEnergy/BasicDTUController), retrieved 2026-10-13.

The NREL 5MW controller repository describes controllers for "various configurations for the NREL 5MW Reference Wind Turbine" and lists separately named controller builds. [16, README](https://github.com/OpenFAST/nrel-5mw-controllers), retrieved 2026-10-13. The inspected baseline source header states "Licensed under the Apache License, Version 2.0". This is file-specific licence evidence; licences of every alternative build or wrapper were not audited. [17, DISCON source header](https://raw.githubusercontent.com/OpenFAST/nrel-5mw-controllers/main/DISCON/DISCON.F90), retrieved 2026-10-13.

### 4. Public turbulent-wind generators

TurbSim's public source describes generation of a wind field in a plane normal to the mean wind direction: "This program simulates a full field of turbulent winds at points in space". [18, program header](https://github.com/OpenFAST/openfast/blob/main/modules/turbsim/src/TurbSim.f90), retrieved 2026-10-13. The containing OpenFAST repository's licence heading states "Apache License" and "Version 2.0, January 2004". [19, licence](https://raw.githubusercontent.com/OpenFAST/openfast/main/LICENSE), retrieved 2026-10-13.

The TurbSim output documentation describes component binary wind files and a text summary for HAWC-format output. The binaries contain "full-field turbulence data for the 3 wind-speed components". The same page documents scaling and mean-removal conventions; those conventions are not replaced with assumed settings in this note. [20, output documentation](https://openfast.readthedocs.io/en/v4.0.5/source/user/turbsim/output.html), retrieved 2026-10-13.

PyConTurb's publisher-supplied package description states that it generates "stochastic turbulence boxes that are constrained by one or more measured time series" and can also operate without those constraints. Its package licence field states "MIT"; its source-code section links GitLab. The documentation fetches failed, so supported export formats are not asserted here. This is package-metadata evidence, not an audit of the source distribution. [21, package description and licence field](https://pypi.org/project/pyconturb/), retrieved 2026-10-13.

### 5. Mapping to the existing eleven-row worksheet

The locally inspected worksheet calls itself a "proposed case-definition worksheet" and leaves marked inputs unresolved. Its eleven identifiers appear below exactly as written. The mapping records documentary support and its limits; it does not replace the worksheet. [22, introduction and Proposed cases](https://github.com/vamseeachanta/digitalmodel/blob/50cd3f70645d6470df2bcfb6e12bd40fd69d3ded/docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md), retrieved 2026-10-13.

| Repository row | Public evidence and extent of support |
|---|---|
| Statics | [7](https://docs.nlr.gov/docs/fy20osti/76773.pdf), retrieved 2026-10-13, section 3.1, Static Surge-Sway Offsets, printed page 20: "equilibrium". Partial: mooring equilibrium during imposed offsets; no complete whole-system convergence prescription. |
| Free decay: surge | [7](https://docs.nlr.gov/docs/fy20osti/76773.pdf), retrieved 2026-10-13, section 3.2, Figure 13, printed page 21: "surge and sway". Direction support. |
| Free decay: sway | [7](https://docs.nlr.gov/docs/fy20osti/76773.pdf), retrieved 2026-10-13, section 3.2, Figure 13, printed page 21: "surge and sway". Direction support. |
| Free decay: heave | [7](https://docs.nlr.gov/docs/fy20osti/76773.pdf), retrieved 2026-10-13, section 3.2, Figure 14, printed page 22: "heave free-decay". Direction support. |
| Free decay: roll | [7](https://docs.nlr.gov/docs/fy20osti/76773.pdf), retrieved 2026-10-13, section 3.2, Figure 15, printed page 22: "pitch and roll". Direction support. |
| Free decay: pitch | [7](https://docs.nlr.gov/docs/fy20osti/76773.pdf), retrieved 2026-10-13, section 3.2, Figure 15, printed page 22: "pitch and roll". Direction support. |
| Free decay: yaw | [7](https://docs.nlr.gov/docs/fy20osti/76773.pdf), retrieved 2026-10-13, section 3.2, Figure 16, printed page 23: "yaw free-decay". Direction support. |
| Operating: source environment | no public reference found for the complete as-held case. [22](https://github.com/vamseeachanta/digitalmodel/blob/50cd3f70645d6470df2bcfb6e12bd40fd69d3ded/docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md), retrieved 2026-10-13, Proposed cases: "reference to be selected: candidates must be public". |
| Operating: environmental variation | no public reference found for the complete unselected variation. [22](https://github.com/vamseeachanta/digitalmodel/blob/50cd3f70645d6470df2bcfb6e12bd40fd69d3ded/docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md), retrieved 2026-10-13, Proposed cases: "reference to be selected: candidates must be public". Candidate family evidence appears in question 2. |
| Parked: source environment | no public reference found for the complete as-held case. [22](https://github.com/vamseeachanta/digitalmodel/blob/50cd3f70645d6470df2bcfb6e12bd40fd69d3ded/docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md), retrieved 2026-10-13, Proposed cases: "reference to be selected: candidates must be public". |
| Parked: environmental variation | no public reference found for the complete unselected variation. [22](https://github.com/vamseeachanta/digitalmodel/blob/50cd3f70645d6470df2bcfb6e12bd40fd69d3ded/docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md), retrieved 2026-10-13, Proposed cases: "reference to be selected: candidates must be public". Candidate family evidence appears in question 2. |

## What the sources state and what is inferred

**Stated**

- Sources [2], [3], [5] and [7] describe separate turbine and support-system definitions. Their scope is recorded in question 1; the descriptions are not a statement that any other implementation matches them.
- Sources [6], [13]-[17], [19] and [21] expose licence labels or text for the inspected repositories, files or package metadata. Report reuse rights are separately marked unresolved.
- Sources [9] and [11] expose contents entries. Source [7] provides an open worked assessment. These are distinct evidence types, preserved as such in the source register.
- Source [22] retains unresolved input fields. The public source selections recorded here do not fill those fields.

**Inferred**

- The report should maintain separate records for the reference definition and the implementation being assessed. Sources [2]-[7] identify different documentary and software objects; a shared turbine or platform name is insufficient evidence that their geometry, structural assumptions, controller tuning and input conventions coincide. A later equivalence check should identify the exact versions compared and record differences before benchmark numbers become comparison criteria.
- The decay citations support selection of a method family. They do not establish the worksheet's initial conditions, retained windows or acceptance limits. Those decisions remain explicit inputs under [22]; source [7] is a candidate basis for reviewing them. In particular, a citation attached to an axis name should not be read as evidence that a complete case has been specified or executed.
- The standards contents in [9] support separating production and parked families, but they do not establish either source environment as a compliant design case. The four unresolved rows should therefore remain unresolved until conditions, controls and comparison criteria are selected. A report can cite the open assessment [7] as an example while preserving this distinction; the example cannot silently replace a full standards assessment.
- The catalogue replacement notice in [8] creates an edition-selection checkpoint. A historical reference study and a later standards edition answer different questions. The report should say whether its purpose is reproduction of a published benchmark, a methodological demonstration or a design assessment under a selected edition. That choice should precede any attempt to describe the worksheet as a complete load-case matrix.
- Controller availability is not evidence of interchangeable binaries or parameters. Sources [6] and [13]-[17] identify multiple controller sources and configuration conventions. Later selection should bind the source revision, compiler output, interface, input file and tuning to the selected turbine representation. This is a reproducibility recommendation, not a conclusion that any inspected controller is compatible with the existing example.
- Wind-generation capability does not establish the correctness of an inflow file for a particular analysis. Sources [18]-[21] identify generation and output conventions. Any later adoption should review the intended turbulence description, spatial grid, time span and consuming interface against the selected case. No wind field was generated or imported, so numerical fidelity and format interoperability remain unverified.
- Rights evidence should travel with the particular object reused. Repository and package licence statements should not be extended automatically to linked reports, third-party wrappers or separately distributed examples. This recommendation follows the different rights evidence recorded in questions 1, 3 and 4; it is not a legal determination about permission to redistribute any particular bundle.

## Sources

1. *IEA - International Energy Agency*. International Energy Agency; date not stated. https://www.iea.org/; retrieved 2026-10-13; type: other; access: open full text. Network proof only; recorded under Not found as context for the failed initial request.
2. *Definition of a 5-MW Reference Wind Turbine for Offshore System Development*. Jonkman et al., NREL, February 2009, NREL/TP-500-38060. https://docs.nlr.gov/docs/fy09osti/38060.pdf; retrieved 2026-10-13; type: report; access: open full text.
3. *Definition of the Semisubmersible Floating System for Phase II of OC4*. Robertson et al., NREL, September 2014, NREL/TP-5000-60601. https://docs.nlr.gov/docs/fy14osti/60601.pdf; retrieved 2026-10-13; type: report; access: open full text.
4. *r-test*. OpenFAST; current branch, publication date not stated. https://github.com/OpenFAST/r-test; retrieved 2026-10-13; type: repository; access: open full text (README and listing only).
5. *IEA Wind TCP Task 37: Definition of the IEA 15-Megawatt Offshore Reference Wind Turbine*. Gaertner et al., NREL, 2020, NREL/TP-5000-75698; catalogue hosted by National Laboratory of the Rockies. https://research-hub.nlr.gov/en/publications/iea-wind-tcp-task-37-definition-of-the-iea-15-megawatt-offshore-r/; retrieved 2026-10-13; type: report; access: abstract only.
6. *IEA-15-240-RWT*. IEA Wind Systems; README heading v1.1. https://github.com/IEAWindSystems/IEA-15-240-RWT; retrieved 2026-10-13; type: repository; access: open full text (README and listing).
7. *Definition of the UMaine VolturnUS-S Reference Platform Developed for the IEA Wind 15-Megawatt Offshore Reference Wind Turbine*. Allen et al., NREL, July 2020, NREL/TP-5000-76773. https://docs.nlr.gov/docs/fy20osti/76773.pdf; retrieved 2026-10-13; type: report; access: open full text.
8. *IEC 61400-3-2:2025*. IEC, edition 1.0, 2025. https://webstore.iec.ch/en/publication/67761; retrieved 2026-10-13; type: standard catalogue page; access: abstract only.
9. *IEC 61400-3-2:2025 preview*. IEC, edition 1.0, 2025; distributor-hosted copy at iTeh. https://cdn.standards.iteh.ai/samples/104665/ec829410245e45188ee32e81bd4e003f/IEC-61400-3-2-2025.pdf; retrieved 2026-10-13; type: standard preview; access: table of contents only for cited material.
10. *DNV-RP-0286 Coupled analysis of floating wind turbines*. DNV, 2019-05 amended 2021-10. https://www.dnv.com/energy/standards-guidelines/dnv-rp-0286-coupled-analysis-of-floating-wind-turbines/; retrieved 2026-10-13; type: standard catalogue page; access: abstract only.
11. *DNV-RP-0286 preview copy*. DNV, May 2019 amended October 2021. https://brandcentral.dnv.com/fr/gallery/10651/others/2dd1ccdad4f9405f9eee1ca4da569a44_hi.pdf; retrieved 2026-10-13; type: standard preview; access: table of contents only for cited material.
12. *DNV-ST-0119 Floating wind turbines*. DNV, edition 2025-12 as displayed. https://www.dnv.com/energy/standards-guidelines/dnv-st-0119-floating-wind-turbines/; retrieved 2026-10-13; type: standard catalogue page; access: abstract only.
13. *ROSCO*. National Renewable Energy Laboratory; current repository hosted by NatLabRockies, publication date not stated. https://github.com/NatLabRockies/ROSCO; retrieved 2026-10-13; type: repository; access: open full text.
14. *ROSCO LICENSE*. Repository licence file; Apache licence version 2.0, January 2004. https://raw.githubusercontent.com/NatLabRockies/ROSCO/main/LICENSE; retrieved 2026-10-13; type: repository; access: open full text.
15. *Basic DTU Wind Energy controller*. DTU Wind Energy; superseded repository, publication date not stated. https://github.com/DTUWindEnergy/BasicDTUController; retrieved 2026-10-13; type: repository; access: open full text.
16. *NREL 5MW Reference Turbine Controllers*. OpenFAST; publication date not stated. https://github.com/OpenFAST/nrel-5mw-controllers; retrieved 2026-10-13; type: repository; access: open full text.
17. *DISCON.F90*. OpenFAST controller source; header copyright dates 2015-2017. https://raw.githubusercontent.com/OpenFAST/nrel-5mw-controllers/main/DISCON/DISCON.F90; retrieved 2026-10-13; type: repository; access: open full text.
18. *TurbSim.f90*. OpenFAST; current branch, publication date not stated. https://github.com/OpenFAST/openfast/blob/main/modules/turbsim/src/TurbSim.f90; retrieved 2026-10-13; type: repository; access: open full text.
19. *OpenFAST LICENSE*. OpenFAST; Apache licence version 2.0, January 2004. https://raw.githubusercontent.com/OpenFAST/openfast/main/LICENSE; retrieved 2026-10-13; type: repository; access: open full text.
20. *Output Files*. OpenFAST documentation, version v4.0.5 URL. https://openfast.readthedocs.io/en/v4.0.5/source/user/turbsim/output.html; retrieved 2026-10-13; type: other (software documentation); access: open full text.
21. *PyConTurb: Constrained Stochastic Turbulence for Wind Energy Applications*. PyConTurb package description on PyPI; release 2.7.4, 2025. https://pypi.org/project/pyconturb/; retrieved 2026-10-13; type: repository (package metadata); access: open full text.
22. *Floating wind first worked-example case table*. digitalmodel; inspected base revision. https://github.com/vamseeachanta/digitalmodel/blob/50cd3f70645d6470df2bcfb6e12bd40fd69d3ded/docs/domains/orcaflex/wind/fowt-worked-example/load-case-table.md; retrieved 2026-10-13; type: repository; access: open full text, read locally.

## Not found

The following nine items distinguish failed requests from unresolved engineering evidence. A failed request is an observation about this retrieval, not a finding that the source is unavailable to every reader.

1. Initial network-proof request: https://www.nrel.gov/wind returned `(502) Bad Gateway`. The successful proof was https://www.iea.org/ [1]. Neither page supplies engineering case criteria.
2. IEA turbine full report: https://docs.nlr.gov/docs/fy20osti/75698.pdf returned `(400) Timeout fetching`. Search: `Definition IEA 15 Megawatt Offshore Reference Wind Turbine 2020 osti`. The abstract at `research-hub.nlr.gov` was opened and is cited as [5], without pretending that its linked full report was inspected.
3. Direct IEC preview request: https://webstore.iec.ch/preview/info_iec61400-3-2%7Bed1.0%7Den.pdf was reported not accessible via the tool. Search: `IEC 61400-3-2 2025 contents design load cases 7.4`. The IEC-authored distributor preview [9] was successfully opened; paid clauses remain uninspected.
4. Current Basic DTU source location: https://gitlab.windenergy.dtu.dk/OpenLAC/BasicDTUController returned `(400) Timeout fetching`. Search: `DTU Basic Controller licence`. Source [15] establishes the older repository's statement only; the replacement repository's current licence and content remain unverified.
5. PyConTurb output documentation: https://pyconturb.pages.windenergy.dtu.dk/pyconturb/notebooks/7_outputs.html returned `(400) Timeout fetching`. Search: `TurbSim PyConTurb license turbulent wind generator`. Export-format claims from the search result were excluded.
6. PyConTurb documentation homepage: https://pyconturb.pages.windenergy.dtu.dk/pyconturb/ returned `(400) Timeout fetching`. The package-index route at `pypi.org` supplied [21], but did not replace inspection of the documentation or source distribution.
7. Complete support for the operating and parked worksheet definitions was not established. Searches used the generic load-case terms listed in Method, across IEC, DNV and laboratory publications. Family-level material was located, but none was demonstrated to match the unselected conditions and controls in [22]. The mapping therefore retains four explicit no-reference findings.
8. DNV-ST-0119 load-case clause number and title were not established from the public catalogue opened at `dnv.com`. Search: `DNV ST 0119 floating wind turbines DNV RP 0286`. No number was supplied from an older edition, an uninspected copy or memory.
9. Reuse permissions covering each reference report, benchmark and its separately distributed illustrations were not established. The laboratory report notices and repository licence displays were inspected; the licence searches listed in Method did not resolve the full rights chain. Those objects remain marked `rights: to be checked` before copying or redistribution.

## Recommendations for the report

- Select the reference turbine/platform pair explicitly, then compare the chosen implementation against its own definitions rather than combining properties across reports [2]-[7].
- Use the six decay directions as a method-selection starting point; retain unresolved conditions and acceptance criteria until reviewed [7], [22].
- Keep the statics citation labelled partial and establish a whole-system equilibrium criterion separately [7], [22].
- Preserve the four operating/parked reference gaps until a reviewed environment and control definition is attached to each family [9], [22].
- State the standards edition and the purpose of the comparison; cite paid clauses by number and title without reproducing their requirements [8]-[12].
- Record controller revision, licence evidence, interface and tuning as separate dependencies [6], [13]-[17].
- Select wind generation together with its output conventions; verify the eventual consuming interface before adopting a generated file [18]-[21].
- Retain report and benchmark rights checks independently of repository licence labels, and resolve the failed retrievals before expanding the evidence claims [2]-[7], [15], [21].
