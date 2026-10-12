# Public-source review of API 579-1/ASME FFS-1 Parts 8, 11, 13, 14

Public-source research screen for issue [#2203](https://github.com/vamseeachanta/digitalmodel/issues/2203); not an implementation.

Single source: research note on the issue, sources checked 2026-10-11.

Research only; no code. All sources were checked on 2026-10-11. "Opened" means I read the page or document text myself; "search snippet" means I only saw a search-engine summary and the claim is **unverified**. Edition statements come only from opened pages. Nothing normative from the standard is reproduced.

## 1. Bottom line

- **No published data on how often each part is used was found**, for offshore, marine or any other sector. A targeted search returned only vendor articles without numbers. Every statement about usage below is qualitative and sourced; treat "how often" as unanswered.
- **Open worked examples exist for three of the four parts.** A file on the standards body's own site is the 2009 example problem manual, whose contents list six example problems for Part 8, three for Part 11 and two for Part 13 [S1]. Part 14 did not exist then; its open material is a conference overview paper [S5].
- The tracker's sequencing (8, 14, 13, 11) is supportable. The one change worth considering is moving Part 11 out of the engine queue altogether: its published practice is field-led (zone mapping, hardness, replication), not calculation-led [S6, S7].
- One gap in the map: Part 12 (dents, gouges and dent-gouge combinations) is listed in the standard's contents [S2] but is in neither this tracker nor the companion issue's list as quoted here. Confirm it is covered elsewhere.

## 2. Part titles as published

The standards body's public table of contents is headed "API 579-1/ASME FFS-1, December 2021" and lists [S2, opened]:
- Part 8, assessment of weld misalignment and shell distortions, with a technical-basis annex;
- Part 11, assessment of fire damage, with a technical-basis annex and an annex on metallurgical investigation and evaluation of mechanical properties;
- Part 12, assessment of dents, gouges and dent-gouge combinations;
- Part 13, assessment of laminations, with a technical-basis annex;
- Part 14, assessment of fatigue damage, with a technical-basis annex and an annex of material properties for fatigue analysis.

## 3. Part by part

| | Part 8: weld misalignment and shell distortion | Part 11: fire damage | Part 13: laminations | Part 14: fatigue |
|---|---|---|---|---|
| What it covers | Peaking and offset at welds; out-of-roundness and bulges. Both misalignment types "produce a centerline offset that loads the weld joint in bending"; imperfections can drive fatigue under cyclic load [S4, visible part opened]. | Deciding which fire-exposed components can be reused, repaired or must be replaced, starting from heat exposure zones [S6]. | Planar separations in plate. The 2016 revision re-arranged the procedure so that Level 1 and Level 2 give identical results, per one practitioner review [S3]. A free review article places laminations alongside hydrogen blisters and hydrogen-induced cracking in its Part 7 discussion (abstract summary; boundary between Parts 7 and 13 should be checked in the text) [S8]. | New in the 2016 edition: the cyclic-failure procedures moved out of an annex; screening is Level 1, S-N and structural-stress methods are Level 2, and a strain-life procedure with critical-plane mean-stress correction is Level 3 [S3]. Welded joints: classical smooth-bar curves with a fatigue strength reduction factor, and the mesh-insensitive structural stress method with a master S-N curve [S5]. |
| Methods and levels | Closed-form solutions for "a few simplified (but very common) geometries"; beyond those, finite element analysis is typically needed [S4]. A free article gives a general procedure "with a worked example covering Level 1 and Level 2" [S8, contents page opened]. | Level 1: screen by material and assigned zone. Level 2: hardness, field metallographic replication, distortion checks, penetrant testing. Level 3: analysis of distorted shells with changed properties, reserved for long-lead items [S6]. | Geometric rules (short, as the tracker says). | See above. One commentary calls the Level 2 calculation set "fairly complicated" (search snippet; **unverified**); Level 3 "will likely need dedicated post-processing software" [S3]. |
| Inputs | Measured shape: peaking height, offset, radius deviations. A CC BY case study used laser-scanned roundness data for a storage tank [S9]. | Zone map from physical evidence (melted jacketing, scaled steel, blistered paint), material of construction, in-situ hardness, replicas, dimensional survey; metal temperature depends on contents, not only on flame temperature [S6, S7]. | Ultrasonic mapping of lamination extent, depth and spacing (not verified in an opened source). | Stress histories, cycle counts, weld classification, material fatigue data. |
| Public worked examples and validation | Six example problems in the 2009 manual [S1]; free Level 1 and Level 2 example [S8]; CC BY Level 3 tank case with seismic and environmental load cases [S9]. | Three example problems in the 2009 manual [S1]. A published refinery case: six zones mapped, most items cleared at Level 1 or 2, one Level 3 nozzle analysis; in the hottest zone 3 of 5 vessels and 6 air coolers were beyond repair, while every item in the next zone returned to service without repair; initial assessment took "about seven 10-hour days" [S7]. | Two example problems in the 2009 manual [S1]. Nothing else found. | None in the 2009 manual (the part did not exist). Open conference overview [S5]. Each part also has a technical-basis-and-validation annex inside the licensed standard [S2]. |
| Published evidence of use offshore or marine | None found. The storage-tank and pipeline uses named in this tracker are plausible; the open cases found are onshore tanks [S9]. | None found for offshore installations. All published cases opened are refinery process units [S6, S7]. | None found. | None found. Offshore structural fatigue is normally done to structural codes rather than this part; that statement is from general knowledge and was **not verified** today. |
| Worth building? | **Yes.** Closed-form Level 1 and 2 are short, examples are open, and it pairs with existing shell and tank work. | **No engine; at most a checklist.** The value is in field metallurgy and experienced judgement ("it is possible that the conclusions can be non-conservative" if zones are wrong, per a search snippet of [S6]). A small analysis-led group would be a subcontractor for the Level 3 distortion analysis only. | **Yes, but small.** Low effort, low demand; do it when a client case appears or to complete the map. | **Yes.** Highest reuse of the existing S-N library and cycle counting; Level 1 screening and the Level 2 welded-joint methods first, strain-life later, as the tracker already says. |

## 4. What needs the licensed standard

Everything normative: the screening tables for Part 11 (material against zone), the Part 8 closed-form expressions and limits, the Part 13 rules, the Part 14 curves and the fatigue material annex. The open sources above are enough to design tests and to choose the order; they are not a substitute for the text when writing clause-cited docstrings.

The 2009 example manual predates two later editions. Its examples are useful as test cases only where the procedure has not changed; the 2016 revision changed Part 13 [S3], so its two lamination examples should be re-checked against the current text before being used as golden values.

## 5. Suggested order, with reasons

1. **Part 14 screening and Level 2 welded-joint methods**: best reuse, open overview paper, and it is the part most likely to matter to cyclically loaded offshore equipment.
2. **Part 8 Level 1 and 2**: best open example coverage.
3. **Part 13**: short; schedule opportunistically.
4. **Part 11**: checklist and routing only.

This swaps the first two items of the tracker's sequence; either order is defensible, and the tracker's "catalog demand" argument for Part 8 first was not something I could test from public sources.

## Sources

- [S1] "Fitness-For-Service Example Problem Manual", API 579-2/ASME FFS-2 2009, dated August 11, 2009, file hosted on the standards body's site. https://www.asme.org/getmedia/4fe70cf9-8738-4285-aa49-9d68ea6d6e34/21639.pdf (table of contents opened; examples not read)
- [S2] Table of contents headed "API 579-1/ASME FFS-1, December 2021". https://www.asme.org/getmedia/5726e673-eb65-456e-913d-3d0dce0359ff/asme-ffs-1_2021_toc.pdf (opened)
- [S3] "Fitness-For-Service API 579-1/ASME FFS-1: Substantive Changes to the 2016 Edition", practitioner article, 30 March 2017. https://becht.com/?p=989 (opened)
- [S4] "FFS Forum: Understanding Part 8 Assessment of Weld Misalignment and Shell Out-of-Round", Inspectioneering Journal, January/February 2024. https://inspectioneering.com/journal/2024-02-29/10939/ffs-forum-understanding-part-8-assessment-of-weld-misalignment-and-shell-out-of (visible part opened; body behind registration)
- [S5] Osage, Dong, Spring, "Fatigue assessment of welded joints in API 579-1/ASME FFS-1 2016: existing methods and new developments", Fatigue Design 2017 proceedings, DOI 10.1016/j.proeng.2018.02.049, listed as open access. https://findanexpert.unimelb.edu.au/scholarlywork/1818212-fatigue-assessment-of-welded-joints-in-api-579-1-asme-ffs-1-2016-existing-methods-and-new-developments (record opened; paper not read)
- [S6] "Understanding Fire Damage Assessments", practitioner article, 25 April 2023. https://e2g.com/?p=20603 (opened)
- [S7] "Fire-damage assessment cuts reconstruction cost, plant downtime", Oil & Gas Journal, 18 September 2000. https://www.ogj.com/general-interest/companies/article/17216136/fire-damage-assessment-cuts-reconstruction-cost-plant-downtime (opened)
- [S8] Journal of High Pressure Institute of Japan, volume 57, issue 5 (2019), seven free-access review articles on Parts 1-9, including "Assessment of Weld Misalignment and Shell Distortions (Part8)" and "Fitness-For-Service Assessment for Hydrogen Damages (Part. 7)". https://www.jstage.jst.go.jp/browse/hpi/57/5/_contents/-char/en (contents page opened; articles not read)
- [S9] Herucakra, Adnyani, Prasetyo, Rizkhita, "Fitness for Service (FFS) and Fatigue Assessment for Shell Distorted Steel Storage Tank Based on API 579-1/ASME FFS-1", Journal of Materials Exploration and Findings 3(1), 2024, CC BY 4.0. https://scholarhub.ui.ac.id/jmef/vol3/iss1/3 (abstract opened)

## Could not verify

- Any usage-frequency statistics by part or by sector.
- Whether a newer example problem manual exists and what it contains for Part 14.
- The Part 7 / Part 13 boundary for laminations, and the inspection inputs for Part 13.
- Offshore or marine published cases for any of the four parts.
