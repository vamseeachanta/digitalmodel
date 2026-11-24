# Method and data

Article: "Fitness for service of corroded pipelines: strength and remaining useful life (RUL)", The Deckhand Chronicles, revision 14 of 23 Nov 2026. This page holds in full what Appendix A of the article summarises.

## Measured scan, the main example

Data. An ultrasonic C-scan of a steel riser section, anonymized and published in the digitalmodel library [9]: about 540 mm [21 in] outside diameter and about 22 mm [0.9 in] nominal wall, both rounded here, grade X80; the exact nominal dimensions are set in the script. The asset, its owner, its location and the inspection are not named. The scan file covers about 2 m, rounded, at a 4 mm axial pitch by 64 circumferential positions, with about 9 % of positions not measured, rounded. Measured values are unmodified apart from rounding to 0.001 mm. The exact counts and reading range are in `data/measured-grid-results.json`.

Preparation, all in `scripts/ffs/c3_measured_grid_assessment.py`:

- Defect patch: the run of axial rows around the thinnest reading that hold a reading with more than 10 % loss (the flaw threshold of the local-loss engine), plus one row each side; columns from the first to the last column with such a reading in that run. Result: 20 by 7 cells, about 80 mm long and 160 mm wide, all rounded; the rows and columns of the scan file are in the results file.
- Readings above the nominal wall are taken as nominal, with no loss and no credit for the extra wall: 8 cells.
- An unmeasured cell takes the thinnest reading of its own axial row in the patch: 21 cells. Every method below uses the deepest loss of each row, so this fill changes no result; the script reruns with these cells set to nominal and records that the results are identical.
- The 64 positions are taken as equally spaced round the full circumference. The scan file carries no spacing record; the spacing enters only the axial-stress check.
- Six rows elsewhere in the scan hold readings with more than 10 % loss, the thinnest about 17 mm, rounded (22.7 % loss). They are separate from the assessed patch and are not assessed here.
- The full scan continues beyond both patch ends with every reading inside the 10 % threshold, so the defect length is taken as bounded.

Assumptions for this assessment. A riser is not a pipeline; the tube is rated as a pipeline would be, so that measured readings run through the same steps as the synthetic grid. Design pressure 16.55 MPa [2,400 psi], about half the ASME B31.8 rating before loss, as for the synthetic grid; ASME B31.8 design factor 0.72 (Location Class 1, Division 2); axial stress 68.95 MPa [10,000 psi] and allowable remaining strength factor 0.90, as for the synthetic grid; specified minimum yield strength 551.6 MPa [80,000 psi] and tensile strength 624.7 MPa [90,600 psi] for X80. Corrosion rate 0.253 mm/year, the rate the inspection records give this scan; the records do not state how the rate was derived. The same records give this scan a much shorter life against the wall needed to resist collapse under external pressure. Collapse, fatigue and the other riser load cases are not covered by this assessment.

Method. The patch is passed to the same function, `evaluate_section` in `scripts/reporting/build_pipeline_ffs_report.py`, that rates the synthetic Section A. It is rated by the simplified local metal-loss relation of API 579-1/ASME FFS-1, Parts 2 and 5 [6], from the ASME B31.8 rating before loss [7]. The burst pressures come from the five methods of References 1 to 5, with the allowable factors stated in "Five methods give four different pressures". The thinnest reading, about 15 mm (rounded), is above the 11.245 mm wall that ASME B31.8 needs at the design pressure, so life is projected at a constant thinning rate to that wall at the thinnest reading, with flaw length and material held fixed.

Results, to 4 significant figures. Remaining strength factor 0.9343 over a flaw length of about 70 mm (rounded), against the allowable 0.90, so the rating is not reduced: 32.71 MPa [4,744 psi], 197.6 % of the design pressure. Allowable and failure pressures, in MPa: B31G 34.52 and 47.98; Modified B31G 35.12 and 48.81; RSTRENG 35.13 and 48.83; RSTRENG-2D 35.13 and 48.83; DNV-RP-F101 33.68 and 51.98. Life 14.94 years at 0.253 mm/year, 29.88 years at half the rate and 7.469 years at double the rate. Measurement sufficiency: sufficient. Decision: accept at the current rating, within the limits above.

## Synthetic grid, Section A, the comparison

Unchanged from revision 5. Section A is rated by the simplified local metal-loss relation of API 579-1/ASME FFS-1, Parts 2 and 5 [6], from the ASME B31.8 rating before loss [7]. The burst pressures come from the five methods of References 1 to 5. Life is projected at a constant thinning rate to the wall at which the reduced rating falls to the design pressure. Its readings are a worked example set up for the assessment, not measured data.

## Editions and evaluation levels

The editions are those the library cites; the linked publisher pages may show later editions. The evaluation level of each method is the one the library implements, listed below. This assessment does not compare these levels with those of ASME B31G-2012, the edition the library cites.

- B31G and Modified B31G [1, 2]: Level 1 evaluation.
- RSTRENG [3]: Level 2 effective-area evaluation.
- RSTRENG-2D [4]: Level 2 effective-area evaluation; the implementation projects each axial row to its greatest depth.

## Data sets and code

Public data set, measured scan: anonymized riser ultrasonic scan in digitalmodel, published by AceEngineer, MIT licence, measured readings, retrieved 20 Nov 2026, repinned 23 Nov 2026 to commit 193bfe3086f67a49ce163fa05324308a35f0cff0. <https://github.com/vamseeachanta/digitalmodel/tree/193bfe3086f67a49ce163fa05324308a35f0cff0/tests/asset_integrity/test_data/real_inspection>

Public data set, synthetic grid: the AceEngineer corroded-pipeline worked example in digitalmodel, published by AceEngineer, MIT licence, worked-example readings, retrieved 12 Nov 2026, repinned 23 Nov 2026 to commit 193bfe3086f67a49ce163fa05324308a35f0cff0. <https://github.com/vamseeachanta/digitalmodel/tree/193bfe3086f67a49ce163fa05324308a35f0cff0/examples/workflows/pipeline-ffs-assessment>

Code: the AceEngineer public code repository, <https://github.com/vamseeachanta/digitalmodel> (MIT licence), at commit 193bfe3086f67a49ce163fa05324308a35f0cff0 for the measured scan.

## References

1. B31G method: ASME B31G, *Manual for Determining the Remaining Strength of Corroded Pipelines*, 2012 as implemented; current edition 2023. <https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines>
2. Modified B31G method: ASME B31G, *Manual for Determining the Remaining Strength of Corroded Pipelines*, 2012 as implemented; current edition 2023. <https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines>
3. RSTRENG method: ASME B31G, *Manual for Determining the Remaining Strength of Corroded Pipelines*, 2012 as implemented; current edition 2023. <https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines>
4. RSTRENG-2D method: ASME B31G, *Manual for Determining the Remaining Strength of Corroded Pipelines*, 2012 as implemented; current edition 2023. <https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines>
5. DNV-RP-F101, *Corroded Pipelines*, January 2015 as implemented, Part B (allowable stress approach); current edition September 2019, amended September 2025. <https://www.dnv.com/energy/standards-guidelines/dnv-rp-f101-corroded-pipelines/>
6. API 579-1/ASME FFS-1, *Fitness-For-Service*, 2021; Part 2, *Fitness-For-Service Engineering Assessment Procedure*; Part 5, *Assessment of Local Metal Loss*. The local-loss relation is simplified. 2021 is the current edition. <https://www.asme.org/codes-standards/find-codes-standards/fitness-for-service>
7. ASME B31.8, *Gas Transmission and Distribution Piping Systems*, 2022 as implemented, 841, *Steel Pipe*; current edition 2025. <https://www.asme.org/codes-standards/find-codes-standards/b31-8-gas-transmission-distribution-piping-systems>
8. AceEngineer, pipeline fitness-for-service assessment, study page, October 2026. <https://vamseeachanta.github.io/digitalmodel/ffs/pipeline-ffs-assessment.html>
9. AceEngineer, anonymized riser ultrasonic scans and inspection records, digitalmodel test data. <https://github.com/vamseeachanta/digitalmodel/tree/193bfe3086f67a49ce163fa05324308a35f0cff0/tests/asset_integrity/test_data/real_inspection>
