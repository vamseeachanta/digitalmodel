# Example well: inputs, results and limits

Dynacard article ("Dynamometer card analysis: from the surface card to the pump card and a diagnosis"), revision 4.1 of 18 Nov 2026. This page holds in full what Appendix A of the article summarises. `examples/domains/artificial_lift/build_dynacard_article.py` writes it from the saved results.

Purpose:

- Give the inputs and results of the public example well behind Figure 1 and the key conclusions.
- State the sensitivity and the limits that apply to the example well and to the web page.

The well is one anonymised field well in this library: [cleansed_well_001.json](https://github.com/vamseeachanta/digitalmodel/blob/f5c0009c6dd9e009cdac636d3dfbd7ae0ec1f79c/tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json), version of 14 Nov 2026.

## Inputs

| Input | Value | Unit |
| --- | --- | --- |
| Pumping speed | 6.42 | SPM |
| Pump depth | 4,844 | ft |
| Plunger diameter | 1.500 | in |
| Rod string, surface first | 0.875 in × 1,400 ft; 0.750 in × 3,350 ft | in; ft |
| Fluid density | 63.237 | lb/ft³ |
| Tubing pressure | 100 | psi |

Table 1. Example well inputs, from the well record; fluid density derived from its well test.

## Results

| Result | Value | Unit |
| --- | --- | --- |
| Surface stroke | 53.99 | in |
| Peak and minimum polished-rod load | 12,240; 7,296 | lbf |
| Pump stroke | 39.57 | in |
| Effective plunger travel | 23.77 | in |
| Pump fillage | 60.08 | % |
| Fluid load | 3,310 | lbf |
| Maximum and minimum pump load | 4,661; 82 | lbf |
| Displacement, fillage-adjusted and full pump | 40.01; 66.60 | bbl/day |

Table 2. Example well results, finite-difference method; displacement at 24 h/day and no slip.

## Notes and limits

- The well record labels this card a shutdown card, the card the pump controller stores when it stops the pump. The source system reports that its own corner detection, which finds the points where the valves open and close, failed on it, so the fillage and the fluid load rest on the corner detection of the digitalmodel library alone. Fillage in normal operation is not established: this is the only card.
- The rod string in the record ends at 4,750 ft and the pump is at 4,844 ft; the record's rod list does not cover the 94 ft between them. The end of the listed rods is taken as the pump (an assumption); confirming it needs the as-run rod and completion tally.
- The shape model gives tagging up a model score of 99.91 %. The score is the model's own class probability over its 18 classes, not the chance that the fault is present. The same model missed tagging up in its ten-example test (Example 5), so the reading is a weak hypothesis. The model is trained on generated cards; on the ten plate shapes it is correct on 6 of 10 (5 of the 8 whose plate label has the same name as a model class). The plate shapes also shape the generated training cards, so they are not an independent test.
- The inputs that matter most: a 1 % larger rod diameter lowers the maximum pump load by about 3.1 % and the fillage by about 0.9 %; a 1 % load-cell gain error raises the maximum pump load by about 2.6 %; a 1 % surface position scale error moves the pump stroke by about 1.3 %. The full sensitivity table is in Appendix A of the [full study](https://github.com/vamseeachanta/digitalmodel/blob/7d02ae07248b038dcdec5e7097565c02fcd152f8/docs/domains/artificial_lift/dynacard-pump-card-report.md).
- Limits: unanchored tubing is not modelled and only steel rods are accepted. Both methods and the web page treat the well as vertical; a deviation survey is flagged, not modelled. A card alone does not give production; run time and slip are needed. The web page also assumes anchored tubing and samples evenly spaced in time. Fluid load and fillage step with the detected corner samples, so rounding the last digit of a card can move the fluid load by tens of lbf: 34 lbf at most in the ten examples.
