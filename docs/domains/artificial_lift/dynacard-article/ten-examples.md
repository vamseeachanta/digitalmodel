# Ten pump conditions: how the examples are built, and their values

Dynacard article ("Dynamometer card analysis: from the surface card to the pump card and a diagnosis"), revision 4 of 18 Nov 2026. This page holds in full what Appendix C of the article summarises. `examples/domains/artificial_lift/build_dynacard_article.py` writes it from the saved results.

Purpose:

- Say how the ten examples of Figure 2 are built from the public diagnostic plate.
- Give the full values of each example and the model readings.

## How the examples are built

- Each of the ten shapes of the public diagnostic plate ([doi:10.17632/hnfwm9mjs6.1](https://doi.org/10.17632/hnfwm9mjs6.1), CC0 1.0, retrieved 11 Oct 2026) is scaled to an assumed 40 in pump stroke and to the static fluid load of a 1.5 in plunger at 4,844 ft with 100 psi tubing pressure and zero pump intake pressure (3,936 lbf). These are shape illustrations, not field pump cards. The plate gives shapes only, with no stroke or load, and does not say whether they are surface or pump cards; they are taken as pump cards by assumption.
- Each scaled shape is lifted to a surface card through the rod string of the example well and then analysed like a measured card. The surface cards are therefore synthesised, not measured.
- The plate labels insufficient supply and sand production have no model class of the same name; they are matched to fluid pound and sand wear by assumption.
- The shape model names 6 of the 10 conditions; the second, shipped model names 4. Model scores are the models' own outputs, not the chance that a fault is present.
- The web page reproduces the library on all ten examples: pump loads within 3 × 10⁻⁹ lbf and the same model labels.

## Values

| No. | Condition | Surface stroke (in) | Pump stroke (in) | Fillage (%) | Fluid load (lbf) | Round trip (% of load range) | Shape model | Shipped model |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | Normal operation | 46.91 | 40.00 | 83.6 | 3,711 | 0.50 | normal operation | plunger undertravel |
| 2 | Insufficient supply | 46.38 | 40.00 | 23.1 | 3,020 | 0.88 | fluid pound | fluid pound |
| 3 | Gas interference | 48.44 | 40.00 | 28.3 | 3,405 | 0.40 | gas interference | traveling valve leak |
| 4 | Rod parting | 48.97 | 40.00 | 92.4 | 2,935 | 0.56 | rod parting | plunger undertravel |
| 5 | Tagging up | 49.99 | 40.00 | 70.2 | 3,456 | 0.76 | traveling valve leak | traveling valve leak |
| 6 | Tagging down | 49.50 | 40.00 | 79.0 | 3,303 | 0.43 | traveling valve leak | traveling valve leak |
| 7 | Traveling valve leakage | 46.90 | 40.00 | 93.7 | 3,403 | 0.50 | traveling valve leak | traveling valve leak |
| 8 | Standing valve leakage | 47.67 | 40.00 | 74.5 | 3,762 | 0.50 | standing valve leak | standing valve leak |
| 9 | Plunger out of barrel | 37.83 | 40.00 | 93.6 | 2,554 | 0.54 | traveling valve leak | plunger out of barrel |
| 10 | Sand production | 45.62 | 40.00 | 77.1 | 3,235 | 2.42 | traveling valve leak | traveling valve leak |

Table 1. Ten worked examples, finite-difference method.

The probabilities of all model classes are shown in the [diagnostics matrix](https://github.com/vamseeachanta/digitalmodel/blob/f5c0009c6dd9e009cdac636d3dfbd7ae0ec1f79c/docs/domains/artificial_lift/dynacard-examples/diagnostics-matrix.png); each example's figure and data are in the [worked examples](https://github.com/vamseeachanta/digitalmodel/blob/f5c0009c6dd9e009cdac636d3dfbd7ae0ec1f79c/docs/domains/artificial_lift/dynacard-examples/README.md). Both are versions of 14 Nov 2026.
