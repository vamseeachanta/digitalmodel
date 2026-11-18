# Reusable prompts: surface card to pump card

Dynacard article ("Dynamometer card analysis: from the surface card to the pump card and a diagnosis"), revision 4 of 18 Nov 2026. This page holds in full what Appendix D of the article summarises. `examples/domains/artificial_lift/build_dynacard_article.py` writes it from the saved results.

These prompts let your team, or your assistant, rebuild the article's analysis from public code and then run it on a card of your own. To have a study of your own rod pump done for you instead, see below.

## What you need to run this

- Data required: none beyond the public data set below.
- Data required to adapt: a surface card over one stroke, the rod string, the pump depth and plunger diameter, and the pumping speed.
- Paid or behind an account: none.
- Changes over time: the prompt runs the latest version of the analysis code until that version is published; the data links are pinned to a commit.

## Reproduce the published result

Give this prompt to your assistant:

> Turn the surface dynamometer card of the public example well in the digitalmodel library into a pump card with the wave equation, and read pump stroke, fluid load and fillage from it. Follow PROMPT.md in examples/report-evals/dynacard-pump-card step by step: it gives the commands, the inputs to ask for and the expected values. Grade the result with eval.py beside it.

- Complete prompt, to copy whole: [PROMPT.md](https://github.com/vamseeachanta/digitalmodel/blob/7d02ae07248b038dcdec5e7097565c02fcd152f8/examples/report-evals/dynacard-pump-card/PROMPT.md), version of 12 Nov 2026.
- Check values on the public example well, finite-difference method: surface stroke 53.99 in, pump stroke 39.57 in, fluid load 3,310 lbf, fillage 60.08 %, displacement 40.01 bbl/day fillage-adjusted and 66.60 bbl/day for a full pump, at 24 h/day and no slip.
- The ten examples of Figure 2 rebuild from the repository root with `PYTHONPATH=src python examples/domains/artificial_lift/build_dynacard_worked_examples.py --check`; their values are in [ten-examples.md](ten-examples.md).

## Adapt it to your project

Give this prompt to your assistant:

> Copy reader-card-template.json beside PROMPT.md, replace the surface card samples and the as-run rod string, pump depth, plunger diameter and pumping speed with my well's values, and run analysis.py on the new file as PROMPT.md shows. Ask me for every item PROMPT.md lists under "Ask me before you go on if". Keep every ASSUMED input, and report every shape reading as a hypothesis to check at the well.

- Template: [reader-card-template.json](https://github.com/vamseeachanta/digitalmodel/blob/7d02ae07248b038dcdec5e7097565c02fcd152f8/examples/report-evals/dynacard-pump-card/reader-card-template.json).
- Without an assistant: the [web page](https://github.com/vamseeachanta/digitalmodel/blob/f5c0009c6dd9e009cdac636d3dfbd7ae0ec1f79c/docs/apps/dynacard/README.md) of the library, version of 14 Nov 2026, loads a surface card as a CSV with the well's rod and pump data and draws the pump card.

## Data set

- Public example well: [cleansed_well_001.json](https://github.com/vamseeachanta/digitalmodel/blob/f5c0009c6dd9e009cdac636d3dfbd7ae0ec1f79c/tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json) of the digitalmodel library, MIT licence, version of 14 Nov 2026.
- Public diagnostic plate of ten pump working conditions: Mendeley Data, version 1, [doi:10.17632/hnfwm9mjs6.1](https://doi.org/10.17632/hnfwm9mjs6.1), CC0 1.0, retrieved 11 Oct 2026.

To have a study of your own rod pump done for you, write to support@aceengineer.com.
