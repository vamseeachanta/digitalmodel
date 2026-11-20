# Dynacard article: detail pages

Article: "Dynamometer card analysis: from the surface card to the pump card and a diagnosis", revision 4.1 of 18 Nov 2026. The article gives a short summary in each appendix; this folder holds the detail in full, one page per appendix.

| Page | What it holds | Article part |
| --- | --- | --- |
| [Example well](example-well.md) | Inputs, results, sensitivity and limits of the public example well | Appendix A |
| [Numerical methods](numerical-methods.md) | The finite-difference and frequency-domain methods and their comparison | Appendix B |
| [Ten examples](ten-examples.md) | How the ten pump conditions are built, and their full values | Appendix C |
| [Reusable prompts](reusable-prompts.md) | The prompts to rebuild the analysis and run it on your own card | Appendix D |

Table 1. Detail pages of the article.

Other files in this folder:

- `dynacard-article.html`: the article page, static and offline.
- `article-inputs.json`: the example well's cards, inputs and shape reading, from the report analysis.
- `figure-1-cards.csv`: the data of Figure 1.

Rebuild every file in this folder, or check that none is stale, from the repository root:

```text
PYTHONPATH=src python examples/domains/artificial_lift/build_dynacard_article.py
PYTHONPATH=src python examples/domains/artificial_lift/build_dynacard_article.py --check
```
