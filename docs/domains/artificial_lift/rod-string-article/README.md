# Rod string article: detail pages

Rod string article ("Dynamometer card: through the rod string"), revision 3, 20 Nov 2026. `examples/domains/artificial_lift/build_rod_string_article.py` writes it from `results.json`; the results come from the library at [a26efaa4](https://github.com/vamseeachanta/digitalmodel/tree/a26efaa413c962105cdd49ce6b5122b5d0ab0b58/) (version of 19 Nov 2026).

Purpose:

- Hold the detail behind the rod string article, part 2 of the rod pump series.
- Give the data behind every figure, so each number can be checked.

## Pages

- [Example well, stretch, stress and sensitivity](stroke-and-stress.md): Appendix A.
- [Damping](damping.md): Appendix B.
- [Method and its limits](method-and-limits.md): Appendix C.
- [Reusable prompts](reusable-prompts.md): Appendix D.

## Data

- [results.json](results.json): every saved result, written by `--refresh`.
- [figure-1-cards-at-depth.csv](figure-1-cards-at-depth.csv): Figure 1.
- [figure-2-3-depth-profile.csv](figure-2-3-depth-profile.csv): Figures 2 and 3.
- [figure-3-rod-stack-up.csv](figure-3-rod-stack-up.csv): Figure 3, the rod string stack-up.
- [figure-4-damping-cards.csv](figure-4-damping-cards.csv): Figure 4.
- [sensitivity.csv](sensitivity.csv): sensitivity factors.
- [opening-card.png](opening-card.png): the article's 1200 x 627 link-preview image; its inset draws the cards of Figure 1.

## Rebuild

```
PYTHONPATH=src python examples/domains/artificial_lift/build_rod_string_article.py --refresh
PYTHONPATH=src python examples/domains/artificial_lift/build_rod_string_article.py --check
```
