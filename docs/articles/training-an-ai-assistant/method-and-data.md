# Method and data

Article: "Training (and attitude towards) an AI assistant", The Deckhand Chronicles, revision 5 of 19 Nov 2026. This page holds in full what Appendix A of the article summarises.

The known-answer check. The Norris data set [1] has 36 observations and NIST certified values for a straight-line fit. Five of the certified regression statistics are checked here: the intercept, the slope, the standard deviation of each, and the residual standard deviation. Our fit reproduced all five to at least 6 significant digits. The steps are in [reusable-prompts.md](reusable-prompts.md).

The chart counts. Each count is the number of Python files under src/ in our public code at the pinned revision whose path contains, case-insensitively, the topic's substring: catenary, hydrostatic, wall_thickness, sn_curve, viv. Each path counts once per topic. The counts measure names, not how much code exists on a topic or what a model knows.

One dashboard for what matters to you. The dashboard can be one spreadsheet or page in the project folder. Add the kind of problem, corrections and known-answer result after each problem. Track your style of working, your calculations and reports, and your path to the engineer you want to become. Keep a short path note beside the instruction file. Ask the assistant to read both and help plan the next step.

Public data set: NIST Statistical Reference Datasets, Norris linear regression, NIST SRD 140, free to download without sign-in; retrieved 12 Nov 2026. Cite NIST as the source.

Code: our public [digitalmodel repository](https://github.com/vamseeachanta/digitalmodel/tree/63584454b62aa05fd5680c9148e950609389c1c2), MIT licence, supplies the chart's file counts at the pinned revision. The Norris calculation in [reusable-prompts.md](reusable-prompts.md) uses Python's standard library.

## References

1. NIST, Statistical Reference Datasets, Standard Reference Database 140. Norris linear least-squares regression, certified values and observations. Retrieved 12 Nov 2026. [Dataset page](https://www.itl.nist.gov/div898/strd/lls/data/Norris.shtml), [ASCII data](https://www.itl.nist.gov/div898/strd/lls/data/LINKS/DATA/Norris.dat), [database DOI](https://doi.org/10.18434/T43G6C).
