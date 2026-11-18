# Two numerical methods for the pump card

Dynacard article ("Dynamometer card analysis: from the surface card to the pump card and a diagnosis"), revision 4 of 18 Nov 2026. This page holds in full what Appendix B of the article summarises. `examples/domains/artificial_lift/build_dynacard_article.py` writes it from the saved results.

Purpose:

- Describe the two methods that compute the pump card from the surface card.
- Compare their pump cards on the public example well, and state what the comparison means.

## Key conclusion

- On the public example well the pump cards of the two methods differ by 0.26 % of the load range, and each is within 0.78 % of the card stored in the well record (Table 1).
- For this problem, at this accuracy, the methods are equivalent. All results in the article use the default method.
- As more types of card come in, the two methods are kept side by side: some physical behaviour may be captured better by one method than by the other.

## Methods

- Finite-difference method, the default: it marches the damped wave equation for axial motion through the tapered rod string, after Everitt and Jennings (1992), [SPE-18189-PA](https://doi.org/10.2118/18189-PA).
- Frequency-domain method: it solves the rod model of Gibbs (1963), [SPE-588-PA](https://doi.org/10.2118/588-PA), harmonic by harmonic, with 15 harmonics, and treats the well as vertical.
- The well record also stores a pump card from the source system's own finite-difference calculation.

## Comparison on the example well

| Pump card | Compared with | Load difference (% of load range) |
| --- | --- | --- |
| Frequency domain | Finite difference (default) | 0.26 |
| Finite difference (default) | Card stored in the well record | 0.78 |
| Frequency domain | Card stored in the well record | 0.74 |

Table 1. Root-mean-square load difference between pump cards.

The stored card uses damping close to that of the default method, so its agreement with that method is a regression check; the frequency-domain method is the independent one.

The method and the checks of both solvers are given in sections 3 and 4 of the [full study](https://github.com/vamseeachanta/digitalmodel/blob/7d02ae07248b038dcdec5e7097565c02fcd152f8/docs/domains/artificial_lift/dynacard-pump-card-report.md).
