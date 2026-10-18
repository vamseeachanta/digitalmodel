# Depth classification and shallow-water screening

`classify_depth(h, T)` uses the PIANC (1992) bands reported in [1, section 1]
and independently in [2, Appendix A.1, p. 201]. Neither a PIANC Report 121
clause nor DNV-RP-C205 is claimed as the depth-band source. The original
1992 report was not directly verified. The citation registry exposes
`get_pianc_depth_band_reference()` using the existing fail-closed wiki pattern;
a maintainer must provision its live wiki record. The test fixture validates
schema/resolution only and is not a live record.

| API class | h/T |
| --- | --- |
| `DEEP` | > 3.0 |
| `MEDIUM` (medium-deep) | > 1.5 and <= 3.0 |
| `SHALLOW` | > 1.2 and <= 1.5 |
| `VERY_SHALLOW` | <= 1.2 |

The sources give strict inequalities; assigning exact edges to the shallower
class is this implementation's convention. Positive infinity denotes deep
water; NaN, nonpositive depth, and nonfinite/nonpositive draft are rejected.
This is classification only, not an under-keel-clearance assessment.

The change affects labels and bank-clearance screening: h/T=1.8 is now medium
with no depth amplification; 1.4 is shallow. The x1.2 shallow and x1.5 very
shallow bank-clearance multipliers are **uncited engineering assumptions**,
not PIANC requirements. They remain for preliminary screening only.
`pianc_bank_clearance_width(..., return_metadata=True)` exposes the width,
depth class, applied multiplier and its uncited status; the default returns
metres as before. All consumers use the central classifier; no legacy bands
are retained.

The preliminary Nomoto estimator and IMO turning check take optional
`water_depth_m` and `draft_m`. Supplied h/T <= 4 raises `ValueError`, following
[3, Annex 2.1.2]; for Nomoto this is a conservative applicability screen,
not a validated coefficient range. Missing depth leaves an explicit warning
and `depth_applicability="unverified"`. Numerical IMO pass flags with unknown
depth do not establish compliance. Passing the depth screen does not check
bank confinement or the remaining trial conditions. Both YAML workflows read
`environment.water_depth_m` and propagate draft from their configuration.

Published tactical-diameter values from [1] are reproduced below as trend
evidence, not fitted by these deep-water estimators:

| Rudder | h/T=3.0, diameter/Lpp | h/T=1.2, diameter/Lpp | Ratio |
| --- | --- | --- | --- |
| 40 degrees | 1.75 | 6.25 | 3.57 |
| 10 degrees | about 5 | about 22 | about 4.4 |

The 40-degree values are in section 4; the 10-degree values are in section
3.2. These results are specific to the published model. No universal depth
correction is implemented. Tests verify the bands, clearance values, invalid
inputs, guards and workflow propagation using synthetic inputs. No reusable
data or figures are bundled. The 11.54 MB article was checked from its
CC BY 4.0 public archive record: https://hdl.handle.net/20.500.11970/116828.

References:

1. Rzeszutko J., Zentari L., Stachel H., el Moctar O. (2026).
   A regression-based maneuvering model for inland waterway ships in shallow
   rivers with real bathymetry and current fields. *Applied Ocean Research*
   176, 105289. https://doi.org/10.1016/j.apor.2026.105289.
2. ITTC Manoeuvring Committee (2002). Final Report and Recommendations,
   *Proceedings of the 23rd International Towing Tank Conference*, volume I,
   Appendix A.1, p. 201. https://www.vliz.be/imisdocs/publications/139636.pdf.
   Reports and attributes the depth bands to PIANC (1992).
3. International Maritime Organization (2002). MSC/Circ.1053, *Explanatory
   Notes to the Standards for Ship Manoeuvrability*, Annex 2.1.2.
   https://www.traficom.fi/files/media/regulation/22701-MSC_1-Circ_1053_-_tulkinta_ship_manoeuvrability.pdf.
