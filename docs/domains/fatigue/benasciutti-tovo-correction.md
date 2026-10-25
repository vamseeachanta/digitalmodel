# Benasciutti-Tovo spectral fatigue correction

The moment-based `benasciutti_tovo_damage` API uses the fitted BT2 weight and
reduces the narrow-band estimate for broad-band spectra. Qualification covers
the published band-limited comparison, an independent scalar calculation and
the narrow-band limit. It establishes numerical agreement for those cases;
engineering acceptance factors remain caller inputs.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Replace the increasing wide-band factor with BT2 and qualify its reduction bounds. |
| 2 | 2026-10-24 | Account for both moment ratios in the boundary roundoff check and independently check the equivalent range. |

## Method and sources

The public paper [Metals 11 (2021), 1333](https://doi.org/10.3390/met11091333)
provides the equations and benchmark. Its
[open PDF](https://mdpi-res.com/d_attachment/metals/metals-11-01333/article_deploy/metals-11-01333.pdf)
was retrieved successfully. Section 4, Eq. (11), gives

```text
alpha1 = m1 / sqrt(m0*m2)
alpha2 = m2 / sqrt(m0*m4)
lambda = w + (1-w)*alpha2**(b-1)
D_BT = lambda * D_NB
```

The moment ratios are defined in Eq. (2). The fitted weight in Eq. (13) is

```text
w = (alpha1-alpha2) * (
    1.112*(1+alpha1*alpha2-alpha1-alpha2)*exp(2.11*alpha2)
    + alpha1-alpha2
) / (1-alpha2)**2
```

Both fitted coefficients come from Eq. (13). The implementation factors
`1+alpha1*alpha2-alpha1-alpha2` into `(1-alpha1)*(1-alpha2)` to avoid
subtraction of nearly equal terms. For a spectral line, `alpha1=alpha2=1`;
Eq. (11) gives `lambda=1` directly, avoiding the undefined weight expression.
For positive moments satisfying `0 < alpha2 <= alpha1 <= 1` and slope `b >= 1`,
the mixture has `alpha2**(b-1) <= lambda <= 1`. This slope restriction follows
from the exponent in Eq. (11); it is not an engineering acceptance criterion.
Inputs outside this domain, or nonfinite moments and S-N parameters, raise
`ValueError`. Each moment ratio has a roundoff allowance of one floating-point
unit at unity. Their ordering check combines both allowances, so scaled
spectral-line inputs reach the narrow-band limit despite opposite rounding
errors. Inconsistencies beyond these allowances are rejected.

## Worked example

Run the [executable example](../../../examples/domains/fatigue/benasciutti_tovo_example.py)
from the repository root:

```sh
PYTHONPATH=src uv run python examples/domains/fatigue/benasciutti_tovo_example.py
```

The inputs describe a synthetic unit-area rectangular PSD with an upper to
lower frequency ratio of five, as in Table 1 of the paper. The normalization
uses unit stress variance, `m0=m2=1`, slope `b=3` and range-based intercept
`A=1`. These are analytical normalization choices, not material properties.
Integrating a unit-area rectangle on `[1,5]` gives
`M_j=(5**(j+1)-1)/(4*(j+1))`. Rescaling angular frequency by `sqrt(3/31)` gives

```python
m0 = 1
m1 = 3 * sqrt(3/31)
m2 = 1
m4 = (781/5) * (3/31)**2
sn_slope = 3
sn_intercept = 0  # log10(A), A=1
```

The resulting correction is **88.6 %** of narrow-band damage, a **11.4 %**
reduction. Narrow-band damage is **4.8 per second** and BT2 damage is
**4.2 per second** for this normalized example. The equivalent constant range
at the reported peak rate is **2.8 stress units**. Large normalized damage
rates are a consequence of `A=1`; this example is not a material assessment.

For an independent calculation, Eq. (5), converted from stress amplitude to
range, gives `D_NB=6*sqrt(2/pi)`. Substitution into Eqs. (11) and (13) gives
the correction and damage above. Full-precision arithmetic anchors are written
out in the test docstrings, rather than inferred from implementation output.
The equivalent range satisfies `D_BT=nu_p*S_eq**b/A` before display rounding.

Table 1 prints `D_TB/D_DK` for this spectrum and slopes three, five and seven.
The tests independently evaluate the Dirlik denominator from Eqs. (8) and (9),
then compare BT2 with the published ratio. The permitted discrepancy is half
the last printed decimal place. That tolerance tests published rounding only;
it is not a fatigue design margin. The production Dirlik implementation is
not used to construct the expected BT2 result.

## Qualification and limits

- Slice 1: replace the increasing factor with the sourced BT2 weight and
  damage correction; reproduce the published broad-band comparison.
- Slice 2: check the exact narrow-band limit, convergence from rectangular
  spectra, finite nonnegative damage, correction bounds and invalid inputs.
- Slice 3: supply the worked example, source traceability and a consistent
  equivalent range without scaling an already rounded narrow-band range.

The unchanged narrow-band and Dirlik tests remain part of the validation.
The qualification assumes stationary Gaussian loading and a single-slope S-N
curve without an endurance limit, as described in Sections 2 and 4 of the
paper. It does not establish accuracy for non-Gaussian or nonstationary loading,
multislope curves, mean-stress effects or arbitrary spectral shapes.

The existing PSD helper integrates powers of frequency in Hz, whereas the
moment model's rate properties divide by `2*pi`. This example supplies angular
moments directly so its absolute rates have the stated units. The correction
ratios are invariant under a consistent frequency rescaling; the Hz helper's
absolute-rate convention is outside this change. The separate
`structural.fatigue.frequency_domain.TovoBenasciuttiMethod` implementation is
also outside this qualification. The `duration` argument remains accepted for
compatibility; the API returns damage per second, not cumulative damage.

Qualification includes scaled spectral-line limits and an equivalent-range
oracle using the scalar damage anchor and analytical peak rate. No fatigue
acceptance factor or licensed-standard coefficient is selected by this change.
