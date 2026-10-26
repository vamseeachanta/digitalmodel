# F101 combined-loading form review

## Summary

**Status: failed.** Equivalence of the simplified interaction to the published
secondary-source yield criterion is not established. A zero-pressure thin-ring
hand calculation also gives a different interaction shape. This finding concerns
the form of the result of the implementation; it is neither a structural design
verdict nor a statement about a standard's text.

Retain the F101 envelope label: **simplified form; not checked against the standard's text**.
The moment-tension report remains a screening calculation that is not reconciled
with the code strategy. No strategy calculation changes accompany this record.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-26 | Record the secondary-source comparison and independent ring calculation; retain the limitation because equivalence is not established. |

Table 1. Review issues.

## Basis

The companion [JSON record](f101-combined-form-review.json) identifies the form,
source, comparison points, agreement and status with reasons. The inspected
implementation is `DnvStF101Strategy._check_combined_loading` in
`src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py`, at
revision `3a5ea5e79f1197335d032c6d6b17cb8751d9911f`. Its content digest is recorded
in JSON so that a changed strategy needs a new review.

The secondary source is David Roylance, *Yield and Plastic Flow*, **2001**,
*Modules in Mechanics of Materials*, Massachusetts Institute of Technology,
printed pages **5–6**: the principal-stress expression preceding equation (2)
and the yield locus in Figure 7. The [open module](https://web.mit.edu/course/3/3.11/www/modules/yield.pdf)
was retrieved on 2026-10-26 with HTTP 200. The institution's
[module introduction](https://web.mit.edu/course/3/3.11/www/modules/)
identifies the series as stand-alone portions of its mechanics textbook approach.
The citation is to the dated module, not an assumed page in a print edition.

This is a material-yielding source, not a pipeline local-buckling model. No
standard document was opened, read or quoted. No design-code edition is asserted.

## Implemented form under review

For positive capacities, transcription of the implementation gives:

```text
U = sqrt(m**2 + n**2 + q**2)
m = gamma_sc * gamma_m * abs(M) / M_p
n = gamma_sc * gamma_m * abs(S) / S_p
q = (p_i - p_e) / p_c
```

These expressions identify the code being reviewed; they are not attributed to
the textbook. `M` is bending moment, `S` is effective tension, `p_i` and `p_e`
are internal and external pressure, and `gamma_sc` and `gamma_m` are the
implementation's factors. `M_p` and `S_p` are its plastic moment and tension
capacities on `t2`, the nominal wall minus corrosion allowance. `p_c` is its
characteristic collapse pressure, rather than a yield-pressure normalisation.
The pressure term has no additional factor in this function. Zero or invalid
capacity branches are outside this review's mathematical comparison.

## Comparison

| Point | Observation | Agreement |
| --- | --- | --- |
| Individual load axes | The root-sum-square form reaches unity on each independently normalised axis. This does not validate the chosen capacities. | Endpoint algebra only |
| Mixed tension and bending | Independent ring equilibrium gives a cosine boundary; the implemented root-sum-square form gives a circular boundary at zero pressure. | No |
| Pressure coupling | The source's plane-stress reduction contains an axial-hoop cross term. Adding the square of net pressure divided by collapse pressure has no demonstrated equivalence to that criterion. | Not established |
| Report envelope | The report uses a separate elastic stress calculation on nominal wall with report factors; the strategy uses plastic capacities on corroded wall. | Not reconciled |

Table 2. Comparison scope and findings.

### Yield criterion from the secondary source

The principal-stress expression on page 5 is, in equivalent notation:

```text
sigma_eq**2 = ((sigma_1-sigma_2)**2 + (sigma_1-sigma_3)**2
              + (sigma_2-sigma_3)**2) / 2
```

Setting the third principal stress to zero and expanding gives the plane-stress
relation used for this comparison:

```text
sigma_eq**2 = sigma_L**2 - sigma_L*sigma_h + sigma_h**2
```

Here the longitudinal and hoop directions are principal directions in the absence
of in-plane shear. This algebraic reduction follows from the cited expression;
it is not a pipeline buckling equation. An ellipse in principal-stress space does
not establish a circular interaction in separately normalised moment, force and
collapse pressure. In particular, the axial-hoop product can change with the sign
of axial stress, whereas the implemented squares discard that distinction.
Internal/external pressure difference is not the hydrostatic stress invariant.

### Independent zero-pressure ring calculation

The following first-principles derivation assumes a thin circular ring with mean
radius `R`, wall `t`, longitudinal tensile yield stress `f`, no pressure, no strain
hardening and unit factors. It compares ideal plastic section equilibrium, not
local buckling. The full derivation is also in the test docstring.

An angular element has area `t*R*dtheta`. Let longitudinal stress be `+f` over an
arc of angle `pi + 2*alpha`, and `-f` over the remaining arc `pi - 2*alpha`.
Integration of stress over area and of stress times the bending lever arm gives:

```text
N = 4*f*t*R*alpha       N_p = 2*pi*f*t*R
M = 4*f*t*R**2*cos(alpha)       M_p = 4*f*t*R**2
n = N/N_p = 2*alpha/pi         m = M/M_p = cos(pi*n/2)
```

These capacities coincide with the implementation's thin-ring plastic capacities
when `R = (D-t2)/2` and the fabrication strength factor is unity. Setting factors
to unity isolates the shape; it does not recommend design factors.

At `n = 1/2`, the ring boundary gives `m = 1/sqrt(2)`, whereas the simplified unity
boundary gives `m = sqrt(3)/2`. The ratio is `sqrt(3/2)`: **22.5 % more moment**
on the simplified boundary, computed before rounding. Both boundaries have the
same pure tension and pure bending endpoints. The numerical assertion uses this
independent calculation, never an output of the strategy.

This counterexample disproves exact ideal plastic ring equivalence; it does not
quantify a design margin for a real pipeline. A yield model alone cannot validate
imperfections, local buckling, pressure sign limits or the factors in a pipeline
resistance model. No general bound on the simplified form is claimed.

## Label control and way forward

`wall_thickness_mt_report.py` reads the companion JSON record. A missing,
unreadable, malformed, incomplete or unrecognised record retains the simplified
form label. Only an explicit `passed` status for this form can release that label.
The independent screening/reconciliation warning remains on the contour,
bending-envelope and pressure-sensitivity chart fragments even if this particular
review later passes. Installed packages without the review file retain the label.

The current record is `failed`; passing automated tests verify that evidence and
label behaviour, not approval of the engineering form. The report's F101
combined-loading description identifies its screening calculation separately
from the strategy. Calculated values and strategy code remain unchanged.

The maintainer can obtain an open secondary source that directly treats pipeline
combined loading, its capacity definitions and applicability limits before a
passing re-review. Report-to-strategy reconciliation is separate work. A review
against a standard's text remains a data gap regardless of secondary-source
agreement.
