# Ship squat and under-keel-clearance screening

## Summary

The screening module evaluates four empirical formulations, returns the
applicability reason for every unavailable result, and subtracts explicit
allowances from the available water depth. The synthetic example below exceeds
its selected model budget at the highest speed under one method. Reducing
speed restores that method's selected criterion. Method disagreement remains
visible; no formula is ranked as best. Operational clearance and navigation
remain `NOT_EVALUATED`.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-22 | Add sourced formula screening, explicit UKC budgets and a synthetic example to expose shallow-water clearance sensitivity. |
| 1 | 2026-10-25 | Identify the 1981 equation's bow-only prediction so comparisons and UKC budgets retain its known end and unavailable stern. |
| 2 | 2026-10-27 | Record the test command and normal-package integration coverage to distinguish executed checks from predictive validation. |

## API and inputs

Implementation:
[squat_screening.py](../../../src/digitalmodel/hydrodynamics/parametric_hull_analysis/squat_screening.py).
The separate filename avoids the files in the existing squat draft; consolidation
of the two APIs remains a maintainer decision.

Import `SquatInputs`, `Formula`, `UKCBudget`, `ukc_table`, `maximum_speed` and
`compare_formulas` from
`digitalmodel.hydrodynamics.parametric_hull_analysis.squat_screening`.
No licensed solver is needed. The existing package imports its normal scientific
Python dependencies; the new calculation module uses the standard library.

All hull inputs are required: length between perpendiculars, beam, static
draught, block coefficient, water depth at the datum, equivalent rectangular
channel width, speed **through water**, gravitational acceleration and physical
channel kind (`OPEN`, `DREDGED` or `CANAL`). Dimensions are metres, speed is m/s,
and acceleration is m/s². Invalid or nonfinite inputs raise `ValueError`.
Static grounding, inapplicable empirical ranges and critical speeds return
`NOT_EVALUATED` with no numerical squat or clearance verdict.

Supplied corrections must correspond to the effective depth and draught after
tide and density adjustments; a source string alone does not qualify them.

`blockage_proxy = Cb*B*T/(W*h)` is exposed with a proxy open/restricted label.
The division at 0.1 adapts the open-channel discussion in [1, A1] as an explicit
implementation convention. Neither opened source defines midship area as
`Cb*B*T`; therefore this proxy is **not** substituted into an empirical formula.
`area_blockage = B*T/(W*h)` follows the rectangular approximation in [2, (2)].
The physical channel kind is independently required. An open label with area
blockage at least 0.1 is conservatively rejected. In open water the supplied
width must be an independently selected effective width [2, section 5]; the
module does not infer it. Neither label identifies the side slopes or trench
height of a dredged channel.

## Methods and applicability

All methods require `Fnh=V/sqrt(g*h)<1` [1, Appendix B; 2, section 3]. Numerical
range edges are included by implementation convention. The depth ranges and
block-coefficient ranges below come from [2, Table 1]. Missing bounds are not
evidence of universal applicability. Every result retains the evaluated inputs,
source identifiers, applicability description and supplied corrections.

| Method | Equation and coefficient source | Screening range and limitations |
| --- | --- | --- |
| Barrass 1979/1981 | `Sb=Cb*(S/(1-S))^(2/3)*Vk^2.08/30`, `S=B*T/(W*h)`; [2, section 6, (2), (4)-(5)] | `1.1<=h/T<=1.5`, `0.5<=Cb<=0.9`; all channel kinds; supplied effective width in open water. Bow squat only; stern and trim are unavailable. |
| Romisch 1989 | `Sb=Cv*Cf*K*T`, `Ss=Cv*K*T`; `Cv=8*r²*((r-0.5)^4+0.0625)`, `Cf=(10*B*Cb/Lpp)²`, `K=0.155*sqrt(h/T)`; [1, A4 (7)-(10)] | `1.19<=h/T<=2.25`, `V<Vcr`; both ends and change in draught difference. Open `Vcr=sqrt(g*h)*0.58*(h*Lpp/(B*T))^0.125` [1, (11)]. Restricted critical speed and its source must be supplied. |
| Huuska/Guliev, open ICORELS equivalent | `Sb=2.4*Cb*B*T/Lpp*Fnh²/sqrt(1-Fnh²)*Ks`; [1, A3 (6)] | `1.1<=h/T<=2.0`; open `Ks=1` [1, A3]; restricted `Ks` and source are required because trench geometry is absent. Applying the HG depth range to the open equivalent is conservative implementation policy; Table 1 gives no numerical ICORELS bounds. Stern is unavailable. |
| Eryuzlu 1994 | `Sb=0.298*h²/T*(V/sqrt(g*T))^2.289*(h/T)^(-2.972)*Kb`; [1, A2 (5)]. Dredged width factor `Kb=3.1/sqrt(W/B)` below `W/B=9.61`, otherwise 1 [2, (8)]; open `Kb=1` [1, A2]. | `1.1<=h/T<=2.5`, `Cb>=0.8`; open or dredged, excluding canals; intended for bulbous-bow cargo/bulk hulls [2, section 6]. Hull-family suitability must be confirmed externally. Stern is unavailable. |

`Vk` is knots; the conversion uses one nautical mile of 1852 m per hour [3],
with the hour defined as 3600 seconds. The 1981 formulation is distinct from
the later B3 formula in [1]; its end-position rule is not borrowed from B3.
For Eryuzlu, the exponent and SI speed in the visibly checked equation [1, (5)]
are used: [2, (7)] contains a conflicting exponent and speed symbol.

`maximum_m` is the largest **predicted** displacement. For bow-only methods,
it is a bow prediction and does not establish the unmodelled stern clearance.
The 1981 result retains `bow_m=maximum_m`, `governing_end="bow prediction only"`,
and unavailable `stern_m` and `trim_change_m` in comparisons and UKC rows.
Romisch's `trim_change_m` is `Sb-Ss`, positive bow down, in metres. It is not an
angle. Initial trim is assumed zero. A full-form synthetic hull has bow-governing
squat; a fine-form synthetic hull can have stern-governing squat under its
shape correction. No full-form threshold is transferred between formulations.

## UKC accounting and speed search

The geometric budget follows [2, (1)]:

```text
effective depth   = depth at datum + tide
effective draught = static draught + density correction
net UKC           = effective depth - effective draught - predicted squat
                    - wave allowance - heel allowance
margin            = net UKC - supplied minimum UKC
```

Every allowance and the minimum criterion are required inputs. A zero value
must be supplied explicitly. Positive tide increases depth; positive density
correction increases draught. Squat is recomputed using both corrections.
Negative residual UKC is retained, and an unavailable squat produces an
unavailable verdict. If corrections yield nonpositive dimensions, inputs are
invalid and raise `ValueError`.

Wave allowance must be an externally assessed downward hull-motion excursion;
it is not wave height. Heel allowance must be an assessed additional immersed
depth. Combined allowances must avoid double counting. A heave RAO alone does
not define a probabilistic extreme or the pitch/roll response at either end;
no automatic wave allowance is inferred here.

`density_draught_correction(rho, FWA)` interpolates a supplied fresh-water
allowance from a salt-water reference draught using [4, section 8(1)]. The
reference densities are 1000 and 1025 kg/m³; the correction is
`FWA*(1025-rho)/25`. Densities outside that interval are rejected. The helper
does not derive FWA from block coefficient, model temperature, or establish
load-line compliance. Do not add this correction if the draught is already
corrected for the water density.

`maximum_speed` requires a selected formula, budget, search cap and numerical
tolerance. It returns the passing lower bracket of a monotonic bisection, or
identifies a passing supplied cap, an infeasible zero-speed budget, or an
inapplicable interval. A cap outside the method envelope returns
`NOT_EVALUATED`; no empirical or critical-speed boundary is crossed. This is a
screened model limit for the supplied criterion, not permission to navigate.
The required minimum must come from an independently qualified assessment;
the generic positive-margin guidance in [2, section 2] is not an adopted value.

## Synthetic dredged-channel example

[Runnable example](../../../examples/hydrodynamics/squat_screening.py) and
[independent arithmetic tests](../../../tests/hydrodynamics/parametric_hull_analysis/test_squat_screening.py)
provide a synthetic bulk-carrier case. These inputs are illustrative assumptions,
not measured particulars or qualified channel data.

| Input | Supplied value |
| --- | --- |
| Lpp / beam / reference draught | 240.0 / 30.0 / 10.0 m |
| Block coefficient | 0.8 |
| Depth at datum / equivalent width | 14.4 / 120.0 m |
| Physical channel kind | Dredged |
| Gravity | 9.81 m/s², supplied |
| Speeds through water | 2.0, 4.0, 6.0 m/s |
| Tide / wave allowance / heel allowance | 0.6 / 0.4 / 0.1 m |
| Density / supplied fresh-water allowance | 1012.5 kg/m³ / 0.4 m |
| Density draught correction | 0.2 m |
| Minimum net UKC criterion | 3.0 m, illustrative required input |
| Restricted critical speed / Huuska width factor | 8.0 m/s / 1.2, illustrative required inputs requiring independent qualification |

Effective depth is 15.0 m, draught 10.2 m, and the clearance remaining before
squat is 4.3 m. For the equal Romisch shape factors here, bow and stern squat
are equal. The largest predicted displacement and the resulting UKC are:

| Speed (m/s) | Barrass squat / UKC (m) | Romisch squat / UKC (m) | Huuska squat / UKC (m) | Eryuzlu squat / UKC (m) | Formula spread (m) |
| --- | --- | --- | --- | --- | --- |
| 2.0 | 0.2 / 4.1 PASS | 0.1 / 4.2 PASS | 0.1 / 4.2 PASS | 0.1 / 4.2 PASS | 0.1 |
| 4.0 | 0.7 / 3.6 PASS | 0.2 / 4.1 PASS | 0.3 / 4.0 PASS | 0.4 / 3.9 PASS | 0.4 |
| 6.0 | 1.5 / 2.8 FAIL | 0.6 / 3.7 PASS | 0.8 / 3.5 PASS | 1.0 / 3.3 PASS | 1.0 |

Verdicts use unrounded numbers and apply only to each method's predicted
displacement. Spread is the maximum minus minimum model output, not an error
bar. It is withheld if any requested method is unavailable. No average or
favoured method is selected. A 6.0 m/s search cap and an explicitly supplied
numerical tolerance place the Barrass criterion boundary between 5.5 and
5.6 m/s; 5.5 m/s is the downward-rounded screened value. The other formulations
pass through the supplied cap, which does not establish their clearance boundary.

One independent arithmetic check uses `V/Vcr=0.5`, `h/T=1.44`, `T=10 m` and
`Cf=1`: [1, (8)] gives `Cv=0.125`, [1, (10)] gives `K=0.186`, and hence
`S=0.2325 m`. Tests retain that precision to check transcription; report tables
round comparisons to one decimal place. The example tests also write out the
reduced expressions for all twelve predictions independently of the module.

To proceed beyond screening, qualify the channel geometry and corrections,
hull-family suitability, motions at both ends, actual draught/trim and the
operational UKC criterion. In particular, an unknown restricted critical speed
or Huuska width factor must remain `NOT_EVALUATED` until supplied with evidence.

## Evidence and known limits

Tests cover every slice: required fields and geometry; four independent
formula calculations and speed monotonicity; full/fine bow-versus-stern cases;
density/tide/motion/heel accounting and a speed root; formula spread and
critical-speed negative controls; and all twelve worked-example predictions.
They check transcription and accounting, not measured prediction accuracy.

The normal package imports and repository pytest configuration were exercised
on 2026-10-27 with Python 3.12.15 and pytest 9.1.1: all 299 parametric-hull cases
passed, comprising 140 screening cases and 159 existing cases, with no skips.
With the scientific dependencies installed and a temporary directory outside
the checkout supplied in `TEST_TMP`, the command is:

```sh
PYTHONPATH=src python -m pytest tests/hydrodynamics/parametric_hull_analysis -q -rA -p no:cacheprovider --basetemp="$TEST_TMP"
```

The standalone example also ran through the normal package imports. These
checks do not cover the full repository suite or measured squat data.

Restricted critical-speed derivation, automatic trench correction, effective
open-water width selection, authenticating supplied references, end-motion RAO
integration and measured validation remain follow-up work. The Cb-weighted
blockage proxy lacks a matching definition in the opened sources, so it remains
explicit assumed metadata. The original empirical publications, Report 121 and
the stability textbook were not directly verified. No licensed publication is
reproduced. The public technical-note download was unavailable; no coefficient
was recovered from memory or solely from its search snippet.

## Sources

1. [PIANC On Course 142 (January 2011)](https://pianc-jp.org/news/images/On%20Course%20142.pdf),
   Appendix A, printed pp. 22-23, equations (5)-(11); Appendix B symbols.
   Formula images were visually checked. This is the accessible guidance
   reproduction used here, not a claim to have checked Report 121 itself.
2. [Brodogradnja 68(2), 2017, DOI 10.21278/brod68201](https://brodogradnja.fsb.hr/wp-content/uploads/sites/25/2023/01/No2_1-11.pdf),
   sections 2, 3, 5 and 6; equations (1)-(17), Table 1.
3. [NOAA Tides & Currents glossary](https://tidesandcurrents.noaa.gov/glossary.html),
   entry “knot”, international nautical mile per hour.
4. [Load Line Regulations, section 8(1)](https://laws-lois.justice.gc.ca/eng/regulations/SOR-2007-99/section-8.html),
   fresh-water allowance interpolation. Only that interpolation is used.

All four sources were opened on 2026-10-22. Source responses, request addresses,
UTC timestamps and HTTP status were retained in temporary evidence storage;
source documents are not bundled with the package.
