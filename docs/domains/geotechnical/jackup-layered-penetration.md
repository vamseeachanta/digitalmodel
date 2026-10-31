# Layered spudcan penetration analysis

The layered API calculates an illustrative vertical reaction curve for a generic
three-leg independent-leg jack-up. It retains clay punch-through, sand load
spread, sand punching shear and bounded clay squeezing. The examples use
assumption ranges; no measured regional profile or installation acceptance is
claimed. The way forward is to replace those assumptions with an investigated
soil profile and qualify the selected methods before engineering use.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-28 | Added layered mechanism screening, explicit tip geometry and reproducible inputs to support subsequent sensitivity studies. |

## Basis and public sources

Two open conference papers supply the layered equations. Their public copies
were retrieved successfully on 2026-10-28. No standard document, complete table
or section is reproduced.

| Source | Adopted equation or observation |
| --- | --- |
| [Spudcan Penetration Analyses for Three Case Histories in Sand Overlying Clay (2015)](https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-11.pdf), equations 1–3 | Load spread, sand punching shear and underlying clay bearing mechanisms; spread ratios of 1:3 to 1:5. The discussion after equation 3 describes repeating calculations at successive depths and records the omitted sand plug. |
| [Theoretical Study on the Effect of a Weak Layer on the Vertical Bearing Capacity of Spudcans in Clay (2015)](https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-19.pdf), equations 2 and 6 | Clay squeezing bounded by general shear in both layers; cylindrical punching with an upper general-shear limit. The introduction describes the approximate bottom-up treatment of multiple layers. |
| Existing [circular reference branches](../../../src/digitalmodel/geotechnical/spudcan.py) | The sand circular-plate branch and constant-strength clay reference are reused. The original research peak solver, interpolated connections and two-argument API retain their behaviour. |

SNAME T&RB 5-5A, **Rev 3 (2008), clause: TO CONFIRM**, and ISO 19905-1,
**2012 edition, clause: TO CONFIRM**, identify the intended method family.
The weak-layer paper identifies those editions in references 1 and 2. Their
clauses have not been checked against a standard copy. These references do not
claim conformity, establish current-edition coverage or qualify the equations
as a complete standard implementation.

## Equations and conventions

Depth `z` is the bearing-plane penetration below mudline. Published results from
the new API use **tip** penetration `z + tip_to_bearing_m`. A zero offset is an
explicit flat-base assumption. An offset changes the depth datum; it does not
supply a conical bearing correction. Requests before full-area engagement are
rejected. The tip itself must remain above the investigated profile bottom,
even when the bearing plane is shallower. The original API continues to report
bearing-plane depths.

`D` is equivalent circular diameter, `h` is the remaining thickness below the
bearing plane, `su` is constant undrained strength within a clay layer, and `q0`
is the effective overburden integrated from the actual layer unit weights.
Net resistance excludes `q0`; gross resistance includes it once. A gross
pressure in kPa multiplied by circular area in square metres gives leg reaction
in kN. This effective-stress bearing reaction does not include footing buoyancy
or a structural load convention correction.

The deeper net support `qL` is calculated at each interface, working upward from
the last clay layer. The following expressions are then evaluated at successive
depths. Repeating wished-in-place calculations is an approximation to penetration;
soil transport, remoulding and changing failure mechanisms are not simulated.

- **Clay general shear:** `qnet = clay_resistance_kpa(su, 0, D) = 6.34 su`.
  This preserves the existing circular reference. The weak-layer paper uses
  6.05 for its surface circular reference; that value is deliberately not
  substituted into the inherited branch. No depth factor is added to general
  shear. The resulting envelope is a declared research factor combination.
- **Strong clay over weaker support:** `qnet = min(6.34 su, qL + 3 su h/D)`.
  This is the weak-layer paper's equation 6 with `4 alpha = 3`, preserving its
  cylindrical shear term and explicit upper bound. Deeper squeezing can change
  `qL` when a third layer is present.
- **Soft clay over stronger support:**
  `qnet = min(qL, max(6.34 su, su (5 + 0.33 D/h + 1.2 z/D)))`.
  This uses equation 2 and its lower and upper general-shear bounds. As `h`
  tends to zero the bounded result tends to `qL`; no infinite pressure is
  emitted. Reusing 6.34 changes the onset relative to the paper's reference.
- **Sand load spread:** `qgross = (1 + 2 h/(D n))² (qL + q0)`, with horizontal
  to vertical spread `1:n`. This implements sand-paper equation 1 as written.
- **Sand punching shear:**
  `qgross = qL + (2 h/D) (gamma_s h + 2 q0) Ks tan(phi) + q0`.
  This implements sand-paper equation 2. `Ks` must be supplied explicitly;
  no coefficient is read from a standard chart. Both sand mechanisms are
  capped by the existing homogeneous circular sand branch at that depth.

The clay equations admit any number of contiguous constant-strength layers.
Only one surface sand layer over clay is supported. Sand `H/D <= 1` and
friction angles from 20.0 to 40.0 degrees are illustrative shallow screening
restrictions, assumed, based on previous experience. They are not published
calibration bounds for the layered equations. A homogeneous sand profile uses
the existing separate reference API.

## Callable interface and screening semantics

```python
from digitalmodel.geotechnical.spudcan import (
    LayeredProfile, SoilLayer, Spudcan, penetration_curve,
)

profile = LayeredProfile((
    SoilLayer('clay', 0, 3, 7, su_kpa=80),
    SoilLayer('clay', 3, 24, 6, su_kpa=15),
))
spudcan = Spudcan(diameter_m=12, tip_to_bearing_m=1.5)
curve = penetration_curve(profile, spudcan, [1.5, 4.5, 24.0], preload_kn=30000)
```

The example values above are assumptions, based on previous experience; their
ranges are in the JSON examples. Supply tip depths spanning the interval of
interest. Layer boundaries, general-shear switches, squeeze caps, sand cap
intersections and internal sand stationary points are inserted within that
interval. A coarse input grid therefore retains the envelope's descending
branches. No point is added outside the requested interval or beyond the profile.

`curve.points` contains pressures, leg reactions and mechanism labels.
`curve.peak` identifies the first descending peak, while `curve.maximum` retains
any larger recovered resistance at depth. Without a capacity loss, both identify
the interval maximum. `post_peak_drop_kn` is the first peak-to-trough loss;
`events` retains every loss, its peak and trough tip depths, percentage loss,
and static upward crossings of the peak and preload reactions. A higher earlier
capacity shields a later event from a smaller applied preload. Equality with a
descending peak is treated as reaching it.

`punch_through_flag` is unresolved (`None`) when preload is omitted. With preload,
it indicates that a descending peak is reached within the evaluated interval.
`preload_support_depth_m` separately records the first upward resistance crossing;
it is `None` if the evaluated profile supplies no support. A surface resistance
already above the load returns the first evaluated depth. An upward discontinuity
crosses at its interface. A false loss flag alone does not establish support.
A restricted interval says nothing about omitted shallower or deeper mechanisms.

Recovery depths are static resistance crossings, not dynamic plunge or arrest
depths. Exact mechanism equations are used for those crossings, rather than
interpolation between sparse user samples. Missing recovery remains explicit;
no extrapolation below the supplied interval is performed.

## Generic scenario inputs and outputs

The three [JSON profiles](../../../examples/data/jackup_layered_penetration/)
are **assumed, based on previous experience**, including their numerical ranges.
They illustrate possible offshore regional stratigraphies without asserting that
a particular regional site has those properties. No publicly verified regional
investigation is in hand. Each numeric scenario parameter has a declared range;
method sources are distinguished from scenario assumptions. Ranges are independent
sensitivity bounds and do not guarantee that all combinations form valid layers.

The stiff-over-soft profile includes a deeper strong clay layer to exercise
squeezing within a strong–soft–strong sequence. The other cases are surface sand
over soft clay and a uniform soft clay control. The sand example compares both
methods using an assumed punching coefficient of 1.5, with range 0.5 to 3.0.
This coefficient is not a standard chart value or a calibrated prediction.

```bash
PYTHONPATH=src uv run --no-project --with pytest python \
  examples/domains/geotechnical/jackup_layered_penetration.py OUTPUT
```

For each profile the script writes `.input.json`, `.curve.csv`, `.summary.json`
and `.figure.json` files together in the supplied output directory. CSV includes
both tip and bearing-plane depths, gross pressure, leg reaction, preload and
mechanism. Sand CSV and figures contain both methods. Plotly figure specs carry
reaction curves, preload lines, peak/trough markers, capacity-loss segments and
assumption metadata. Their generation uses the standard library and performs
no network requests; a report can embed them using a locally bundled Plotly
renderer. Machine-readable output retains calculation precision.

The following figures are from the default assumed cases, with 30.0 MN preload
per leg. They are demonstration results, not site limits.

| Case and method | First descending peak or interval maximum (MN) | Tip depth (m) | First capacity loss (MN) | Preload reaches a loss |
| --- | --- | --- | --- | --- |
| Stiff-over-soft clay, with deeper strong clay | 17.5 | 1.5 | 4.4 | Yes |
| Sand over soft clay, load spread | 21.4 | 1.5 | 2.6 | Yes |
| Sand over soft clay, punching shear | 19.0 | 4.7 | 0.1 | Yes |
| Uniform soft clay control | 26.0 | 24.0 | 0.0 | No |

The control does not support 30.0 MN within its evaluated extent despite having
no descending branch. That distinction is retained in the support-depth output.
The two sand methods can predict different peak depths and capacity losses;
no averaging or preferred prediction is inferred from this comparison.

## Evidence and known limits

The pre-change spudcan baseline comprised 47 passing tests. New tests use hand
calculations written independently in the test file for both sand equations,
clay punching, bounded squeezing, effective overburden, tip conversion, load
conversion and nonlinear static recovery. They also cover multiple losses,
shielding, unsupported preload, profile limits, coarse-grid transitions and
portable offline export. No expected capacity is copied from the implementation.
The inherited public-example regression tests remain unchanged.

This is a static, vertical, full circular-area screen. It excludes partial
engagement, installation dynamics, soil plugs, cyclic changes, remoulding,
consolidation, scour, inclined/combined loading, footprint interaction and load
redistribution among legs. Constant-strength layer boundaries can create abrupt
changes. Thick strong-over-weak clay and three-layer interactions are approximate;
the weak-layer paper documents limitations of the simple punching and squeezing
methods. A datum offset alone does not qualify an actual spudcan shape.

Confirm standards clauses and the required edition, obtain an investigated soil
profile, justify punching coefficients and bearing factors, then assess actual
geometry, buoyancy, loads, installation procedure and dynamics. Independent
engineering review and comparison with suitable published or measured penetration
curves remain required before design or operational use.
