# Production engineering method selection

## Summary

The selector provides a deterministic IPR choice for a specified oil-well
pressure interval. Published VLP methods remain unavailable. A request for a
physical VLP regime raises `NotImplementedError`; only an explicit oil screening
request returns the existing simplified traverse. Selection establishes neither
calibration nor numerical validation against a published example.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-14 | Define selection decisions and expose implementation gaps to prevent misleading VLP substitution. |
| 2 | 2026-10-18 | Add optional selection assessments with source locators and explicit unevaluated qualification checks. |

## IPR comparison and policy

The primary choices below are engineering screening rules inferred from the
source bases in the [literature survey](production-engineering-literature.md).
They are not a universal accuracy ranking. Parameters are fitted or supplied by
the caller; the selector does not estimate them.

| Condition | Primary choice | Rationale | Existing implementation |
| --- | --- | --- | --- |
| Valid multirate test available for an oil well | Fetkovich | Use measured rate-pressure behavior rather than an assumed normalized curve. The fitted coefficient and exponent are required when constructing the model. | `ipr_models.FetkovichIpr` |
| Entire flowing-pressure interval at or above bubble point | Linear PI | Single-phase inflow is the basis of the PI relation discussed by Vogel. The equality boundary has no below-bubble-point interval. | `ipr_models.LinearIpr` |
| Reservoir above bubble point, interval extends below bubble point, confirmed solution-gas drive | Composite | Retain the linear segment and join a Vogel extension below bubble point. This is the existing implementation's piecewise construction; its secondary-source attribution remains unverified. | `ipr_models.CompositeIpr` |
| Reservoir at or below bubble point, confirmed solution-gas drive | Vogel | Use a normalized solution-gas-drive IPR, rather than extend a constant PI through two-phase inflow. | `ipr_models.VogelIpr` |
| Gas, condensate, explicit three-phase fluid, significant skin, or unconfirmed drive below bubble point | No automatic choice | These cases exceed the qualified scope of this policy. Absence of evidence does not select a default oil model. | Exception |

Precedence is unsupported-fluid/skin rejection, fitted multirate model,
single-phase interval, then the two solution-gas-drive cases. A damaged well is
not automatically qualified by the presence of multirate data. Forecasting a
depleted reservoir from an earlier test is outside this selector.

## VLP comparison and policy

| Regime | Literature candidate | Rationale and limit | Selector behavior |
| --- | --- | --- | --- |
| Vertical oil well | Beggs-Brill as a candidate; Hagedorn-Brown for comparison | Aggour et al. rank Beggs-Brill first over their combined dataset; Hagedorn-Brown performs better in its high-water-cut subset. Neither finding establishes a universal default. | `vertical_oil` raises `NotImplementedError`. |
| Inclined oil well | Beggs-Brill | Its defining record explicitly covers inclination; the original Hagedorn-Brown experimental basis is vertical. | `inclined_oil` raises `NotImplementedError`. |
| Vertical gas/condensate with liquid | Gray | The open Marlim3 documentation identifies this application. The corrected-versus-original revision basis and a published validation example remain unresolved. | `gas_condensate` raises `NotImplementedError`. |
| Exploratory oil screening | Simplified homogeneous traverse | Existing fixed-slip approximation; useful only as an explicitly requested exploratory calculation. It is neither of the named published correlations. | `screening`, with `phase="oil"`, returns `simplified_homogeneous`. |

For physical design, no implemented primary VLP choice is established. The
literature candidates require implementation, source-based range checks and
calibration against measured pressure surveys before selection for design.
Vohra et al. find that no correlation leads across every tested range. The
deprecated names `hagedorn_brown` and `beggs_brill` therefore never appear as
selector results.

## Interface

`src/digitalmodel/production_engineering/method_selector.py` exports:

- `select_ipr_model(reservoir_state, fluid_props) -> str`
- `select_vlp_correlation(regime, fluid_props) -> str`

Both property arguments are mappings. The IPR state requires finite numeric
`reservoir_pressure_psi`, `bubble_point_psi` and `minimum_pwf_psi`. The first two
are positive; minimum flowing pressure is nonnegative and cannot exceed
reservoir pressure. Boolean values are rejected as pressures. The optional
`multirate_test_available` flag defaults to false. The fluid requires
`phase="oil"`; `solution_gas_drive` and `significant_skin` are optional booleans
defaulting to false. A missing drive flag is therefore unconfirmed drive.
Malformed numeric inputs and flags raise `ValueError`.

IPR names map to the classes listed above; they are policy results, not instances
with fitted parameters. The VLP result can be passed to `vlp_curve` as its
`correlation` argument. Unknown regime strings raise `ValueError`. No fluid
property, geometry, correlation coefficient or standard-derived value is
invented by the selector.

`tests/production_engineering/test_method_selector.py` exercises the choices,
bubble-point boundaries, malformed pressures, absent drive evidence,
unsupported fluids and refusal to substitute published-method aliases.

### Selection evidence

The same module additionally exports:

- `assess_ipr_selection(reservoir_state, fluid_props) -> SelectionAssessment`
- `assess_vlp_selection(regime, fluid_props) -> SelectionAssessment`

These call the existing selectors, so their successful names and precedence
are unchanged. The immutable result contains `method`, `rationale`, `sources`,
`open_checks`, and `qualification_status`. Sources are passage locators into
the literature survey or an implementation inventory, not accuracy scores.
All results carry `qualification_status="NOT_EVALUATED"`, including successful
screening selections. This status concerns engineering qualification, not
whether a screening decision was made.

Unsupported physical cases have `method=None` and the original rejection
reason. Unknown regimes and malformed pressures/flags still raise `ValueError`.
The original string APIs continue to raise `NotImplementedError` for unsupported
physical cases. Callers needing an executable choice must check `method`;
neither its presence nor the metadata establishes design approval.

IPR assessments record numerical applicability, parameter fit/field calibration
and original-paper example validation as unevaluated. VLP assessments record
published-method implementation/ranges, pressure-survey calibration and
original-paper validation as unevaluated. No numerical threshold or standard
value is inferred from a bibliographic abstract. The fitted Fetkovich parameters
remain required when constructing that model, even with declared multirate data.

The new `test_selection_assessment.py` includes independent synthetic arithmetic
using the opened passages listed in the literature survey. It checks the
Vogel saturated boundary and composite bubble-point boundary, metadata gaps,
unsupported cases, immutability and preserved input errors.

## Remaining evidence

The literature survey records unread full texts and unverified detailed ranges.
The selection policy does not complete the published-example validation work,
the Gray implementation or the validated nodal plot. Those items remain under
the existing validation issue.
