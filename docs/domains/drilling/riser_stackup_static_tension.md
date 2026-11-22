# Static riser stack-up weight and tension

`digitalmodel.drilling_riser.stackup_static_weights` implements static riser
stack-up weight and tension arithmetic. The functions are pure and the
input dataclasses are frozen. All dimensional calculation inputs use SI units:
metres, square metres, newtons, newtons per metre, kilograms per cubic metre,
kilograms per metre, pascals and metres per second squared. Field names identify
the units; `_1`, fractions and integer counts are dimensionless.

## Method

- `adjusted_joint_weights`: dry weight = base dry weight × (1 + steel tolerance)
  + buoyancy dry weight. Submerged weight = base submerged weight × (1 + steel
  tolerance) + signed buoyancy submerged weight × buoyancy factor. Uplift is
  negative in this joint-weight interface. For a supplied combined assembly,
  use zero base weights, the combined dry/submerged weights as buoyancy weights,
  and explicit neutral factors.
- `flooded_material_weight_n`: dry weight × (1 − water density / material density).
  This approximation applies to flooded material without trapped volumes.
- `section_weights`: dry and reference flooded wet forces are divided by joint
  length and the respective auxiliary attached-line weights are added. Internal
  water mass per length is (main bore area + auxiliary bore area) × water density.
  Equivalent exterior displaced-water mass per length is (dry − wet force per
  length) / gravity + internal water mass per length. Installed air or submerged
  force is installed length × [dry force per length + gravity × (internal fluid
  mass per length − applicable displaced-water mass per length)]. This includes
  internal fluid once. Auxiliary bore fluids can have different densities.
  Installed length can represent a partial joint or a quantity of whole joints.
- `minimum_tension`: slip-ring demand = flooded steel weight × steel factor
  + unfactored supported weight − buoyancy uplift × buoyancy factor
  + internal fluid weight − displaced internal water weight. Here buoyancy
  uplift is positive. Minimum vertical top tension = slip-ring demand ×
  total tensioner count / remaining tensioner count. Riser overpull is the
  difference between these tensions; lower-package overpull additionally
  subtracts the lower package's submerged weight. Any shortfall against required
  package overpull is added, followed by the supplied recoil overpull. Maximum
  setting is dynamic tension limit × setting fraction.
- `axial_wall_capacity_n`: allowable factor × yield stress × annular material area.
  `governing_tension_n` selects the larger of two supplied tension requirements.

The existing `section.cross_sectional_area`, `stackup.minimum_slip_ring_tension`
and `assembly.minimum_top_tension_16q` functions are reused. All applicable
factors are passed explicitly. The source omits mechanical-efficiency and
fleet-angle reductions. This implementation makes the tensioner efficiency Rf
(in (0, 1]) and the fleet-angle factor (>= 1) required inputs with no default,
because unity is not conservative: at Rf = 0.90 the minimum top tension rises
by 11.1 %. Take both from rig data. The module does not adopt the existing
chain's other default assumptions.

Inputs are SI. Convert customary units at the calling boundary with the shared
registry in `digitalmodel.units` (pint).

## Standard and Input gaps

The source reference is written `API-RP-16Q`; calculation notes also use
`API RP 16Q [1]`. **clause not stated in source**. No edition is established by
the source. The module reproduces source arithmetic and does not establish
compliance with any edition or clause of the standard.

Required inputs remain explicit: joint inventory and lengths; dry and flooded
wet weights and their reference-fluid condition; bore and auxiliary geometry;
fluid/material densities; gravity; steel allowance and buoyancy factors;
tensioner availability; minimum package and recoil overpull; dynamic tension
limit and setting fraction; material yield stress and allowable capacity factor.
No standard-derived numerical factor is supplied as a default. The source's
capacity factor has no stated clause or derivation. Weight allowances already
embedded in supplied component weights must be identified to avoid applying
them again. Source aggregation ranges require engineering confirmation before
use with a different inventory.

## Limits and next revision

These are algebraic static results. Negative net submerged force represents
uplift; a negative calculated tension is not clamped to zero and does not define
an operating setting. No capacity verdict is issued automatically. The material
area screen excludes combined pressure/bending, connectors, fatigue, buckling,
dynamic amplification and time-domain recoil. Counts, geometry and finite
inputs are validated; validation does not qualify the engineering input basis.
Inferred exterior displaced-water mass must be nonnegative; supplied flooded
wet weights that imply negative displacement are rejected. Scalar capacity and
flooded-material weight results must also be finite.

Tabulated density-curve results in the source are not regenerated by this
module; selecting between tabulated values does not validate them.
Remaining work includes density-curve provenance, detailed elevation/stroke and
running/retrieval checks, tensioner geometry and efficiency, conductor/soil
models, solver-deck generation and additional variant input sets. Tests contain
only illustrative example inputs with independently calculated expectations.
