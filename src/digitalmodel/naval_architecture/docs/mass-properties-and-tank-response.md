# Mass properties and rectangular-tank response

These helpers generalize methods from a legacy engineering workbook. They
operate in SI and do not supply project data, engineering acceptance limits or
empirical defaults. All test inputs are **illustrative example** inputs; expected
results follow independent balances, integrals and closed-form limits.

Import the submodules `mass_properties` and `tank_sloshing` from
`digitalmodel.naval_architecture`.

| Function | Calculation and units |
| --- | --- |
| `combined_centre_of_gravity` | Sum of mass times coordinate divided by total mass; kg and m. Reuses the existing point-mass balance. |
| `coordinate_offset` | Point minus reference, m, common Cartesian datum. |
| `cuboid_centroidal_inertia` | Uniform cuboid: Ixx = mass times (breadth squared + height squared) / 12; cyclic axes, kg m². |
| `parallel_axis_inertia` | Transfer contribution: mass times sum of perpendicular offsets squared, kg m². Add to centroidal inertia. |
| `radius_of_gyration` | Square root of inertia divided by mass, m; reuses the existing rotational gyradius helper. |
| `estimate_gyradii` | Caller-supplied roll ratio times beam; pitch and yaw ratios times length, m. |
| `calculate_mass_properties` | Total kg, combined centre m, offsets m, per-item and total kg m², and gyradii m about the explicit reference. |
| `available_depth_m` | Overall height minus nonnegative bottom height, m; requires a positive result. |
| `liquid_depth_m` | Fill fraction times available depth, m, for constant horizontal area. |
| `tank_natural_frequency_hz` | Reciprocal of the first period from `hydrodynamics.sloshing.rectangular_tank_periods`, with `n_modes=1` and caller-supplied gravity; Hz. |
| `natural_period_s` | Reciprocal of positive frequency, s. |
| `period_ratio` | Ship period divided by tank period; dimensionless. |
| `calculate_tank_response` | Independent breadth/fill cases using the preceding relations. |

Table 1. Methods and returned units. Each public calculation takes a typed frozen
input dataclass; dimensional fields carry unit suffixes.

## Limits

The rigid bodies are uniform cuboids aligned with a common Cartesian frame:
x longitudinal, y transverse, z vertical. Zero mass and zero dimensions are
allowed as limits; an empty or zero-total-mass group is rejected. The inertia
reference is required and can differ from the combined centre. Radii then
refer to that reference, not automatically to centroidal or principal axes.
Products of inertia, rotated bodies, fluid motion and added mass are excluded.
A caller evaluating ingress cases supplies each complete body group and the
selected reference; scenario inheritance and physical ingress are not inferred.

The tank equation assumes linear, inviscid, small-amplitude gravity waves in a
rigid rectangular tank, with uniform depth and an exposed horizontal free
surface. It predicts a natural frequency, not wave amplitude, damping, loads
or a coupled vessel response. Fill must lie in (0, 1]. A fill of one represents
a geometric depth limit with an exposed surface, not a pressed-full tank.
An empty tank is rejected because it has no finite period under this relation.
A period ratio alone gives no engineering acceptance decision.

Inputs and outputs must be finite and representable with floating-point
arithmetic. Invalid or nonfinite quantities are rejected. The helpers do not
provide arbitrary-precision behavior for extreme magnitudes. Existing fleet
estimates and their empirical defaults are not used.

## Input gaps

The following nine categories distinguish identities from caller decisions.

1. **Conflicting distance labels:** coordinate subtraction is a geometric
   identity. Supply all positions, dimensions and references explicitly in m
   after boundary conversion; no legacy heading is used to infer units.
2. **Reference selection across cases:** this is a required caller decision.
   `MassCaseInput.reference_m` has no default. Choosing an earlier case's centre
   requires an external engineering basis.
3. **Cuboid divisor:** integrating squared distance over a centred uniform
   interval gives dimension squared / 12. This is a textbook identity, fixed in
   the formula, rather than an estimated coefficient.
4. **Gyradius estimate ratios:** all three dimensionless ratios are required
   arguments without defaults. Their engineering basis is outside these helpers.
5. **Unverified supporting mass/geometry data:** masses, coordinates and body
   dimensions are required without defaults. Their provenance is not established
   by this method or its illustrative tests.
6. **Boundary conversion:** inputs require SI units. The existing
   `hydrostatics.fluid_column.ft_to_m` helper supplies the exact foot conversion;
   these helpers do not expose another unit-conversion API.
7. **Gravity:** `gravity_m_s2` is required without a default. Supply the value
   justified for the intended conditions; no conventional value is implied.
8. **Height estimates and conflicting annotations:** overall and bottom heights
   are required, in m, without defaults. Available and liquid depths follow the
   subtraction and constant-area fill identities; annotations supply no values.
9. **Period/frequency label conflict:** `ship_period_s` is a required period in s,
   without a default. Its source and applicability require caller qualification.
   Frequency conversion is the textbook reciprocal identity.

No standard, numbered clause or source-specific acceptance criterion is asserted.
