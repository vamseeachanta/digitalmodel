# Coupled rod wave solution

The Gibbs comparison path now propagates both displacement and tensile force.
It supports ordered rod sections and their prescribed viscous damping rates.
Analytical checks establish the implemented transfer equations, while field
calibration and diagnostic accuracy remain **NOT_EVALUATED**.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-10-19 | Replaced affine load scaling with coupled section transfers and independent analytical cases. |
| 2 | 2026-10-19 | Used supplied rod weight for static tension, added harmonic selection and clarified the damping basis and relative displacement datum. |

## Equations and source

The public textbook [Applied Mechanics of Solids, section 4.3.3,
1-D Bar subjected to end loading](https://solidmechanics.org/Text/Chapter4_3/Chapter4_3.php)
provides the thin-rod wave equation and axial stress/displacement relation.
This is the undated online edition, read on 2026-10-19; a publication year is
not established by that page. The following transfer equations are derived
here from that rod model. The damped rod model is cited separately below. They also
implement the companion force equation specified in issue #1955.

[SPE-588-PA, Predicting the Behavior of Sucker-Rod Pumping Systems
(1963), The Mathematical Model](https://doi.org/10.2118/588-PA) supplies the
viscously damped rod model and its boundary conditions. Its forward formulation
specifies initial stress and velocity, polished-rod motion and pump operation.
This inverse comparison path instead takes both measured surface displacement
and surface tension as boundary inputs and reconstructs the pump card; it does
not implement a forward pump-valve boundary model. The dimensionless damping
coefficient conversion to the rate c is **NOT_EVALUATED**. Callers shall supply
an independently established rate in inverse seconds; a dimensionless damping
factor cannot be passed unchanged.

Let x increase from surface to pump, u be upward displacement in inches,
F be tensile force in pounds, E be the section modulus in psi, A its area
in square inches, a its wave speed in inches/second, and c a prescribed decay
rate in inverse seconds. With this coordinate convention:

```text
F = -EA du/dx
u_tt + c u_t = a² u_xx
u(x,t) = Re[U(x) exp(i omega t)]
gamma² = (-omega² + i omega c)/a²

U_out = cosh(gamma L) U_in - sinh(gamma L)/(EA gamma) F_in
F_out = cosh(gamma L) F_in - EA gamma sinh(gamma L) U_in
```

The force equation follows by differentiating U_out and substituting
F = -EA U_x. Its sign is fixed by the stated coordinates. The section
matrix has determinant one because cosh²(z) - sinh²(z) = 1. Displacement and
force are continuous at taper interfaces; apply matrices in surface-to-pump
order rather than averaging diameters. Section count is descriptive; each
section's length is its total physical length, consistent with rod_length.

The zero-frequency limit is evaluated separately. Static equilibrium and
Archimedes' displaced-fluid weight give, for weight/volume densities in lb/ft³:

```text
A = pi d²/4
rho_effective = weight_per_foot * 144 / A
W = (rho_effective - density_fluid) A L_ft / 144
F_out = F_in - W
```

The divisor is the exact square-inch to square-foot conversion. Effective
density is derived by the same adapter helper as the default solver, so supplied
coupling-inclusive weight is retained. When omitted, weight per foot is derived
from the section's material density by the input model. These equilibrium
relations are independent hand derivations, not fitted calibration factors.
Static displacement is omitted because the returned card uses a zero-minimum
datum; absolute elastic stretch cannot be recovered from this card.

## Inputs and numerical interpretation

- Cards must contain finite, equally spaced time samples over one cycle,
  excluding a repeated closing endpoint. SPM supplies the cycle period.
  A position-digitized card without reliable timing does not establish this input.
- Each section supplies length, diameter, modulus, wave speed, density, and
  `damping_factor`. In this solver `damping_factor` means **c in inverse seconds**,
  not a dimensionless modal damping ratio. No conversion from a modal ratio is
  inferred. The pre-existing zero default selects the ideal lossless model;
  a calibrated nonzero rate must be supplied for dissipative field use.
- Existing default material and fluid data are **assumed, based on previous
  experience** until independently supplied. The synthetic example prescribes
  every property used by the transfer; its numbers are mathematical inputs,
  not recommended rod properties.
- Material density, modulus and wave speed are independent inputs; their mass
  consistency is not checked. The defaults do not imply a single consistent
  dynamic mass. Static tension uses the supplied weight per foot, while dynamic
  response continues to use the prescribed wave speed.
- `solve_wave_equation(harmonic_count=None)` retains all resolved harmonics.
  An integer from zero through half the sample count (rounded down) retains DC
  and that many positive-frequency harmonics. Zero returns static load and zero
  relative motion. Discarded coefficients are removed before propagation.
  No automatic or field-calibrated bandwidth is selected.
  At even sample counts the unpaired
  Nyquist coefficient is reconstructed by its real projection. Noise and high
  damping can make inverse propagation ill-conditioned; non-finite solutions
  raise PhysicsError. No denoising bandwidth or field accuracy is inferred.
- `LOAD_ATTENUATION_FACTOR`, `STROKE_SCALING_FACTOR`, the fixed damping constant,
  and the empirical buoyancy factor are no longer used by this solver. Existing
  constants remain available for import compatibility.
- Elastic overtravel is allowed. A universal pump-stroke ceiling at the surface
  stroke is incompatible with the wave solution. Card finiteness checks remain.
  The revision in linked issue #1855 also withdraws the original universal
  shorter-stroke assumption.

## Worked example

Run `PYTHONPATH=src python examples/domains/artificial_lift/gibbs_transfer_demo.py`.
The example prescribes a uniform ideal rod with A = 1 in², E = 1440 psi,
a = 60 ft/s, L = 15 ft, and c = 0. It uses a 1 Hz cycle (60 SPM),
rod density 144 lb/ft³ and zero fluid density. These deliberately synthetic
values define a quarter-wave case; no empirical property is inferred.

For theta = 2 pi t, set surface u = 2 cos(theta) inches and
F = 20 + 12 sin(theta) pounds. Then omega L/a = pi/2, cos(pi/2) = 0,
and sin(pi/2) = 1. From the equations above, EA omega/a = 4 pi lb/in:

```text
pump u before datum shift = -(3/pi) sin(theta) inches
pump F = 5 + 8 pi cos(theta) pounds
pump stroke = 6/pi inches
surface stroke = 4 inches
rod weight = 15 pounds
```

The printed pump stroke is 1.9 inches and the mean pump tension is 5.0 pounds.
These rounded display values come from the exact expressions, which are used
for the example's assertions. Adding a surface-load ripple proportional to
sin(3 theta) leaves the pump force unchanged: its cosine transfer factor is
cos(3 pi/2) = 0 and surface displacement has no third harmonic. This directly
checks that load morphology changes rather than undergoing affine scaling.

## Checks and limits

`test_gibbs_transfer.py` writes out independent solutions for the lossless
quarter-wave case, a damped complex propagation constant, a two-section taper,
static buoyancy, and subdivision invariance. It also checks the issue's
correlation requirement and rejects invalid boundary/material inputs. Existing
fillage tests verify dependence on the solved card; the comparison gate still
raises on disagreement unless an explicit divergence opt-in is provided.

The original field-fillage ratio in #1857 is **NOT_EVALUATED**. The revision in
#1855 withdraws its displacement basis and identifies unresolved timing and load
calibration. A reliable synchronized surface/pump reference, damping calibration,
and an independently supported diagnostic label are needed before asserting
field fillage or excluding a particular failure mode. The legacy finite-difference
solver is unchanged and remains unqualified. The default workflow solver is
unchanged. Analytical correctness does not validate a classifier trained on
synthetic cards.
