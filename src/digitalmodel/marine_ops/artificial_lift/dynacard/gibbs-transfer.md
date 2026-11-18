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
| 3 | 2026-11-12 | Recorded coupled load provenance, removed exact closing samples before FFT, and measured stored-card parity at 15 harmonics. |
| 4 | 2026-11-12 | Added an independent comparison-card derivation, enforced absolute numerical tolerances, shared the density conversion, and clarified the static datum and weight update policy. |

Table 1. Coupled transfer revisions and their purpose.

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
density is derived by the shared public `effective_density_lb_ft3` function used
by both solvers, so supplied
coupling-inclusive weight is retained. When omitted, weight per foot is derived
from the section's material density by the input model at construction only.
Changing diameter or density by assignment or `model_copy(update=...)` retains
the stored dry line weight. To derive bare-body weight again, reconstruct with
`RodSection(**values)` or `RodSection.model_validate(values)` and set
`weight_per_foot=0`; supply an updated measured weight to retain coupling mass.
The shared conversion rejects non-finite or nonpositive weight and diameter
instead of substituting a material-density default. These equilibrium
relations are independent hand derivations, not fitted calibration factors.

The Gibbs transfer approximates displaced fluid volume by nominal rod-body area times
length, although supplied dry weight may include couplings. Coupling displaced
volume is omitted. For omitted volume delta_V in ft³, the neglected buoyancy is
`density_fluid * delta_V` lbf: the subtracted submerged weight is too large and
the mean pump tension is too low by that amount. In Gibbs this changes the static load
datum, not the oscillatory load range. With zero fluid density the error is
zero. A numerical correction is not established without coupling geometry;
agreement between the solvers does not qualify the approximation.
Static displacement is omitted because the returned card uses a zero-minimum
datum; absolute elastic stretch cannot be recovered from this card.

## Inputs and numerical interpretation

- Cards must contain finite, equally spaced time samples over one cycle.
  An exactly repeated closing position/load pair is removed before FFT and
  harmonic-count validation; both coordinates must match. The input remains
  unchanged and the output contains one fewer sample. Near endpoints remain
  distinct. At least three samples must remain. An endpoint quantized to the
  same position/load pair is indistinguishable from an exported closing sample;
  its timing provenance is not established by coordinate equality alone.
  SPM supplies the cycle period.
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
- `downhole_load_basis` records `method="coupled_frequency_domain"`, the summed
  buoyant weight actually subtracted in lbf, the requested `harmonic_count`
  (`None` means all harmonics for the coupled method), and `closing_sample_dropped`.
  `attenuation_factor` is `None` and `validation_status` is `NOT_EVALUATED`.
  Legacy saved `affine_surface_load` records remain readable; absent provenance
  remains unknown. See [the static load basis example](../../../../../docs/domains/artificial_lift/load-basis.html).
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

Across five fixture wells with zero rod damping, 15 harmonics give median
1.3% and maximum 2.0% load nRMSE against the stored reference cards. The old
affine path measured 17.3% median. The per-fixture values are 0.5%, 1.7%,
1.3%, 2.0% and 0.4%; the fifth input and reference each have one repeated
closing sample, which is excluded from the comparison. The measured median
and maximum are below regression bounds of 1.4% and 2.1%, respectively;
these are not engineering acceptance limits. Retaining all harmonics gives
0.7%, 2.2%, 3.4%, 4.1% and 1.8% (median 2.2%, maximum 4.1%). The comparison uses `compare_cards`,
which normalizes by reference load range and aligns traversal and sample origin.

At 15 harmonics, surface/pump load Pearson correlations at the shared sample
indices are 0.992, 0.963, 0.905, 0.945 and 0.965. The requested
<0.99 ceiling is not established for the first fixture. A measured <0.995
regression bound excludes the affine path's correlation of 1.0; it does not
establish field diagnostic accuracy.

No openly readable published worked example with tabulated card values was
found, so validation rests on (a) closed-form cases derived from the cited
equations and (b) the five stored reference cards. A published-example check
is **NOT_EVALUATED**. These fixtures do not establish nonzero damping
calibration or field diagnostic accuracy.

`test_gibbs_transfer.py` writes out independent solutions for the lossless
quarter-wave case, a damped complex propagation constant, a two-section taper,
static buoyancy, and subdivision invariance. It also checks the issue's
correlation ceiling on a synthetic case and rejects invalid boundary/material inputs. Existing
fillage tests verify dependence on the solved card; the comparison gate still
raises on disagreement unless an explicit divergence opt-in is provided.

`test_dynacard.py` derives the uniform lossless pump card directly from the
surface sine/cosine inputs and the thin-rod equations cited above. Both axes
are checked before the expected nRMSE is computed against the default solver's
card. Absolute budgets are 1e-8 lbf and 1e-10 in for the card, and 1e-9
percentage point for the secondary metric check. These qualify numerical
arithmetic; they do not establish field accuracy or damping calibration.

The original field-fillage ratio in #1857 is **NOT_EVALUATED**. The revision in
#1855 withdraws its displacement basis and identifies unresolved timing and load
calibration. A reliable synchronized surface/pump reference, damping calibration,
and an independently supported diagnostic label are needed before asserting
field fillage or excluding a particular failure mode. The legacy finite-difference
solver is unchanged and remains unqualified. The default workflow solver is
unchanged. Analytical correctness does not validate a classifier trained on
synthetic cards.
