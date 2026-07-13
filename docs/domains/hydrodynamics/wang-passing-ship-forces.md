# Wang passing-ship forces — legacy calculator port (validation lineage)

## What this is

`digitalmodel.hydrodynamics.wang_passing_ship` and the routed workflow
basename `passing_ship_forces` are a port of a legacy in-house
passing-ship force calculator (Excel/VBA `modPassingShip` +
`modQuadrature`, 2014–2016 era, with a companion MathCAD cross-check
worksheet). The method is S. Wang's slender-body closed form:

> S. Wang, "Dynamic Effects of Ship Passage on Moored Vessels", *Journal
> of the Waterways, Harbors and Coastal Engineering Division*, ASCE,
> Vol. 101, WW3, p. 247, 1975.

The identical formulation — including the finite-depth method-of-images
extension — is published in Varyani, Krishnankutty & Vantorre,
"Prediction of Load on Mooring Ropes of a Container Ship due to the
Forces Induced by a Passing Bulk Carrier", MARSIM'03, Eqs. (2.4)–(2.9).
It computes the surge force, sway force and yaw moment on a moored ship
as a function of the passing ship's stagger (midship-to-midship
longitudinal offset), lateral separation, speed and water depth. The
force/moment histories feed moored-vessel response analyses (mooring
codes, time-domain simulators).

## Method, as the legacy code actually computes it

1. **Parabolic sectional-area hulls.** Both hulls are idealised as
   `S_i(x) = A_i (1 − 4x²/L_i²)` with length `L_i` and midship area
   `A_i` (typically `beam × draft × midship coefficient`).
2. **Closed-form inner integrals.** Wang's kernels over the passing-ship
   length are integrated analytically (the legacy `funcF`/`funcG`
   antiderivatives), leaving a single smooth outer integral over the
   moored-ship length:
   - surge `X = ρU²/(2π) ∫ S₁'(x₁) F(x₁) dx₁`
   - sway  `Y = ρU²η/π ∫ S₁'(x₁) G(x₁) dx₁`
   - yaw   `N = ρU²η/π ∫ (S₁'(x₁)x₁ + S₁(x₁)) G(x₁) dx₁`
3. **Quadrature.** The legacy tool used 5-point Gauss–Lobatto panels with
   a 6-point error estimate and adaptive bisection to a relative
   tolerance of 1e-6. That scheme is retained verbatim as the pure-Python
   reference path (`wang_forces_scalar`); the production path evaluates
   the same integrand with fixed-order Gauss–Legendre quadrature (default
   96 nodes), fully vectorised over the stagger sweep with NumPy.
   (Transcription note: the loose legacy VBA source skips the first of
   its five initial panels — an off-by-one; the validated tool outputs
   correspond to the full-domain integral, which both paths compute.)
4. **Finite depth by images.** For depth `h` (needed when
   `h < ~2 × draft`) the legacy 21-term image sum is reproduced: with
   `η_n = sqrt(η² + 4n²h²)`, `n = −10..10`,
   `X_h = Σ X(ξ, η_n)`, `Y_h = η Σ Y(ξ, η_n)/η_n`,
   `N_h = η Σ N(ξ, η_n)/η_n`.
5. **Conventions and units.** Sway > 0 = attraction toward the passing
   ship (peak near zero stagger); surge and yaw are odd in stagger for
   symmetric hulls. Any consistent unit set (legacy: ft, ft², slug/ft³,
   ft/s → lbf, ft·lbf; SI in → N, N·m out).

## Validation lineage

`tests/hydrodynamics/test_wang_passing_ship.py` pins the port to two
legacy oracles (36 tests across core + workflow):

- **MathCAD worksheet** (Wang formulation, finite depth): abeam sway
  force reproduced to 1e-8 relative; 21-point subsamples of the saved
  201-point finite-depth stagger sweeps for surge/sway/yaw reproduced to
  1e-6 relative; the worksheet's non-dimensional deep-water sway spot
  value to 1e-6.
- **Legacy VBA workbook output table** (equal vessels, infinite and
  finite depth): 21-row × 6-column subsample reproduced within 1%
  (the stored table carries only ~4 significant figures; agreement of
  the underlying values is ~1e-4 relative or better).
- Consistency: vectorised path vs legacy-quadrature scalar path to
  1e-6; deep-water limit of the image sum; U² scaling; odd/even stagger
  symmetries.

## Relationship to the `passing_ship` package

`digitalmodel.hydrodynamics.passing_ship` (basename `passing_ship`) is a
spec-era implementation with its own configuration/CLI/visualization
stack; its depth treatment is a heuristic correction factor rather than
the legacy image sum. `passing_ship_forces` exists to give results
traceable to the validated legacy VBA/MathCAD calculator — same
closed-form inner integrals, same image-method depth correction, same
constants. Prefer `passing_ship_forces` when reproducing or extending
legacy passing-ship studies; consolidation of the two stacks is a
follow-up decision.

## Workflow usage

```yaml
basename: passing_ship_forces
passing_ship_forces:
  moored_vessel:  {length: 941.109, midship_area: 8103.879}
  passing_vessel: {length: 941.109, midship_area: 8103.879}
  water_density: 1.9905          # slug/ft^3 (or kg/m^3)
  passing_velocity: 6.0          # ft/s (or m/s)
  separation_distance: 364.042   # centreline to centreline
  water_depth: 55.0              # omit for deep water
  stagger:
    range: {start: -2.0, stop: 2.0, num: 201, normalized: true}
  output_dir: results
```

Outputs: a per-stagger CSV (`stagger, surge_force, sway_force,
yaw_moment`) and a summary CSV (peak values and their stagger
locations), plus the same content in the returned config under
`passing_ship_forces`.
