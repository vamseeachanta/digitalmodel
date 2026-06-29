# Hull-Girder Ultimate Strength — Smith Incremental-Iterative Method — Validation Record (2026-06-28)

Backing for `src/digitalmodel/naval_architecture/hull_girder_smith.py`. This is
the **rigorous successor** to the *simplified* single-step ultimate
`M_U = σ_U · Z` in `hull_girder_strength.py` (issue #1082), which assumes the
compression flange reaches its buckling-reduced ultimate while the tension flange
yields and ignores neutral-axis migration and post-buckling redistribution.

## What it computes

| Object | Role |
|---|---|
| `HullGirderElement` | one structural element (stiffener+plate, or hard-corner plate): area, height `z`, `fy`, compressive ultimate `σ_u` (buckling-reduced), `E`, post-buckling residual |
| `HullGirderElement.stress(strain)` | the **load-shortening curve** σ(ε) |
| `smith_ultimate(elements, …)` | the **Smith march**: returns hogging + sagging `M_U`, moment-curvature data, neutral-axis migration |
| `stiffened_plate_element` / `plate_element` / `hard_corner_element` | factories that take `σ_u` from the validated DNV-RP-C201 solvers (no buckling re-implemented) |

`code_reference = "IACS UR S11A / Smith method"`. Units: areas m², `z` m,
stresses MPa, curvatures 1/m, moments **kN·m** (matching `loading_computer` /
`hull_girder_strength`).

## Method

**Load-shortening idealisation** (`HullGirderElement.stress`):
- Tension (ε ≥ 0): elastic `E·ε`, capped at `+fy` (perfectly-plastic plateau).
- Compression (ε < 0): elastic `−E·|ε|` until the capacity `min(σ_u, fy)`, then
  - **non-buckling** element (`σ_u ≥ fy`): a `−capacity` plateau (elastic-perfectly-plastic);
  - **buckling** element (`σ_u < fy`): a post-buckling decay
    `−capacity·(r + (1−r)·ε_u/|ε|)` toward a residual fraction `r` of the
    capacity (`ε_u = capacity/E`). `r = 1` recovers the pure buckling-cap (no
    softening); the default `r = 0.5` is a Smith-style softening.

**Smith march** (`smith_ultimate`): impose increasing curvatures κ (positive =
hogging = deck in tension; negative = sagging = deck in compression). At each κ,
solve the neutral-axis height `z_NA` so Σ(σ_i·A_i) = 0 (sign-change scan +
bisection, robust to the post-buckling branch's mild non-monotonicity); strain_i
= κ·(z_i − z_NA); M = Σ σ_i·A_i·(z_i − z_NA). The **peak** of M(κ) = `M_U`,
reported separately for hogging and sagging, with the curvature at the peak and
the full moment-curvature + NA-migration trace.

## Validation — golden tests

Test section: a **doubly-symmetric box girder**, depth D = 12 m, AH36
(fy = 355 MPa, E = 206 GPa): deck + bottom flanges 0.30 m² each, two side walls
0.20 m² total over symmetric heights. Centroid = plastic NA = D/2 = 6 m. Lumped
Z = 3.989 m³; first-yield `M_y = fy·Z = 1.4161×10⁶ kN·m`; fully-plastic
`M_p = fy·ΣA|z−z_c| = 1.4910×10⁶ kN·m` (shape factor 1.053).

1. **Elastic march** — at κ = 0.1·κ_y the march reproduces beam theory
   `M = E·I·κ` and `z_NA = 6 m` (rel_tol 1e-6). Proves the equilibrium loop in
   the elastic regime.
2. **First yield** — at κ_y the recorded moment equals `fy·Z` exactly (rel_tol 1e-6).
3. **Stocky → fully-plastic (MAKE-OR-BREAK)** — with `σ_u = fy` everywhere
   (no element buckles), the Smith peak at high curvature equals the analytic
   plastic moment `M_p` in **both** hogging and sagging (rel_tol 1e-4), and
   hog = sag (symmetry). `M_p > M_y` confirms the march captured plastification +
   NA-driven redistribution, not just the elastic response. This is the proof
   that the march + equilibrium loop is correct: with all elements saturating at
   ±fy the equilibrium NA migrates to the plastic NA and M → M_p analytically.
4. **Buckling reduction** — deck `σ_u = 0.6·fy`, `r = 0.6`. Sagging (deck in
   compression) drops to `8.984×10⁵ kN·m = 0.60·M_p`; hogging (deck in tension,
   keel compression unbuckled) stays at `M_p` (rel_tol 2e-3). Reduction is in the
   right direction and magnitude-sane; the NA migrates off the centroid
   (6 → 5.985 m, toward the still-effective keel) and the sagging peak occurs at
   an interior curvature (κ ≈ −1.10×10⁻³, below κ_max) — a genuine post-buckling
   peak, not a clipped endpoint.
5. **Convergence** — refining the curvature step (50 → 200 → 800 steps)
   converges `M_U` monotonically; the 200-step grid is within 0.2 % of the
   800-step result.
6. **Reuse** — `stiffened_plate_element` returns `σ_u` identical to
   `StiffenedPanelBucklingAnalyzer.check_panel(...).critical_stress`; the
   buckling physics is delegated, not duplicated.

`tests/naval_architecture/test_hull_girder_smith.py` — **8 tests**, black +
flake8 (E9,F63,F7,F82,F401,F811,F841) clean, runs under the `naval-architecture`
CI domain.

## Simplification level vs a classified CSR Smith implementation (required disclosure)

This module is a **clean, validated Smith engine**, but deliberately simpler than
a full IACS CSR (Common Structural Rules) hull-girder ultimate calculation:

- **Load-shortening curves** — a single idealised curve (elastic →
  capacity-cap → smooth post-buckling decay to a residual fraction). CSR uses a
  *family* of mode-specific curves (CSR Pt 1 Ch 5 Sec 4 / IACS UR S11A App.1):
  beam-column buckling, torsional/tripping, web local buckling, plate buckling,
  and the elasto-plastic curve, each with its own closed form. Here the **peak**
  σ_u is sourced from the validated DNV-RP-C201 panel solver, but the *shape* of
  the post-buckling branch is a single parametric decay, not the per-mode CSR
  shapes.
- **No local-pressure / lateral-load interaction** — pure in-plane axial
  response; lateral pressure on the plating (which lowers stiffener capacity in
  CSR) is not coupled in.
- **Lumped point-area elements** — each element's own bending inertia is
  neglected (consistent point-area idealisation); a CSR model integrates the
  element stress over its area.
- **Effective breadth** — taken from the panel solver (full stiffener spacing);
  no progressive effective-width reduction of very slender plate fields as
  curvature grows.
- **Single hull material per element, no residual stresses / initial
  imperfections** beyond what the panel solver's σ_u already embeds; CSR applies
  explicit imperfection and weld-residual knock-downs.
- **Symmetric-bending only** — vertical hull-girder bending; no biaxial /
  horizontal-moment interaction, no shear-lag.

Consequently this `M_U` is a **rigorous preliminary-to-intermediate** figure: it
correctly captures progressive collapse, NA migration and post-buckling
redistribution (the three things the simplified `σ_U·Z` omits) and reduces
*exactly* to the analytic plastic / first-yield limit for a non-buckling section
— but it is not a drop-in CSR-compliant capacity for a classed scantling
submission.

## Builds on (no physics reimplemented)

- `structural/structural_analysis/panel_buckling.py` — `σ_u` from
  `StiffenedPanelBucklingAnalyzer.check_panel(...).critical_stress` (DNV-RP-C201).
- `structural/structural_analysis/buckling.py` —
  `PlateBucklingAnalyzer.johnson_ostenfeld` for unstiffened-plate `σ_u`.
- `structural/structural_analysis/models.py` — `MaterialProperties` /
  `MARINE_GRADES` (Grade A / AH36 / EH40).
- `naval_architecture/hull_girder_strength.py` — the simplified `σ_U·Z` ultimate
  this method supersedes (kept for preliminary use).
