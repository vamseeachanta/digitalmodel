# ABOUTME: Metal-loss Fitness-For-Service (FFS) layer for plates and stiffened
# ABOUTME: panels: thins the section, re-runs the VALIDATED buckling check, reports verdict.
"""
Plate / Stiffened-Panel Metal-Loss Fitness-For-Service
======================================================

A thin Fitness-For-Service (FFS) layer that sits *on top of* the validated
DNV-RP-C201 buckling solvers in :mod:`digitalmodel.structural.structural_analysis`.
It does **not** re-implement any buckling physics. Instead it:

1. reduces the plate thickness to account for measured metal loss plus a
   future-corrosion allowance (FCA), and
2. re-runs the validated buckling check at the reduced thickness, comparing the
   utilisation and critical (buckling) stress against the un-corroded
   ("nominal") section.

This mirrors the structure of an API 579-1 / ASME FFS-1 assessment (the
controlling buckling utilisation replacing a stress-based RSF):

- **Level 1 (general / uniform metal loss)** -- the whole plate field loses
  thickness uniformly; ``assess_plate_uniform_loss`` /
  ``assess_panel_uniform_loss``.
- **Level 2 (local metal loss)** -- a localized thinned patch;
  ``assess_plate_local_loss``. The patch is idealised as an isolated,
  simply-supported plate field of the patch dimensions at the reduced
  thickness (see that function's docstring for the simplification and why it is
  conservative).

Key reported quantities (see :class:`MetalLossFFSResult`):

- ``utilization``            -- buckling utilisation at the reduced thickness.
- ``utilization_original``   -- buckling utilisation of the nominal section.
- ``capacity_retained_frac`` -- fraction of buckling capacity retained
  (see each assessment function for the exact definition; it is in ``(0, 1]``
  for any non-negative loss and decreases monotonically with loss).
- ``max_acceptable_loss_mm`` -- the largest *metal loss* (excluding FCA) for
  which the section still passes (utilisation <= 1.0) at the given stress.

Units are consistent with the rest of ``structural_analysis``: lengths/thickness
in mm, stresses in MPa.
"""

from dataclasses import dataclass, field
from typing import Dict, Optional

from .models import MaterialProperties, PlateGeometry
from .buckling import PlateBucklingAnalyzer
from .panel_buckling import (
    StiffenedPanelBucklingAnalyzer,
    StiffenedPanelGeometry,
    StiffenerGeometry,
)


# This layer only orchestrates the validated buckling solvers; it adds no new
# (unvalidated) closed-form physics of its own.
PRELIMINARY = False


@dataclass
class MetalLossFFSResult:
    """Result of a metal-loss FFS assessment.

    ``mode`` is populated for a plate assessment; ``governing_mode`` is populated
    for a stiffened-panel assessment (the most-utilised of plate-field / column /
    torsional). The other is left ``None``.
    """

    level: int                          # 1 (uniform) or 2 (local)
    remaining_thickness_mm: float       # t_nominal - metal_loss - FCA
    utilization: float                  # at the reduced thickness
    utilization_original: float         # nominal (un-corroded) section
    capacity_retained_frac: float       # in (0, 1] for loss >= 0
    passes: bool
    mode: Optional[str] = None              # plate buckling mode
    governing_mode: Optional[str] = None    # panel governing mode
    max_acceptable_loss_mm: Optional[float] = None
    details: Dict = field(default_factory=dict)


# ---------------------------------------------------------------------------
# Input validation helpers
# ---------------------------------------------------------------------------
def _validate_loss_inputs(t_nominal: float, metal_loss_mm: float, fca_mm: float) -> float:
    """Validate metal-loss inputs and return the remaining thickness (mm).

    Raises ``ValueError`` if any input is negative or if the remaining wall
    thickness (``t_nominal - metal_loss - fca``) is not strictly positive --
    a non-positive remaining wall is a perforation, not a buckling problem.
    """
    if t_nominal <= 0:
        raise ValueError(f"nominal thickness must be > 0, got {t_nominal}")
    if metal_loss_mm < 0:
        raise ValueError(f"metal_loss_mm must be >= 0, got {metal_loss_mm}")
    if fca_mm < 0:
        raise ValueError(f"fca_mm must be >= 0, got {fca_mm}")
    t_rem = t_nominal - metal_loss_mm - fca_mm
    if t_rem <= 0:
        raise ValueError(
            f"remaining thickness <= 0 (t_nominal={t_nominal}, "
            f"metal_loss={metal_loss_mm}, fca={fca_mm}); the wall is fully "
            "consumed -- buckling FFS does not apply."
        )
    return t_rem


# ---------------------------------------------------------------------------
# Plate evaluators (thin wrappers returning (utilisation, critical_stress, mode))
# ---------------------------------------------------------------------------
def _plate_eval(material, geometry, t, sigma_x, sigma_y, tau, gamma_m):
    res = PlateBucklingAnalyzer(material).check_plate_buckling(
        PlateGeometry(geometry.length, geometry.width, t),
        sigma_x, sigma_y=sigma_y, tau=tau, gamma_m=gamma_m,
    )
    return float(res.utilization), float(res.critical_stress), res.mode


def _panel_eval(material, panel, t, sigma_x, sigma_y, tau, gamma_m):
    thinned = StiffenedPanelGeometry(
        plate_length=panel.plate_length,
        plate_thickness=t,
        stiffener=panel.stiffener,
        torsional_restraint_spacing=panel.torsional_restraint_spacing,
    )
    res = StiffenedPanelBucklingAnalyzer(material).check_panel(
        thinned, sigma_x, sigma_y=sigma_y, tau=tau, gamma_m=gamma_m,
    )
    return float(res.utilization), float(res.critical_stress), res.governing_mode


# ---------------------------------------------------------------------------
# Level 1 -- uniform / general metal loss
# ---------------------------------------------------------------------------
def assess_plate_uniform_loss(
    geometry: PlateGeometry,
    material: MaterialProperties,
    metal_loss_mm: float,
    fca_mm: float = 0.0,
    sigma_x: float = 0.0,
    sigma_y: float = 0.0,
    tau: float = 0.0,
    gamma_m: float = 1.15,
) -> MetalLossFFSResult:
    """Level-1 (uniform) metal-loss FFS for an unstiffened plate field.

    ``t_rem = thickness - metal_loss_mm - fca_mm`` (raises if <= 0). The
    validated :class:`PlateBucklingAnalyzer` check is re-run at ``t_rem`` and
    compared with the nominal section.

    ``capacity_retained_frac = critical_stress(t_rem) / critical_stress(t_nom)``
    -- the ratio of the (Johnson-Ostenfeld corrected) buckling stresses. Because
    that critical stress is monotonically increasing in thickness, the retained
    fraction is in ``(0, 1]`` and decreases monotonically with metal loss.
    """
    t_nom = geometry.thickness
    t_rem = _validate_loss_inputs(t_nom, metal_loss_mm, fca_mm)

    util, sigma_cr, mode = _plate_eval(
        material, geometry, t_rem, sigma_x, sigma_y, tau, gamma_m)
    util0, sigma_cr0, _ = _plate_eval(
        material, geometry, t_nom, sigma_x, sigma_y, tau, gamma_m)

    capacity_retained = sigma_cr / sigma_cr0 if sigma_cr0 > 0 else 0.0

    max_loss = max_acceptable_loss(
        geometry, material, sigma_x, sigma_y=sigma_y, tau=tau,
        gamma_m=gamma_m, fca_mm=fca_mm, is_panel=False)

    return MetalLossFFSResult(
        level=1,
        remaining_thickness_mm=t_rem,
        utilization=util,
        utilization_original=util0,
        capacity_retained_frac=float(capacity_retained),
        passes=bool(util <= 1.0),
        mode=mode,
        governing_mode=None,
        max_acceptable_loss_mm=max_loss["metal_loss_mm"],
        details={
            "kind": "plate_uniform",
            "nominal_thickness_mm": t_nom,
            "metal_loss_mm": metal_loss_mm,
            "fca_mm": fca_mm,
            "critical_stress_reduced_MPa": sigma_cr,
            "critical_stress_nominal_MPa": sigma_cr0,
            "max_acceptable_loss_pct": max_loss["metal_loss_pct"],
            "standard": "DNV-RP-C201 (buckling) + ASME FFS-1-style screening",
            "preliminary": PRELIMINARY,
        },
    )


def assess_panel_uniform_loss(
    panel: StiffenedPanelGeometry,
    material: MaterialProperties,
    metal_loss_mm: float,
    fca_mm: float = 0.0,
    sigma_x: float = 0.0,
    sigma_y: float = 0.0,
    tau: float = 0.0,
    gamma_m: float = 1.15,
) -> MetalLossFFSResult:
    """Level-1 (uniform) metal-loss FFS for a stiffened panel.

    Only the *plate field* thickness is reduced (the stiffener cross-section is
    assumed protected / not measured here); the validated
    :class:`StiffenedPanelBucklingAnalyzer` re-screens all three modes
    (plate-field, overall column, stiffener tripping) at the reduced plate
    thickness and reports the governing one.

    For a panel the governing mode can switch as the plate thins (e.g.
    plate-field -> tripping, because a thinner plate gives the stiffener less
    rotational restraint). ``capacity_retained_frac`` is therefore defined as
    the *allowable-load ratio* ``utilization_original / utilization`` (at fixed
    applied stress this equals the governing-capacity ratio). It is robust to
    mode switching and decreases monotonically with loss. ``details`` records
    whether the governing mode changed vs nominal.
    """
    t_nom = panel.plate_thickness
    t_rem = _validate_loss_inputs(t_nom, metal_loss_mm, fca_mm)

    util, crit, gov_mode = _panel_eval(
        material, panel, t_rem, sigma_x, sigma_y, tau, gamma_m)
    util0, crit0, gov_mode0 = _panel_eval(
        material, panel, t_nom, sigma_x, sigma_y, tau, gamma_m)

    # Allowable-load ratio (governing-mode robust). util increases with loss,
    # so this is in (0, 1] and monotonically decreasing.
    capacity_retained = util0 / util if util > 0 else 1.0

    max_loss = max_acceptable_loss(
        panel, material, sigma_x, sigma_y=sigma_y, tau=tau,
        gamma_m=gamma_m, fca_mm=fca_mm, is_panel=True)

    return MetalLossFFSResult(
        level=1,
        remaining_thickness_mm=t_rem,
        utilization=util,
        utilization_original=util0,
        capacity_retained_frac=float(capacity_retained),
        passes=bool(util <= 1.0),
        mode=None,
        governing_mode=gov_mode,
        max_acceptable_loss_mm=max_loss["metal_loss_mm"],
        details={
            "kind": "panel_uniform",
            "nominal_plate_thickness_mm": t_nom,
            "metal_loss_mm": metal_loss_mm,
            "fca_mm": fca_mm,
            "governing_mode_nominal": gov_mode0,
            "governing_mode_changed": bool(gov_mode != gov_mode0),
            "critical_stress_reduced_MPa": crit,
            "critical_stress_nominal_MPa": crit0,
            "max_acceptable_loss_pct": max_loss["metal_loss_pct"],
            "standard": "DNV-RP-C201 (buckling) + ASME FFS-1-style screening",
            "preliminary": PRELIMINARY,
        },
    )


# ---------------------------------------------------------------------------
# Maximum acceptable metal loss (bisection on the loss giving utilisation = 1)
# ---------------------------------------------------------------------------
def max_acceptable_loss(
    geometry,
    material: MaterialProperties,
    sigma_x: float,
    sigma_y: float = 0.0,
    tau: float = 0.0,
    gamma_m: float = 1.15,
    fca_mm: float = 0.0,
    is_panel: bool = False,
    tol_mm: float = 1e-4,
) -> Dict:
    """Largest *metal loss* (mm, excluding FCA) for which utilisation <= 1.0.

    Bisects on metal loss over ``[0, t_nominal - fca)``. Returns a dict with:

    - ``metal_loss_mm`` -- the acceptable metal loss,
    - ``metal_loss_pct`` -- the same as a percentage of nominal thickness,
    - ``remaining_thickness_mm`` -- ``t_nominal - metal_loss - fca``,
    - ``limited_by`` -- ``"buckling"`` (utilisation limit reached) or
      ``"already_failing"`` (the nominal/zero-loss section already fails) or
      ``"perforation"`` (the section passes right down to the wall limit set by
      FCA, so loss is bounded by geometry not buckling).

    Utilisation is monotonically increasing in metal loss (thinner -> weaker),
    so a simple bisection on the utilisation = 1 crossing is exact to ``tol_mm``.
    """
    t_nom = geometry.plate_thickness if is_panel else geometry.thickness
    if t_nom <= 0:
        raise ValueError(f"nominal thickness must be > 0, got {t_nom}")
    if fca_mm < 0:
        raise ValueError(f"fca_mm must be >= 0, got {fca_mm}")
    if fca_mm >= t_nom:
        raise ValueError(
            f"fca_mm ({fca_mm}) consumes the whole wall ({t_nom}); no loss budget.")

    evaluator = _panel_eval if is_panel else _plate_eval

    def util_at_loss(loss):
        t = t_nom - loss - fca_mm
        u, _, _ = evaluator(material, geometry, t, sigma_x, sigma_y, tau, gamma_m)
        return u

    # Loss budget: from 0 up to (but not including) the perforation limit.
    hi_limit = t_nom - fca_mm
    eps = min(tol_mm, hi_limit * 1e-6)

    if util_at_loss(0.0) > 1.0:
        return {
            "metal_loss_mm": 0.0,
            "metal_loss_pct": 0.0,
            "remaining_thickness_mm": t_nom - fca_mm,
            "limited_by": "already_failing",
        }

    # Does it still pass right up against the perforation limit?
    if util_at_loss(hi_limit - eps) <= 1.0:
        loss = hi_limit - eps
        return {
            "metal_loss_mm": float(loss),
            "metal_loss_pct": float(100.0 * loss / t_nom),
            "remaining_thickness_mm": float(t_nom - loss - fca_mm),
            "limited_by": "perforation",
        }

    # Bisect for the utilisation = 1 crossing. lo passes, hi fails.
    lo, hi = 0.0, hi_limit - eps
    while hi - lo > tol_mm:
        mid = 0.5 * (lo + hi)
        if util_at_loss(mid) <= 1.0:
            lo = mid
        else:
            hi = mid

    loss = lo  # largest loss that still passes
    return {
        "metal_loss_mm": float(loss),
        "metal_loss_pct": float(100.0 * loss / t_nom),
        "remaining_thickness_mm": float(t_nom - loss - fca_mm),
        "limited_by": "buckling",
    }


# ---------------------------------------------------------------------------
# Level 2 -- local metal-loss patch
# ---------------------------------------------------------------------------
def assess_plate_local_loss(
    geometry: PlateGeometry,
    material: MaterialProperties,
    metal_loss_mm: float,
    patch_length_mm: float,
    patch_width_mm: float,
    fca_mm: float = 0.0,
    sigma_x: float = 0.0,
    sigma_y: float = 0.0,
    tau: float = 0.0,
    gamma_m: float = 1.15,
) -> MetalLossFFSResult:
    """Level-2 (local) metal-loss FFS for an unstiffened plate field.

    A localized thinned patch of plan dimensions ``patch_length_mm`` x
    ``patch_width_mm`` has lost ``metal_loss_mm`` of wall. The patch is modelled
    as an **isolated, simply-supported plate field of the patch dimensions at
    the reduced thickness** ``t_rem = thickness - metal_loss - fca`` and run
    through the validated :class:`PlateBucklingAnalyzer`.

    Documented simplification / why this is conservative
    ----------------------------------------------------
    Treating the patch as a stand-alone simply-supported plate ignores two
    favourable effects of the surrounding *intact, thicker* plate:
      1. in-plane load redistribution away from the soft (thinned) patch, and
      2. the rotational/edge restraint the thicker surround provides to the
         patch boundary.
    Both raise the true buckling capacity, so the isolated-patch idealisation is
    generally conservative for a patch embedded in a larger plate. It does *not*
    capture the (un-conservative) case of a patch so large it approaches the
    full panel -- for that, use the Level-1 uniform assessment, which this
    function reduces to when the patch equals the full plate.

    Reference frame for the reported numbers
    ----------------------------------------
    All three of ``utilization``, ``utilization_original`` and
    ``capacity_retained_frac`` are for the *patch field* (so they form a
    consistent set): ``utilization_original`` is the patch field at the nominal
    thickness, ``utilization`` is the patch field at ``t_rem``, and
    ``capacity_retained_frac = critical_stress(patch, t_rem) /
    critical_stress(patch, t_nom)`` -- the fraction of the patch's intact local
    buckling capacity it retains (in ``(0, 1]``, decreasing with loss). Note a
    small patch can be *less* utilised than the wider full plate (a narrow field
    buckles at a higher stress); the governing full-plate nominal utilisation is
    recorded in ``details["full_plate_nominal_utilization"]`` for context.
    """
    if patch_length_mm <= 0 or patch_width_mm <= 0:
        raise ValueError("patch dimensions must be > 0")
    if patch_length_mm > geometry.length or patch_width_mm > geometry.width:
        raise ValueError("patch cannot be larger than the plate it sits in")

    t_nom = geometry.thickness
    t_rem = _validate_loss_inputs(t_nom, metal_loss_mm, fca_mm)

    patch = PlateGeometry(patch_length_mm, patch_width_mm, t_rem)
    util, sigma_cr, mode = _plate_eval(
        material, patch, t_rem, sigma_x, sigma_y, tau, gamma_m)
    # Reference: the same patch field, intact (nominal thickness).
    util0, sigma_cr0, _ = _plate_eval(
        material, patch, t_nom, sigma_x, sigma_y, tau, gamma_m)
    # Context only: the governing full-plate nominal utilisation.
    full_util0, _, _ = _plate_eval(
        material, geometry, t_nom, sigma_x, sigma_y, tau, gamma_m)

    capacity_retained = sigma_cr / sigma_cr0 if sigma_cr0 > 0 else 0.0

    # Max acceptable loss *for this patch geometry* (patch held fixed).
    max_loss = max_acceptable_loss(
        patch, material, sigma_x, sigma_y=sigma_y, tau=tau,
        gamma_m=gamma_m, fca_mm=fca_mm, is_panel=False)

    return MetalLossFFSResult(
        level=2,
        remaining_thickness_mm=t_rem,
        utilization=util,
        utilization_original=util0,
        capacity_retained_frac=float(capacity_retained),
        passes=bool(util <= 1.0),
        mode=mode,
        governing_mode=None,
        max_acceptable_loss_mm=max_loss["metal_loss_mm"],
        details={
            "kind": "plate_local_patch",
            "nominal_thickness_mm": t_nom,
            "metal_loss_mm": metal_loss_mm,
            "fca_mm": fca_mm,
            "patch_length_mm": patch_length_mm,
            "patch_width_mm": patch_width_mm,
            "full_plate_nominal_utilization": full_util0,
            "model": "isolated simply-supported plate field of patch size at t_rem",
            "model_note": (
                "Conservative for a patch embedded in thicker intact plate "
                "(ignores load redistribution and edge restraint from the "
                "surround)."
            ),
            "critical_stress_reduced_MPa": sigma_cr,
            "critical_stress_nominal_MPa": sigma_cr0,
            "standard": "DNV-RP-C201 (buckling) + ASME FFS-1-style screening",
            "preliminary": PRELIMINARY,
        },
    )
