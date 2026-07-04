# ABOUTME: Cross-stiffened (orthogonally stiffened) grillage buckling solver per
# ABOUTME: DNV-RP-C201 Sec. 7.3-7.4: overall orthotropic mode + governing-mode pick.
"""
Grillage / Cross-Stiffened Panel Buckling
=========================================

A closed-form buckling check for a **grillage**: a plate field stiffened by
*both* longitudinal stiffeners and transverse frames (the real hull-plating
layout), following DNV-RP-C201 ("Buckling Strength of Plated Structures"),
Sec. 7.3-7.4.

Four failure modes are screened and the governing (most-utilised) one is
reported:

1. ``plate_field`` -- buckling of the unstiffened plate panel bounded by two
   longitudinal stiffeners (width = longitudinal spacing ``s_L``) and two
   transverse frames (length = transverse spacing ``s_T``). Delegated to the
   validated :class:`PlateBucklingAnalyzer`.
2. ``longitudinal_stiffener`` -- column + lateral-torsional (tripping) buckling
   of a longitudinal stiffener spanning between transverse frames
   (``L_eff = s_T``). Delegated *unchanged* to the validated
   :class:`StiffenedPanelBucklingAnalyzer`.
3. ``transverse_frame`` -- column + tripping buckling of a transverse frame
   spanning the panel width (``L_eff = width_y``), carrying the transverse
   stress ``sigma_y``. Also delegated to :class:`StiffenedPanelBucklingAnalyzer`.
4. ``overall_grillage`` -- **the new physics**: buckling of the whole stiffened
   panel as an equivalent orthotropic plate (smeared stiffener rigidities),
   minimised over the integer buckle-mode numbers ``m`` (along ``x``) and ``n``
   (along ``y``). This is the cross-stiffener interaction that the
   single-stiffener panel solver cannot capture.

Orthotropic overall mode (DNV-RP-C201 Sec. 7.3-7.4)
---------------------------------------------------
For a simply supported orthotropic plate ``a x b`` (``a`` = gross length in the
compression direction ``x``, ``b`` = gross width) the critical axial force per
unit width under uniaxial compression ``N_x`` solves::

    N_x(m, n) = pi^2 [ Dx (m/a)^2
                       + 2 H (n/b)^2
                       + Dy (n/b)^4 (a/m)^2 ]

minimised over integer half-wave counts ``m >= 1`` (and ``n >= 1``; ``n = 1``
governs for pure ``sigma_x``). The smeared flexural rigidities are::

    Dx = E I_L / s_L      (longitudinal stiffener + effective plating)
    Dy = E I_T / s_T      (transverse frame + effective plating)
    H  = sqrt(Dx Dy)      (equivalent-isotropic torsional rigidity -- the
                           standard simplification; it makes the panel collapse
                           to the isotropic DNV k = 4 plate result and vanishes
                           correctly as a stiffener direction softens)

The critical *stress* is ``sigma_e = N_x,cr / t_x`` with the smeared axial
thickness ``t_x = t_p + A_L / s_L``; the inelastic Johnson-Ostenfeld correction
(reused from :class:`PlateBucklingAnalyzer`) gives the characteristic capacity.

Limit behaviour (validated)
---------------------------
- **Isotropic collapse**: with ``Dx = Dy = H = E t^3 / (12 (1 - nu^2))`` the
  long-panel discrete minimum is ``4 pi^2 sqrt(Dx Dy) / b^2``, i.e. exactly the
  DNV plate-buckling stress with ``k = 4`` (golden: 223.11 MPa for
  ``t = 12, b = 700, E = 210000, nu = 0.3``).
- **Reduces to the single stiffener**: for a one-bay grillage
  (``length_x ~ s_T``) the overall orthotropic mode is *suppressed* (it is only
  meaningful for a multi-bay field; ``Dy -> 0`` as ``s_T -> inf`` would
  otherwise spuriously dominate) and the governing utilisation equals the
  standalone :class:`StiffenedPanelBucklingAnalyzer` result for the longitudinal
  stiffener.
- **Closer transverse frames raise capacity**: shrinking ``s_T`` raises ``Dy``
  and the overall elastic buckling stress (monotone).

Simplification level vs a full FE grillage analysis
---------------------------------------------------
This is a *screening* model, not a finite-element grillage solution. Compared
to an FE plate/beam grillage:

- Smeared-orthotropic-plate idealisation: discrete stiffener positions, local
  stiffener flexibility, and the true coupled mode shapes are homogenised into
  equivalent rigidities; ``H = sqrt(Dx Dy)`` neglects the stiffeners' own
  (usually small, open-section) St-Venant torsional contribution.
- Simply supported boundary on all four edges; no rotational edge restraint,
  no lateral pressure / continuous-beam bending, no residual stresses, and only
  the leading buckle mode is taken (integer ``m, n`` sweep, not a full
  eigenvalue spectrum).
- Effective plate width for the (wide) transverse-frame flange uses the
  DNV-RP-C201 ``C_xs`` reduction; the longitudinal mode keeps the validated
  panel solver's full-spacing assumption.

Use it to rank modes and flag a grillage for detailed FE, not as a final
capacity statement for a critical structure.

Units: stresses in MPa, lengths in mm, ``E`` in MPa, areas in mm^2, second
moments in mm^4 (consistent with the rest of ``structural_analysis``).
"""

import math
from dataclasses import dataclass, field
from typing import Dict

import numpy as np

from .models import MaterialProperties, PlateGeometry
from .buckling import PlateBucklingAnalyzer
from .panel_buckling import (
    StiffenerGeometry,
    StiffenedPanelGeometry,
    StiffenedPanelBucklingAnalyzer,
)

# Self-contained code reference (do not import from digitalmodel.codes).
GRILLAGE_CODE_REFERENCE = "DNV-RP-C201"

# This solver is a validated screening model (see module docstring). The overall
# orthotropic mode is new; modes 1-3 reuse already-validated solvers.
PRELIMINARY = False


@dataclass
class GrillageGeometry:
    """A cross-stiffened (orthogonally stiffened) grillage.

    All dimensions in mm. The compression of interest acts along ``x`` (the
    longitudinal-stiffener direction).

    ``longitudinal.spacing`` is ``s_L`` (transverse pitch of longitudinal
    stiffeners); ``transverse.spacing`` is ``s_T`` (longitudinal pitch of the
    transverse frames). ``length_x`` is the gross grillage length ``L_g`` (the
    overall span used for the overall-grillage mode); the longitudinal-stiffener
    column span is ``s_T`` (between transverse frames), so the two length scales
    are kept distinct.
    """

    length_x: float  # L_g, gross length in compression direction (mm)
    width_y: float  # B, gross width (mm)
    plate_thickness: float  # t_p (mm)
    longitudinal: StiffenerGeometry  # spacing = s_L
    transverse: StiffenerGeometry  # spacing = s_T

    def num_frame_bays(self) -> int:
        """Number of plate bays along ``x`` (= number of transverse-frame
        spaces in the gross length). 1 means a single bay (frames only at the
        two ends)."""
        return max(1, int(round(self.length_x / self.transverse.spacing)))


@dataclass
class GrillageBucklingResult:
    """Result of a grillage buckling check (governing among four modes)."""

    governing_mode: str
    utilization: float
    plate_utilization: float
    longitudinal_utilization: float
    transverse_utilization: float
    overall_utilization: float
    critical_stress: float  # MPa, governing mode
    overall_elastic_stress: float  # MPa, orthotropic sigma_e (pre Johnson-Ostenfeld)
    overall_critical_stress: float  # MPa, after Johnson-Ostenfeld
    overall_half_waves: int  # m (x-direction) at the overall minimum
    overall_mode_applicable: bool  # False for a single-bay grillage
    passes: bool
    details: Dict = field(default_factory=dict)
    code_reference: str = GRILLAGE_CODE_REFERENCE


class GrillageBucklingAnalyzer:
    """Cross-stiffened grillage buckling analyzer per DNV-RP-C201 Sec. 7.3-7.4.

    Reuses the validated single-stiffener panel solver for the plate-field,
    longitudinal-stiffener and transverse-frame modes; adds the overall
    orthotropic-plate (grillage) mode and the governing-mode aggregation.
    """

    def __init__(self, material: MaterialProperties):
        self.material = material
        self.panel_analyzer = StiffenedPanelBucklingAnalyzer(material)
        self.plate_analyzer = PlateBucklingAnalyzer(material)

    # ------------------------------------------------------------------
    # Sub-panel geometries delegated to the validated solver
    # ------------------------------------------------------------------
    def longitudinal_panel(self, g: GrillageGeometry) -> StiffenedPanelGeometry:
        """Longitudinal stiffener + attached plate, spanning between transverse
        frames (column span ``L_eff = s_T``)."""
        return StiffenedPanelGeometry(
            plate_length=g.transverse.spacing,  # span between transverse frames
            plate_thickness=g.plate_thickness,
            stiffener=g.longitudinal,  # spacing = s_L
        )

    def transverse_panel(self, g: GrillageGeometry) -> StiffenedPanelGeometry:
        """Transverse frame + attached plate, spanning the panel width
        (column span ``L_eff = width_y``)."""
        return StiffenedPanelGeometry(
            plate_length=g.width_y,  # span across the grillage width
            plate_thickness=g.plate_thickness,
            stiffener=g.transverse,  # spacing = s_T
        )

    # ------------------------------------------------------------------
    # Effective plate width (DNV-RP-C201 C_xs) -- used for the smeared rigidity
    # of the (wide) transverse-frame attached plate.
    # ------------------------------------------------------------------
    def effective_width_factor(self, spacing: float, t: float) -> float:
        """DNV-RP-C201 effective-width factor ``C_xs`` for an attached plate
        flange (1.0 for stocky plates, reduced for slender wide plates)."""
        E = self.material.youngs_modulus
        fy = self.material.yield_strength
        lam_p = 0.525 * (spacing / t) * math.sqrt(fy / E)
        if lam_p <= 0.673:
            return 1.0
        return (lam_p - 0.22) / lam_p**2

    # ------------------------------------------------------------------
    # Smeared orthotropic flexural rigidities
    # ------------------------------------------------------------------
    def flexural_rigidities(self, g: GrillageGeometry) -> Dict:
        """Smeared orthotropic plate rigidities ``Dx, Dy, H`` (N*mm).

        ``Dx`` uses the longitudinal stiffener with its full-spacing attached
        plate (validated panel-solver convention); ``Dy`` uses the transverse
        frame with a DNV ``C_xs`` effective-width-reduced attached plate, since
        the transverse spacing is wide and the full width overstates ``I_T``.
        """
        E = self.material.youngs_modulus

        # Longitudinal: I_L over full spacing s_L (validated convention).
        long_panel = self.longitudinal_panel(g)
        long_sec = self.panel_analyzer.effective_section(long_panel)
        I_L = long_sec["I"]
        s_L = g.longitudinal.spacing
        A_L = long_sec["area_web"] + long_sec["area_flange"]  # stiffener only

        # Transverse: I_T with effective-width-reduced attached plate.
        c_xs = self.effective_width_factor(g.transverse.spacing, g.plate_thickness)
        b_eff_T = c_xs * g.transverse.spacing
        trans_eff = StiffenerGeometry(
            web_height=g.transverse.web_height,
            web_thickness=g.transverse.web_thickness,
            flange_width=g.transverse.flange_width,
            flange_thickness=g.transverse.flange_thickness,
            spacing=b_eff_T,
            section_type=g.transverse.section_type,
        )
        trans_eff_panel = StiffenedPanelGeometry(
            plate_length=g.width_y,
            plate_thickness=g.plate_thickness,
            stiffener=trans_eff,
        )
        I_T = self.panel_analyzer.effective_section(trans_eff_panel)["I"]
        s_T = g.transverse.spacing

        Dx = E * I_L / s_L
        Dy = E * I_T / s_T
        H = math.sqrt(Dx * Dy)  # equivalent-isotropic torsional rigidity

        # Smeared axial thickness in the x (compression) direction.
        t_x = g.plate_thickness + A_L / s_L

        return {
            "Dx": Dx,
            "Dy": Dy,
            "H": H,
            "I_L": I_L,
            "I_T": I_T,
            "t_x": t_x,
            "Cxs_transverse": c_xs,
            "effective_width_transverse": b_eff_T,
        }

    # ------------------------------------------------------------------
    # Orthotropic critical force per unit width (the new physics)
    # ------------------------------------------------------------------
    @staticmethod
    def orthotropic_critical_force(
        Dx: float,
        Dy: float,
        H: float,
        a: float,
        b: float,
        m_max: int = 40,
        n_max: int = 4,
    ) -> Dict:
        """Minimum critical axial force per unit width ``N_x,cr`` (N/mm) for a
        simply supported orthotropic plate ``a x b`` under uniaxial ``sigma_x``.

        ``N_x(m, n) = pi^2 [ Dx (m/a)^2 + 2 H (n/b)^2 + Dy (n/b)^4 (a/m)^2 ]``,
        minimised over integer ``m`` in ``1..m_max`` and ``n`` in ``1..n_max``.
        """
        m = np.arange(1, m_max + 1, dtype=float)[:, None]  # x half-waves
        n = np.arange(1, n_max + 1, dtype=float)[None, :]  # y half-waves
        nx = (math.pi**2) * (
            Dx * (m / a) ** 2
            + 2.0 * H * (n / b) ** 2
            + Dy * (n / b) ** 4 * (a / m) ** 2
        )
        idx = np.unravel_index(int(np.argmin(nx)), nx.shape)
        return {
            "Nx_cr": float(nx[idx]),
            "m": int(m[idx[0], 0]),
            "n": int(n[0, idx[1]]),
        }

    def overall_buckling(
        self,
        g: GrillageGeometry,
        sigma_x: float,
        gamma_m: float = 1.15,
    ) -> Dict:
        """Overall orthotropic (grillage) buckling check.

        Suppressed (``applicable = False``) for a single-bay grillage, where the
        smeared idealisation is not meaningful and the longitudinal-stiffener
        column mode governs instead.
        """
        rig = self.flexural_rigidities(g)
        applicable = g.num_frame_bays() >= 2

        ortho = self.orthotropic_critical_force(
            rig["Dx"], rig["Dy"], rig["H"], g.length_x, g.width_y
        )
        sigma_e = ortho["Nx_cr"] / rig["t_x"]
        sigma_cr = self.plate_analyzer.johnson_ostenfeld(sigma_e)

        if applicable and sigma_cr > 0:
            util = sigma_x / (sigma_cr / gamma_m)
        else:
            util = 0.0

        return {
            "applicable": bool(applicable),
            "sigma_e": sigma_e,
            "sigma_cr": sigma_cr,
            "utilization": max(0.0, util),
            "m": ortho["m"],
            "n": ortho["n"],
            "num_frame_bays": g.num_frame_bays(),
            **rig,
        }

    # ------------------------------------------------------------------
    # Plate-field buckling (panel bounded by both stiffener families)
    # ------------------------------------------------------------------
    def plate_field(
        self,
        g: GrillageGeometry,
        sigma_x: float,
        sigma_y: float = 0.0,
        tau: float = 0.0,
        gamma_m: float = 1.15,
    ):
        """Unstiffened plate panel bounded by longitudinals (width ``s_L``) and
        transverse frames (length ``s_T``)."""
        plate = PlateGeometry(
            length=g.transverse.spacing,  # bay length between frames
            width=g.longitudinal.spacing,  # spacing between longitudinals
            thickness=g.plate_thickness,
        )
        return self.plate_analyzer.check_plate_buckling(
            plate, sigma_x, sigma_y=sigma_y, tau=tau, gamma_m=gamma_m
        )

    # ------------------------------------------------------------------
    # Top-level grillage check
    # ------------------------------------------------------------------
    def check_grillage(
        self,
        g: GrillageGeometry,
        sigma_x: float,
        sigma_y: float = 0.0,
        tau: float = 0.0,
        gamma_m: float = 1.15,
    ) -> GrillageBucklingResult:
        """Run all four modes and report the governing (most-utilised) one."""
        # Mode 1: plate field (between both stiffener families).
        plate_res = self.plate_field(
            g, sigma_x, sigma_y=sigma_y, tau=tau, gamma_m=gamma_m
        )
        plate_util = plate_res.utilization

        # Mode 2: longitudinal stiffener (delegated, unchanged).
        long_res = self.panel_analyzer.check_panel(
            self.longitudinal_panel(g),
            sigma_x,
            sigma_y=sigma_y,
            tau=tau,
            gamma_m=gamma_m,
        )
        long_col = long_res.details["column_result"].utilization
        long_tors = long_res.details["torsional"]["utilization"]
        long_util = max(long_col, long_tors)

        # Mode 3: transverse frame -- carries the transverse stress sigma_y as
        # its axial load. Built from the same validated panel methods (column +
        # tripping). The tripping (torsional) check is only evaluated inside the
        # panel solver's validated domain (stiffener spacing <= span and a
        # non-zero axial stress); a widely spaced transverse frame
        # (s_T > 2*width_y) falls outside it, so only column buckling is used
        # there (documented simplification).
        trans_panel = self.transverse_panel(g)
        trans_col_res = self.panel_analyzer.column_buckling(
            trans_panel, sigma_y, gamma_m=gamma_m
        )
        trans_col = trans_col_res.utilization
        trans_tors = 0.0
        tors_in_domain = (
            sigma_y > 0.0 and trans_panel.stiffener.spacing <= trans_panel.plate_length
        )
        if tors_in_domain:
            trans_tors = self.panel_analyzer.torsional_buckling(
                trans_panel, sigma_y, tau=tau, gamma_m=gamma_m
            )["utilization"]
        trans_util = max(trans_col, trans_tors)

        # Mode 4: overall orthotropic grillage (the new physics).
        overall = self.overall_buckling(g, sigma_x, gamma_m=gamma_m)
        overall_util = overall["utilization"]

        modes = {
            "plate_field": (plate_util, plate_res.critical_stress),
            "longitudinal_stiffener": (long_util, long_res.critical_stress),
            "transverse_frame": (trans_util, trans_col_res.critical_stress),
            "overall_grillage": (overall_util, overall["sigma_cr"]),
        }
        governing_mode = max(modes, key=lambda k: modes[k][0])
        governing_util, governing_stress = modes[governing_mode]

        details = {
            "plate_result": plate_res,
            "longitudinal_result": long_res,
            "transverse_column_result": trans_col_res,
            "transverse_tripping_in_domain": bool(tors_in_domain),
            "overall": overall,
            "modes_utilization": {k: v[0] for k, v in modes.items()},
            "preliminary": PRELIMINARY,
            "standard": GRILLAGE_CODE_REFERENCE,
            "section_long": "x=longitudinal (compression); column L_eff=s_T",
        }

        return GrillageBucklingResult(
            governing_mode=governing_mode,
            utilization=governing_util,
            plate_utilization=plate_util,
            longitudinal_utilization=long_util,
            transverse_utilization=trans_util,
            overall_utilization=overall_util,
            critical_stress=governing_stress,
            overall_elastic_stress=overall["sigma_e"],
            overall_critical_stress=overall["sigma_cr"],
            overall_half_waves=int(overall["m"]),
            overall_mode_applicable=bool(overall["applicable"]),
            passes=bool(governing_util <= 1.0),
            details=details,
            code_reference=GRILLAGE_CODE_REFERENCE,
        )
