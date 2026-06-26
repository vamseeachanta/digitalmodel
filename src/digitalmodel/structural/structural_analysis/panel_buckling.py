# ABOUTME: Simplified closed-form stiffened-panel (plate + longitudinal stiffener)
# ABOUTME: buckling solver per DNV-RP-C201. PRELIMINARY / NOT YET VALIDATED.
"""
Stiffened Panel Buckling Analysis (PRELIMINARY)
================================================

A clean, hand-derivable, closed-form buckling solver for a single stiffened
panel made of a plate field reinforced by one longitudinal stiffener
(tee, flat-bar, or angle section), following the simplified spirit of
DNV-RP-C201 ("Buckling Strength of Plated Structures").

Two failure modes are screened:

1. ``plate_induced`` -- buckling of the unstiffened plate field that spans
   *between* longitudinal stiffeners (width = stiffener spacing), delegated to
   :class:`PlateBucklingAnalyzer`.
2. ``column`` -- overall (Euler/flexural) buckling of the plate-stiffener
   combination acting as a column of length equal to the span between
   transverse frames, delegated to :class:`ColumnBucklingAnalyzer`.

The governing utilisation is the larger of the two.

.. warning::

   **PRELIMINARY -- NOT YET VALIDATED.** This stiffened-panel solver has *not*
   been checked against a DNV-RP-C201 worked example. The effective-section
   model uses the full stiffener spacing as the effective plate width (no
   effective-width reduction), and stiffener torsional / tripping failure is
   NOT modelled (see TODO). Results are *indicative only* and must not be used
   for design without independent validation. The module-level flag
   ``PRELIMINARY`` is ``True`` to make this explicit to callers.

Units (consistent with the rest of ``structural_analysis``): stresses in MPa,
lengths in mm, Young's modulus in MPa, areas in mm^2, second moments in mm^4.
"""

from dataclasses import dataclass, field
from typing import Dict

from .models import MaterialProperties, PlateGeometry
from .buckling import PlateBucklingAnalyzer, ColumnBucklingAnalyzer


# Honesty flag: this stiffened-panel solver is preliminary and unvalidated.
PRELIMINARY = True


@dataclass
class StiffenerGeometry:
    """Longitudinal stiffener cross-section.

    All dimensions in mm.

    ``section_type`` is one of ``"tee"``, ``"flatbar"`` or ``"angle"``.
    For a flat-bar the flange dimensions are ignored (set them to 0). For an
    angle the flange is treated, in this simplified vertical-section model,
    identically to a tee flange sitting at the top of the web (the lateral
    offset of an angle flange is neglected -- a documented simplification).
    """

    web_height: float       # h_w (mm)
    web_thickness: float     # t_w (mm)
    flange_width: float      # b_f (mm) -- 0 for flatbar
    flange_thickness: float  # t_f (mm) -- 0 for flatbar
    spacing: float           # s, spacing between stiffeners (mm)
    section_type: str = "tee"

    def __post_init__(self):
        st = self.section_type.lower()
        if st not in ("tee", "flatbar", "angle"):
            raise ValueError(
                f"section_type must be 'tee', 'flatbar' or 'angle', got {self.section_type!r}"
            )
        self.section_type = st
        if st == "flatbar":
            # A flat-bar has no flange.
            self.flange_width = 0.0
            self.flange_thickness = 0.0


@dataclass
class StiffenedPanelGeometry:
    """One stiffened panel: a plate field plus a single longitudinal stiffener.

    The plate width carried by one stiffener equals ``stiffener.spacing``.
    """

    plate_length: float      # span between transverse frames, L (mm)
    plate_thickness: float   # t_p (mm)
    stiffener: StiffenerGeometry


@dataclass
class PanelBucklingResult:
    """Result of a stiffened-panel buckling check."""

    governing_mode: str
    utilization: float
    plate_utilization: float
    stiffener_utilization: float
    column_utilization: float
    critical_stress: float   # MPa (governing mode)
    passes: bool
    details: Dict = field(default_factory=dict)


class StiffenedPanelBucklingAnalyzer:
    """Simplified stiffened-panel buckling analyzer (PRELIMINARY).

    See the module docstring for the validation warning and modelling
    assumptions.
    """

    def __init__(self, material: MaterialProperties):
        self.material = material
        self.plate_analyzer = PlateBucklingAnalyzer(material)
        self.column_analyzer = ColumnBucklingAnalyzer(material)

    # ------------------------------------------------------------------
    # Effective combined section (plate + stiffener)
    # ------------------------------------------------------------------
    def effective_section(self, panel: StiffenedPanelGeometry) -> Dict:
        """Combined plate + stiffener section properties.

        Effective plate width is taken as the *full* stiffener spacing (a
        simplifying assumption -- no effective-width reduction is applied in
        this preliminary version).

        Reference axis: plate mid-thickness, with positive ``y`` pointing from
        the plate toward the stiffener. Returns a dict with component areas,
        neutral-axis position (mm from plate mid-thickness), second moment of
        area ``I`` about the neutral axis (mm^4), and radius of gyration ``r``
        (mm).
        """
        st = panel.stiffener
        t_p = panel.plate_thickness
        s = st.spacing

        # --- Component areas (mm^2) ---
        a_plate = s * t_p
        a_web = st.web_height * st.web_thickness
        a_flange = st.flange_width * st.flange_thickness
        a_total = a_plate + a_web + a_flange

        # --- Component centroids, y from plate mid-thickness (mm) ---
        y_plate = 0.0
        y_web = t_p / 2.0 + st.web_height / 2.0
        y_flange = t_p / 2.0 + st.web_height + st.flange_thickness / 2.0

        # --- Neutral axis (mm from plate mid-thickness) ---
        y_na = (a_plate * y_plate + a_web * y_web + a_flange * y_flange) / a_total

        # --- Own (centroidal) second moments (mm^4) ---
        i_plate_own = s * t_p ** 3 / 12.0
        i_web_own = st.web_thickness * st.web_height ** 3 / 12.0
        i_flange_own = (
            st.flange_width * st.flange_thickness ** 3 / 12.0
            if a_flange > 0
            else 0.0
        )

        # --- Parallel-axis transfer to the neutral axis ---
        i_na = (
            i_plate_own + a_plate * (y_plate - y_na) ** 2
            + i_web_own + a_web * (y_web - y_na) ** 2
            + i_flange_own + a_flange * (y_flange - y_na) ** 2
        )

        r = (i_na / a_total) ** 0.5 if a_total > 0 else 0.0

        return {
            "area_plate": a_plate,
            "area_web": a_web,
            "area_flange": a_flange,
            "area_total": a_total,
            "neutral_axis": y_na,        # mm from plate mid-thickness
            "I": i_na,                   # mm^4 about NA
            "r": r,                      # mm
            "effective_plate_width": s,  # full spacing (assumption)
        }

    # ------------------------------------------------------------------
    # Overall column buckling of the plate-stiffener combination
    # ------------------------------------------------------------------
    def column_buckling(
        self,
        panel: StiffenedPanelGeometry,
        sigma_x: float,
        gamma_m: float = 1.15,
    ):
        """Treat the plate+stiffener as a built-up column of length
        ``plate_length`` carrying a uniform axial stress ``sigma_x`` (MPa,
        positive = compression).

        Effective length L_eff = plate_length (pinned ends, factor 1.0).
        Built-up welded section -> EC3 buckling curve "c".
        """
        sec = self.effective_section(panel)
        axial_force = sigma_x * sec["area_total"]  # N (MPa * mm^2)
        return self.column_analyzer.check_column_buckling(
            axial_force=axial_force,
            area=sec["area_total"],
            I_min=sec["I"],
            L_eff=panel.plate_length,
            buckling_curve="c",
            gamma_m=gamma_m,
        )

    # ------------------------------------------------------------------
    # Plate-field buckling between stiffeners
    # ------------------------------------------------------------------
    def plate_induced(
        self,
        panel: StiffenedPanelGeometry,
        sigma_x: float,
        sigma_y: float = 0.0,
        tau: float = 0.0,
        gamma_m: float = 1.15,
    ):
        """Buckling of the plate field spanning between stiffeners.

        The plate field has length = ``plate_length`` (between transverse
        frames) and width = stiffener spacing.
        """
        plate = PlateGeometry(
            length=panel.plate_length,
            width=panel.stiffener.spacing,
            thickness=panel.plate_thickness,
        )
        return self.plate_analyzer.check_plate_buckling(
            plate, sigma_x, sigma_y=sigma_y, tau=tau, gamma_m=gamma_m
        )

    # ------------------------------------------------------------------
    # Top-level panel check
    # ------------------------------------------------------------------
    def check_panel(
        self,
        panel: StiffenedPanelGeometry,
        sigma_x: float,
        sigma_y: float = 0.0,
        tau: float = 0.0,
        gamma_m: float = 1.15,
    ) -> PanelBucklingResult:
        """Run plate-field and overall-column buckling checks and report the
        governing (most-utilised) mode.

        TODO: stiffener torsional / tripping (lateral-torsional) failure is not
        yet modelled. For deep, thin webs this can govern and is currently
        absent -- another reason this solver is flagged PRELIMINARY.
        """
        plate_res = self.plate_induced(
            panel, sigma_x, sigma_y=sigma_y, tau=tau, gamma_m=gamma_m
        )
        col_res = self.column_buckling(panel, sigma_x, gamma_m=gamma_m)

        plate_util = plate_res.utilization
        col_util = col_res.utilization
        # Stiffener tripping not modelled -> 0.0 (documented limitation).
        stiffener_util = 0.0

        if col_util >= plate_util:
            governing_mode = "column"
            governing_util = col_util
            critical_stress = col_res.critical_stress
        else:
            governing_mode = "plate_induced"
            governing_util = plate_util
            critical_stress = plate_res.critical_stress

        details = {
            "plate_result": plate_res,
            "column_result": col_res,
            "section": self.effective_section(panel),
            "preliminary": PRELIMINARY,
            "stiffener_tripping_modelled": False,
            "note": (
                "PRELIMINARY stiffened-panel solver: not validated against a "
                "DNV-RP-C201 worked example; stiffener tripping not modelled."
            ),
        }

        return PanelBucklingResult(
            governing_mode=governing_mode,
            utilization=governing_util,
            plate_utilization=plate_util,
            stiffener_utilization=stiffener_util,
            column_utilization=col_util,
            critical_stress=critical_stress,
            passes=governing_util <= 1.0,
            details=details,
        )
