# ABOUTME: Closed-form stiffened-panel (plate + longitudinal stiffener) buckling
# ABOUTME: solver per DNV-RP-C201. VALIDATED against the 0119-015 worked example.
"""
Stiffened Panel Buckling Analysis
=================================

A clean, hand-derivable, closed-form buckling solver for a single stiffened
panel made of a plate field reinforced by one longitudinal stiffener
(tee, flat-bar, or angle section), following DNV-RP-C201 ("Buckling Strength of
Plated Structures").

Three failure modes are screened and the governing (most-utilised) one is
reported:

1. ``plate_induced`` -- buckling of the unstiffened plate field that spans
   *between* longitudinal stiffeners (width = stiffener spacing), delegated to
   :class:`PlateBucklingAnalyzer`.
2. ``column`` -- overall (Euler/flexural) buckling of the plate-stiffener
   combination acting as a column of length equal to the span between
   transverse frames, delegated to :class:`ColumnBucklingAnalyzer`.
3. ``torsional`` -- lateral-torsional (tripping) buckling of the stiffener,
   per DNV-RP-C201 Sec. 7.5.2.

Validation (see ``docs/domains/plate-buckling/panel-buckling-validation-2026-06-26.md``)
---------------------------------------------------------------------------------------
Validated against the 0119-015 stiffened-panel worked example (plate 700 x 12 mm,
tee stiffener web 600 x 10 / flange 200 x 20 mm, fy 234.6 MPa, E 210 105 MPa):

==========================  ===========  ===========  =========
Quantity                    This solver  Reference    Delta
==========================  ===========  ===========  =========
Effective area A (mm^2)        18 400       18 400      0.0%
Neutral axis (mm, outer)       239.70       239.70      0.0%
Second moment I (mm^4)       1.255e9      1.255e9*     0.0%
Plate elastic stress (MPa)    223.3        223.52      0.1%
Column slenderness lambda     0.090        0.0902      0.2%
Torsional fET (MPa)           442.1        441.66      0.1%
Torsional slenderness lT      0.7285       0.729       0.1%
Torsional resistance fT       215.6        215.61      0.0%
==========================  ===========  ===========  =========

\\* The legacy spreadsheet reports I = 1.273e9 because its flange parallel-axis
term mixes the neutral-axis reference (a 6 mm error); the hand calculation
1.255e9 is correct and is what this solver returns.

Remaining modelling limitations (documented, not blocking)
----------------------------------------------------------
- Effective plate width is taken as the full stiffener spacing (no
  effective-width reduction for very slender plate fields).
- Lateral pressure / continuous-beam bending of the stiffener is not included
  (pure in-plane stress screening).
- A single longitudinal stiffener with its associated plate flange is modelled;
  girder-level (cross-stiffener) interaction is out of scope.

Units (consistent with the rest of ``structural_analysis``): stresses in MPa,
lengths in mm, Young's modulus in MPa, areas in mm^2, second moments in mm^4.
"""

import math
from dataclasses import dataclass, field
from typing import Dict, Optional

from .models import MaterialProperties, PlateGeometry
from .buckling import PlateBucklingAnalyzer, ColumnBucklingAnalyzer
from digitalmodel.codes import DNV_RP_C201


# This stiffened-panel solver is validated (see module docstring) -> production.
PRELIMINARY = False


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

    ``torsional_restraint_spacing`` is the distance between points where the
    stiffener is restrained against torsion (tripping brackets / transverse
    members), L_T in DNV-RP-C201. If not given it defaults to ``plate_length``.
    """

    plate_length: float      # span between transverse frames, L (mm)
    plate_thickness: float   # t_p (mm)
    stiffener: StiffenerGeometry
    torsional_restraint_spacing: Optional[float] = None  # L_T (mm)

    def __post_init__(self):
        if self.torsional_restraint_spacing is None:
            self.torsional_restraint_spacing = self.plate_length


@dataclass
class PanelBucklingResult:
    """Result of a stiffened-panel buckling check."""

    governing_mode: str
    utilization: float
    plate_utilization: float
    stiffener_utilization: float   # torsional (tripping) utilisation
    column_utilization: float
    critical_stress: float   # MPa (governing mode)
    passes: bool
    details: Dict = field(default_factory=dict)
    code_reference: str = ""   # governing code, e.g. "DNV-RP-C201 (2010)"


class StiffenedPanelBucklingAnalyzer:
    """Stiffened-panel buckling analyzer per DNV-RP-C201.

    See the module docstring for the validation table and modelling
    assumptions.
    """

    def __init__(self, material: MaterialProperties):
        self.material = material
        self.plate_analyzer = PlateBucklingAnalyzer(material)
        self.column_analyzer = ColumnBucklingAnalyzer(material)

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------
    @staticmethod
    def _von_mises(sigma_x: float, sigma_y: float, tau: float) -> float:
        """Plane-stress von Mises equivalent stress (MPa)."""
        return math.sqrt(
            sigma_x ** 2 + sigma_y ** 2 - sigma_x * sigma_y + 3.0 * tau ** 2
        )

    def plate_elastic_reference(self, panel: StiffenedPanelGeometry) -> Dict:
        """DNV-RP-C201 plate-field elastic buckling reference stresses (MPa).

        f_Epx = 3.62 E (t/s)^2, f_Epy = 0.9 E (t/s)^2, f_Ept = 5.0 E (t/s)^2.
        """
        E = self.material.youngs_modulus
        t = panel.plate_thickness
        s = panel.stiffener.spacing
        base = E * (t / s) ** 2
        return {
            "fEpx": 3.62 * base,
            "fEpy": 0.9 * base,
            "fEpt": 5.0 * base,
        }

    # ------------------------------------------------------------------
    # Effective combined section (plate + stiffener)
    # ------------------------------------------------------------------
    def effective_section(self, panel: StiffenedPanelGeometry) -> Dict:
        """Combined plate + stiffener section properties.

        Effective plate width is taken as the *full* stiffener spacing.

        Reference axis: plate mid-thickness, with positive ``y`` pointing from
        the plate toward the stiffener. Returns a dict with component areas,
        neutral-axis position (mm from plate mid-thickness *and* from the plate
        outer face), second moment of area ``I`` about the neutral axis
        (mm^4), and radius of gyration ``r`` (mm).
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

        # --- Parallel-axis transfer to the neutral axis (consistent NA) ---
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
            "neutral_axis": y_na,                 # mm from plate mid-thickness
            "neutral_axis_outer": y_na + t_p / 2.0,  # mm from plate outer face
            "I": i_na,                            # mm^4 about NA
            "r": r,                               # mm
            "effective_plate_width": s,           # full spacing (assumption)
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
    # Stiffener torsional (tripping) buckling -- DNV-RP-C201 Sec. 7.5.2
    # ------------------------------------------------------------------
    def torsional_buckling(
        self,
        panel: StiffenedPanelGeometry,
        sigma_x: float,
        sigma_y: float = 0.0,
        tau: float = 0.0,
        gamma_m: float = 1.15,
    ) -> Dict:
        """Lateral-torsional (tripping) buckling of the stiffener.

        Implements the DNV-RP-C201 Sec. 7.5.2 closed form, including the
        plate rotational-restraint coefficient ``beta`` derived from the
        applied-stress interaction. Validated against the 0119-015 worked
        example (fET=441.66 MPa, lambda_T=0.729, fT=215.61 MPa).

        Returns a dict with ``fET`` (elastic torsional buckling stress),
        ``lambda_T`` (torsional slenderness), ``fT`` (characteristic torsional
        buckling resistance) and ``utilization`` (= sigma_x / (fT/gamma_m)).
        """
        st = panel.stiffener
        t_p = panel.plate_thickness
        s = st.spacing
        E = self.material.youngs_modulus
        nu = self.material.poissons_ratio
        fy = self.material.yield_strength
        G = E / (2.0 * (1.0 + nu))

        h_w, t_w = st.web_height, st.web_thickness
        b_f, t_f = st.flange_width, st.flange_thickness
        A_w = h_w * t_w
        A_f = b_f * t_f
        L_t = panel.torsional_restraint_spacing

        # Flange moment of inertia about the web line (DNV warping term, I_z).
        I_z = t_f * b_f ** 3 / 12.0

        # --- Plate rotational-restraint coefficient beta (DNV-RP-C201) ---
        ref = self.plate_elastic_reference(panel)
        sigma_j = self._von_mises(sigma_x, sigma_y, tau)
        if sigma_j <= 0:
            beta = 1.0  # no in-plane stress -> no plate restraint reduction
        else:
            c = 2.0 - s / panel.plate_length
            interaction = (
                (sigma_x / ref["fEpx"]) ** c
                + (sigma_y / ref["fEpy"]) ** c
                + (tau / ref["fEpt"]) ** c
            ) ** (1.0 / c)
            lambda_e = fy / sigma_j * interaction
            fe_p = fy / math.sqrt(1.0 + lambda_e ** 2)
            x3 = sigma_j / fe_p
            restraint = max(0.0, 1.0 - x3)
            C = (h_w / s) * (t_p / t_w) ** 3 * math.sqrt(restraint)
            beta = (3.0 * C + 0.2) / (C + 0.2)

        # --- Elastic torsional buckling stress fET ---
        if A_f > 0:
            term1 = (
                (A_w + (t_f / t_w) ** 2 * A_f) / (A_w + 3.0 * A_f)
                * beta * G * (t_w / h_w) ** 2
            )
            term2 = math.pi ** 2 * E * I_z / ((A_w / 3.0 + A_f) * L_t ** 2)
        else:
            # Flat-bar: no flange -> warping term vanishes, web St-Venant only.
            term1 = beta * G * (t_w / h_w) ** 2
            term2 = 0.0
        fET = term1 + term2

        # --- Reduction (DNV column-type curve for torsional buckling) ---
        lambda_T = math.sqrt(fy / fET) if fET > 0 else float("inf")
        if lambda_T > 0.6:
            mu = 0.35 * (lambda_T - 0.6)
            fT = (
                (1.0 + mu + lambda_T ** 2
                 - math.sqrt((1.0 + mu + lambda_T ** 2) ** 2 - 4.0 * lambda_T ** 2))
                / (2.0 * lambda_T ** 2)
            ) * fy
        else:
            fT = fy

        util = sigma_x / (fT / gamma_m) if fT > 0 else float("inf")
        return {
            "fET": fET,
            "lambda_T": lambda_T,
            "fT": fT,
            "beta": beta,
            "utilization": max(0.0, util),
        }

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
        """Run plate-field, overall-column and stiffener-torsional buckling
        checks and report the governing (most-utilised) mode.
        """
        plate_res = self.plate_induced(
            panel, sigma_x, sigma_y=sigma_y, tau=tau, gamma_m=gamma_m
        )
        col_res = self.column_buckling(panel, sigma_x, gamma_m=gamma_m)
        tors = self.torsional_buckling(
            panel, sigma_x, sigma_y=sigma_y, tau=tau, gamma_m=gamma_m
        )

        plate_util = plate_res.utilization
        col_util = col_res.utilization
        tors_util = tors["utilization"]

        modes = {
            "plate_induced": (plate_util, plate_res.critical_stress),
            "column": (col_util, col_res.critical_stress),
            "torsional": (tors_util, tors["fT"]),
        }
        governing_mode = max(modes, key=lambda m: modes[m][0])
        governing_util, critical_stress = modes[governing_mode]

        details = {
            "plate_result": plate_res,
            "column_result": col_res,
            "torsional": tors,
            "section": self.effective_section(panel),
            "preliminary": PRELIMINARY,
            "stiffener_tripping_modelled": True,
            "standard": "DNV-RP-C201",
        }

        return PanelBucklingResult(
            governing_mode=governing_mode,
            utilization=governing_util,
            plate_utilization=plate_util,
            stiffener_utilization=tors_util,
            column_utilization=col_util,
            critical_stress=critical_stress,
            passes=governing_util <= 1.0,
            details=details,
            code_reference=DNV_RP_C201.label,
        )
