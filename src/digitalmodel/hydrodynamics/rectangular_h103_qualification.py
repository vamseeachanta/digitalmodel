"""Qualification diagnostics for the legacy rectangular H103 approximation.

This opt-in API neither imports the legacy solver adapter nor changes its
coefficients. It reports geometry, supplied finite table coverage and flow
numbers; it does not establish hydrodynamic validity. Overall qualification
remains NOT_EVALUATED pending a geometry/flow-specific engineering basis and
an independent force benchmark. Horizontal use of the heave correction is
unqualified. The heave basis also needs independent source readback.

Sources opened for the dimensionless definitions:
* NASA Glenn, 'Reynolds Number' (undated), Re = U D / nu:
  https://www.grc.nasa.gov/www/k-12/airplane/reynolds.html
* 'Forces on Cylinders and Plates in an Oscillating Fluid' (1958), Journal of
  Research of the National Bureau of Standards 60(5), p.423, UT/D:
  https://nvlpubs.nist.gov/nistpubs/jres/60/jresv60n5p423_A1b.pdf

DNV-RP-H103 (April 2011), Table A-2 and section 4.6.3.3 are the issue's
requested qualification targets, not independently verified sources here.
No licensed table values or inferred applicability thresholds are supplied.
"""

import math
from dataclasses import dataclass


REYNOLDS_SOURCE = (
    "NASA Glenn, Reynolds Number (undated), Re=UD/nu; "
    "https://www.grc.nasa.gov/www/k-12/airplane/reynolds.html"
)
KC_SOURCE = (
    "Forces on Cylinders and Plates in an Oscillating Fluid (1958), "
    "J. Res. NBS 60(5), p.423, UT/D; "
    "https://nvlpubs.nist.gov/nistpubs/jres/60/jresv60n5p423_A1b.pdf"
)


def _positive(name: str, value: float) -> None:
    """Reject non-finite or nonpositive lengths, times and viscosity."""
    if not math.isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be finite and positive")


@dataclass(frozen=True)
class FiniteLookupDomain:
    """Caller-sourced finite tabulated ratio endpoints, excluding sentinels.

    The source should identify document, edition and table. Its authenticity
    is not verified by this class. A large finite substitute for infinity is
    not a tabulated endpoint and shall not be passed as ``upper``. Only the
    caller with access to the original table can establish that distinction.
    """

    lower: float
    upper: float
    source: str

    def __post_init__(self) -> None:
        _positive("lower", self.lower)
        _positive("upper", self.upper)
        if self.lower > self.upper:
            raise ValueError("lower must not exceed upper")
        if not isinstance(self.source, str) or not self.source.strip():
            raise ValueError("source must identify the finite table domain")

    def coverage(self, ratio: float) -> str:
        """Report inclusive numeric coverage, never engineering acceptance."""
        _positive("ratio", ratio)
        if self.lower <= ratio <= self.upper:
            return "WITHIN_SUPPLIED_DOMAIN"
        return "OUTSIDE_SUPPLIED_DOMAIN"


def flow_numbers(
    *,
    speed_m_s: float,
    characteristic_length_m: float,
    kinematic_viscosity_m2_s: float | None = None,
    period_s: float | None = None,
) -> dict:
    """Return Re and KC without assuming a fluid property or acceptance limit.

    Speed is a nonnegative magnitude; for sinusoidal flow it is the peak
    relative velocity. The caller supplies the characteristic length justified
    by the selected reference geometry; sqrt(projected area) is not selected
    automatically. Missing viscosity or period yields None, not a true zero.
    A missing period does not establish steady flow.
    """
    if not math.isfinite(speed_m_s) or speed_m_s < 0:
        raise ValueError("speed_m_s must be finite and nonnegative")
    _positive("characteristic_length_m", characteristic_length_m)
    missing = []
    for name, value in (
        ("kinematic_viscosity_m2_s", kinematic_viscosity_m2_s),
        ("period_s", period_s),
    ):
        if value is None:
            missing.append(name)
        else:
            _positive(name, value)
    # NASA Glenn, Reynolds Number (undated): Re=UD/nu; no fitted coefficient.
    reynolds = (
        None
        if kinematic_viscosity_m2_s is None
        else speed_m_s * characteristic_length_m / kinematic_viscosity_m2_s
    )
    # J. Res. NBS 60(5) (1958), p.423: period parameter UT/D.
    kc = None if period_s is None else speed_m_s * period_s / characteristic_length_m
    for name, value in (
        ("reynolds_number", reynolds),
        ("keulegan_carpenter_number", kc),
    ):
        if value is not None and not math.isfinite(value):
            raise ValueError(f"{name} exceeds finite arithmetic range")
    return {
        "reynolds_number": reynolds,
        "keulegan_carpenter_number": kc,
        "status": "NOT_EVALUATED",
        "limitation": "No geometry-specific Reynolds or oscillatory-flow limits supplied",
        "missing_inputs": missing,
        "sources": [REYNOLDS_SOURCE, KC_SOURCE],
    }


def _axis_diagnostics(
    a: float,
    b: float,
    c: float,
    plate: FiniteLookupDomain | None,
    prism: FiniteLookupDomain | None,
    added_mass: FiniteLookupDomain | None,
) -> dict:
    """Mirror legacy projection arithmetic, without evaluating coefficients."""
    area = a * b  # Rectangular projected face area; geometric identity.
    _positive("derived geometry", area)
    equivalent_width = math.sqrt(area)
    projected_ratio = b / a
    streamwise_ratio = c / equivalent_width
    for value in (area, equivalent_width, projected_ratio, streamwise_ratio):
        _positive("derived geometry", value)
    # Existing code_dnvrph103_hydrodynamics_rectangular.get_translational_cd:
    # aspect_ratio < 1 selects plate; equality selects prism. This is a legacy
    # branch boundary, not a source-backed engineering applicability limit.
    branch = "plate" if streamwise_ratio < 1 else "prism"
    domain = plate if branch == "plate" else prism
    geometry = "PLATE_APPROXIMATION"
    if branch == "prism":
        geometry = (
            "SQUARE_ROD_GEOMETRY_ONLY" if a == b else "NON_SQUARE_ROD_APPROXIMATION"
        )
    return {
        "projected_area_m2": area,
        "equivalent_width_m": equivalent_width,
        "projected_aspect_ratio": projected_ratio,
        "streamwise_aspect_ratio": streamwise_ratio,
        "drag_branch": branch,
        "drag_geometry": geometry,
        "drag_lookup_status": (
            "NOT_EVALUATED"
            if domain is None
            else domain.coverage(
                projected_ratio if branch == "plate" else streamwise_ratio
            )
        ),
        "added_mass_lookup_status": (
            "NOT_EVALUATED"
            if added_mass is None
            else added_mass.coverage(projected_ratio)
        ),
        "added_mass_applicability": "NOT_EVALUATED",
    }


def qualify_rectangular_h103(
    *,
    length_m: float,
    width_m: float,
    height_m: float,
    plate_drag_domain: FiniteLookupDomain | None = None,
    prism_drag_domain: FiniteLookupDomain | None = None,
    added_mass_domain: FiniteLookupDomain | None = None,
) -> dict:
    """Return axis diagnostics alongside, rather than modifying, legacy output.

    x is along length, y along width, z along height. Each projected face is
    sorted into a<=b; c is streamwise depth. Areas are gross outer-envelope
    projections, before perforation corrections. Supplied domains describe
    original finite table nodes, never the infinity replacement. Beyond them
    the API reports OUTSIDE_SUPPLIED_DOMAIN and performs no interpolation.

    Example: qualify_rectangular_h103(length_m=8, width_m=2, height_m=1).
    These synthetic dimensions define geometry only; no engineering properties
    or applicability limits are assumed. Inspect status and each axis record.
    """
    for name, value in (
        ("length_m", length_m),
        ("width_m", width_m),
        ("height_m", height_m),
    ):
        _positive(name, value)
    faces = {
        "x": (width_m, height_m, length_m),
        "y": (length_m, height_m, width_m),
        "z": (length_m, width_m, height_m),
    }
    axes = {}
    for axis, (first, second, depth) in faces.items():
        axes[axis] = _axis_diagnostics(
            min(first, second),
            max(first, second),
            depth,
            plate_drag_domain,
            prism_drag_domain,
            added_mass_domain,
        )
    return {
        "status": "NOT_EVALUATED",
        "axes": axes,
        "lookup_sources": {
            name: None if domain is None else domain.source
            for name, domain in (
                ("plate_drag", plate_drag_domain),
                ("prism_drag", prism_drag_domain),
                ("added_mass", added_mass_domain),
            )
        },
        "limitations": [
            "Horizontal permutation of the heave correction lacks an established basis",
            "Heave source readback and independent force benchmark remain outstanding",
            "Square-rod geometry alone does not establish drag applicability",
            "Finite table coverage does not establish physical applicability",
        ],
    }
