"""Cross-section property computation via sectionproperties.

Provides a standardized API for computing structural section properties
for I-sections, CHS, RHS, angles, channels, and custom polygons.

Reference: vamseeachanta/workspace-hub#1498
"""

from dataclasses import asdict, dataclass, field
from typing import Optional

import matplotlib

matplotlib.use("Agg")

from sectionproperties.analysis import Section
from sectionproperties.pre.library import (
    angle_section,
    channel_section,
    circular_hollow_section,
    i_section,
    rectangular_hollow_section,
)


@dataclass
class SectionProperties:
    """Computed cross-section properties.

    All values use consistent mm-based units:
        area  : mm^2
        ixx   : mm^4  (second moment of area, centroidal x)
        iyy   : mm^4  (second moment of area, centroidal y)
        zx    : mm^3  (plastic section modulus, x — AISC "Z")
        sx    : mm^3  (elastic section modulus, x — AISC "S")
        j     : mm^4  (St. Venant torsion constant)
        cw    : mm^6  (warping constant)
        zy/sy : mm^3  (y-axis moduli, when applicable)
        rx/ry : mm    (radii of gyration)
    """

    label: str
    section_type: str  # "i_section", "chs", "rhs", "angle", "channel", "custom"
    area: float
    ixx: float
    iyy: float
    zx: float
    sx: float
    j: float
    cw: float
    zy: Optional[float] = None
    sy: Optional[float] = None
    rx: Optional[float] = None
    ry: Optional[float] = None

    def to_dict(self) -> dict:
        """Return properties as a dictionary for pipeline consumption.

        All values are JSON-serializable (floats and strings).
        """
        return asdict(self)


def _analyze_section(geom, label: str, section_type: str, mesh_size: float) -> SectionProperties:
    """Internal helper: mesh, analyze, and extract properties.

    Steps:
        1. Create mesh with given mesh_size
        2. Run geometric, warping, and plastic analyses
        3. Extract all properties into SectionProperties dataclass

    API notes (sectionproperties without Material):
        - get_ic()    -> (ixx_c, iyy_c, ixy_c)
        - get_z()     -> (sxx+, sxx-, syy+, syy-)  plastic moduli (AISC "Z")
        - get_s()     -> (zxx+, zxx-)               elastic moduli (AISC "S")
        - get_j()     -> J (torsion constant)
        - get_gamma() -> Cw (warping constant)
        - get_area()  -> A
    """
    geom.create_mesh(mesh_sizes=[mesh_size])
    sec = Section(geom)
    sec.calculate_geometric_properties()
    sec.calculate_warping_properties()
    sec.calculate_plastic_properties()

    ic = sec.get_ic()  # (ixx_c, iyy_c, ixy_c)
    z_plastic = sec.get_z()  # (sxx+, sxx-, syy+, syy-)
    s_elastic = sec.get_s()  # (zxx+, zxx-)

    area = sec.get_area()
    ixx = ic[0]
    iyy = ic[1]

    # Radii of gyration: r = sqrt(I / A)
    rx = (ixx / area) ** 0.5 if area > 0 else 0.0
    ry = (iyy / area) ** 0.5 if area > 0 else 0.0

    return SectionProperties(
        label=label or f"{section_type}",
        section_type=section_type,
        area=area,
        ixx=ixx,
        iyy=iyy,
        zx=z_plastic[0],  # sxx+ (plastic, x)
        sx=s_elastic[0],  # zxx+ (elastic, x)
        j=sec.get_j(),
        cw=sec.get_gamma(),
        zy=z_plastic[2],  # syy+ (plastic, y)
        sy=None,  # sectionproperties get_s() only returns x-axis elastic moduli
        rx=rx,
        ry=ry,
    )


def compute_i_section(
    d: float,
    b: float,
    t_f: float,
    t_w: float,
    r: float,
    n_r: int = 16,
    mesh_size: float = 10,
    label: Optional[str] = None,
) -> SectionProperties:
    """Compute properties for an I-section (wide-flange / H-section).

    Args:
        d: Overall depth (mm).
        b: Flange width (mm).
        t_f: Flange thickness (mm).
        t_w: Web thickness (mm).
        r: Fillet radius (mm).
        n_r: Number of points discretising the fillet radius.
        mesh_size: Target finite-element mesh area (mm^2).
        label: Optional display label.

    Returns:
        SectionProperties dataclass with computed values.
    """
    geom = i_section(d=d, b=b, t_f=t_f, t_w=t_w, r=r, n_r=n_r)
    return _analyze_section(geom, label or f"I {d:.0f}x{b:.0f}x{t_f:.1f}", "i_section", mesh_size)


def compute_chs(
    d: float,
    t: float,
    n: int = 64,
    mesh_size: float = 15,
    label: Optional[str] = None,
) -> SectionProperties:
    """Compute properties for a circular hollow section (CHS).

    Args:
        d: Outside diameter (mm).
        t: Wall thickness (mm).
        n: Number of points discretising the circle.
        mesh_size: Target finite-element mesh area (mm^2).
        label: Optional display label.

    Returns:
        SectionProperties dataclass with computed values.
    """
    geom = circular_hollow_section(d=d, t=t, n=n)
    return _analyze_section(geom, label or f"CHS {d:.0f}x{t:.0f}", "chs", mesh_size)


def compute_rhs(
    d: float,
    b: float,
    t: float,
    r_out: float,
    n_r: int = 8,
    mesh_size: float = 10,
    label: Optional[str] = None,
) -> SectionProperties:
    """Compute properties for a rectangular hollow section (RHS).

    Args:
        d: Depth (mm).
        b: Width (mm).
        t: Wall thickness (mm).
        r_out: Outside corner radius (mm).
        n_r: Number of points discretising the corner radius.
        mesh_size: Target finite-element mesh area (mm^2).
        label: Optional display label.

    Returns:
        SectionProperties dataclass with computed values.
    """
    geom = rectangular_hollow_section(d=d, b=b, t=t, r_out=r_out, n_r=n_r)
    return _analyze_section(geom, label or f"RHS {d:.0f}x{b:.0f}x{t:.0f}", "rhs", mesh_size)


def compute_angle(
    d: float,
    b: float,
    t: float,
    r_r: float,
    r_t: float,
    n_r: int = 8,
    mesh_size: float = 5,
    label: Optional[str] = None,
) -> SectionProperties:
    """Compute properties for an angle section (L-section).

    Args:
        d: Depth (mm).
        b: Width (mm).
        t: Thickness (mm).
        r_r: Root fillet radius (mm).
        r_t: Toe fillet radius (mm).
        n_r: Number of points discretising the fillet radii.
        mesh_size: Target finite-element mesh area (mm^2).
        label: Optional display label.

    Returns:
        SectionProperties dataclass with computed values.
    """
    geom = angle_section(d=d, b=b, t=t, r_r=r_r, r_t=r_t, n_r=n_r)
    return _analyze_section(geom, label or f"L {d:.0f}x{b:.0f}x{t:.0f}", "angle", mesh_size)


def compute_channel(
    d: float,
    b: float,
    t_f: float,
    t_w: float,
    r: float,
    n_r: int = 8,
    mesh_size: float = 10,
    label: Optional[str] = None,
) -> SectionProperties:
    """Compute properties for a channel section (C-section).

    Args:
        d: Overall depth (mm).
        b: Flange width (mm).
        t_f: Flange thickness (mm).
        t_w: Web thickness (mm).
        r: Fillet radius (mm).
        n_r: Number of points discretising the fillet radius.
        mesh_size: Target finite-element mesh area (mm^2).
        label: Optional display label.

    Returns:
        SectionProperties dataclass with computed values.
    """
    geom = channel_section(d=d, b=b, t_f=t_f, t_w=t_w, r=r, n_r=n_r)
    return _analyze_section(geom, label or f"C {d:.0f}x{b:.0f}", "channel", mesh_size)
