"""Flexible representation of the IEA 15 MW / VolturnUS-S fowt spec.

The tower as a finite-element line from the workbook's floating-tower station
table, and the RNA as a lumped buoy with its inertia about its own centre.
Split from ``iea15_orcaflex_spec``, which still provides every name here.
"""

from __future__ import annotations

import math

from digitalmodel.floating_wind.iea15_orcaflex_spec import _v, build_spec

STEP_SPAN = 0.01  # station pairs closer than this (m) are wall-thickness steps


def _tube(outer: float, thickness: float) -> tuple[float, float]:
    """Area (m2) and transverse second moment of area (m4) of a circular tube."""
    inner = outer - 2.0 * thickness
    return (math.pi / 4.0 * (outer ** 2 - inner ** 2),
            math.pi / 64.0 * (outer ** 4 - inner ** 4))


def tower_moduli(data: dict) -> dict[str, list[float]]:
    """Young's and shear moduli implied by each tabulated station (Pa).

    E = EI / I and E = EA / A, G = GJ / (2 I), from the station's diameter
    and wall thickness. The sheet's values imply one steel at every station
    (tested), which justifies the tube formulas used for tapered spans.
    """
    moduli: dict[str, list[float]] = {"E_bending": [], "E_axial": [], "G": []}
    for row in _v(data["floating_tower"]):
        area, inertia = _tube(row[1], row[2] / 1000.0)
        moduli["E_bending"].append(row[6] / inertia)
        moduli["E_axial"].append(row[9] / area)
        moduli["G"].append(row[8] / (2.0 * inertia))
    return moduli


def tower_sections(data: dict, taper_subsections: int = 4) -> list[dict]:
    """Prismatic OrcaFlex tower sections (te/m, kN.m2, kN) from the station table.

    Spans whose two end stations carry equal values become one section.
    Station pairs closer than ``STEP_SPAN`` are wall-thickness steps; their
    length is added to the following section. A tapered span is split into
    ``taper_subsections`` sections, each with the mass per length at its
    mid-height (linear between stations, so the section masses sum to the
    piecewise-linear integral) and stiffnesses from tube formulas at its
    mid-height diameter and thickness, using the moduli implied by the span's
    end stations. Poisson's ratio follows from the same moduli as
    E / (2 G) - 1.
    """
    rows = _v(data["floating_tower"])
    moduli = tower_moduli(data)
    sections: list[dict] = []
    carry = 0.0
    for k in range(len(rows) - 1):
        a, b = rows[k], rows[k + 1]
        span = b[0] - a[0]
        if span < STEP_SPAN:
            carry += span
            continue
        if a[1:] == b[1:]:
            poisson = moduli["E_bending"][k] / (2.0 * moduli["G"][k]) - 1.0
            pieces = [(span, a[1], a[2], a[3], a[6], a[9], a[8])]
        else:
            young = (moduli["E_bending"][k] + moduli["E_bending"][k + 1]) / 2.0
            young_axial = (moduli["E_axial"][k] + moduli["E_axial"][k + 1]) / 2.0
            shear = (moduli["G"][k] + moduli["G"][k + 1]) / 2.0
            poisson = young / (2.0 * shear) - 1.0
            pieces = []
            for j in range(taper_subsections):
                f = (j + 0.5) / taper_subsections
                outer = a[1] + f * (b[1] - a[1])
                thickness = (a[2] + f * (b[2] - a[2])) / 1000.0
                area, inertia = _tube(outer, thickness)
                pieces.append((span / taper_subsections, outer, thickness * 1000.0,
                               a[3] + f * (b[3] - a[3]), young * inertia,
                               young_axial * area, shear * 2.0 * inertia))
        for n, (length, outer, _t, mass, ei, ea, gj) in enumerate(pieces):
            sections.append({
                "length": length + (carry if n == 0 else 0.0),
                "outer_diameter": outer,
                "mass_per_length": mass / 1000.0,
                "bending_stiffness": ei / 1000.0,
                "axial_stiffness": ea / 1000.0,
                "torsional_stiffness": gj / 1000.0,
                # Isotropic relation nu = E / (2 G) - 1 with the moduli the
                # station table implies (EI/I and GJ/J).
                "poisson_ratio": poisson,
            })
        carry = 0.0
    return sections


def rna_about_own_centre(basis: dict) -> dict:
    """RNA lumped mass in tower-top axes (te, m, te.m2).

    basis.yml adopts the published RNA tensor as taken about the tower top
    (``rna_inertia_cg.adopted_origin``); the inertia about the RNA's own
    centre of mass follows from the parallel-axis theorem.
    """
    system = basis["system"]
    mass = _v(system["rna_mass"])
    top = _v(system["tower_top"])
    r = [c - t for c, t in zip(_v(system["rna_center_of_gravity"]), [0.0, 0.0, top])]
    at_top = _v(system["rna_inertia_cg"])
    rr = sum(x * x for x in r)
    own = [[at_top[i][j] - mass * ((rr if i == j else 0.0) - r[i] * r[j]) for j in range(3)]
           for i in range(3)]
    return {"mass": mass / 1000.0, "centre_of_mass_from_tower_top": r,
            "inertia_about_centre_of_mass": [[x / 1000.0 for x in row] for row in own]}


def build_flexible_spec(basis: dict, data: dict, **kwargs) -> dict:
    """IEA 15 MW spec with the flexible representation.

    The hull carries the platform alone (ElastoDyn PtfmMass, PtfmCM and
    inertias, ballast included); the tower is a finite-element line from the
    floating-tower station table; the RNA is a lumped 6D buoy at the tower
    top. Hydrostatics, radiation, damping and mooring are those of the rigid
    spec.
    """
    kwargs.setdefault("name", "iea15_volturnus_s_flexible_lc0_statics")
    spec = build_spec(basis, data, **kwargs)
    system = basis["system"]
    fowt = spec["fowt"]
    fowt["representation"] = "flexible"
    hull = fowt["hull"]
    hull["mass"] = _v(system["platform_mass"]) / 1000.0
    hull["centre_of_mass"] = list(_v(system["platform_center_of_gravity"]))
    hull["inertia_about_centre_of_mass"] = [[x / 1000.0 for x in row]
                                            for row in _v(system["platform_inertia_cg"])]
    fowt["tower"] = {"base_elevation": _v(data["floating_tower"])[0][0],
                     "target_segment_length": 5.0, "sections": tower_sections(data)}
    fowt["rna"] = rna_about_own_centre(basis)
    fowt["source_note"] += (
        " Flexible representation: hull = platform (ElastoDyn PtfmMass, PtfmCM, PtfmRIner, "
        "PtfmPIner, PtfmYIner); tower from generator_data.yml floating_tower (tapered top span "
        "in four sections, tube formulas); RNA lumped at its centre of mass with the adopted "
        "tower-top tensor moved to that centre. The tensor's products of inertia are kept: "
        "a massless child buoy on its principal axes carries the moments. Tower Poisson "
        "ratio E/(2G)-1 from the moduli implied by the station table.")
    spec["metadata"]["description"] = spec["metadata"]["description"].replace(
        "rigid", "flexible")
    return spec
