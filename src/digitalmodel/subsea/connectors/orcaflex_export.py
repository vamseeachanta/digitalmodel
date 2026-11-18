# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Map supplied dynamic-jumper structural properties to generic line-type data.

The dictionary follows the General-category conversion pattern in
``marine_ops.installation.jumper_to_modular_spec`` and is consumed by the
existing ``GenericLineType`` schema and ``GenericModelBuilder``. The
installation converter's geometry and engineering defaults are not reused.
No solver binding is imported or queried.
"""

from __future__ import annotations

import math
from numbers import Real
from typing import Any

from .jumper_catalog import JumperSpec
from .section_properties import PipeSection

# BIPM, SI Brochure, ninth edition (2019), Table 7: kilo = 10^3;
# Table 8: tonne = 10^3 kg. The generic schema uses kN, m and tonnes.
# https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf
_N_PER_KN = 1e3
_KG_PER_TONNE = 1e3


def _quantity(value: float, name: str, *, positive: bool = False) -> float:
    """Check mathematical input domains, not code acceptance criteria."""
    if (
        isinstance(value, bool)
        or not isinstance(value, Real)
        or not math.isfinite(value)
        or value < 0
        or (positive and value == 0)
    ):
        domain = "positive" if positive else "nonnegative"
        raise ValueError(f"{name} must be finite and {domain}")
    return float(value)


def build_jumper_line_type(
    spec: JumperSpec,
    section: PipeSection,
    *,
    name: str,
    mass_per_length_kg_m: float,
    axial_stiffness_n: float,
    bending_stiffness_nm2: tuple[float, float],
    torsional_stiffness_nm2: float,
) -> dict[str, Any]:
    """Build a structural fragment for ``generic.line_types``.

    Args:
        spec: Catalogue entry with its dynamic flag enabled.
        section: Equivalent outer/bore diameters in metres. This geometry
            does not determine effective stiffness or layer stresses.
        name: Nonempty line-type identifier supplied by the caller.
        mass_per_length_kg_m: Positive dry structural mass per length in kg/m.
            Contents mass belongs to the line definition and is excluded.
        axial_stiffness_n: Positive effective axial stiffness EA in N.
        bending_stiffness_nm2: Nonnegative effective EI in N.m2 for each of
            the local x and y bending directions, in that order.
        torsional_stiffness_nm2: Nonnegative effective torsional GJ in N.m2.

    Returns:
        Generic line-type data in metres, tonnes/m, kN and kN.m2. Conversion
        uses the SI factors cited above, with no derived material properties
        or standard-derived coefficients. Effective properties for flexible
        construction must be supplied from a qualified engineering basis.

        This is not a complete model. Hydrodynamics, pressure response,
        damping, contents, line geometry, connections, environment and
        simulation settings still require explicit model assembly and review.
        No API 17R/17J/17B design acceptance is evaluated by this mapping.

    Raises:
        ValueError: If the entry is static, the name is empty, the geometry
            is nonfinite, or a structural property has an invalid domain.
    """
    if not spec.dynamic:
        raise ValueError("line-type export requires a dynamic catalogue entry")
    if not isinstance(name, str) or not name.strip():
        raise ValueError("name must be a nonempty string")
    if not all(math.isfinite(value) for value in (
        section.outer_diameter, section.wall_thickness,
    )):
        raise ValueError("section dimensions must be finite")
    if not isinstance(bending_stiffness_nm2, (tuple, list)) or len(
        bending_stiffness_nm2
    ) != 2:
        raise ValueError("bending_stiffness_nm2 requires local x and y values")

    mass = _quantity(mass_per_length_kg_m, "mass_per_length_kg_m", positive=True)
    ea = _quantity(axial_stiffness_n, "axial_stiffness_n", positive=True)
    ei = [_quantity(value, "bending_stiffness_nm2")
          for value in bending_stiffness_nm2]
    gj = _quantity(torsional_stiffness_nm2, "torsional_stiffness_nm2")

    return {
        "name": name,
        "category": "General",
        "outer_diameter": section.outer_diameter,
        "inner_diameter": section.inner_diameter,
        "mass_per_length": mass / _KG_PER_TONNE,
        "axial_stiffness": ea / _N_PER_KN,
        "bending_stiffness": [value / _N_PER_KN for value in ei],
        "properties": {"GJ": gj / _N_PER_KN},
    }
