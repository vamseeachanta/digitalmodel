# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Edition-explicit SI defaults read directly from an incorporated standard.

Source: https://law.resource.org/pub/us/cfr/ibr/002/api.650.2007.pdf
The cover includes all three addenda and the October 2011 errata; the filename
alone does not identify the revision. These helpers select that historical
edition, never a current-edition basis. Only three SI material rows are included.
No wiki citation or complete tank qualification is inferred from a table lookup.
"""

from dataclasses import dataclass
import math
from types import MappingProxyType

from digitalmodel.reporting.thickness import validate_number

from .one_foot import MaterialAllowables, OneFootBasis


_STANDARD = (
    "API 650, Welded Tanks for Oil Storage, eleventh edition (June 2007), "
    "Addendum 1 (November 2008), Addendum 2 (November 2009), "
    "Addendum 3 (August 2011), errata October 2011"
)
_SOURCE = "https://law.resource.org/pub/us/cfr/ibr/002/api.650.2007.pdf"

# Table 5-2a (SI), p. 5-13: Sd, St in MPa. The final entry is the shell-plate
# thickness cap in mm from 4.2.2(a), (c), (e), p. 4-2, respectively.
# SI specification names are intentional: do not convert USC Table 5-2b rows.
_MATERIALS = MappingProxyType({
    "A36M": (160, 171, 40, "4.2.2(a)"),
    "A283M C": (137, 154, 25, "4.2.2(c)"),
    "A516M 485": (173, 195, 40, "4.2.2(e)"),
})


@dataclass(frozen=True, kw_only=True)
class _Material(MaterialAllowables):
    maximum_nominal_mm: float
    design_temperature_c: float

    def validate_selected_plate(self, thickness_mm: float) -> None:
        """Enforce the cited cap on selected plates, not unused stock sizes."""
        validate_number("material maximum nominal thickness", self.maximum_nominal_mm)
        if thickness_mm > self.maximum_nominal_mm:
            raise ValueError("Selected plate exceeds material thickness limit; see 4.2.2")


@dataclass(frozen=True, kw_only=True)
class _Basis(OneFootBasis):
    minimum_bottom_nominal_mm: float
    nominal_floor_diameter_m: float

    def nominal_floor_mm(self, course: int) -> float | None:
        """Apply 5.6.1.1 Note 4 to the lowest shell course only."""
        if course == 1:
            return max(self.minimum_bottom_nominal_mm, self.minimum_nominal_mm or 0)
        return self.minimum_nominal_mm

    def validate_diameter(self, diameter_m: float) -> None:
        """Prevent an old floor band being reused after tank geometry changes."""
        if diameter_m != self.nominal_floor_diameter_m:
            raise ValueError("Default nominal floor must be selected for this diameter")
        validate_number("bottom nominal floor", self.minimum_bottom_nominal_mm)


def api650_11th_material(
    grade: str, *, design_temperature_c: float,
) -> MaterialAllowables:
    """Select one of three SI Table 5-2a rows for non-refrigerated service.

    The caller supplies the maximum design temperature. Section 1.1.1, p. 1-1,
    limits this basis to at most 93 degrees Celsius; Appendix M is not evaluated.
    Low-temperature toughness, material certification, purchaser requirements
    and all other service eligibility remain separate engineering checks.
    Unsupported specifications fail rather than borrowing another grade's row.
    """
    if (
        isinstance(design_temperature_c, bool)
        or not isinstance(design_temperature_c, (float, int))
        or not math.isfinite(design_temperature_c)
        or design_temperature_c > 93
    ):
        raise ValueError("design temperature must be finite and at most 93 C; see 1.1.1")
    if grade not in _MATERIALS:
        raise ValueError("Unsupported SI material grade for this edition")
    design, test, maximum, clause = _MATERIALS[grade]
    return _Material(
        name=grade, design_mpa=design, test_mpa=test,
        source=(
            f"{_STANDARD}; Table 5-2a (SI), p. 5-13, {grade}; "
            f"{clause}, p. 4-2; 1.1.1, p. 1-1; {_SOURCE}"
        ),
        maximum_nominal_mm=maximum, design_temperature_c=design_temperature_c,
    )


def api650_11th_basis(diameter_m: float) -> OneFootBasis:
    """Select SI method inputs and diameter-specific shell floors.

    Section 5.6.1.1, p. 5-12: diameter bands below 15, below 36, through 60,
    and above 60 m have floors 5, 6, 8 and 10 mm respectively. Note 4 raises
    the lowest course to 6 mm when 3.2 < diameter < 15 m. Section 5.6.3.1,
    p. 5-15, allows the method through 61 m; 5.6.3.2 supplies 4.9 and 0.3 m.
    All values are nominal SI quantities from the edition named in ``source``.
    """
    validate_number("diameter", diameter_m)
    if diameter_m > 61:
        raise ValueError("Diameter exceeds one-foot limit; use API 650 5.6.4")
    if diameter_m < 15:
        floor = 5
    elif diameter_m < 36:
        floor = 6
    elif diameter_m <= 60:
        floor = 8
    else:
        floor = 10
    bottom = 6 if 3.2 < diameter_m < 15 else floor
    return _Basis(
        coefficient=4.9, head_offset_m=0.3, diameter_limit_m=61,
        minimum_nominal_mm=floor,
        source=(
            f"{_STANDARD}; 5.6.3.1 and 5.6.3.2, p. 5-15; "
            f"5.6.1.1 including Note 4, p. 5-12; {_SOURCE}"
        ),
        minimum_bottom_nominal_mm=bottom, nominal_floor_diameter_m=diameter_m,
    )
