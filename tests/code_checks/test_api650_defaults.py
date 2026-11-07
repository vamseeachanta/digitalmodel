# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Values independently read from the public incorporated API 650 copy.

API 650, Welded Tanks for Oil Storage, eleventh edition (June 2007),
Addenda 1 (November 2008), 2 (November 2009), 3 (August 2011),
errata October 2011. Source: https://law.resource.org/pub/us/cfr/ibr/002/api.650.2007.pdf
Table 5-2a (SI), p. 5-13; 4.2.2, p. 4-2; 5.6.1.1, p. 5-12;
5.6.3.1 and 5.6.3.2, p. 5-15; 1.1.1, p. 1-1.
The PDF was inspected directly; fixtures are not a secondary-source table.
"""

from dataclasses import replace

import pytest

from digitalmodel.code_checks.storage_tanks import (
    MaterialAllowables,
    OneFootBasis,
    TankDesign,
    api650_11th_basis,
    api650_11th_material,
    design_shell,
)


@pytest.mark.parametrize("grade,quoted_cells,maximum,clause", [
    # Table 5-2a cells, read directly: specification/grade, yield, tensile, Sd, St.
    # All stress columns are MPa; caps are from 4.2.2(a), (c), (e), respectively.
    ("A36M", "A36M 250 400 160 171", 40, "4.2.2(a)"),
    ("A283M C", "A 283M C 205 380 137 154", 25, "4.2.2(c)"),
    ("A516M 485", "A516M 485 260 485 173 195", 40, "4.2.2(e)"),
])
def test_each_si_material_row_and_plate_limit(grade, quoted_cells, maximum, clause):
    material = api650_11th_material(grade, design_temperature_c=93)
    design, test = map(float, quoted_cells.split()[-2:])
    assert (material.design_mpa, material.test_mpa) == (design, test)
    assert material.maximum_nominal_mm == maximum
    assert "Table 5-2a" in material.source
    assert clause in material.source
    assert "1.1.1" in material.source
    assert "Welded Tanks for Oil Storage" in material.source
    assert "June 2007" in material.source
    assert "November 2008" in material.source
    assert "November 2009" in material.source
    assert "August 2011" in material.source
    assert "October 2011" in material.source
    assert grade in material.source


@pytest.mark.parametrize("diameter,floor,bottom", [
    # 5.6.1.1 SI rows and Note 4: the bottom exception has strict endpoints.
    (3.2, 5, 5), (3.200001, 5, 6), (14.999999, 5, 6),
    (15, 6, 6), (35.999999, 6, 6), (36, 8, 8),
    (60, 8, 8), (60.000001, 10, 10), (61, 10, 10),
])
def test_nominal_floor_rows_and_boundaries(diameter, floor, bottom):
    basis = api650_11th_basis(diameter)
    assert basis.minimum_nominal_mm == floor
    assert basis.nominal_floor_mm(1) == bottom
    assert basis.nominal_floor_mm(2) == floor
    assert basis.nominal_floor_diameter_m == diameter
    assert "5.6.1.1" in basis.source
    assert "Note 4" in basis.source


def test_method_limit_and_si_equation_coefficients():
    # 5.6.3.1: "larger than 61 m"; 5.6.3.2 SI equations: 4.9D(H-0.3).
    basis = api650_11th_basis(61)
    assert basis.diameter_limit_m == 61
    assert basis.coefficient == 4.9
    assert basis.head_offset_m == 0.3
    assert "5.6.3.1" in basis.source and "5.6.3.2" in basis.source
    with pytest.raises(ValueError, match="5.6.4"):
        api650_11th_basis(61.000001)


@pytest.mark.parametrize("diameter", [0, -1, True, float("nan"), float("inf")])
def test_invalid_diameter_rejected(diameter):
    with pytest.raises(ValueError):
        api650_11th_basis(diameter)


@pytest.mark.parametrize("temperature", [93.000001, True, float("inf"), float("nan")])
def test_hot_or_unknown_temperature_does_not_get_ambient_allowables(temperature):
    with pytest.raises(ValueError, match="temperature"):
        api650_11th_material("A36M", design_temperature_c=temperature)


@pytest.mark.parametrize("grade", ["A36", "A516 70", "unknown"])
def test_us_customary_and_unsupported_grades_are_not_silently_converted(grade):
    with pytest.raises(ValueError, match="grade"):
        api650_11th_material(grade, design_temperature_c=20)


def default_tank(diameter=30):
    return TankDesign(
        diameter_m=diameter, course_heights_m=(2.25,) * 8,
        design_level_m=18, test_level_m=18, specific_gravity=1,
        corrosion_allowance_mm=1.5, density_kg_m3=7800,
        plate_sizes_mm=(5, 6, 8, 10, 12, 14, 16, 18, 20),
        material=api650_11th_material("A36M", design_temperature_c=20),
        basis=api650_11th_basis(diameter),
    )


def test_default_values_in_independent_one_foot_hand_calculation():
    # 5.6.3.2 with Table 5-2a A36M; geometry matches the existing example.
    # Bottom: 4.9*30*17.7/160+1.5=17.761875; test=4.9*30*17.7/171.
    # Upper: 4.9*30*4.2/160+1.5=5.35875; the 6 mm nominal floor governs.
    result = design_shell(default_tank())
    bottom, upper = result.courses[0], result.courses[6]
    assert bottom.design_mm == pytest.approx(17.761875)
    assert bottom.test_mm == pytest.approx(15.21578947368421)
    assert bottom.selected_mm == 18
    assert upper.design_mm == pytest.approx(5.35875)
    assert upper.required_mm == 6
    assert result.status == "NOT_EVALUATED"
    assert result.inputs.material.citation is None
    assert any("qualified" in gap for gap in result.not_evaluated)


def test_bottom_exception_is_applied_only_to_lowest_course_and_report():
    from digitalmodel.code_checks.storage_tanks.report import _basis, _courses, _thickness

    tank = replace(default_tank(10), design_level_m=0, test_level_m=0)
    result = design_shell(tank)
    assert [c.selected_mm for c in result.courses] == [6] + [5] * 7
    table = _courses(result)
    floor_index = table.columns.index("Floor [mm]")
    assert [row[floor_index] for row in table.rows] == ["6.000"] + ["5.000"] * 7
    trace = next(t for t in _thickness(result).plotly["data"] if t["name"] == "Nominal floor")
    assert trace["y"] == [6, 6] + [5] * 14
    basis_rows = _basis(result, 9.81, "assumed").blocks[0].rows
    assert any(row[:2] == ["Lowest-course nominal floor", "6.000"] for row in basis_rows)


def test_diameter_dependent_floor_cannot_be_reused_for_a_different_tank():
    with pytest.raises(ValueError, match="diameter"):
        design_shell(replace(default_tank(), diameter_m=36))


def test_selected_plate_limit_is_checked_but_unused_catalogue_sizes_are_allowed():
    tank = replace(default_tank(), plate_sizes_mm=(18, 20, 45))
    assert design_shell(tank).courses[0].selected_mm == 18
    with pytest.raises(ValueError, match="material.*thickness"):
        design_shell(replace(tank, plate_sizes_mm=(45,)))


@pytest.mark.parametrize("grade,maximum", [
    ("A36M", 40), ("A283M C", 25), ("A516M 485", 40),
])
def test_each_material_plate_limit_is_inclusive(grade, maximum):
    material = api650_11th_material(grade, design_temperature_c=20)
    tank = replace(default_tank(), material=material, plate_sizes_mm=(maximum,))
    assert design_shell(tank).courses[0].selected_mm == maximum
    with pytest.raises(ValueError, match="material.*thickness"):
        design_shell(replace(tank, plate_sizes_mm=(maximum + 0.001,)))


def test_explicit_caller_route_keeps_existing_hand_calculation():
    tank = replace(
        default_tank(), material=MaterialAllowables("Synthetic", 150, 160, "assumed"),
        basis=OneFootBasis(4.9, 0.3, 60, 6, "assumed"),
    )
    assert design_shell(tank).courses[0].design_mm == pytest.approx(18.846)
