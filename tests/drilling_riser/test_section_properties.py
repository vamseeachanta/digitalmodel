"""Oracle tests for drilling-riser section stiffness properties.

Oracle values reproduce a corpus drilling-riser section-stiffness spreadsheet
(generic tubular joints; no client/project identifiers). Values are checked
against the spreadsheet's computed cells at rtol ~1e-3.
"""

import math

import pytest

from digitalmodel.drilling_riser.section import (
    cross_sectional_area,
    moment_of_inertia,
    polar_moment_of_inertia,
    section_properties,
    section_properties_imperial,
    shear_modulus,
)

RTOL = 1e-3


def test_cross_sectional_area_matches_oracle():
    # OD=23.75 in, ID=18.75 in -> A=166.8971097 sq-in (spreadsheet F3)
    assert cross_sectional_area(23.75, 18.75) == pytest.approx(166.8971097, rel=RTOL)
    # OD=21 in, ID=19 in -> 62.83185307 sq-in (F5)
    assert cross_sectional_area(21.0, 19.0) == pytest.approx(62.83185307, rel=RTOL)


def test_moment_of_inertia_matches_oracle():
    # OD=23.75, ID=18.75 -> I=9550.947881 qu-in (H3)
    assert moment_of_inertia(23.75, 18.75) == pytest.approx(9550.947881, rel=RTOL)
    # OD=21, ID=19 -> 3149.446635 qu-in (H5)
    assert moment_of_inertia(21.0, 19.0) == pytest.approx(3149.446635, rel=RTOL)


def test_polar_moment_is_twice_inertia():
    # J = 2I (J3 = 19101.89576 qu-in)
    assert polar_moment_of_inertia(23.75, 18.75) == pytest.approx(
        19101.89576, rel=RTOL
    )
    assert polar_moment_of_inertia(23.75, 18.75) == pytest.approx(
        2.0 * moment_of_inertia(23.75, 18.75), rel=1e-12
    )


def test_shear_modulus_from_E_and_nu():
    # G = E / (2(1+nu)); E=30750 ksi, nu=0.293
    expected = 30750.0 / (2.0 * (1.0 + 0.293))
    assert shear_modulus(30750.0, 0.293) == pytest.approx(expected, rel=1e-12)


def test_imperial_stiffness_in_SI_matches_oracle():
    # Joint: OD=23.75 in, ID=18.75 in, E=30750 ksi, nu=0.293
    # Oracle (spreadsheet, meter/Pa columns):
    #   area  G3 = 0.1076753393 m^2
    #   I     I3 = 0.003975404652 m^4
    #   EA    P3 = 2.2828656428e10 N
    #   EI    R3 = 8.428405942e8 N.m^2
    #   GJ    T3 = 6.518488741e8 N.m^2
    props = section_properties_imperial(23.75, 18.75, 30750.0, 0.293)
    assert props.area == pytest.approx(0.1076753393, rel=RTOL)
    assert props.moment_of_inertia == pytest.approx(0.003975404652, rel=RTOL)
    assert props.axial_stiffness == pytest.approx(2.2828656428e10, rel=RTOL)
    assert props.bending_stiffness == pytest.approx(8.428405942e8, rel=RTOL)
    assert props.torsional_stiffness == pytest.approx(6.518488741e8, rel=RTOL)


def test_imperial_thinner_joint_oracle():
    # Buoyancy joint: OD=21 in, ID=19.25 in, E=30750 ksi, nu=0.293
    #   EA P8 = 7.567028175e9 ; EI R8 = 2.476251621e8 ; GJ T8 = 1.91512113e8
    props = section_properties_imperial(21.0, 19.25, 30750.0, 0.293)
    assert props.axial_stiffness == pytest.approx(7.567028175e9, rel=RTOL)
    assert props.bending_stiffness == pytest.approx(2.476251621e8, rel=RTOL)
    assert props.torsional_stiffness == pytest.approx(1.91512113e8, rel=RTOL)


def test_consistent_unit_path_agrees_with_helpers():
    od, idia, e, nu = 0.508, 0.4566, 2.0e11, 0.3
    p = section_properties(od, idia, e, nu)
    assert p.area == pytest.approx(cross_sectional_area(od, idia), rel=1e-12)
    assert p.axial_stiffness == pytest.approx(e * p.area, rel=1e-12)
    assert p.bending_stiffness == pytest.approx(e * p.moment_of_inertia, rel=1e-12)
    assert p.torsional_stiffness == pytest.approx(
        shear_modulus(e, nu) * p.polar_moment, rel=1e-12
    )
