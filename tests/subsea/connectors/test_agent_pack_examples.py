# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic mechanics examples; standards acceptance remains NOT_EVALUATED."""

import pytest

from digitalmodel.subsea.connectors import (
    ConnectorMaterial,
    ConnectorType,
    JumperType,
    PipeSection,
    assess_jumper_fatigue,
    bending_from_radius,
    end_cap_force,
    get_jumper_spec,
    thermal_expansion_load,
    verify_connector,
)


def test_end_cap_literal():
    """Independent pressure definition F=p*A; circle A=pi*(D/2)^2.

    Synthetic p=4 Pa, D=1 m: F=4*pi/4=pi N.
    API 17R/ISO 13628-1 edition and clause basis NOT_EVALUATED.
    """
    assert end_cap_force(4, 1) == pytest.approx(3.141592653589793)


def test_thermal_literals():
    """Independent restrained-bar algebra; engineering basis NOT_EVALUATED.

    Synthetic Do=2, t=0.5 m -> Di=1, A=3*pi/4 m2.
    alpha=0.001/K, dT=10 K -> strain=0.01. L=5 m -> growth=0.05 m.
    E=1000 Pa -> full stress=10 Pa; restraint=0.5 -> force=15*pi/4 N.
    No material default is used.
    """
    result = thermal_expansion_load(PipeSection(2, 0.5), 5, 10, 0.001, 1000, 0.5)
    assert result.thermal_strain == pytest.approx(0.01)
    assert result.free_growth == pytest.approx(0.05)
    assert result.restrained_stress == pytest.approx(10.0)
    assert result.axial_force == pytest.approx(11.780972450961723)


def test_bending_literals():
    """Independent annulus integration; model qualification NOT_EVALUATED.

    Synthetic Do=2, Di=1 m: I=pi*(16-1)/64=15*pi/64 m4.
    E=1000 Pa, R=10 m -> kappa=0.1/m, stress=100 Pa,
    M=100*15*pi/64=375*pi/16 N m. Allowable=200 Pa -> U=0.5.
    """
    result = bending_from_radius(PipeSection(2, 0.5), 10, 1000, 200)
    assert result.curvature == pytest.approx(0.1)
    assert result.bending_stress == pytest.approx(100.0)
    assert result.bending_moment == pytest.approx(73.63107781851078)
    assert result.utilisation == pytest.approx(0.5)
    assert result.passed


def test_body_and_seal_literals():
    """Independent uniaxial example; pressure and moment are zero.

    Synthetic Do=2, Di=1 -> A=3*pi/4; applied tension=3*pi/4 N
    (independent literal input) -> sigma_vm=1 Pa. SMYS=2 Pa, eta=1 -> U=0.5.
    Preload=3*pi/4+4 N; seating=2 N -> residual=4 N -> U_seal=0.5.
    Supplied eta is synthetic; standard acceptance NOT_EVALUATED.
    """
    result = verify_connector(
        PipeSection(2, 0.5), ConnectorMaterial("synthetic", 2, 1000), 0,
        applied_axial=2.356194490192345, preload=6.356194490192345,
        seal_seating_force=2, usage_factor=1, connector_type=ConnectorType.CLAMPED,
    )
    assert result.sigma_vm == pytest.approx(1.0)
    assert result.f_residual == pytest.approx(4.0)
    assert result.utilisation_body == pytest.approx(0.5)
    assert result.utilisation_seal == pytest.approx(0.5)
    assert result.passed


def test_fatigue_conversion_literals():
    """Independent signed elastic stress: E*Do/(2*R)*SCF / 10^6.

    Synthetic E=10^9 Pa, Do=0.2 m, R=(10,-10,10) m, SCF=2:
    stresses=(20,-20,20) MPa. SI prefix conversion is dimensional arithmetic.
    S-N coefficients, damage and design acceptance NOT_EVALUATED here.
    """
    result = assess_jumper_fatigue(
        get_jumper_spec(JumperType.FLEXIBLE), PipeSection(0.2, 0.02),
        [10, -10, 10], youngs_modulus_pa=1e9, block_years=1, design_years=1,
        stress_concentration_factor=2,
    )
    assert result.stress_history_mpa == pytest.approx([20.0, -20.0, 20.0])
