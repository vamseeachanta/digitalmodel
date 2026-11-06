# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""CPT checks; all input numbers are synthetic unless a source is stated.

G22 = Guide to Cone Penetration Testing, seventh edition (2022), pp. 18,
30-32, 41, 48-49 and 53. R09 = Interpretation of cone penetration tests —
a unified approach (2009), equations 5-7. See the documentation source links.
Numerical comparison tolerances below concern arithmetic, not design acceptance.
"""

from dataclasses import FrozenInstanceError, asdict, replace
from math import log10, sqrt
from pathlib import Path
import runpy

import pytest

from digitalmodel.geotechnical.cpt import (
    CPTData,
    DrainageBands,
    ProfileProvenance,
    SandConditions,
    assess_drainage,
    correct_cpt,
    correct_partial_drainage,
    ic_zone,
    normalise_cpt,
    sand_state,
    to_database_rows,
    undrained_strength,
)
from digitalmodel.geotechnical.database_schema import Source


def three_rows(**overrides):
    """Independent synthetic measurements; pressures explicitly in kPa."""
    inputs = dict(
        depth_m=[1, 2, 3],
        qc=[1000, 2000, 3000],
        fs=[10, 20, 30],
        u2=[100, 200, 300],
        area_ratio=0.8,
        pressure_unit="kPa",
    )
    inputs.update(overrides)
    return CPTData(**inputs)


def test_three_row_corrections():
    """G22 pp.18,31-32: qt=qc+0.2*u2 gives 1020,2040,3060 kPa.

    With total stress=20,40,60 and u0=10,20,30 kPa, qn=1000,2000,3000;
    Rf=100*10/1020=50/51 percent at each row; Bq=90/1000=0.09.
    """
    rows = correct_cpt(
        three_rows(),
        total_stress_pa=[20000, 40000, 60000],
        equilibrium_pore_pressure_pa=[10000, 20000, 30000],
    )
    assert [r.qt_pa for r in rows] == pytest.approx([1020000, 2040000, 3060000])
    assert [r.qn_pa for r in rows] == pytest.approx([1000000, 2000000, 3000000])
    assert [r.rf_percent for r in rows] == pytest.approx([50 / 51] * 3)
    assert [r.bq for r in rows] == pytest.approx([0.09] * 3)
    assert [r.effective_stress_pa for r in rows] == [10000, 20000, 30000]
    assert all(r.status == "EVALUATED" for r in rows)


@pytest.mark.parametrize("unit,factor", [("Pa", 1), ("kPa", 1000), ("MPa", 1000000)])
def test_explicit_pressure_units(unit, factor):
    """SI prefix definitions: kilo=1000, mega=1000000, independent of CPT."""
    data = three_rows(pressure_unit=unit)
    assert data.qc_pa == (1000 * factor, 2000 * factor, 3000 * factor)


def test_detached_immutable_inputs():
    depths = [1, 2, 3]
    data = three_rows(depth_m=depths)
    depths[0] = 99
    assert data.depth_m == (1, 2, 3)
    with pytest.raises(FrozenInstanceError):
        data.area_ratio = 0.7


@pytest.mark.parametrize(
    "overrides",
    [
        {"depth_m": []},
        {"depth_m": [1, 1, 3]},
        {"depth_m": [1, 3, 2]},
        {"depth_m": [-1, 2, 3]},
        {"depth_m": [1, float("nan"), 3]},
        {"qc": [1, 2]},
        {"fs": [1, float("inf"), 3]},
        {"fs": [-1, 2, 3]},
        {"qc": [-1, 2, 3]},
        {"u2": [1, float("nan"), 3]},
        {"u2": [1]},
        {"area_ratio": -0.1},
        {"area_ratio": 1.1},
        {"area_ratio": float("nan")},
        {"area_ratio": True},
        {"pressure_unit": "psi"},
        {"pressure_unit": "m"},
    ],
)
def test_invalid_measurements(overrides):
    with pytest.raises(ValueError):
        three_rows(**overrides)


def test_area_ratio_and_units_are_required():
    with pytest.raises(TypeError):
        CPTData(depth_m=[1], qc=[1], fs=[1], pressure_unit="Pa")
    with pytest.raises(TypeError):
        CPTData(depth_m=[1], qc=[1], fs=[1], area_ratio=0.8)


def test_missing_pore_pressure_is_explicit():
    rows = correct_cpt(
        three_rows(u2=[100, None, 300]),
        total_stress_pa=[20000, 40000, 60000],
        equilibrium_pore_pressure_pa=[10000, 20000, 30000],
    )
    assert rows[1].status == "NOT_EVALUATED"
    assert rows[1].qt_pa is rows[1].qn_pa is rows[1].bq is None
    assert "u2" in rows[1].reason
    assert rows[0].status == rows[2].status == "EVALUATED"


def test_negative_pore_pressure_is_valid():
    """G22 p.32 permits negative excess pore pressure: -100-10=-110 kPa.

    Synthetic qt=1000-100*0.2=980; qn=980-20=960; Bq=-110/960.
    """
    rows = correct_cpt(
        three_rows(u2=[-100, 200, 300]),
        total_stress_pa=[20000, 40000, 60000],
        equilibrium_pore_pressure_pa=[10000, 20000, 30000],
    )
    assert rows[0].bq == pytest.approx(-110 / 960)


@pytest.mark.parametrize(
    "stress,water",
    [
        ([1], [1, 2, 3]),
        ([1, 2, 3], [1]),
        ([1, -1, 3], [0, 0, 0]),
        ([1, 2, 3], [0, float("inf"), 0]),
    ],
)
def test_invalid_stresses(stress, water):
    with pytest.raises(ValueError):
        correct_cpt(
            three_rows(), total_stress_pa=stress, equilibrium_pore_pressure_pa=water
        )


def test_nonpositive_net_resistance_has_no_bq():
    """A zero denominator is not a pore-pressure ratio of zero."""
    rows = correct_cpt(
        three_rows(),
        total_stress_pa=[1020000, 3000000, 60000],
        equilibrium_pore_pressure_pa=[10000, 20000, 30000],
    )
    assert rows[0].qn_pa == 0
    assert rows[0].bq is rows[1].bq is None
    assert rows[0].status == rows[1].status == "NOT_EVALUATED"


def corrected_row(qn=1000000, fs=10000, effective=100000):
    """Synthetic qt=qn+200 kPa, u2=0 and area ratio=1 (no area correction)."""
    data = CPTData([1], [qn + 200000], [fs], u2=[0], area_ratio=1, pressure_unit="Pa")
    return correct_cpt(
        data,
        total_stress_pa=[200000],
        equilibrium_pore_pressure_pa=[200000 - effective],
    )[0]


def test_normalisation_at_reference_pressure():
    """R09 eqs.5-7: effective=pa=100 kPa makes Qtn=qn/pa=10 for any n.

    Fr=100*10/1000=1%; Ic=sqrt((3.47-1)^2+1.22^2);
    n=min(1,0.381*Ic+0.05-0.15). Expected values use this closed form.
    """
    (result,) = normalise_cpt([corrected_row()])
    expected_ic = sqrt(2.47**2 + 1.22**2)
    assert result.qtn == pytest.approx(10)
    assert result.fr_percent == pytest.approx(1)
    assert result.ic == pytest.approx(expected_ic)
    assert result.stress_exponent == pytest.approx(0.381 * expected_ic - 0.1)
    assert result.zone.startswith("4:")
    assert result.status == "EVALUATED"


def test_nontrivial_iteration_constructed_fixed_point():
    """Independent inverse of R09 eqs.5-7, not a production-code result.

    Choose Ic=2, effective/pa=4 => n=0.381*2+0.05*4-0.15=0.812.
    Choose log10 Qtn=2.47 => (3.47-log10 Qtn)^2=1.
    Then log10 Fr=sqrt(3)-1.22, qn=pa*10^2.47*4^0.812,
    fs=qn*10^(sqrt(3)-1.22)/100. This forces the stated fixed point.
    """
    qn = 100000 * 10**2.47 * 4**0.812
    fs = qn * 10 ** (sqrt(3) - 1.22) / 100
    (result,) = normalise_cpt([corrected_row(qn, fs, 400000)])
    assert result.qtn == pytest.approx(10**2.47)
    assert result.ic == pytest.approx(2)
    assert result.stress_exponent == pytest.approx(0.812)
    assert result.iterations > 1


def test_1990_option_and_exponent_cap():
    """R09 eq.6 n=1 gives Qt=1000/400=2.5; eq.7 caps clay n at 1."""
    (old,) = normalise_cpt([corrected_row(effective=400000)], method="1990")
    assert old.qtn == pytest.approx(2.5)
    assert old.stress_exponent == 1
    (clay,) = normalise_cpt([corrected_row(qn=100000, fs=10000)])
    assert clay.stress_exponent == 1


@pytest.mark.parametrize(
    "ic,zone",
    [
        (0, "7:"),
        (1.30, "7:"),
        (1.31, "6:"),
        (2.04, "6:"),
        (2.05, "5:"),
        (2.59, "5:"),
        (2.60, "4:"),
        (2.94, "4:"),
        (2.95, "3:"),
        (3.59, "3:"),
        (3.60, "2:"),
        (4, "2:"),
    ],
)
def test_sbt_boundaries(ic, zone):
    """R09 fig.2 p.1342: 1.31,2.05,2.60,2.95,3.60, read from the paper.

    Equality is assigned to the finer zone by an explicit software convention;
    zones 1,8,9 cannot be identified with Ic alone (G22 p.32).
    """
    assert ic_zone(ic).startswith(zone)


@pytest.mark.parametrize("ic", [-1, float("nan"), float("inf")])
def test_invalid_ic(ic):
    with pytest.raises(ValueError):
        ic_zone(ic)


@pytest.mark.parametrize(
    "change",
    [
        {"qn_pa": 0},
        {"qn_pa": -1},
        {"fs_pa": 0},
        {"effective_stress_pa": 0},
        {"effective_stress_pa": -1},
        {"qt_pa": None},
    ],
)
def test_normalisation_domain_is_not_evaluated(change):
    (result,) = normalise_cpt([replace(corrected_row(), **change)])
    assert result.status == "NOT_EVALUATED"
    assert result.ic is result.zone is result.qtn is None
    assert result.reason


def test_iteration_limit_is_not_a_successful_classification():
    (result,) = normalise_cpt(
        [corrected_row(qn=10000000, effective=400000)], max_iterations=1
    )
    assert result.status == "NOT_EVALUATED"
    assert "converge" in result.reason
    assert result.zone is None


@pytest.mark.parametrize(
    "kwargs",
    [
        {"method": "unknown"},
        {"atmospheric_pressure_pa": 0},
        {"iteration_tolerance": 0},
        {"max_iterations": 0},
        {"max_iterations": 1.5},
        {"max_iterations": True},
    ],
)
def test_invalid_normalisation_controls(kwargs):
    with pytest.raises(ValueError):
        normalise_cpt([corrected_row()], **kwargs)


def synthetic_bands(**overrides):
    """Toy band limits test the interface; these are NOT published soil limits."""
    inputs = dict(
        drained_max=0.2,
        undrained_min=5,
        consolidation_basis="vertical",
        citation="Synthetic calibration for software tests",
        locator="test inputs",
    )
    inputs.update(overrides)
    return DrainageBands(**inputs)


def drainage(row=None, **overrides):
    inputs = dict(
        penetration_rate_m_s=0.02,
        cone_diameter_m=0.05,
        consolidation_coefficient_m2_s=0.001,
        consolidation_basis="vertical",
        bands=synthetic_bands(),
    )
    inputs.update(overrides)
    return assess_drainage(corrected_row() if row is None else row, **inputs)


def test_velocity_and_user_supplied_regime_bands():
    """V=v*d/cv: .02*.05/.001=1. Toy bands are .2 and 5.

    They exercise control flow only; no geological regime limit is asserted.
    """
    result = drainage()
    assert result.normalised_velocity == pytest.approx(1)
    assert result.regime == "PARTIALLY_DRAINED"
    assert result.status == "EVALUATED"
    assert result.citation == synthetic_bands().citation
    assert result.locator == synthetic_bands().locator


@pytest.mark.parametrize(
    "velocity,regime",
    [
        (0, "DRAINED"),
        (0.1, "DRAINED"),
        (0.2, "DRAINED"),
        (1, "PARTIALLY_DRAINED"),
        (5, "UNDRAINED"),
        (10, "UNDRAINED"),
    ],
)
def test_supplied_band_boundaries(velocity, regime):
    """Synthetic limits as above; V=velocity when v=.02*velocity."""
    assert drainage(penetration_rate_m_s=0.02 * velocity).regime == regime


@pytest.mark.parametrize(
    "missing",
    [
        "penetration_rate_m_s",
        "cone_diameter_m",
        "consolidation_coefficient_m2_s",
        "consolidation_basis",
        "bands",
    ],
)
def test_missing_drainage_input_is_not_evaluated(missing):
    result = drainage(**{missing: None})
    assert result.status == result.regime == "NOT_EVALUATED"
    assert result.reason
    if missing == "bands":
        assert result.normalised_velocity == pytest.approx(1)


def test_missing_u2_never_silently_skips_drainage():
    data = three_rows(u2=None)
    rows = correct_cpt(
        data,
        total_stress_pa=[20000, 40000, 60000],
        equilibrium_pore_pressure_pa=[10000, 20000, 30000],
    )
    result = drainage(row=rows[0])
    assert result.status == result.regime == "NOT_EVALUATED"
    assert "u2" in result.reason


def test_horizontal_vertical_calibration_mismatch():
    result = drainage(consolidation_basis="horizontal")
    assert result.status == "NOT_EVALUATED"
    assert "basis" in result.reason


@pytest.mark.parametrize(
    "overrides",
    [
        {"penetration_rate_m_s": -1},
        {"cone_diameter_m": 0},
        {"consolidation_coefficient_m2_s": 0},
        {"cone_diameter_m": float("nan")},
        {"penetration_rate_m_s": float("inf")},
        {"consolidation_basis": "unknown"},
    ],
)
def test_invalid_drainage_inputs(overrides):
    with pytest.raises(ValueError):
        drainage(**overrides)


@pytest.mark.parametrize(
    "overrides",
    [
        {"drained_max": 0},
        {"undrained_min": 0.1},
        {"undrained_min": 0.2},
        {"drained_max": float("nan")},
        {"citation": ""},
        {"locator": " "},
        {"consolidation_basis": "unknown"},
    ],
)
def test_invalid_calibration_inputs(overrides):
    with pytest.raises(ValueError):
        synthetic_bands(**overrides)


@pytest.mark.parametrize("rate", [0, 0.02, 1000000])
def test_unread_backbone_is_not_an_invented_correction(rate):
    """No open fit was read: neither limiting velocities nor a flag supply one.

    Paper worked points, monotonicity and asymptote tests are deferred with
    the fit, rather than manufacturing expected engineering values.
    """
    result = correct_partial_drainage(1000000, drainage(penetration_rate_m_s=rate))
    assert result.measured_resistance_pa == 1000000
    assert result.corrected_resistance_pa is None
    assert result.status == "NOT_EVALUATED"
    assert "fit" in result.reason


def test_negative_resistance_cannot_be_corrected():
    with pytest.raises(ValueError):
        correct_partial_drainage(-1, drainage())


def test_undrained_strength_hand_calculation():
    """G22 p.41: synthetic qn=1000 kPa, specified Nkt=20 => Su=50 kPa.

    Nkt=20 deliberately exceeds the quoted typical range. That range is a
    message, never a hidden default or an acceptance criterion.
    """
    result = undrained_strength(corrected_row(), nkt=20, undrained=True)
    assert result.value == 50000
    assert result.unit == "Pa"
    assert result.status == "EVALUATED"
    assert "20" in result.method
    assert "10 to 18" in result.reason


def test_strength_requires_factor_and_undrained_basis():
    with pytest.raises(TypeError):
        undrained_strength(corrected_row(), undrained=True)
    assert undrained_strength(corrected_row(), nkt=20, undrained=False).value is None
    assert (
        undrained_strength(
            replace(corrected_row(), qn_pa=None), nkt=20, undrained=True
        ).status
        == "NOT_EVALUATED"
    )


@pytest.mark.parametrize("factor", [0, -1, float("nan"), float("inf"), True])
def test_invalid_cone_factor(factor):
    with pytest.raises(ValueError):
        undrained_strength(corrected_row(), nkt=factor, undrained=True)


def sand_conditions(**overrides):
    inputs = dict(
        clean=True,
        quartz=True,
        uncemented=True,
        unaged=True,
        rounded=True,
        drained=True,
        medium_compressibility=True,
    )
    inputs.update(overrides)
    return SandConditions(**inputs)


def sand_point(qtn=87.5, ic=1.5):
    (result,) = normalise_cpt([corrected_row()])
    return replace(result, qtn=qtn, ic=ic)


def test_sand_state_hand_calculation():
    """G22 p.49,53: synthetic Qtn=87.5, Ic=1.5 and eligible clean sand.

    Dr=sqrt(87.5/350)=0.5; phi=17.6+11*log10(87.5) degrees.
    The expected values come directly from printed equations, not this module.
    """
    state = sand_state(sand_point(), conditions=sand_conditions())
    assert state.relative_density.value == pytest.approx(0.5)
    assert state.relative_density.unit == "-"
    assert state.friction_angle.value == pytest.approx(17.6 + 11 * log10(87.5))
    assert state.friction_angle.unit == "degrees"
    assert state.relative_density.status == state.friction_angle.status == "EVALUATED"


@pytest.mark.parametrize(
    "condition",
    [
        "clean",
        "quartz",
        "uncemented",
        "unaged",
        "rounded",
        "drained",
        "medium_compressibility",
    ],
)
def test_sand_material_validity_is_required(condition):
    state = sand_state(sand_point(), conditions=sand_conditions(**{condition: False}))
    assert (
        state.relative_density.status == state.friction_angle.status == "NOT_EVALUATED"
    )


def test_sand_density_validity_and_physical_bounds():
    """G22 p.49 restricts the simple Dr equation to Ic<1.6.

    Qtn=350 => Dr=1 by arithmetic; Qtn=1400 => Dr=2, physically inadmissible.
    The latter is reported unavailable, never clipped to a believable value.
    """
    assert (
        sand_state(
            sand_point(qtn=350), conditions=sand_conditions()
        ).relative_density.value
        == 1
    )
    assert (
        sand_state(
            sand_point(qtn=1400), conditions=sand_conditions()
        ).relative_density.value
        is None
    )
    state = sand_state(sand_point(ic=1.6), conditions=sand_conditions())
    assert state.relative_density.value is None
    assert "Ic" in state.relative_density.reason


def test_unavailable_normalisation_blocks_sand_correlations():
    point = replace(sand_point(), status="NOT_EVALUATED", qtn=None, ic=None)
    state = sand_state(point, conditions=sand_conditions())
    assert state.relative_density.value is state.friction_angle.value is None


def test_conditions_are_booleans_not_truthy_strings():
    with pytest.raises(ValueError):
        sand_conditions(drained="yes")
    with pytest.raises(ValueError):
        undrained_strength(corrected_row(), nkt=20, undrained="yes")


def synthetic_provenance(**overrides):
    inputs = dict(
        row_prefix="synthetic-cpt",
        soil_unit="synthetic unit",
        site_or_region="synthetic",
        source=Source(
            "synthetic",
            "Synthetic measurements for software tests",
            "test fixture",
            "synthetic inputs",
            "yes",
            "read",
        ),
        locator="three-row synthetic profile",
        licence="BY",
        confidence_grade="C",
    )
    inputs.update(overrides)
    return ProfileProvenance(**inputs)


def test_database_round_trip_preserves_values_units_and_provenance(tmp_path):
    """G22 hand calculation from slice 1 survives the existing schema and YAML.

    qt=1020 kPa and Bq=.09 at row zero, as derived above. Database SI access
    must recover 1020000 Pa; provenance and the applied area ratio must survive.
    """
    import yaml
    from digitalmodel.geotechnical.database import load_database

    corrected = correct_cpt(
        three_rows(),
        total_stress_pa=[20000, 40000, 60000],
        equilibrium_pore_pressure_pa=[10000, 20000, 30000],
    )
    points = normalise_cpt(corrected)
    checks = [drainage(row=r) for r in corrected]
    provenance = synthetic_provenance()
    rows = to_database_rows(points, drainage=checks, provenance=provenance)
    payload = dict(
        meta={"basis": "synthetic"},
        sources=[asdict(provenance.source)],
        parameters=[
            {k: v for k, v in asdict(r).items() if k != "source"} for r in rows
        ],
        gaps=[],
        excluded=[],
    )
    path = tmp_path / "profile.yaml"
    path.write_text(yaml.safe_dump(payload), encoding="utf-8")
    db = load_database(path)
    assert len(db.rows) == len(rows)
    assert len(db.rows) == len(set(r.row_id for r in rows))
    for row in rows:
        assert db.rows[row.row_id] == row
        assert row.source_id == "synthetic" and row.locator == provenance.locator
        assert row.licence == "BY" and row.confidence_grade == "C"
        assert "point observation" in row.depth_range
    qt = db.lookup("parameter", "Corrected cone resistance qt")[0]
    assert db.quantities_for(qt.row_id)[0].in_si().value == pytest.approx(1020000)
    bq = db.lookup("parameter", "Pore-pressure ratio Bq")[0]
    assert float(bq.value_or_range) == pytest.approx(0.09)
    assert "2022" in qt.test_or_derivation_method
    assert "0.8" in db.lookup("parameter", "Cone area ratio a")[0].value_or_range
    assert (
        db.lookup("parameter", "Drainage regime")[0].value_or_range
        == "PARTIALLY_DRAINED"
    )
    assert (
        float(db.lookup("parameter", "Drained velocity boundary")[0].value_or_range)
        == 0.2
    )
    assert db.lookup("parameter", "Consolidation basis")[0].value_or_range == "vertical"


def test_database_missing_pressure_preserves_not_evaluated():
    from digitalmodel.geotechnical.database import load_database

    corrected = correct_cpt(
        three_rows(u2=None),
        total_stress_pa=[20000, 40000, 60000],
        equilibrium_pore_pressure_pa=[10000, 20000, 30000],
    )
    rows = to_database_rows(
        normalise_cpt(corrected),
        drainage=[drainage(row=r) for r in corrected],
        provenance=synthetic_provenance(),
    )
    missing = [r for r in rows if r.parameter == "Pore pressure u2"]
    assert all(r.value_or_range == "NOT_EVALUATED" for r in missing)
    flags = [r for r in rows if r.parameter == "Drainage regime"]
    assert all(r.value_or_range == "NOT_EVALUATED" and "u2" in r.note for r in flags)
    payload = dict(
        meta={},
        sources=[asdict(rows[0].source)],
        parameters=[
            {k: v for k, v in asdict(r).items() if k != "source"} for r in rows
        ],
        gaps=[],
        excluded=[],
    )
    db = load_database(payload)
    with pytest.raises(ValueError, match="numeric access unavailable"):
        db.quantities_for(missing[0].row_id)[0].in_printed_units()


def test_database_exports_optional_correlations():
    point = sand_point()
    state = sand_state(point, conditions=sand_conditions())
    strength = undrained_strength(point.corrected, nkt=20, undrained=False)
    rows = to_database_rows(
        [point],
        drainage=[drainage(row=point.corrected)],
        provenance=synthetic_provenance(),
        strengths=[strength],
        sand_states=[state],
    )
    by_name = {r.parameter: r for r in rows}
    assert float(by_name["Relative density Dr"].value_or_range) == pytest.approx(0.5)
    assert by_name["Undrained strength Su"].value_or_range == "NOT_EVALUATED"
    assert "undrained basis" in by_name["Undrained strength Su"].note


@pytest.mark.parametrize(
    "overrides",
    [
        {"row_prefix": ""},
        {"locator": ""},
        {"licence": "invalid"},
        {"confidence_grade": "invalid"},
        {"soil_unit": ""},
        {"source": Source("id", "citation", "access", "terms", "yes", "unread")},
    ],
)
def test_database_requires_provenance(overrides):
    with pytest.raises(ValueError):
        synthetic_provenance(**overrides)


def test_database_rejects_misaligned_observations():
    point = sand_point()
    with pytest.raises(ValueError, match="length"):
        to_database_rows([point], drainage=[], provenance=synthetic_provenance())
    with pytest.raises(ValueError, match="observation"):
        to_database_rows(
            [point],
            drainage=[drainage(row=corrected_row(qn=2000000))],
            provenance=synthetic_provenance(),
        )
    with pytest.raises(ValueError, match="length"):
        to_database_rows(
            [point],
            drainage=[drainage()],
            strengths=[],
            provenance=synthetic_provenance(),
        )


def example_module():
    path = (
        Path(__file__).resolve().parents[2]
        / "examples/workflows/cpt-interpretation/example.py"
    )
    return runpy.run_path(str(path))


def test_twenty_metre_worked_profile():
    """Independent first-row calculation for the synthetic example.

    z=1 m: qc=68, fs=3, u2=50, sigma_v=18, u0=10 kPa; a=.8.
    qt=78, qn=60, Rf=100*3/78, Bq=40/60, Su=60/15=4 kPa.
    For clay qn/effective=60/8=7.5, Fr=5%, n caps at 1 and
    Ic=sqrt((3.47-log10(7.5))^2+(log10(5)+1.22)^2), zone 3.
    At z=20 in sand: qc=14000, u2=200 => qt=14040, qn=13680 kPa.
    V_clay=.02*.04/1e-7=8000; V_sand=.02*.04/.01=.08.
    """
    example = example_module()["build_example"]()
    assert example["data"].depth_m == tuple(range(1, 21))
    first = example["corrected"][0]
    assert first.qt_pa == pytest.approx(78000)
    assert first.qn_pa == pytest.approx(60000)
    assert first.rf_percent == pytest.approx(100 * 3 / 78)
    assert first.bq == pytest.approx(2 / 3)
    assert example["strengths"][0].value == pytest.approx(4000)
    assert example["normalised"][0].ic == pytest.approx(
        sqrt((3.47 - log10(7.5)) ** 2 + (log10(5) + 1.22) ** 2)
    )
    assert all(p.zone.startswith("3:") for p in example["normalised"][:10])
    assert all(p.zone.startswith("6:") for p in example["normalised"][10:])
    assert example["corrected"][-1].qt_pa == pytest.approx(14040000)
    assert example["corrected"][-1].qn_pa == pytest.approx(13680000)
    assert example["drainage"][0].normalised_velocity == pytest.approx(8000)
    assert example["drainage"][-1].normalised_velocity == pytest.approx(0.08)
    assert all(d.regime == "NOT_EVALUATED" for d in example["drainage"])
    assert len(example["database_rows"]) > 20


def test_example_prints_inputs_methods_and_unavailable_flags():
    module = example_module()
    rendered = module["render_example"](module["build_example"]())
    assert "Inputs" in rendered and "Method" in rendered and "Answer" in rendered
    assert "NOT_EVALUATED" in rendered
    assert "conditional" in rendered
    assert "20.0" in rendered
    assert "nkt=15.0" in rendered


def test_example_drainage_calibration_is_explicit():
    """Toy bands still require a citation; they are never example defaults."""
    example = example_module()["build_example"](bands=synthetic_bands())
    assert all(d.regime == "UNDRAINED" for d in example["drainage"][:10])
    assert all(d.regime == "DRAINED" for d in example["drainage"][10:])


def test_package_exports_the_interpretation_api():
    from digitalmodel import geotechnical

    assert geotechnical.CPTData is CPTData
    assert geotechnical.normalise_cpt is normalise_cpt
    assert geotechnical.to_database_rows is to_database_rows
    assert "CPTData" in geotechnical.__all__
