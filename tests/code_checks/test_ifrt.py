"""Independent arithmetic for EPA AP-42 Ch. 7.1 (June 2026), section 7.1.3.2.

All geometry, inventory and operating data below are synthetic assumptions.
The selected method factors alone are from the cited public EPA publication.
"""

from dataclasses import replace
import json
import math

import pytest

from digitalmodel.citations.schema import CitationResolutionError, CitedValue
from digitalmodel.code_checks.storage_tanks.ifrt import (
    DeckFitting,
    IFRTCase,
    Liquid,
    NormalVentingBasis,
    RatedVent,
    Tank,
    check_normal_venting,
    compare_seals,
    estimate_losses,
    gasoline_clingage,
    public_fitting_factor,
    public_seal_factor,
)


@pytest.fixture
def case():
    return IFRTCase(
        tank=Tank(30.0, 15.0, (math.pi * 0.3,) * 6),
        liquid=Liquid(35.0, 101.325, 62.0, 740.0),
        rim_seal=public_seal_factor("vapor_mounted"),
        fittings=tuple(
            DeckFitting(name, count, public_fitting_factor(name))
            for name, count in (
                ("access_hatch_bolted_gasketed", 1),
                ("gauge_hatch_gasketed", 1),
                ("vacuum_breaker_gasketed", 1),
                ("unslotted_guidepole_gasketed_sleeve", 1),
                ("column_well_fabric_sleeve", 6),
                ("ifrt_leg_adjustable", 20),
            )
        ),
        deck_type="bolted",
        annual_net_throughput_m3=100_000.0,
        clingage=gasoline_clingage(),
    )


def test_30_m_gasoline_hand_calculation(case):
    # AP-42 Eqs 2-2/3/4/13/14/18/19, native units:
    # D=30/0.3048=98.42519685 ft; r=35/101.325=0.3454231434.
    # P*=r/(1+sqrt(1-r))^2=0.1055469903.
    # B=P* x 62 lb/lbmol x 0.45359237 kg/lb = 2.9682691878.
    # LR=6.7 D B=1957.4216101 kg/yr.
    # FF=1.6+.47+6.2+8.6+6(10)+20(7.9)=234.87 lbmol/yr.
    # LF=FF B=697.1573841 kg/yr.
    # LD=.14(.20)D^2 B=805.1446275 kg/yr (NO extra pi/4).
    # Q=100000/.158987294928=628981.0770 bbl/yr;
    # WL=740(.003785411784)/.45359237=6.1755992945 lb/US gal.
    # LW=.943 Q(.0015)WL/D x (1+6(.3)/30) x .45359237
    #   =26.84015097 kg/yr. Total=3486.5637727 kg/yr.
    result = estimate_losses(case)
    assert result.vapor_pressure_function == pytest.approx(0.105546990309)
    assert result.rim_kg_year == pytest.approx(1957.421610070)
    assert result.fittings_kg_year == pytest.approx(697.157384140)
    assert result.seams_kg_year == pytest.approx(805.144627482)
    assert result.withdrawal_kg_year == pytest.approx(26.840150971)
    assert result.standing_kg_year == pytest.approx(3459.723621693)
    assert result.total_kg_year == pytest.approx(3486.563772664)
    assert case.tank.volume_m3 == pytest.approx(10602.875205866)
    sidecar = json.loads(json.dumps(result.citation_sidecar()))
    assert sidecar["citations"][0]["revision"] == "June 2026"
    assert any(v["value"] == 0.943 for _, v in sidecar["values"])


def test_seal_and_deck_comparison_holds_other_inputs_fixed(case):
    results = compare_seals(
        case,
        {
            "vapor": public_seal_factor("vapor_mounted"),
            "liquid": public_seal_factor("liquid_mounted"),
            "liquid_secondary": public_seal_factor(
                "liquid_mounted", secondary="rim_mounted"
            ),
            "tight_vapor": public_seal_factor(
                "vapor_mounted", fit="tight", gaps_verified=True
            ),
        },
    )
    assert results["liquid"].total_kg_year == pytest.approx(1996.586129178)
    assert results["liquid_secondary"].total_kg_year == pytest.approx(1616.787906329)
    assert results["tight_vapor"].total_kg_year == pytest.approx(3165.196045638)
    assert all(
        r.fittings_kg_year == results["vapor"].fittings_kg_year
        for r in results.values()
    )
    welded = estimate_losses(replace(case, deck_type="welded"))
    assert welded.seams_kg_year == 0.0
    assert welded.total_kg_year == pytest.approx(2681.419145182)
    assert case.rim_seal.value == 6.7


def test_measured_seam_length_matches_density_default(case):
    # SD=.20 ft/ft2 = .20/.3048 m/m2; Lseam=SD*pi*D^2/4.
    length = 0.20 / 0.3048 * math.pi * 30**2 / 4
    actual = estimate_losses(replace(case, seam_length_m=length))
    assert actual.seams_kg_year == pytest.approx(805.144627482)
    assert estimate_losses(replace(case, seam_length_m=0.0)).seams_kg_year == 0


def test_column_clingage_uses_fixed_columns_not_support_legs(case):
    no_columns = estimate_losses(replace(case, tank=Tank(30, 15)))
    assert no_columns.withdrawal_kg_year == pytest.approx(26.840150971 / 1.06)
    no_fittings = estimate_losses(replace(case, fittings=()))
    assert no_fittings.withdrawal_kg_year == estimate_losses(case).withdrawal_kg_year


def test_zero_vapor_pressure_does_not_erase_liquid_clingage(case):
    result = estimate_losses(
        replace(case, liquid=replace(case.liquid, vapor_pressure_kpa=0))
    )
    assert result.standing_kg_year == 0
    assert result.withdrawal_kg_year > 0
    assert (
        estimate_losses(replace(case, annual_net_throughput_m3=0)).withdrawal_kg_year
        == 0
    )


def test_unit_conversion_equivalent_usc_geometry(case):
    from digitalmodel.units import Q_

    tank = Tank(
        Q_(98.4251968503937, "ft").to("m").magnitude, 15, case.tank.column_perimeters_m
    )
    density = Q_(6.1755992945, "pound/gallon").to("kg/m^3").magnitude
    result = estimate_losses(
        replace(case, tank=tank, liquid=replace(case.liquid, density_kg_m3=density))
    )
    assert result.total_kg_year == pytest.approx(3486.563772664, rel=1e-8)


@pytest.mark.parametrize(
    "flag", ["roof_afloat", "freely_vented", "stable_stock", "intact_seals"]
)
def test_out_of_scope_operation_rejected(case, flag):
    with pytest.raises(ValueError, match=flag):
        estimate_losses(replace(case, **{flag: False}))


@pytest.mark.parametrize("bad", [True, "30", 0, -1, math.nan, math.inf])
def test_invalid_geometry_rejected(case, bad):
    with pytest.raises(ValueError):
        estimate_losses(replace(case, tank=Tank(bad, 15)))


@pytest.mark.parametrize("bad", [-1, 101.325, 150, math.nan, True])
def test_invalid_absolute_vapor_pressure_rejected(case, bad):
    with pytest.raises(ValueError):
        estimate_losses(
            replace(case, liquid=replace(case.liquid, vapor_pressure_kpa=bad))
        )


@pytest.mark.parametrize("count", [-1, 0.5, True])
def test_invalid_fitting_counts_rejected(case, count):
    fitting = replace(case.fittings[0], count=count)
    with pytest.raises(ValueError, match="count"):
        estimate_losses(replace(case, fittings=(fitting,)))


def test_tight_fit_requires_attestation_and_unknown_tables_fail():
    with pytest.raises(ValueError, match="gaps_verified"):
        public_seal_factor("vapor_mounted", fit="tight")
    with pytest.raises(ValueError):
        public_seal_factor("unknown")
    with pytest.raises(ValueError):
        public_fitting_factor("unknown")


def test_citations_fail_closed_even_for_zero_count(case):
    broken = replace(
        case.rim_seal.citation, wiki_path="wikis/engineering-standards/missing.md"
    )
    with pytest.raises(CitationResolutionError):
        estimate_losses(replace(case, rim_seal=replace(case.rim_seal, citation=broken)))
    orphan = replace(
        case.fittings[0], count=0, factor=replace(case.rim_seal, citation=broken)
    )
    with pytest.raises(CitationResolutionError):
        estimate_losses(replace(case, fittings=(orphan,)))


def test_factor_units_and_negative_values_rejected(case):
    with pytest.raises(ValueError, match="units"):
        estimate_losses(replace(case, rim_seal=replace(case.rim_seal, units="kg/year")))
    with pytest.raises(ValueError):
        estimate_losses(replace(case, clingage=replace(case.clingage, value=-1)))
    with pytest.raises(ValueError, match="citation"):
        estimate_losses(
            replace(case, rim_seal=CitedValue(6.7, None, case.rim_seal.units))
        )


@pytest.fixture
def vent_inputs(tmp_path):
    # Synthetic design inputs; no API table values are asserted by this test.
    from digitalmodel.citations.schema import Citation

    source = tmp_path / "wikis/engineering-standards/vent-basis.md"
    source.parent.mkdir(parents=True)
    source.write_text(
        "---\ncode_id: TEST-VENT\npublisher: Test\nrevision: 1\n---\nSynthetic inputs.",
        encoding="utf-8",
    )
    citation = Citation(
        "TEST-VENT",
        "Test",
        "1",
        "synthetic example",
        "wikis/engineering-standards/vent-basis.md",
    )
    return dict(
        fill_m3_h=500.0,
        withdrawal_m3_h=750.0,
        basis=NormalVentingBasis(1.0, 2.0, 1000.0, 1000.0, citation),
        pressure_vent=RatedVent(2200.0, 1.5, 0.2, "Synthetic rated pressure curve"),
        vacuum_vent=RatedVent(2000.0, 0.3, 0.1, "Synthetic rated vacuum curve"),
        tank_pressure_limit_kpag=2.0,
        tank_vacuum_limit_kpa=0.5,
        wiki_root=tmp_path,
    )


def test_normal_vent_sum_and_rated_pressure_check(vent_inputs):
    # Qin=750*1+1000=1750; Qout=500*2+1000=2000 Nm3/h air.
    # Capacity margins 2000-1750=250 and 2200-2000=200.
    # Pressure demands 1.5+.2=1.7<2; vacuum magnitude .3+.1=.4<.5 kPa.
    result = check_normal_venting(**vent_inputs)
    assert result.required_in_nm3_h == 1750
    assert result.required_out_nm3_h == 2000
    assert result.in_margin_nm3_h == 250
    assert result.out_margin_nm3_h == 200
    assert result.capacity_sufficient
    assert result.pressure_limits_satisfied
    assert result.normal_check_passed
    assert result.status == "COMPARISON_ONLY"
    assert result.pressure_margin_kpa == pytest.approx(0.3)
    assert result.vacuum_margin_kpa == pytest.approx(0.1)
    assert result.pressure_vent.source == "Synthetic rated pressure curve"
    assert result.citation_sidecar()["citations"][0]["code_id"] == "TEST-VENT"


def test_normal_vent_shortfall_and_equality(vent_inputs):
    result = check_normal_venting(
        **{**vent_inputs, "pressure_vent": RatedVent(1999, 1.5, 0.2, "Test")}
    )
    assert result.out_margin_nm3_h == -1
    assert not result.normal_check_passed
    equal = check_normal_venting(
        **{**vent_inputs, "pressure_vent": RatedVent(2000, 1.8, 0.2, "Test")}
    )
    assert equal.normal_check_passed


@pytest.mark.parametrize(
    "key,vent",
    [
        ("pressure_vent", RatedVent(9999, 1.9, 0.2, "Test")),
        ("vacuum_vent", RatedVent(9999, 0.5, 0.1, "Test")),
    ],
)
def test_capacity_cannot_mask_pressure_or_vacuum_failure(vent_inputs, key, vent):
    result = check_normal_venting(**{**vent_inputs, key: vent})
    assert result.capacity_sufficient
    assert not result.pressure_limits_satisfied
    assert not result.normal_check_passed


def test_normal_vent_rejects_wrong_flow_reference_and_missing_evidence(vent_inputs):
    with pytest.raises(ValueError, match="flow_basis"):
        check_normal_venting(
            **{
                **vent_inputs,
                "basis": replace(vent_inputs["basis"], flow_basis="actual_m3_h"),
            }
        )
    with pytest.raises(ValueError, match="citation"):
        check_normal_venting(
            **{**vent_inputs, "basis": replace(vent_inputs["basis"], citation=None)}
        )
    with pytest.raises(ValueError):
        check_normal_venting(**{**vent_inputs, "fill_m3_h": math.inf})


def test_zero_normal_duty(vent_inputs):
    basis = replace(vent_inputs["basis"], thermal_in_nm3_h=0, thermal_out_nm3_h=0)
    result = check_normal_venting(
        **{**vent_inputs, "basis": basis, "fill_m3_h": 0, "withdrawal_m3_h": 0}
    )
    assert result.required_in_nm3_h == result.required_out_nm3_h == 0
    assert result.normal_check_passed


@pytest.mark.parametrize(
    "field", ["capacity_nm3_h", "relieving_differential_kpa", "connected_loss_kpa"]
)
def test_vent_negative_and_nonfinite_inputs(vent_inputs, field):
    for value in (-1, math.nan, math.inf, True):
        vent = replace(vent_inputs["pressure_vent"], **{field: value})
        with pytest.raises(ValueError):
            check_normal_venting(**{**vent_inputs, "pressure_vent": vent})


def test_vacuum_shortfall_independent_of_pressure_capacity(vent_inputs):
    vent = replace(vent_inputs["vacuum_vent"], capacity_nm3_h=1749)
    result = check_normal_venting(**{**vent_inputs, "vacuum_vent": vent})
    assert result.in_margin_nm3_h == -1
    assert not result.normal_check_passed


def test_missing_bundled_source_fails_closed(case, monkeypatch, tmp_path):
    import digitalmodel.code_checks.storage_tanks.ifrt as module

    # Simulate a wheel missing the bundled source record, without modifying it.
    monkeypatch.setattr(module, "__file__", str(tmp_path / "ifrt.py"))
    with pytest.raises(CitationResolutionError, match="page_missing"):
        estimate_losses(case)
