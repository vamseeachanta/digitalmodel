"""Synthetic arithmetic benchmarks for the hull wastage screening adapter."""
from copy import deepcopy

import pytest

from digitalmodel.hull_wastage_screen.screen import assess


def case():
    """Synthetic: renewal = 12 - 3 - 1 = 8 mm; reserve selected as 0.5 mm.

    Equation: CSR BC&OT, July 2025, Pt 1 Ch 13 Sec 2 [2.1.1, 2.1.3].
    Values are arbitrary test inputs, not published scantlings.
    """
    return {
        "rule_family": "csr",
        "limits": {"reserve_mm": 0.5, "source": "Synthetic selected limits"},
        "elements": [{"id": "plate", "as_built_mm": 12., "corrosion_mm": 3.,
                      "voluntary_mm": 1., "measured_mm": 9., "uncertainty_mm": 0.}],
    }


@pytest.mark.parametrize("measured,status", [
    (9., "ACCEPT"), (8.5, "ACCEPT"), (8.49, "MONITOR"),
    (8., "MONITOR"), (7.99, "RENEW"), (0., "RENEW"),
])
def test_reserve_boundaries(measured, status):
    """Hand comparison with renewal 8 and upper band 8.5 mm (case docstring)."""
    data = case()
    data["elements"][0]["measured_mm"] = measured
    result = assess(data)
    assert result["elements"][0]["status"] == status
    assert result["elements"][0]["renewal_mm"] == 8.
    # Missing global/buckling/pitting data must never produce overall ACCEPT.
    assert result["status"] == ("RENEW" if status == "RENEW" else "NOT_EVALUATED")


def test_uncertainty_and_no_mutation():
    """9 - 1.1 = 7.9 mm lower bound, below 8 mm renewal."""
    data = case()
    data["elements"][0]["uncertainty_mm"] = 1.1
    before = deepcopy(data)
    result = assess(data)
    assert result["status"] == "RENEW"
    assert result["decision"]["verdict"] == "REPAIR"
    assert data == before


@pytest.mark.parametrize("measured,status", [(9., "ACCEPT"), (8.99, "MONITOR"), (8., "MONITOR"), (7.9, "RENEW")])
def test_percentage_family(measured, status):
    """Hand calc: margin 12-8=4; selected fraction .75 gives boundary 9 mm.

    Strict excess follows IACS UR Z7 Rev.21 (2014), 1.2.8.
    """
    data = case()
    data["rule_family"] = "percentage"
    data["limits"]["substantial_fraction"] = .75
    data["elements"][0].update(renewal_mm=8., measured_mm=measured)
    assert assess(data)["elements"][0]["status"] == status


def section_case():
    """Three horizontal strips at z=0,1,2 m, breadth 1 m.

    Gross t=.012 m, measured t=.010 m: NA=1 m, A=.030 m2,
    I=2*.010 + 3*.010**3/12 m4; Zdeck=Zbottom=I/1.
    Chosen modulus factor .8: measured/gross > .8. NA strip at z=1
    has t=.010, gross .012 and corrosion .004: reference .010 for
    selected corrosion fraction .5. CSR 2025 Ch13 Sec2 [2.2.2].
    """
    data = case()
    data["elements"] = [
        {"id": key, "as_built_mm": 12., "corrosion_mm": 4.,
         "voluntary_mm": 0., "measured_mm": 10., "uncertainty_mm": 0.}
        for key in ("bottom", "deck", "middle")
    ]
    data["global"] = {
        "complete_section": True, "depth_m": 2., "modulus_fraction": .8,
        "na_corrosion_fraction": .5, "source": "Synthetic selected limits",
        "members": [
            {"element_id": key, "zone": zone, "orientation": "horizontal",
             "span_m": 1., "z_m": z, "gross_offered_mm": 12.}
            for key, zone, z in [("bottom", "bottom", 0.), ("deck", "deck", 2.),
                                 ("middle", "neutral_axis", 1.)]
        ],
    }
    return data


def test_section_hand_calculation():
    """Middle strip adds only its own inertia .010**3/12 about NA=1."""
    result = assess(section_case())["checks"]["global"]
    assert result["status"] == "ACCEPT"
    assert result["measured"]["neutral_axis_m"] == pytest.approx(1.)
    assert result["measured"]["inertia_m4"] == pytest.approx(.02 + 3 * .01**3 / 12)
    assert result["na_area_m2"] == pytest.approx(.01)
    assert result["na_required_m2"] == pytest.approx(.01)


def test_neutral_axis_failure_and_modulus_failure():
    """NA area .009 < .010; separately .010/.012 < selected .9 modulus ratio."""
    data = section_case()
    data["elements"][2]["measured_mm"] = 9.
    assert assess(data)["checks"]["global"]["status"] == "RENEW"
    data = section_case()
    data["global"]["modulus_fraction"] = .9
    assert assess(data)["checks"]["global"]["status"] == "RENEW"


@pytest.mark.parametrize("field,value", [
    ("measured_mm", float("nan")), ("uncertainty_mm", -1.),
    ("corrosion_mm", -1.), ("as_built_mm", 0.), ("voluntary_mm", 15.),
])
def test_invalid_measurements(field, value):
    data = case()
    data["elements"][0][field] = value
    with pytest.raises(ValueError):
        assess(data)


def test_missing_limits_and_empty_input():
    data = case()
    del data["limits"]["reserve_mm"]
    with pytest.raises(ValueError):
        assess(data)
    data = case()
    data["elements"] = []
    with pytest.raises(ValueError):
        assess(data)


def test_missing_zones_is_not_evaluated():
    data = section_case()
    data["global"]["members"] = data["global"]["members"][:2]
    assert assess(data)["checks"]["global"]["status"] == "NOT_EVALUATED"


def test_duplicate_ids_rejected():
    data = case()
    data["elements"] *= 2
    with pytest.raises(ValueError, match="unique"):
        assess(data)


def buckling_case():
    data = case()
    data["elements"][0]["measured_mm"] = 10.
    data["buckling"] = {
        "source": "Synthetic elastic plate benchmark; existing plate solver",
        "element_id": "plate", "length_mm": 2000., "width_mm": 1000.,
        "sigma_x_mpa": 10., "sigma_y_mpa": 0., "tau_mpa": 0., "gamma_m": 1.,
        "material": {"yield_strength": 300., "ultimate_strength": 400.,
                     "youngs_modulus": 200000., "poissons_ratio": .3, "density": 7800.},
    }
    return data


def test_thinned_plate_elastic_hand_calculation():
    """Independent simply-supported long plate: k=4, E=200000, nu=.3,
    t=10, b=1000: sigma_e=4*pi^2*200000/(12*(1-.3^2))*(10/1000)^2.
    This is below half the selected yield stress, so no inelastic correction.
    Applied compression 10 MPa, gamma=1: U=10/sigma_e; retention=(10/12)^2.
    Source: NACA TN 3781 (1957), equation (1) gives the stress expression;
    equation (20), half-wave length equal to width, gives k=4.
    Synthetic inputs and independent arithmetic, not standard qualification.
    """
    import math
    result = assess(buckling_case())["checks"]["buckling"]
    critical = 4 * math.pi**2 * 200000 / (12 * (1 - .3**2)) * (.01)**2
    assert result["status"] == "ACCEPT"
    assert result["utilization"] == pytest.approx(10 / critical)
    assert result["capacity_retained_fraction"] == pytest.approx((10 / 12)**2)


def test_transverse_load_not_silently_ignored():
    data = buckling_case()
    data["buckling"]["sigma_y_mpa"] = 1.
    assert assess(data)["checks"]["buckling"]["status"] == "NOT_EVALUATED"


def test_buckling_failure_requires_strength_review():
    data = buckling_case()
    data["buckling"]["sigma_x_mpa"] = 100.
    result = assess(data)
    assert result["checks"]["buckling"]["status"] == "RENEW"
    assert result["status"] == "RENEW"


def test_csr_gross_offered_definition_rejects_nonconservative_input():
    """CSR Pt1 Ch3 Sec2 [1.2.2]: gross offered = 12 - 1 = 11 mm."""
    data = section_case()
    for item, member in zip(data["elements"], data["global"]["members"]):
        item["voluntary_mm"] = 1.
        member["gross_offered_mm"] = 11.
    assert assess(data)["checks"]["global"]["status"] == "ACCEPT"
    data["global"]["members"][1]["gross_offered_mm"] = 10.5
    with pytest.raises(ValueError, match="gross offered.*as-built.*voluntary"):
        assess(data)


def test_csr_gross_offered_tolerance_uses_defined_value():
    """Roundoff within the absolute 0.000001 mm tolerance cannot change capacity."""
    data = section_case()
    expected = assess(data)["checks"]["global"]["gross"]
    for member in data["global"]["members"]:
        member["gross_offered_mm"] -= .0000005
    assert assess(data)["checks"]["global"]["gross"] == expected


def test_buckling_basis_describes_evaluated_model():
    result = assess(buckling_case())["checks"]["buckling"]
    assert "NACA TN 3781" in result["basis"]
    assert "equations (1), (20)" in result["basis"]
    assert "uncited" not in result["basis"]
    assert "shear" in result["limitation"]
    assert "inelastic" in result["limitation"]
    assert "API 579" not in result["basis"]
    assert "DNV-RP-C201" not in result["basis"]
    assert result["code_qualified"] is False


@pytest.mark.parametrize("compression", [100., 1000.])
def test_thinned_plate_inelastic_is_not_evaluated(compression):
    """NACA TN 3781 eqs (1)/(20): sigma_e > fy/2 at t=20, b=1000 mm.

    The inherited inelastic correction lacks a primary citation and benchmark.
    Neither a passing nor a failing utilization can establish a disposition.
    """
    import math
    data = buckling_case()
    data["elements"][0].update(as_built_mm=24., measured_mm=20.)
    data["buckling"]["sigma_x_mpa"] = compression
    result = assess(data)["checks"]["buckling"]
    elastic = 4 * math.pi**2 * 200000 / (12 * (1 - .3**2)) * (.02)**2
    assert elastic > 300 / 2
    assert result["status"] == "NOT_EVALUATED"
    assert "inelastic" in result["reason"]
    assert "utilization" not in result
    assert "capacity_retained_fraction" not in result


@pytest.mark.parametrize("compression,shear,nominal,measured", [
    (0., 200., 50., 50.),
    (10., 200., 50., 50.),
    (0., 20., 12., 10.),
    (10., 20., 12., 10.),
    (1000., 20., 12., 10.),
    (10., 1.e-12, 12., 10.),
])
def test_shear_cannot_establish_plate_disposition(compression, shear, nominal, measured):
    """Pure shear at 200 MPa gives sqrt(3)*200 > selected yield 300 MPa.

    The uncited shear capacity and interaction cannot govern any disposition,
    including for smaller loads, combined loads or a plate that would fail.
    """
    data = buckling_case()
    data["elements"][0].update(as_built_mm=nominal, measured_mm=measured)
    data["buckling"].update(sigma_x_mpa=compression, tau_mpa=shear)
    result = assess(data)
    plate = result["checks"]["buckling"]
    assert plate["status"] == "NOT_EVALUATED"
    assert "shear" in plate["reason"].lower()
    assert "utilization" not in plate
    assert "capacity_retained_fraction" not in plate
    assert result["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("compression", [10., 1000.])
def test_nominal_inelastic_capacity_cannot_govern_retention(compression):
    """Measured elastic stress is below fy/2 but nominal stress exceeds it."""
    data = buckling_case()
    data["elements"][0]["as_built_mm"] = 20.
    data["buckling"]["sigma_x_mpa"] = compression
    plate = assess(data)["checks"]["buckling"]
    assert plate["status"] == "NOT_EVALUATED"
    assert "inelastic" in plate["reason"]
    assert "capacity_retained_fraction" not in plate


@pytest.mark.parametrize("yield_factor,status", [
    (.999999, "NOT_EVALUATED"), (1., "ACCEPT"), (1.000001, "ACCEPT"),
])
def test_nominal_elastic_branch_boundary(yield_factor, status):
    """The inherited correction starts strictly above half the yield stress."""
    import math
    data = buckling_case()
    nominal_elastic = 4 * math.pi**2 * 200000 / (12 * (1 - .3**2)) * (.012)**2
    data["buckling"]["material"]["yield_strength"] = 2 * nominal_elastic * yield_factor
    assert assess(data)["checks"]["buckling"]["status"] == status


@pytest.mark.parametrize("field,value,label", [
    ("reserve_mm", .6, "reserve_mm"),
    ("modulus_fraction", .8, "modulus_fraction"),
    ("na_corrosion_fraction", .6, "na_corrosion_fraction"),
])
def test_csr_deviations_are_explicit(field, value, label):
    data = section_case()
    data["global"]["modulus_fraction"] = .9
    target = data["limits"] if field == "reserve_mm" else data["global"]
    target[field] = value
    deviations = assess(data)["csr_deviations"]
    assert len(deviations) == 1
    assert label in deviations[0]


def test_standard_csr_values_have_no_deviations():
    data = section_case()
    data["global"]["modulus_fraction"] = .9
    assert assess(data)["csr_deviations"] == []


@pytest.mark.parametrize("age,status", [
    (None, "NOT_ESTABLISHED"), (10., "NOT_APPLICABLE"), (10.1, "AGE_THRESHOLD_MET"),
])
def test_csr_global_age_applicability(age, status):
    data = section_case()
    if age is not None:
        data["global"]["age_years"] = age
    result = assess(data)["checks"]["global"]
    assert result["age_applicability"]["status"] == status
    assert "special surveys" in result["age_applicability"]["basis"]
    assert result["age_applicability"]["age_years"] == age


def test_swapped_deck_and_neutral_axis_zones_rejected():
    data = section_case()
    members = data["global"]["members"]
    members[1]["zone"], members[2]["zone"] = members[2]["zone"], members[1]["zone"]
    with pytest.raises(ValueError, match="deck zone.*0.9"):
        assess(data)


@pytest.mark.parametrize("z,zone,valid", [
    (1.8, "deck", True), (1.793, "deck", False),
    (1.8, "neutral_axis", False), (1.8, "bottom", False),
])
def test_deck_zone_line_boundary(z, zone, valid):
    data = section_case()
    data["global"]["members"][1].update(z_m=z, zone=zone)
    if valid:
        assess(data)
    else:
        with pytest.raises(ValueError, match="deck zone.*0.9"):
            assess(data)


def test_vertical_deck_member_crossing_line_is_allowed():
    data = section_case()
    data["global"]["members"][1].update(orientation="vertical", z_m=1.7, span_m=.6)
    assess(data)


def test_missing_limits_block_raises_value_error():
    data = case()
    del data["limits"]
    with pytest.raises(ValueError, match="limits"):
        assess(data)


def test_missing_material_block_raises_value_error():
    data = buckling_case()
    del data["buckling"]["material"]
    with pytest.raises(ValueError, match="material"):
        assess(data)


def test_incomplete_global_basis_remains_not_evaluated():
    data = case()
    data["global"] = {"complete_section": False}
    assert assess(data)["checks"]["global"]["status"] == "NOT_EVALUATED"


def test_negative_age_rejected():
    data = section_case()
    data["global"]["age_years"] = -1.
    with pytest.raises(ValueError, match="age_years"):
        assess(data)


def test_horizontal_deck_member_crossing_line_is_allowed():
    data = section_case()
    data["global"]["members"][1]["z_m"] = 1.799
    assess(data)
