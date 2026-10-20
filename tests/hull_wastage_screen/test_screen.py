"""Synthetic arithmetic benchmarks; no vessel or measured inspection data."""
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
    """Two lumped horizontal strips at z=0,2 m, breadth 1 m.

    Gross t=.012 m, measured t=.010 m: NA=1 m, A=.020 m2,
    I=2*(.010 + .010**3/12) m4; Zdeck=Zbottom=I/1.
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
    Source: NACA TN 3781 (1957), Boundary Conditions, equation (20),
    half-wave length equal to width gives k=4. Elastic plate rigidity gives
    the stress expression above. Existing solver uses DNV-RP-C201 basis;
    synthetic inputs and independent arithmetic, not standard qualification.
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
