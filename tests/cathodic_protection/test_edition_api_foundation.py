"""Edition API foundation tests for B401 merge work."""

from __future__ import annotations

import inspect

import pytest


def test_default_edition_exports_from_package_facade():
    from digitalmodel.cathodic_protection import DEFAULT_EDITION, Edition

    assert DEFAULT_EDITION == "2021"
    assert "2017" in Edition.__args__
    assert "2021" in Edition.__args__


@pytest.mark.parametrize(
    ("raw", "expected"),
    [
        ("2017", "2017"),
        ("dnv-rp-b401-2017", "2017"),
        ("DNV_RP_B401_2021", "2021"),
        ("2021-05", "2021"),
    ],
)
def test_normalize_edition_accepts_supported_aliases(raw, expected):
    from digitalmodel.cathodic_protection import normalize_edition

    assert normalize_edition(raw) == expected


def test_normalize_edition_warns_and_defaults_when_missing():
    from digitalmodel.cathodic_protection import normalize_edition

    with pytest.warns(UserWarning, match="defaulting to DNV-RP-B401 2021"):
        assert normalize_edition(None) == "2021"


def test_normalize_edition_rejects_unknown_values():
    from digitalmodel.cathodic_protection import normalize_edition

    with pytest.raises(ValueError, match="Unsupported DNV-RP-B401 edition"):
        normalize_edition("2011")


@pytest.mark.parametrize(
    "function_name",
    [
        "current_demand",
        "anode_mass_requirement",
        "coating_breakdown_factor",
        "anode_resistance_slender_standoff",
        "anode_current_output",
        "equivalent_radius_from_mass",
        "number_of_anodes",
        "protected_length",
        "flush_anode_resistance",
    ],
)
def test_dnv_b401_public_functions_accept_edition_kwarg(function_name):
    from digitalmodel.cathodic_protection import dnv_rp_b401

    signature = inspect.signature(getattr(dnv_rp_b401, function_name))

    assert "edition" in signature.parameters


def test_dnv_b401_explicit_edition_preserves_current_demand_numeric():
    from digitalmodel.cathodic_protection.dnv_rp_b401 import current_demand

    assert current_demand(100.0, 0.05, 0.2, edition="2017") == pytest.approx(1.0)
    assert current_demand(100.0, 0.05, 0.2, edition="2021") == pytest.approx(1.0)


def test_dnv_b401_missing_edition_warns_and_preserves_numeric():
    from digitalmodel.cathodic_protection.dnv_rp_b401 import current_demand

    with pytest.warns(UserWarning, match="defaulting to DNV-RP-B401 2021"):
        assert current_demand(100.0, 0.05, 0.2) == pytest.approx(1.0)
