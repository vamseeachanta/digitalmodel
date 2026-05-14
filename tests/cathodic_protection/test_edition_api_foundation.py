"""Edition API foundation tests for B401 merge work."""

from __future__ import annotations

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
