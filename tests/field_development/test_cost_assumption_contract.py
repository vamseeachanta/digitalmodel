# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Contracts for unsourced screening costs and unscaled reference ranges."""

import builtins
import importlib
import subprocess
import sys

import pytest

from digitalmodel.field_development import capex_estimator, opex_estimator
from digitalmodel.field_development.concept_selection import HostType
from digitalmodel.field_development.economics import (
    EconomicsInput,
    FiscalRegime,
    _resolve_capex_adapter,
)


DISCLOSURE = "screening assumption, not sourced; "
REFERENCE_CAPEX = [
    (HostType.TLP, (2.0, 4.0, 6.0)),
    (HostType.SPAR, (3.0, 5.0, 7.0)),
    (HostType.SEMI, (4.0, 7.0, 10.0)),
    (HostType.FPSO, (5.0, 8.5, 12.0)),
    (HostType.SUBSEA_TIEBACK, (0.2, 0.45, 1.2)),
    (HostType.DRY_TREE_UNIT, (3.5, 6.0, 8.0)),
]
REFERENCE_OPEX = [
    (HostType.TLP, (80.0, 160.0, 260.0)),
    (HostType.SPAR, (90.0, 175.0, 290.0)),
    (HostType.SEMI, (110.0, 210.0, 360.0)),
    (HostType.FPSO, (140.0, 270.0, 480.0)),
    (HostType.SUBSEA_TIEBACK, (20.0, 50.0, 90.0)),
    (HostType.DRY_TREE_UNIT, (95.0, 185.0, 300.0)),
]


@pytest.mark.parametrize("host,amounts", REFERENCE_CAPEX)
def test_unscaled_reference_capex_range(host, amounts):
    """Reference endpoints include the subsea range without distance scaling."""
    result = capex_estimator.reference_capex_range(host)
    assert isinstance(result, tuple)
    assert result == (amounts[0], amounts[2])


def test_selection_uses_estimator_reference_range(monkeypatch):
    """Selection reads the accessor at call time, preserving the shared enum."""
    selection = importlib.import_module(
        "digitalmodel.field_development.concept_selection"
    )
    seen = []

    def reference_range(host):
        assert isinstance(host, HostType)
        seen.append(host)
        return (1.1, 2.2)

    monkeypatch.setattr(capex_estimator, "reference_capex_range", reference_range)
    result = selection.concept_selection(900, 200, 50, "oil")
    assert seen == list(selection.LEGACY_HOST_TYPES)
    assert all(o.capex_estimate_usd_bn == (1.1, 2.2) for o in result.ranked_options)
    assert not hasattr(selection, "_CAPEX_RANGES")


@pytest.mark.parametrize(
    "host,amounts", [row for row in REFERENCE_CAPEX if row[0] is not HostType.SUBSEA_TIEBACK]
)
def test_host_capex_discloses_assumptions_without_changing_amounts(host, amounts):
    estimate = capex_estimator.estimate_capex(host, 100_000, 1500)
    assert (estimate.low_usd_bn, estimate.base_usd_bn, estimate.high_usd_bn) == amounts
    assert estimate.basis.startswith(DISCLOSURE)
    assert "2024 basis" in estimate.basis
    assert "not escalated" in estimate.basis
    assert "unsourced" in estimate.basis.split("Analogues:")[0]


@pytest.mark.parametrize(
    "distance,amounts",
    [(5.0, (0.1, 0.35, 0.5)), (15.0, (0.3, 0.6, 0.85)), (30.0, (0.62, 1.05, 1.48))],
)
def test_tieback_capex_discloses_assumptions_in_each_distance_band(distance, amounts):
    estimate = capex_estimator.estimate_capex(
        HostType.SUBSEA_TIEBACK, 100_000, 1500, tieback_distance_km=distance
    )
    assert (estimate.low_usd_bn, estimate.base_usd_bn, estimate.high_usd_bn) == amounts
    assert estimate.basis.startswith(DISCLOSURE)
    assert "2024 basis" in estimate.basis
    assert "not escalated" in estimate.basis


@pytest.mark.parametrize("host,amounts", REFERENCE_OPEX)
def test_opex_discloses_assumptions_without_changing_amounts(host, amounts):
    estimate = opex_estimator.estimate_opex(host, 100_000, 5)
    assert (
        estimate.low_usd_mm_per_yr,
        estimate.base_usd_mm_per_yr,
        estimate.high_usd_mm_per_yr,
    ) == amounts
    assert estimate.basis.startswith(DISCLOSURE)
    assert "2024 basis" in estimate.basis
    assert "not escalated" in estimate.basis
    assert "Sources:" not in estimate.basis
    assert "assumed $15-50/bbl" in estimate.basis


def test_ageing_rate_is_held_and_documented_as_an_assumption():
    assert opex_estimator._age_factor(6) == pytest.approx(1.025)
    assert "2.5% per year after year 5" in opex_estimator.__doc__
    assert "screening assumption, not sourced" in opex_estimator.__doc__
    estimate = opex_estimator.estimate_opex(HostType.TLP, 100_000, 6)
    assert estimate.base_usd_mm_per_yr == 164.0


def test_economics_fallback_preserves_disclosure_in_truncated_source(monkeypatch):
    """An absent optional cost dependency retains the disclosure and USD units."""
    real_import = builtins.__import__
    blocked = []

    def without_optional_cost(name, *args, **kwargs):
        if name == "worldenergydata.cost" or name.startswith("worldenergydata.cost."):
            blocked.append(name)
            raise ModuleNotFoundError("optional cost dependency unavailable")
        return real_import(name, *args, **kwargs)

    inp = EconomicsInput(
        field_name="synthetic",
        water_depth_m=1500,
        host_type="TLP",
        production_capacity_bopd=100_000,
        oil_price_usd_per_bbl=70,
        discount_rate=0.1,
        fiscal_regime=FiscalRegime.US,
        field_life_years=25,
    )
    with monkeypatch.context() as isolated:
        isolated.setattr(builtins, "__import__", without_optional_cost)
        amount_mm, source = _resolve_capex_adapter()(inp)
    assert builtins.__import__ is real_import
    assert blocked == ["worldenergydata.cost"]
    assert amount_mm == 4000.0
    assert DISCLOSURE in source


@pytest.mark.parametrize("first", ["capex_estimator", "concept_selection"])
def test_import_order_preserves_host_enum_and_reference_range(first):
    """Exercise each import order in a fresh interpreter."""
    code = f"""
import importlib
importlib.import_module('digitalmodel.field_development.{first}')
from digitalmodel.field_development.capex_estimator import reference_capex_range, HostType
from digitalmodel.field_development.concept_selection import HostType as SelectionHost
assert HostType is SelectionHost
assert reference_capex_range(HostType.SUBSEA_TIEBACK) == (0.2, 1.2)
"""
    result = subprocess.run(
        [sys.executable, "-c", code], capture_output=True, text=True, timeout=60, check=False
    )
    assert result.returncode == 0, result.stderr
