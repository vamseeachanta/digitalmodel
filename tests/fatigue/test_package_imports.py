"""Smoke import tests for digitalmodel.fatigue — guards against missing-dep regressions.

Filed under #2441: pylife dependency was missing from pyproject.toml for 60+ runs
(2026-04-05 onward), causing 10 test-collection ModuleNotFoundError failures. These
tests fail fast and clearly if pylife (or any other fatigue-package dep) is dropped
again, surfacing a single targeted error rather than a pytest collection abort.

Imports live INSIDE each test so pytest reports four targeted failures rather than
one collection-time crash.
"""
from __future__ import annotations


def test_fatigue_package_imports():
    import digitalmodel.fatigue
    assert digitalmodel.fatigue is not None


def test_fatigue_exports_get_sn_curve():
    from digitalmodel.fatigue import get_sn_curve
    assert get_sn_curve is not None
    assert callable(get_sn_curve)


def test_fatigue_exports_dnv_curves():
    from digitalmodel.fatigue import DNV_CURVES
    assert DNV_CURVES is not None
    assert len(DNV_CURVES) > 0


def test_pylife_woehlercurve_importable():
    import pandas as pd
    from pylife.materiallaws.woehlercurve import WoehlerCurve

    params = pd.Series({"k_1": 3.0, "SD": 52.63, "ND": 1e7, "k_2": 5.0})
    curve = WoehlerCurve(params)
    assert curve is not None
