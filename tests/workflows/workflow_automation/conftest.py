"""Shared fixtures for workflow_automation tests that execute workflows."""

import sys
import types

import pytest


@pytest.fixture
def run_dir(tmp_path, monkeypatch):
    """Run in a scratch directory.

    The orchestrator caches task results under ./.workflow_cache and the HTML
    reporter writes to ./reports unless told otherwise.
    """
    monkeypatch.chdir(tmp_path)
    return tmp_path


@pytest.fixture
def analysis_modules(monkeypatch):
    """Register stand-in analysis modules under ``digitalmodel.modules``.

    The orchestrator imports each task module as
    ``digitalmodel.modules.<module>``. That package is not in this
    repository, so tests register modules under the same names. Keys are
    ``"<module>:<function>"``; every call is recorded as ``(key, kwargs)``.
    """
    calls = []

    def install(functions):
        modules = {}
        for key, func in functions.items():
            module_name, func_name = key.split(":")
            full_name = f"digitalmodel.modules.{module_name}"
            module = modules.setdefault(full_name, types.ModuleType(full_name))

            def recorded(_key=key, _func=func, **kwargs):
                calls.append((_key, kwargs))
                return _func(**kwargs)

            setattr(module, func_name, recorded)
        for full_name, module in modules.items():
            monkeypatch.setitem(sys.modules, full_name, module)
        return calls

    return install


RISER_MODULES = {
    "catenary_riser.simple_catenary:analyze_simple_catenary": lambda **kw: {
        "top_tension": 2.5e5,
        "effective_weight": 1.2e3,
    },
    "viv_analysis.frequency_calculator:calculate_natural_frequency": lambda **kw: {
        "natural_frequency": 0.25,
    },
    "viv_analysis.screening:screen_viv_susceptibility": lambda **kw: {
        "viv_status": "LOW",
        "reduced_velocity": kw["current_speed"]
        / (kw["natural_frequency"] * kw["diameter"]),
    },
    "fatigue_analysis:assess_fatigue_life": lambda **kw: {
        "fatigue_life": 45.6,
        "damage_ratio": 0.4,
    },
}

MOORING_MODULES = {
    "hydrodynamics.ocimf_loading:calculate_combined_loads": lambda **kw: {
        "horizontal_load": 1.5e6,
        "resultant_load": 1.8e6,
    },
    "mooring_analysis.catenary:analyze_catenary_mooring": lambda **kw: {
        "line_tension": kw["horizontal_load"] * 1.1,
        "anchor_load": 1.2e6,
    },
    "mooring_analysis.designer:verify_safety_factors": lambda **kw: {
        "safety_factor": 1.85,
        "status": "PASS",
    },
}

STRUCTURAL_MODULES = {
    "structural_analysis.stress:calculate_von_mises_stress": lambda **kw: {
        "von_mises_stress": 210.5,
        "utilization": 0.593,
    },
    "structural_analysis.buckling:check_plate_buckling": lambda **kw: {
        "buckling_capacity": 320.0,
        "buckling_status": "PASS",
    },
    "structural_analysis:verify_overall_capacity": lambda **kw: {
        "overall_status": "ADEQUATE",
        "safety_margin": 0.4,
    },
}


@pytest.fixture
def riser_modules(analysis_modules):
    """Stand-ins for the four riser-analysis task modules; returns the call log."""
    return analysis_modules(RISER_MODULES)


@pytest.fixture
def mooring_modules(analysis_modules):
    """Stand-ins for the three mooring-design task modules; returns the call log."""
    return analysis_modules(MOORING_MODULES)


@pytest.fixture
def structural_modules(analysis_modules):
    """Stand-ins for the three structural-check task modules; returns the call log."""
    return analysis_modules(STRUCTURAL_MODULES)
