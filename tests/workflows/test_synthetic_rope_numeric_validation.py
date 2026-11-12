# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Numeric-domain regressions for the synthetic-rope screening route."""

from copy import deepcopy
import csv
import math
from pathlib import Path
import re

import pytest

from digitalmodel.synthetic_rope_mooring_fatigue.workflow import router


MODULE = "synthetic_rope_mooring_fatigue"
MODEL_FIELDS = [
    ("tn_curve", "intercept"),
    ("tn_curve", "slope"),
    ("tn_curve", "mean_load_knockdown"),
    ("creep", "reference_life_years"),
    ("creep", "load_ratio_ref"),
    ("creep", "decades_per_load_ratio"),
]
NUMERIC_FIELDS = [
    ("design_life_years",),
    ("dff",),
    ("creep_safety_factor",),
    *MODEL_FIELDS,
    ("min_tension", "min_ratio_allow"),
    ("lines", 0, "MBL_kN"),
    ("lines", 0, "mean_tension_kN"),
    ("lines", 0, "min_tension_kN"),
    ("lines", 0, "temperature_C"),
    ("lines", 0, "tension_range_bins", 0, "tension_range_kN"),
    ("lines", 0, "tension_range_bins", 0, "n_cycles"),
    *(("lines", 0, *path) for path in MODEL_FIELDS),
]


@pytest.fixture
def cfg(tmp_path):
    """Supply invented arithmetic inputs; these are not design coefficients.

    Hand calculation using agent-pack.md section 3's implemented expressions:
    MBL=10, mean=5, minimum=2; intercept=8, slope=2, knockdown=0.
    Ranges 2 and 4 give R=1/5 and 2/5, hence N=200 and 50.
    Counts 4 and 1 give D=4/200+1/50=1/25; life=2/(1/25)=50 years.
    Fatigue margin=50/(2*2)=12.5; creep=20*10**((0.5-0.5)*2)=20
    years, creep margin=20/(2*2)=5; compression=(2/10)/(1/10)=2.
    These exact rational results test arithmetic, not model qualification.
    """
    return {
        MODULE: {
            "design_life_years": 2.0,
            "dff": 2.0,
            "creep_safety_factor": 2.0,
            "tn_curve": {
                "intercept": 8.0,
                "slope": 2.0,
                "mean_load_knockdown": 0.0,
            },
            "creep": {
                "reference_life_years": 20.0,
                "load_ratio_ref": 0.5,
                "decades_per_load_ratio": 2.0,
            },
            "min_tension": {"min_ratio_allow": 0.1},
            "output_dir": str(tmp_path / "results"),
            "lines": [{
                "id": "test-line",
                "material": "POLYESTER",
                "MBL_kN": 10.0,
                "mean_tension_kN": 5.0,
                "min_tension_kN": 2.0,
                "temperature_C": -5.0,
                "tension_range_bins": [
                    {"tension_range_kN": 2.0, "n_cycles": 4.0},
                    {"tension_range_kN": 4.0, "n_cycles": 1.0},
                ],
            }],
        }
    }


def _field_parent(cfg, path):
    settings = cfg[MODULE]
    # Exercise line-specific models as well as the global models.
    settings["lines"][0]["tn_curve"] = deepcopy(settings["tn_curve"])
    settings["lines"][0]["creep"] = deepcopy(settings["creep"])
    parent = settings
    for key in path[:-1]:
        parent = parent[key]
    return parent


@pytest.mark.parametrize("path", NUMERIC_FIELDS, ids=lambda p: ".".join(map(str, p)))
@pytest.mark.parametrize(
    "invalid",
    [math.nan, math.inf, -math.inf, True, False, "nan", "inf", "-inf"],
    ids=["nan", "inf", "negative-inf", "true", "false", "text-nan", "text-inf",
         "text-negative-inf"],
)
def test_rejects_nonfinite_and_boolean_inputs(cfg, path, invalid):
    """Every numeric field must reject non-real magnitudes before writing CSV.

    Source: Python 3.11 Library Reference (2022), math.isfinite: NaN and both
    infinities are non-finite. Booleans are excluded by the audit requirement.
    """
    _field_parent(cfg, path)[path[-1]] = invalid
    with pytest.raises(ValueError, match=re.escape(path[-1])):
        router(cfg)
    assert not list(Path(cfg[MODULE]["output_dir"]).glob("*.csv"))


@pytest.mark.parametrize(
    "invalid", ["not-numeric", [], {}, 10**400],
    ids=["text", "list", "mapping", "integer-overflow"],
)
@pytest.mark.parametrize("path", NUMERIC_FIELDS, ids=lambda p: ".".join(map(str, p)))
def test_conversion_errors_name_the_field(cfg, path, invalid):
    """Invalid scalar conversion must retain the field name in ValueError."""
    _field_parent(cfg, path)[path[-1]] = invalid
    with pytest.raises(ValueError, match=re.escape(path[-1])):
        router(cfg)


def test_finite_inputs_preserve_hand_calculated_results(cfg):
    """Use the fixture's independent rational calculation as the oracle."""
    result = router(cfg)
    summary = result[MODULE]["lines"][0]
    # Oracle: cfg's docstring. Relative tolerance: module docs/agent-pack.md,
    # section 5, G2 (2026-10-17); this is arithmetic QA, not a design limit.
    expected = {
        "total_damage": 1 / 25,
        "fatigue_life_years": 50.0,
        "fatigue_margin": 12.5,
        "creep_life_years": 20.0,
        "creep_margin": 5.0,
        "compression_margin": 2.0,
    }
    for key, value in expected.items():
        assert summary[key] == pytest.approx(value, rel=1e-12, abs=0.0)
    assert summary["temperature_C"] == -5.0
    assert summary["governing_mechanism"] == "compression"
    assert summary["passes"] is True
    assert result["screening_status"] == "pass"


@pytest.mark.parametrize("per_line", [False, True], ids=["global", "per-line"])
def test_nonzero_model_coefficients_match_independent_calculation(cfg, per_line):
    """Exercise both coefficients with invented, non-neutral arithmetic inputs.

    Knockdown ln(4) at mean ratio 1/2 gives exp(-ln(4)/2)=1/2, so
    the effective intercept is 4 and allowable counts are 100 and 25.
    Damage=4/100+1/25=2/25; life=2/(2/25)=25; fatigue margin=25/4.
    Creep reference ratio 1/4 and coefficient 4 give exponent -1,
    life=20/10=2 years and margin=2/4=1/2, governing a failed screen.
    Relative tolerance follows agent-pack.md section 5, G2.
    """
    settings = cfg[MODULE]
    models = {
        "tn_curve": {**settings["tn_curve"], "mean_load_knockdown": math.log(4)},
        "creep": {
            **settings["creep"],
            "load_ratio_ref": 0.25,
            "decades_per_load_ratio": 4.0,
        },
    }
    target = settings["lines"][0] if per_line else settings
    target.update(models)
    output_dir = Path(settings["output_dir"])
    result = router(cfg)
    summary = result[MODULE]["lines"][0]
    expected = {
        "total_damage": 2 / 25,
        "fatigue_life_years": 25.0,
        "fatigue_margin": 25 / 4,
        "creep_life_years": 2.0,
        "creep_margin": 1 / 2,
        "compression_margin": 2.0,
    }
    for key, value in expected.items():
        assert summary[key] == pytest.approx(value, rel=1e-12, abs=0.0)
    with (output_dir / f"{MODULE}_{MODULE}.csv").open() as stream:
        rows = list(csv.DictReader(stream))
    assert [float(row["allowable_cycles"]) for row in rows] == pytest.approx(
        [100.0, 25.0], rel=1e-12, abs=0.0,
    )
    assert summary["governing_mechanism"] == "creep"
    assert summary["passes"] is False
    assert result["screening_status"] == "fail"


@pytest.mark.parametrize("path", NUMERIC_FIELDS, ids=lambda p: ".".join(map(str, p)))
def test_all_numeric_fields_accept_finite_numeric_strings(cfg, path):
    """Scalar conversion accepts numeric strings for every numeric field."""
    parent = _field_parent(cfg, path)
    parent[path[-1]] = str(parent[path[-1]])
    summary = router(cfg)[MODULE]["lines"][0]
    assert summary["total_damage"] == pytest.approx(1 / 25, rel=1e-12, abs=0.0)
    assert summary["creep_margin"] == pytest.approx(5.0, rel=1e-12, abs=0.0)
    assert summary["compression_margin"] == pytest.approx(2.0, rel=1e-12, abs=0.0)


@pytest.mark.parametrize("temperature", [None, 0.0, "-5.0"])
def test_optional_temperature_accepts_finite_values(cfg, temperature):
    """Temperature metadata permits omission, zero and negative finite values."""
    cfg[MODULE]["lines"][0]["temperature_C"] = temperature
    result = router(cfg)[MODULE]["lines"][0]
    expected = None if temperature is None else float(temperature)
    assert result["temperature_C"] == expected


def test_accumulated_damage_overflow_is_rejected(cfg):
    """Finite bins can overflow their sum: N=1 and D=1e308+1e308=+inf.

    Inputs here are an arithmetic stress case, not a physical load spectrum.
    With intercept=1, range/MBL=1 and knockdown=0, each bin has N=1.
    Python 3.11 Library Reference (2022), math.isfinite, excludes infinite sums.
    """
    settings = cfg[MODULE]
    settings["tn_curve"]["intercept"] = 1.0
    settings["lines"][0]["tension_range_bins"] = [
        {"tension_range_kN": 10.0, "n_cycles": 1e308},
        {"tension_range_kN": 10.0, "n_cycles": 1e308},
    ]
    with pytest.raises(ValueError, match="test-line total_damage"):
        router(cfg)
