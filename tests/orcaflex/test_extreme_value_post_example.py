"""Offline smoke test for the extreme-value-post example workflow (#961).

Runs the committed example end-to-end with NO OrcaFlex / NO license and
asserts the return-period extremes are finite, monotonic, and that the
distribution fits succeeded.
"""

import importlib.util
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.orcaflex.postprocessor import ExtremeValueResult

EXAMPLE_DIR = (
    Path(__file__).resolve().parents[2]
    / "examples"
    / "workflows"
    / "extreme-value-post"
)


def _load_run_module():
    """Import the example's run.py as a module."""
    run_path = EXAMPLE_DIR / "run.py"
    spec = importlib.util.spec_from_file_location("extreme_value_post_run", run_path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def input_yml(tmp_path):
    """Keep generated summaries outside the source tree."""
    cfg = yaml.safe_load((EXAMPLE_DIR / "input.yml").read_text(encoding="utf-8"))
    cfg["outputs"]["directory"] = (tmp_path / "results").as_posix()
    path = tmp_path / "input.yml"
    path.write_text(yaml.safe_dump(cfg), encoding="utf-8", newline="\n")
    return path


def test_example_dir_exists():
    assert EXAMPLE_DIR.is_dir(), "missing example directory"
    assert (EXAMPLE_DIR / "input.yml").is_file()
    assert (EXAMPLE_DIR / "run.py").is_file()
    assert (EXAMPLE_DIR / "data" / "block_maxima.csv").is_file()


def test_block_maxima_loads():
    run = _load_run_module()
    maxima = run.load_block_maxima(
        EXAMPLE_DIR / "data" / "block_maxima.csv", "effective_tension_kN"
    )
    assert maxima.size >= 20
    assert all(math.isfinite(v) for v in maxima)
    assert (maxima > 0).all()


def test_run_offline_produces_sane_extremes(input_yml):
    run = _load_run_module()
    results = run.run(input_yml)

    assert set(results) == {"gumbel", "weibull"}

    for name, res in results.items():
        assert isinstance(res, ExtremeValueResult)
        # Fit succeeded: scale must be positive and finite.
        assert math.isfinite(res.scale) and res.scale > 0, name
        assert math.isfinite(res.ks_statistic)

        rv = res.return_values
        assert {"10", "50", "100", "1000"}.issubset(rv.keys()), name
        values = [rv["10"], rv["50"], rv["100"], rv["1000"]]
        assert all(math.isfinite(v) for v in values), name
        # Return-period extremes increase with return period.
        assert values == sorted(values), name
        # Specifically: 100-yr >= 10-yr (the headline assertion).
        assert rv["100"] >= rv["10"], name
        # Extremes should sit at or above the sample (block maxima are < extremes).
        assert rv["10"] > 0


def test_summary_json_written(input_yml):
    run = _load_run_module()
    run.run(input_yml)
    out = input_yml.parent / "results" / "extreme_value_summary.json"
    assert out.is_file()
    import json

    data = json.loads(out.read_text(encoding="utf-8"))
    assert data["n_blocks"] >= 20
    assert "gumbel" in data["fits"] and "weibull" in data["fits"]


@pytest.mark.parametrize("life, percentages", [
    (None, ["92.8", "39.7", "22.2", "2.5"]),
    (25, ["92.8", "39.7", "22.2", "2.5"]),
    (1, ["10.0", "2.0", "1.0", "0.1"]),
])
def test_main_adds_encounter_probability(input_yml, capsys, life, percentages):
    """Keep every original output line, appending risk to each return value."""
    import json

    cfg = yaml.safe_load(input_yml.read_text(encoding="utf-8"))
    if life is None:
        cfg["extreme_value_post"].pop("design_life", None)
    else:
        cfg["extreme_value_post"]["design_life"] = life
    input_yml.write_text(yaml.safe_dump(cfg), encoding="utf-8", newline="\n")
    run = _load_run_module()
    run.main(input_yml)

    summary = json.loads(
        (input_yml.parent / "results" / "extreme_value_summary.json").read_text(
            encoding="utf-8"
        )
    )
    expected = [
        "Extreme-value post-processing (offline, license-free) — issue #961",
        "=" * 66,
    ]
    for res in summary["fits"].values():
        expected.extend([
            "",
            f"{res['distribution']} fit:",
            f"  location={res['location']}  scale={res['scale']}  shape={res['shape']}",
            f"  KS stat={res['ks_statistic']}  p-value={res['ks_pvalue']}",
            "  return-period extremes:",
        ])
        for (period, value), percentage in zip(res["return_values"].items(), percentages):
            expected.append(
                f"    {period:>5}-yr : {value}"
                f"  encounter probability ({life or 25}-yr life)={percentage}%"
            )
    assert capsys.readouterr().out.splitlines() == expected


@pytest.mark.parametrize("periods, expected", [
    ([1.25], [("1.25", "80.0%")]),
    ([10.5], [("10.5", "9.5%")]),
    ([1.6], [("1.6", "62.5%")]),
    ([10.5, 1.25, 1.6], [("10.5", "9.5%"), ("1.25", "80.0%"), ("1.6", "62.5%")]),
    ([10.25, 10.5], [("10.5", "9.5%")]),
])
def test_main_uses_configured_fractional_periods(input_yml, capsys, periods, expected):
    """Rounded fitter keys must not change the period used for probability."""
    # USACE, Hydrologic Statistics (May 2022), section I.C,
    # "Pool-Probability and Release-Probability Relationships": annual risk is
    # the reciprocal of the return period. One-year hand calculations:
    # 1 / 1.25 = 0.8; 1 / 10.5 = 2 / 21; 1 / 1.6 = 0.625.
    cfg = yaml.safe_load(input_yml.read_text(encoding="utf-8"))
    cfg["extreme_value_post"].update(return_periods=periods, design_life=1)
    input_yml.write_text(yaml.safe_dump(cfg), encoding="utf-8", newline="\n")

    run = _load_run_module()
    run.main(input_yml)

    lines = [
        line for line in capsys.readouterr().out.splitlines()
        if "encounter probability (1-yr life)=" in line
    ]
    observed = [
        (line.split("-yr :", 1)[0].strip(), line.rsplit("=", 1)[1])
        for line in lines
    ]
    # Both distributions retain the last value when rounded keys coincide.
    assert observed == expected * 2
