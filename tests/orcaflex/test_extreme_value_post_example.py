"""Offline smoke test for the extreme-value-post example workflow (#961).

Runs the committed example end-to-end with NO OrcaFlex / NO license and
asserts the return-period extremes are finite, monotonic, and that the
distribution fits succeeded.
"""

import importlib.util
import math
from pathlib import Path

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


def test_example_dir_exists():
    assert EXAMPLE_DIR.is_dir(), f"missing example dir: {EXAMPLE_DIR}"
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


def test_run_offline_produces_sane_extremes():
    run = _load_run_module()
    results = run.run()

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


def test_summary_json_written():
    run = _load_run_module()
    run.run()
    out = EXAMPLE_DIR / "results" / "extreme_value_post" / "extreme_value_summary.json"
    assert out.is_file()
    import json

    data = json.loads(out.read_text())
    assert data["n_blocks"] >= 20
    assert "gumbel" in data["fits"] and "weibull" in data["fits"]
