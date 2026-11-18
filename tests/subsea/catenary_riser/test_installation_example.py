# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check the synthetic comparison against a scaled hand worksheet."""

import importlib.util
import math
from pathlib import Path

import pytest


def example():
    path = Path(__file__).resolve().parents[3] / "examples/subsea/catenary_installation_screen.py"
    spec = importlib.util.spec_from_file_location("catenary_example", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_scaled_1000_m_example_and_rounding():
    """lambda=1000/(12-2sqrt13), H=300lambda, Vtop=400lambda.

    Tlazy=500lambda; Slazy=16lambda; Xlazy=lambda(9ln3+6asinh(2/3)).
    Simple at equal H,w,depth: s²=d²+6lambda*d, T=300lambda+100d.
    All expected quantities below use these hand identities, not package outputs.
    """
    module = example()
    states = module.worked_comparison()
    scale = 1000 / (12 - 2 * math.sqrt(13))
    lazy, simple = states["lazy_wave"], states["simple"]
    assert lazy.water_depth == pytest.approx(1000)
    assert lazy.bottom_tension == pytest.approx(300 * scale)
    assert lazy.top_tension == pytest.approx(500 * scale)
    assert lazy.suspended_length == pytest.approx(16 * scale)
    assert lazy.horizontal_span == pytest.approx(scale * (9 * math.log(3) + 6 * math.asinh(2 / 3)))
    assert simple.top_tension == pytest.approx(300 * scale + 100000)
    assert simple.suspended_length == pytest.approx(math.sqrt(1000000 + 6000 * scale))
    assert simple.bottom_tension == pytest.approx(lazy.bottom_tension)
    assert simple.max_curvature == pytest.approx(1 / (3 * scale))
    text = module.render_worked_example()
    assert "1000.0" in text
    assert "not certified" in text
    # No rounded result is fed back into the engineering calculation.
    assert "36.9" in text
