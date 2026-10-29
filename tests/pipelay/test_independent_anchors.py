# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic arithmetic anchors, not independent physical qualification.

Equation forms: screening-basis.md E1-E6. Background: Bai and Bai, Subsea
Pipeline Design, Analysis and Installation (2014), chapters 5 and 6; pages
NOT_EVALUATED (publisher access refused). Catenary source opened separately:
Tatum, Classical Mechanics, The Catenary, sections 18.2 and 18.3 (2022),
https://phys.libretexts.org/Bookshelves/Classical_Mechanics/Classical_Mechanics_(Tatum)/18%3A_The_Catenary
Equations 18.2.1-18.2.6 and 18.3.3-18.3.6 give force and geometry identities.
"""

import json
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.pipelay.workflow import router, run_jlay, run_reel_lay

ROOT = Path(__file__).resolve().parents[2]


def reel_config(diameter, radius, efficiency, span):
    """Use explicitly synthetic geometry; ancillary pipe inputs use the template."""
    source = ROOT / "examples/workflows/reel-lay-pipelay/input.yml"
    cfg = yaml.safe_load(source.read_text(encoding="utf-8"))
    cfg["pipelay"].pop("operability")
    cfg["pipelay"]["pipe"]["outer_diameter"] = diameter
    cfg["pipelay"]["config"].update(
        reel_core_radius_m=radius,
        straightener_efficiency=efficiency,
        residual_out_of_straightness_span_m=span,
    )
    return cfg


def assert_reel_values(result, reeling, curvature, radius, residual, sagitta):
    """Absolute tolerances are half a last place of _reel_result rounding.

    workflow.py rounds strains to six decimals, curvature to eight, radius to
    one and sagitta to five. No physical acceptance tolerance is asserted.
    """
    assert result["reeling_bending_strain"] == pytest.approx(reeling, rel=0, abs=0.5e-6)
    assert result["residual_curvature_1_per_m"] == pytest.approx(curvature, rel=0, abs=0.5e-8)
    assert result["residual_bending_strain"] == pytest.approx(residual, rel=0, abs=0.5e-6)
    assert result["residual_out_of_straightness_m"] == pytest.approx(sagitta, rel=0, abs=0.5e-5)
    if radius is None:
        assert result["residual_radius_m"] is None
        assert result["residual_radius_status"] == "unbounded_zero_curvature"
    else:
        assert result["residual_radius_m"] == pytest.approx(radius, rel=0, abs=0.05)
        assert result["residual_radius_status"] == "finite"


def test_synthetic_reel_partial_straightening():
    """Synthetic D=0.2 m, R=10 m, eta=0.8, L=8 m; E5-E6 hand arithmetic.

    Bai and Bai (2014), chs. 5-6, pages NOT_EVALUATED; repository E5-E6:
    reeling = 0.2/(2*10) = 0.01; kappa = (1-0.8)/10 = 0.02 /m;
    radius = 1/0.02 = 50 m; residual = 0.2*0.02/2 = 0.002;
    sagitta = 0.02*8**2/8 = 0.16 m. These are synthetic, not benchmarks.
    """
    result = run_reel_lay(reel_config(0.2, 10, 0.8, 8)["pipelay"])
    assert_reel_values(result, 0.01, 0.02, 50, 0.002, 0.16)


def test_synthetic_reel_repeating_curvature():
    """Synthetic D=0.3 m, R=12 m, eta=0.9, L=10 m; E5-E6 arithmetic.

    Bai and Bai (2014), chs. 5-6, pages NOT_EVALUATED; repository E5-E6:
    reeling = 0.3/(2*12) = 0.0125; kappa = (1-0.9)/12 = 1/120 /m;
    radius = 120 m; residual = 0.3/(120*2) = 0.00125;
    sagitta = 10**2/(120*8) = 5/48 m. Synthetic, not physical validation.
    """
    result = run_reel_lay(reel_config(0.3, 12, 0.9, 10)["pipelay"])
    assert_reel_values(result, 0.0125, 1 / 120, 120, 0.00125, 5 / 48)


def test_synthetic_reel_perfect_straightening(tmp_path):
    """Synthetic D=0.4 m, R=20 m, eta=1, L=16 m; E5-E6 arithmetic.

    Bai and Bai (2014), chs. 5-6, pages NOT_EVALUATED; repository E5-E6:
    reeling = 0.4/(2*20) = 0.01; kappa = (1-1)/20 = 0 /m;
    residual = 0.4*0/2 = 0; sagitta = 0*16**2/8 = 0 m.
    The reciprocal of zero curvature is unbounded, encoded as JSON null.
    """
    cfg = reel_config(0.4, 20, 1, 16)
    cfg["pipelay"]["outputs"]["directory"] = str(tmp_path)
    result = router(cfg)["pipelay"]
    assert_reel_values(result, 0.01, 0, None, 0, 0)
    payload = json.loads(Path(result["summary_json"]).read_bytes())
    assert payload["residual_radius_m"] is None
    assert payload["residual_radius_status"] == "unbounded_zero_curvature"


def independent_catenary(depth, angle_deg, pipe):
    """E1 annulus weight plus Tatum (2022) 18.2.1-6, 18.3.3-6.

    At the top, slope = cot(angle). Thus x/a = asinh(cot(angle)),
    depth = a*(cosh(x/a)-1); this solves a without the implementation's
    depth*sin(angle)/(1-sin(angle)) expression. Then s=a*sinh(x/a),
    H=w*a, V=w*s, T=hypot(H,V)=w*sqrt(a*a+s*s).
    """
    d, wall, coating = (pipe[k] for k in
                        ("outer_diameter", "wall_thickness", "coating_thickness"))
    # E1, retained gravity convention; no standard allowable or new coefficient.
    w = 9.80665 * math.pi / 4 * (
        pipe["steel_density"] * (d*d - (d-2*wall)**2)
        + pipe["coating_density"] * ((d+2*coating)**2 - d*d)
        - pipe["seawater_density"] * (d+2*coating)**2
    )
    u = math.asinh(1 / math.tan(math.radians(angle_deg)))
    a = depth / (math.cosh(u) - 1)
    x = a * u
    s = a * math.sinh(u)
    horizontal, vertical = w * a, w * s
    return w, a, x, s, horizontal, vertical, math.hypot(horizontal, vertical)


@pytest.mark.parametrize("depth, angle", [(1500, 3), (800, 6), (2000, 10)])
def test_jlay_plain_catenary_closure(depth, angle):
    """Synthetic geometry cases; independently integrate the plain catenary.

    Tatum (2022), 18.2.1-6, 18.3.3-6: a=H/w, s=a*sinh(x/a), V=w*s,
    T=sqrt(H**2+V**2)=w*sqrt(a**2+s**2). Helper solves from slope and
    height, not from any returned quantity. All expected values use inputs.

    calculate_sagbend rounds x and s to one decimal: abs error <=0.05 m.
    Workflow weight rounds to three decimals: abs error <=0.0005 N/m.
    For unrounded a, H and T, rel=1e-12 is a numerical roundoff allowance
    for equivalent double-precision identities, not a physical tolerance.
    Vertical tension recovered from rounded s adds at most w*0.05 N.
    """
    source = ROOT / "examples/workflows/jlay-pipelay/input.yml"
    settings = yaml.safe_load(source.read_text(encoding="utf-8"))["pipelay"]
    w, a, x, s, horizontal, vertical, total = independent_catenary(
        depth, angle, settings["pipe"])
    settings["config"].update(water_depth_m=depth, tower_angle_deg=angle,
                              top_tension_kN=total / 1000)
    result = run_jlay(settings)
    sagbend = result["sagbend"]
    assert result["pipe_submerged_weight_N_per_m"] == pytest.approx(w, rel=0, abs=0.0005)
    assert sagbend["catenary_parameter_m"] == pytest.approx(a, rel=1e-12)
    assert sagbend["sagbend_radius_m"] == pytest.approx(a, rel=1e-12)
    assert sagbend["horizontal_offset_m"] == pytest.approx(x, rel=0, abs=0.05)
    assert sagbend["suspended_length_m"] == pytest.approx(s, rel=0, abs=0.05)
    assert sagbend["horizontal_tension_kN"] * 1000 == pytest.approx(horizontal, rel=1e-12)
    assert sagbend["derived_top_tension_kN"] * 1000 == pytest.approx(total, rel=1e-12)
    assert w * sagbend["suspended_length_m"] == pytest.approx(vertical, rel=0, abs=w*0.05)
    assert total == pytest.approx(w * math.sqrt(a*a + s*s), rel=1e-12)
