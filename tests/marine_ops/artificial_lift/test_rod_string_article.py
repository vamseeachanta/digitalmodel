"""Checks of the rod string article (rod pump series, part 2) against hand-worked values.

The article's numbers come from docs/domains/artificial_lift/rod-string-article/results.json, written by
examples/domains/artificial_lift/build_rod_string_article.py --refresh. These tests read the saved results
(no solver run) and assert the reported values against hand calculations, then check that the committed
pages are current and that the public text carries no local or private detail.
Rebuild command: PYTHONPATH=src python examples/domains/artificial_lift/build_rod_string_article.py --refresh
"""
import importlib.util
import json
import math
from pathlib import Path

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump.rod_string import (
    rod_area, rod_elastic_constant,
)

ROOT = Path(__file__).resolve().parents[3]
FOLDER = ROOT / "docs/domains/artificial_lift/rod-string-article"
BUILDER = ROOT / "examples/domains/artificial_lift/build_rod_string_article.py"
RESULTS = json.loads((FOLDER / "results.json").read_text(encoding="utf-8"))
KEY = RESULTS["key_results"]


def _builder():
    spec = importlib.util.spec_from_file_location("build_rod_string_article", BUILDER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def test_rod_area_and_elastic_constant_hand_values():
    # 3/4 in rod: area = pi/4 * 0.75^2 = 0.441786 in^2.
    assert rod_area(0.75) == pytest.approx(0.441786, abs=1e-6)
    # 7/8 in rod: area = pi/4 * 0.875^2 = 0.601320 in^2.
    assert rod_area(0.875) == pytest.approx(0.601320, abs=1e-6)
    # Er = 12 / (A E); 3/4 in at E = 30.5e6 psi: 12 / (0.441786 * 30.5e6) = 8.90572e-7 in/lbf/ft.
    assert rod_elastic_constant(0.75, 30.5e6) == pytest.approx(8.90572e-7, rel=1e-5)
    # 7/8 in at E = 30.5e6 psi: 12 / (0.601320 * 30.5e6) = 6.54297e-7 in/lbf/ft.
    assert rod_elastic_constant(0.875, 30.5e6) == pytest.approx(6.54297e-7, rel=1e-5)


def test_static_stretch_against_hand_calculation():
    # Hand: 3,310.36 lbf * (6.54297e-7 * 1,400 + 8.90572e-7 * 3,350) = 3,310.36 * 3.89944e-3 = 12.9085 in.
    assert KEY["fluid_load_lbf"] == pytest.approx(3310.36, abs=0.01)
    assert KEY["static_stretch_in"] == pytest.approx(12.9085, abs=0.001)
    # Taper split: 3,310.36 * 9.16016e-4 = 3.0323 in; 3,310.36 * 2.98342e-3 = 9.8762 in.
    assert KEY["static_stretch_by_taper_in"][0] == pytest.approx(3.0323, abs=0.001)
    assert KEY["static_stretch_by_taper_in"][1] == pytest.approx(9.8762, abs=0.001)
    # Share of the stroke lost: 12.9085 / (53.9868 - 39.5700) = 12.9085 / 14.4168 = 89.54 %.
    assert KEY["stroke_loss_in"] == pytest.approx(14.4168, abs=0.001)
    assert KEY["stretch_share_of_loss_pct"] == pytest.approx(89.54, abs=0.01)


def test_polished_rod_and_taper_top_stress():
    # Polished rod: 12,240 lbf / 0.601320 in^2 = 20,355.2 psi.
    assert KEY["peak_stress_top_taper1_psi"] == pytest.approx(20355.2, abs=0.1)
    # Top of the 3/4 in rods: interpolated joint load over the 3/4 in area.
    assert KEY["peak_stress_top_taper2_psi"] == pytest.approx(KEY["load_at_joint_max_lbf"] / 0.441786, rel=1e-5)
    # The thinner taper's top is the most stressed point on the string.
    assert max(p["max_stress_psi"] for p in RESULTS["profile"]) <= KEY["peak_stress_top_taper2_psi"] * 1.001


def test_energy_check_with_no_damping():
    zero = next(m for m in RESULTS["damping_sweep"] if m["scale"] == 0.0)
    # With no damping the pump card's work equals the surface card's work.
    assert zero["pump_card_area_ft_lbf"] == pytest.approx(zero["surface_card_area_ft_lbf"], rel=5e-4)
    base = next(m for m in RESULTS["damping_sweep"] if m["scale"] == 1.0)
    assert base["damping_share_pct"] == pytest.approx(KEY["damping_share_pct"], abs=1e-6)


def test_courant_limit_hand_value_and_demonstration():
    g = RESULTS["grid"]
    # Hand: dx = 1,447.8 m / 199 = 7.2754 m; c = sqrt(2.10290e11 Pa / 8,531.3 kg/m^3) = 4,964.8 m/s;
    # limit = floor(9.35 s * 4,964.75 / 7.27538) = floor(6,380.5) = 6,380 samples a stroke.
    assert g["dx_m"] == pytest.approx(7.2754, abs=1e-4)
    assert g["wave_speed_m_s"] == pytest.approx(4964.75, abs=0.1)
    assert g["courant_limit_samples_per_stroke"] == math.floor(g["period_s"] * g["wave_speed_m_s"] / g["dx_m"]) == 6380
    runs = {c["samples_per_stroke"]: c for c in RESULTS["courant"]}
    assert runs[6380]["courant_number"] <= 1.0 < runs[6381]["courant_number"]
    assert abs(runs[6380]["max_pump_load_lbf"] / runs[187]["max_pump_load_lbf"] - 1) < 0.01
    assert runs[6400]["max_pump_load_lbf"] > 1e9


def test_committed_pages_are_current():
    assert _builder().main(["--check"]) == 0, (
        "rebuild: PYTHONPATH=src python examples/domains/artificial_lift/build_rod_string_article.py")


def test_public_text_has_no_local_or_private_detail():
    texts = [p.read_text(encoding="utf-8") for p in FOLDER.iterdir() if p.suffix in (".md", ".html")]
    assert texts
    banned = ["C:\\", "C:/", "/mnt/", "Users/", "ace-linux", "localhost", "Goodman", "Reed", "fleet-ops"]
    for text in texts:
        for word in banned:
            assert word not in text, word
    article = FOLDER / "rod-string-article.html"
    if article.exists():
        text = article.read_text(encoding="utf-8")
        assert "support@aceengineer.com. We read every reply and credit the ideas we use" in text
        assert " AI " not in text and "artificial intelligence" not in text.lower()
