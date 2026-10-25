"""Arithmetic anchors and consistency checks for the screening report."""
import importlib.util
import json
from pathlib import Path
import re

import pytest

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
REBUILD = "PYTHONPATH=src python docs/reports/modules/mooringresilience/generate.py"


def builder():
    spec = importlib.util.spec_from_file_location("mooring_report", HERE / "generate.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_hand_worked_anchors():
    data = builder().calculate()
    first, third = data["cases"][0], data["cases"][2]
    # Delta T = 22.6 kN/m * 2 m = 45.2 kN; stress = 45200/22600 = 2 MPa.
    # Chosen curve N = 1000/2 = 500 cycles; life = 500/100 = 5 years.
    assert first["result"]["fatigue_life_years"] == pytest.approx(5.0, abs=1e-10)
    # Margin = 5 years / chosen 10-year duration = 0.5, dimensionless.
    assert first["result"]["fatigue_margin"] == pytest.approx(0.5, abs=1e-10)
    # At 4 m, stress = (22.6*4*1000)/22600 = 4 MPa; life = 1000/4/100 = 2.5 years.
    assert third["result"]["fatigue_life_years"] == pytest.approx(2.5, abs=1e-10)


def test_displayed_input_headline_consistency():
    data = builder().calculate()
    f, cfg = data["factors"], data["configuration"]
    h = data["cases"][0]["wave_height_m"]
    stress = f["dyn_tension_per_Hs_kN"] * h * 1000 / cfg["line_area_mm2"]
    life = f["sn_a_D"] * stress ** -f["sn_m"] / f["annual_cycles"]
    assert round(life, 1) == data["cases"][0]["result"]["fatigue_life_years"]


def test_module_consistency_and_rebuild(tmp_path):
    from digitalmodel.mooring_resilience import Metocean, MooringConfig, ResilienceFactors, assess

    mod = builder()
    data = mod.calculate()
    for case in data["cases"]:
        result = assess(MooringConfig(**data["configuration"]),
                        Metocean(case["wave_height_m"], data["period_s"]),
                        ResilienceFactors(**data["factors"]))
        for key, value in case["result"].items():
            actual = getattr(result, key)
            if isinstance(value, float):
                digits = 1 if key.endswith("_kN") or key.endswith("_years") else 3
                assert value == round(actual, digits)
            else:
                assert value == actual
    fresh = mod.render(data)
    committed = (HERE / "report.html").read_text(encoding="utf-8")
    assert fresh == committed, f"Report is stale. Rebuild: {REBUILD}"
    out = tmp_path / "report.html"
    mod.write_report(out)
    assert out.read_text(encoding="utf-8") == committed, REBUILD
    assert b"\r" not in out.read_bytes()


def test_document_contract():
    from html.parser import HTMLParser

    class Structure(HTMLParser):
        headings = 0
        def handle_starttag(self, tag, attrs):
            if tag == "h2":
                self.headings += 1

    html = (HERE / "report.html").read_text(encoding="utf-8")
    parser = Structure()
    parser.feed(html)
    assert parser.headings == 10
    assert 'class="hs-doc"' in html
    assert "Draft for maintainer review" in html
    assert "<script src=" not in html and '<link rel="stylesheet"' not in html
    assert 'id="comments"' in html and "showSaveFilePicker" in html
    assert 'id="change-panel"' in html
    assert "fatigue margin limit" in html
    changes = json.loads((HERE / "changes.json").read_text(encoding="utf-8"))
    assert changes[0]["revision"] == "R1"


@pytest.mark.parametrize(("selector", "property_name", "expected"), [
    (".wrap", "background", "var(--panel)"),
    ("svg text", "fill", "var(--ink)"),
])
def test_report_surface_and_svg_text_follow_theme(selector, property_name, expected):
    html = (HERE / "report.html").read_text(encoding="utf-8")
    css = re.search(r"<style>(.*?)</style>", html, re.S).group(1)
    rules = re.findall(re.escape(selector) + r"\s*\{([^}]*)\}", css)
    values = [match.group(1).strip() for rule in rules
              for match in re.finditer(r"(?:^|;)\s*" + property_name + r"\s*:\s*([^;]+)", rule)]
    assert values, f"No theme declaration for {selector} {property_name}"
    assert values[-1] == expected
