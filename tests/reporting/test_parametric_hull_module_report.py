# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic and delivery checks for the bounded hull report."""

import copy
import hashlib
import importlib.util
import json
import math
import re
import subprocess
import sys
from fractions import Fraction
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit

import numpy as np
import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/parametric_hull_analysis"
SCRIPT = REPORT / "build_report.py"
# Independent derivation in sources.json, hand-calculation and numerical-control:
# phi(U*t,t) = -(omega-k*U*cos(beta))*t; k=omega**2/g in deep water.
RTOL = ATOL = 1e-12  # Roundoff bounds specified for this synthetic case.


def builder():
    assert SCRIPT.is_file(), "The local report builder is required"
    spec = importlib.util.spec_from_file_location("hull_module_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def hand_calculation():
    """Use rational arithmetic and exact cardinal cosines, without production."""
    # Synthetic inputs and cardinal headings: sources.json, hand-calculation.
    omega = [Fraction(2), Fraction(4)]
    gravity, speed = Fraction(8), Fraction(1)
    k = [w * w / gravity for w in omega]
    cosine = [Fraction(1), Fraction(0), Fraction(-1)]
    moving = [[float(w - ki * speed * c) for c in cosine]
              for w, ki in zip(omega, k)]
    stationary = [[float(w)] * len(cosine) for w in omega]
    assert [float(ki) for ki in k] == [0.5, 2.0]
    assert moving == [[1.5, 2.0, 2.5], [2.0, 4.0, 6.0]]
    return [float(ki) for ki in k], moving, stationary


class Page(HTMLParser):
    """Collect visible numeric cells, hyperlinks and the coordinate sketch."""

    def __init__(self, text):
        super().__init__()
        self.values, self.links, self.headings = [], [], []
        self.active = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "span" and "data-value" in attrs:
            self.active = [attrs["data-value"], attrs["data-unit"], ""]
        if tag in ("a", "link", "img", "script"):
            for key in ("href", "src"):
                if key in attrs:
                    self.links.append(attrs[key])
        if tag == "g" and "data-heading" in attrs:
            self.headings.append(attrs)

    def handle_data(self, text):
        if self.active is not None:
            self.active[2] += text

    def handle_endtag(self, tag):
        if tag == "span" and self.active is not None:
            self.values.append(tuple(self.active))
            self.active = None


def payload(text):
    return json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>',
        text, re.S,
    )[1])


def test_report_values_have_independent_oracle(tmp_path):
    """Changing a worked input, result, unit or visible rounding must fail."""
    build = builder()
    k, moving, stationary = hand_calculation()
    text = build.render(tmp_path / "arithmetic")
    actual = payload(text)
    inputs = {
        "g": 8.0, "omega": [2.0, 4.0], "speed": 1.0,
        "stationary_speed": 0.0, "heading_rad": [0.0, math.pi / 2, math.pi],
        "water_depth": "deep water (infinite)",
    }
    assert actual["inputs"] == inputs
    assert actual["units"] == {
        "g": "m/s²", "omega": "rad/s", "speed": "m/s",
        "stationary_speed": "m/s", "heading_rad": "rad",
        "water_depth": "m", "k": "1/m", "moving": "rad/s",
        "stationary": "rad/s", "difference_k": "1/m",
        "difference_moving": "rad/s", "difference_stationary": "rad/s",
        "rtol": "dimensionless", "atol": "same unit as compared quantity",
    }
    assert actual["inputs_preserved"] is True
    assert actual["rtol"] == RTOL and actual["atol"] == ATOL
    expected = {"k": k, "moving": moving, "stationary": stationary}
    for key, reference in expected.items():
        assert np.shape(actual[key]) == np.shape(reference)
        assert np.shape(actual["reference"][key]) == np.shape(reference)
        np.testing.assert_allclose(actual[key], reference, rtol=RTOL, atol=ATOL)
        np.testing.assert_allclose(actual["reference"][key], reference, rtol=RTOL, atol=ATOL)
        difference = np.asarray(actual[key]) - reference
        np.testing.assert_allclose(actual["difference_" + key], difference,
                                   rtol=RTOL, atol=ATOL)
        np.testing.assert_allclose(difference, np.zeros_like(difference),
                                   rtol=RTOL, atol=ATOL)

    # Test the original arrays directly; a payload assertion alone cannot prove
    # that production calls preserved their callers' inputs.
    omega, headings = np.array(inputs["omega"]), np.array(inputs["heading_rad"])
    original_omega, original_headings = omega.copy(), headings.copy()
    for speed, reference in [(inputs["speed"], moving), (inputs["stationary_speed"], stationary)]:
        result = build.forward_speed.encounter_frequency(
            omega, speed, headings, water_depth=np.inf, g=inputs["g"],
        )
        assert result.shape == (2, 3)  # Two input frequencies, three headings.
        np.testing.assert_allclose(result, reference, rtol=RTOL, atol=ATOL)
        np.testing.assert_array_equal(omega, original_omega)
        np.testing.assert_array_equal(headings, original_headings)

    # Enumerate every displayed numeric value independently of the formatter.
    visible = {
        "inputs.g": ("m/s²", "8.0"), "inputs.speed": ("m/s", "1.0"),
        "inputs.stationary_speed": ("m/s", "0.0"),
        "rtol": ("dimensionless", "1.0e-12"),
        "atol": ("same unit as compared quantity", "1.0e-12"),
    }
    for i, w in enumerate(inputs["omega"]):
        visible[f"inputs.omega.{i}"] = ("rad/s", f"{w:.1f}")
    for j, label in enumerate(["0", "π/2", "π"]):
        visible[f"inputs.heading_rad.{j}"] = ("rad", label)
    for key, reference in expected.items():
        unit = "1/m" if key == "k" else "rad/s"
        for indices in np.ndindex(np.shape(reference)):
            suffix = ".".join(str(i) for i in indices)
            value = np.asarray(reference)[indices]
            visible[f"{key}.{suffix}"] = (unit, f"{value:.1f}")
            visible[f"reference.{key}.{suffix}"] = (unit, f"{value:.1f}")
            visible[f"difference_{key}.{suffix}"] = (unit, "0.0e+00")
    page = Page(text)
    assert {path for path, _, _ in page.values} == set(visible)
    for path, unit, shown in page.values:
        assert (unit, shown) == visible[path], path
    assert 'fill="currentColor" stroke="none"' in text
    assert len(page.headings) == len(inputs["heading_rad"])
    for heading, angle, cosine in zip(page.headings, inputs["heading_rad"], [1, 0, -1]):
        assert float(heading["data-heading"]) == angle
        assert float(heading["data-cosine"]) == pytest.approx(cosine, abs=ATOL)


def cli_build(destination):
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(destination / "scratch"),
         "--output", str(destination / "output")], cwd=ROOT,
        check=True, capture_output=True, text=True,
    )
    return destination / "output"


def test_rebuild_is_deterministic(tmp_path):
    """No volatile metadata is present; compare all delivered bytes unchanged."""
    build = builder()
    first, second = cli_build(tmp_path / "first"), cli_build(tmp_path / "second")
    assert {p.name for p in first.iterdir()} == {"index.html", "changes.json", "sources.json"}
    for name in ("index.html", "changes.json", "sources.json"):
        assert (first / name).read_bytes() == (second / name).read_bytes()
        assert (first / name).read_bytes() == (REPORT / name).read_bytes(), "Rebuild the delivered report"
    data = payload((first / "index.html").read_text(encoding="utf-8"))
    required = {str(SCRIPT.relative_to(ROOT).as_posix()),
                "src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py",
                "tests/reporting/test_parametric_hull_module_report.py",
                "docs/api/_assets/brand.css"}
    assert required <= set(data["source_sha256"])
    for relative, digest in data["source_sha256"].items():
        assert digest == hashlib.sha256((ROOT / relative).read_bytes()).hexdigest()


@pytest.mark.parametrize("field", ["k", "moving", "stationary", "reference", "difference_moving"])
def test_report_rejects_stale_calculation(tmp_path, monkeypatch, field):
    """A changed returned calculation must never leave a stale displayed result."""
    build = builder()
    fixture = copy.deepcopy(build.calculate(tmp_path / "fixture"))
    if field == "reference":
        fixture[field]["moving"][0][0] += 0.25
    elif field == "k":
        fixture[field][0] += 0.25  # Deliberate fault exceeds the roundoff bounds.
    else:
        fixture[field][0][0] += 0.25
    monkeypatch.setattr(build, "calculate", lambda scratch: fixture)
    with pytest.raises(ValueError, match="hand calculation"):
        build.render(tmp_path / "changed")


def test_structure_sources_changes_and_local_links(tmp_path):
    build = builder()
    text = build.render(tmp_path / "structure")
    assert re.findall(r'<h2 id="s(\d+)">', text) == [str(i) for i in range(1, 11)]
    assert text.index("1. Summary") < text.index("Revision history") < text.index("2. Scope")
    assert 'class="hs-doc"' in text and "changes-panel" in text
    assert "showSaveFilePicker" in text
    for method in ("BEM execution", "Meshing", "Empirical speed/amplitude corrections",
                   "Bank effects", "Standards conformity", "Finite-depth dispersion",
                   "Passing interaction", "Squat screening"):
        assert f"{method}: NOT_EVALUATED" in text
    for link in Page(text).links:
        parsed = urlsplit(link)
        if not parsed.scheme:
            assert not parsed.netloc
            assert not Path(unquote(parsed.path)).is_absolute()
            target = REPORT / unquote(parsed.path) if parsed.path else REPORT / "index.html"
            assert target.is_file(), link
            if parsed.fragment:
                assert f'id="{parsed.fragment}"' in target.read_text(encoding="utf-8"), link
    sources = json.loads((REPORT / "sources.json").read_text(encoding="utf-8"))
    assert sources["hand-calculation"]["derivation"]
    assert sources["hand-calculation"]["status"] == "synthetic software reference"
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert json.loads(re.search(r'<script id="changes" type="application/json">(.*?)</script>', text, re.S)[1]) == changes
    for change in changes:
        for value in change.values():
            assert value in text
    assert "github.com/" in text and "/issues/3327" in text


def test_scratch_cannot_be_inside_checkout():
    build = builder()
    with pytest.raises(ValueError, match="outside"):
        build.render(REPORT / "scratch")


def test_heading_docstring_matches_existing_contract():
    build = builder()
    doc = build.forward_speed.encounter_frequency.__doc__
    assert "0 = following seas" in doc
    assert "Head seas (beta=pi)" in doc
    assert "Following seas (beta=0)" in doc
    assert "following the DNV convention" not in doc
