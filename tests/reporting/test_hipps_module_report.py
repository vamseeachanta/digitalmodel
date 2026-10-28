# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent synthetic integrals, delivery integrity and offline report checks."""

import copy
import hashlib
import importlib.util
import json
import math
import re
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_hipps_module_report.py"
REPORT = ROOT / "docs/reports/modules/hipps"
ARTIFACTS = {"index.html", "results.json", "changes.json", "README.md"}
# Numerical roundoff tolerance, not an engineering acceptance tolerance. The
# short arithmetic chains below accumulate far less than this relative error.
REL = 1e-12


def builder():
    spec = importlib.util.spec_from_file_location("hipps_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def assert_hand_oracles(data):
    """Use literal rational targets derived here, never production calculations.

    Synthetic p(t)=lambda*t, lambda=1e-5 /h and T=1000 h. Integrating
    p(t)/T gives lambda*T/2=1/200. Integrating p(t)^2/T for independent
    channels gives lambda^2*T^2/3=1/30000. These are linear approximations,
    not exponential failure probabilities. The reciprocal is 200.
    Uniform travel: (12/100 m)/(2/100 m/s)=6 s. At 3 s, remaining
    linear conductance is 1/2 and R^(-t/T)=100^(-1/2)=1/10. Seating
    is a separate exact-zero rule, including the equal-percentage jump.
    """
    expected = {
        "single_average": 1 / 200,
        "independent_average": 1 / 30000,
        "reciprocal": 200,
        "closure_time": 6,
        "linear_half": 1 / 2,
        "equal_percentage_half": 1 / 10,
    }
    results = data["results"]
    assert set(results) == set(expected) | {"linear_seated", "equal_percentage_seated"}
    for key, target in expected.items():
        value = results[key]["value"]
        assert math.isfinite(value)
        assert value == pytest.approx(target, rel=REL, abs=0), key
    assert results["linear_seated"]["value"] == 0
    assert results["equal_percentage_seated"]["value"] == 0
    assert data["inputs"] == {
        "lambda_per_hour": 1e-5, "proof_interval_h": 1000.0, "beta": 0.0,
        "stroke_m": 0.12, "loaded_speed_m_s": 0.02,
        "halfway_s": 3.0, "seating_s": 6.0, "rangeability": 100.0,
    }
    # Plot sampling is also checked independently, including endpoints. There
    # are only the stipulated start, halfway and seating sample instants.
    assert [row["elapsed_s"] for row in data["closure_samples"]] == [0, 3, 6]
    for row in data["closure_samples"]:
        t = row["elapsed_s"]
        if t == 6:
            assert row["linear"] == row["equal_percentage"] == 0
        else:
            assert row["linear"] == pytest.approx(1 - t / 6, rel=REL, abs=0)
            assert row["equal_percentage"] == pytest.approx(100 ** (-t / 6), rel=REL, abs=0)


def test_reliability_integrals_and_closure_kinematics_match_hand_calculation():
    """Compare normal public APIs directly to independent integrals/kinematics."""
    from digitalmodel.subsea.hipps import (
        closure_fraction, pfd_avg_1oo1, pfd_avg_1oo2,
        risk_reduction_factor, stroke_time,
    )

    one = pfd_avg_1oo1(1e-5, 1000)
    assert one == pytest.approx(1 / 200, rel=REL, abs=0)
    assert pfd_avg_1oo2(1e-5, 1000, beta=0) == pytest.approx(1 / 30000, rel=REL, abs=0)
    assert risk_reduction_factor(one) == pytest.approx(200, rel=REL, abs=0)
    time = stroke_time(stroke_length=0.12, actuator_speed=0.02)
    assert time == pytest.approx(6, rel=REL, abs=0)
    assert closure_fraction(3, time, "linear") == pytest.approx(1 / 2, rel=REL, abs=0)
    assert closure_fraction(3, time, "equal_percentage", rangeability=100) == pytest.approx(
        1 / 10, rel=REL, abs=0,
    )
    assert closure_fraction(6, time, "linear") == 0
    assert closure_fraction(6, time, "equal_percentage", rangeability=100) == 0


def run_build(tmp_path):
    output = tmp_path / "output"
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(tmp_path / "scratch"),
         "--output-dir", str(output)], cwd=ROOT, check=True, capture_output=True, text=True,
    )
    return output


def load_data(directory):
    return json.loads((directory / "results.json").read_text(encoding="utf-8"))


def embedded_data(page):
    return json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S,
    )[1])


def assert_manifest(directory):
    """Independent digest readback, including sources and the generator."""
    manifest = json.loads((directory / "manifest.json").read_text(encoding="utf-8"))
    assert set(manifest["artifacts"]) == ARTIFACTS
    assert manifest["reviewed_revision"] == "09089a121a16069c4b657d1a320a630a23ab1dd5"
    for name, expected in manifest["artifacts"].items():
        assert hashlib.sha256((directory / name).read_bytes()).hexdigest() == expected, name
    for name, expected in manifest["sources"].items():
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == expected, name
    assert manifest["generator"] == SCRIPT.relative_to(ROOT).as_posix()
    assert manifest["generator_sha256"] == hashlib.sha256(SCRIPT.read_bytes()).hexdigest()


def test_rebuild_is_byte_identical(tmp_path):
    first = run_build(tmp_path / "first")
    second = run_build(tmp_path / "second")
    names = ARTIFACTS | {"manifest.json"}
    assert {p.name for p in first.iterdir()} == names
    assert {p.name for p in second.iterdir()} == names
    for name in names:
        assert (first / name).read_bytes() == (second / name).read_bytes()
        assert (first / name).read_bytes() == (REPORT / name).read_bytes(), name


def test_report_values_have_independent_oracles():
    """Delivered answers must satisfy hand targets even after consistent rebuilds."""
    data = load_data(REPORT)
    assert_hand_oracles(data)
    assert embedded_data((REPORT / "index.html").read_text(encoding="utf-8")) == data


def test_manifest_binds_delivered_bytes(tmp_path):
    assert_manifest(REPORT)
    assert_manifest(run_build(tmp_path))


@pytest.mark.parametrize("key", [
    "single_average", "independent_average", "reciprocal", "closure_time",
    "linear_half", "equal_percentage_half", "linear_seated", "equal_percentage_seated",
])
@pytest.mark.parametrize("fault", ["value", "sign"])
def test_consistent_regeneration_cannot_replace_independent_oracle(tmp_path, monkeypatch, key, fault):
    """A builder and report agreeing on a wrong value must still fail numerically."""
    build = builder()
    original = build.calculate

    def wrong():
        data = original()
        value = data["results"][key]["value"]
        # For seating zero, use a nonzero signed residual; -0.0 equals zero.
        data["results"][key]["value"] = (-(value or 1) if fault == "sign" else value + 1)
        return data

    monkeypatch.setattr(build, "calculate", wrong)
    output = tmp_path / "wrong-output"
    build.build(tmp_path / "scratch", output)
    data = load_data(output)
    assert embedded_data((output / "index.html").read_text(encoding="utf-8")) == data
    assert_manifest(output)  # Fresh hashes alone cannot establish numerical correctness.
    with pytest.raises(AssertionError):
        assert_hand_oracles(data)


def test_tampered_result_is_detected(tmp_path):
    output = run_build(tmp_path)
    data = load_data(output)
    data["results"]["single_average"]["value"] *= -1
    (output / "results.json").write_text(json.dumps(data) + "\n", encoding="utf-8")
    with pytest.raises(AssertionError, match="results.json"):
        assert_manifest(output)
    with pytest.raises(AssertionError):
        assert_hand_oracles(data)


class Links(HTMLParser):
    def __init__(self):
        super().__init__()
        self.ids, self.links, self.remote_assets = set(), [], []
        self.headings, self.current, self.text = [], None, []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            assert attrs["id"] not in self.ids
            self.ids.add(attrs["id"])
        if tag == "a" and "href" in attrs:
            self.links.append(attrs["href"])
        if tag in {"script", "img", "link", "iframe"}:
            self.remote_assets.extend(attrs[k] for k in ("src", "href") if k in attrs)
        if tag in {"h2", "h3"}:
            self.current, self.text = tag, []

    def handle_data(self, text):
        if self.current:
            self.text.append(text)

    def handle_endtag(self, tag):
        if tag == self.current:
            self.headings.append((tag, "".join(self.text)))
            self.current = None


def test_report_links_and_required_sections():
    page = (REPORT / "index.html").read_text(encoding="utf-8")
    parsed = Links()
    parsed.feed(page)
    assert not parsed.remote_assets
    expected = ["Summary", "Context and scope", "Input basis", "Method", "Worked calculation",
                "Checks", "Results", "Limits / NOT_EVALUATED", "Way forward", "References and evidence"]
    assert [text for tag, text in parsed.headings if tag == "h2"] == [
        f"{i}. {text}" for i, text in enumerate(expected, 1)
    ]
    assert parsed.headings[1] == ("h3", "Revision history")
    assert all(f"s{i}" in parsed.ids for i in range(1, 11))
    for link in parsed.links:
        parts = urlsplit(link)
        assert not parts.scheme and not parts.netloc
        if parts.path:
            assert (REPORT / unquote(parts.path)).is_file(), link
        else:
            assert parts.fragment in parsed.ids
    assert 'class="hs-doc"' in page and '<svg ' in page
    assert "not exact exponential reliability probabilities" in page
    assert "not an achieved system SIL" in page
    assert "seating discontinuity" in page
    assert "sampled rise rate" in page
    data = load_data(REPORT)
    build = builder()
    for row in data["results"].values():
        assert row["unit"] and row["basis"] and row["api"]
        assert build.result_row(row) in page
    history = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    for row in history:
        assert row["date"] in page and row["change"] in page
    from digitalmodel.subsea import hipps
    assert {row["name"] for row in data["inventory"]} == set(hipps.__all__)
    assert "<script src=" not in page
    assert not re.search(r"https?://", page)


def test_production_replay_is_separate_from_oracle():
    """Freshness comparison only; the independent check is above."""
    assert load_data(REPORT) == builder().calculate()


def test_render_escapes_text_and_embedded_json():
    build = builder()
    data = copy.deepcopy(build.calculate())
    data["results"]["single_average"]["basis"] = '</script><img src="probe">'
    page = build.render(data)
    assert '</script><img src="probe">' not in page
    assert "&lt;/script&gt;" in page
    assert embedded_data(page) == data


def test_scratch_inside_checkout_is_rejected(tmp_path):
    with pytest.raises(ValueError, match="outside the checkout"):
        builder().build(ROOT / "calculation-scratch", tmp_path / "output")
