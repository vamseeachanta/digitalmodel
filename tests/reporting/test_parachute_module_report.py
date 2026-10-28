# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic, delivery integrity and offline report contracts.

Drag assumptions and exact unit definitions: the package's
``docs/worked-synthetic-canopy-drag.md``. Cantilever assumptions: the existing
``TestCantileverBeam`` fixture in test_frame_solver.py. Expected responses below
come from SI dimensional conversion and integration/balance, never the builder.
"""
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
SCRIPT = ROOT / "scripts/reports/build_parachute_module_report.py"
REPORT = ROOT / "docs/reports/modules/parachute"
ARTIFACTS = {"index.html", "results.json", "changes.json", "README.md", "manifest.json"}


def builder():
    spec = importlib.util.spec_from_file_location("parachute_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def assert_drag_oracle(rows):
    """Use exact NIST definitions cited by the worked document, not helpers."""
    ft_m, mile_m, hour_s, lbf_n = 0.3048, 1609.344, 3600.0, 4.4482216152605
    # Circle area integrates 2*pi*r dr from zero to the assumed disk radius.
    area_m2 = math.pi * (5.0 * ft_m / 2) ** 2
    density_si = 0.0023 * (lbf_n / ft_m) / ft_m**3
    assert len(rows) == 2
    for row, speed in zip(rows, (40.0, 60.0)):
        force_si = density_si * (speed * mile_m / hour_s)**2 * 1.1 * area_m2 * 1.25 / 2
        assert row["speed_mph"] == speed
        # Rounded production N/lbf conversion; not a physical accuracy tolerance.
        assert row["force_n"] == pytest.approx(force_si, rel=5e-7, abs=0)
        assert row["force_lbs"] == pytest.approx(force_si / lbf_n, rel=1e-12, abs=0)
        assert row["speed_fps"] == pytest.approx(speed * mile_m / hour_s / ft_m, rel=1e-12, abs=0)
        assert row["chute_area_ft2"] == pytest.approx(area_m2 / ft_m**2, rel=1e-12, abs=0)
        assert (row["cd"], row["cx"], row["rho"]) == (1.1, 1.25, 0.0023)
    # The independent algebraic ratio is 9/4 at unchanged density/area/coefficients.
    for key in ("force_n", "force_lbs"):
        assert rows[1][key] / rows[0][key] == pytest.approx(9 / 4, rel=1e-12, abs=0)


def assert_cantilever_oracle(row):
    """EI*v''=P*(L-x), v(0)=v'(0)=0 gives v(L)=P*L**3/(3*EI)."""
    assert row["tip_uy_in"] == pytest.approx(-100 / 87, rel=1e-10, abs=0)
    # Independent force and moment balance for a negative tip force.
    assert row["root_fy_lbf"] == pytest.approx(1000.0, rel=1e-10, abs=0)
    assert row["root_mz_lbf_in"] == pytest.approx(100000.0, rel=1e-10, abs=0)
    assert abs(row["root_mz_lbf_in"]) == pytest.approx(100000.0, rel=1e-10, abs=0)


def assert_numerical_oracles(data):
    assert_drag_oracle(data["drag"])
    assert_cantilever_oracle(data["cantilever"])
    assert data["inputs"] == {
        "drag": {"chute_diameter_ft": 5.0, "cd": 1.1, "cx": 1.25, "rho": 0.0023,
                 "speeds_mph": [40.0, 60.0]},
        "cantilever": {"length_in": 100.0, "E_psi": 29e6, "I_in4": 10.0,
                       "A_in2": 5.0, "tip_fy_lbf": -1000.0},
    }


def test_drag_report_matches_independent_si_force_and_speed_square_ratio(tmp_path):
    from dataclasses import asdict
    from digitalmodel.structural.parachute.parachute_drag import calculate_drag_force

    # Normal package API and report both face an independent oracle.
    rows = [asdict(calculate_drag_force(speed, 5.0, 1.1, 1.25, 0.0023))
            for speed in (40.0, 60.0)]
    assert_drag_oracle(rows)
    assert_drag_oracle(builder().calculate(tmp_path / "drag")["drag"])


def test_cantilever_report_matches_integrated_elastic_curve(tmp_path):
    from digitalmodel.structural.parachute.frame_solver import solve_frame

    result = solve_frame(
        nodes={0: (0.0, 0.0), 1: (100.0, 0.0)},
        members=[{"id": 0, "nodes": (0, 1), "section": {"A": 5.0, "I": 10.0}}],
        fixed_nodes=[0], forces={1: (0.0, -1000.0, 0.0)}, E=29e6,
    )
    assert_cantilever_oracle({"tip_uy_in": result.displacements[4],
                             "root_fy_lbf": result.reactions[0]["fy"],
                             "root_mz_lbf_in": result.reactions[0]["mz"]})
    assert_cantilever_oracle(builder().calculate(tmp_path / "frame")["cantilever"])


def test_report_values_have_independent_oracles():
    assert_numerical_oracles(json.loads((REPORT / "results.json").read_text()))


def test_production_replay_is_separate_from_oracles(tmp_path):
    """Exact replay establishes freshness only, without validating the method."""
    assert builder().calculate(tmp_path / "replay") == json.loads((REPORT / "results.json").read_text())


def build_delivery(path):
    output = path / "delivery"
    subprocess.run([sys.executable, str(SCRIPT), "--scratch", str(path / "scratch"),
                    "--output-dir", str(output)], cwd=ROOT, check=True,
                   capture_output=True, text=True)
    return output


def test_rebuild_is_byte_identical(tmp_path):
    first = build_delivery(tmp_path / "first")
    second = build_delivery(tmp_path / "second")
    assert {p.name for p in first.iterdir()} == ARTIFACTS
    assert {p.name for p in second.iterdir()} == ARTIFACTS
    for name in ARTIFACTS:
        assert (first / name).read_bytes() == (second / name).read_bytes()
        assert (first / name).read_bytes() == (REPORT / name).read_bytes(), name


def assert_manifest(directory):
    """Read exact bytes independently; no builder digest helper is used."""
    manifest = json.loads((directory / "manifest.json").read_text())
    assert set(manifest["artifacts"]) == ARTIFACTS - {"manifest.json"}
    for name, digest in manifest["artifacts"].items():
        assert hashlib.sha256((directory / name).read_bytes()).hexdigest() == digest, name
    assert manifest["generator"] == SCRIPT.relative_to(ROOT).as_posix()
    assert manifest["generator_sha256"] == hashlib.sha256(SCRIPT.read_bytes()).hexdigest()
    assert manifest["sources"]
    for name, digest in manifest["sources"].items():
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == digest, name
    assert manifest["reviewed_source_revision"] == "09089a121a16069c4b657d1a320a630a23ab1dd5"


def test_manifest_binds_delivered_bytes(tmp_path):
    assert_manifest(build_delivery(tmp_path / "fresh"))
    assert_manifest(REPORT)


def test_tampered_result_is_detected(tmp_path):
    output = build_delivery(tmp_path)
    data = json.loads((output / "results.json").read_text())
    data["cantilever"]["tip_uy_in"] *= -1
    (output / "results.json").write_text(json.dumps(data) + "\n")
    with pytest.raises(AssertionError, match="results.json"):
        assert_manifest(output)
    # Re-signing the changed bytes still cannot satisfy the independent equation.
    manifest = json.loads((output / "manifest.json").read_text())
    manifest["artifacts"]["results.json"] = hashlib.sha256((output / "results.json").read_bytes()).hexdigest()
    (output / "manifest.json").write_text(json.dumps(manifest) + "\n")
    assert_manifest(output)
    with pytest.raises(AssertionError):
        assert_numerical_oracles(data)


@pytest.mark.parametrize("case,key,factor", [
    ("drag", "force_n", 1.1), ("drag", "force_lbs", -1.0),
    ("cantilever", "tip_uy_in", -1.0), ("cantilever", "root_fy_lbf", -1.0),
    ("cantilever", "root_mz_lbf_in", 1.1),
])
def test_internally_consistent_wrong_calculation_fails(tmp_path, monkeypatch, case, key, factor):
    build = builder()
    original = build.calculate

    def wrong(scratch):
        data = copy.deepcopy(original(scratch))
        row = data[case][0] if case == "drag" else data[case]
        row[key] *= factor
        return data

    monkeypatch.setattr(build, "calculate", wrong)
    output = tmp_path / "changed"
    build.build(tmp_path / "scratch", output)
    # Builder, HTML and delivered JSON agree; digest replay alone would pass.
    data = json.loads((output / "results.json").read_text())
    assert embedded_data((output / "index.html").read_text()) == data
    assert_manifest(output)
    with pytest.raises(AssertionError):
        assert_numerical_oracles(data)


def embedded_data(page):
    return json.loads(re.search(r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S)[1])


class Links(HTMLParser):
    def __init__(self):
        super().__init__()
        self.urls = []
        self.ids = set()

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.add(attrs["id"])
        for key in ("href", "src"):
            if key in attrs:
                self.urls.append(attrs[key])
        if tag == "script":
            assert "src" not in attrs


def test_report_links_and_required_sections():
    page = (REPORT / "index.html").read_text()
    assert all(line == line.rstrip() for line in page.splitlines())
    assert "@import" not in page and "url(" not in page
    parser = Links()
    parser.feed(page)
    for url in parser.urls:
        parts = urlsplit(url)
        assert not parts.scheme and not parts.netloc, url
        if parts.path:
            assert (REPORT / unquote(parts.path)).is_file(), url
        elif parts.fragment:
            assert parts.fragment in parser.ids, url
    titles = re.findall(r'<h2 id="s\d+">(.*?)</h2>', page)
    assert titles == ["1. Summary", "2. Context and scope", "3. Input basis", "4. Method",
                      "5. Worked calculation", "6. Checks", "7. Results",
                      "8. Limits / NOT_EVALUATED", "9. Way forward", "10. References and evidence"]
    assert page.index("1. Summary") < page.index("Revision history") < page.index("2. Context and scope")
    changes = json.loads((REPORT / "changes.json").read_text())
    for change in changes:
        for value in change.values():
            assert value in page
    assert "changes-panel" in parser.ids
    assert "class=\"hs-doc\"" in page and "<svg" in page
    assert "NOT_EVALUATED" in page
    data = embedded_data(page)
    assert data == json.loads((REPORT / "results.json").read_text())
    # Display rounding must be linked to the actual unrounded values.
    for row in data["drag"]:
        for key in ("speed_mph", "speed_fps", "chute_area_ft2", "force_n", "force_lbs"):
            assert f">{row[key]:.1f}<" in page
    for value in data["cantilever"].values():
        assert f">{value:.1f}<" in page


def test_scratch_must_be_external(tmp_path):
    with pytest.raises(ValueError, match="outside"):
        builder().calculate(ROOT / "not-created")
    assert not (ROOT / "not-created").exists()
