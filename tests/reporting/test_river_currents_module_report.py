# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic, delivery integrity and freshness are separate checks."""

import ast
import hashlib
import importlib.util
import json
import re
import subprocess
import sys
from html import escape
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_river_currents_module_report.py"
REPORT = ROOT / "docs/reports/modules/river_currents"
ARTIFACTS = {"index.html", "results.json", "changes.json", "README.md"}


def builder():
    """Load the report command without replacing package imports."""
    spec = importlib.util.spec_from_file_location("river_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def assert_hand_calculation(values):
    """Independent synthetic basis, with all lengths in m and speeds in m/s.

    Stipulate H=8, u_s=3, m=2: u=3*sqrt(z/8). At z=0,2,8 the
    answers are 0,3/2,3. Direct integration gives
    q=(3/sqrt(8))*(2/3)*8**(3/2)=16 m2/s and mean=16/8=2 m/s.
    Stipulate r=2/5. Integral of 1-r+6*r*x*(1-x) on [0,1] is
    1-r+6*r*(1/2-1/3)=1; bank=3/5 and centre=6/5.
    Stipulate width=20, beam=4, draft=2: A=160, occupied area=8,
    continuity factor=160/(160-8)=20/19. A fixed centre-plus-blockage
    correction gives quarter speed=(3/2)*(6/5)*(20/19)=36/19 and
    surface speed=3*(6/5)*(20/19)=72/19. These are mathematical
    assumptions, not default river coefficients or measured site data.

    Relative tolerance 1e-12 covers binary64 roundoff in this short chain;
    zero absolute tolerance prevents a small positive result passing as zero.
    It is a software arithmetic tolerance, not a hydraulic acceptance limit.
    """
    expected = {
        "quarter_speed_m_s": 3 / 2,
        "surface_speed_m_s": 3,
        "discharge_per_width_m2_s": 16,
        "mean_speed_m_s": 2,
        "left_bank_factor": 3 / 5,
        "right_bank_factor": 3 / 5,
        "centre_factor": 6 / 5,
        "lateral_mean_factor": 1,
        "remaining_area_factor": 20 / 19,
        "corrected_quarter_speed_m_s": 36 / 19,
        "corrected_surface_speed_m_s": 72 / 19,
    }
    assert set(values) == {"bed_speed_m_s", *expected}
    assert values["bed_speed_m_s"]["value"] == 0, "bed zero"
    for key, reference in expected.items():
        assert values[key]["value"] == pytest.approx(reference, rel=1e-12, abs=0), key


def test_profile_integral_and_channel_corrections_match_hand_calculation():
    """Compare normal public APIs directly with the derivation above."""
    from digitalmodel.hydrodynamics.river_currents import (
        blockage_factor, corrected_profile, lateral_factor,
        power_law_depth_averaged, power_law_profile,
    )

    profile = lambda z: power_law_profile(z, 8, 3, exponent_m=2)
    mean = power_law_depth_averaged(8, 3, exponent_m=2)
    left, centre, right = [lateral_factor(x, 20, 0.4) for x in (0, 10, 20)]
    factor = blockage_factor(20, 8, 4, 2)
    corrected = corrected_profile(profile, centre * factor)
    # Simpson weights integrate a quadratic exactly; they are not a fit.
    values = dict(zip((
        "bed_speed_m_s", "quarter_speed_m_s", "surface_speed_m_s",
        "discharge_per_width_m2_s", "mean_speed_m_s", "left_bank_factor",
        "right_bank_factor", "centre_factor", "lateral_mean_factor",
        "remaining_area_factor", "corrected_quarter_speed_m_s",
        "corrected_surface_speed_m_s",
    ), (profile(0), profile(2), profile(8), mean * 8, mean, left, right,
        centre, (left + 4 * centre + right) / 6, factor, corrected(2), corrected(8))))
    assert_hand_calculation({key: {"value": value} for key, value in values.items()})


def run_builder(directory):
    """Exercise the actual CLI with fresh external output and scratch paths."""
    output = directory / "output"
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(directory / "scratch"),
         "--output-dir", str(output)], cwd=ROOT, check=True, capture_output=True,
        text=True, encoding="utf-8",
    )
    return output


def payload(page):
    """Read the escaped machine payload without using the report renderer."""
    return json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S,
    )[1])


def assert_manifest(directory):
    """Read delivered and source bytes independently of the builder."""
    manifest = json.loads((directory / "manifest.json").read_text(encoding="utf-8"))
    assert set(manifest["artifacts"]) == ARTIFACTS
    assert manifest["reviewed_revision"] == "09089a121a16069c4b657d1a320a630a23ab1dd5"
    assert manifest["sources"]
    assert manifest["generator"] == SCRIPT.relative_to(ROOT).as_posix()
    assert manifest["generator_sha256"] == hashlib.sha256(SCRIPT.read_bytes()).hexdigest()
    for name, digest in manifest["artifacts"].items():
        assert hashlib.sha256((directory / name).read_bytes()).hexdigest() == digest, name
    for name, digest in manifest["sources"].items():
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == digest, name


def test_rebuild_is_byte_identical(tmp_path):
    """Freshness only: two external builds must equal every delivered byte."""
    first, second = run_builder(tmp_path / "first"), run_builder(tmp_path / "second")
    for folder in (first, second, REPORT):
        assert {p.name for p in folder.iterdir()} == ARTIFACTS | {"manifest.json"}
    for name in ARTIFACTS | {"manifest.json"}:
        assert (first / name).read_bytes() == (second / name).read_bytes()
        assert (first / name).read_bytes() == (REPORT / name).read_bytes(), name


def test_report_values_have_independent_oracles():
    """Numerical truth is checked without calling the report builder."""
    result = json.loads((REPORT / "results.json").read_text(encoding="utf-8"))
    # These stipulated inputs are the independent derivation's basis, not defaults.
    assert {key: row["value"] for key, row in result["inputs"].items()} == {
        "depth_m": 8, "surface_speed_m_s": 3, "exponent_m": 2,
        "retardation": 0.4, "width_m": 20, "beam_m": 4, "draft_m": 2,
    }
    assert_hand_calculation(result["results"])
    assert payload((REPORT / "index.html").read_text(encoding="utf-8")) == result


def test_production_replay_and_rendered_answers():
    """Replay checks wiring and presentation, separately from the hand oracle."""
    from digitalmodel.hydrodynamics.river_currents import river_velocity_profile

    data = json.loads((REPORT / "results.json").read_text(encoding="utf-8"))
    assert builder().calculate() == data
    page = (REPORT / "index.html").read_text(encoding="utf-8")
    for key, row in data["results"].items():
        assert (f'<td>{escape(key)}</td><td>{row["value"]:.1f}</td>'
                f'<td>{escape(row["unit"])}</td><td>{escape(row["method"])}</td>'
                f'<td>{escape(row["basis"])}</td>') in page
    inp = {key: row["value"] for key, row in data["inputs"].items()}
    heights, speeds = river_velocity_profile(
        len(data["profile"]["height_m"]), inp["depth_m"], model="power_law",
        surface_velocity=inp["surface_speed_m_s"], exponent_m=inp["exponent_m"],
    )
    assert data["profile"]["height_m"] == heights.tolist()
    assert data["profile"]["speed_m_s"] == speeds.tolist()


def test_manifest_binds_delivered_bytes():
    """Bind every artifact, reviewed source and generator to independent hashes."""
    assert_manifest(REPORT)


def test_tampered_result_is_detected(tmp_path):
    """An edited delivered answer must fail the unchanged byte manifest."""
    output = run_builder(tmp_path)
    damaged = json.loads((output / "results.json").read_text(encoding="utf-8"))
    damaged["results"]["mean_speed_m_s"]["value"] = -2  # Sign fault against the derivation.
    (output / "results.json").write_text(json.dumps(damaged), encoding="utf-8")
    with pytest.raises(AssertionError, match="results.json"):
        assert_manifest(output)


@pytest.mark.parametrize("fault", ["magnitude", "sign"])
def test_consistent_regeneration_cannot_hide_arithmetic_fault(tmp_path, monkeypatch, fault):
    """A shared bad API answer can pass hashes and replay but must fail the oracle.

    A tenth increase and a sign reversal are deliberate faults, not inputs or
    acceptance tolerances. Both differ from the hand-derived mean of 2 m/s.
    """
    import digitalmodel.hydrodynamics.river_currents as api

    original = api.power_law_depth_averaged
    multiplier = 1.1 if fault == "magnitude" else -1
    monkeypatch.setattr(api, "power_law_depth_averaged",
                        lambda *args, **kwargs: multiplier * original(*args, **kwargs))
    build = builder()
    build.build(tmp_path / "scratch", tmp_path / "output")
    result = json.loads((tmp_path / "output/results.json").read_text(encoding="utf-8"))
    assert payload((tmp_path / "output/index.html").read_text(encoding="utf-8")) == result
    assert_manifest(tmp_path / "output")
    assert build.calculate()["results"] == result["results"]  # Replay also agrees.
    with pytest.raises(AssertionError):
        assert_hand_calculation(result["results"])


class Document(HTMLParser):
    """Collect local links and section anchors without browser dependencies."""

    def __init__(self):
        super().__init__()
        self.ids = set()
        self.links = []
        self.remote_assets = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.add(attrs["id"])
        if "href" in attrs:
            self.links.append(attrs["href"])
        if "src" in attrs or tag == "link":
            self.remote_assets.append(attrs)


def test_report_links_and_required_sections():
    """Require the ten-section form, consistent history and resolvable evidence."""
    build = builder()
    page = (REPORT / "index.html").read_text(encoding="utf-8")
    doc = Document()
    doc.feed(page)
    assert not doc.remote_assets
    assert 'class="hs-doc"' in page and "<svg" in page
    assert len(re.findall(r'<h2 id="s\d+">', page)) == 10
    assert {f"s{i}" for i in range(1, 11)} <= doc.ids
    assert page.index('id="s1"') < page.index("Revision history") < page.index('id="s2"')
    assert "NOT_EVALUATED" in page and "changes-panel" in doc.ids
    history = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert history == build.CHANGES
    for row in history:
        assert all(value in page for value in row.values())
        assert all(value in (REPORT / "README.md").read_text(encoding="utf-8") for value in row.values())
    for link in doc.links:
        url = urlsplit(link)
        if url.scheme:
            assert url.scheme == "https"
            continue
        target = (REPORT / unquote(url.path)).resolve() if url.path else REPORT / "index.html"
        assert target.is_relative_to(ROOT) and target.is_file(), link
        if url.fragment:
            if target.suffix == ".py":
                assert re.fullmatch(r"L\d+", url.fragment)
                assert int(url.fragment[1:]) <= len(target.read_text(encoding="utf-8").splitlines())
            elif target.suffix == ".html":
                linked = Document()
                linked.feed(target.read_text(encoding="utf-8"))
                assert url.fragment in linked.ids, link
    # The source inventory must include every non-private function and fit method.
    expected = set()
    package = ROOT / "src/digitalmodel/hydrodynamics/river_currents"
    for source in package.glob("*.py"):
        tree = ast.parse(source.read_text(encoding="utf-8"))
        for node in tree.body:
            if isinstance(node, ast.FunctionDef) and not node.name.startswith("_"):
                expected.add(f"{source.stem}.{node.name}")
            if isinstance(node, ast.ClassDef):
                for method in node.body:
                    if isinstance(method, ast.FunctionDef) and not method.name.startswith("_"):
                        expected.add(f"{source.stem}.{node.name}.{method.name}")
    assert {row["function"] for row in build.INVENTORY} == expected
    for name in expected:
        assert name in page
    assert "09089a121a16069c4b657d1a320a630a23ab1dd5" in page


def test_scratch_must_be_external(tmp_path):
    """Reject calculation intermediates inside the checkout before writing."""
    with pytest.raises(ValueError, match="outside"):
        builder().build(ROOT / "scratch", tmp_path / "output")


def test_render_escapes_text_and_embedded_json():
    """An HTML delimiter in a basis label must remain text and round-trip JSON."""
    build = builder()
    data = build.calculate()
    marker = '</script><img src="invalid" onerror="invalid"> & text'
    data["results"]["mean_speed_m_s"]["basis"] = marker
    page = build.render(data)
    assert marker not in page
    assert escape(marker) in page
    assert payload(page)["results"]["mean_speed_m_s"]["basis"] == marker
    doc = Document()
    doc.feed(page)
    assert not doc.remote_assets


def test_documentation_pointers_resolve():
    """Both index surfaces must expose the delivered report and rebuild guide."""
    for source in (ROOT / "docs/README.md", ROOT / "src/digitalmodel/hydrodynamics/river_currents/README.md"):
        text = source.read_text(encoding="utf-8")
        paths = [match for match in re.findall(r'\]\(([^)]+)\)', text) if "reports/modules/river_currents/" in match]
        assert len(paths) == 2
        assert {(source.parent / path).resolve() for path in paths} == {
            REPORT / "index.html", REPORT / "README.md",
        }
        assert all((source.parent / path).is_file() for path in paths)
