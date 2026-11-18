# ABOUTME: Reproducibility and provenance checks for the connected-tank study.
"""Check the public study against its committed analytical sources."""

import importlib.util
import re
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit
from xml.etree import ElementTree

import pytest

ROOT = Path(__file__).resolve().parents[2]
BUILDER = ROOT / "scripts/capabilities/build_dual_tank_sloshing_study.py"
PAGE = ROOT / "docs/api/cfd/dual-tank-sloshing-cfd-study.html"
EXISTING = PAGE.with_name("sloshing-cfd-study.html")


class References(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.refs = []
        self.ids = []
        self.svgs = 0

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        self.refs.extend(attrs[key] for key in ("href", "src") if key in attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        self.svgs += tag == "svg"


def assert_references_resolve(text, page):
    parsed = References()
    parsed.feed(text)
    assert len(parsed.ids) == len(set(parsed.ids))
    for ref in parsed.refs:
        url = urlsplit(ref)
        if url.scheme or url.netloc:
            continue
        target = page.parent / unquote(url.path) if url.path else page
        assert target.is_file(), ref
        if url.fragment:
            other = References()
            other.feed(text if target == page else target.read_text(encoding="utf-8"))
            assert unquote(url.fragment) in other.ids, ref
    return parsed


@pytest.fixture(scope="module")
def builder():
    spec = importlib.util.spec_from_file_location("dual_study", BUILDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_builder_reproduces_utf8_html(builder, tmp_path):
    target = tmp_path / PAGE.name
    elapsed = builder.build(target)
    raw = target.read_bytes()
    text = raw.decode("utf-8", errors="strict")
    assert b"\r" not in raw
    assert text.startswith("<!DOCTYPE html>")
    assert text.endswith("</html>\n")
    assert elapsed < 60
    assert raw == PAGE.read_bytes()
    assert builder.render() == text
    style = re.search(r"<style>(.*?)</style>", text, re.S).group(1)
    assert style == re.search(
        r"<style>(.*?)</style>", EXISTING.read_text(encoding="utf-8"), re.S
    ).group(1)


def test_builder_does_not_require_generated_showcase(builder, monkeypatch, tmp_path):
    expected = builder.render()
    showcase = tmp_path / "showcase.html"
    monkeypatch.setattr(builder, "EXISTING", showcase)
    assert builder.render() == expected
    showcase.write_text("<html><style>", encoding="utf-8")
    assert builder.render() == expected


def test_fill_damping_ticks_are_distinct_and_match_table(builder):
    evidence = builder.render_cfd_evidence()
    fill = evidence.split('id="cfd-fill"', 1)[1].split('id="cfd-loss"', 1)[0]
    svg = ElementTree.fromstring(
        re.search(r'<svg id="cfd-fill-damping".*?</svg>', fill, re.S).group()
    )
    ticks = [node.text for node in svg.findall("text[@text-anchor='end']")]
    assert len(ticks) == len(set(ticks)) == 2
    assert ticks == ["0.514", "0.536"]
    table = ElementTree.fromstring(re.search(r"<table>.*?</table>", fill, re.S).group())
    damping_values = {row.findall("td/data")[-1].text for row in table.findall("tr")[1:]}
    assert set(ticks) <= damping_values


def test_area_damping_ticks_are_distinct_and_match_table(builder):
    evidence = builder.render_cfd_evidence()
    loss = evidence.split('id="cfd-loss"', 1)[1].split('id="cfd-forced"', 1)[0]
    svg = ElementTree.fromstring(
        re.search(r'<svg id="cfd-loss-area".*?</svg>', loss, re.S).group()
    )
    ticks = [node.text for node in svg.findall("text[@text-anchor='end']")]
    table = ElementTree.fromstring(re.search(r"<table>.*?</table>", loss, re.S).group())
    damping = [cell for row in table.findall("tr")[1:]
               for cell in row.findall("td/data")[-2:]]
    expected = sorted({f"{float(cell.attrib['value']):.3f}" for cell in damping})
    assert len(ticks) == len(set(ticks)) == len(expected)
    assert ticks == expected
    assert set(ticks) == {cell.text for cell in damping}


def test_claims_are_read_from_published_sources(builder):
    claims = builder.published_claims()
    assert claims["moment_rms"] == 4.3
    assert claims["moment_min_period"] == 20
    assert claims["degrades_below"] == 16
    assert claims["length_inflation"] == 2.76
    assert claims["cases"] == 35
    assert claims["area_exponent"] == builder.MODEL.DEFAULT_AREA_EXPONENT
    assert claims["loss_rms"] == builder.MODEL.CALIBRATED_AREA_EXPONENT_RMS_PERCENT
    assert claims["linear_rms"] == builder.MODEL.LINEAR_AREA_EXPONENT_RMS_PERCENT
    source = (ROOT / "tests/hydrodynamics/test_utube_tank.py").read_text()
    for key in ("area_exponent", "loss_rms", "linear_rms"):
        assert str(claims[key]) in source
    text = builder.render()
    for value in claims.values():
        assert str(value) in text
    assert "CAUSE IS NOT ESTABLISHED" in text
    assert "An earlier explanation was withdrawn." in text
    assert "residual also depends on roll amplitude" in text
    assert "±" in text and "exception" in text
    assert "does not qualify the invented illustrative geometry" in text


def test_curves_call_library_and_reduction(builder, monkeypatch):
    calls = {}
    for name in ("natural_period", "equivalent_damping", "tank_moment",
                 "coupled_roll_response", "_prismatic_properties"):
        original = getattr(builder.MODEL, name)

        def counted(*args, _name=name, _original=original, **kwargs):
            calls[_name] = calls.get(_name, 0) + 1
            return _original(*args, **kwargs)

        monkeypatch.setattr(builder.MODEL, name, counted)
    data = builder.compute()
    assert all(calls.values()) and len(calls) == 5
    assert data["mu"] == 0.05
    assert [curve["peaks"] for curve in data["absorbers"]] == [2, 2, 2, 1, 1]
    for curve in data["absorbers"]:
        assert curve["ys"] == pytest.approx(curve["published"], abs=0.000051)
    previous, highest = data["absorbers"][-2:]
    assert highest["zeta"] == max(curve["zeta"] for curve in data["absorbers"])
    assert highest["zeta"] > previous["zeta"]
    assert max(highest["ys"]) > max(previous["ys"])
    assert min(data["areas"]) <= data["area"] <= max(data["areas"])
    assert builder.FROZEN_VOLUME_INERTIA_INDEX == 2
    _, _, volume_inertia, _ = builder.MODEL._prismatic_properties(
        data["geometry"], data["areas"][0]
    )
    assert data["inertia"][0] == pytest.approx(
        builder.MODEL.DEFAULT_DENSITY * volume_inertia
    )
    assert all(value >= 0 for value in data["periods"])


def test_illustrative_geometry_is_independent(builder, monkeypatch):
    reference = builder.test_inputs()
    data = builder.compute()
    geometry = data["geometry"]
    assert (geometry.leg_length, geometry.leg_width, geometry.fill_depth,
            geometry.centroid_offset, data["area"]) == (30.0, 8.0, 4.0, 10.0, 8.0)
    constants = reference["constants"]
    assert geometry.leg_area != constants["LEG_LENGTH"] * constants["LEG_WIDTH"]
    assert {geometry.leg_length, geometry.leg_width, geometry.centroid_offset}.isdisjoint(
        {constants["LEG_LENGTH"], constants["LEG_WIDTH"], constants["CENTROID_OFFSET"]}
    )
    assert data["area"] != geometry.fill_depth
    changed = {**reference, "constants": {key: value + 100 for key, value in constants.items()}}
    monkeypatch.setattr(builder, "test_inputs", lambda: changed)
    recomputed = builder.compute()
    for key in ("geometry", "area", "natural", "inertia", "damping", "moments", "responses"):
        assert recomputed[key] == data[key]


def test_related_screening_links(builder):
    screening = "generic-hull-roll-screening.html"
    study = PAGE.name
    text = builder.render()
    assert_references_resolve(text, PAGE)
    summary = References()
    summary.feed(text.split('id="summary"', 1)[1].split('id="revisions"', 1)[0])
    assert screening in summary.refs
    index = PAGE.with_name("index.html")
    refs = assert_references_resolve(index.read_text(encoding="utf-8"), index).refs
    assert refs[refs.index(study) + 1] == screening
    screening_page = PAGE.with_name(screening)
    assert study in assert_references_resolve(
        screening_page.read_text(encoding="utf-8"), screening_page
    ).refs


def test_output_structure_links_and_privacy(builder):
    text = builder.render()
    parsed = assert_references_resolve(text, PAGE)
    assert parsed.svgs == 14
    svgs = re.findall(r"<svg\b.*?</svg>", text, re.S)
    assert len(svgs) == 14
    for svg in svgs:
        root = ElementTree.fromstring(svg)
        assert root.find("title").text
        assert root.find("desc").text
        assert root.findall("polyline")
    assert text.count("Shaded periods are outside") == 2
    assert "<script" not in text
    assert "<img" not in text
    assert "Way forward" in text and "Not done" in text
    assert "Revision history" in text
    assert text.index("Summary") < text.index("Revision history") < text.index("Illustrative geometry")
    forbidden = ["/" + part + "/" for part in ("home", "mnt")]
    assert all(value not in text for value in forbidden)
    assert not re.search(r"[\w.+-]+@[\w.-]+\.[A-Za-z]{2,}", text)
    assert not re.search(r"[A-Za-z]:\\Users\\", text, re.I)
    existing = EXISTING.read_text(encoding="utf-8")
    section = existing.split("Coupled anti-roll response", 1)[1].split("<h2>", 1)[0]
    assert f'href="{PAGE.name}"' in section
    assert_references_resolve(text, PAGE)


def test_public_study_files_have_no_private_identifiers():
    paths = (
        PAGE,
        PAGE.parent / "data/dual_tank_cfd_evidence.json",
        BUILDER,
        ROOT / "scripts/capabilities/extract_dual_tank_cfd_evidence.py",
        Path(__file__),
        Path(__file__).with_name("test_dual_tank_cfd_evidence.py"),
    )
    patterns = (
        "d" + "m" + "1528",
        "/" + "mnt" + "/",
        "/" + "home" + "/",
        "B" + "15" + r"\d{2}",
        "gpu" + "-claw",
        "ace" + "-linux",
        "spa" + "rk",
        "rds" + "02",
    )
    counts = []
    for path in paths:
        text = path.read_text(encoding="utf-8")
        counts.append([len(re.findall(pattern, text, re.I)) for pattern in patterns])
    if any(any(row) for row in counts):
        pytest.fail(f"Prohibited identifier counts: {counts}", pytrace=False)


def assert_public_wording(text):
    process_words = (
        r"\b(?:own" r"er|la" r"ne|ag" r"ent|jobs?|runs?|pr|sha|"
        r"peer[- ]review|dispatch" r"er|fle" r"et|committed|builder|"
        r"regenerates|reproducibility)\b"
    )
    forbidden = ["/" + part + "/" for part in ("home", "mnt")]
    assert all(value not in text for value in forbidden)
    assert not re.search(r"[\w.+-]+@[\w.-]+\.[A-Za-z]{2,}", text)
    assert not re.search(r"[A-Za-z]:\\Users\\", text, re.I)
    assert not re.search(process_words, text, re.I)
    assert not re.search(r"</?(?:b|strong)\b", text, re.I)
    assert not re.search(r"\b(?:could|should|would)\b", text, re.I)


def test_built_html_uses_public_engineering_wording(builder, tmp_path):
    target = tmp_path / PAGE.name
    builder.build(target)
    assert_public_wording(target.read_text(encoding="utf-8"))


@pytest.mark.parametrize("location", ["<p>{}</p>", '<meta content="{}">',
                                     "<svg><title>{}</title></svg>"])
@pytest.mark.parametrize("forbidden", [
    "own" + "er", "la" + "ne", "ag" + "ent", "job", "run", "PR", "sha",
    "peer-review", "committed", "builder", "regenerates", "reproducibility",
    "/" + "home" + "/example", "/" + "mnt" + "/example",
    "example" + "@" + "example.invalid", "C:" + "\\Users\\example",
])
def test_public_wording_scan_rejects_disclosure_and_process_text(location, forbidden):
    with pytest.raises(AssertionError):
        assert_public_wording(location.format(forbidden))
