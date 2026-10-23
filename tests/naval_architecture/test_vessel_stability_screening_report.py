# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Hand anchors and consistency checks for the stability module report."""

import importlib.util
import json
import re
from html.parser import HTMLParser
from pathlib import Path

import pytest

from digitalmodel.naval_architecture.vessel_stability_screening import gz_from_kn


ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/vesselstabilityscreening"
REBUILD = "PYTHONPATH=src python docs/reports/modules/vesselstabilityscreening/build_report.py --scratch <temporary-directory>"
spec = importlib.util.spec_from_file_location("stability_report", REPORT / "build_report.py")
builder = importlib.util.module_from_spec(spec)
spec.loader.exec_module(builder)


@pytest.fixture(scope="module")
def result(tmp_path_factory):
    return builder.calculate(tmp_path_factory.mktemp("report-calculations"))


def test_hand_loading_mass(result):
    # Conservation of mass: 4000 + 3000 + 1000 + 200 = 8200 t.
    # Absolute tolerance is 1e-9 t for exact integer input arithmetic.
    assert result["intact"]["displacement_t"] == pytest.approx(8200.0, abs=1e-9)


def test_hand_free_surface_rise(result):
    # Virtual rise = sum free-surface moments / mass = 410 / 8200 = 0.05 m.
    # Absolute tolerance is 1e-12 m for this rational calculation.
    assert result["intact"]["fsc_m"] == pytest.approx(0.05, abs=1e-12)


def test_hand_fluid_metacentric_height(result):
    # Box hydrostatics at T=4: KM = 4/2 + 20^2/(12*4) = 31/3 m.
    # Weight moment = 4000*5 + 3000*6 + 1000*2 + 200*1 = 40200 t m.
    # GM = 31/3 - 40200/8200 - 410/8200 = 5.380894308943089 m.
    # Absolute tolerance is 1e-9 m, tighter than the displayed 0.0001 m.
    assert result["intact"]["gm_fluid_m"] == pytest.approx(5.380894308943089, abs=1e-9)


def test_hand_published_righting_arms(result):
    # Course example 4.1: GZ30 = (15.02 - 23.84*1/2)*0.3048
    # = 3.10*0.3048 = 0.94488 m; GZ90 = (25.45-23.84)*0.3048
    # = 1.61*0.3048 = 0.490728 m. Absolute tolerance is 1e-9 m.
    values = dict(zip(result["public"]["heel_deg"], result["public"]["gz_m"]))
    assert values[30] == pytest.approx(0.94488, abs=1e-9)
    assert values[90] == pytest.approx(0.490728, abs=1e-9)


def test_consistency_public_curve_matches_direct_api(result):
    # Transcription anchor: Course Notes, Summer 2020, example 4.1, page 4-14.
    heels = [0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110]
    kn_ft = [0, 2.55, 5.08, 7.60, 10.10, 15.02, 19.67,
             22.96, 24.97, 26.04, 26.28, 25.45, 23.60, 20.95]
    actual = gz_from_kn(heels, [x * 0.3048 for x in kn_ft], 23.84 * 0.3048)
    assert result["public"]["heel_deg"] == heels
    assert result["public"]["kn_ft"] == kn_ft
    assert result["public"]["gz_m"] == pytest.approx(actual.gz_m, abs=1e-9)


class Bindings(HTMLParser):
    """Read result spans and reject unbound worked-example numerals."""

    def __init__(self):
        super().__init__()
        self.bound = None
        self.items = []
        self.example = False
        self.unbound = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "article":
            self.example = attrs.get("id") == "worked-example"
        if "data-value" in attrs:
            self.bound = attrs

    def handle_endtag(self, tag):
        if tag == "article":
            self.example = False
        if tag == "span":
            self.bound = None

    def handle_data(self, data):
        if self.bound:
            self.items.append((self.bound, data))
        elif self.example and re.search(r"\d", data):
            self.unbound.append(data)


def test_consistency_every_displayed_value(result):
    page = builder.render(result)
    parser = Bindings()
    parser.feed(page)
    assert not parser.unbound
    assert len(parser.items) > 100
    for attrs, text in parser.items:
        value = builder.lookup(result, attrs["data-value"])
        assert text == builder.format_value(value, int(attrs["data-digits"]))


def test_headline_from_displayed_inputs(result):
    # The public headline is recomputed from the displayed source inputs.
    # Input precision is unchanged: 15.02 ft, 23.84 ft and 0.3048 m/ft.
    public = result["public"]
    i = public["heel_deg"].index(30)
    kn = float(builder.format_value(public["kn_ft"][i], 2))
    kg = float(builder.format_value(public["kg_ft"], 2))
    factor = float(builder.format_value(public["m_per_ft"], 4))
    assert f"{(kn - kg * 0.5) * factor:.4f}" == f"{public['gz_m'][i]:.4f}"


def test_consistency_rebuild_and_source_freshness(result, tmp_path):
    page = builder.render(result)
    committed = (REPORT / "report.html").read_text(encoding="utf-8")
    # Embedded arrays are rounded; compare numerically with 0.000001 tolerance.
    pattern = r'(<script id="report-data" type="application/json">)(.*?)(</script>)'
    old = json.loads(re.search(pattern, committed, re.S)[2])
    fresh = json.loads(re.search(pattern, page, re.S)[2])

    def compare(a, b):
        if isinstance(a, dict):
            assert a.keys() == b.keys(), REBUILD
            for key in a:
                compare(a[key], b[key])
        elif isinstance(a, list):
            assert len(a) == len(b), REBUILD
            for x, y in zip(a, b):
                compare(x, y)
        elif isinstance(a, float):
            assert a == pytest.approx(b, abs=1e-6), REBUILD
        else:
            assert a == b, REBUILD

    compare(old, fresh)
    assert re.sub(pattern, "<calculation-data/>", page, flags=re.S) == re.sub(
        pattern, "<calculation-data/>", committed, flags=re.S
    ), REBUILD
    builder.build(tmp_path / "report.html", tmp_path / "calculation")
    assert (tmp_path / "report.html").read_text(encoding="utf-8") == page, REBUILD


def test_source_and_style_records():
    sources = json.loads((REPORT / "sources.json").read_text(encoding="utf-8"))
    assert all(item["http_status"] == 200 for item in sources["public_sources"])
    assert sources["public_example"]["kn_ft"][5] == 15.02
    assert sources["public_example"]["kg_ft"] == 23.84
    page = (REPORT / "report.html").read_text(encoding="utf-8")
    assert '<body class="hs-doc">' in page
    assert builder.house_style() in page
    assert 'src="http' not in page and 'rel="stylesheet"' not in page
    assert page.index("Revision history") < page.index("2. Introduction")
    assert len(re.findall(r"<h2\b", page)) == 10
    assert page.count("<h1>") == 1
    assert not re.search(r"<h[4-6]", page)
    ids = re.findall(r'\bid="([^"]+)"', page)
    assert len(ids) == len(set(ids))
    for anchor in re.findall(r'href="#([^"]+)"', page):
        assert anchor in ids


def test_comment_export_and_change_panel():
    page = (REPORT / "report.html").read_text(encoding="utf-8")
    assert "showSaveFilePicker" in page
    assert 'suggestedName: "report.comments.json"' in page
    assert "textContent" in page
    assert "JSON.stringify(comments" in page
    assert 'id="comment-import"' in page
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert changes == builder.CHANGES
    assert "report.comments.json" not in [p.name for p in REPORT.iterdir()]


def test_public_wording():
    page = (REPORT / "report.html").read_text(encoding="utf-8")
    assert "Draft for maintainer review" in page
    restricted = ("ow" + "ner", "fl" + "eet", "dis" + "patcher", "ses" + "sion", "la" + "ne")
    wording = r"\b(?:" + "|".join(restricted) + r")\b"
    for pattern in (r"/h" + "ome/", r"/m" + "nt/", wording, r"\b(?:could|should|would)\b"):
        assert not re.search(pattern, page, re.I)


def test_artifacts_are_unignored_and_scratch_is_external(tmp_path):
    import subprocess

    files = [str(p.relative_to(ROOT)) for p in REPORT.iterdir() if p.is_file()]
    check = subprocess.run(["git", "check-ignore", *files], cwd=ROOT, capture_output=True)
    assert check.returncode == 1, "Report deliverables must be unignored"
    with pytest.raises(ValueError, match="outside the repository"):
        builder.calculate(ROOT / "scratch")
