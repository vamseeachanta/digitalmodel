# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the spectral fatigue report and verify its numerical bindings."""

import copy
import csv
import importlib.util
import json
import math
import re
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/spectralfatigue"


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location("spectral_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def record(builder, tmp_path):
    return builder.calculate(tmp_path / "calculation")


def test_rebuild_is_current_and_deterministic(builder, tmp_path):
    first = tmp_path / "first"
    second = tmp_path / "second"
    builder.build(first, tmp_path / "calc-first")
    builder.build(second, tmp_path / "calc-second")
    for name in ("report.html", "changes.json"):
        assert b"\r" not in (first / name).read_bytes(), "Generated text must use LF"
        assert b"\r" not in (REPORT / name).read_bytes(), "Stored text must use LF"
    message = ("Rebuild with PYTHONPATH=src python "
               "docs/reports/modules/spectralfatigue/build_report.py "
               '--work-dir "$TMPDIR/spectral-calculation"')
    for other in (second, REPORT):
        actual = (first / "report.html").read_text(encoding="utf-8")
        expected = (other / "report.html").read_text(encoding="utf-8")
        for script_id in ("report-data", "chart-data"):
            pattern = rf'(<script id="{script_id}" type="application/json">)(.*?)(</script>)'
            assert_records_close(json.loads(re.search(pattern, actual, re.S)[2]),
                                 json.loads(re.search(pattern, expected, re.S)[2]))
            actual = re.sub(pattern, r"\1NUMERIC_RECORD\3", actual, flags=re.S)
            expected = re.sub(pattern, r"\1NUMERIC_RECORD\3", expected, flags=re.S)
        values = r'data-value="([^"]+)"'
        assert [float(v) for v in re.findall(values, actual)] == pytest.approx(
            [float(v) for v in re.findall(values, expected)], rel=1e-12, abs=0.0
        ), message
        assert re.sub(values, 'data-value="NUMBER"', actual) == re.sub(
            values, 'data-value="NUMBER"', expected
        ), message
        assert json.loads((first / "changes.json").read_text(encoding="utf-8")) == json.loads(
            (other / "changes.json").read_text(encoding="utf-8")
        ), message


def assert_records_close(actual, expected):
    """Compare rebuilt numeric records without requiring identical float text."""
    if isinstance(expected, dict):
        assert actual.keys() == expected.keys()
        for key in expected:
            assert_records_close(actual[key], expected[key])
    elif isinstance(expected, list):
        assert len(actual) == len(expected)
        for left, right in zip(actual, expected):
            assert_records_close(left, right)
    elif isinstance(expected, (int, float)) and not isinstance(expected, bool):
        assert actual == pytest.approx(expected, rel=1e-12, abs=0.0)
    elif isinstance(expected, str) and re.fullmatch(r"[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?", expected):
        # CSV fields in the embedded record are numeric strings.
        assert float(actual) == pytest.approx(float(expected), rel=1e-12, abs=0.0)
    else:
        assert actual == expected


def test_public_example_is_used_without_numeric_overrides(record, tmp_path):
    from digitalmodel.spectral_fatigue.workflow import router

    source = yaml.safe_load((ROOT / "examples/workflows/spectral-fatigue/input.yml").read_text(encoding="utf-8"))
    assert record["input"] == source["spectral_fatigue"]
    source["spectral_fatigue"]["output_dir"] = str(tmp_path / "direct")
    actual = router(copy.deepcopy(source))["spectral_fatigue"]
    assert record["summary"] == actual["locations"]
    assert record["governing_location"] == actual["governing_location"]
    assert record["screening_status"] == actual["screening_status"]
    with Path(actual["results_csv"]).open(encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream))
    assert record["sea_states"] == rows


@pytest.mark.parametrize("index", [0, 1, 2])
def test_each_state_matches_library(record, index):
    from digitalmodel.fatigue.spectral_fatigue import compute_spectral_moments, dirlik_damage

    state = record["states"][index]
    location = record["input"]["locations"][state["location_index"]]
    inputs = location["sea_states"][state["state_index"]]
    moments = compute_spectral_moments(inputs["frequency_Hz"], inputs["stress_psd_MPa2_Hz"])
    curve = record["input"]["sn_curve"]
    result = dirlik_damage(moments, curve["slope"], curve["log_intercept"])
    assert state["moments"] == moments.model_dump()
    assert state["result"] == result.model_dump()
    assert state["rms"] == moments.rms
    assert state["alpha2"] == moments.irregularity_factor
    assert state["epsilon"] == moments.bandwidth_parameter
    assert state["peak_rate"] == moments.nu_p
    assert state["crossing_rate"] == moments.nu_0


def test_frequency_convention_audit_has_independent_reference(record):
    for state in record["states"]:
        m = state["moments"]
        assert state["hertz_peak_rate"] == pytest.approx(math.sqrt(m["m4"] / m["m2"]))
        assert state["peak_difference_percent"] == pytest.approx(
            100 * (state["peak_rate"] / state["hertz_peak_rate"] - 1)
        )
        assert state["peak_difference_percent"] < 0
        assert state["alpha2"] ** 2 + state["epsilon"] ** 2 == pytest.approx(1)


def test_reported_moments_match_hand_trapezoids(record):
    # Metals 11 (2021), 1333, section 2, eqs. (1)-(2), DOI:10.3390/met11091333.
    # On the public Hz grid: sum((f[j+1]-f[j])*(f[j]**n*S[j]
    # + f[j+1]**n*S[j+1])/2). For m0 the seven areas are
    # 3.5 + 15 + 37 + 40 + 38 + 19.2 + 7.2 = 159.9 MPa^2.
    # Applying n=1,2,4 gives the other columns below; no estimator is called.
    expected = [
        (159.9, 19.164, 2.61936, 0.083452416),
        (448.0, 53.584, 7.31616, 0.235487616),
        (31.98, 3.8328, 0.523872, 0.0166904832),
    ]
    for state, values in zip(record["states"], expected):
        assert [state["moments"][key] for key in ("m0", "m1", "m2", "m4")] == pytest.approx(values)


def hand_state(inputs, log_intercept, reference):
    """Scalar hand calculation: Metals 11 (2021), 1333, sections 2-3, eqs. (1)-(9)."""
    # DOI:10.3390/met11091333. This oracle is restricted to the public m=3 case.
    # The input trapezoids are accumulated without the numerical library.
    f, spectrum = inputs["frequency_Hz"], inputs["stress_psd_MPa2_Hz"]
    m0, m1, m2, m4 = [math.fsum(
        (f[j+1] - f[j]) * (f[j]**n * spectrum[j] + f[j+1]**n * spectrum[j+1]) / 2
        for j in range(len(f) - 1)) for n in (0, 1, 2, 4)]
    alpha = m2 / math.sqrt(m0 * m4)
    x = m1 / m0 * math.sqrt(m2 / m4)
    c = alpha if reference else math.sqrt(1 - alpha**2)
    d1 = 2 * (x - c**2) / (1 + c**2)
    denominator = 1 - c - d1 + d1**2
    radius = (c - x - d1**2) / denominator
    d2 = denominator / (1 - radius)
    d3 = 1 - d1 - d2
    q = 1.25 * (c - d3 - d2 * radius) / d1
    assert min(d1, d2, d3, q) > 0, "The public-case oracle requires no other parameter clips"
    # The implementation clips R after Q is formed; the open reference does not.
    used_radius = radius if reference else max(radius, 1e-12)
    # Gamma(4)=6 and Gamma(5/2)=3*sqrt(pi)/4; ranges are twice amplitudes.
    mean_range_cubed = (2 * math.sqrt(m0))**3 * (
        d1 * q**3 * 6 + 2**1.5 * (3 * math.sqrt(math.pi) / 4)
        * (d2 * abs(used_radius)**3 + d3))
    peak = math.sqrt(m4 / m2) / (1 if reference else 2 * math.pi)
    # 365.25*24*3600 = 31557600 s/year is the reproduced calendar convention.
    annual = peak * mean_range_cubed / 10**log_intercept * 31557600
    return {"annual": annual, "coefficients": [d1, d2, d3, radius, q]}


def test_occurrence_weighting_and_life_match_hand_reproduction(record):
    # Hand reproduction of the CURRENT implementation, not reference damage.
    # For the mild state: alpha2=2.61936/sqrt(159.9*0.083452416),
    # epsilon=sqrt(1-alpha2^2), x=(19.164/159.9)*sqrt(2.61936/0.083452416).
    # Substituting epsilon into eq. (8), Metals 11 (2021), 1333, section 3,
    # gives D1=0.249851424297, D2=0.0876097442180, D3=0.662538831485,
    # Q=0.312314280371 and R=-0.318986597067, clipped by the library to 1e-12.
    # With m=3, E[range^3]=(2*sqrt(159.9))^3 *
    # [D1*Q^3*6 + 2^1.5*(3*sqrt(pi)/4)*(D2*R^3+D3)].
    # Annual damage = E[range^3]*sqrt(m4/m2)/(2*pi)*31557600/10^12.164.
    # The year is the module's 365.25*24*3600 seconds; it is a calendar choice.
    annual = [0.02521684081471794, 0.11861043372296305, 0.0022554628095800187]
    reproduced = []
    for state in record["states"]:
        inputs = record["input"]["locations"][state["location_index"]]["sea_states"][state["state_index"]]
        hand = hand_state(inputs, record["input"]["sn_curve"]["log_intercept"], reference=False)
        reproduced.append(hand["annual"])
    assert reproduced == pytest.approx(annual, rel=1e-11)
    mild_inputs = record["input"]["locations"][0]["sea_states"][0]
    assert hand_state(mild_inputs, 12.164, reference=False)["coefficients"] == pytest.approx(
        [0.249851424297, 0.0876097442180, 0.662538831485, -0.318986597067, 0.312314280371]
    )
    assert [state["result"]["damage_per_year"] for state in record["states"]] == pytest.approx(reproduced)
    # Splash zone: 0.85*0.02521684081471794 + 0.15*0.11861043372296305
    # = 0.03922587975095470/year; life=1/damage=25.493373414414275 years.
    # Keel: 1.0*0.0022554628095800187/year; life=443.367984500798 years.
    assert [row["annual_damage"] for row in record["summary"]] == pytest.approx(
        [0.03922587975095470, 0.0022554628095800187]
    )
    assert [row["fatigue_life_years"] for row in record["summary"]] == pytest.approx(
        [1 / (0.85 * reproduced[0] + 0.15 * reproduced[1]), 1 / reproduced[2]]
    )
    assert [float(row["damage_per_year_weighted"]) for row in record["sea_states"]] == pytest.approx(
        [0.02143431469251025, 0.01779156505844446, 0.0022554628095800187]
    )


def test_reference_damage_is_distinct_from_reported_reproduction(record):
    # Independent hand reference: Metals 11 (2021), 1333, section 2 eqs. (1)-(5)
    # and section 3 eqs. (6)-(9), DOI:10.3390/met11091333. The range form
    # follows from r=2*s and K=A/2^m. No library/router supplies expectations.
    # Mild-state Hz moments above give alpha2=0.717053404507,
    # x=0.671453337525, D1=2*(x-alpha2^2)/(1+alpha2^2)=0.207755022538,
    # D2=0.120842913723, D3=0.671402063739, R=0.0205985713319,
    # Q=0.259693778173; nu_p=sqrt(0.083452416/2.61936)=0.178493276440 Hz.
    # Annual reference damage uses the same expression as above but these
    # coefficients and the Hz rate (no 2*pi division). Repeat for each state:
    reference_annual = [0.15903501451685098, 0.7448674450802898, 0.014224524130493781]
    mild = record["states"][0]
    assert mild["hertz_peak_rate"] == pytest.approx(0.17849327643999993)
    assert mild["peak_rate"] == pytest.approx(0.028408087254094133)
    assert mild["peak_difference_percent"] == pytest.approx(-84.08450569081046)
    moments = mild["moments"]
    x = moments["m1"] / moments["m0"] * math.sqrt(moments["m2"] / moments["m4"])
    assert 2 * (x - mild["alpha2"]**2) / (1 + mild["alpha2"]**2) == pytest.approx(0.2077550225380668)
    assert 2 * (x - mild["epsilon"]**2) / (1 + mild["epsilon"]**2) == pytest.approx(0.2498514242971356)
    reference = []
    for state in record["states"]:
        inputs = record["input"]["locations"][state["location_index"]]["sea_states"][state["state_index"]]
        reference.append(hand_state(inputs, record["input"]["sn_curve"]["log_intercept"], reference=True))
    assert [row["annual"] for row in reference] == pytest.approx(reference_annual, rel=1e-11)
    assert reference[0]["coefficients"] == pytest.approx(
        [0.207755022538, 0.120842913723, 0.671402063739, 0.0205985713319, 0.259693778173]
    )
    # Undo only the rate division to isolate the effect of the coefficient
    # substitution and R clipping: 0.0252168408147*2*pi/0.159035014517.
    assert mild["result"]["damage_per_year"] * (2 * math.pi) / reference[0]["annual"] == pytest.approx(0.996271696405)
    # Reference weighting: 0.85*0.15903501451685098 + 0.15*0.7448674450802898
    # =0.2469098791013668/year; inverse=4.050060708949836 years, not 25.5.
    assert 1 / (0.85 * reference[0]["annual"] + 0.15 * reference[1]["annual"]) == pytest.approx(4.050060708949836)
    assert 1 / reference[2]["annual"] == pytest.approx(70.3011215578209)
    assert 1 / reference[2]["annual"] < record["summary"][1]["required_life_years"]
    assert 1 / (0.85 * reference[0]["annual"] + 0.15 * reference[1]["annual"]) < record["summary"][0]["required_life_years"]
    assert record["summary"][0]["fatigue_life_years"] != pytest.approx(4.050060708949836)


class Page(HTMLParser):
    """Collect numeric bindings, scripts, headings and external resources."""

    def __init__(self):
        super().__init__()
        self.bindings = []
        self.attrs = None
        self.bound_text = ""
        self.scripts = {}
        self.script_id = None
        self.resources = []
        self.headings = []
        self.ids = []
        self.links = []
        self.body_class = None
        self.worked = False
        self.unbound = []
        self.hidden = False
        self.in_heading = False

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if "id" in a:
            self.ids.append(a["id"])
        if tag == "body":
            self.body_class = a.get("class")
        if tag == "section":
            self.worked = a.get("id") == "detailed-results"
        if "data-number" in a:
            self.attrs, self.bound_text = a, ""
        if tag == "script":
            self.script_id = a.get("id")
            self.hidden = True
        if tag == "style":
            self.hidden = True
        if tag in {"h1", "h2", "h3", "h4", "h5", "h6"}:
            self.headings.append(tag)
            self.in_heading = True
        if tag in {"script", "img", "link", "iframe"} and (a.get("src") or a.get("href")):
            self.resources.append(a)
        if tag == "a" and a.get("href", "").startswith("#"):
            self.links.append(a["href"][1:])

    def handle_data(self, data):
        if self.attrs is not None:
            self.bound_text += data
        elif self.worked and not self.hidden and not self.in_heading and re.search(r"\d", data):
            self.unbound.append(data)
        if self.script_id:
            self.scripts[self.script_id] = self.scripts.get(self.script_id, "") + data

    def handle_endtag(self, tag):
        if tag == "span" and self.attrs is not None:
            self.bindings.append((self.attrs, self.bound_text))
            self.attrs = None
        if tag in {"script", "style"}:
            self.script_id = None
            self.hidden = False
        if tag == "section":
            self.worked = False
        if tag in {"h1", "h2", "h3"}:
            self.in_heading = False


def test_all_displayed_example_numbers_are_bound_to_module(record, builder):
    page = Page()
    page.feed((REPORT / "report.html").read_text(encoding="utf-8"))
    assert len(page.bindings) > 50
    assert not page.unbound
    for attrs, visible in page.bindings:
        value = builder.lookup(record, attrs["data-number"])
        assert float(attrs["data-value"]) == pytest.approx(value, rel=1e-12, abs=0.0)
        assert visible == builder.format_number(value, attrs["data-format"])
    assert_records_close(json.loads(page.scripts["report-data"]), record)


def test_chart_points_and_limits_are_returned_values(record):
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    data = json.loads(re.search(r'<script id="chart-data" type="application/json">(.*?)</script>', text, re.S)[1])
    assert len(data) == len(record["summary"])
    for series, summary in zip(data, record["summary"]):
        assert series["limit"] == summary["required_life_years"]
        assert series["points"][-1] == [record["input"]["design_life_years"], summary["fatigue_life_years"]]
        for year, life in series["points"]:
            assert life == summary["fatigue_life_years"]
        assert series["limit_points"][-1][1] == summary["required_life_years"]
    assert "e.setAttribute('data-limit','required-life')" in text
    assert "tabindex:0" in text


def test_house_style_structure_and_links(builder):
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    page = Page()
    page.feed(text)
    assert page.body_class == "hs-doc"
    assert not page.resources
    assert not set(page.headings) & {"h4", "h5", "h6"}
    assert len(re.findall(r'<section id="(?:executive-summary|introduction|design-data|methodology|detailed-results|findings|limitations|recommendations|references|acronyms)"', text)) == 10
    assert set(page.links) <= set(page.ids)
    assert len(page.ids) == len(set(page.ids))
    assert builder.house_style() in text
    assert text.index('id="revision-history"') < text.index('id="introduction"')
    assert "<strong>" not in text and "<b>" not in text


def test_change_panel_and_comment_persistence_contract():
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    page = Page()
    page.feed(text)
    assert json.loads(page.scripts["change-data"]) == changes
    assert [row["revision"] for row in changes["revisions"]] == ["1", "0"]
    assert set(re.findall(r"<tr><td>(\d+)</td><td>(\d{4}-\d{2}-\d{2})</td>", text)) == {
        (row["revision"], row["date"]) for row in changes["revisions"]
    }
    assert "showDirectoryPicker" in text
    assert "createWritable" in text
    assert "report.comments.json" in text
    assert "textContent" in text


def test_visible_wording_and_required_registers():
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    visible = re.sub(r"<(script|style)\b.*?</\1>", "", text, flags=re.S)
    visible = re.sub(r"<[^>]+>", " ", visible)
    for word in ("could", "should", "would", "certified", "compliant"):
        assert not re.search(rf"\b{word}\b", visible, re.I)
    for required in ("Draft for maintainer review", "ASSUMED - to be confirmed", "NOT_EVALUATED", "Data register", "QA pre-flight", "Not covered"):
        assert required in visible


@pytest.mark.parametrize("scenario", ["core", "preserve", "invalid", "unreadable", "fallback", "wrong-folder", "wrong-revision"])
def test_chart_interaction_and_comments_saved_beside_report(tmp_path, scenario):
    import os
    import shutil
    import subprocess

    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the offline interaction check")
    env = dict(os.environ)
    for key in ("UV_TOOL_DIR", "PRE_COMMIT_HOME", "XDG_CACHE_HOME", "npm_config_cache"):
        folder = tmp_path / key.lower()
        folder.mkdir()
        env[key] = str(folder)
    result = subprocess.run(
        [node, str(Path(__file__).with_name("spectralfatigue_interactions.cjs")), str(REPORT / "report.html"), scenario],
        cwd=tmp_path, env=env, capture_output=True, text=True, encoding="utf-8", check=False,
    )
    assert result.returncode == 0, result.stderr
