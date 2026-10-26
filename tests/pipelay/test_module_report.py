# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check rebuilt report text, numerical bindings and standalone controls."""

import copy
import importlib.util
import json
import math
import os
import re
import shutil
import subprocess
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/pipelay"


def git_env():
    """Keep fixture Git commands bound to their explicit working directory."""
    return {key: value for key, value in os.environ.items()
            if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}


@pytest.fixture
def ignore_repo(tmp_path):
    """Exercise repository ignore rules without global or shared exclusions."""
    repo = tmp_path / "ignore-repository"
    repo.mkdir()
    subprocess.run(["git", "init", "-q"], cwd=repo, env=git_env(), check=True)
    (repo / ".gitignore").write_text((ROOT / ".gitignore").read_text(encoding="utf-8"),
                                    encoding="utf-8", newline="\n")
    for name in ("build_report.py", "report.html", "changes.json"):
        path = repo / "docs/reports/modules/pipelay" / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("fixture\n", encoding="utf-8", newline="\n")
        subprocess.run(["git", "add", "-f", path.relative_to(repo).as_posix()],
                       cwd=repo, env=git_env(), check=True)
    return repo


@pytest.fixture
def builder():
    spec = importlib.util.spec_from_file_location("pipelay_module_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class Page(HTMLParser):
    """Inspect the built artifact rather than its template alone."""

    def __init__(self):
        super().__init__()
        self.ids = []
        self.links = []
        self.headings = []
        self.bindings = []
        self.unbound_worked_numbers = []
        self.hidden = 0
        self.bound = False
        self.worked = False
        self.active = None
        self.resources = []
        self.visible = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag == "h2":
            self.headings.append(attrs.get("id"))
        if tag == "section" and attrs.get("id") == "worked-example":
            self.worked = True
        if tag in {"script", "style"}:
            self.hidden += 1
        if "data-value" in attrs:
            self.bound = True
            self.active = [attrs, ""]
        if tag in {"script", "link", "img", "iframe"}:
            self.resources.extend(attrs[k] for k in ("src", "href") if k in attrs)
        self.visible.extend(attrs[key] for key in ("title", "aria-label", "alt", "placeholder") if key in attrs)

    def handle_endtag(self, tag):
        if tag in {"span", "tspan"} and self.bound:
            self.bindings.append(self.active)
            self.active = None
            self.bound = False
        if tag in {"script", "style"}:
            self.hidden -= 1
        if tag == "section":
            self.worked = False

    def handle_data(self, data):
        if self.active:
            self.active[1] += data
        if not self.hidden:
            self.visible.append(data)
            if self.worked and not self.bound and re.search(r"\d", data):
                self.unbound_worked_numbers.append(data)


def assert_record_agreement(actual, expected):
    """Compare numerical records at tolerance rather than serialized precision."""
    if isinstance(expected, dict):
        assert actual.keys() == expected.keys()
        for key in expected:
            assert_record_agreement(actual[key], expected[key])
    elif isinstance(expected, list):
        assert len(actual) == len(expected)
        for a, b in zip(actual, expected):
            assert_record_agreement(a, b)
    elif isinstance(expected, (int, float)) and not isinstance(expected, bool):
        assert actual == pytest.approx(expected, rel=1e-12, abs=1e-12)
    else:
        assert actual == expected


def test_report_rebuild_matches_presentation_and_records(builder, tmp_path):
    """Check normalized presentation, exact source digests and numeric agreement."""
    builder.build(tmp_path / "report", tmp_path / "calculations")
    for name in ("report.html", "changes.json"):
        rebuilt = (tmp_path / "report" / name).read_text(encoding="utf-8")
        stored = (REPORT / name).read_text(encoding="utf-8")
        command = ('python docs/reports/modules/pipelay/build_report.py '
                   '--output-dir docs/reports/modules/pipelay '
                   '--scratch-dir "${TMPDIR:?Set TMPDIR to an external scratch directory}/pipelay-report"')
        if name == "report.html":
            records = []
            for text in (rebuilt, stored):
                record = json.loads(re.search(
                    r'<script id="calculation-record" type="application/json">(.*?)</script>', text, re.S)[1])
                records.append(record)
                config = json.loads(re.search(
                    r'<script id="review-config" type="application/json">(.*?)</script>', text, re.S)[1])
                bare = re.sub(r'<script id="review-config" type="application/json">.*?</script>', '', text, flags=re.S)
                assert builder.text_digest(bare) == config["report_digest"], f"Rebuild with: {command}"
                page = Page()
                page.feed(text)
                for attrs, value in page.bindings:
                    expected = float(builder.lookup(record, attrs["data-value"])) * float(attrs["data-scale"])
                    shown = float(value.replace("−", "-"))
                    places = int(attrs.get("data-places", 1))
                    exponent = math.floor(math.log10(abs(expected))) if 0 < abs(expected) < 0.05 else 0
                    quantum = 10 ** (exponent - places)
                    epsilon = 4 * math.ulp(expected)
                    if attrs.get("data-rounding") == "up":
                        assert expected - epsilon <= shown < expected + quantum + epsilon
                    else:
                        assert abs(shown - expected) <= 0.5 * quantum + epsilon
                for value in re.findall(r'Reel core; demand ([0-9.e+-]+) µm/m;', text):
                    expected = record['reel']['display']['reeling_bending_strain'] * 1e6
                    assert abs(float(value) - expected) <= 0.05 + 4 * math.ulp(expected)
                for bar in re.findall(r'<rect [^>]*data-screen="[^"]+"[^>]*>', text):
                    case = re.search(r'data-screen="([^"]+)"', bar)[1]
                    width = float(re.search(r'width="([^"]+)"', bar)[1])
                    assert width == pytest.approx(min(builder.screen(record, case)[1] * 100, 140) * 3.6,
                                                 abs=5e-4 + 1e-12)
            assert_record_agreement(*records)
            # Presentation and source digests stay strict; raw floats are compared above.
            def normalize(text):
                text = re.sub(r'<script id="(?:calculation-record|review-config)" type="application/json">.*?</script>',
                              '', text, flags=re.S)
                text = re.sub(r'(<(?:span|tspan)\b[^>]*data-value="[^"]+"[^>]*>)[^<]*(</(?:span|tspan)>)',
                              r'\1NUMBER\2', text)
                text = re.sub(r'<rect [^>]*data-screen="[^"]+"[^>]*>',
                              lambda match: re.sub(r'width="[^"]+"', 'width="NUMBER"', match[0]), text)
                # Only the checked reel-demand tie may vary in tooltip text.
                return re.sub(r'(Reel core; demand )[0-9.e+-]+( µm/m;)', r'\1NUMBER\2', text)
            rebuilt, stored = normalize(rebuilt), normalize(stored)
        assert builder.text_digest(rebuilt) == builder.text_digest(stored), f"Rebuild with: {command}"


def test_report_deliverables_are_unignored(ignore_repo):
    for name in ("build_report.py", "report.html", "changes.json"):
        result = subprocess.run(["git", "-c", "core.excludesFile=", "check-ignore", "--no-index", "-q",
                                 f"docs/reports/modules/pipelay/{name}"],
                                cwd=ignore_repo, env=git_env(), capture_output=True)
        assert result.returncode == 1, name
    # Review comments remain local and are not offered for publication.
    result = subprocess.run(["git", "-c", "core.excludesFile=", "check-ignore", "--no-index", "-q",
                             "docs/reports/modules/pipelay/report.comments.json"],
                            cwd=ignore_repo, env=git_env(), capture_output=True)
    assert result.returncode == 0


@pytest.mark.parametrize("quantity", ["offset", "reel_tie"])
def test_rebuild_tolerates_float_representation_differences(builder, tmp_path, monkeypatch, quantity):
    from digitalmodel.pipelay import workflow

    original = workflow.run_jlay

    def changed(settings):
        result = original(settings)
        result["sagbend"]["horizontal_offset_m"] = math.nextafter(
            result["sagbend"]["horizontal_offset_m"], math.inf)
        return result

    if quantity == "offset":
        monkeypatch.setattr(workflow, "run_jlay", changed)
    else:
        display = builder.display_values

        def changed_display(inputs, result):
            values = display(inputs, result)
            if "reeling_bending_strain" in values:
                values["reeling_bending_strain"] = math.nextafter(values["reeling_bending_strain"], 0)
            return values

        monkeypatch.setattr(builder, "display_values", changed_display)
    test_report_rebuild_matches_presentation_and_records(builder, tmp_path)


@pytest.mark.parametrize("kind", ["bound", "tooltip", "scientific"])
def test_rebuild_rejects_wrong_stored_numbers(builder, tmp_path, monkeypatch, kind):
    original = Path.read_text

    def changed(path, *args, **kwargs):
        text = original(path, *args, **kwargs)
        if path == REPORT / "report.html":
            if kind == "scientific":
                text = re.sub(r'(<span data-value="jlay.display.tension_closure_relative_error"[^>]*>)[^<]+',
                              r'\g<1>0.000e+20', text)
            else:
                text = (text.replace('>1954.4</span>', '>1954.3</span>') if kind == "bound"
                        else text.replace('demand 1954.4 µm/m', 'demand 1.0 µm/m'))
            bare = re.sub(r'<script id="review-config" type="application/json">.*?</script>', '', text, flags=re.S)
            config = json.dumps({"revision": builder.REVISION, "report_digest": builder.text_digest(bare)})
            text = re.sub(r'(<script id="review-config" type="application/json">).*?(</script>)',
                          lambda match: match[1] + config + match[2], text, flags=re.S)
        return text

    monkeypatch.setattr(Path, "read_text", changed)
    with pytest.raises(AssertionError):
        test_report_rebuild_matches_presentation_and_records(builder, tmp_path)


def test_perfect_straightening_reports_unbounded_radius(builder, tmp_path, monkeypatch):
    original = builder.load_example

    def changed(case):
        cfg = original(case)
        if case == "reel":
            cfg["pipelay"]["config"]["straightener_efficiency"] = 1.0
        return cfg

    monkeypatch.setattr(builder, "load_example", changed)
    page = builder.render(builder.calculate(tmp_path))
    assert "Unbounded (zero residual curvature)" in page


def test_all_worked_numbers_equal_fresh_module_returns(builder, tmp_path):
    from digitalmodel.pipelay.workflow import router

    record = builder.calculate(tmp_path)
    page = Page()
    page.feed(builder.render(record))
    assert not page.unbound_worked_numbers
    assert len(page.bindings) >= 25
    # A separate invocation verifies the report record against the public router.
    for case in ("jlay", "reel"):
        cfg = builder.load_example(case)
        cfg["pipelay"]["outputs"]["directory"] = (tmp_path / "independent" / case).as_posix()
        returned = router(cfg)["pipelay"]
        for block in ("pipe", "config"):
            assert record[case]["inputs"][block] == returned[block]
        for key, value in record[case]["result"].items():
            assert value == returned[key]
    for attrs, text in page.bindings:
        value = builder.lookup(record, attrs["data-value"])
        expected = builder.format_value(value, float(attrs["data-scale"]),
                                        int(attrs.get("data-places", 1)), attrs.get("data-rounding", "nearest"))
        assert text == expected


def test_rebuild_rejects_inconsistent_module_geometry(builder, tmp_path, monkeypatch):
    from digitalmodel.pipelay import workflow

    original = workflow.run_jlay

    def changed(settings):
        result = original(settings)
        result["sagbend"]["horizontal_offset_m"] += 10
        return result

    monkeypatch.setattr(workflow, "run_jlay", changed)
    with pytest.raises(ValueError, match="offset"):
        builder.render(builder.calculate(tmp_path / "after"))


@pytest.mark.parametrize("case,limit_key,limit", [
    ("jlay", "allowable_sagbend_strain", 0.0019),
    ("reel", "allowable_reeling_strain", 0.017),
])
def test_failed_screen_rebuilds_with_failed_text_and_colour(builder, tmp_path, monkeypatch,
                                                           case, limit_key, limit):
    original = builder.load_example

    def changed(name):
        cfg = original(name)
        if name == case:
            cfg["pipelay"]["config"][limit_key] = limit
        return cfg

    monkeypatch.setattr(builder, "load_example", changed)
    record = builder.build(tmp_path / "report", tmp_path / "calculations")
    assert record[case]["result"]["screening_status"] == "fail"
    page = (tmp_path / "report/report.html").read_text(encoding="utf-8")
    name = "J-lay" if case == "jlay" else "reel-lay"
    assert f"The public {name} example exceeds" in page
    assert f'data-screen="{case}" data-status="fail"' in page
    assert 'fill="var(--nogo)"' in builder.component_chart(record)
    assert "The public examples meet" not in page


def test_stress_verdict_uses_smys_and_reports_both_values(builder, tmp_path):
    record = builder.calculate(tmp_path)
    record["jlay"]["inputs"]["pipe"]["smys"] = 300e6
    record["jlay"]["display"] = builder.display_values(record["jlay"]["inputs"], record["jlay"]["result"])
    page = builder.render(record)
    assert "stress proxy exceeds" in page
    assert "300.0" in page
    assert "stress proxy is below" not in page


def test_router_verdict_disagreement_stops_rendering(builder, tmp_path):
    record = builder.calculate(tmp_path)
    record["reel"]["result"]["screening_status"] = "fail"
    with pytest.raises(ValueError, match="disagrees"):
        builder.render(record)


def test_minimum_radius_rounds_up_from_inputs(builder, tmp_path):
    record = builder.calculate(tmp_path)
    # E5: 0.3238/(2*0.002)=80.95 m; 81.0 m gives strain below 0.002.
    value = builder.lookup(record, "jlay.display.min_stinger_radius_m")
    assert value == pytest.approx(80.95)
    assert "81.0" in builder.number(record, "jlay.result.min_stinger_radius_m")
    record["jlay"]["inputs"]["pipe"]["coating_thickness"] = 0.0254
    assert "25.400" in builder.number(record, "jlay.inputs.pipe.coating_thickness", 1000)


@pytest.mark.parametrize("bad", [None, True, 0, -1, float("inf"), float("nan")])
def test_invalid_decision_limit_cannot_be_presented_as_pass(builder, tmp_path, bad):
    record = builder.calculate(tmp_path)
    record["jlay"]["result"]["allowable_sagbend_strain"] = bad
    with pytest.raises((ValueError, TypeError), match="finite|positive|limit"):
        builder.render(record)


@pytest.mark.parametrize("bad", [None, "unknown", "not_applicable"])
def test_unknown_screen_status_cannot_be_presented_as_pass(builder, tmp_path, bad):
    record = builder.calculate(tmp_path)
    record["reel"]["result"]["screening_status"] = bad
    with pytest.raises(ValueError, match="status"):
        builder.render(record)


def test_display_values_match_independent_hand_calculations(builder, tmp_path):
    page = Page()
    page.feed(builder.render(builder.calculate(tmp_path)))
    bindings = {attrs["data-value"]: text for attrs, text in page.bindings}
    # E1-E5: w=g*(rho_s*As+rho_c*Ac-rho_w*Ad)=1658.331318 N/m;
    # a=1500*sin(3 deg)/(1-sin(3 deg))=82.839414 m;
    # T=w*1500/(1-sin(3 deg))/1000=2624.872173 kN;
    # epsilon=0.3238/(2*a)=0.001954383687; epsilon/0.003*100=65.1461 %;
    # x=a*acosh(1+1500/a)=301.745330 m; s=sqrt(1500**2+2*a*1500)=1580.670188 m.
    expected = {
        "jlay.result.pipe_submerged_weight_N_per_m": "1658.3",
        "jlay.result.top_tension_kN": "2624.9",
        "jlay.result.sagbend.catenary_parameter_m": "82.8",
        "jlay.result.sagbend.sagbend_bending_strain": "1954.4",
        "jlay.display.sagbend_strain_utilisation": "65.1",
        "jlay.result.sagbend.horizontal_offset_m": "301.7",
        "jlay.result.sagbend.suspended_length_m": "1580.7",
        # E5-E6: 0.2731/(2*8)*1e6=17068.75; /0.02*100=85.34375 %;
        # 0.2731*(1-0.95)/(2*8)*1e6=853.4375;
        # (1-0.95)/8*1000=6.25; ((1-0.95)/8)*12**2/8*1000=112.5 mm.
        "reel.display.reeling_bending_strain": "17068.8",
        "reel.display.reeling_strain_utilisation": "85.3",
        "reel.display.residual_bending_strain": "853.4",
        "reel.result.residual_curvature_1_per_m": "6.25",
        "reel.result.residual_out_of_straightness_m": "112.5",
        "jlay.display.min_stinger_radius_m": "81.0",
        # Linear elastic yield strain: 448e6/207e9*1e6=2164.2512 µm/m.
        "jlay.display.yield_strain": "2164.3",
        "jlay.inputs.pipe.wall_thickness": "20.600",
        "jlay.inputs.pipe.coating_thickness": "60.000",
    }
    for key, literal in expected.items():
        assert bindings[key] == literal, key
    rows = re.findall(r"<tr>(.*?)</tr>", builder.render(builder.calculate(tmp_path / "units")), re.S)
    for quantity, unit in [("Empty-pipe submerged weight", "N/m"), ("Derived top tension", "kN"),
                           ("Core bending strain", "µm/m"), ("Small-slope residual span offset", "mm")]:
        assert any(quantity in row and f"<td>{unit}</td>" in row for row in rows)


def test_report_tables_are_captioned_and_figures_follow_reading_order(builder, tmp_path):
    page = builder.render(builder.calculate(tmp_path))
    assert page.count("</table>") == page.count('class="table-caption"')
    assert re.findall(r"Figure ([A-Z])\.", page) == ["A", "B", "C", "D"]
    assert "total reeling-strain criterion" in page
    assert "plastic-strain limit" not in page


def test_lf_writes_and_source_hashes_ignore_checkout_line_endings(builder, tmp_path, monkeypatch):
    original = Path.read_bytes
    reference = builder.calculate(tmp_path / "reference")["source_digests"]

    def crlf(path):
        return original(path).replace(b"\r\n", b"\n").replace(b"\n", b"\r\n")

    monkeypatch.setattr(Path, "read_bytes", crlf)
    assert builder.calculate(tmp_path / "crlf")["source_digests"] == reference
    monkeypatch.setattr(Path, "read_bytes", original)
    original_write = Path.write_text
    writes = []

    def write(path, text, **kwargs):
        if path.parent == tmp_path / "report":
            writes.append(kwargs)
        return original_write(path, text, **kwargs)

    monkeypatch.setattr(Path, "write_text", write)
    builder.build(tmp_path / "report", tmp_path / "outputs")
    assert len(writes) == 2
    assert all(w.get("encoding") == "utf-8" and w.get("newline") == "\n" for w in writes)
    assert b"\r\n" not in (tmp_path / "report/report.html").read_bytes()


def test_module_parent_ignore_rule_allows_independent_module_exceptions(ignore_repo):
    rules = (ROOT / ".gitignore").read_text(encoding="utf-8").splitlines()
    assert rules.count("!docs/reports/modules/") == 1
    assert "docs/reports/modules/*/*" in rules
    assert "docs/reports/modules/*" not in rules
    with (ignore_repo / ".gitignore").open("a", encoding="utf-8", newline="\n") as stream:
        for module in ("example", "other"):
            stream.write(f"!docs/reports/modules/{module}/\n!docs/reports/modules/{module}/report.html\n")
    for module in ("pipelay", "example", "other"):
        for name, expected in (("report.html", 1), ("report.comments.json", 0)):
            result = subprocess.run(["git", "-c", "core.excludesFile=", "check-ignore", "--no-index", "-q",
                                     f"docs/reports/modules/{module}/{name}"],
                                    cwd=ignore_repo, env=git_env())
            assert result.returncode == expected
    root_comment = subprocess.run(["git", "-c", "core.excludesFile=", "check-ignore", "--no-index", "-q",
                                   "docs/reports/modules/report.comments.json"],
                                  cwd=ignore_repo, env=git_env())
    assert root_comment.returncode == 0


def test_charts_are_module_samples_and_show_limits(builder, tmp_path):
    record = builder.calculate(tmp_path)
    html = builder.render(record)
    embedded = json.loads(re.search(r'<script id="calculation-record" type="application/json">(.*?)</script>', html, re.S)[1])
    assert embedded == record
    for case in ("jlay", "reel"):
        op = record[case]["result"]["operability"]
        assert f'data-limit="{op["operating_limit_m"]}"' in html
        assert len(op["lay_rate_table"]) > 1
        assert all(row["lay_rate_km_per_day"] == 0 for row in op["lay_rate_table"]
                   if row["hs_m"] > op["operating_limit_m"])
    assert 'aria-live="polite"' in html
    assert 'tabindex="0"' in html
    assert 'class="limit"' in html


def test_house_style_sections_links_and_public_text(builder, tmp_path):
    page_text = builder.render(builder.calculate(tmp_path))
    page = Page()
    page.feed(page_text)
    assert '<body class="hs-doc">' in page_text
    assert builder.house_style() in page_text
    assert not page.resources
    assert len(page.headings) == 10
    assert len(page.ids) == len(set(page.ids))
    assert all(link[1:] in page.ids for link in page.links if link.startswith("#"))
    for link in page.links:
        if link and not link.startswith("#"):
            assert (REPORT / link.split("#")[0]).resolve().exists(), link
    visible = " ".join(page.visible)
    assert "Draft for maintainer review" in visible
    assert "ASSUMED - to be confirmed" in visible
    assert "NOT_EVALUATED" in visible
    assert not re.search(r"\b(could|should|would|validated|certified|compliant)\b", visible, re.I)
    assert "<b>" not in page_text and "<strong>" not in page_text
    assert page_text.index('id="revision-history"') < page_text.index('id="introduction"')


def test_comment_and_change_controls_are_revision_bound(builder, tmp_path):
    page = builder.render(builder.calculate(tmp_path))
    assert "showSaveFilePicker" in page
    assert "report.comments.json" in page
    assert "textContent" in page
    assert "innerHTML" not in builder.REVIEW_SCRIPT
    assert "report_digest" in page
    assert "crypto.subtle.digest" in page
    assert "showDirectoryPicker" in page
    assert "createWritable" in page
    changes = builder.changes()
    assert changes["revision"] == builder.REVISION
    assert changes["changes"]
    assert json.dumps(changes, ensure_ascii=False) in page


def test_no_runtime_paths_or_nonfinite_values_are_embedded(builder, tmp_path):
    record = builder.calculate(tmp_path)
    text = builder.render(record)
    assert str(tmp_path) not in text
    assert "summary_json" not in record["jlay"]["result"]
    altered = copy.deepcopy(record)
    altered["jlay"]["result"]["top_tension_kN"] = float("nan")
    with pytest.raises(ValueError):
        builder.render(altered)


def test_comment_save_load_and_digest_checks_without_browser(builder, tmp_path):
    """Exercise persistence APIs with a small DOM substitute and no browser."""
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the standalone comment-control check")
    page = builder.render(builder.calculate(tmp_path))
    harness = r'''
const assert = require('node:assert/strict');
const vm = require('node:vm');
const crypto = require('node:crypto').webcrypto;
const input = JSON.parse(require('node:fs').readFileSync(0, 'utf8'));
const jsonBlock = id => JSON.parse(input.page.match(new RegExp(
  '<script id="' + id + '" type="application/json">([\\s\\S]*?)</script>'))[1]);
const config = jsonBlock('review-config');
const nodes = new Map();
function element() {
  return {value: '', textContent: '', children: [],
    appendChild(child) { this.children.push(child); if (!this.value) this.value = child.value; },
    replaceChildren() { this.children = []; }};
}
function get(id) { if (!nodes.has(id)) nodes.set(id, element()); return nodes.get(id); }
get('review-config').textContent = JSON.stringify(config);
get('calculation-record').textContent = JSON.stringify(jsonBlock('calculation-record'));
const sections = [...input.page.matchAll(/<section id="([^"]+)"><h2>([^<]+)<\/h2>/g)]
  .map(match => ({id: match[1], querySelector: () => ({textContent: match[2]})}));
let saved = '';
let selectedPage = input.page;
let writes = 0;
let createCalls = 0;
const savedHandle = {
  getFile: async () => ({size: saved.length, text: async () => saved}),
  createWritable: async () => ({write: async data => { saved = data; writes++; }, close: async () => {}})
};
const folder = {getFileHandle: async (name, options) => {
  if (name === 'report.html') return {getFile: async () => ({text: async () => selectedPage})};
  assert.equal(name, 'report.comments.json'); assert.deepEqual(JSON.parse(JSON.stringify(options)), {create: true});
  createCalls++; return savedHandle;
}};
const document = {getElementById: get, createElement: element,
  querySelectorAll: selector => selector === 'main > section' ? sections : []};
const window = {showDirectoryPicker: async () => folder};
vm.runInNewContext(input.script, {document, window, crypto, TextEncoder, Blob, URL, setTimeout});
(async () => {
  assert.equal(sections.length, 10);
  get('comment-text').value = 'Confirm the supplied coating density.';
  get('add-comment').onclick();
  await get('save-comments').onclick();
  assert.equal(writes, 1);
  const first = JSON.parse(saved);
  assert.equal(first.comments.length, 1);
  assert.equal(first.report_digest, config.report_digest);
  // A CRLF checkout represents the same report and retains its comments.
  selectedPage = input.page.replace(/\n/g, '\r\n');
  await get('save-comments').onclick();
  assert.equal(writes, 2);
  selectedPage = input.page;
  // Existing compatible comments are retained on save without an explicit import.
  saved = JSON.stringify({...first, comments: [...first.comments,
    {section: 'design-data', text: 'Confirm the empty-bore assumption.'}]});
  get('comment-text').value = 'Confirm the strain criterion.';
  get('add-comment').onclick();
  await get('save-comments').onclick();
  assert.equal(JSON.parse(saved).comments.length, 3);
  // A changed report is rejected before the sidecar is opened for writing.
  selectedPage = input.page.replace('Pipelay module analysis', 'Changed report');
  const callsBefore = createCalls;
  await get('save-comments').onclick();
  assert.equal(createCalls, callsBefore);
  assert.match(get('comment-status').textContent, /different report/);
  selectedPage = input.page;
  // An incompatible existing sidecar is preserved.
  saved = JSON.stringify({...first, revision: 'different'});
  const retained = saved;
  const writesBefore = writes;
  await get('save-comments').onclick();
  assert.equal(saved, retained); assert.equal(writes, writesBefore);
  assert.match(get('comment-status').textContent, /do not match/);
  // A mismatched import changes neither the existing comments nor their display.
  const countBefore = get('comment-list').children.length;
  await get('load-comments').onchange({target: {files: [{size: retained.length,
    text: async () => retained}], value: 'selected'}});
  assert.equal(get('comment-list').children.length, countBefore);
  assert.match(get('comment-status').textContent, /do not match/);
  // The compatible import is de-duplicated with comments already held locally.
  await get('load-comments').onchange({target: {files: [{size: 1,
    text: async () => JSON.stringify(first)}], value: 'selected'}});
  assert.equal(get('comment-list').children.length, countBefore);
  console.log('Comment persistence and report binding passed.');
})().catch(error => { console.error(error); process.exitCode = 1; });
'''
    result = subprocess.run([node, "-e", harness], input=json.dumps({"page": page, "script": builder.REVIEW_SCRIPT}),
                            text=True, capture_output=True, timeout=30)
    assert result.returncode == 0, result.stderr
    assert "Comment persistence and report binding passed." in result.stdout
