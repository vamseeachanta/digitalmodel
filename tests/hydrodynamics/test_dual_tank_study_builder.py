# ABOUTME: Reproducibility and provenance checks for the connected-tank study.
"""Check the public study against its committed analytical sources."""

import importlib.util
import math
import re
from html import escape as html_escape
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
    assert style == builder.SHOWCASE._CSS + builder.LAYOUT_CSS
    assert style.startswith(re.search(
        r"<style>(.*?)</style>", EXISTING.read_text(encoding="utf-8"), re.S
    ).group(1))


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
    ticks = [float(node.attrib['data-value']) for node in svg.findall("text[@class='y-tick']")]
    assert len(ticks) == len(set(ticks))
    assert ticks == pytest.approx([0, .2, .4, .6])
    values = [float(node.attrib['data-y']) for node in svg.findall(".//g[@data-series='0']")]
    table = ElementTree.fromstring(re.search(r"<table>.*?</table>", fill, re.S).group())
    table_values = {cell.text for cell in table.findall('.//td/data')}
    assert {f'{value:.3f}' for value in values} <= table_values


def test_area_damping_ticks_are_distinct_and_match_table(builder):
    evidence = builder.render_cfd_evidence()
    loss = evidence.split('id="cfd-loss"', 1)[1].split('id="cfd-forced"', 1)[0]
    svg = ElementTree.fromstring(
        re.search(r'<svg id="cfd-loss-area".*?</svg>', loss, re.S).group())
    ticks = [float(node.attrib['data-value']) for node in svg.findall("text[@class='y-tick']")]
    assert len(ticks) == len(set(ticks))
    gaps = [b-a for a, b in zip(ticks, ticks[1:])]
    assert gaps == pytest.approx([gaps[0]] * len(gaps))
    table = ElementTree.fromstring(re.search(r"<table>.*?</table>", loss, re.S).group())
    damping = [float(cell.attrib['value']) for row in table.findall('tr')[1:]
               for cell in row.findall('td/data')[-2:]]
    plotted = [float(node.attrib['data-y']) for node in svg.findall('.//g[@data-y]')]
    assert sorted(plotted) == sorted(damping)


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
    assert "the cause is not established" in text
    assert "an earlier explanation was withdrawn" not in text
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
        if root.attrib.get("class") == "study-chart":
            assert root.findall(".//g[@data-y]")
    assert sum("outside the published moment-fit band" in "".join(svg.itertext())
               for svg in chart_roots(text)) == 2
    assert text.count("<script") == 1
    assert "<img" not in text
    assert "Conclusions and next steps" in text and "Not done" in text
    assert "Revision history" in text
    assert text.index('id="summary"') < text.index('id="revisions"') < text.index('id="geometry"')
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
    # This exact source sentence is prescribed for the discrete-period view.
    scanned = text.replace('9.2 Data record and builder links', '').replace('No fitted curve: the archived geometry is not published, so the model cannot be drawn between the cases from committed inputs.', '')
    assert not re.search(process_words, scanned, re.I)
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


# Revision 2 acceptance checks exercise the rendered output, not just its metadata.
def chart_roots(text):
    return [ElementTree.fromstring(svg) for svg in re.findall(r'<svg\b.*?</svg>', text, re.S)
            if 'class="study-chart"' in svg]


def test_a1_chart_csv_freshness(builder, tmp_path):
    builder.build(tmp_path / PAGE.name)
    specs = builder.chart_specs()
    assert len(specs) == 13
    for spec in specs:
        relative = Path('data/charts') / (spec['id'] + '.csv')
        raw = (tmp_path / relative).read_bytes()
        assert raw == (PAGE.parent / relative).read_bytes()
        assert b'\r' not in raw


def test_a2_preserved_inputs_and_table_cells(builder):
    import hashlib
    import html
    import json

    expected = {
        builder.EVIDENCE_PATH: '61299a9d7b9031c87c2d0157850233c654266d74fd2217a3a752ad3f66ae6c22',
        builder.JSON_PATH: '70c875573807cfb86d8bdb947fe98583a78247475315a2bf002ae7fac7e91bca',
    }
    for path, digest in expected.items():
        assert hashlib.sha256(path.read_bytes()).hexdigest() == digest
    # Every raw evidence value and locator retains the pre-correction binding,
    # including moments now displayed in MN m and the frequency-error range.
    class EvidenceBindings(HTMLParser):
        def __init__(self):
            super().__init__()
            self.values = []

        def handle_starttag(self, tag, attrs):
            if tag == 'data':
                attrs = dict(attrs)
                self.values.append((attrs['data-provenance'], attrs['value']))

    bindings = EvidenceBindings()
    bindings.feed(builder.render())
    assert len(bindings.values) == 211
    digest = hashlib.sha256(json.dumps(sorted(bindings.values), ensure_ascii=False).encode()).hexdigest()
    assert digest == '507bcb21019c7de157586b27789f7c607708049ea5dea5780441174aa305617c'
    text = re.sub(r'<details id="revision-history">.*?</details>', '', builder.render(), flags=re.S)
    text = re.sub(r'<div id="sensitivity-matrix">.*?</div>', '', text, flags=re.S)
    cells = [re.sub(r' \[I\d+\]', '', html.unescape(re.sub('<[^>]+>', '', c)))
             for c in re.findall('<td>(.*?)</td>', text, re.S)]
    # The revision table is checked separately. Pin the revised engineering
    # labels, MN m presentation and distinguishable frequency-error bounds.
    assert len(cells) == 244
    digest = hashlib.sha256(json.dumps(sorted(cells), ensure_ascii=False).encode()).hexdigest()
    assert digest == 'f90157044a70b0c2d09b596948f559b3e72c54a5de0e4b202e6af4e2528a7555'


def test_a3_derived_findings(builder):
    import csv
    import json

    rows = list(csv.DictReader((PAGE.parent / 'data/charts/coupled-response.csv').open()))
    peaks = [max(float(r[key]) for r in rows) for key in
             ('roll_bare_per_unit_moment', 'roll_with_tank_per_unit_moment')]
    text = re.sub('<[^>]+>', '', builder.render())
    assert f'falls from {peaks[0]:.1f} to {peaks[1]:.1f} per unit wave moment' in text
    evidence = json.loads(builder.EVIDENCE_PATH.read_text())
    inside = sum(abs(r['error']['value']) <= evidence['published_band']['value']
                 for r in evidence['period_checks'])
    assert inside == 4 and len(evidence['period_checks']) == 5
    assert 'four of five off-calibration checks' in text


def test_a4_a6_a7_every_plotted_point_matches_csv(builder):
    import csv

    text = builder.render()
    roots = {svg.attrib['id']: svg for svg in chart_roots(text)}
    for spec in builder.chart_specs():
        svg = roots[spec['id']]
        csv_path = 'data/charts/' + spec['id'] + '.csv'
        assert f'href="{csv_path}"' in text
        with (PAGE.parent / csv_path).open(encoding='utf-8', newline='') as stream:
            reader = csv.DictReader(stream)
            assert reader.fieldnames == spec['headers']
            rows = list(reader)
        assert len(rows) == len(spec['rows'])
        assert [list(row.values()) for row in rows] == [
            ['' if v is None else str(v) for v in row] for row in spec['rows']]
        for index, series in enumerate(spec['series']):
            nodes = svg.findall(f".//g[@data-series='{index}']")
            assert len(nodes) == len(series['points'])
            for node, point in zip(nodes, series['points']):
                row = rows[int(node.attrib['data-row'])]
                assert float(node.attrib['data-x']) == float(row[series['xkey']])
                assert float(node.attrib['data-y']) == float(row[series['ykey']])
                assert node.attrib['tabindex'] == '0'
                title = node.find('title').text
                assert series['name'] in title
                assert spec['xunit'] in title and spec['yunit'] in title
                assert builder.chart_reference(spec['id']) in title
                reference = text.split(f'id="ref-chart-{spec["id"]}"', 1)[1].split('</ul></li>', 1)[0]
                assert html_escape(point['source']) in reference
                assert node.attrib['data-tooltip'] == title
        assert bool(svg.findall('polyline')) == spec['continuous']
        if not spec['continuous']:
            assert not svg.findall('.//path')


def test_a5_linear_axes_round_ticks_and_band(builder):
    roots = {svg.attrib['id']: svg for svg in chart_roots(builder.render())}
    for spec in builder.chart_specs():
        svg = roots[spec['id']]
        for axis in ('x', 'y'):
            title = svg.find(f".//text[@class='{axis}-label']")
            assert '(' in ''.join(title.itertext())
            ticks = svg.findall(f".//text[@class='{axis}-tick']")
            labels = [''.join(t.itertext()) for t in ticks]
            assert len(labels) == len(set(labels))
            if spec.get('categories') and axis == 'x':
                continue
            values = [float(t.attrib['data-value']) for t in ticks]
            gaps = [b - a for a, b in zip(values, values[1:])]
            assert gaps == pytest.approx([gaps[0]] * len(gaps))
            assert builder.nice_step(gaps[0]) == pytest.approx(gaps[0]) or spec['id'] in {
                'natural-period-vs-area', 'period-error'}
        if not spec.get('categories'):
            points = svg.findall('.//g[@data-x]')
            for node in points:
                low, high = spec['xrange']
                expected = builder.PLOT_LEFT + (float(node.attrib['data-x']) - low) / (high-low) * (
                    builder.PLOT_RIGHT - builder.PLOT_LEFT)
                assert float(node.attrib['data-px']) == pytest.approx(expected, abs=.01)
    c2 = roots['natural-period-vs-area']
    assert len(c2.findall(".//g[@class='whisker']")) == 4
    assert 'calibration reference (not an independent check)' in ' '.join(''.join(c2.itertext()).split())
    c3 = roots['period-error']
    red = [g for g in c3.findall('.//g[@data-x]') if g.attrib['data-color'] == 'var(--red)']
    assert len(red) == 1
    assert 'outside the band (low fill)' in ''.join(red[0].itertext())
    for identifier in ('coupled-response', 'moment', 'period-error'):
        assert roots[identifier].find(".//rect[@class='band']") is not None


def test_a8_schematic_is_constant_driven(builder, monkeypatch):
    text = builder.render()
    assert text.index('<h2 id="geometry"') < text.index('<svg id="schematic"') < text.index('<h2 id="method"')
    svg = ElementTree.fromstring(re.search(r'<svg id="schematic".*?</svg>', text, re.S).group())
    labels = ' '.join(svg.itertext())
    for label in ('Left leg', 'Right leg', 'conduit', 'A_c', 'free surface', 'Δh',
                  'roll angle φ', 'tank height not specified', 'archived CFD twin-tank geometry is not shown'):
        assert label in labels
    before = builder.schematic()
    for key in ('LEG_LENGTH', 'LEG_WIDTH', 'FILL_DEPTH', 'CENTROID_OFFSET', 'CONDUIT_AREA'):
        name = 'ILLUSTRATIVE_' + key
        with monkeypatch.context() as context:
            context.setattr(builder, name, getattr(builder, name) + 1)
            assert builder.schematic() != before


def test_a9_order_and_a10_exact_writer_copy(builder):
    import html

    text = builder.render()
    ids = re.findall(r'<h2 id="([^"]+)"', text)
    assert ids == ['summary', 'revisions', 'introduction', 'geometry', 'method', 'curves',
                   'validation', 'limits', 'forward', 'basis', 'sensitivity', 'reproduce']
    assert '<details><summary>' in text
    spec = (ROOT / 'docs/plans/2026-10-25-dual-tank-page-layout-spec.md').read_text()
    visible = html.unescape(re.sub('<[^>]+>', '', text))
    for section in ('6.1 Summary', '6.3 Method'):
        passage = spec.split('### ' + section, 1)[1].split('\n\n"', 1)[1].split('"', 1)[0]
        passage = passage.replace('illustrative inputs below', 'illustrative inputs in Section 3')
        passage = passage.replace('key results are summarized below', 'key results are summarized in Section 1')
        assert passage in visible
    assert 'based on the benchmarked single tank' not in visible
    assert 'connecting lines guide the eye' not in visible
    assert 'OPEN:' not in visible
    expected_terms = {'validated': 1, 'verified': 0, 'proven': 0}
    for word, count in expected_terms.items():
        assert len(re.findall(r'\b' + word + r'\b', visible, re.I)) == count


def test_a12_public_charts_and_no_external_dependencies(builder):
    text = builder.render()
    assert_public_wording(text)
    assert len(re.findall('<script', text)) <= 1
    assert not re.search(r'<script[^>]+src=', text)
    assert re.findall(r'<link[^>]+href="([^"]+)"', text) == ['../_assets/brand.css']
    for path in (PAGE.parent / 'data/charts').glob('*.csv'):
        if path.stem in {spec['id'] for spec in builder.chart_specs()}:
            assert_public_wording(path.read_text())


def test_a4_csv_values_equal_model_and_evidence_sources(builder):
    import csv
    import json

    data = builder.compute()
    evidence = json.loads(builder.EVIDENCE_PATH.read_text())
    val = lambda fact: fact['value']
    cases = evidence['period_checks']
    calibration = evidence['calibration']
    expected = {
        'coupled-response': [list(row) for row in zip(data['periods'], *data['responses'])],
        'natural': [list(row) for row in zip(data['areas'], data['natural'])],
        'inertia': [[area, inertia / 1e6] for area, inertia in zip(data['areas'], data['inertia'])],
        'damping': [list(row) for row in zip(data['areas'], *[[v * 100 for v in ys] for ys in data['damping']])],
        'moment': [[t, m.in_phase / 1e6, m.quadrature / 1e6, m.magnitude / 1e6]
                   for t, m in zip(data['periods'], data['moments'])],
        'split': [list(row) for row in zip(data['gs'], *[a['ys'] for a in data['absorbers']])],
        'cfd-amplitude': [[val(r[k]) for k in ('area', 'cfd_amplitude', 'analytical_amplitude')]
                          for r in evidence['area_sweep']],
        'period-error': [[val(r[k]) for k in ('area', 'fill', 'predicted_period', 'measured_period', 'error')]
                         + [val(evidence['published_band']), abs(val(r['error'])) <= val(evidence['published_band'])]
                         for r in cases],
        'cfd-forced-exchange': [[val(r[k]) for k in ('period', 'exchange')] for r in evidence['forced_sweep']],
    }
    fill = sorted((r for r in cases if val(r['area']) == val(calibration['area'])
                   and val(r['fill']) != val(calibration['fill'])), key=lambda r: val(r['fill']))
    expected['cfd-fill-period'] = [[val(r[k]) for k in ('fill', 'measured_period', 'predicted_period')] for r in fill]
    expected['cfd-fill-damping'] = []
    for r in fill:
        selected = min(r['damping'], key=lambda c:
                       (val(c['roll']), abs(val(c['period']) - val(r['predicted_period']))))
        expected['cfd-fill-damping'].append([val(r['fill']), val(selected['measured']), val(selected['linear_prediction'])])
    area_cases = sorted((r for r in cases if val(r['fill']) == val(calibration['fill'])),
                        key=lambda r: val(r['area']))
    expected['cfd-loss-area'] = []
    for r in area_cases:
        chosen = min(r['damping'], key=lambda c: abs(val(c['period']) - val(r['predicted_period'])))
        expected['cfd-loss-area'].append([val(r['area']), val(chosen['measured']), val(chosen['linear_prediction'])])
    historical = {val(r['area']): val(r['historical_period']) for r in evidence['area_sweep']}
    expected['natural-period-vs-area'] = [
        [val(r['area']), val(r['fill']), 'calibration reference' if r is calibration else 'independent check',
         val(r['measured_period']), val(r['predicted_period']), historical.get(val(r['area']))]
        for r in sorted([*area_cases, calibration], key=lambda r: val(r['area']))]
    assert len(expected) == 13
    computed_ids = {'coupled-response', 'natural', 'inertia', 'damping', 'moment', 'split'}
    for identifier, values in expected.items():
        if identifier in computed_ids:
            values = [[float(format(value, '.12g')) for value in row] for row in values]
        with (PAGE.parent / f'data/charts/{identifier}.csv').open(encoding='utf-8', newline='') as stream:
            reader = csv.reader(stream)
            headers, *rows = reader
        if headers[-1] == 'evidence_pointer':
            assert all(row[-1] for row in rows)
            rows = [row[:-1] for row in rows]
        assert rows == [['' if value is None else str(value) for value in row] for row in values]


def hand_tank_properties(area):
    """Integrate two 30 × 8 × 4 m legs and a 20 m conduit about the roll axis.

    Inputs: the illustrative geometry table and the fluid properties in
    src/digitalmodel/hydrodynamics/utube_tank.py. Each cuboid contributes
    V * ((width² + height²) / 12 + y_centroid² + z_centroid²).
    """
    height = area / 30
    first_z = 1920 * (height + 2) + 20 * area * height / 2
    inertia = (1920 * ((8**2 + 4**2) / 12 + 10**2 + (height + 2)**2)
               + 20 * area * ((20**2 + height**2) / 12 + (height / 2)**2))
    return first_z, inertia, 2 * 30 * 8**3 / 12


def hand_tank_moment(period, angle):
    """Solve harmonic balance algebraically, without the model's iteration.

    Equation basis: _forced_volume_response and tank_moment in
    src/digitalmodel/hydrodynamics/utube_tank.py. With Q the volume amplitude,
    D the detuning and B the loss factor, Q²(D² + B²Q²) = F². The positive
    quadratic root below replaces the iterative amplitude calculation.
    """
    omega = 2 * math.pi / period
    inertia_length = 5.2004 / 8 + 8 / 240
    detuning = 19.62 / (240 * inertia_length) - omega**2
    loss = 4 * 2.3894 * omega**2 / (3 * math.pi * 8**2.138 * inertia_length)
    forcing = (196.2 * math.tan(angle)
               + 20 * (4 + 5.2004 + 8 / 60) * omega**2 * angle) / inertia_length
    q_squared = 2 * forcing**2 / (
        detuning**2 + math.sqrt(detuning**4 + 4 * loss**2 * forcing**2))
    damping = loss * math.sqrt(q_squared)
    denominator = detuning**2 + damping**2
    q_real = forcing * detuning / denominator
    q_imag = -196.2 * math.sin(angle) / inertia_length * damping / denominator
    first_z, inertia, surface = hand_tank_properties(8)
    return 1025 * (9.81 * (first_z + surface) * math.sin(angle)
                   - inertia * omega**2 * angle + 196.2 * complex(q_real, q_imag))


def hand_roll_response(period):
    """Bisect the scalar moment balance instead of iterating a complex response.

    Basis: coupled_roll_response in src/digitalmodel/hydrodynamics/utube_tank.py;
    hull period 20 s, damping 0.05, tank authority 0.12 and a 5 degree reference.
    The amplitude satisfies |Z_hull * angle - scale * M_tank(angle)| = M_wave.
    These checks assess the equations' evaluation, not their physical validity.
    """
    ratio = 20 / period
    hull = complex(1 - ratio**2, 0.1 * ratio)
    wave = 0.1 * math.radians(5)
    first_z, _, surface = hand_tank_properties(8)
    scale = 0.12 / (1025 * 9.81 * (first_z + surface + 48000))
    low, high = 0.0, 0.5
    assert abs(hull * high - scale * hand_tank_moment(period, high)) > wave
    for _ in range(60):
        angle = (low + high) / 2
        if abs(hull * angle - scale * hand_tank_moment(period, angle)) < wave:
            low = angle
        else:
            high = angle
    return 1 / abs(hull), (low + high) / (2 * wave)


def assert_computed_exports_against_independent_equations(builder, tmp_path):
    import csv

    builder.build(tmp_path / PAGE.name)

    def rows(identifier):
        with (tmp_path / 'data/charts' / (identifier + '.csv')).open() as stream:
            return [[float(value) for value in row] for row in list(csv.reader(stream))[1:]]

    # Rigid-column energy balance: T² = 4π²(L_eff*S/A + 2h)/(2g).
    # Harmonic head loss: zeta = 2*K*omega*Q/(3π*A^p*L*omega_n).
    for index in (0, 55, 110):
        area, period = rows('natural')[index]
        expected_period = 2 * math.pi * math.sqrt((5.2004 * 240 / area + 8) / 19.62)
        assert period == pytest.approx(expected_period, rel=1e-10)
        assert rows('inertia')[index][1] == pytest.approx(
            1025 * hand_tank_properties(area)[1] / 1e6, rel=1e-10)
        length = 5.2004 / area + 8 / 240
        damping = [100 * 2 * 2.3894 * (2 * math.pi / 20) * (240 * level / 2)
                   / (3 * math.pi * area**2.138 * length * (2 * math.pi / expected_period))
                   for level in (0.1, 0.5)]
        assert rows('damping')[index][1:] == pytest.approx(damping, rel=1e-10)
        forcing_period, real, imag, magnitude = rows('moment')[index]
        moment = hand_tank_moment(forcing_period, math.radians(5)) / 1e6
        assert [real, imag, magnitude] == pytest.approx(
            [moment.real, moment.imag, abs(moment)], rel=1e-8)
        # Solve the two-degree-of-freedom dynamic stiffness matrix directly.
        # Equation basis: scripts/cfd/reduce_tld_two_peak.py module docstring.
        ratio, *responses = rows('split')[index]
        expected = []
        for damping_ratio in (0.03, 0.095, 0.1273, 0.18, 0.305):
            coupling = complex(1, 2 * damping_ratio * ratio)
            impedance = 1 - ratio**2 - 0.05 * ratio**2 * coupling / (coupling - ratio**2)
            expected.append(1 / abs(impedance))
        assert responses == pytest.approx(expected, rel=1e-10)

    # All 111 grid points are checked, so both reported grid peaks and their
    # locations are constrained independently of compute() and the exports.
    response_rows = rows('coupled-response')
    expected = [hand_roll_response(16 + 0.4 * index) for index in range(111)]
    for index, (period, bare, coupled) in enumerate(response_rows):
        assert period == pytest.approx(16 + 0.4 * index)
        assert [bare, coupled] == pytest.approx(expected[index], rel=2e-6)
    for column in (0, 1):
        peak_index = max(range(111), key=lambda i: expected[i][column])
        assert max(range(111), key=lambda i: response_rows[i][column + 1]) == peak_index
        assert max(row[column + 1] for row in response_rows) == pytest.approx(
            expected[peak_index][column], rel=2e-6)
    peaks = [max(row[column] for row in expected) for column in (0, 1)]
    visible = re.sub('<[^>]+>', '', builder.render())
    assert f'falls from {peaks[0]:.1f} to {peaks[1]:.1f} per unit wave moment' in visible


def test_computed_exports_against_independent_equations(builder, tmp_path):
    assert_computed_exports_against_independent_equations(builder, tmp_path)


@pytest.mark.parametrize('method', ['natural_period', 'coupled_roll_response'])
def test_independent_checks_reject_regenerated_calculation_errors(builder, monkeypatch, tmp_path, method):
    original = getattr(builder.MODEL, method)
    monkeypatch.setattr(builder.MODEL, method, lambda **kwargs: 1.01 * original(**kwargs))
    with pytest.raises(AssertionError):
        assert_computed_exports_against_independent_equations(builder, tmp_path)


def test_fill_damping_names_original_inverse_area_prediction(builder):
    spec = next(s for s in builder.chart_specs() if s['id'] == 'cfd-fill-damping')
    assert spec['series'][1]['name'] == 'Original inverse-area prediction'
    assert spec['headers'][2] == 'inverse_area_damping_no_units'
    svg = next(svg for svg in chart_roots(builder.render())
               if svg.attrib['id'] == 'cfd-fill-damping')
    assert 'Original inverse-area prediction' in ' '.join(''.join(svg.itertext()).split())


def test_a6_exact_chart_set_and_independent_marker_counts(builder):
    import json

    text = builder.render()
    roots = {svg.attrib['id']: svg for svg in chart_roots(text)}
    assert set(roots) == {'coupled-response', 'natural-period-vs-area', 'period-error',
                          'natural', 'inertia', 'damping', 'moment', 'split', 'cfd-amplitude',
                          'cfd-fill-period', 'cfd-fill-damping', 'cfd-loss-area', 'cfd-forced-exchange'}
    summary = text.split('<h3>5.1 Coupled roll response</h3>')[1].split('<h3>5.2 Illustrative response curves</h3>')[0]
    assert {svg.attrib['id'] for svg in chart_roots(summary)} == {
        'coupled-response'}
    archived = text.split('<h3>5.4 Archived CFD comparisons</h3>')[1]
    assert {'natural-period-vs-area', 'period-error'} <= {
        svg.attrib['id'] for svg in chart_roots(archived)}
    evidence = json.loads(builder.EVIDENCE_PATH.read_text())
    counts = {
        'natural-period-vs-area': [3, 1, 4, 3], 'period-error': [len(evidence['period_checks'])],
        'cfd-amplitude': [len(evidence['area_sweep'])] * 2,
        'cfd-fill-period': [2, 2], 'cfd-fill-damping': [2, 2], 'cfd-loss-area': [3, 3],
        'cfd-forced-exchange': [len(evidence['forced_sweep'])],
    }
    for identifier, expected in counts.items():
        svg = roots[identifier]
        actual = [len(svg.findall(f".//g[@data-series='{i}']")) for i in range(len(expected))]
        assert actual == expected
        assert not svg.findall('polyline') and not svg.findall('.//path')


def test_a3_and_a7_response_points_follow_changed_model(builder, monkeypatch):
    original = builder.MODEL.coupled_roll_response

    def changed(**kwargs):
        return original(**kwargs) * 2

    before = builder.chart_specs()[0]
    monkeypatch.setattr(builder.MODEL, 'coupled_roll_response', changed)
    after = builder.chart_specs()[0]
    assert [r[0] for r in before['rows']] == [r[0] for r in after['rows']]
    assert [r[1] for r in after['rows']] == pytest.approx([r[1] * 2 for r in before['rows']])
    assert [r[2] for r in after['rows']] == pytest.approx([r[2] * 2 for r in before['rows']])
    text = builder.render()
    peak = max(r[1] for r in after['rows'])
    assert f'falls from {peak:.1f} to' in text


def test_a1_exports_ignore_one_ulp_model_roundoff(builder, monkeypatch):
    import math

    before = builder.render()
    original = builder.MODEL.coupled_roll_response

    def rounded_differently(**kwargs):
        value = original(**kwargs)
        return complex(math.nextafter(value.real, math.inf), value.imag)

    monkeypatch.setattr(builder.MODEL, 'coupled_roll_response', rounded_differently)
    assert builder.render() == before


def test_reporting_section_order_and_pinned_headlines(builder):
    """Pin the reporting sequence and the existing evidence headline values."""
    import html

    text = builder.render()
    headings = re.findall(r'<h2 id="[^"]+">(.*?)</h2>', text)
    assert headings == [
        '1 Executive summary', 'Revision history', '2 Introduction', '3 Design data',
        '4 Analysis methodology', '5 Results', '6 Validation and benchmark',
        '7 Limitations', '8 Conclusions and next steps', '9 References',
        'Appendix A – Sensitivity matrix', 'Appendix B – Reproduce commands',
    ]
    summary = text.split('<h2 id="summary">', 1)[1].split('<h2 id="introduction">', 1)[0]
    visible = html.unescape(re.sub('<[^>]+>', '', summary))
    # Existing evidence record: loss, calibration and off-calibration period checks.
    for value in ('2.138', '8.6 %', '35', '36.5 %', '3.7 %', '14.31 s',
                  'four of five off-calibration checks'):
        assert value in visible
    # Existing response CSV, independently constrained by the equation checks.
    assert 'falls from 10.0 to 5.7 per unit wave moment' in visible
    assert 3 <= summary.count('<li>') <= 5
    assert summary.index('Use these results') < summary.index('design qualification is not established')
    assert '<h2 id="left">' not in text and ': screening estimate' not in text


def test_design_basis_assumptions_and_standard_record(builder):
    text = builder.render()
    design = text.split('<h2 id="geometry">', 1)[1].split('<h2 id="method">', 1)[0]
    assert re.findall(r'<h3[^>]*>(.*?)</h3>', design) == [
        '3.1 Site or field data', '3.2 Structure or equipment data',
        '3.3 Environmental data', '3.4 Vessel data', '3.5 Criteria',
        '3.6 Load cases',
    ]
    assert design.count('ASSUMED') >= 7
    revisions = text.split('<details id="revision-history">', 1)[1].split('</details>', 1)[0]
    assert '<summary>' in revisions and ' open' not in revisions[:revisions.index('<summary>')]
    assert 'numbered sections' in revisions
    assert not re.search(r'\b[A-Z]\d+\b|[0-9a-f]{9}', revisions)
    assert revisions.count('Not assigned') == 2  # Preserve undated historical entries.


def test_body_has_citations_instead_of_raw_source_lists(builder):
    text = builder.render()
    body = text.split('<h2 id="summary">', 1)[1].split('<h2 id="basis">', 1)[0]
    # Metadata retains machine-readable provenance; visible text uses references.
    visible = re.sub('<[^>]+>', '', body)
    assert 'archived-cfd-campaign-' not in visible
    assert not re.search(r'(?:src|scripts|tests|docs)/|\.json#/|\.py#', visible)
    assert '<ul class="sources">' not in body
    for svg in chart_roots(body):
        for node in svg.findall('.//g[@data-tooltip]'):
            assert 'archived-cfd-campaign-' not in node.attrib['data-tooltip']
            assert '[I' in node.find('title').text
    refs = text.split('<h2 id="basis">', 1)[1]
    assert '9.1 Public references' in refs and '9.2 Data record and builder links' in refs
    assert 'archived-cfd-campaign-' in refs
    assert_references_resolve(text, PAGE)


def test_numbered_objects_and_print_style(builder):
    text = builder.render()
    captions = re.findall('<figcaption>(.*?)</figcaption>', text, re.S)
    assert len(captions) == 14
    assert all(re.match(r'Figure \d+\.\d+ – ', caption) for caption in captions)
    assert len(re.findall(r'<caption>Table [\dA-CR]+\.\d+ – ', text)) == text.count('<table>')
    assert '@media print' in builder.LAYOUT_CSS
    assert '.study-chart text{font-size:13px' in builder.LAYOUT_CSS
    assert '.study-chart{max-width:440px' in builder.LAYOUT_CSS
    for svg in chart_roots(text):
        assert all(float(circle.attrib['r']) <= 2.5 for circle in svg.findall('.//circle'))


def test_schematic_dimensions_are_proportional_and_explicit(builder):
    svg = ElementTree.fromstring(re.search(r'<svg id="schematic".*?</svg>', builder.schematic(), re.S).group())
    legs = svg.findall(".//rect[@class='tank-leg']")
    assert len(legs) == 2
    water = svg.find(".//rect[@class='water-depth']")
    scale = float(legs[0].attrib['width']) / builder.ILLUSTRATIVE_LEG_WIDTH
    assert float(water.attrib['height']) == pytest.approx(scale * builder.ILLUSTRATIVE_FILL_DEPTH)
    assert float(legs[1].attrib['x']) - float(legs[0].attrib['x']) == pytest.approx(
        scale * 2 * builder.ILLUSTRATIVE_CENTROID_OFFSET)
    assert len(svg.findall(".//g[@class='dimension']")) >= 3
    assert 'fill="var(--panel)"' in builder.schematic()


@pytest.mark.parametrize('viewport,scrollbar', [(390, 0), (390, 15), (600, 15)])
def test_schematic_text_has_readable_narrow_screen_scale(builder, viewport, scrollbar):
    """Bound the final SVG scale with the wrapper and scrollbar allowance.

    This checks the layout arithmetic; browser font rendering is not measured.
    """
    svg = ElementTree.fromstring(re.search(r'<svg id="schematic".*?</svg>', builder.schematic(), re.S).group())
    width = float(svg.attrib['viewBox'].split()[2])
    style = re.search(r'\.schematic svg\{([^}]+)', builder.LAYOUT_CSS)[1]
    font = float(re.search(r'font-size:(\d+)px', style)[1])
    maximum = float(re.search(r'max-width:(\d+)px', style)[1])
    available = viewport - scrollbar - 16
    assert font * min(available, maximum) / width >= 12
    assert width <= 440


def test_results_keep_illustrative_and_archived_figures_separate(builder):
    text = builder.render()
    results = text.split('<h2 id="curves">', 1)[1].split('<h2 id="validation">', 1)[0]
    illustrative, archived = results.split('<h3>5.4 Archived CFD comparisons</h3>')
    assert {svg.attrib['id'] for svg in chart_roots(illustrative)} == {
        'coupled-response', 'natural', 'inertia', 'damping', 'moment', 'split'}
    assert {svg.attrib['id'] for svg in chart_roots(archived)} == {
        'natural-period-vs-area', 'period-error', 'cfd-amplitude', 'cfd-fill-period',
        'cfd-fill-damping', 'cfd-loss-area', 'cfd-forced-exchange'}
    assert 'quadratic loss makes it amplitude-dependent' in illustrative.split('id="coupled-response"')[0]
    assert 'archived CFD campaign' in text.split('<h2 id="introduction">')[1].split('<h2 id="geometry">')[0]


def test_summary_defines_alpha_and_qualifies_the_predicted_benefit(builder):
    text = builder.render()
    visible = re.sub('<[^>]+>', '', text)
    summary = visible.split('1 Executive summary', 2)[-1].split('Revision history')[0]
    assert 'illustrative screening prediction' in summary
    assert 'anti-roll benefit on a real vessel is not established' in summary
    assert 'alpha' in summary and 'normalized stiffness scale' in summary
    assert 'tank authority' not in visible.lower()
    design = visible.split('3.6 Load cases')[1].split('4 Analysis methodology')[0]
    assert 'μ = 0.05 and tuning ratio 1.0 are ASSUMED' in design


def test_reader_definitions_and_precision_are_source_bound(builder):
    text = re.sub('<[^>]+>', '', builder.render())
    for phrase in ('Exchange amplitude is the volume', 'Frozen-fluid inertia is',
                   'exchange plateau is the broad', 'forced-sweep peak',
                   'normal solver-completion message', 'automated case-launch scripts',
                   'open-boundary transfer check', 'list of analytical screening cases',
                   'imposed forcing condition', 'over-critical', '195.4 %'):
        assert phrase in text
    assert 'Refitted inverse-area law RMS' in text
    assert 'Refitted linear-in-area law' not in text
    assert 'Equivalent damping ratio (no units)' in text
    assert 'CFD-derived damping (no units)' in text
    assert '0.30–0.33 %' in text
    assert 'Roll-moment amplitude (MN m)' in text
    assert '7.5' in text
    for phrase in ('version confirmation is pending', 'an earlier explanation was withdrawn',
                   'source-neutral review'):
        assert phrase not in text


def test_reference_locators_stay_in_reference_register(builder):
    text = builder.render()
    body = text.split('<h2 id="basis">')[0]
    # Body tooltips and metadata keep resolvable markers, not visible locator lists.
    assert '<summary>Chart sources and archive locators</summary>' not in body
    assert '<summary>Chart sources and archive locators</summary>' in text.split('9.2 Data record and builder links')[1]


def test_qualification_has_two_locations_and_appendix_links_to_limits(builder):
    text = builder.render()
    qualification = 'These results support screening only; design qualification is not established.'
    assert text.count(qualification) == 2
    for start, end in [('summary', 'introduction'), ('limits', 'forward')]:
        assert qualification in text.split(f'<h2 id="{start}">')[1].split(f'<h2 id="{end}">')[0]
    appendix = text.split('<h2 id="sensitivity">')[1].split('<h2 id="reproduce">')[0]
    assert '<a href="#limits">Section 7</a>' in appendix
    assert qualification not in appendix
    assert 'The moment fit holds for forcing periods' not in appendix


def test_revision_history_preserves_each_earlier_entry(builder):
    """Pin the earlier rows, with only the public-reference redaction."""
    history = builder.render().split('<details id="revision-history">')[1].split('</details>')[0]
    expected = [
        ('3', '2026-10-25', 'Engineering reporting conventions: numbered sections, source register, dimensioned schematic and consistent plots; calculation inputs and results retained.'),
        ('2', '2026-10-25', 'schematic added; three summary charts; every chart interactive with a CSV; the limits gathered in one place'),
        ('Display precision', '2026-10-23', 'Damping-axis labels distinguished and comparison percentages rounded to one decimal place; source precision retained in the evidence record.'),
        ('Evidence', '2026-10-23', 'Archived CFD reductions added with provenance and evidence-class limits to distinguish measurements from illustrative predictions.'),
        ('Geometry', '2026-10-23', 'Independent illustrative dimensions and conduit area selected to separate the predictions from the reference geometry.'),
        ('Editorial', 'Not assigned', 'Engineering descriptions clarified to distinguish illustrative predictions from calibration evidence.'),
        ('Initial', 'Not assigned', 'Public-source analysis added to document model scope and remaining work.'),
    ]
    rows = re.findall(r'<tr[^>]*><td>(.*?)</td><td>(.*?)</td><td>(.*?)</td></tr>', history)
    assert rows[2:] == expected
    assert rows[0][0] == '5' and rows[1] == (
        '4', '2026-10-25',
        'Illustrative and archived results separated; response terms defined; narrow-screen schematic and display precision clarified; calculation inputs and results retained.')


@pytest.fixture(scope='module')
def rendered_study():
    """Serve committed assets locally; use an installed browser without downloads."""
    import os
    from functools import partial
    from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
    from threading import Thread

    from playwright.sync_api import sync_playwright

    class QuietHandler(SimpleHTTPRequestHandler):
        def log_message(self, *_args):
            pass

    server = ThreadingHTTPServer(('127.0.0.1', 0), partial(QuietHandler, directory=ROOT))
    thread = Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        with sync_playwright() as playwright:
            executable = os.environ.get('PLAYWRIGHT_CHROMIUM_EXECUTABLE')
            browser = playwright.chromium.launch(executable_path=executable)
            try:
                yield browser, f'http://127.0.0.1:{server.server_port}/{PAGE.relative_to(ROOT).as_posix()}'
            finally:
                browser.close()
    finally:
        server.shutdown()
        server.server_close()
        thread.join()


def rendered_svg_metrics(frame):
    return frame.locator('svg[role="img"]').evaluate_all('''svgs => svgs.map(svg => {
      const rect = svg.getBoundingClientRect();
      const texts = [...svg.querySelectorAll('text')].filter(t => t.textContent.trim());
      return {id: svg.id, width: rect.width, height: rect.height,
        minimumFont: Math.min(...texts.map(t => parseFloat(getComputedStyle(t).fontSize)
          * Math.hypot(t.getScreenCTM().a, t.getScreenCTM().b))),
        overflow: texts.filter(t => {
          const r = t.getBoundingClientRect();
          return r.left < rect.left - 1 || r.right > rect.right + 1
            || r.top < rect.top - 1 || r.bottom > rect.bottom + 1;
        }).map(t => t.textContent)};
    })''')


@pytest.mark.parametrize('width,theme,scripts', [
    (390, 'light', True), (390, 'dark', True), (600, 'light', True),
    (1280, 'light', True), (390, 'light', False),
])
def test_rendered_exact_width_frames_have_readable_text_and_no_scroll(
        rendered_study, width, theme, scripts, record_property):
    browser, url = rendered_study
    with browser.new_context(viewport={'width': 1440, 'height': 1000}, java_script_enabled=scripts) as context:
        page = context.new_page()
        page.set_content(f'<iframe title="Study" src="{url}" style="width:{width}px;height:900px;border:0"></iframe>')
        frame = page.frames[1]
        frame.wait_for_selector('#schematic')
        frame.evaluate('theme => document.documentElement.dataset.theme = theme', theme)
        frame.locator('details').evaluate_all('nodes => nodes.forEach(n => n.open = true)')
        frame.evaluate('document.fonts.ready')
        assert page.locator('iframe').bounding_box()['width'] == width
        layout = frame.evaluate('''() => ({width: innerWidth,
          client: document.documentElement.clientWidth,
          scroll: document.documentElement.scrollWidth, body: document.body.scrollWidth})''')
        assert layout['width'] == width
        assert layout['scroll'] == layout['client'] == layout['body']
        metrics = rendered_svg_metrics(frame)
        assert len(metrics) == 14
        assert all(item['minimumFont'] >= 12 for item in metrics), metrics
        assert all(not item['overflow'] for item in metrics), metrics
        record_property('rendered_layout', {'browser': browser.version, 'layout': layout, 'svg': metrics})
        assert_references_resolve(frame.content(), PAGE)


def test_rendered_print_bounds_and_pdf(rendered_study, tmp_path, record_property):
    from pypdf import PdfReader

    browser, url = rendered_study
    # A4 content width and height after the report's 16 mm print margins.
    printable_width = (210 - 32) * 96 / 25.4
    printable_height = (297 - 32) * 96 / 25.4
    with browser.new_context(viewport={'width': round(printable_width), 'height': 1000}) as context:
        page = context.new_page()
        page.goto(url)
        page.emulate_media(media='print')
        metrics = rendered_svg_metrics(page)
        assert len(metrics) == 14
        assert all(not item['overflow'] for item in metrics), metrics
        assert all(item['minimumFont'] >= 12 for item in metrics), metrics
        for selector in ('figure', 'table'):
            sizes = page.locator(selector).evaluate_all('nodes => nodes.map(n => ({width:n.getBoundingClientRect().width, height:n.getBoundingClientRect().height, split:getComputedStyle(n).breakInside}))')
            assert all(s['width'] <= printable_width + 1 and s['height'] < printable_height
                       and s['split'] == 'avoid' for s in sizes), sizes
        assert not page.locator('nav').is_visible()
        assert not page.locator('.chart-data').first.is_visible()
        pdf = tmp_path / 'study.pdf'
        page.pdf(path=str(pdf), format='A4', prefer_css_page_size=True)
        document = PdfReader(pdf)
        printed = '\n'.join(p.extract_text() for p in document.pages)
        assert len(document.pages) > 1
        for heading in ('1 Executive summary', '7 Limitations', '9 References', 'Appendix A', 'Appendix B'):
            assert heading in printed
        assert 'roll angle' in printed
        assert len(re.findall(r'Figure \d+\.\d+\s*–', printed)) == 14
        record_property('print_layout', {'browser': browser.version, 'pages': len(document.pages), 'svg': metrics})


def test_rendered_chart_hover_keyboard_touch_escape_and_csv(rendered_study, tmp_path):
    browser, url = rendered_study
    with browser.new_context(viewport={'width': 390, 'height': 900}, has_touch=True) as context:
        page = context.new_page()
        page.goto(url)
        tooltip = page.locator('#chart-tooltip')
        charts = page.locator('.study-chart')
        assert charts.count() == 13
        for chart in charts.all():
            points = chart.locator('[data-tooltip]')
            # The last series is painted on top of coincident observations.
            point = points.last
            expected = point.get_attribute('data-tooltip')
            point.hover(force=True)
            assert tooltip.is_visible() and tooltip.inner_text() == expected
            page.keyboard.press('Escape')
            assert tooltip.is_hidden()
            points.first.focus()
            assert tooltip.is_visible() and tooltip.inner_text() == points.first.get_attribute('data-tooltip'), (
                chart.get_attribute('id'), page.evaluate('''() => ({active:document.activeElement.outerHTML.slice(0,200), focused:document.hasFocus()})'''))
            page.keyboard.press('Tab')
            assert tooltip.is_visible() and tooltip.inner_text() == points.nth(1).get_attribute('data-tooltip')
            # Scrolling a focused point can move the pointer onto its chart.
            chart.dispatch_event('pointerover', {'pointerType': 'mouse'})
            assert tooltip.is_visible() and tooltip.inner_text() == points.nth(1).get_attribute('data-tooltip')
            chart.dispatch_event('pointerout', {'pointerType': 'mouse'})
            assert tooltip.is_visible() and tooltip.inner_text() == points.nth(1).get_attribute('data-tooltip')
            page.keyboard.press('Escape')
            assert tooltip.is_hidden()
            point.tap(force=True)
            assert tooltip.is_visible() and tooltip.inner_text() == expected
            page.keyboard.press('Escape')
            link = chart.locator('xpath=..').get_by_role('link', name='Data (CSV)')
            with page.expect_download() as pending:
                link.click()
            download = pending.value
            target = tmp_path / download.suggested_filename
            download.save_as(target)
            assert target.read_bytes() == (PAGE.parent / 'data/charts' / target.name).read_bytes()
