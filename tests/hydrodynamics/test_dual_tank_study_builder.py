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
    assert "an earlier explanation was withdrawn." in text
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
    # This exact source sentence is prescribed for the discrete-period view.
    scanned = text.replace('No fitted curve: the archived geometry is not published, so the model cannot be drawn between the cases from committed inputs.', '')
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
    text = re.sub(r'<tr class="revision-2">.*?</tr>', '', builder.render(), flags=re.S)
    cells = [html.unescape(re.sub('<[^>]+>', '', c))
             for c in re.findall('<td>(.*?)</td>', text, re.S)]
    assert len(cells) == 259
    digest = hashlib.sha256(json.dumps(sorted(cells), ensure_ascii=False).encode()).hexdigest()
    assert digest == 'eef4a55dc509428f7c0c685a13ac8aebc877ae99f82ab22599f0de17f98f23dd'


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
                assert point['source'] in title
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
    assert text.index('<svg id="schematic"') < text.index('<h2 id="summary"')
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
    assert ids == ['summary', 'limits', 'revisions', 'method', 'geometry', 'validation',
                   'curves', 'comparison', 'cfd-evidence', 'forward', 'left', 'basis']
    assert '<details><summary>' in text
    spec = (ROOT / 'docs/plans/2026-10-25-dual-tank-page-layout-spec.md').read_text()
    visible = html.unescape(re.sub('<[^>]+>', '', text))
    for section in ('6.1 Summary', '6.2 Limits', '6.3 Method'):
        passage = spec.split('### ' + section, 1)[1].split('\n\n"', 1)[1].split('"', 1)[0]
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


def test_a6_exact_chart_set_and_independent_marker_counts(builder):
    import json

    text = builder.render()
    roots = {svg.attrib['id']: svg for svg in chart_roots(text)}
    assert set(roots) == {'coupled-response', 'natural-period-vs-area', 'period-error',
                          'natural', 'inertia', 'damping', 'moment', 'split', 'cfd-amplitude',
                          'cfd-fill-period', 'cfd-fill-damping', 'cfd-loss-area', 'cfd-forced-exchange'}
    summary = text.split('<h2 id="summary">')[1].split('<h2 id="limits">')[0]
    assert {svg.attrib['id'] for svg in chart_roots(summary)} == {
        'coupled-response', 'natural-period-vs-area', 'period-error'}
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
