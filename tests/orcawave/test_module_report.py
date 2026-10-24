"""Numerical provenance and offline delivery contract for the module report."""
import importlib.util
import json
import re
from pathlib import Path

import numpy as np
import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/orcawave"
EXAMPLE_DATA = re.compile(
    r'<script id="example-data" type="application/json">(.*?)</script>', re.S,
)


def builder():
    path = REPORT / "build_report.py"
    spec = importlib.util.spec_from_file_location("orcawave_module_report", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def example_data(html):
    return json.loads(EXAMPLE_DATA.search(html).group(1))


def assert_reports_match(actual, expected):
    actual_data, expected_data = example_data(actual), example_data(expected)
    for key in ("omega", "wave", "response"):
        assert np.allclose(actual_data.pop(key), expected_data.pop(key),
                           rtol=1e-12, atol=0)
    for key in ("results", "derived"):
        assert actual_data.pop(key) == pytest.approx(
            expected_data.pop(key), rel=1e-12, abs=0,
        )
    assert actual_data == expected_data
    assert EXAMPLE_DATA.sub("", actual) == EXAMPLE_DATA.sub("", expected)


def test_rebuild_matches_committed_report(tmp_path):
    module = builder()
    module.build(tmp_path)
    assert_reports_match((tmp_path / "index.html").read_text(encoding="utf-8"),
                         (REPORT / "index.html").read_text(encoding="utf-8"))
    assert (tmp_path / "changes.json").read_text(encoding="utf-8") == (
        REPORT / "changes.json").read_text(encoding="utf-8")


def test_render_matches_saved_artifacts_without_filesystem_mutation():
    module = builder()
    assert_reports_match(module.render(), (REPORT / "index.html").read_text(
        encoding="utf-8"))
    assert module.CHANGES == json.loads((REPORT / "changes.json").read_text())


def test_rendered_and_saved_report_have_no_trailing_whitespace():
    for html in (builder().render(),
                 (REPORT / "index.html").read_text(encoding="utf-8")):
        assert not re.search(r"[ \t]+$", html, re.M)


def test_every_example_value_and_chart_ordinate_matches_module():
    from digitalmodel.orcawave.motion_statistics import (
        compute_response_spectrum, short_term_statistics,
    )
    from digitalmodel.orcawave.wave_spectrum import pierson_moskowitz

    module = builder()
    html = module.render()
    data = example_data(html)
    omega = np.linspace(0.1, 2.0, 200)
    spectrum = pierson_moskowitz(omega, hs=3.0, tp=10.0)
    response = compute_response_spectrum(omega, np.full(200, 0.8), spectrum)
    stats = short_term_statistics(response)
    expected = {**stats.model_dump(), "m0": response.m0,
                "m2": response.m2, "m4": response.m4}
    assert data["results"] == pytest.approx(expected, rel=1e-12, abs=0)
    for actual, expected_array in ((data["omega"], omega),
                                   (data["wave"], spectrum),
                                   (data["response"], response.spectral_density)):
        assert np.allclose(actual, expected_array, rtol=1e-12, atol=0)
    values = {**data["inputs"], **data["results"], **data["derived"]}
    displayed = re.findall(r'data-value="([^"]+)">([^<]+)</span>', html)
    assert set(key for key, _ in displayed) == set(values)
    for key, displayed_value in displayed:
        assert displayed_value == module.number(values[key], key)
    assert data["derived"]["variance_difference_percent"] == (
        100 * (response.m0 / data["derived"]["reference_variance"] - 1)
    )


def test_independent_reference_values():
    """Independent scalar integration, without importing calculation functions.

    With p=2*pi/10 and b=5*p**4/4, integrate
    0.8**2*(5/16)*3**2*p**4*w**(n-5)*exp(-b/w**4) over [0.1, 2].
    Scalar quadrature gives m0=0.355643169413 and m2=0.246713139079.
    Hence sigma=sqrt(m0)=0.596358255928, Tz=2*pi*sqrt(m0/m2)
    =7.543813875864 and N=10800/Tz=1431.636593601.
    The Rayleigh mode is sigma*sqrt(2*ln(N))=2.273458706245;
    adding Euler*sigma**2/mode gives the Gumbel mean 2.363754082157.
    The full-range integral is (0.8*3/4)**2=0.36 exactly in decimal.
    Relative tolerance 1e-5 allows the 200-point trapezoidal discretisation.
    """
    data = builder().calculate()
    assert data["derived"]["reference_variance"] == 0.36
    expected = {"m0": 0.355643, "rms": 0.596358255928,
                "mean_zero_crossing_period": 7.543813875864,
                "most_probable_maximum_3hr": 2.273458706245,
                "expected_maximum_3hr": 2.363754082157}
    for key, value in expected.items():
        assert data["results"][key] == pytest.approx(value, rel=1e-5)


@pytest.mark.parametrize("key", ["omega", "wave", "response"])
def test_regeneration_tolerates_only_roundoff(key):
    html = builder().render()
    data = example_data(html)
    data[key][100] *= 1 + 5e-13
    perturbed = EXAMPLE_DATA.sub(
        lambda _: '<script id="example-data" type="application/json">'
        + json.dumps(data) + '</script>', html,
    )
    assert_reports_match(perturbed, html)
    data[key][100] *= 1 + 1e-5
    changed = EXAMPLE_DATA.sub(
        lambda _: '<script id="example-data" type="application/json">'
        + json.dumps(data) + '</script>', html,
    )
    with pytest.raises(AssertionError):
        assert_reports_match(changed, html)


def test_report_display_precision():
    html = builder().render()
    displayed = dict(re.findall(r'data-value="([^"]+)">([^<]+)</span>', html))
    assert displayed["duration"] == "10800"
    assert displayed["points"] == "200"
    assert displayed["cycles"] == "1432"
    assert displayed["reference_variance"] == "0.3600"
    assert displayed["variance_difference_percent"] == "−1.2"


def test_equation_citations_and_bandwidth_qualification():
    html = builder().render()
    assert re.search(r'coefficients.*DNV-RP-C205 \(2014\).*3\.5\.5', html)
    assert 'Hₛ = 4√m₀' in html
    assert re.search(r'maximum expressions.*R1', html)
    paragraph = next(p for p in re.findall(r'<p>(.*?)</p>', html, re.S)
                     if 'most probable maximum estimate' in p)
    assert 'not narrow-band' in paragraph
    assert 'spectral_bandwidth' in paragraph


def test_single_file_and_maintainer_wording():
    html = builder().render()
    assert '<body class="hs-doc">' in html
    assert len(re.findall(r'<h2>\d+\.', html)) == 10
    assert '<script src=' not in html
    assert '<link rel="stylesheet"' not in html
    assert "showSaveFilePicker" in html
    assert "comments.json" in html
    assert "changes-data" in html
    assert "ASSUMED - to be confirmed" in html
    assert "NOT_EVALUATED" in html
    assert "Draft for maintainer review" in html
    visible = re.sub(r'<(script|style)\b.*?</\1>', '', html, flags=re.S)
    visible = re.sub('<[^>]+>', ' ', visible)
    assert not re.search(r'\b(owner|lane|session|certified|compliant|should|would|could)\b',
                         visible, re.I)
    assert '<b>' not in html and '<strong>' not in html


def test_provenance_sources_exist_and_match():
    import hashlib

    data = builder().calculate()
    assert set(data["sources"]) == {
        "src/digitalmodel/orcawave/motion_statistics.py",
        "src/digitalmodel/orcawave/wave_spectrum.py",
    }
    for path, digest in data["sources"].items():
        text = (ROOT / path).read_text(encoding="utf-8")
        assert hashlib.sha256(text.encode("utf-8")).hexdigest() == digest


def test_example_inputs_remain_bound_to_public_test():
    import ast

    path = ROOT / "tests/orcawave/test_motion_statistics.py"
    tree = ast.parse(path.read_text(encoding="utf-8"))
    example = next(node for node in ast.walk(tree)
                   if isinstance(node, ast.FunctionDef)
                   and node.name == "test_significant_response")
    calls = [node for node in ast.walk(example) if isinstance(node, ast.Call)]
    grid = next(node for node in calls if isinstance(node.func, ast.Attribute)
                and node.func.attr == "linspace")
    pm = next(node for node in calls if isinstance(node.func, ast.Name)
              and node.func.id == "pierson_moskowitz")
    inputs = builder().calculate()["inputs"]
    assert [ast.literal_eval(arg) for arg in grid.args] == [
        inputs["omega_min"], inputs["omega_max"], inputs["points"]]
    assert {kw.arg: ast.literal_eval(kw.value) for kw in pm.keywords} == {
        "hs": inputs["hs"], "tp": inputs["tp"]}
    rao = next(node.value for node in example.body if isinstance(node, ast.Assign)
               and node.targets[0].id == "rao")
    assert ast.literal_eval(rao.right) == inputs["rao"]
