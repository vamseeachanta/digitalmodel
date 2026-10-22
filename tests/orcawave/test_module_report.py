"""Numerical provenance and offline delivery contract for the module report."""
import importlib.util
import json
import re
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/orcawave"


def builder():
    path = REPORT / "build_report.py"
    spec = importlib.util.spec_from_file_location("orcawave_module_report", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_rebuild_matches_committed_report(tmp_path):
    module = builder()
    module.build(tmp_path)
    for name in ("index.html", "changes.json"):
        assert (tmp_path / name).read_text(encoding="utf-8") == (
            REPORT / name).read_text(encoding="utf-8")


def test_render_matches_saved_artifacts_without_filesystem_mutation():
    module = builder()
    assert module.render() == (REPORT / "index.html").read_text(encoding="utf-8")
    assert module.CHANGES == json.loads((REPORT / "changes.json").read_text())


def test_every_example_value_and_chart_ordinate_matches_module():
    from digitalmodel.orcawave.motion_statistics import (
        compute_response_spectrum, short_term_statistics,
    )
    from digitalmodel.orcawave.wave_spectrum import pierson_moskowitz

    module = builder()
    html = module.render()
    data = json.loads(re.search(
        r'<script id="example-data" type="application/json">(.*?)</script>',
        html, re.S,
    ).group(1))
    omega = np.linspace(0.1, 2.0, 200)
    spectrum = pierson_moskowitz(omega, hs=3.0, tp=10.0)
    response = compute_response_spectrum(omega, np.full(200, 0.8), spectrum)
    stats = short_term_statistics(response)
    expected = {**stats.model_dump(), "m0": response.m0,
                "m2": response.m2, "m4": response.m4}
    assert data["results"] == expected
    assert data["omega"] == omega.tolist()
    assert data["wave"] == spectrum.tolist()
    assert data["response"] == response.spectral_density
    values = {**data["inputs"], **data["results"], **data["derived"]}
    displayed = re.findall(r'data-value="([^"]+)">([^<]+)</span>', html)
    assert set(key for key, _ in displayed) == set(values)
    for key, displayed_value in displayed:
        assert displayed_value == module.number(values[key])
    assert data["derived"]["variance_difference_percent"] == (
        100 * (response.m0 / data["derived"]["reference_variance"] - 1)
    )


def test_single_file_and_client_wording():
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
    visible = re.sub(r'<(script|style)\b.*?</\1>', '', html, flags=re.S)
    visible = re.sub('<[^>]+>', ' ', visible)
    assert not re.search(r'\b(owner|lane|session|certified|compliant|should|would|could)\b',
                         visible, re.I)
    assert '<b>' not in html and '<strong>' not in html


def test_provenance_sources_exist_and_match():
    import hashlib

    data = builder().calculate()
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
