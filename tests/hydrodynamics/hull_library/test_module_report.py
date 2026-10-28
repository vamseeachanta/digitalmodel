# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the bounded panel report against Cartesian hand calculations."""

from __future__ import annotations

import ast
import copy
import importlib.util
import json
import math
import os
import re
import subprocess
import sys
from hashlib import sha256
from html.parser import HTMLParser
from pathlib import Path

import numpy as np
import pytest

ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/hull_library"
PACKAGE = ROOT / "src/digitalmodel/hydrodynamics/hull_library"
DELIVERABLES = {"report.html", "changes.json", "sources.json"}
# Synthetic inputs and equations: package docs/worked-panel-scaling.md,
# Inputs and Derivation. Coordinates in m, areas in m^2, factors/normals in 1.
VERTICES = [[1, 2, -3], [5, 2, -3], [5, 5, 1], [1, 5, 1]]
REFERENCE = [2, -1, 3]
FACTORS = {"source": [1, 1, 1], "uniform": [2, 2, 2], "parametric": [3, 2, 1 / 2]}
UNITS = {
    "factors": "1", "vertices": "m", "reference_point": "m", "panels": "index",
    "edge_ab": "m", "edge_ad": "m", "edge_lengths": "m", "normal": "1",
    "center": "m", "panel_area": "m^2", "total_area": "m^2",
    "n_vertices": "count", "n_panels": "count",
}
EQUATIONS = {
    "factors": "E1", "vertices": "E1", "reference_point": "E1", "panels": "E5",
    "edge_ab": "E2", "edge_ad": "E2", "edge_lengths": "E2", "normal": "E3",
    "center": "E4", "panel_area": "E2", "total_area": "E2",
    "n_vertices": "E5", "n_panels": "E5",
}
LIMITS = ("physical hull suitability", "hydrodynamic responses", "response-table provenance",
          "mesh convergence", "solver qualification")


def oracle(factors):
    """Use perpendicular-edge geometry, independent of all mesh calculations."""
    sx, sy, sz = factors
    # AB=(4,0,0), AD=(0,3,4); their dot product is zero before/after scaling.
    # |AB'|=4*sx; |AD'|=sqrt((3*sy)^2+(4*sz)^2); A'=|AB'||AD'|.
    # Thus the areas are 20, 80 and 12*sqrt(40) m^2. Winding normal is
    # (0,-4*sz,3*sy)/|AD'|; the centre is the arithmetic mean of corners.
    ab_length = 4 * sx
    ad_length = math.sqrt((3 * sy) ** 2 + (4 * sz) ** 2)
    return {
        "factors": factors,
        "vertices": [[p[i] * factors[i] for i in range(3)] for p in VERTICES],
        "reference_point": [REFERENCE[i] * factors[i] for i in range(3)],
        "panels": [[0, 1, 2, 3]],
        "edge_ab": [ab_length, 0, 0], "edge_ad": [0, 3 * sy, 4 * sz],
        "edge_lengths": [ab_length, ad_length],
        "normal": [0, -4 * sz / ad_length, 3 * sy / ad_length],
        "center": [3 * sx, 3.5 * sy, -sz],
        "panel_area": ab_length * ad_length, "total_area": ab_length * ad_length,
        "n_vertices": len(VERTICES), "n_panels": 1,
    }


def flatten(value, prefix=""):
    """Enumerate scalar bindings without consulting the builder."""
    if isinstance(value, (dict, list)):
        items = value.items() if isinstance(value, dict) else enumerate(value)
        for key, child in items:
            yield from flatten(child, f"{prefix}.{key}" if prefix else str(key))
    else:
        yield prefix, value


def assert_roundoff(actual, expected):
    """Apply a synthetic-geometry budget, never an engineering tolerance."""
    # IEEE binary64 epsilon = ulp(1). Products/differences of these dyadic
    # inputs are exact. Cross products, squared norm, square root, triangle
    # sum and normalization require fewer than 64 scalar operations, without
    # subtractive cancellation in nonzero components for this rectangle.
    # Budget = 64*epsilon*max(1,abs(q)) in each quantity's stated unit.
    assert actual == pytest.approx(expected, rel=0, abs=64 * math.ulp(1.0) * max(1, abs(expected)))


class Page(HTMLParser):
    """Read visible bound quantities, units, source links and headings."""

    def __init__(self, text):
        super().__init__()
        self.bindings, self.links, self.ids, self.resources = {}, [], [], []
        self.visible, self.sections, self.headings, self.scripts = [], [], [], []
        self.numeric_rows, self.row, self.cell, self.row_bindings = [], None, None, []
        self.current, self.heading, self.hidden = None, None, None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "section":
            self.sections.append(attrs.get("id"))
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag in {"img", "link", "script", "iframe"}:
            self.resources.extend(attrs[k] for k in ("src", "href") if k in attrs)
        if tag in {"script", "style"}:
            self.hidden = tag
        if tag == "h2":
            self.heading = ""
        if tag == "tr":
            self.row, self.row_bindings = [], []
        if tag == "td":
            self.cell = ""
        if "data-value" in attrs:
            key = attrs["data-value"]
            assert key not in self.bindings, f"Duplicate displayed binding: {key}"
            self.bindings[key] = [attrs, ""]
            self.current = key
            self.row_bindings.append(key)

    def handle_endtag(self, tag):
        if tag == "span":
            self.current = None
        if tag == "h2":
            self.headings.append(self.heading)
            self.heading = None
        if tag == "td":
            self.row.append(self.cell)
            self.cell = None
        if tag == "tr" and self.row_bindings:
            self.numeric_rows.append((self.row_bindings, self.row))
        if tag == self.hidden:
            self.hidden = None

    def handle_data(self, data):
        if self.hidden == "script":
            self.scripts.append(data)
        if not self.hidden:
            self.visible.append(data)
        if self.heading is not None:
            self.heading += data
        if self.current is not None:
            self.bindings[self.current][1] += data
        if self.cell is not None:
            self.cell += data


@pytest.fixture(scope="module")
def builder():
    spec = importlib.util.spec_from_file_location("hull_module_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_worked_values_match_independent_calculation(builder):
    record = builder.calculate()
    assert record["units"] == UNITS
    for name, factors in FACTORS.items():
        expected = dict(flatten(oracle(factors)))
        for field in ("cases", "reference"):
            actual = dict(flatten(record[field][name]))
            assert actual.keys() == expected.keys()
            for key, value in expected.items():
                if key.startswith(("panels.", "n_")):
                    assert actual[key] == value  # Topology/counts are exact, not approximate.
                else:
                    assert_roundoff(actual[key], value)
    assert record["source_unchanged"] is True


def check_visible(text, record):
    page = Page(text)
    expected = {f"{group}.{key}": value for group in ("cases", "reference", "difference")
                for key, value in flatten(record[group])}
    assert page.bindings.keys() == expected.keys()  # No missing or extra quantities.
    for key, value in expected.items():
        attrs, visible = page.bindings[key]
        unit = UNITS[key.split(".")[2]]
        digits = 0 if unit in {"index", "count"} else 1
        assert visible == f"{value:.{digits}f}", key
        assert attrs["data-unit"] == unit
        assert attrs["data-equation"] == EQUATIONS[key.split(".")[2]]
        assert attrs["data-source"] == "synthetic-panel"
    assert len(page.numeric_rows) * 3 == len(expected)
    for bindings, cells in page.numeric_rows:
        key = bindings[0].removeprefix("cases.")
        assert bindings == [f"{group}.{key}" for group in ("cases", "reference", "difference")]
        assert cells[0] == key
        assert cells[4] == UNITS[key.split(".")[1]]  # Visible unit, not metadata alone.
    assert not page.resources


def test_visible_values_follow_computed_results(builder, monkeypatch):
    from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
    from digitalmodel.hydrodynamics.hull_library.mesh_scaler import (
        scale_mesh_parametric, scale_mesh_uniform,
    )

    record = builder.calculate()
    source = PanelMesh(vertices=np.array(VERTICES, dtype=float), panels=np.array([[0, 1, 2, 3]]),
                       reference_point=REFERENCE.copy())
    fresh = {"source": source, "uniform": scale_mesh_uniform(source, 2),
             "parametric": scale_mesh_parametric(source, 3, 2, 1 / 2)}
    for name, mesh in fresh.items():
        # Fresh public calls corroborate the builder; the oracle remains geometry.
        actual = record["cases"][name]
        np.testing.assert_array_equal(actual["vertices"], mesh.vertices)
        np.testing.assert_array_equal(actual["panels"], mesh.panels)
        np.testing.assert_array_equal(actual["reference_point"], mesh.reference_point)
        np.testing.assert_array_equal(actual["center"], mesh.panel_centers[0])
        np.testing.assert_array_equal(actual["normal"], mesh.normals[0])
        assert actual["panel_area"] == mesh.panel_areas[0]
        assert actual["total_area"] == mesh.total_area
    def zeros(value):
        if isinstance(value, dict):
            return {k: zeros(v) for k, v in value.items()}
        if isinstance(value, list):
            return [zeros(v) for v in value]
        return 0

    independent = {"cases": {k: oracle(v) for k, v in FACTORS.items()},
                   "reference": {k: oracle(v) for k, v in FACTORS.items()},
                   "difference": {k: zeros(oracle(v)) for k, v in FACTORS.items()}}
    check_visible((REPORT / "report.html").read_text(encoding="utf-8"), independent)

    original = builder.scale_mesh_uniform

    def altered_area(mesh, factor):
        changed = original(mesh, factor)
        # Controlled +1 m^2 fault, chosen above the one-decimal display increment.
        changed.panel_areas = changed.panel_areas + 1
        return changed

    monkeypatch.setattr(builder, "scale_mesh_uniform", altered_area)
    mutated = builder.calculate()
    rendered = builder.build(mutated)
    check_visible(rendered, mutated)
    before = Page(builder.build(record)).bindings
    after = Page(rendered).bindings
    for key in ("cases.uniform.panel_area", "cases.uniform.total_area"):
        assert before[key][1] != after[key][1]
        assert float(after[key][1]) == float(before[key][1]) + 1
    assert float(after["difference.uniform.total_area"][1]) == 1


def test_report_rebuild_is_deterministic(builder, tmp_path):
    environment = {k: v for k, v in os.environ.items()
                   if k not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    environment.update(PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    outputs = [tmp_path / "first", tmp_path / "different" / "second"]
    for output in outputs:
        subprocess.run([sys.executable, str(REPORT / "build_report.py"), "--output-dir", str(output)],
                       cwd=tmp_path, env=environment, check=True, capture_output=True, timeout=60)
        assert {p.name for p in output.iterdir()} == DELIVERABLES
    for name in DELIVERABLES:
        assert (outputs[0] / name).read_bytes() == (outputs[1] / name).read_bytes()
        assert (outputs[0] / name).read_bytes() == (REPORT / name).read_bytes(), f"Rebuild with {builder.REBUILD}"
    assert not list(tmp_path.rglob("__pycache__"))


def test_documented_rebuild_routes_import_caches_to_scratch(builder, tmp_path):
    assert 'MPLCONFIGDIR="$TMPDIR/' in builder.REBUILD
    assert 'XDG_CACHE_HOME="$TMPDIR/' in builder.REBUILD
    environment = {k: v for k, v in os.environ.items()
                   if k not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "MPLCONFIGDIR", "XDG_CACHE_HOME"}}
    environment["TMPDIR"] = str(tmp_path)
    subprocess.run(["bash", "-c", builder.REBUILD], cwd=ROOT, env=environment,
                   check=True, capture_output=True, timeout=60)
    assert {p.name for p in (tmp_path / "hull-report").iterdir()} == DELIVERABLES
    assert not list(REPORT.rglob("__pycache__"))


def test_scope_limits_and_report_pointers(builder):
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    page = Page(text)
    assert page.sections == [f"section-{i}" for i in range(1, 11)]
    titles = ["Executive Summary", "Introduction", "Data Basis", "Theory and Equations",
              "Worked Example", "Validation Evidence", "Limits of Use", "Recommendations", "References", "Appendix"]
    assert page.headings == [f"{i}. {title}" for i, title in enumerate(titles, 1)]
    assert len(page.ids) == len(set(page.ids))
    assert text.index('id="section-1"') < text.index('id="revision-history"') < text.index('id="section-2"')
    assert text.index('</section>') < text.index('id="revision-history"')
    assert "engineering-changes" in page.ids
    for limit in LIMITS:
        assert f"{limit}: NOT_EVALUATED" in " ".join(page.visible)
    assert "Reproducibility does not establish physical validation" in " ".join(page.visible)
    for link in page.links:
        if link.startswith("#"):
            assert link[1:] in page.ids
        else:
            assert not re.match(r"[a-z]+:", link)
            assert (REPORT / link.split("#")[0]).is_file(), link
    for readme, target in ((REPORT / "README.md", REPORT / "report.html"),
                           (PACKAGE / "README.md", REPORT / "README.md")):
        links = re.findall(r"\]\(([^)]+)\)", readme.read_text(encoding="utf-8"))
        assert any((readme.parent / p).resolve() == target for p in links)
    sources = json.loads((REPORT / "sources.json").read_text(encoding="utf-8"))
    for source in sources["sources"]:
        path = Path(source["path"])
        assert not path.is_absolute() and ".." not in path.parts
        assert sha256((ROOT / path).read_bytes()).hexdigest() == source["sha256"], f"Rebuild with {builder.REBUILD}"
        assert source["basis"] and source["locator"]
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert changes["revision"] == builder.REVISION and changes["changes"]
    assert all(change in " ".join(page.visible) for change in changes["changes"])


def test_source_map_covers_public_exports(builder):
    tree = ast.parse((PACKAGE / "__init__.py").read_text(encoding="utf-8"))
    exports = next(ast.literal_eval(node.value) for node in tree.body
                   if isinstance(node, ast.Assign) and any(isinstance(t, ast.Name) and t.id == "__all__"
                                                          for t in node.targets))
    methods = builder.method_map()
    assert {r["name"] for r in methods} == set(exports)
    assert {r["name"] for r in methods if r["checked"] == "INDEPENDENTLY_CHECKED"} == {
        "scale_mesh_uniform", "scale_mesh_parametric"}
    for row in methods:
        assert row["implemented"] is True
        assert row["checked"] in {"INDEPENDENTLY_CHECKED", "NOT_EVALUATED"}
        assert (ROOT / row["path"]).is_file()


def test_html_and_json_are_escaped(builder):
    changed = copy.deepcopy(builder.CHANGES)
    payload = '<img src=x onerror="alert(1)"></script><script>alert(1)</script>&'
    changed["changes"] = [payload]
    page = Page(builder.build(builder.calculate(), changes=changed))
    assert not page.resources
    assert payload in " ".join(page.visible)
    data = json.loads("".join(page.scripts))
    assert data["changes"]["changes"] == [payload]
    assert data["calculation"]["cases"]["parametric"]["total_area"] == 75.9


def test_outputs_are_public_and_path_independent():
    path_names = ("ho" + "me", "m" + "nt")
    words = ("ow" + "ner", "cli" + "ent", "se" + "at", "la" + "ne", "ses" + "sion")
    path_pattern = re.compile(
        rf"(?:/(?:{'|'.join(path_names)})/|[A-Za-z]:\\|[\w.+-]+@[\w-]+\.[\w.-]+)"
    )
    word_pattern = re.compile(rf"\b(?:{'|'.join(words)})\b", re.I)
    for part in path_names:
        assert path_pattern.search("/" + part + "/example")
    assert path_pattern.search("Q:" + chr(92) + "example")
    assert path_pattern.search("example" + chr(64) + "example.invalid")
    for word in words:
        assert word_pattern.search(word.upper())
    paths = [REPORT / name for name in sorted(DELIVERABLES)] + [Path(__file__)]
    for path in paths:
        text = path.read_text(encoding="utf-8")
        assert "\r" not in text
        assert str(ROOT) not in text and str(Path.home()) not in text
        assert not path_pattern.search(text)
        assert not word_pattern.search(text)
