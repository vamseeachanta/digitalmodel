# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic, visible values, inventory and delivery contracts."""
import ast
import hashlib
from html.parser import HTMLParser
import importlib.util
import json
import math
import re
import subprocess
import sys
from pathlib import Path

import numpy as np
import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_fowt_quasi_static_module_report.py"
REPORT = ROOT / "docs/reports/modules/fowt_quasi_static"


class Tables(HTMLParser):
    """Read visible table values independently of the report's row formatter."""

    def __init__(self):
        super().__init__()
        self.tables = []
        self.cell = None

    def handle_starttag(self, tag, attrs):
        if tag == "table":
            self.tables.append([])
        elif tag == "tr":
            self.tables[-1].append([])
        elif tag in ("th", "td"):
            self.cell = []

    def handle_data(self, value):
        if self.cell is not None:
            self.cell.append(value)

    def handle_endtag(self, tag):
        if tag in ("th", "td"):
            self.tables[-1][-1].append(''.join(self.cell))
            self.cell = None


def builder():
    spec = importlib.util.spec_from_file_location("fowt_module_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_natural_periods_match_hand_eigenvalues(tmp_path):
    """det(diag(j²)-w²I)=product(j²-w²); hence T_j=2*pi/j."""
    data = builder().calculate(tmp_path / "scratch")
    expected = [2 * math.pi / j for j in range(1, 7)]
    assert set(data["periods"]) == {"sampled", "between", "endpoints"}
    for values in data["periods"].values():
        np.testing.assert_allclose(values, expected, rtol=1e-11, atol=1e-12)


def test_response_matches_scalar_impedance(tmp_path):
    """2/[5-(2+1)-i]=(4+2i)/5; the diagonal condition number is one."""
    data = builder().calculate(tmp_path / "scratch")
    actual = np.array(data["response_real"]) + 1j * np.array(data["response_imag"])
    expected = np.zeros(6, dtype=complex)
    expected[0] = (4 + 2j) / 5
    np.testing.assert_allclose(actual, expected, rtol=1e-12, atol=1e-12)
    np.testing.assert_allclose(data["amplitude"], abs(expected), rtol=1e-12, atol=1e-12)
    # Phase at zero amplitude is undefined, rather than an asserted physical zero.
    assert data["phase_rad"][1:] == [None] * 5
    assert data["phase_rad"][0] == pytest.approx(math.atan2(2, 4), abs=1e-12)


def test_rebuild_is_deterministic(tmp_path):
    build = builder()
    first, second = tmp_path / "first", tmp_path / "second"
    build.build(tmp_path / "scratch-a", first)
    build.build(tmp_path / "scratch-b", second)
    assert {p.name for p in first.iterdir()} == set(build.DELIVERABLES)
    for name in build.DELIVERABLES:
        assert (first / name).read_bytes() == (second / name).read_bytes()
        assert (first / name).read_bytes() == (REPORT / name).read_bytes()


def test_cli_manifest_matches_written_files(tmp_path):
    output = tmp_path / "output"
    subprocess.run(
        [sys.executable, str(SCRIPT), "--scratch", str(tmp_path / "scratch"),
         "--output-dir", str(output)], cwd=ROOT, check=True, capture_output=True,
    )
    manifest = json.loads((output / "manifest.json").read_text())
    assert set(manifest["artifacts"]) == {"index.html", "changes.json", "README.md"}
    for name, digest in manifest["artifacts"].items():
        assert hashlib.sha256((output / name).read_bytes()).hexdigest() == digest
    assert str(SCRIPT.relative_to(ROOT)) in manifest["sources"]
    for name, digest in manifest["sources"].items():
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == digest
    assert (output / "manifest.json").read_bytes() == (REPORT / "manifest.json").read_bytes()


def test_every_displayed_worked_value_and_qualification(tmp_path):
    build = builder()
    data = build.calculate(tmp_path / "scratch")
    page = build.render(data)
    assert page == (REPORT / "index.html").read_text()
    assert len(re.findall(r'<h2 id="s\d+">', page)) == 10
    assert page.index('id="s1"') < page.index('id="revision-history"') < page.index('id="s2"')
    assert 'class="hs-doc"' in page and 'id="changes-panel"' in page
    assert "showSaveFilePicker" in page
    for mode in range(6):
        assert build.period_row(mode, data) in page
        assert build.response_row(mode, data) in page
    tables = Tables()
    tables.feed(page)
    periods = next(t for t in tables.tables if t[0][0] == "Mode j")
    responses = next(t for t in tables.tables if t[0][0] == "Mode")
    assert len(periods) == len(responses) == 7
    for j in range(1, 7):
        assert periods[j] == [str(j), *[f'{2 * math.pi / j:.4g}'] * 3, "H1, S1, C1"]
        expected = (4 + 2j) / 5 if j == 1 else 0j
        assert responses[j] == [
            str(j), f'{expected.real:.4g}', f'{expected.imag:.4g}',
            f'{abs(expected):.4g}', f'{math.atan2(2, 4):.4g}' if j == 1 else "undefined",
            "H2, S1, C1",
        ]
    payload = json.loads(re.search(
        r'<script id="calculation" type="application/json">(.*?)</script>', page, re.S,
    )[1])
    assert payload == data
    assert payload["inputs"] == build.INPUTS
    for name in ("campaign_convergence", "mooring_design_acceptance", "native_bem"):
        assert data["assessments"][name] == "NOT_EVALUATED"
        assert f'{name}: NOT_EVALUATED' in page
    assert "ARITHMETIC_QUALIFIED" in page
    assert "condition number one" in page
    assert "undefined" in page
    assert "1e-11" in page and "1e-12" in page


def test_inventory_covers_every_public_function():
    build = builder()
    inventory = build.inventory()
    actual = {(row["module"], row["function"]) for row in inventory}
    expected = set()
    for path in build.MODULE.glob("*.py"):
        for node in ast.parse(path.read_text()).body:
            if isinstance(node, ast.FunctionDef) and not node.name.startswith("_"):
                expected.add((path.stem, node.name))
    assert actual == expected
    assert {row["module"] for row in inventory} >= {
        "basis", "geometry", "hydrodynamics", "mooring", "response", "workflow", "report",
    }
    for row in inventory:
        assert row["purpose"] and row["contract"]
        if row["function"] not in ("natural_periods", "response_operators"):
            assert row["status"] == "NOT_EVALUATED"


def test_scratch_must_be_outside_checkout():
    with pytest.raises(ValueError, match="outside"):
        builder().calculate(ROOT / "temporary-calculation")
