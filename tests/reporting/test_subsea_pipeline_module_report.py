# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise report arithmetic, qualification boundaries and reproducible output.

Independent basis: polar area and second-moment integrals over radii 1..2 m
give A=3*pi, Ai=pi, Ao=4*pi m^2 and I=15*pi/4 m^4; t=1 m.
Uniform strain +/-1/5000 integrated over 1000 m gives +/-1/5 m.
Arbitrary supplied capacity 100 MPa times 4/5, 9/10, 3/5 gives 80/90/60 MPa.
These synthetic calculations supply every numeric expectation below. The
1e-12 comparison tolerance permits floating-point rounding only. Document
structure, SHA-256 coverage and output names follow the module report contract.
"""

import ast
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_subsea_pipeline_module_report.py"
REPORT = ROOT / "docs/reports/modules/subsea_pipeline"
DELIVERABLES = {"index.html", "changes.json", "manifest.json", "README.md"}
REBUILD = (
    "PYTHONPATH=src python scripts/reports/build_subsea_pipeline_module_report.py "
    '--scratch "$REPORT_SCRATCH"'
)


@pytest.fixture(params=["utf-8", "cp1252"], autouse=True)
def simulated_text_locale(request, monkeypatch):
    """Exercise report reads with UTF-8 and a legacy locale default."""
    original = Path.read_text

    def read_text(path, encoding=None, errors=None):
        return original(
            path,
            encoding=request.param if encoding is None else encoding,
            errors=errors,
        )

    monkeypatch.setattr(Path, "read_text", read_text)


def builder():
    """Load the standalone builder without invoking a CLI or workflow."""
    assert SCRIPT.is_file(), "The module report builder is missing"
    spec = importlib.util.spec_from_file_location("subsea_pipeline_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_annulus_payload_matches_integrals():
    """Catch omitted units, fields, dimension routes or substituted geometry."""
    data = builder().build_payload()["annulus"]
    assert data["inputs"] == {
        "outer_diameter": 4.0,
        "inner_diameter": 2.0,
        "wall_thickness": 1.0,
    }
    expected = {
        **data["inputs"],
        "A": 3 * math.pi,
        "Ai": math.pi,
        "Ao": 4 * math.pi,
        "I": 15 * math.pi / 4,
    }
    assert set(data["routes"]) == {"od_id", "od_t", "id_t", "od_id_t"}
    for result in data["routes"].values():
        assert result == pytest.approx(expected, rel=1e-12, abs=0.0)
    assert data["section"] == pytest.approx(
        {key: expected[key] for key in ("A", "Ai", "Ao", "I")}, rel=1e-12, abs=0.0
    )
    assert data["wall_thickness"] == 1.0
    assert data["derived_inner_diameter"] == 2.0
    assert data["derived_outer_diameter"] == 4.0
    assert data["consistent"] is True
    assert data["units"] == {
        "outer_diameter": "m",
        "inner_diameter": "m",
        "wall_thickness": "m",
        "A": "m^2",
        "Ai": "m^2",
        "Ao": "m^2",
        "I": "m^4",
    }
    assert data["evaluation"] == "HAND_CALCULATION_CHECKED"
    assert data["basis"] == "A"


def test_growth_payload_matches_uniform_strain():
    """Catch reversed cooling, coefficient substitution or unsupported restraint."""
    data = builder().build_payload()["growth"]
    assert data["inputs"] == {"length": 1000.0, "alpha": 1 / 100000}
    assert data["units"] == {
        "length": "m",
        "alpha": "K^-1",
        "temperature_change": "K",
        "strain": "1",
        "growth": "m",
    }
    assert len(data["cases"]) == 2
    for case, temperature, strain, growth in zip(
        data["cases"], [20.0, -20.0], [1 / 5000, -1 / 5000], [1 / 5, -1 / 5]
    ):
        assert case == pytest.approx(
            {"temperature_change": temperature, "strain": strain, "growth": growth},
            rel=1e-12,
            abs=0.0,
        )
    assert data["evaluation"] == "HAND_CALCULATION_CHECKED"
    assert data["buckling_and_walking"] == "NOT_EVALUATED"
    assert data["basis"] == "B"


def test_pressure_factor_payload_matches_supplied_arithmetic():
    """Catch accidental use of a calculated burst capacity or code factors."""
    data = builder().build_payload()["pressure_factors"]
    assert data["inputs"] == {
        "base_capacity": 100.0,
        "factors": {
            "design": 4 / 5,
            "incidentalPressure": 9 / 10,
            "hydroStaticTest": 3 / 5,
        },
    }
    assert data["results"] == pytest.approx(
        {"design": 80.0, "accidental": 90.0, "test": 60.0}, rel=1e-12, abs=0.0
    )
    assert data["units"] == {"base_capacity": "MPa", "factors": "1", "results": "MPa"}
    assert data["evaluation"] == "ARITHMETIC_ONLY"
    assert data["capacity_qualification"] == "NOT_EVALUATED"
    assert data["factor_qualification"] == "NOT_EVALUATED"
    assert data["basis"] == "C"


def _assert_manifest(directory):
    """Verify exact source and written-artifact bytes with an external digest."""
    manifest = json.loads((directory / "manifest.json").read_text(encoding="utf-8"))
    assert manifest["algorithm"] == "sha256"
    assert set(manifest["artifacts"]) == DELIVERABLES - {"manifest.json"}
    assert (
        manifest["self_digest"]
        == "Excluded to avoid a self-referential digest; compare manifest.json bytes externally."
    )
    for name, digest in manifest["artifacts"].items():
        assert digest == hashlib.sha256((directory / name).read_bytes()).hexdigest(), (
            f"Rebuild with {REBUILD}"
        )
    sources = set(manifest["sources"])
    pipeline_sources = {
        path.relative_to(ROOT).as_posix()
        for path in (ROOT / "src/digitalmodel/subsea/pipeline").rglob("*.py")
    }
    assert pipeline_sources <= sources
    assert {
        SCRIPT.relative_to(ROOT).as_posix(),
        "tests/subsea/pipeline/test_module_report_hand_values.py",
        "tests/reporting/test_subsea_pipeline_module_report.py",
        "src/digitalmodel/units.py",
    } <= sources
    for name, digest in manifest["sources"].items():
        assert not Path(name).is_absolute()
        assert digest == hashlib.sha256((ROOT / name).read_bytes()).hexdigest(), (
            f"Rebuild with {REBUILD}"
        )
    return manifest


def test_rebuild_is_deterministic(tmp_path):
    """Catch ambient paths, timestamps, incomplete output or stale committed bytes."""
    build = builder()
    first, second = tmp_path / "first", tmp_path / "second"
    build.build_report(scratch=tmp_path / "scratch-a", output_dir=first)
    build.build_report(scratch=tmp_path / "scratch-b", output_dir=second)
    assert {path.name for path in first.iterdir()} == DELIVERABLES
    assert {path.name for path in second.iterdir()} == DELIVERABLES
    for name in DELIVERABLES:
        assert (first / name).read_bytes() == (second / name).read_bytes(), (
            f"Rebuild with {REBUILD}"
        )
        assert (first / name).read_bytes() == (REPORT / name).read_bytes(), (
            f"Rebuild with {REBUILD}"
        )
    assert _assert_manifest(first) == _assert_manifest(second), (
        f"Rebuild with {REBUILD}"
    )
    report = (first / "index.html").read_text(encoding="utf-8")
    assert re.findall(r'<h2 id="s\d+">(.*?)</h2>', report) == [
        "1. Executive Summary",
        "2. Introduction",
        "3. Design Data and Basis",
        "4. Analysis Methodology",
        "5. Detailed Results",
        "6. Key Findings",
        "7. Limitations and Not Covered",
        "8. Recommendations",
        "9. References and Appendices",
        "10. Acronyms and Review Notes",
    ]
    summary = report.split('<h2 id="s1">', 1)[1].split('<h2 id="s2">', 1)[0]
    assert re.search(r"</p>\s*<h3>Revision history</h3>", summary)
    assert 'id="change-panel"' in summary
    assert "free thermal growth does not predict buckling or walking" in report
    assert "factor multiplication does not qualify its supplied capacity" in report
    payload = json.loads(
        re.search(
            r'<script id="calculation" type="application/json">(.*?)</script>',
            report,
            re.S,
        )[1]
    )
    assert payload == build.build_payload()
    changes = json.loads((first / "changes.json").read_text(encoding="utf-8"))
    assert len(changes) == 1
    assert set(changes[0]) == {"revision", "date", "change"}
    for value in changes[0].values():
        assert value in summary


def test_cli_manifest_matches_written_files(tmp_path):
    """Exercise the CLI in a different cwd, preserving caller Git bindings."""
    builder()
    environment = dict(os.environ, PYTHONPATH=str(ROOT / "src"))
    # Hostile bindings are inert: the builder locates sources from its own file.
    environment.update(
        GIT_DIR=str(tmp_path / "absent-git"), GIT_WORK_TREE=str(tmp_path)
    )
    destination = tmp_path / "cli-output"
    subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--scratch",
            str(tmp_path / "cli-scratch"),
            "--output-dir",
            str(destination),
        ],
        cwd=tmp_path,
        env=environment,
        check=True,
        capture_output=True,
        text=True,
    )
    _assert_manifest(destination)
    assert {path.name for path in destination.iterdir()} == DELIVERABLES
    for name in DELIVERABLES:
        assert (destination / name).read_bytes() == (REPORT / name).read_bytes(), (
            f"Rebuild with {REBUILD}"
        )


def test_inventory_covers_public_methods_without_transferring_qualification():
    """Every public declaration has a locator; engineering methods stay unevaluated."""
    inventory = builder().build_payload()["methods"]
    expected = set()
    for source in (ROOT / "src/digitalmodel/subsea/pipeline").rglob("*.py"):
        path = source.relative_to(ROOT).as_posix()
        for node in ast.parse(source.read_text(encoding="utf-8")).body:
            if isinstance(node, ast.FunctionDef) and not node.name.startswith("_"):
                expected.add((path, node.name, node.lineno))
            if isinstance(node, ast.ClassDef) and not node.name.startswith("_"):
                for method in node.body:
                    if isinstance(
                        method, ast.FunctionDef
                    ) and not method.name.startswith("_"):
                        expected.add(
                            (path, f"{node.name}.{method.name}", method.lineno)
                        )
    assert {(row["path"], row["symbol"], row["line"]) for row in inventory} == expected
    assert {row["category"] for row in inventory} == {
        "geometry",
        "thermal strain",
        "buckling",
        "span",
        "pressure",
        "workflow",
    }
    for row in inventory:
        assert row["evidence_needed"]
        if row["category"] in {"buckling", "span", "workflow"}:
            assert row["evaluation"] == "NOT_EVALUATED"
        if (
            row["category"] == "pressure"
            and row["symbol"] != "APISTD2RDCalculations.calculate_design_pressures"
        ):
            assert row["evaluation"] == "NOT_EVALUATED"


@pytest.mark.parametrize("scratch", [".", "docs/reports/modules/subsea_pipeline"])
def test_scratch_inside_checkout_is_rejected_before_writes(tmp_path, scratch):
    """An output path inside the checkout cannot turn scratch into retained data."""
    with pytest.raises(ValueError, match="outside the checkout"):
        builder().build_report(scratch=ROOT / scratch, output_dir=tmp_path / "out")
    assert not (tmp_path / "out").exists()
