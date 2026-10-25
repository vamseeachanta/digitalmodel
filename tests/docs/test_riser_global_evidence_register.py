"""Offline documentation contracts; source files are read as retained Git blobs.

No global-model test module is collected, imported or queried by these tests.
Expected evidence states follow issue 2530 Detection/Acceptance and the global
model knowledge pack revision 2 (2026), sections 1, 5, 8 and 10.
"""

from __future__ import annotations

import ast
import hashlib
import json
import re
import subprocess
from decimal import Decimal
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REGISTER = ROOT / "docs/domains/risers/global-model-evidence-register.json"
REPORT = REGISTER.with_suffix(".md")
TEST_ROOT = "tests/drilling_riser/global_model/"


def record():
    return json.loads(REGISTER.read_text(encoding="utf-8"))


def blob(revision, path):
    """Read only repository-relative paths at a full retained commit digest."""
    assert re.fullmatch(r"[0-9a-f]{40}", revision)
    assert not Path(path).is_absolute() and ".." not in Path(path).parts
    return subprocess.check_output(["git", "show", f"{revision}:{path}"], cwd=ROOT)


def test_no_missing_evidence_is_promoted_to_qualified_pass():
    data = record()
    assert data["qualification_status"] == "NOT_EVALUATED"
    assert data["target_version"] is None
    assert data["native_execution"] == "NOT_RUN"
    assert data["aqg_primary_text"] == "REFERENCED_ONLY"
    assert data["model_fingerprint"] is None
    assert set(data["appointments"]) == {"quality_manager", "deputy"}
    for appointment in data["appointments"].values():
        assert appointment == {
            "status": "UNCONFIRMED",
            "appointment_record": None,
            "independence_record": None,
        }
    for row in data["requirements"]:
        assert row["status"] == "NOT_EVALUATED"
        assert row["evidence_records"] == []
        assert row["acceptance_basis"] is None
        assert row["responsible_role"] and row["missing"] and row["required_records"]
    diffraction = next(r for r in data["requirements"] if r["id"] == "diffraction")
    assert diffraction["applicability"] == "UNDETERMINED"


def test_every_family_gate_and_issue_requirement_has_a_disposition():
    rows = record()["requirements"]
    assert len({r["id"] for r in rows}) == len(rows)
    assert {g for r in rows for g in r["gates"]} == {"G0", "G1", "G2", "G3", "G4", "G5"}
    assert {r["id"] for r in rows} == {
        "appointments",
        "version",
        "aqg",
        "inputs",
        "native_load",
        "hand_comparison",
        "offset_sweep",
        "all_objects",
        "short_dynamics",
        "diffraction",
        "appendix",
        "directional_stroke",
        "joint_angle",
        "clearance",
    }
    text = REPORT.read_text(encoding="utf-8")
    for row in rows:
        assert f"`{row['id']}`" in text
        assert row["sources"] and set(row["sources"]) <= set(record()["sources"])


def test_all_source_fingerprints_match_the_retained_revision():
    data = record()
    for source in data["sources"].values():
        raw = blob(data["snapshot_revision"], source["path"])
        assert hashlib.sha256(raw).hexdigest() == source["sha256"]
        for symbol in source["symbols"]:
            tree = ast.parse(raw)
            assert any(
                isinstance(node, (ast.FunctionDef, ast.ClassDef))
                and node.name == symbol
                for node in ast.walk(tree)
            ), symbol


def test_inventory_covers_every_retained_test_file_without_importing_it():
    data = record()
    paths = subprocess.check_output(
        [
            "git",
            "ls-tree",
            "-r",
            "--name-only",
            data["snapshot_revision"],
            "--",
            TEST_ROOT,
        ],
        cwd=ROOT,
        text=True,
    ).splitlines()
    expected = {
        p for p in paths if Path(p).name.startswith("test_") and p.endswith(".py")
    }
    rows = data["test_inventory"]
    assert {r["path"] for r in rows} == expected
    assert len(rows) == len(expected)
    for row in rows:
        raw = blob(data["snapshot_revision"], row["path"])
        assert hashlib.sha256(raw).hexdigest() == row["sha256"]
        tree = ast.parse(raw)
        functions = [
            n
            for n in tree.body
            if isinstance(n, ast.FunctionDef) and n.name.startswith("test_")
        ]
        assert row["named_functions"] == [n.name for n in functions]
        probes = sorted(
            n.lineno
            for n in ast.walk(tree)
            if isinstance(n, ast.Call)
            and isinstance(n.func, ast.Attribute)
            and n.func.attr == "available"
        )
        assert row["availability_probe_lines"] == probes
        module_solver = any(
            isinstance(n, ast.Assign)
            and any(isinstance(t, ast.Name) and t.id == "pytestmark" for t in n.targets)
            and "pytest.mark.solver" in ast.unparse(n.value)
            for n in tree.body
        )
        native = [
            n.name
            for n in functions
            if module_solver
            or any("pytest.mark.solver" in ast.unparse(d) for d in n.decorator_list)
        ]
        assert row["solver_marked_functions"] == native
        expected_class = (
            "MIXED"
            if native and len(native) < len(functions)
            else ("NATIVE_ONLY" if native else "NO_DIRECT_NATIVE_MARKER")
        )
        assert row["classification"] == expected_class
        assert row["execution_status"] == "NOT_RUN"


def test_report_inventory_rows_agree_with_the_snapshot_register():
    text = REPORT.read_text(encoding="utf-8")
    for row in record()["test_inventory"]:
        expected = (
            f"| `{Path(row['path']).name}` | {len(row['named_functions'])} | "
            f"{len(row['solver_marked_functions'])} | "
            f"{len(row['availability_probe_lines'])} | {row['classification']} | NOT_RUN |"
        )
        assert expected in text
    assert "named functions are not parametrized case counts" in text
    assert "transitive import or fixture safety" in text


def test_example_is_independent_arithmetic_and_retains_missing_qualification():
    """Constructed inputs, not design allowables: demand 104 N, reference 100 N.

    Hand calculation per qualification.py evaluate_gate's documented equation:
    (104 - 100) / abs(100) = 4 / 100 = 0.04. The caller selects tolerance 0.05
    for this arithmetic illustration. A scalar PASS has no native gate evidence;
    the qualification answer must therefore remain NOT_EVALUATED.
    """
    example = record()["worked_example"]
    assert (
        example["basis"] == "constructed arithmetic inputs; no design acceptance limit"
    )
    assert example["demand_n"] == 104 and example["reference_n"] == 100
    deviation = (Decimal(104) - Decimal(100)) / abs(Decimal(100))
    assert deviation == Decimal("0.04")
    assert Decimal(str(example["signed_deviation"])) == deviation
    assert Decimal(str(example["tolerance"])) == Decimal("0.05")
    assert example["scalar_status"] == "PASS"
    assert example["qualification_status"] == "NOT_EVALUATED"
    assert example["evidence_records"] == []


@pytest.mark.parametrize(
    "field",
    [
        "native_load",
        "hand_comparison",
        "offset_sweep",
        "all_objects",
        "short_dynamics",
        "appendix",
    ],
)
def test_native_requirements_bind_version_fingerprint_and_review(field):
    row = next(r for r in record()["requirements"] if r["id"] == field)
    assert {"target_version", "model_fingerprint", "independent_review"} <= set(
        row["required_records"]
    )


@pytest.mark.parametrize(
    "field, required",
    [
        (
            "all_objects",
            {
                "expected_objects",
                "native_objects",
                "properties",
                "connections",
                "ranges",
                "reconciliation",
            },
        ),
        (
            "offset_sweep",
            {
                "headings",
                "offsets",
                "design_maximum",
                "reactions",
                "hand_first_step",
                "compression",
            },
        ),
        (
            "directional_stroke",
            {
                "design_basis",
                "datum",
                "extension_travel",
                "collapse_travel",
                "centering",
                "channel_sign",
            },
        ),
        (
            "joint_angle",
            {
                "design_basis",
                "joint_id",
                "axes",
                "sign",
                "operational_condition",
                "angle_limits",
            },
        ),
        (
            "clearance",
            {
                "design_basis",
                "obstruction_id",
                "opening_geometry",
                "pivot",
                "swept_envelope",
                "motions",
            },
        ),
    ],
)
def test_issue_detection_records_cannot_be_replaced_by_line_only_readback(
    field, required
):
    """Required record fields follow issue 2530 Detection, not calculated results."""
    row = next(r for r in record()["requirements"] if r["id"] == field)
    assert required <= set(row["required_records"])


def test_local_document_links_resolve_and_index_exposes_the_register():
    text = REPORT.read_text(encoding="utf-8")
    for target in re.findall(r"\]\(([^)]+)\)", text):
        if not target.startswith(("https:", "#")):
            assert (REPORT.parent / target.split("#")[0]).is_file(), target
    assert REPORT.name in (REPORT.parent / "risers.md").read_text(encoding="utf-8")
    assert text.index("## Revision history") < text.index("## Scope")
