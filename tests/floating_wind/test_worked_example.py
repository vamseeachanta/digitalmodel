"""Solver-free Phase A evidence checks; all fixture text is synthetic.

Expected dependency names and line numbers are obtained by counting the literal
lines below. No fixture represents solver output or an engineering limit.
"""

from pathlib import Path

import pytest

from digitalmodel.floating_wind import worked_example as subject

ROOT = Path(__file__).resolve().parents[2]


def test_external_references_keep_repeated_uses_and_ignore_provenance(tmp_path):
    model = tmp_path / "model.yml"
    model.write_text(
        "# File: original.yml\nFileName: control.py\n"
        "  FileName: control.py\n  ControllerDLL: missing.dll\n"
        '  InputFile: "controller.IN"\nincludefile: includes/data.yml\n',
        encoding="utf-8",
    )
    held = tmp_path / "elsewhere" / "CONTROL.PY"
    held.parent.mkdir()
    held.write_text("# synthetic controller, never executed\n")
    rows = subject.external_references(tmp_path, model)
    assert [(r["file"], r["line"]) for r in rows] == [
        ("control.py", 2),
        ("control.py", 3),
        ("missing.dll", 4),
        ("controller.IN", 5),
        ("includes/data.yml", 6),
    ]
    assert rows[0]["held_paths"] == ["elsewhere/CONTROL.PY"]
    assert rows[0]["status"] == "HELD_ELSEWHERE_COMPATIBILITY_UNVERIFIED"
    assert rows[2]["status"] == "MISSING"


def test_include_requires_exact_relative_path_not_basename(tmp_path):
    model = tmp_path / "master.yml"
    model.write_text("includefile: includes/data.yml\n")
    (tmp_path / "data.yml").write_text("{}")
    assert subject.external_references(tmp_path, model)[0]["status"] == "MISSING"


def test_scan_refuses_model_outside_root(tmp_path):
    with pytest.raises(ValueError, match="outside"):
        subject.external_references(tmp_path / "child", tmp_path / "model.yml")


def test_citation_checks_key_and_transcribed_value():
    lines = ["WaveHs: 7", "StageDuration:", "  - 8", "  - 200"]
    assert subject.cited_value(lines, 1) == "7"
    assert subject.cited_value(lines, 2) == "8; 200"
    with pytest.raises(ValueError, match="line"):
        subject.cited_value(lines, 99)


def test_input_audit_detects_changed_value_and_key():
    html = "<tr><td>Wave height</td><td>7 (L02 line 1: WaveHs)</td></tr>"
    assert subject.audit_input_echo(html, {"L02": ["WaveHs: 7"]}) == []
    assert "value" in subject.audit_input_echo(html, {"L02": ["WaveHs: 9"]})[0]
    assert "key" in subject.audit_input_echo(html, {"L02": ["WindSpeed: 7"]})[0]


def test_result_audit_rejects_numeric_or_unmarked_results():
    good = (
        '<tr class="result"><td>Surge natural period</td>'
        "<td>[RESULT: not calculated]</td></tr>"
    )
    assert subject.audit_result_rows(good) == []
    assert subject.audit_result_rows(good.replace("[RESULT: not calculated]", "12 s"))
    assert subject.audit_result_rows(
        good.replace("[RESULT: not calculated]", "[RESULT: 12 s]")
    )
    assert subject.audit_result_rows("<p>No results table</p>")


def test_repository_phase_a_evidence():
    evidence = subject.audit_phase_a(ROOT)
    assert evidence["errors"] == []
    assert evidence["solver_status"] == "NOT_EVALUATED"
    assert evidence["execution_ready"] is False
    models = {m["model"]: m for m in evidence["models"]}
    # Source: L02 lines 53, 59; K03 lines 58, 29795, 29796.
    assert [r["file"] for r in models["L02"]["references"]] == [
        "PythonController.py",
        "PythonController.py",
    ]
    assert {r["file"] for r in models["K03"]["references"]} == {
        "BladedControllerWrapper.dll",
        "libdiscon.dll",
        "DISCON-UMaineSemi.IN",
    }
    assert (
        len(evidence["case_families"]) == 11
    )  # Worksheet: statics + six axes + four families.
    assert all(c["status"] == "NOT_EVALUATED" for c in evidence["case_families"])
    assert all(len(m["sha256"]) == 64 for m in evidence["models"])


def test_cli_failure_is_portable(tmp_path, capsys):
    assert subject.main(["--root", str(tmp_path)]) == 1
    output = capsys.readouterr().out
    assert str(tmp_path) not in output
    assert "FileNotFoundError" in output


def test_cli_success_reports_document_audit_only(capsys):
    import json

    assert subject.main(["--root", str(ROOT)]) == 0
    output = capsys.readouterr().out
    result = json.loads(output)
    assert result["execution_ready"] is False
    assert result["solver_status"] == "NOT_EVALUATED"
    assert str(ROOT) not in output


def test_external_symlink_is_not_a_holding(tmp_path):
    root = tmp_path / "repository"
    root.mkdir()
    external = tmp_path / "control.py"
    external.write_text("# never executed")
    (root / "control.py").symlink_to(external)
    model = root / "model.yml"
    model.write_text("FileName: control.py\n")
    row = subject.external_references(root, model)[0]
    assert row["status"] == "MISSING"
    assert row["held_paths"] == []


def test_inventory_rejects_false_held_claim_and_missing_occurrence():
    references = [
        {"file": "missing.dll", "line": 4, "held_paths": [], "status": "MISSING"}
    ]
    false = "| K03 | `missing.dll` | K03 L4 | function | held | source |"
    assert subject.audit_inventory(false, "K03", references)
    good = false.replace("| held |", "| not held (GAP) |")
    assert subject.audit_inventory(good, "K03", references) == []
    assert subject.audit_inventory(good.replace("K03 L4", "K03 L5"), "K03", references)


@pytest.mark.parametrize(
    "cell",
    [
        "<td><strong>9</strong> (L02 line 1: WaveHs)</td>",
        '<td class="value">9 (L02 line 1: WaveHs)</td>',
        "<td>7</td>",
        "<th>9 (L02 line 1: WaveHs)</th>",
    ],
)
def test_input_audit_validates_every_model_cell(cell):
    html = (
        '<section id="as-held"><table><tr><th>Input</th>'
        "<th>L02 as held</th><th>K03 as held</th></tr>"
        "<tr><td>Wave height</td>"
        + cell
        + "<td>4 (K03 line 1: WaveHs)</td></tr></table></section>"
    )
    assert subject.audit_input_echo(html, {"L02": ["WaveHs: 7"], "K03": ["WaveHs: 4"]})


def test_input_audit_accepts_inline_markup_and_attributes():
    html = (
        '<tr><td>Wave height</td><td class="value"><strong>7</strong> '
        "(<span>L02 line 1: WaveHs</span>)</td></tr>"
    )
    assert subject.audit_input_echo(html, {"L02": ["WaveHs: 7"]}) == []


def test_input_audit_requires_each_model_column():
    html = (
        '<section id="as-held"><table><tr><td>Wave height</td>'
        "<td>7 (L02 line 1: WaveHs)</td></tr></table></section>"
    )
    assert subject.audit_input_echo(html, {"L02": ["WaveHs: 7"]})


def test_input_audit_does_not_skip_value_rows_marked_as_headers():
    html = (
        '<section id="as-held"><table>'
        "<tr><th>Wave height</th><th>9 (L02 line 1: WaveHs)</th>"
        "<th>4 (K03 line 1: WaveHs)</th></tr>"
        "<tr><td>Wave height</td><td>7 (L02 line 1: WaveHs)</td>"
        "<td>4 (K03 line 1: WaveHs)</td></tr></table></section>"
    )
    assert subject.audit_input_echo(html, {"L02": ["WaveHs: 7"], "K03": ["WaveHs: 4"]})


@pytest.mark.parametrize("retained", [True, False])
def test_inventory_rejects_removed_occurrence_and_dependency(retained):
    inventory = "| L02 | `control.py` | L02 L2; L02 L3 | function | not held | source |"
    references = [
        {"file": "control.py", "line": 3, "held_paths": [], "status": "MISSING"}
    ]
    assert subject.audit_inventory(inventory, "L02", references if retained else [])


def test_inventory_reconciles_holding_paths_in_both_directions():
    inventory = (
        "| L02 | `control.py` | L02 L2 | function | held elsewhere: "
        "[docs/control.py](../../../../control.py); compatibility unverified | source |"
    )
    references = [
        {
            "file": "control.py",
            "line": 2,
            "held_paths": ["docs/control.py"],
            "status": "HELD_ELSEWHERE_COMPATIBILITY_UNVERIFIED",
        }
    ]
    assert subject.audit_inventory(inventory, "L02", references) == []
    stale = inventory.replace(
        "; compatibility", "; [stale](../../../../stale.py); compatibility"
    )
    assert subject.audit_inventory(stale, "L02", references)
    references[0]["held_paths"] = []
    references[0]["status"] = "MISSING"
    stale = inventory.replace("held elsewhere:", "not held (GAP):")
    assert subject.audit_inventory(stale, "L02", references)


@pytest.mark.parametrize(
    "mutation",
    ["markup", "attribute", "citation", "occurrence", "dependency", "holding"],
)
def test_complete_audit_rejects_stale_evidence(monkeypatch, mutation):
    read_text, read_bytes = Path.read_text, Path.read_bytes

    def changed_text(path, *args, **kwargs):
        if path == ROOT / subject.MODELS["L02"]:
            return changed_bytes(path).decode("utf-8-sig")
        text = read_text(path, *args, **kwargs)
        if path == ROOT / subject.DOCUMENTS / "report-skeleton.html":
            old = "<td>7 (L02 line 104: WaveHs)</td>"
            replacements = {
                "markup": "<td><strong>9</strong> (L02 line 104: WaveHs)</td>",
                "attribute": '<td class="value">9 (L02 line 104: WaveHs)</td>',
                "citation": "<td>7</td>",
            }
            text = text.replace(old, replacements.get(mutation, old))
        if (
            mutation == "holding"
            and path == ROOT / subject.DOCUMENTS / "external-files-inventory.md"
        ):
            text = text.replace(
                "; compatibility unverified |",
                "; [stale](stale.py); compatibility unverified |",
            )
        return text

    def changed_bytes(path):
        payload = read_bytes(path)
        if path == ROOT / subject.MODELS["L02"] and mutation in {
            "occurrence",
            "dependency",
        }:
            lines = payload.splitlines(keepends=True)
            # Raw L02 controller declarations: lines 53 and 59. Keep all locators fixed.
            for number in [53] if mutation == "occurrence" else [53, 59]:
                lines[number - 1] = b"# declaration removed\n"
            payload = b"".join(lines)
        return payload

    monkeypatch.setattr(Path, "read_text", changed_text)
    monkeypatch.setattr(Path, "read_bytes", changed_bytes)
    assert subject.audit_phase_a(ROOT)["errors"]
