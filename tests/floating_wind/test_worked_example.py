"""Solver-free Phase A evidence checks; all fixture text is synthetic.

Expected dependency names and line numbers are obtained by counting the literal
lines below. No fixture represents solver output or an engineering limit.
"""

from pathlib import Path

import pytest

from digitalmodel.floating_wind import worked_example as subject

ROOT = Path(__file__).resolve().parents[2]
RESULT_HEADER = (
    "<tr><th>Result field</th><th>Result and governing case</th>"
    "<th>Criterion / comparator</th><th>Evidence / disposition</th></tr>"
)


def result_table(rows):
    return '<section id="results"><table>' + RESULT_HEADER + rows + "</table></section>"


@pytest.fixture(scope="module")
def repository_holdings():
    return subject._holdings(ROOT)


@pytest.fixture(autouse=True)
def reuse_repository_holdings(monkeypatch, repository_holdings):
    scan = subject._holdings
    monkeypatch.setattr(
        subject,
        "_holdings",
        lambda root: repository_holdings if root == ROOT else scan(root),
    )


def test_external_references_keep_repeated_uses_and_ignore_provenance(tmp_path):
    model = tmp_path / "model.yml"
    model.write_text(
        "# File: original.yml\nFileName: control.py\n"
        "  FileName: control.py\n  ControllerDLL: missing.dll\n"
        '  InputFile: "controller.IN"\nincludefile: includes/data.yml\n',
        encoding="utf-8",
        newline="\n",
    )
    held = tmp_path / "elsewhere" / "CONTROL.PY"
    held.parent.mkdir()
    held.write_text(
        "# synthetic controller, never executed\n", encoding="utf-8", newline="\n"
    )
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
    model.write_text("includefile: includes/data.yml\n", encoding="utf-8", newline="\n")
    (tmp_path / "data.yml").write_text("{}", encoding="utf-8", newline="\n")
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
        "<td>[RESULT: not calculated]</td>"
        "<td>[RESULT: comparator unselected]</td>"
        "<td>[RESULT: evidence absent]</td></tr>"
    )
    html = result_table(good)
    assert subject.audit_result_rows(html) == []
    assert subject.audit_result_rows(html.replace("[RESULT: not calculated]", "12 s"))
    assert subject.audit_result_rows(
        html.replace("[RESULT: not calculated]", "[RESULT: 12 s]")
    )
    assert subject.audit_result_rows("<p>No results table</p>")


@pytest.mark.parametrize(
    "row_tag",
    [
        '<tr class="result" id="surge">',
        '<tr class="result highlighted">',
        "<tr id='surge' class='highlighted result'>",
    ],
)
@pytest.mark.parametrize("cell_tag", ["td", "th"])
def test_result_audit_accepts_placeholder_rows_with_attributes(row_tag, cell_tag):
    html = (
        row_tag + '<td scope="row">Surge natural period</td>'
        f'<{cell_tag} class="value"><strong>[RESULT: not calculated]</strong>'
        f"</{cell_tag}><{cell_tag}>[RESULT: comparator unselected]</{cell_tag}>"
        f"<{cell_tag}>[RESULT: evidence absent]</{cell_tag}></tr>"
    )
    assert subject.audit_result_rows(result_table(html)) == []


@pytest.mark.parametrize(
    "cell",
    [
        '<td class="value">12 s</td>',
        "<td><strong>12</strong> s</td>",
        '<td class="value">[RESULT: &#49;2 s]</td>',
        '<th scope="row">12 s</th>',
    ],
)
def test_result_audit_checks_every_value_cell(cell):
    html = (
        '<tr class="result"><td>Surge natural period</td>'
        + cell
        + "<td>[RESULT: comparator unselected]</td>"
        "<td>[RESULT: evidence absent]</td></tr>"
    )
    assert subject.audit_result_rows(result_table(html))


@pytest.mark.parametrize(
    "mutation", ["renamed-section", "duplicate-section", "header-row"]
)
def test_complete_audit_rejects_results_structure_bypasses(monkeypatch, mutation):
    read_text = Path.read_text

    def changed_text(path, *, encoding="utf-8", **kwargs):
        text = read_text(path, encoding=encoding, **kwargs)
        if path == ROOT / subject.DOCUMENTS / "report-skeleton.html":
            if mutation == "renamed-section":
                text = text.replace('id="results"', 'id="archived-results"')
                text = text.replace(
                    '<tr class="result"><td>Surge natural period</td>'
                    "<td>[RESULT: not calculated; case unselected]</td>",
                    "<tr><td>Surge natural period</td><td>12.4 s</td>",
                )
            elif mutation == "duplicate-section":
                text += '<section id="results"></section>'
            else:
                text = text.replace(
                    '<section id="results">',
                    '<section id="results"><table><tr><th>Surge natural period</th>'
                    "<th>12.4 s</th></tr></table>",
                )
        return text

    monkeypatch.setattr(Path, "read_text", changed_text)
    evidence = subject.audit_phase_a(ROOT)
    assert any("report" in error for error in evidence["errors"])
    assert evidence["execution_ready"] is False


@pytest.mark.parametrize("mutation", ["missing", "duplicate", "changed-label"])
def test_result_audit_requires_exact_header_once(mutation):
    html = result_table(
        '<tr class="result"><td>Surge natural period</td>'
        "<td>[RESULT: not calculated]</td><td>[RESULT: comparator unselected]</td>"
        "<td>[RESULT: evidence absent]</td></tr>"
    )
    replacement = {
        "missing": "",
        "duplicate": RESULT_HEADER * 2,
        "changed-label": RESULT_HEADER.replace("Result field", "Other field"),
    }[mutation]
    assert subject.audit_result_rows(html.replace(RESULT_HEADER, replacement))


@pytest.mark.parametrize("values", [1, 2, 4])
def test_result_audit_requires_all_four_columns(values):
    row = (
        '<tr class="result"><td>Surge natural period</td>'
        + "<td>[RESULT: evidence absent]</td>" * values
        + "</tr>"
    )
    assert subject.audit_result_rows(result_table(row))


def test_result_audit_rejects_marked_rows_outside_results():
    row = (
        '<tr class="result"><td>Surge natural period</td>'
        "<td>[RESULT: not calculated]</td><td>[RESULT: comparator unselected]</td>"
        "<td>[RESULT: evidence absent]</td></tr>"
    )
    assert subject.audit_result_rows(result_table(row) + "<table>" + row + "</table>")


@pytest.mark.parametrize("cell", ["[RESULT: not calculated] extra ]", "[RESULT: ]"])
def test_result_audit_rejects_incomplete_or_mixed_placeholders(cell):
    html = result_table(
        '<tr class="result"><td>Surge natural period</td>'
        f"<td>{cell}</td><td>[RESULT: comparator unselected]</td>"
        "<td>[RESULT: evidence absent]</td></tr>"
    )
    assert subject.audit_result_rows(html)


@pytest.mark.parametrize(
    "row_tag, value_cell",
    [
        ('<tr class="result">', '<td class="value">12 s</td>'),
        ('<tr class="result" id="surge">', "<td>12 s</td>"),
        ('<tr class="result highlighted">', "<td>12 s</td>"),
        ("<tr>", "<td>12 s</td>"),
        ('<tr class="highlighted">', "<td>12 s</td>"),
    ],
)
def test_complete_audit_rejects_numeric_result_markup(
    monkeypatch, capsys, row_tag, value_cell
):
    read_text = Path.read_text

    def changed_text(path, *, encoding="utf-8", **kwargs):
        text = read_text(path, encoding=encoding, **kwargs)
        if path == ROOT / subject.DOCUMENTS / "report-skeleton.html":
            old = (
                '<tr class="result"><td>Surge natural period</td>'
                "<td>[RESULT: not calculated; case unselected]</td>"
            )
            assert text.count(old) == 1
            text = text.replace(
                old, row_tag + "<td>Surge natural period</td>" + value_cell
            )
        return text

    monkeypatch.setattr(Path, "read_text", changed_text)
    assert subject.main(["--root", str(ROOT)]) == 1
    import json

    evidence = json.loads(capsys.readouterr().out)
    assert any("numeric result" in error for error in evidence["errors"])
    assert evidence["execution_ready"] is False


def test_result_section_requires_class_even_on_placeholder_rows():
    row = (
        "<tr><td>Period</td><td>[RESULT: not calculated]</td>"
        "<td>[RESULT: comparator unselected]</td><td>[RESULT: evidence absent]</td></tr>"
    )
    marked = row.replace("<tr>", '<tr class="result">')
    assert subject.audit_result_rows(result_table(marked)) == []
    assert subject.audit_result_rows(result_table(row + marked))


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
    external.write_text("# never executed", encoding="utf-8", newline="\n")
    try:
        (root / "control.py").symlink_to(external)
    except OSError:
        pytest.skip("symlink creation is unavailable")
    model = root / "model.yml"
    model.write_text("FileName: control.py\n", encoding="utf-8", newline="\n")
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

    def changed_text(path, *, encoding="utf-8", **kwargs):
        if path == ROOT / subject.MODELS["L02"]:
            return changed_bytes(path).decode("utf-8-sig")
        text = read_text(path, encoding=encoding, **kwargs)
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


@pytest.mark.parametrize("end", [38, 40, 36])
def test_complete_audit_checks_stage_range_end(monkeypatch, end):
    read_text = Path.read_text

    def changed_text(path, *, encoding="utf-8", **kwargs):
        text = read_text(path, encoding=encoding, **kwargs)
        if path == ROOT / subject.DOCUMENTS / "report-skeleton.html":
            text = text.replace("L02 lines 37-39:", f"L02 lines 37-{end}:")
        return text

    monkeypatch.setattr(Path, "read_text", changed_text)
    assert subject.audit_phase_a(ROOT)["errors"]


@pytest.mark.parametrize("copies", [0, 2])
def test_complete_audit_requires_each_input_row_once(monkeypatch, copies):
    read_text = Path.read_text

    def changed_text(path, *, encoding="utf-8", **kwargs):
        text = read_text(path, encoding=encoding, **kwargs)
        if path == ROOT / subject.DOCUMENTS / "report-skeleton.html":
            row = next(
                row
                for row in text.splitlines(keepends=True)
                if row.startswith("<tr><td>Wave significant height")
            )
            text = text.replace(row, row * copies)
        return text

    monkeypatch.setattr(Path, "read_text", changed_text)
    assert subject.audit_phase_a(ROOT)["errors"]


@pytest.mark.parametrize("mutation", ["deletion", "target", "locator"])
def test_complete_audit_requires_exact_include_inventory_row(monkeypatch, mutation):
    read_text = Path.read_text

    def changed_text(path, *, encoding="utf-8", **kwargs):
        text = read_text(path, encoding=encoding, **kwargs)
        if path == ROOT / subject.DOCUMENTS / "external-files-inventory.md":
            row = next(
                row
                for row in text.splitlines(keepends=True)
                if row.startswith("| L02 modular |")
                and "includes/01_general.yml" in row
            )
            replacement = {
                "deletion": "",
                "target": row.replace(
                    "modular/includes/01_general.yml)", "modular/stale.yml)"
                ),
                "locator": row.replace("master L6", "master L9").replace(
                    "#L6)", "#L9)"
                ),
            }[mutation]
            text = text.replace(row, replacement)
        return text

    monkeypatch.setattr(Path, "read_text", changed_text)
    assert subject.audit_phase_a(ROOT)["errors"]


def test_audit_explicitly_reads_utf8(monkeypatch):
    read_text = Path.read_text

    def checked_text(path, *, encoding=None, **kwargs):
        assert encoding == "utf-8"
        return read_text(path, encoding=encoding, **kwargs)

    monkeypatch.setattr(Path, "read_text", checked_text)
    assert subject.audit_phase_a(ROOT)["errors"] == []


def test_reference_paths_are_portable(tmp_path):
    model = tmp_path / "master.yml"
    model.write_text(
        "includefile: includes\\data.yml\n", encoding="utf-8", newline="\n"
    )
    target = tmp_path / "includes" / "data.yml"
    target.parent.mkdir()
    target.write_text("{}", encoding="utf-8", newline="\n")
    row = subject.external_references(tmp_path, model)[0]
    assert row["file"] == "includes/data.yml"
    assert row["held_paths"] == ["includes/data.yml"]


def test_holdings_do_not_walk_directory_junctions(tmp_path, monkeypatch):
    linked = tmp_path / "linked"
    linked.mkdir()
    (linked / "control.py").write_text("# synthetic", encoding="utf-8", newline="\n")
    monkeypatch.setattr(Path, "is_junction", lambda path: path == linked, raising=False)
    assert subject._holdings(tmp_path) == {}


def test_holdings_do_not_walk_reparse_directories(tmp_path, monkeypatch):
    from types import SimpleNamespace

    linked = tmp_path / "linked"
    linked.mkdir()
    (linked / "control.py").write_text("# synthetic", encoding="utf-8", newline="\n")
    lstat = Path.lstat

    def reparse_stat(path):
        info = lstat(path)
        if path == linked:
            return SimpleNamespace(st_mode=info.st_mode, st_file_attributes=0x400)
        return info

    monkeypatch.setattr(Path, "is_junction", lambda path: False, raising=False)
    monkeypatch.setattr(Path, "lstat", reparse_stat)
    assert subject._holdings(tmp_path) == {}
