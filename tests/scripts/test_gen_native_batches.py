# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Caller-configured CAD batches, using synthetic paths and no CAD applications.

Expected counts are independent row enumerations, not calculations by the
producer. Format groups and queue ordering come from the existing CAD adapter
and its README; they are workflow categories, not engineering coefficients.
"""

import ast
import csv
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest

SCRIPT = (
    Path(__file__).resolve().parents[2]
    / "docs/cad-inventory/adapters/seat-export/gen_native_batches.py"
)


def test_locations_are_caller_configured():
    """Required CLI flags replace the original fixed deployment locations."""
    tree = ast.parse(SCRIPT.read_text(encoding="utf-8"))
    literals = {
        n.value
        for n in ast.walk(tree)
        if isinstance(n, ast.Constant) and isinstance(n.value, str)
    }
    assert {"--index", "--output-dir", "--source-root", "--windows-root"} <= literals


@pytest.fixture
def adapter():
    spec = importlib.util.spec_from_file_location("native_batches", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write_index(path, rows):
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(
            stream,
            fieldnames=["path", "format", "readability", "project"],
            delimiter="\t",
        )
        writer.writeheader()
        writer.writerows(rows)


def row(path, extension="sldprt", project="docs/library", readability="seat-only"):
    return {
        "path": str(path),
        "format": extension,
        "readability": readability,
        "project": project,
    }


def clean_env():
    return {
        k: v
        for k, v in os.environ.items()
        if k not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}
    }


def test_import_is_read_only(tmp_path):
    probe = (
        "import importlib.util; "
        f"s = importlib.util.spec_from_file_location('probe', {str(SCRIPT)!r}); "
        "m = importlib.util.module_from_spec(s); "
        "s.loader.exec_module(m)"
    )
    result = subprocess.run(
        [sys.executable, "-c", probe],
        cwd=tmp_path,
        env=clean_env(),
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == ""
    assert list(tmp_path.iterdir()) == []


def test_cli_requires_locations_without_writing(tmp_path):
    result = subprocess.run(
        [sys.executable, str(SCRIPT)],
        cwd=tmp_path,
        env=clean_env(),
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    for flag in ("--index", "--output-dir", "--source-root", "--windows-root"):
        assert flag in result.stderr
    assert list(tmp_path.iterdir()) == []


def run_configured_cli(tmp_path, index, output, source):
    return subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--index",
            str(index),
            "--output-dir",
            str(output),
            "--source-root",
            str(source),
            "--windows-root",
            "Q:\\",
        ],
        cwd=tmp_path,
        env=clean_env(),
        capture_output=True,
        text=True,
    )


def test_cli_generates_exact_batches(tmp_path):
    """Hand enumeration: two SW + one INV + one other = four rows in three groups.

    The unsupported extension and the open-reader row contribute zero. The
    original adapter ranks docs before ordinary groups, then sorts group names.
    """
    source = tmp_path / "source"
    index = tmp_path / "index.tsv"
    output = tmp_path / "batches"
    write_index(
        index,
        [
            row(source / "parts/one.sldprt"),
            row(source / "parts/two.sldasm", "sldasm"),
            row(source / "assemblies/three.iam", "iam", "generic/assembly"),
            row(source / "parts/four.prt", "prt", "generic/other"),
            row(source / "ignored/file.stl", "stl"),
            row(source / "ignored/file.ipt", "ipt", readability="oss-3d"),
        ],
    )
    result = run_configured_cli(tmp_path, index, output, source)
    assert result.returncode == 0, result.stderr
    manifest = json.loads((output / "_manifest.json").read_text())
    assert manifest["totals"] == {"sw": 2, "inv": 1, "other": 1, "projects": 3}
    assert [batch["project"] for batch in manifest["batches"]] == [
        "docs/library",
        "generic/assembly",
        "generic/other",
    ]
    sw = manifest["batches"][0]
    assert sw["priority"] == 2  # Existing generator: docs groups have queue rank 2.
    assert (output / sw["sw_file"]).read_bytes() == (
        b"Q:\\parts\\one.sldprt\r\nQ:\\parts\\two.sldasm\r\n"
    )
    inv = manifest["batches"][1]
    other = manifest["batches"][2]
    assert (output / inv["inv_file"]).read_bytes() == b"Q:\\assemblies\\three.iam\r\n"
    assert (output / other["other_file"]).read_bytes() == b"Q:\\parts\\four.prt\r\n"
    assert str(source) not in result.stdout
    assert str(output) not in result.stdout


def test_unc_mapping(adapter, tmp_path):
    source = tmp_path / "source"
    assert adapter.winpath(
        str(source / "parts/item.ipt"), source, r"\\example.invalid\archive"
    ) == (r"\\example.invalid\archive\parts\item.ipt")


@pytest.mark.parametrize(
    "relative",
    [
        "../outside.ipt",
        "part/../../outside.ipt",
        "part\\outside.ipt",
        "part\nextra.ipt",
        "part\rextra.ipt",
        "part:stream.ipt",
        "part*.ipt",
        "part?.ipt",
        "part|name.ipt",
        "part<name>.ipt",
        'part"name.ipt',
    ],
)
def test_invalid_source_paths_leave_no_output(adapter, tmp_path, relative):
    source = tmp_path / "source"
    index = tmp_path / "index.tsv"
    output = tmp_path / "batches"
    write_index(index, [row(str(source) + "/" + relative)])
    with pytest.raises(ValueError):
        adapter.generate_batches(index, output, source, "Q:\\")
    assert not output.exists()


@pytest.mark.parametrize("kind", ["sibling", "relative", "root"])
def test_containment_is_by_components(adapter, tmp_path, kind):
    source = tmp_path / "source"
    candidate = {
        "sibling": str(tmp_path / "source-copy/file.ipt"),
        "relative": "file.ipt",
        "root": str(source),
    }[kind]
    with pytest.raises(ValueError):
        adapter.winpath(candidate, source, "Q:\\")


@pytest.mark.parametrize("root", ["Q:", "relative", "Q:\\folder\nextra", "/archive"])
def test_invalid_windows_root_leaves_no_output(adapter, tmp_path, root):
    index = tmp_path / "index.tsv"
    output = tmp_path / "batches"
    write_index(index, [row(tmp_path / "source/part.ipt", "ipt")])
    with pytest.raises(ValueError):
        adapter.generate_batches(index, output, tmp_path / "source", root)
    assert not output.exists()


def test_filename_collision_leaves_no_output(adapter, tmp_path):
    index = tmp_path / "index.tsv"
    output = tmp_path / "batches"
    write_index(
        index,
        [
            row(tmp_path / "source/a.ipt", "ipt", "generic/a"),
            row(tmp_path / "source/b.ipt", "ipt", "generic_a"),
        ],
    )
    with pytest.raises(ValueError, match="collision"):
        adapter.generate_batches(index, output, tmp_path / "source", "Q:\\")
    assert not output.exists()


def test_existing_output_is_preserved(adapter, tmp_path):
    index = tmp_path / "index.tsv"
    output = tmp_path / "batches"
    output.mkdir()
    existing = output / "_manifest.json"
    existing.write_bytes(b"retained output\n")
    write_index(index, [row(tmp_path / "source/a.ipt", "ipt")])
    with pytest.raises(FileExistsError):
        adapter.generate_batches(index, output, tmp_path / "source", "Q:\\")
    assert existing.read_bytes() == b"retained output\n"
    assert list(output.iterdir()) == [existing]


def test_missing_columns_leave_no_output(adapter, tmp_path):
    index = tmp_path / "index.tsv"
    output = tmp_path / "batches"
    index.write_text("path\tformat\nfile.ipt\tipt\n", encoding="utf-8")
    with pytest.raises(ValueError, match="columns"):
        adapter.generate_batches(index, output, tmp_path / "source", "Q:\\")
    assert not output.exists()


def test_relative_source_root_is_rejected(adapter, tmp_path):
    with pytest.raises(ValueError):
        adapter.winpath("source/file.ipt", Path("source"), "Q:\\")


@pytest.mark.parametrize(
    "projects", [("generic/a", "generic/A"), ("g" * 80 + "a", "g" * 80 + "b")]
)
def test_casefold_and_truncation_collisions(adapter, tmp_path, projects):
    """The original adapter truncates stems to 80 characters; both collide."""
    index = tmp_path / "index.tsv"
    output = tmp_path / "batches"
    write_index(
        index,
        [
            row(tmp_path / "source/a.ipt", "ipt", projects[0]),
            row(tmp_path / "source/b.ipt", "ipt", projects[1]),
        ],
    )
    with pytest.raises(ValueError, match="collision"):
        adapter.generate_batches(index, output, tmp_path / "source", "Q:\\")
    assert not output.exists()


@pytest.mark.parametrize("project", ["", "group\nextra", "group\rextra"])
def test_invalid_group_leaves_no_output(adapter, tmp_path, project):
    index = tmp_path / "index.tsv"
    output = tmp_path / "batches"
    write_index(index, [row(tmp_path / "source/a.ipt", "ipt", project)])
    with pytest.raises(ValueError):
        adapter.generate_batches(index, output, tmp_path / "source", "Q:\\")
    assert not output.exists()


def test_invalid_later_row_does_not_publish_valid_earlier_row(adapter, tmp_path):
    index = tmp_path / "index.tsv"
    output = tmp_path / "batches"
    write_index(
        index,
        [
            row(tmp_path / "source/a.ipt", "ipt"),
            row(tmp_path / "elsewhere/b.ipt", "ipt"),
        ],
    )
    with pytest.raises(ValueError):
        adapter.generate_batches(index, output, tmp_path / "source", "Q:\\")
    assert not output.exists()


def test_cli_failure_does_not_print_private_locations(tmp_path):
    missing = tmp_path / "absent.tsv"
    result = subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--index",
            str(missing),
            "--output-dir",
            str(tmp_path / "output"),
            "--source-root",
            str(tmp_path / "source"),
            "--windows-root",
            "Q:\\",
        ],
        cwd=tmp_path,
        env=clean_env(),
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    assert "FileNotFoundError" in result.stderr
    assert str(tmp_path) not in result.stdout + result.stderr
    assert not (tmp_path / "output").exists()


@pytest.mark.parametrize(
    "group, expected",
    [
        ("digitalmodel/subsea-risers/library", 1),
        ("docs/library", 2),
        ("generic", 3),
        ("generic.preexisting", 7),
        ("ri-hoard", 9),
    ],
)
def test_legacy_priority_order(adapter, group, expected):
    """Queue ranks are copied from the original adapter, not engineering limits."""
    assert adapter.prio(group) == expected


def test_empty_index_has_zero_counts(adapter, tmp_path):
    """An empty input has no rows: each group sum and the group count is zero."""
    index = tmp_path / "index.tsv"
    output = tmp_path / "batches"
    write_index(index, [])
    result = adapter.generate_batches(index, output, tmp_path / "source", "Q:\\")
    assert result == {
        "totals": {"sw": 0, "inv": 0, "other": 0, "projects": 0},
        "batches": [],
    }
    assert sorted(p.name for p in output.iterdir()) == ["_manifest.json"]


def test_duplicate_columns_are_rejected(adapter, tmp_path):
    index = tmp_path / "index.tsv"
    output = tmp_path / "batches"
    index.write_text("path\tformat\treadability\tproject\tpath\n", encoding="utf-8")
    with pytest.raises(ValueError, match="columns"):
        adapter.generate_batches(index, output, tmp_path / "source", "Q:\\")
    assert not output.exists()


def test_cli_handles_csv_failure_without_traceback(
    adapter, monkeypatch, capsys, tmp_path
):
    def reject(*args):
        raise csv.Error(str(tmp_path / "private-index.tsv"))

    monkeypatch.setattr(adapter, "generate_batches", reject)
    with pytest.raises(SystemExit) as caught:
        adapter.main(
            [
                "--index",
                "index.tsv",
                "--output-dir",
                "batches",
                "--source-root",
                str(tmp_path),
                "--windows-root",
                "Q:\\",
            ]
        )
    assert caught.value.code != 0
    message = capsys.readouterr().err
    assert "batch generation failed" in message
    assert str(tmp_path) not in message


def test_posix_root_is_not_part_of_windows_filename(adapter, tmp_path):
    """The stripped POSIX prefix may contain a colon; the mapped suffix may not."""
    source = tmp_path / "archive:release"
    assert (
        adapter.winpath(str(source / "parts/item.ipt"), source, "Q:\\")
        == r"Q:\parts\item.ipt"
    )
