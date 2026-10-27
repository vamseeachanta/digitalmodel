# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise the reverse-extraction example with external input and output files."""

import json
import os
import runpy
import subprocess
import sys
from pathlib import Path, PureWindowsPath

import pytest
import yaml

from digitalmodel.solvers.orcaflex.format_converter.protocols import ConversionReport


ROOT = Path(__file__).resolve().parents[4]
EXAMPLE = ROOT / "examples" / "orcaflex" / "reverse_extraction"

# Fail the example if it attempts network access, a licensed binding import or
# another process. These are runtime guards, rather than source-text assertions.
OFFLINE_RUNNER = """
import runpy
import sys


def guard(event, args):
    if event in {"socket.connect", "socket.getaddrinfo", "subprocess.Popen"}:
        raise RuntimeError("Example must run offline without other processes")
    if event == "import" and args[0].split(".")[0] == "OrcFxAPI":
        raise RuntimeError("Example must not import the licensed interface")


sys.addaudithook(guard)
sys.argv = sys.argv[1:]
runpy.run_path(sys.argv[0], run_name="__main__")
"""


def run_example(tmp_path: Path, *args: str) -> subprocess.CompletedProcess:
    """Run from an external working directory with external temporary storage."""
    scratch = tmp_path / "scratch"
    scratch.mkdir(exist_ok=True)
    env = dict(os.environ, TMPDIR=str(scratch), PYTHONDONTWRITEBYTECODE="1")
    env["PYTHONPATH"] = str(ROOT / "src")
    return subprocess.run(
        [sys.executable, "-c", OFFLINE_RUNNER, str(EXAMPLE / "convert.py"), *args],
        cwd=tmp_path,
        env=env,
        text=True,
        encoding="utf-8",
        capture_output=True,
        check=False,
    )


@pytest.mark.parametrize("depth", [42.0, 84.0])
@pytest.mark.parametrize("existing_outputs", [False, True])
def test_reverse_example_reports_unmapped_content(
    tmp_path: Path, depth: float, existing_outputs: bool,
):
    """Authored inputs survive while harmless grouping metadata is disclosed."""
    # Numeric basis: reverse_extraction/README.md, "Synthetic input basis".
    # The second depth is twice the authored depth, an independent input change.
    source = tmp_path / "strict.yml"
    data = yaml.safe_load((EXAMPLE / "strict.yml").read_text(encoding="utf-8"))
    data["Environment"]["WaterDepth"] = depth
    source.write_text(yaml.safe_dump(data), encoding="utf-8", newline="\n")
    original = source.read_bytes()
    spec_path = tmp_path / "output" / "spec.yml"
    report_path = tmp_path / "output" / "report.json"
    if existing_outputs:
        spec_path.parent.mkdir()
        spec_path.write_text("previous spec\n", encoding="utf-8", newline="\n")
        report_path.write_text("previous report\n", encoding="utf-8", newline="\n")

    result = run_example(
        tmp_path,
        "--input", str(source),
        "--spec-output", str(spec_path),
        "--report-output", str(report_path),
    )

    assert result.returncode == 0, result.stderr
    spec = yaml.safe_load(spec_path.read_text(encoding="utf-8"))
    report = json.loads(report_path.read_text(encoding="utf-8"))
    assert report["source_path"] == source.as_posix()
    assert report["target_path"] == spec_path.as_posix()
    assert b"\r\n" not in report_path.read_bytes()
    assert spec["environment"]["water"]["depth"] == depth
    assert spec["environment"]["wind"]["speed"] == 0.0
    assert "Groups" not in spec
    assert report["success"] is True
    assert report["source_format"] == "single"
    assert report["target_format"] == "spec"
    assert report["is_best_effort"] is True
    # ConversionReport defines confidence on the unit interval. Sparse input
    # must have partial coverage; this is not a physical-validation probability.
    assert 0.0 < report["confidence"] < 1.0
    assert report["unmapped_sections"] == ["Groups"]
    assert report["expected_gaps"] == ["Groups"]
    assert report["actionable_gaps"] == []
    assert "Conversion report" in result.stdout
    assert "Produced spec" in result.stdout
    assert '"Groups"' in result.stdout
    assert '"confidence"' in result.stdout
    assert spec_path.read_text(encoding="utf-8") in result.stdout
    assert "not a lossless" in result.stdout.lower()
    assert "Review before use" in result.stdout
    assert source.read_bytes() == original
    files = sorted(
        p.relative_to(tmp_path).as_posix()
        for p in tmp_path.rglob("*") if p.is_file()
    )
    assert files == ["output/report.json", "output/spec.yml", "strict.yml"]


def test_reverse_example_requires_explicit_outputs(tmp_path: Path):
    """Missing output arguments cause usage failure without creating files."""
    result = run_example(tmp_path, "--input", str(tmp_path / "strict.yml"))
    assert result.returncode != 0
    assert "--spec-output" in result.stderr
    assert "--report-output" in result.stderr
    assert not any(p.is_file() for p in tmp_path.rglob("*"))


@pytest.mark.parametrize("collision", ["input-spec", "input-report", "spec-report"])
def test_reverse_example_rejects_overlapping_paths(tmp_path: Path, collision: str):
    """Explicit output paths must not overwrite the source or each other."""
    source = tmp_path / "strict.yml"
    original = (EXAMPLE / "strict.yml").read_bytes()
    source.write_bytes(original)
    spec = source if collision == "input-spec" else tmp_path / "spec.yml"
    report = source if collision == "input-report" else tmp_path / "report.json"
    if collision == "spec-report":
        report = spec
    result = run_example(
        tmp_path,
        "--input", str(source),
        "--spec-output", str(spec),
        "--report-output", str(report),
    )
    assert result.returncode != 0
    assert "distinct" in result.stderr
    assert source.read_bytes() == original
    assert sorted(p.name for p in tmp_path.rglob("*") if p.is_file()) == ["strict.yml"]


@pytest.mark.parametrize("pair", [(0, 1), (0, 2), (1, 2)], ids=[
    "input-spec", "input-report", "spec-report",
])
def test_reverse_example_rejects_hard_link_aliases(tmp_path: Path, pair: tuple):
    """Shared file identities are rejected before any existing file is changed."""
    paths = [tmp_path / name for name in ("strict.yml", "spec.yml", "report.json")]
    contents = [
        (EXAMPLE / "strict.yml").read_bytes(),
        b"previous spec\n",
        b"previous report\n",
    ]
    first, second = pair
    for index, path in enumerate(paths):
        if index != second:
            path.write_bytes(contents[index])
    os.link(paths[first], paths[second])
    before = {path.name: path.read_bytes() for path in paths}

    result = run_example(
        tmp_path,
        "--input", str(paths[0]),
        "--spec-output", str(paths[1]),
        "--report-output", str(paths[2]),
    )

    assert result.returncode != 0
    assert "distinct" in result.stderr
    assert {p.name: p.read_bytes() for p in tmp_path.rglob("*") if p.is_file()} == before


def test_reverse_example_formats_windows_report_paths():
    """Report metadata uses forward slashes on every supported platform."""
    report = ConversionReport(
        success=True, source_format="single", target_format="spec",
        source_path=PureWindowsPath("input/strict.yml"),
        target_path=PureWindowsPath("output/spec.yml"),
    )
    format_report = runpy.run_path(str(EXAMPLE / "convert.py"))["format_report"]

    data = json.loads(format_report(report))

    assert data["source_path"] == "input/strict.yml"
    assert data["target_path"] == "output/spec.yml"
    assert data["success"] is True


def test_reverse_example_displays_failure_report(tmp_path: Path):
    """An empty source returns failure, writes its report and produces no spec."""
    source = tmp_path / "empty.yml"
    source.write_text("# No model data\n", encoding="utf-8", newline="\n")
    spec = tmp_path / "spec.yml"
    report = tmp_path / "report.json"
    result = run_example(
        tmp_path,
        "--input", str(source),
        "--spec-output", str(spec),
        "--report-output", str(report),
    )
    assert result.returncode != 0
    assert json.loads(report.read_text(encoding="utf-8"))["success"] is False
    assert "Conversion report" in result.stdout
    assert not spec.exists()
