# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Read back author-selected illustrative assumptions for issue #3340."""

from fractions import Fraction
import os
from pathlib import Path
import re
import subprocess
import sys
import xml.etree.ElementTree as ET


ROOT = Path(__file__).resolve().parents[3]
BUILDER = ROOT / "docs/reports/modules/hipps/build_worked_example.py"
REPORT = "closure_kinematics.html"


def run_builder(output_dir, cwd):
    """Run the documented CLI away from the source directory."""
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"))
    subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(output_dir)],
        cwd=cwd,
        env=env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=True,
    )
    return output_dir / REPORT


def table_rows(document, table_id):
    """Read displayed table cells rather than hidden metadata."""
    table = document.find(f".//table[@id='{table_id}']")
    assert table is not None
    return [[cell.text for cell in row.findall("td")]
            for row in table.findall("./tbody/tr")]


def test_closure_example_states_input_provenance(tmp_path):
    """Require author-selected illustrative assumptions on every basis surface."""
    report = run_builder(tmp_path / "selected", tmp_path)
    document = ET.fromstring(report.read_text(encoding="utf-8"))
    basis = " ".join(document.find(".//section[@id='basis']").itertext())
    basis = " ".join(basis.split())
    assert "author-selected illustrative assumptions" in basis
    assert "not supplied by issue #3340" in basis
    assert "Maintainer acceptance of this basis is required before merge" in basis
    assert "Draft for maintainer review" in " ".join(document.itertext())
    method = " ".join(document.find(".//section[@id='method']").itertext())
    assert "2016 edition" in method and "section 3.4" in method
    assert "Equation 3.10" in method
    assert "not a source of the selected numeric inputs" in method

    # Scan every line, including adjacent comments and test documentation.
    unsupported = re.compile(
        r"(?:source:\s+|specified\s+in\s+|the\s+)"
        r"(?:issue\s+#3340|issue\s+specifies)", re.IGNORECASE,
    )
    for path in (BUILDER, BUILDER.with_name("README.md"), Path(__file__)):
        source = " ".join(path.read_text(encoding="utf-8").split())
        assert "author-selected illustrative assumptions" in source
        assert not unsupported.search(source), path.relative_to(ROOT).as_posix()


def test_closure_example_matches_hand_kinematics(tmp_path):
    """Independent hand calculation for author-selected illustrative assumptions.

    Method: OpenStax, University Physics Volume 1, 2016 edition,
    section 3.4, Equation 3.10, for constant velocity.
    Travel L=0.6 m=3/5 m and loaded speed v=0.2 m/s=1/5 m/s
    give T=L/v=3 s. With elapsed time measured from motion start,
    completed travel fraction is clip(t/T, 0, 1). The assumed linear
    remaining conductance is 1-clip(t/T, 0, 1), dimensionless.
    At t=(0, 3/2, 3, 4) s this gives (1, 1/2, 0, 0).
    The endpoints 0 and 1 are normalization bounds, not equipment data.
    """
    report = run_builder(tmp_path / "selected", tmp_path)
    document = ET.fromstring(report.read_text(encoding="utf-8"))
    inputs = table_rows(document, "inputs")
    assert inputs == [
        ["Full actuator travel", "0.6", "m"],
        ["Constant loaded actuator speed", "0.2", "m/s"],
    ]
    closure_time = Fraction(3, 5) / Fraction(1, 5)
    assert table_rows(document, "closure-time") == [
        ["Closure time", f"{float(closure_time):.1f}", "s"],
    ]
    rows = table_rows(document, "conductance")
    elapsed = [Fraction(0), Fraction(3, 2), Fraction(3), Fraction(4)]
    expected = [Fraction(1), Fraction(1, 2), Fraction(0), Fraction(0)]
    assert len(rows) == len(elapsed)
    for row, time, remaining in zip(rows, elapsed, expected):
        assert Fraction(row[0]) == time
        assert row[1] == "s"
        assert Fraction(row[2]) == remaining
        assert row[3] == "dimensionless"

    text = " ".join(document.itertext())
    assert "Synthetic" in text
    assert "actual flow" in text
    assert "actuator travel" in text
    assert "idealized conductance" in text
    assert "T = L / v" in text
    assert "g(t) = 1 - clip(t / T, 0, 1)" in text
    assert table_rows(document, "not-evaluated") == [
        [name, "NOT_EVALUATED"] for name in (
            "Detection delay", "SIL", "Pressure transients", "Capacity",
            "Equipment suitability", "Standards compliance", "Actual flow",
        )
    ]
    sections = [section.attrib["id"]
                for section in document.findall("./body/main/section")]
    assert sections[:2] == ["summary", "revision-history"]
    assert table_rows(document, "revisions")


def test_closure_example_rebuild_is_deterministic(tmp_path):
    """Identical synthetic inputs yield identical bytes at every destination.

    The author-selected hand calculation T=(3/5)/(1/5)=3 s and the clipped
    linear time fraction have no dependence on wall-clock time, directory
    names or external data. Importing the builder must not execute it.
    """
    caller = tmp_path / "caller"
    caller.mkdir()
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"))
    subprocess.run(
        [sys.executable, "-c",
         "import runpy, sys; runpy.run_path(sys.argv[1], run_name='import_check')",
         str(BUILDER)],
        cwd=caller, env=env, capture_output=True, text=True,
        encoding="utf-8", check=True,
    )
    assert list(caller.iterdir()) == []
    first = run_builder(tmp_path / "first", caller)
    original = first.read_bytes()
    assert run_builder(first.parent, caller).read_bytes() == original
    second = run_builder(tmp_path / "second", caller)
    assert second.read_bytes() == original
    assert sorted(path.relative_to(tmp_path).as_posix()
                  for path in tmp_path.rglob("*") if path.is_file()) == [
        f"first/{REPORT}", f"second/{REPORT}",
    ]

    # A destination symlink must not redirect a write out of the selection.
    sentinel = tmp_path / "preserved.html"
    sentinel.write_bytes(original)
    linked_output = tmp_path / "linked"
    linked_output.mkdir()
    (linked_output / REPORT).symlink_to(sentinel)
    result = subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(linked_output)],
        cwd=caller, env=env, capture_output=True, text=True, encoding="utf-8",
    )
    assert result.returncode != 0
    assert "must not be a symbolic link" in result.stderr
    assert sentinel.read_bytes() == original
    assert list(caller.iterdir()) == []
