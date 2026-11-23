# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Example-input, hosted-domain and engine-dispatch regression contracts."""

import sys
from pathlib import Path

import pytest
import yaml

from digitalmodel import __main__ as cli
from digitalmodel.riser_configuration_screen.workflow import screen

ROOT = Path(__file__).resolve().parents[2]


def test_screen_tests_have_one_hosted_domain():
    """Keep the screening suite assigned to the subsea domain."""
    rows = [line for line in (ROOT / "tests/DOMAINS.md").read_text(encoding="utf-8").splitlines()
            if "`tests/riser_configuration_screen/`" in line]
    assert len(rows) == 1
    assert rows[0].split("|")[1].strip() == "subsea"


@pytest.mark.parametrize("text", [
    "value: [", "value: !custom-tag payload", b"basename: x\n# \xb0\n",
])
def test_unparseable_non_screen_input_reaches_engine(tmp_path, monkeypatch, text):
    """A failed screen pre-check leaves the existing engine path available."""
    source = tmp_path / "input.yml"
    if isinstance(text, bytes):
        source.write_bytes(text)
    else:
        source.write_text(text, encoding="utf-8")
    monkeypatch.setattr(sys, "argv", ["digitalmodel", str(source)])
    calls = []

    def engine():
        calls.append(tuple(sys.argv))
        return {}

    monkeypatch.setattr(cli, "engine", engine)
    assert cli.main() == 0
    assert calls == [("digitalmodel", str(source))]


def test_unreadable_input_reaches_engine(tmp_path, monkeypatch):
    """An open failure in the pre-check also preserves engine dispatch."""
    source = tmp_path / "input.yml"
    source.write_text("basename: other_workflow")
    monkeypatch.setattr(sys, "argv", ["digitalmodel", str(source)])
    calls = []

    def unreadable(*args, **kwargs):
        raise OSError("input cannot be opened")

    def engine():
        calls.append(tuple(sys.argv))
        return {}

    monkeypatch.setattr(Path, "open", unreadable)
    monkeypatch.setattr(cli, "engine", engine)
    assert cli.main() == 0
    assert calls == [("digitalmodel", str(source))]


@pytest.mark.parametrize("name, grid_size, feasible_count, selection", [
    ("scr", 7, 7, [(20.0,), (18.0,), (16.0,)]),
    ("slwr", 400, 226, [
        (14.0, 2.0, 0.1, 0.5), (14.0, 2.5, 0.1, 0.5),
        (14.0, 1.5, 0.1, 0.5), (14.0, 3.0, 0.1, 0.5),
        (14.0, 3.5, 0.1, 0.5), (12.0, 1.5, 0.1, 0.5),
        (12.0, 2.0, 0.1, 0.5), (12.0, 2.5, 0.1, 0.5),
        (12.0, 3.0, 0.1, 0.5), (12.0, 3.5, 0.1, 0.5),
    ]),
])
def test_example_inputs_reproduce_screen_selection(name, grid_size, feasible_count, selection):
    """Run each shipped example and pin its grid, feasibility and shortlist."""
    source = ROOT / "examples/workflows/riser-configuration-screen" / f"{name}.yml"
    settings = yaml.safe_load(source.read_text(encoding="utf-8"))["riser_configuration_screen"]
    result = screen(settings)
    assert result["grid_size"] == grid_size
    assert sum(row["feasible"] for row in result["ranked_candidates"]) == feasible_count
    axes = ("hangoff_angle_deg",) if name == "scr" else (
        "hangoff_angle_deg", "buoyancy_factor", "sag_fraction", "hog_fraction")
    expected = [dict(zip(axes, values)) for values in selection]
    assert result["solver_candidates"] == expected
