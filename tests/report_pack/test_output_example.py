"""Exercise the documented worked example with independent literal answers."""

from pathlib import Path
import runpy


def test_output_destination_example(tmp_path):
    example = Path(__file__).resolve().parents[2] / "examples/reporting/output_destinations.py"
    run = runpy.run_path(str(example))["run"]
    assert run(tmp_path / "example") == {
        "figure_link": "../inputs/figure.svg",
        "collision_rejected": True,
        "retained_bytes_unchanged": True,
    }
