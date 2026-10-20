"""The offline worked example must give the independently specified failure result."""

from pathlib import Path
import runpy


def test_timeout_example(tmp_path):
    """A fatal log and timeout imply failed status, with both evidence checks true."""
    path = Path(__file__).resolve().parents[2] / "examples/ansys/runner_timeout.py"
    namespace = runpy.run_path(str(path))
    assert namespace["demonstrate_timeout"](tmp_path) == {
        "status": "failed",
        "timeout_observed": True,
        "fatal_log_retained": True,
        "partial_output_retained": True,
    }
