"""Fail-closed warm starts for OpenFOAM calm-water resistance cases."""

from .cli import main
from .checks import CheckResult, evaluate_checkpoint
from .decision import Decision, decide

__all__ = ["CheckResult", "Decision", "decide", "evaluate_checkpoint", "main"]
