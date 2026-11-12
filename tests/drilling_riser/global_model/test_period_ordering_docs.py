"""Executable documentation for the natural-period return order."""

from __future__ import annotations

import doctest
from pathlib import Path


def test_natural_period_ordering_worked_example():
    """Check the guide against an independent uniform-string hand calculation.

    Source: OpenStax, University Physics Volume 1 (2016), section 16.6,
    equation 16.16: f_n = n*v/(2*L), with v = sqrt(Te/m).
    The synthetic fixture uses L = 300 m, Te = 2,000,000 N and m = 800 kg/m.
    Therefore v = 50 m/s and T_n = 12/n s, giving 12, 6, 4, 3 and 2.4 s.
    These expected values are derived without the finite-element implementation.
    """
    guide = (
        Path(__file__).resolve().parents[3]
        / "src/digitalmodel/drilling_riser/global_model/docs/agent-pack.md"
    )
    text = " ".join(guide.read_text(encoding="utf-8").split())
    assert (
        "increasing natural frequency (decreasing periods, fundamental mode first)"
        in text
    )
    result = doctest.testfile(str(guide), module_relative=False, encoding="utf-8")
    assert result.attempted > 0, "The guide must contain a worked example."
    assert result.failed == 0
