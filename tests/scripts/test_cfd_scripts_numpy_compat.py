"""Standalone CFD integration compatibility and hand-value regression pins."""

import re
from pathlib import Path

import numpy as np
import pytest

ROOT = Path(__file__).resolve().parents[2]
FOLDERS = (ROOT / "docs/api/cfd", ROOT / "scripts/cfd")
FALLBACK_BINDING = '_trapezoid = getattr(np, "trapezoid", None) or np.trapz'
_trapezoid = getattr(np, "trapezoid", None) or np.trapz


def test_cfd_sources_use_only_single_trapz_fallback():
    """Direct legacy attribute access is restricted to one binding per file."""
    offenders = []
    for folder in FOLDERS:
        assert folder.is_dir(), folder.relative_to(ROOT)
        for path in sorted(folder.rglob("*")):
            if not path.is_file():
                continue
            lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
            assert lines.count(FALLBACK_BINDING) <= 1, path.relative_to(ROOT)
            for number, line in enumerate(lines, 1):
                if re.search(r"\b(?:np|numpy)\.trapz\b", line):
                    if line != FALLBACK_BINDING:
                        offenders.append(f"{path.relative_to(ROOT)}:{number}: {line.strip()}")
    assert not offenders, "\n".join(offenders)


@pytest.mark.parametrize(
    "start, stop, slope, intercept, expected",
    [(0.0, 1.0, 1.0, 0.0, 0.5), (0.0, 3.0, 0.0, 2.0, 6.0)],
    ids=["linear-half", "constant-six"],
)
def test_trapezoid_hand_values_regression_pin(start, stop, slope, intercept, expected):
    """Pins: integral of x on [0, 1] is 0.5; integral of 2 on [0, 3] is 6.0."""
    x = np.linspace(start, stop, 11)
    y = slope * x + intercept
    assert _trapezoid(y, x) == pytest.approx(expected)
