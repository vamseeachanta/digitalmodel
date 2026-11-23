"""The campaign verifier must not report a rebuild it did not observe.

Review found that a base probe which raised was recorded as the string
``<unreadable: ...>``; when the same probe failed on both sides the two strings
compared equal and the case counted as rebuilt -- ten unreadable probes, exit 0.
An unread property is absence of evidence, never agreement.

The verdict logic is tested without OrcaFlex: the module must import without
OrcFxAPI, so this runs wherever the tests run.
"""

from __future__ import annotations

import importlib.util
import subprocess
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts" / "verification" / "verify_orcaflex_campaign.py"


@pytest.fixture(scope="module")
def vc():
    spec = importlib.util.spec_from_file_location("verify_campaign", SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


DELTA = ["General:", "  StageDuration: [10, 100]", "Environment:", "  Hs: 2.5"]
BASE = {"#object_count": 7, "pipeline.Length": (100.0, 200.0)}


def test_matching_case_rebuilds(vc):
    v = vc.case_verdict(DELTA, DELTA, DELTA, dict(BASE), dict(BASE))
    assert v["rebuilds"] is True
    assert v["unreadable_probes"] == []


def test_a_probe_unreadable_on_both_sides_is_not_agreement(vc):
    bad = vc.Unreadable("KeyError")
    a = dict(BASE, **{"vessel_winch.StageValue": bad})
    b = dict(BASE, **{"vessel_winch.StageValue": vc.Unreadable("KeyError")})
    v = vc.case_verdict(DELTA, DELTA, DELTA, a, b)
    assert v["rebuilds"] is False
    assert v["unreadable_probes"] == ["vessel_winch.StageValue"]


def test_a_probe_unreadable_on_one_side_is_not_agreement(vc):
    b = dict(BASE, **{"pipeline.Length": vc.Unreadable("OrcaFlexError")})
    v = vc.case_verdict(DELTA, DELTA, DELTA, dict(BASE), b)
    assert v["rebuilds"] is False
    assert "pipeline.Length" in v["unreadable_probes"]


def test_a_probe_missing_from_one_side_is_not_agreement(vc):
    b = {k: v for k, v in BASE.items() if k != "#object_count"}
    v = vc.case_verdict(DELTA, DELTA, DELTA, dict(BASE), b)
    assert v["rebuilds"] is False


def test_a_changed_delta_is_detected(vc):
    other = DELTA[:-1] + ["  Hs: 3.5"]
    v = vc.case_verdict(DELTA, other, other, dict(BASE), dict(BASE))
    assert v["rebuilds"] is False
    assert v["delta_differences"] == [
        {"line": 3, "rebuild": "  Hs: 2.5", "sim": "  Hs: 3.5"}]


def test_a_non_deterministic_save_is_not_a_rebuild(vc):
    other = DELTA[:-1] + ["  Hs: 2.6"]
    v = vc.case_verdict(DELTA, DELTA, other, dict(BASE), dict(BASE))
    assert v["rebuilds"] is False
    assert v["save_is_deterministic"] is False


def test_unreadable_values_never_compare_equal(vc):
    assert vc.Unreadable("X") != vc.Unreadable("X")


def test_the_module_imports_without_orcaflex():
    """The verdict logic has to be testable where OrcaFlex is not installed."""
    code = (
        "import sys, importlib.util\n"
        "sys.modules['OrcFxAPI'] = None\n"  # makes `import OrcFxAPI` fail
        f"spec = importlib.util.spec_from_file_location('v', r'{SCRIPT}')\n"
        "m = importlib.util.module_from_spec(spec); spec.loader.exec_module(m)\n"
        "print('ok')\n"
    )
    out = subprocess.run([__import__("sys").executable, "-c", code],
                         capture_output=True, text=True)
    assert out.returncode == 0, out.stderr
    assert out.stdout.strip() == "ok"


def test_the_report_states_the_shared_base_check_is_partial(vc):
    """Review: the base probes omit mass, stiffness and hydrodynamics."""
    text = " ".join(vc.CAVEATS).lower()
    assert "partial" in text
    for missing in ("mass", "stiffness", "hydrodynamic"):
        assert missing in text
