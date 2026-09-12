"""Golden-value assertions for the committed ANSYS example decks (#2094).

The drift guards elsewhere in this directory compare a committed ``.inp`` to the
generator that produced it. That proves the generator is deterministic. It proves
nothing about whether the deck is right, and for months it proved nothing while
one deck over-predicted peak stress by 3.8x and another had never solved at all.

These tests assert against a comparator the producing system did not make. They
are **always on**: they read committed files and need no licence, so they run in
CI and on any developer machine. A test that skips its way to green would
reproduce the condition this issue exists to remove.

The re-solve test at the bottom is the only licence-dependent one and carries the
``requires_mapdl`` marker.
"""

from __future__ import annotations

import importlib.util
import json
import math
import shutil
from pathlib import Path

import pytest

from tests.ansys.golden_acceptance import (
    load_golden, validate_acceptance, validate_pv_comparator, validate_equilibrium,
)

EXAMPLES = Path(__file__).resolve().parents[2] / "examples" / "ansys"

# Cases carrying a committed golden. A case is added here only once its golden
# exists; an absent golden is a hard failure below, never a skip.
GOLDEN_CASES = ["pressure-vessel", "mudmat"]


@pytest.mark.parametrize("tamper", ["deck", "provenance"])
def test_golden_rejects_unbound_deck(tmp_path, monkeypatch, tamper):
    original = EXAMPLES / "pressure-vessel"
    copied = tmp_path / "pressure-vessel"
    shutil.copytree(original / "golden", copied / "golden")
    shutil.copyfile(original / "pv.inp", copied / "pv.inp")
    if tamper == "deck":
        with (copied / "pv.inp").open("ab") as stream:
            stream.write(b"\n! changed input\n")
    else:
        path = copied / "golden" / "PROVENANCE.json"
        provenance = json.loads(path.read_text())
        provenance["input"]["sha256"] = "0" * 64
        path.write_text(json.dumps(provenance))
    monkeypatch.setitem(globals(), "EXAMPLES", tmp_path)
    with pytest.raises(AssertionError, match="deck SHA-256"):
        _golden("pressure-vessel")


def _golden(case: str) -> tuple[dict[str, float], dict]:
    return load_golden(EXAMPLES / case)


def _build_module(case: str):
    build = EXAMPLES / case / "build.py"
    spec = importlib.util.spec_from_file_location(f"{case}_build", build)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


# --- comparator: the value is checked against something the deck did not make --


def test_pressure_vessel_golden_matches_closed_form():
    """Lame thick-wall cylinder, computed here from the generator's own inputs.

    The comparator is derived rather than hard-coded, so changing the example's
    geometry changes the expected value instead of silently invalidating it.
    """
    digest, prov = _golden("pressure-vessel")
    module = _build_module("pressure-vessel")

    r_i = module.GEOM.inner_diameter_mm / 2.0
    r_o = r_i + module.GEOM.wall_thickness_mm
    p = module.CONDITIONS.design_pressure_mpa

    # Lame, open-ended: no end cap and a free top edge, so sigma_z is exactly 0.
    s_theta = p * (r_o**2 + r_i**2) / (r_o**2 - r_i**2)
    s_r = -p
    s_z = 0.0
    expected_vm = math.sqrt(
        0.5 * ((s_theta - s_r) ** 2 + (s_r - s_z) ** 2 + (s_z - s_theta) ** 2)
    )

    validate_pv_comparator(digest, prov, expected_vm, s_theta)


# --- internal consistency and recorded status -------------------------------


@pytest.mark.parametrize("case", GOLDEN_CASES)
def test_golden_unity_check_is_internally_consistent(case: str):
    """uc must equal peak/allowable. Catches a digest assembled incorrectly."""
    digest, _ = _golden(case)
    expected = digest["max_seqv_mpa"] / digest["allowable_mpa"]
    assert digest["uc"] == pytest.approx(expected, rel=1e-4)


@pytest.mark.parametrize("case", GOLDEN_CASES)
def test_golden_matches_recorded_status(case: str):
    """A silent move across the allowable fails.

    Without this, a regression that pushed the unity check from 0.98 to 1.4
    would still satisfy a test asserting only that some number was recorded.
    """
    digest, prov = _golden(case)
    validate_acceptance(digest, prov, _build_module(case))


@pytest.mark.parametrize("case", GOLDEN_CASES)
def test_golden_respects_linear_elastic_validity(case: str):
    """D2 requires sub-yield stress; provenance cannot waive that criterion."""
    digest, prov = _golden(case)
    validate_acceptance(digest, prov, _build_module(case))


# --- equilibrium: the check that would have caught the original defect -------


@pytest.mark.parametrize("case", GOLDEN_CASES)
def test_golden_reactions_balance(case: str):
    """Apply self-balanced or supported-load equilibrium as appropriate."""
    digest, prov = _golden(case)
    validate_equilibrium(case, digest, prov, _build_module(case))


@pytest.mark.parametrize("case", GOLDEN_CASES)
def test_golden_provenance_declares_a_comparator_class(case: str):
    """A golden without a declared comparator is a regression guard only.

    Recording the class is what stops an `archived-run` value being read later as
    evidence of correctness.
    """
    _, prov = _golden(case)
    klass = prov["comparator"]["class"]
    assert klass in {
        "closed-form",
        "measured",
        "cross-solver",
        "conservation",
        "archived-run",
    }, f"{case} declares an unknown comparator class {klass!r}"
    assert prov["input"]["sha256"], f"{case} golden records no input hash"
    assert prov["solver"]["argv"], f"{case} golden records no run configuration"


# --- licence-dependent ------------------------------------------------------


@pytest.mark.requires_mapdl
@pytest.mark.parametrize("case", GOLDEN_CASES)
def test_committed_golden_is_stable(case: str):
    """Re-solving reproduces the committed value.

    Skipped unless ANSYS_NATIVE_TESTS=1; callers may also deselect the marker.
    Tolerance is 1e-3
    relative and is valid **only** against the run configuration recorded in
    provenance: distributed and iterative solvers are not bit-reproducible
    across core counts, so a tighter gate would fail a correct result.
    """
    import os
    from tests.ansys.native_resolve_support import resolve_golden

    if os.environ.get("ANSYS_NATIVE_TESTS") != "1":
        pytest.skip("Native capture disabled; explicitly enable after seat preflight")
    if os.environ.get("PYTEST_XDIST_WORKER"):
        pytest.fail("Native captures must run serially, without xdist")
    executable = Path(os.environ["ANSYS_NATIVE_EXECUTABLE"])
    output_root = Path(os.environ["ANSYS_NATIVE_OUTPUT_ROOT"])
    assert executable.is_file(), "Explicit native executable does not exist"
    resolve_golden(EXAMPLES / case, output_root / case, executable)
