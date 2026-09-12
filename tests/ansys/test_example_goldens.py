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

import hashlib
import importlib.util
import json
import math
import shutil
from pathlib import Path

import pytest

from digitalmodel.ansys.results_extractor import ResultsExtractor

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
    gdir = EXAMPLES / case / "golden"
    digest_files = sorted(gdir.glob("*_result.csv"))
    assert digest_files, f"no committed golden digest for {case} in {gdir}"
    digest = ResultsExtractor().parse_result_digest(
        digest_files[0].read_text(encoding="utf-8")
    )
    provenance = json.loads((gdir / "PROVENANCE.json").read_text(encoding="utf-8"))
    deck = EXAMPLES / case / provenance["input"]["deck"]
    actual_sha256 = hashlib.sha256(deck.read_bytes()).hexdigest()
    assert actual_sha256 == provenance["input"]["sha256"], (
        f"{case} deck SHA-256 differs from the recorded golden input"
    )
    return digest, provenance


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

    observed = digest["max_seqv_mpa"]
    tol = prov["comparator"]["tolerance_pct"]
    deviation = abs(observed - expected_vm) / expected_vm * 100.0
    assert deviation <= tol, (
        f"golden {observed:.4f} MPa deviates {deviation:.2f}% from the closed-form "
        f"{expected_vm:.4f} MPa (tolerance {tol}%)"
    )


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
    status = prov["acceptance"]["expected_status"]
    assert status in {"within_allowable", "exceeds_allowable"}
    if status == "within_allowable":
        assert digest["uc"] <= 1.0, f"{case} records within_allowable but uc={digest['uc']}"
    else:
        assert digest["uc"] > 1.0


@pytest.mark.parametrize("case", GOLDEN_CASES)
def test_golden_respects_linear_elastic_validity(case: str):
    """A linear result at yield is not an admissible screening answer.

    It must either sit inside the elastic range or say explicitly that it does
    not. Passing silently at 99.8 percent of yield is what the superseded
    parameterisation did.
    """
    digest, prov = _golden(case)
    module = _build_module(case)
    yield_mpa = getattr(
        getattr(module, "CONDITIONS", None), "yield_strength_mpa", None
    ) or getattr(module.GEOM, "yield_strength_mpa", None)
    assert yield_mpa, f"{case} build.py exposes no yield strength"

    exceeded = prov["acceptance"]["linear_elastic_limit_exceeded"]
    if not exceeded:
        assert digest["max_seqv_mpa"] < yield_mpa, (
            f"{case} peak {digest['max_seqv_mpa']} MPa reaches the {yield_mpa} MPa "
            "yield while its provenance records the elastic limit as not exceeded"
        )


# --- equilibrium: the check that would have caught the original defect -------


@pytest.mark.parametrize("case", GOLDEN_CASES)
def test_golden_reactions_balance(case: str):
    """Every reported reaction must vanish.

    Both decks are constructed so their applied loads are self-balancing: the
    pressure vessel applies no axial load and restrains nothing radially, and the
    mudmat's applied field and soil patch integrate to the same force and moment.
    The restraints therefore carry no load, and a non-zero reaction means the
    applied system does not close.

    This is the check that would have made every defect in #2094 self-announcing:
    a large radial reaction at the over-constrained node, an applied resultant
    4.88 percent short of intent, a near-zero reaction against 800 kN applied.
    """
    digest, prov = _golden(case)
    tol = prov.get("comparator", {}).get("tolerance_n", 1.0)
    reactions = {k: v for k, v in digest.items() if k.startswith("reaction_")}
    assert reactions, f"{case} golden records no reaction: equilibrium unchecked"
    for key, value in reactions.items():
        assert abs(value) < tol, (
            f"{case} {key}={value} N exceeds {tol} N; the applied system does not close"
        )


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

    Deselected by default (`-m "not requires_mapdl"`). Tolerance is 1e-3
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
