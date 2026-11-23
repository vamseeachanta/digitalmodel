"""FAD curve consolidation (#2160): one BS 7910 Option 1 implementation.

Comparators are closed-form values written out by hand from the published
equation forms (not produced by calling the code under test):

- BS 7910:2013 Option 1 (Cl. 7.3.2 form):
  f(Lr) = (1 + 0.5 Lr^2)^-0.5 (0.3 + 0.7 exp(-mu Lr^6))            Lr <= 1
  f(Lr) = f(1) Lr^((N-1)/(2N))                                     1 < Lr <= Lr_max
  mu = min(0.001 E/sigma_y, 0.6),  N = 0.3 (1 - sigma_y/sigma_u),
  Lr_max = (sigma_y + sigma_u)/(2 sigma_y).
- API 579-1/ASME FFS-1 Part 9 Level 2 (2016) = BS 7910 Level 2A generic
  curve = original R6 Option 1 curve (open-literature citation on the #2157
  plan, PR #2194):
  f(Lr) = (1 - 0.14 Lr^2)(0.3 + 0.7 exp(-0.65 Lr^6))               Lr <= Lr_max

Material for the hand values: sigma_y = 500 MPa, sigma_u = 600 MPa,
E = 200 000 MPa, so that mu = 0.4, N = 0.05, (N-1)/(2N) = -9.5 and
Lr_max = 1.1 exactly.
"""

from __future__ import annotations

import math
from pathlib import Path
from types import SimpleNamespace

import pytest
import yaml

from digitalmodel.asset_integrity.assessment import crack_fad
from digitalmodel.asset_integrity.assessment.crack_fad import (
    fad_curve_api579_level2,
    fad_curve_option1,
    lr_max,
)

SY, SU, E = 500.0, 600.0, 200_000.0
LR_MAX = 1.1  # (500 + 600) / (2 * 500)

# Hand-derived Option 1 values (see module docstring for the arithmetic).
#   Lr = 0.5: (1 + 0.125)^-0.5 * (0.3 + 0.7 exp(-0.4 * 0.015625))
#   Lr = 1.0: 1.5^-0.5 * (0.3 + 0.7 exp(-0.4))
#   Lr = 1.1: f(1) * 1.1^-9.5
OPT1_EXPECTED = {
    0.0: 1.0,
    0.5: 1.125 ** -0.5 * (0.3 + 0.7 * math.exp(-0.00625)),  # 0.938697
    1.0: 1.5 ** -0.5 * (0.3 + 0.7 * math.exp(-0.4)),  # 0.628069
    LR_MAX: 1.5 ** -0.5 * (0.3 + 0.7 * math.exp(-0.4)) * 1.1 ** -9.5,  # 0.253967
}

# Hand-derived Level 2A / API 579 Level 2 generic values.
#   Lr = 0.5: (1 - 0.035)(0.3 + 0.7 exp(-0.65 * 0.015625))
#   Lr = 1.0: 0.86 (0.3 + 0.7 exp(-0.65))
#   Lr = 1.1: (1 - 0.14 * 1.21)(0.3 + 0.7 exp(-0.65 * 1.771561))
API579_EXPECTED = {
    0.0: 1.0,
    0.5: 0.965 * (0.3 + 0.7 * math.exp(-0.01015625)),  # 0.958174
    1.0: 0.86 * (0.3 + 0.7 * math.exp(-0.65)),  # 0.572272
    LR_MAX: 0.8306 * (0.3 + 0.7 * math.exp(-0.65 * 1.771561)),  # 0.433000
}


# ---------------------------------------------------------------------------
# Closed-form comparators
# ---------------------------------------------------------------------------
def test_lr_max_hand_value():
    assert lr_max(SY, SU) == pytest.approx(LR_MAX, abs=1e-12)


@pytest.mark.parametrize("lr, expected", sorted(OPT1_EXPECTED.items()))
def test_option1_matches_hand_values(lr, expected):
    assert fad_curve_option1(lr, SY, SU, E) == pytest.approx(expected, abs=1e-9)


@pytest.mark.parametrize("lr, expected", sorted(API579_EXPECTED.items()))
def test_api579_level2_matches_hand_values(lr, expected):
    assert fad_curve_api579_level2(lr, SY, SU) == pytest.approx(expected, abs=1e-9)


def test_option1_literal_decimals():
    """Literal decimals (4 s.f. by hand) as a second, formula-free anchor."""
    assert fad_curve_option1(0.5, SY, SU, E) == pytest.approx(0.9387, abs=5e-5)
    assert fad_curve_option1(1.0, SY, SU, E) == pytest.approx(0.6281, abs=5e-5)
    assert fad_curve_option1(1.1, SY, SU, E) == pytest.approx(0.2540, abs=5e-5)


def test_api579_literal_decimals():
    assert fad_curve_api579_level2(0.5, SY, SU) == pytest.approx(0.9582, abs=5e-5)
    assert fad_curve_api579_level2(1.0, SY, SU) == pytest.approx(0.5723, abs=5e-5)
    assert fad_curve_api579_level2(1.1, SY, SU) == pytest.approx(0.4330, abs=5e-5)


CURVES = [
    pytest.param(lambda lr: fad_curve_option1(lr, SY, SU, E), id="option1"),
    pytest.param(lambda lr: fad_curve_api579_level2(lr, SY, SU), id="api579_level2"),
]


@pytest.mark.parametrize("curve", CURVES)
def test_both_curves_cut_off_at_lr_max_and_reject_negative_lr(curve):
    assert curve(LR_MAX) > 0.0
    assert curve(LR_MAX + 1e-9) == 0.0
    assert curve(5.0) == 0.0
    with pytest.raises(ValueError):
        curve(-0.01)


@pytest.mark.parametrize("curve", CURVES)
def test_both_curves_monotone_non_increasing(curve):
    grid = [LR_MAX * i / 400 for i in range(401)]
    vals = [curve(lr) for lr in grid]
    assert all(a >= b - 1e-12 for a, b in zip(vals, vals[1:]))


def test_option1_and_api579_are_different_curves():
    """The two curves are NOT equivalent (the old docstring said they were).

    At Lr = 1.721 the Level 2A generic curve is material-independent to 4 d.p.
    because exp(-0.65 Lr^6) has vanished: (1 - 0.14 * 1.721^2) * 0.3 = 0.1756.
    """
    sy, su, e = 200.0, 500.0, 207_000.0  # Lr_max = 1.75 > 1.721
    api = fad_curve_api579_level2(1.721, sy, su)
    opt1 = fad_curve_option1(1.721, sy, su, e)
    assert api == pytest.approx(0.1756, abs=5e-5)
    assert abs(api - opt1) > 1e-3
    # and they already differ in the knee region
    assert abs(
        fad_curve_option1(1.0, SY, SU, E) - fad_curve_api579_level2(1.0, SY, SU)
    ) > 1e-3


def test_docstrings_do_not_claim_option1_equals_api579_level2():
    assert "equivalent to the API 579" not in (crack_fad.__doc__ or "")
    assert "== API 579" not in (fad_curve_option1.__doc__ or "")
    assert "Option 1" in (fad_curve_option1.__doc__ or "")
    assert "Level 2A" in (fad_curve_api579_level2.__doc__ or "")
    assert "R6" in (fad_curve_api579_level2.__doc__ or "")


# ---------------------------------------------------------------------------
# One implementation: the legacy entry points delegate to the canonical curve
# ---------------------------------------------------------------------------
def _legacy_cfg(smys, smus, e):
    return {
        "Outer_Pipe": {"Material": {"Material": "steel", "Material_Grade": "G"}},
        "Material": {"steel": {"E": e, "Grades": {"G": {"SMYS": smys, "SMUS": smus}}}},
    }


@pytest.mark.parametrize(
    "smys, smus, e",
    [
        (SY, SU, E),
        (448.0e6, 531.0e6, 200.0e9),  # X65 in Pa (ratios are unit-free)
        (65000.0, 77000.0, 30.0e6),  # X65 in psi
        (551.6, 620.5, 207_000.0),  # X80 in MPa
    ],
)
def test_three_legacy_entry_points_return_canonical_kr(smys, smus, e):
    from digitalmodel.asset_integrity.common.BS7910_critical_flaw_limits import (
        BS7910_2013,
    )
    from digitalmodel.asset_integrity.common.fad import FAD

    # Entry point 1: common.fad.FAD.BS7910_2013_option_1 (Lr/Kr table)
    legacy = FAD(_legacy_cfg(smys, smus, e))
    legacy.BS7910_2013_option_1()
    table = legacy.FAD["option_1"]
    assert list(table.columns) == ["L_r", "K_r"]
    assert len(table) == 201  # 101 (0..1) + 99 (1..Lr_max) + drop-to-zero row
    assert table["L_r"].iloc[-1] == pytest.approx(lr_max(smys, smus))
    assert table["K_r"].iloc[-1] == 0.0  # plot-convention closing row

    # Entry point 2: common.BS7910_critical_flaw_limits.get_K_r_allowable,
    # exercised unbound on a stub carrying only what the method reads.
    stub = SimpleNamespace(
        fad=legacy.FAD,
        material_grade_properties={"SMYS": smys, "SMUS": smus},
        material_properties={"E": e},
    )

    checked = 0
    for lr_val, kr_table in zip(table["L_r"], table["K_r"]):
        lr_val = float(lr_val)
        canonical = fad_curve_option1(lr_val, smys, smus, e)  # entry point 3
        if lr_val < lr_max(smys, smus):
            assert float(kr_table) == pytest.approx(canonical, abs=1e-12)
        stub.L_r = lr_val
        BS7910_2013.get_K_r_allowable(stub)
        assert stub.K_r_allowable == pytest.approx(canonical, abs=1e-12)
        checked += 1
    assert checked == 201


def test_get_k_r_allowable_beyond_cutoff_is_zero():
    from digitalmodel.asset_integrity.common.BS7910_critical_flaw_limits import (
        BS7910_2013,
    )

    stub = SimpleNamespace(
        material_grade_properties={"SMYS": SY, "SMUS": SU},
        material_properties={"E": E},
        L_r=LR_MAX + 0.05,
    )
    BS7910_2013.get_K_r_allowable(stub)
    assert stub.K_r_allowable == 0.0


# ---------------------------------------------------------------------------
# Repo test-vector fixture (tests/fixtures/test_vectors/structural)
# ---------------------------------------------------------------------------
FIXTURE = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "test_vectors"
    / "structural"
    / "fracture_mechanics.yaml"
)


def _fad_worked_examples():
    data = yaml.safe_load(FIXTURE.read_text(encoding="utf-8"))
    return [
        ex
        for ex in data["worked_examples"]
        if ex.get("use_as_test") and ({"Lr_max", "Kr"} & set(ex["outputs"]))
    ]


@pytest.mark.parametrize(
    "example", _fad_worked_examples(), ids=lambda ex: ex["description"][:60]
)
def test_fixture_fad_worked_examples(example):
    inp, out = example["inputs"], example["outputs"]
    smys = float(inp.get("SMYS", SY))
    smus = float(inp.get("SMUS", SU))
    tol = 1e-4
    if "Lr_max" in out:
        assert lr_max(smys, smus) == pytest.approx(out["Lr_max"], abs=tol)
    if "Kr" in out:
        # E only enters through mu; the fixture's Kr cases are at Lr = 0
        # (Kr = 1 by definition) and beyond Lr_max (cutoff, Kr = 0), both
        # independent of mu.
        kr = fad_curve_option1(float(inp["Lr"]), smys, smus, E)
        assert kr == pytest.approx(out["Kr"], abs=tol)


def test_fixture_has_fad_examples():
    assert len(_fad_worked_examples()) >= 5
