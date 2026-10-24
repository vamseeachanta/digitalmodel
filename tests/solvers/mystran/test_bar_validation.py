# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline deck and comparison regressions for rectangular CBAR bending.

Independent hand calculation for the assumed example inputs L=1 m,
E=210 GPa, nu=0.3 and P=1000 N (not certified material properties):

* b=1/20 m, h=1/10 m: A=1/200 m², I1=h*b³/12=1/960000 m⁴,
  I2=b*h³/12=1/240000 m⁴; delta_z=1000/(3*210e9/240000)=1/2625 m.
* b=1/10 m, h=1/20 m: A=1/200 m², I1=1/240000 m⁴,
  I2=1/960000 m⁴; delta_z=1000/(3*210e9/960000)=4/2625 m.
* b=h=1/10 m: A=1/100 m², I1=I2=1/120000 m⁴;
  delta_z=1000/(3*210e9/120000)=1/5250 m.

Sources: MYSTRAN User Manual (2026-07-01), section 6.2.2.4 and PBAR
section 11.51: I1 is about local z, I2 about local y; blank K1/K2 omit
shear flexibility. Rectangle integrals: University of Illinois Dynamics
Reference, 'Moments of inertia', area-moment warning (web edition):
https://dynref.engr.illinois.edu/rem.html . Cantilever formula: MIT 2.080J
Structural Mechanics (2013), Problem Set 10 solutions, Problem 12-2, p. 8.
The worked example in the module docs gives the public source links.

Synthetic F06 values below come from this arithmetic, never a solver run.
"""

import pytest

from digitalmodel.solvers.mystran.fem_chain import MystranChain


@pytest.fixture
def offline_chain(tmp_path, monkeypatch):
    """Inject an explicitly supplied F06 tip value at the solve boundary."""
    chain = MystranChain(tmp_path)

    def run(b, h, tip):
        def synthetic_solve():
            # Four elements give tip grid 5; zero entries are manufactured.
            output = (
                " D I S P L A C E M E N T S\n"
                f" 5 0 0.0 0.0 {tip:.16e} 0.0 0.0 0.0\n"
            )
            (tmp_path / f"{chain.JOB_NAME}.F06").write_text(output)
            return {"success": True}

        monkeypatch.setattr(chain, "solve", synthetic_solve)
        result = chain.run_cantilever_bar_validation(
            n_elems=4, L=1.0, b=b, h=h, E=210e9, nu=0.3, P=1000.0,
        )
        deck = (tmp_path / f"{chain.JOB_NAME}.bdf").read_text()
        return result, [line.split(",") for line in deck.splitlines()]

    return run


@pytest.mark.parametrize(
    "b,h,area,i1,i2,tip",
    [
        (0.05, 0.1, 1 / 200, 1 / 960000, 1 / 240000, 1 / 2625),
        (0.1, 0.05, 1 / 200, 1 / 240000, 1 / 960000, 4 / 2625),
        (0.1, 0.1, 1 / 100, 1 / 120000, 1 / 120000, 1 / 5250),
    ],
    ids=["tall", "wide", "square"],
)
def test_deck_and_z_reference_use_matching_second_moment(
    offline_chain, b, h, area, i1, i2, tip,
):
    """Pin both deck planes and the reference to the independent hand sums."""
    result, cards = offline_chain(b, h, tip)
    pbar, = [card for card in cards if card[0] == "PBAR"]
    # _f writes eight significant digits: half of the last place gives
    # relative error <= 5e-8 (absolute half-place / smallest leading digit).
    assert [float(value) for value in pbar[3:6]] == pytest.approx(
        [area, i1, i2], rel=5e-8, abs=0.0,
    )
    bars = [card for card in cards if card[0] == "CBAR"]
    assert len(bars) == 4  # Assumed mesh subdivisions in the example.
    assert all([float(v) for v in bar[5:8]] == [0, 1, 0] for bar in bars)
    force, = [card for card in cards if card[0] == "FORCE"]
    assert force[2] == "5"
    assert [float(v) for v in force[4:8]] == [1000, 0, 0, 1]
    assert result["exact"] == pytest.approx(tip)
    assert result["tip_displacement"] == pytest.approx(tip)
    assert result["tip_displacement_mean"] == pytest.approx(tip)
    assert result["error_pct"] == pytest.approx(0.0)


def test_comparison_exposes_wrong_axis_response(offline_chain):
    """Old tall-section response / right response = 4; (4-1)*100 = 300%."""
    result, _ = offline_chain(0.05, 0.1, 4 / 2625)
    assert result["exact"] == pytest.approx(1 / 2625)
    assert result["error_pct"] == pytest.approx(300.0)
