# ABOUTME: Sampling stability limit of the Everitt-Jennings depth march (#4159).
# ABOUTME: A card sampled above the Courant limit must be refused before marching.
"""Tests for the depth-march stability check.

Without damping the march is ``u[i+1] = 2u[i] - u[i-1] + r^2 D2t(u[i])`` with
``r = dx / (c dt)`` and ``c = sqrt(E / rho)``; it is stable only for ``r <= 1``
in every taper (Courant, Friedrichs and Lewy, Math. Ann. 100, 1928). On the
example well below, 6,377 intervals per stroke is the largest stable sampling.
Before this check 6,400 intervals returned a peak pump load of about 2.29e8 lbf
with no flag, while 6,377 returned 4,758.4 lbf.
"""

import math

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    surface_card_from_pump_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings import (
    EverittJenningsSolver,
    RodString,
    build_simulation,
    rod_string_from_context,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings import (
    units as U,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.solver import (
    max_stable_intervals,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    CardData,
    DynacardAnalysisContext,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow

# Example well of the worked dynacard examples: illustrative design values,
# no measured well. Rod order is surface first; ft, in, lb/ft, psi.
EXAMPLE_WELL = {
    "api14": "PLATE-EXAMPLE",
    "spm": 6.42,
    "fluid_density": 63.237,
    "pump": {"diameter": 1.5, "depth": 4844.0},
    "input_params": {"strokes_per_minute": 6.42, "tubing_pressure": 100.0},
    "rod_string": [
        {"diameter": 0.875, "length": 1400.0, "weight_per_foot": 2.224,
         "modulus_of_elasticity": 30500000.0},
        {"diameter": 0.75, "length": 3350.0, "weight_per_foot": 1.634,
         "modulus_of_elasticity": 30500000.0},
    ],
}
SPM = 6.42
# The workflow's Everitt-Jennings context solver uses 200 depth nodes.
WORKFLOW_NODES = 200
EXAMPLE_LIMIT = 6377


def smooth_card(intervals: int) -> CardData:
    """Closed surface card of the issue report, ``intervals`` steps per stroke."""
    phase = 2.0 * np.pi * np.arange(intervals + 1) / intervals
    return CardData(
        position=(20.0 * (1.0 - np.cos(phase))).tolist(),
        load=(10500.0 + 1850.0 * np.sin(phase)).tolist(),
    )


def example_context(intervals: int) -> DynacardAnalysisContext:
    return DynacardAnalysisContext(**EXAMPLE_WELL, surface_card=smooth_card(intervals))


@pytest.fixture(scope="module")
def example_rods() -> RodString:
    return rod_string_from_context(example_context(8))


def simulate(rods: RodString, intervals: int, n_nodes: int = WORKFLOW_NODES):
    card = smooth_card(intervals)
    return build_simulation(
        position=np.asarray(card.position) * U.IN2M,
        load=np.asarray(card.load) * U.LB2N,
        rods=rods,
        strokes_per_minute=SPM,
        n_nodes=n_nodes,
    )


def test_example_well_limit(example_rods):
    """Largest stable sampling: floor(T * c_min / dx) on the workflow grid."""
    period = 60.0 / SPM
    dx = example_rods.total_length / (WORKFLOW_NODES - 1)
    slowest = np.min(np.sqrt(example_rods.moduli / example_rods.densities))
    assert math.floor(period * slowest / dx) == EXAMPLE_LIMIT
    assert max_stable_intervals(example_rods, SPM, WORKFLOW_NODES) == EXAMPLE_LIMIT


def test_card_at_limit_is_accepted(example_rods):
    sim = simulate(example_rods, EXAMPLE_LIMIT)
    # Courant number dx / (c dt) does not exceed 1 in any taper.
    assert np.all(sim.dx / (sim.wave_speed * sim.dt) <= 1.0)


def test_card_one_interval_above_limit_is_refused(example_rods):
    with pytest.raises(ValueError, match=r"at most 6377 intervals per stroke") as info:
        simulate(example_rods, EXAMPLE_LIMIT + 1)
    # Just above 1, the Courant number is still printed as exceeding 1.
    assert "Courant number 1.00006 > 1" in str(info.value)


def test_refusal_states_the_sampling_and_the_courant_number(example_rods):
    with pytest.raises(ValueError) as info:
        simulate(example_rods, 6400)
    message = str(info.value)
    assert "6400 intervals per stroke" in message
    assert "199 depth cells" in message
    assert "Courant number 1.00351 > 1" in message
    assert "use more depth nodes" in message


def test_limit_follows_the_depth_grid(example_rods):
    """A longer depth cell needs a longer time step: fewer nodes, lower limit."""
    slowest = np.min(np.sqrt(example_rods.moduli / example_rods.densities))
    for n_nodes in (100, 400):
        assert max_stable_intervals(example_rods, SPM, n_nodes) == math.floor(
            (60.0 / SPM) * slowest / (example_rods.total_length / (n_nodes - 1))
        )
    coarse = max_stable_intervals(example_rods, SPM, 100)
    assert coarse < EXAMPLE_LIMIT < max_stable_intervals(example_rods, SPM, 400)
    simulate(example_rods, coarse, n_nodes=100)
    with pytest.raises(ValueError, match=rf"at most {coarse} intervals per stroke"):
        simulate(example_rods, coarse + 1, n_nodes=100)
    # The card refused on the workflow grid is accepted on a finer one.
    simulate(example_rods, EXAMPLE_LIMIT + 1, n_nodes=400)


def test_slowest_taper_governs():
    """dt >= dx / c must hold in every taper, so the smallest c sets the limit."""
    rods = RodString(
        diameters=np.array([0.0254, 0.0222]),
        lengths=np.array([500.0, 1000.0]),
        densities=np.array([7850.0, 2700.0]),
        moduli=np.array([2.07e11, 0.70e11]),
    )
    speeds = np.sqrt(rods.moduli / rods.densities)
    assert speeds[1] < speeds[0]
    dx = rods.total_length / 99
    expected = math.floor(60.0 / 10.0 * speeds[1] / dx)
    assert max_stable_intervals(rods, 10.0, 100) == expected


@pytest.mark.parametrize(
    "strokes_per_minute, n_nodes",
    [(0.0, 200), (-1.0, 200), (float("nan"), 200), (6.42, 1), (6.42, 2.5)],
)
def test_limit_rejects_invalid_arguments(example_rods, strokes_per_minute, n_nodes):
    with pytest.raises(ValueError):
        max_stable_intervals(example_rods, strokes_per_minute, n_nodes)


def test_workflow_refuses_unstable_card_before_marching():
    """The issue's 6,400-interval card is refused instead of returning ~2.29e8 lbf."""
    workflow = DynacardWorkflow(example_context(6400))
    with pytest.raises(ValueError, match=r"at most 6377 intervals per stroke"):
        workflow.run_full_analysis()


@pytest.mark.slow
def test_workflow_extrema_at_limit_match_coarser_sampling():
    """Sampling regression on the illustrative tapered well, to 0.1 lbf."""
    extrema = []
    for intervals in (1000, EXAMPLE_LIMIT):
        results = DynacardWorkflow(example_context(intervals)).run_full_analysis()
        load = np.asarray(results.downhole_card.load)
        assert np.isfinite(load).all()
        extrema.append([load.min(), load.max()])
    np.testing.assert_allclose(extrema[1], extrema[0], rtol=0, atol=0.1)


def test_uniform_rod_at_limit_reverses_the_harmonic_load():
    """Half-wave solution from the bar equation in Applied Mechanics of Solids
    (online edition, §4.3.3, 1-D Bar subjected to end loading).

    With a fixed surface, harmonic axial displacement is proportional to
    sin(kx) sin(phase). Axial force EA du/dx therefore varies as cos(kx).
    Here c = sqrt(E/rho) = 5000 m/s and T = 1 s. At L = cT/2 the harmonic
    force changes sign. Neutral buoyancy preserves the 1000 N mean tension.
    """
    rods = RodString(
        diameters=np.array([0.0254]), lengths=np.array([2500.0]),
        densities=np.array([8000.0]), moduli=np.array([2.0e11]),
    )
    # Four 625 m cells, each crossed in 1/8 s, give Courant 1 at eight intervals.
    phase = np.linspace(0.0, 2.0 * np.pi, 9)
    card = EverittJenningsSolver(n_nodes=5, smooth_window=0).solve(
        position=np.zeros_like(phase), load=1000.0 + 200.0 * np.sin(phase),
        rods=rods, strokes_per_minute=60.0, pump_diameter=0.04, tubing_id=0.06,
        fluid_density=8000.0, damping=np.zeros(2),
    )

    # F_p = 1000 - 200 sin(phase) N gives mean 1000, trough 800 and crest 1200.
    diagonal = 100.0 * math.sqrt(2.0)
    expected = [1000, 1000 - diagonal, 800, 1000 - diagonal, 1000,
                1000 + diagonal, 1200, 1000 + diagonal, 1000]
    # At Courant 1 the sampled wave travels exactly one cell per time step.
    # The harmonic strain increments at the two ends of a half wavelength
    # are equal and opposite. At the closing phase the harmonic vanishes.
    # Allow 1e-10 N for roundoff across four cells on a 1200 N load scale.
    np.testing.assert_allclose(card.load, expected, rtol=0, atol=1e-10)


def test_forward_model_refuses_unstable_pump_card():
    """The upward march is the algebraic inverse on the same grid and limit."""
    with pytest.raises(ValueError, match=r"at most 6377 intervals per stroke"):
        surface_card_from_pump_card(
            smooth_card(EXAMPLE_LIMIT + 1), example_context(8), n_nodes=WORKFLOW_NODES
        )
