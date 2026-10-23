# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent boundary checks for synthetic pump-to-surface conversion.

All dimensioned fixture values are arbitrary mathematical test inputs, not
qualified operating data. Static expectations use MIT, *Trusses* (2000),
"Deflections: Geometrical approach": delta = P L / (A E), and force equilibrium.
Source: https://web.mit.edu/course/3/3.11/www/modules/truss.pdf
"""

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard import card_generators
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings import units
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.solver import (
    GRAVITY_M_S2,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    CardData,
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow


@pytest.fixture
def context():
    """Assumed mathematical inputs, shared with the worked static example."""
    return DynacardAnalysisContext(
        api14="SYNTHETIC",
        surface_card=CardData(position=[0.0] * 5, load=[1000.0] * 5),
        rod_string=[RodSection(diameter=1.0, length=100.0)],
        pump=PumpProperties(diameter=1.75, depth=100.0),
        spm=6.0,
    )


@pytest.mark.parametrize("nodes", [3, 5, 17])
@pytest.mark.parametrize("zero_datum", [False, True])
def test_static_forward_boundary_matches_hand_calculation(context, nodes, zero_datum):
    """With no time variation, inertia and viscous damping both vanish.

    Integrate delta = P L/(EA): surface position = pump position + delta.
    Equilibrium gives surface force = pump force + (rho-rho_f) A L g.
    Here L=100 ft, d=1 in, P=1000 lbf are assumed test inputs; material
    and fluid properties are the supplied context values. For each grid,
    the recurrence is a straight line in depth, so only floating point
    roundoff remains. The 1e-9 relative/absolute tolerances below are
    arithmetic checks, not a field acceptance criterion.
    """
    rod = context.rod_string[0]
    length = rod.length * units.FT2M
    area = np.pi * (rod.diameter * units.IN2M) ** 2 / 4
    modulus = rod.modulus_of_elasticity * units.PSI2PA
    pump_force = context.surface_card.load[0] * units.LB2N
    extension = pump_force * length / (modulus * area) * units.M2IN
    buoyant_weight = (
        (rod.density - context.fluid_density) * units.LBPFT32KGPM3
        * area * length * GRAVITY_M_S2 * units.N2LB
    )

    surface = card_generators.surface_card_from_pump_card(
        context.surface_card, context, n_nodes=nodes, zero_position_datum=zero_datum
    )

    np.testing.assert_allclose(
        surface.position, 0.0 if zero_datum else extension, rtol=1e-9, atol=1e-9
    )
    np.testing.assert_allclose(surface.load, 1000.0 + buoyant_weight, rtol=1e-9)


@pytest.mark.parametrize(
    "position,load,reason",
    [
        ([], [], "at least"),
        ([0.0] * 3, [1.0] * 3, "at least"),
        ([0.0] * 5, [1.0] * 4, "same number"),
        ([0.0, np.nan, 1.0, 0.0], [1.0] * 4, "finite"),
        ([0.0, 1.0, np.inf, 0.0], [1.0] * 4, "finite"),
        ([0.0] * 4, [1.0, np.inf, 1.0, 1.0], "finite"),
    ],
)
def test_invalid_pump_card_fails_before_marching(context, position, load, reason):
    """The periodic stencil needs equal, finite arrays and an interior sample."""
    with pytest.raises(ValueError, match=reason):
        card_generators.surface_card_from_pump_card(
            CardData(position=position, load=load), context
        )


@pytest.mark.parametrize("nodes", [0, 1, 2, 3.5, True])
def test_invalid_spatial_grid_is_rejected(context, nodes):
    """A three-row recurrence requires an integer grid with at least three rows."""
    with pytest.raises(ValueError, match="n_nodes"):
        card_generators.surface_card_from_pump_card(
            context.surface_card, context, n_nodes=nodes
        )


@pytest.mark.parametrize("speed", [0.0, -1.0, np.nan, np.inf])
def test_invalid_speed_is_rejected(context, speed):
    """A finite positive period is required to define time differences."""
    context.spm = speed
    with pytest.raises(ValueError, match="spm"):
        card_generators.surface_card_from_pump_card(context.surface_card, context)


def test_unsettled_turnaround_is_not_returned_as_a_surface_card(context, monkeypatch):
    """A two-cycle has no fixed point: each march selects a different maximum.

    This is an exact discrete consistency condition, not a tuned tolerance.
    The artificial march isolates convergence from the physical coefficients.
    """
    calls = 0

    def alternating_turnaround(u_pump, u_above, sim, coeff):
        nonlocal calls
        calls += 1
        u = np.zeros((sim.n_x, sim.n_t))
        u[0, 1 if sim.up_dn != 1 else 2] = -1.0
        u[1] = u[0] + 1.0
        return u

    monkeypatch.setattr(card_generators, "_march_up", alternating_turnaround)
    with pytest.raises(ValueError, match="turnaround.*converge"):
        card_generators.surface_card_from_pump_card(context.surface_card, context)
    assert calls == card_generators._MAX_TURNAROUND_ITERATIONS


@pytest.mark.parametrize("value", [np.inf, np.nan, np.finfo(float).max])
def test_nonfinite_march_is_rejected(context, monkeypatch, value):
    """An overflow must fail before a nonfinite surface card reaches diagnosis."""
    monkeypatch.setattr(
        card_generators, "_march_up",
        lambda u_pump, u_above, sim, coeff: np.full((sim.n_x, sim.n_t), value),
    )
    with pytest.raises(ValueError, match="nonfinite"):
        card_generators.surface_card_from_pump_card(context.surface_card, context)


def test_explicit_well_data_takes_precedence_over_synthetic_settings(context, monkeypatch):
    """Measured inputs must not be forward-modelled a second time."""
    original = context.model_dump()
    cfg = {"well_data": original, "synthetic_card": {"mode": "NORMAL"}}

    def unexpected_forward(*args, **kwargs):
        pytest.fail("explicit surface data must not enter the synthetic model")

    monkeypatch.setattr(card_generators, "surface_card_from_pump_card", unexpected_forward)
    DynacardWorkflow()._apply_synthetic_card(cfg)
    assert cfg["well_data"] is original
    assert cfg["well_data"] == context.model_dump()
