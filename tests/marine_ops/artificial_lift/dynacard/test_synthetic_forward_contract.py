# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent boundary checks for synthetic pump-to-surface conversion.

All dimensioned fixture values are arbitrary mathematical test inputs, not
qualified operating data. Static expectations use MIT, *Trusses* (2000),
"Deflections: Geometrical approach": delta = P L / (A E), and force equilibrium.
Source: https://web.mit.edu/course/3/3.11/www/modules/truss.pdf
"""

import importlib.util
import json
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard import card_generators
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings import (
    solver as marching_solver,
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

    Expected values use stated literals rather than importing solver constants:
    g=9.80665 m/s^2; 1 ft=0.3048 m; 1 in=0.0254 m;
    1 psi=6894.7572931783 Pa. Force and density retain the model's rounded
    conversions: 1 N=0.224809 lbf and 1 lb/ft^3=16.0185 kg/m^3.
    """
    rod = context.rod_string[0]
    length = rod.length * 0.3048
    area = np.pi * (rod.diameter * 0.0254) ** 2 / 4
    modulus = rod.modulus_of_elasticity * 6894.7572931783
    pump_force = context.surface_card.load[0] / 0.224809
    extension = pump_force * length / (modulus * area) / 0.0254
    buoyant_weight = (
        (rod.density - context.fluid_density) * 16.0185
        * area * length * 9.80665 * 0.224809
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


@pytest.mark.parametrize("direction", ["forward", "downward"])
@pytest.mark.parametrize("offset", [-1, 0])
def test_periodic_sample_boundary_is_shared(context, direction, offset):
    """Both directions reject one below the stencil minimum and accept it."""
    count = marching_solver.MIN_CARD_SAMPLES + offset
    card = CardData(position=[0.0] * count, load=[1000.0] * count)

    def convert():
        if direction == "forward":
            surface = card_generators.surface_card_from_pump_card(
                card, context, n_nodes=3
            )
            return len(surface.position)
        sim = marching_solver.build_simulation(
            position=np.asarray(card.position),
            load=np.asarray(card.load),
            rods=marching_solver.RodString(
                diameters=np.array([0.0254]), lengths=np.array([30.48])
            ),
            strokes_per_minute=context.spm,
            n_nodes=3,
        )
        return sim.n_t

    if offset < 0:
        with pytest.raises(ValueError, match=f"at least {marching_solver.MIN_CARD_SAMPLES}"):
            convert()
    else:
        assert convert() == count


def test_dynamic_example_matches_readme_figures():
    """Pin the documented rounded observation, not a physical acceptance limit."""
    example = (
        Path(__file__).resolve().parents[4] / "examples/workflows/dynacard-diagnostics"
    )
    spec = importlib.util.spec_from_file_location(
        "dynacard_round_trip", example / "round_trip.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    expected = json.loads(
        (example / "README.md").read_text().split("```json\n", 1)[1].split("```", 1)[0]
    )

    result = module.run_example()

    assert result.keys() == expected.keys()
    for key, value in expected.items():
        if isinstance(value, (int, float)):
            assert result[key] == pytest.approx(value, rel=0.0, abs=0.05), key
        else:
            assert result[key] == value, key


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
