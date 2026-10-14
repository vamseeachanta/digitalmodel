# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent solutions of the PDE already stated in finite_difference.py.

No fitted damping, well data or reference solver supplies either expected curve.
The criteria file fixes tolerances before the retained computations start.
"""

import hashlib
import json
from datetime import datetime
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.solver import (
    EverittJenningsSolver, RodString,
)

DATA = Path(__file__).with_name("testdata")
CRITERIA = DATA / "solver_guard_criteria.json"
RESULTS = DATA / "solver_guard_results.json"
MANIFEST = DATA / "solver_guard_manifest.json"


def native_case(case):
    """Run one uniform, undamped, frictionless synthetic boundary condition."""
    inputs = json.loads(CRITERIA.read_text())["inputs"]
    phase = np.linspace(0.0, 2.0 * np.pi, inputs["samples"])
    area = np.pi * inputs["diameter_m"] ** 2 / 4.0
    weight = ((inputs["density_kg_m3"] - inputs["fluid_density_kg_m3"])
              * area * inputs["length_m"] * inputs["gravity_m_s2"])
    position = inputs["surface_stroke_m"] / 2.0 * (1.0 - np.cos(phase))
    speed = inputs["harmonic_speed_spm"]
    if case == "static":
        load = weight + inputs["load_range_n"] * position / inputs["surface_stroke_m"]
        speed *= inputs["static_speed_fraction"]
    else:
        load = np.full(inputs["samples"], weight + inputs["load_range_n"])
    rods = RodString(
        diameters=np.array([inputs["diameter_m"]]),
        lengths=np.array([inputs["length_m"]]),
        densities=np.array([inputs["density_kg_m3"]]),
        moduli=np.array([inputs["modulus_pa"]]),
    )
    card = EverittJenningsSolver(n_nodes=inputs["nodes"], smooth_window=0).solve(
        position=position, load=load, rods=rods, strokes_per_minute=speed,
        pump_diameter=inputs["pump_diameter_m"], tubing_id=inputs["tubing_id_m"],
        fluid_density=inputs["fluid_density_kg_m3"], damping=np.zeros(2),
    )
    return inputs, phase, position, load, weight, card


def _retained_case(case):
    """Fail closed when the native arrays were not retained or are stale."""
    record = json.loads(RESULTS.read_text())["analytical"][case]
    values = native_case(case)
    card = values[-1]
    np.testing.assert_allclose(record["position_m"], card.position, rtol=0.0, atol=1e-12)
    np.testing.assert_allclose(record["load_n"], card.load, rtol=0.0, atol=1e-8)
    return values


def test_static_limit_load_and_elastic_shortening():
    """As omega -> 0, u_xx -> 0 for the gravity-subtracted vertical datum.

    Integrating Hooke's law gives F_p(t)=F_s(t)-W_b and
    p_p(t)=p_s(t)-(F_s(t)-W_b)/K_r, K_r=EA/L. The synthetic load
    changes by F_o across a surface stroke S, so S_p=S-F_o/K_r.
    This checks the load datum AND stroke reduction; a load offset alone
    cannot satisfy the second assertion.
    """
    inputs, _, position, load, weight, card = _retained_case("static")
    criteria = json.loads(CRITERIA.read_text())
    stiffness = inputs["modulus_pa"] * np.pi * inputs["diameter_m"] ** 2 / (4.0 * inputs["length_m"])
    expected_load = load - weight
    expected_position = position - expected_load / stiffness
    assert np.max(np.abs(card.load - expected_load)) / inputs["load_range_n"] < criteria["static_load_relative_tolerance"]
    assert card.stroke == pytest.approx(float(np.ptp(expected_position)), rel=criteria["analytical_position_relative_tolerance"])


def test_uniform_undamped_harmonic_wave_solution():
    """For u_tt=c^2 u_xx, set k=omega/c and u=-p (positive-down).

    The constant reduced surface load F_0 gives u_x(0,t)=F_0/(EA).
    Surface p(t)=A(1-cos omega t) then has the independent solution
    p(x,t)=A(1-cos(kx)cos omega t)-F_0*x/(EA),
    F(x,t)=F_0-EA*A*k*sin(kx)*cos omega t.
    Both PDE and the two surface boundary conditions are satisfied by
    differentiation. Compare absolute pump position and phase-resolved load,
    not only stroke, correlation or a round-trip through the same solver.
    """
    inputs, phase, _, _, _, card = _retained_case("harmonic")
    criteria = json.loads(CRITERIA.read_text())
    ea = inputs["modulus_pa"] * np.pi * inputs["diameter_m"] ** 2 / 4.0
    speed = np.sqrt(inputs["modulus_pa"] / inputs["density_kg_m3"])
    omega = 2.0 * np.pi * inputs["harmonic_speed_spm"] / 60.0
    k = omega / speed
    amplitude = inputs["surface_stroke_m"] / 2.0
    pump_amplitude = amplitude * np.cos(k * inputs["length_m"])
    load_amplitude = ea * amplitude * k * np.sin(k * inputs["length_m"])
    expected_position = amplitude - pump_amplitude * np.cos(phase) - inputs["load_range_n"] * inputs["length_m"] / ea
    expected_load = inputs["load_range_n"] - load_amplitude * np.cos(phase)
    assert np.max(np.abs(card.position - expected_position)) / abs(pump_amplitude) < criteria["analytical_position_relative_tolerance"]
    assert np.max(np.abs(card.load - expected_load)) / abs(load_amplitude) < criteria["analytical_load_relative_tolerance"]


def test_retained_record_pins_criteria_before_both_kinds_of_run():
    manifest = json.loads(MANIFEST.read_text())
    assert manifest["criteria_sha256"] == hashlib.sha256(CRITERIA.read_bytes()).hexdigest()
    assert manifest["results_sha256"] == hashlib.sha256(RESULTS.read_bytes()).hexdigest()
    root = Path(__file__).resolve().parents[4]
    for relative, expected_digest in manifest["source_sha256"].items():
        assert hashlib.sha256((root / relative).read_bytes()).hexdigest() == expected_digest
    written = datetime.fromisoformat(manifest["criteria_written_at"])
    assert written < datetime.fromisoformat(manifest["first_comparison_started_at"])
    assert written < datetime.fromisoformat(manifest["first_analytical_started_at"])
