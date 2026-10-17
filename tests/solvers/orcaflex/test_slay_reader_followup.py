"""Synthetic reader-to-criteria regressions; no native qualification is claimed."""

import json
import math
from types import SimpleNamespace

import pytest

from digitalmodel.solvers.orcaflex.modular_generator import slay_readback as reader
from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import evaluate_static_acceptance


def vessel_double(forces, contacts):
    return SimpleNamespace(
        vars=lambda **_: ["Support reaction force", "Support contact arc length"],
        StaticResult=lambda name, extra: (
            forces if name == "Support reaction force" else contacts
        )[extra.SupportIndex - 1],
    )


def graph(stations, values):
    return SimpleNamespace(X=stations, Mean=values)


@pytest.mark.parametrize("last_force,failed", [(56.089, 2), (0.0, 1)])
def test_d0_t0_shaped_reader_keeps_rejected_rows(last_force, failed):
    # Synthetic geometry, not a retained native array. Forces are in kN.
    forces, contacts = reader.support_readback(
        vessel_double([1.0] * 24 + [last_force], [i * 5 / 24 for i in range(25)]),
        SimpleNamespace, 25, "pipe",
    )
    # Twelve firing supports precede thirteen stinger rollers.
    selected, lift_off = reader.last_contact(list(range(0, 65, 5)), forces, 12)
    assert selected == (24 if last_force else 23)
    assert lift_off == (60 if last_force else 55)
    nodes = [0, 5, 10, 15, 20, 25]
    force_arcs = [0, 2.5, 7.5, 12.5, 17.5, 22.5, 25]
    reading = reader.static_readback(
        graph(nodes, [0, -5, -10, -15, -20, -25]),
        graph(nodes, [0, -1, -3, -6, -7, -7.5]),
        graph(force_arcs, [1, 2, -100, 3, 80, 4, 5]),
        graph(force_arcs, [10, 20, 30, 40, 50, -99, -4494.19]),
        contact_station=contacts[selected], seabed_z=-7,
    )
    assert min(reading["grounded_tension"]) == -4494.19
    result = evaluate_static_acceptance(
        wire_length=10, end_a_reaction=forces[0], last_support_force=forces[-1],
        grounded_tension=reading["grounded_tension"],
        # Synthetic stiffness scaling keeps overbend below the existing S1 screen.
        overbend_strain=reading["overbend_moment"] * 0.0025509 / 100,
        lift_off_arc=lift_off, departure_angle=reading["departure_angle"],
        reference={"lift_off_arc": 60, "departure_angle": math.degrees(math.atan2(3, 10))},
    )
    assert result["rows"]["stinger_conformity"]["status"] == ("FAIL" if last_force else "PASS")
    assert result["rows"]["grounded_tension"]["status"] == "FAIL"
    assert result["rows"]["lift_off_arc"]["status"] == "PASS"  # Existing +/-5 m band.
    assert result["counts"] == {"passed": 6 - failed, "failed": failed, "not_evaluated": 3}


@pytest.mark.parametrize("bad_arc", [float("nan"), float("inf"), -float("inf")])
def test_unused_nonfinite_arcs_are_recorded(bad_arc):
    diagnostics = {}
    forces, contacts = reader.support_readback(
        vessel_double([1, 2, 0], [bad_arc, 5, bad_arc]),
        SimpleNamespace, 3, "pipe", diagnostics=diagnostics,
    )
    assert forces == [1, 2, 0]
    assert contacts[1] == 5
    assert diagnostics == {"nonfinite_contact_support_indices": [1, 3]}
    json.dumps(diagnostics, allow_nan=False)


@pytest.mark.parametrize("bad_arc", [float("nan"), float("inf")])
def test_selected_nonfinite_arc_is_rejected_after_diagnostics(bad_arc):
    diagnostics = {}
    with pytest.raises(ValueError, match="selected support 2"):
        reader.support_readback(vessel_double([1, 2, 0], [5, bad_arc, 10]),
                                SimpleNamespace, 3, "pipe", diagnostics=diagnostics)
    assert diagnostics["nonfinite_contact_support_indices"] == [2]


@pytest.mark.parametrize("bad_force", [float("nan"), float("inf")])
def test_every_support_force_must_remain_finite(bad_force):
    with pytest.raises(ValueError, match="Finite support forces"):
        reader.support_readback(vessel_double([bad_force, 2], [5, 10]),
                                SimpleNamespace, 2, "pipe")


def test_midpoint_tolerance_keeps_ulp_perturbations_on_lower_node():
    midpoint = 0.1 + (0.2 - 0.1) / 2
    targets = [math.nextafter(midpoint, -math.inf), midpoint,
               math.nextafter(midpoint, math.inf), midpoint + 0.5e-9,
               midpoint + 2e-9]
    assert reader.material_node_indices([0.1, 0.2], targets) == [0, 0, 0, 0, 1]


@pytest.mark.parametrize("fails", [False, True])
def test_statics_record_keeps_progress_and_reraises_error(fails):
    previous = object()
    model = SimpleNamespace(staticsProgressHandler=previous)
    error = RuntimeError("Error 27: statics not converged")

    def calculate():
        assert model.staticsProgressHandler(model, "iteration 1: residual 12") is False
        assert model.staticsProgressHandler(model, "iteration 2: residual 11") is False
        if fails:
            raise error

    model.CalculateStatics = calculate
    properties = {}
    if fails:
        with pytest.raises(RuntimeError) as caught:
            reader.calculate_statics_recorded(model, properties.__setitem__)
        assert caught.value is error
    else:
        reader.calculate_statics_recorded(model, properties.__setitem__)
    record = json.loads(properties["static_convergence"])
    assert record["progress"] == ["iteration 1: residual 12", "iteration 2: residual 11"]
    assert record["status"] == ("failed" if fails else "completed")
    assert record["error"] == (str(error) if fails else None)
    assert model.staticsProgressHandler is previous


def test_tiny_positive_support_force_is_still_loaded():
    diagnostics = {}
    with pytest.raises(ValueError, match="selected support 3"):
        reader.support_readback(vessel_double([1, 2, 1e-12], [5, 10, float("nan")]),
                                SimpleNamespace, 3, "pipe", diagnostics=diagnostics)
    assert reader.last_contact([0, 5], [1, 2, 1e-12], 1) == (2, 5)


def test_recorder_failure_preserves_native_error_and_restores_none():
    model = SimpleNamespace(staticsProgressHandler=None)
    native_error = RuntimeError("Error 27")

    def calculate():
        raise native_error

    def broken_recorder(*_):
        raise OSError("record unavailable")

    model.CalculateStatics = calculate
    with pytest.raises(RuntimeError) as caught:
        reader.calculate_statics_recorded(model, broken_recorder)
    assert caught.value is native_error
    assert model.staticsProgressHandler is None
    assert "record unavailable" in " ".join(native_error.__notes__)


def test_midpoint_absolute_tolerance_boundary_and_exact_grid():
    boundary = 1e-9
    targets = [-2e-9, -boundary, 0, math.nextafter(boundary, 0),
               boundary, math.nextafter(boundary, math.inf), 2e-9]
    assert reader.material_node_indices([-1, 1], targets) == [0, 0, 0, 0, 0, 1, 1]
    assert reader.material_node_indices([0, 5, 10], [0, 2.5, 5, 7.5, 10]) == [0, 0, 1, 1, 2]


def test_zero_and_negative_supports_do_not_create_stinger_contact():
    forces, _ = reader.support_readback(
        vessel_double([0, -1, 0], [float("nan")] * 3), SimpleNamespace, 3, "pipe")
    with pytest.raises(ValueError, match="Stinger contact"):
        reader.last_contact([0, 5], forces, 1)


def test_recorder_failure_on_success_propagates_after_handler_restoration():
    model = SimpleNamespace(staticsProgressHandler=None, CalculateStatics=lambda: None)

    def broken_recorder(*_):
        assert model.staticsProgressHandler is None
        raise OSError("record unavailable")

    with pytest.raises(OSError, match="record unavailable"):
        reader.calculate_statics_recorded(model, broken_recorder)


def test_interrupt_and_recorder_failure_keep_original_interrupt():
    model = SimpleNamespace(staticsProgressHandler=None)
    interrupt = KeyboardInterrupt()

    def calculate():
        raise interrupt

    def broken_recorder(*_):
        assert model.staticsProgressHandler is None
        raise OSError("record unavailable")

    model.CalculateStatics = calculate
    with pytest.raises(KeyboardInterrupt) as caught:
        reader.calculate_statics_recorded(model, broken_recorder)
    assert caught.value is interrupt
    assert "record unavailable" in " ".join(interrupt.__notes__)
