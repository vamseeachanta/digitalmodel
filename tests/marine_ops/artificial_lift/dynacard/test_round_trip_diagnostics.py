"""Synthetic round-trip contracts, not field diagnostic accuracy.

Geometry is the shipped examples/workflows/artificial-lift-field-health/input.yml.
Seeds are test inputs, not engineering constants. Expected labels are the input
generator names; preservation is checked separately from physical validity.
"""

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    ALL_GENERATORS,
    SYNTHETIC_LABEL_EXCLUSIONS,
    surface_card_from_pump_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import PumpDiagnostics
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import (
    EverittJenningsContextSolver,
    solve_downhole_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.solver import (
    EverittJenningsSolver,
    RodString,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.feature_extraction import FeatureExtractor
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    AnalysisResults,
    CardData,
    DynacardAnalysisContext,
    DiagnosticResult,
    PumpProperties,
    RodSection,
)
from digitalmodel.marine_ops.artificial_lift import field_health
from digitalmodel.marine_ops.artificial_lift.dynacard.report_sections import (
    build_diagnostic_report_html,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow
from digitalmodel.marine_ops.artificial_lift.dynacard.visualization.diagnostic_annotator import (
    DiagnosticAnnotator,
)


def context_for(card):
    """Use the repository's field-health example geometry, in oilfield units."""
    return DynacardAnalysisContext(
        api14="SYNTHETIC",
        surface_card=card,
        rod_string=[RodSection(diameter=1.0, length=5000.0)],
        pump=PumpProperties(diameter=1.75, depth=5000.0),
        spm=10.0,
    )


@pytest.mark.parametrize(
    "mode,seed",
    [
        pytest.param(
            mode, seed,
            marks=pytest.mark.xfail(
                strict=True,
                reason="Closed raw DELAYED_TV_CLOSURE seed 42 is already VALVE_LEAK_TV; "
                "classifier calibration is NOT_EVALUATED, separate from solver fidelity",
            ) if (mode, seed) == ("DELAYED_TV_CLOSURE", 42) else (),
        )
        for mode in ALL_GENERATORS for seed in (0, 7, 42)
    ],
)
def test_round_trip_preserves_eligible_label_or_declared_exclusion(mode, seed):
    """Eligible labels survive; excluded defaults have explicit predictions.

    This tests input -> forward surface -> production inverse -> classification.
    Eligible labels use the supplied label as the oracle. Excluded defaults
    remain characterization controls, not qualified diagnoses.
    """
    pump = ALL_GENERATORS[mode](seed=seed)
    ctx = context_for(pump)
    ctx.surface_card = surface_card_from_pump_card(pump, ctx)
    results = EverittJenningsContextSolver(ctx, use_diagnostic_card=True).solve()
    # SYNTHETIC_LABEL_EXCLUSIONS records defaults without a distinguishing
    # signature. Pin outputs without counting them as retained supplied labels.
    excluded_predictions = {"TUBING_MOVEMENT": "NORMAL", "PLUNGER_UNDERTRAVEL": "NORMAL"}
    assert set(excluded_predictions) == set(SYNTHETIC_LABEL_EXCLUSIONS)
    expected = excluded_predictions.get(mode, mode)
    assert PumpDiagnostics().classify_with_context(results).classification == expected


def test_calculation_wrappers_agree_and_diagnostic_card_survives_serialization():
    """Wrappers agree on calculation inputs; both cards survive serialization."""
    pump = ALL_GENERATORS["SAND_ABRASION"](seed=0)
    ctx = context_for(pump)
    ctx.surface_card = surface_card_from_pump_card(pump, ctx)
    results = EverittJenningsContextSolver(ctx, use_diagnostic_card=True).solve()
    assert results.downhole_card == solve_downhole_card(ctx, n_nodes=200)
    restored = AnalysisResults.model_validate_json(results.model_dump_json())
    assert restored.diagnostic_card == results.diagnostic_card
    assert restored.diagnostic_card != restored.downhole_card
    assert restored.diagnostic_card.load[-1] == restored.diagnostic_card.load[0]
    assert restored.diagnostic_card.position[-1] == restored.diagnostic_card.position[0]


@pytest.mark.parametrize(
    "solver_options,expected_load",
    [
        ({"smooth_window": 0}, [7, 14, 6, 15, 11, 10, 10, 7]),
        ({}, np.array([1660, 2976, 2031, 2605, 2941, 2010, 2430, 1660]) / 231),
    ],
    ids=["unfiltered", "default_filter"],
)
def test_manufactured_calculation_samples(solver_options, expected_load):
    """Hand calculation independent of wrappers, generators and filter calls.

    Synthetic SI inputs set EA = rho*A = dx = dt = 1, zero damping and zero
    buoyant weight. The surface boundary gives u1 = -position + load. One
    spatial step gives u2[j] = u1[j+1] + u1[j-1] - u0[j], cyclically:
    u2 = [18, 23, 18, 22, 18, 18, 19, 18]. Calculation load at sample zero
    uses (3*u2 - 4*u1 + u0)/2 = 7; the other unique samples use u2 - u1.

    The default window clips to seven samples, with polynomial order five.
    For seven equally spaced points the vector v = [1,-6,15,-20,15,-6,1]
    is orthogonal to every polynomial through degree five and v.v = 924.
    Hence the fitted values are y - v*(v.y)/924. The left and right windows
    have v.y = -172 and 80, respectively. This gives the rational samples
    above, with the last replaced by the first AFTER filtering to close the
    loop (the unclosed final fit would be 1597/231).

    These are manufactured numerical inputs, not material or design values.
    The tolerance allows float64 arithmetic round-off only.
    """
    rods = RodString(
        diameters=np.array([2 / np.sqrt(np.pi)]),
        lengths=np.array([2.0]),
        moduli=np.array([1.0]),
        densities=np.array([1.0]),
    )
    result = EverittJenningsSolver(n_nodes=3, **solver_options).solve(
        position=np.array([0, 1, 2, 3, 3, 2, 1, 0]),
        load=np.array([10, 10, 14, 10, 10, 10, 10, 10]),
        rods=rods,
        strokes_per_minute=60 / 7,
        pump_diameter=0.1,
        tubing_id=0.2,
        fluid_density=1.0,
        damping=np.zeros(2),
    )
    # Savitzky-Golay's degree-five least-squares coefficient solve adds float64
    # round-off. Use a relative allowance with margin over the measured error
    # across SciPy versions; this is not an engineering acceptance tolerance.
    np.testing.assert_allclose(result.load, expected_load, rtol=1e-11, atol=0)
    np.testing.assert_allclose(
        result.position, [-18, -23, -18, -22, -18, -18, -19, -18],
        rtol=0, atol=1e-12,
    )
    np.testing.assert_allclose(
        result.diagnostic_load, [8, 14, 6, 15, 11, 10, 10, 8],
        rtol=0, atol=1e-12,
    )
    assert result.load[-1] == result.load[0]
    assert result.position[-1] == result.position[0]


def test_consistent_diagnostic_strain_recovers_the_supplied_pump_load():
    """F = EA (u_last - u_above) / dx at EVERY cyclic sample.

    Independent algebra: the forward boundary is u_above = u_last - F*dx/EA;
    substituting gives EA * (F*dx/EA) / dx = F, including sample zero.
    The cyclic solver uses the final sample as a duplicate of the first:
    the independently expected load is the input with that endpoint closed.
    Sources: OpenStax University Physics vol. 1 (2016), sec. 12.3, eqs.
    12.34-12.36; card_generators.py, forward-model boundary definition.
    The tolerance is a floating-point round-off allowance, not a design limit:
    sqrt(float64 epsilon) times the supplied maximum load.
    """
    pump = ALL_GENERATORS["DELAYED_TV_CLOSURE"](seed=42)
    ctx = context_for(pump)
    ctx.surface_card = surface_card_from_pump_card(pump, ctx)
    results = EverittJenningsContextSolver(ctx, use_diagnostic_card=True).solve()
    tolerance = np.sqrt(np.finfo(float).eps) * max(np.abs(pump.load))
    expected = np.array(pump.load)
    expected[-1] = expected[0]
    np.testing.assert_allclose(
        results.diagnostic_card.load, expected, rtol=0, atol=tolerance,
    )


def test_closed_input_label_survives_the_known_calibration_limit():
    """Separate path fidelity from the unresolved generator-label disagreement.

    The solver represents the endpoint as a duplicate of sample zero. Closing
    the raw input before classification provides the equivalent input path;
    no alternative diagnosis is pinned as the correct physical answer.
    """
    pump = ALL_GENERATORS["DELAYED_TV_CLOSURE"](seed=42)
    ctx = context_for(pump)
    ctx.surface_card = surface_card_from_pump_card(pump, ctx)
    closed = CardData(position=pump.position, load=pump.load[:-1] + pump.load[:1])
    results = EverittJenningsContextSolver(ctx, use_diagnostic_card=True).solve()
    assert (
        PumpDiagnostics().classify_with_context(results).classification
        == PumpDiagnostics.classify_card(closed)
    )


@pytest.mark.parametrize("card_field", ["downhole_card", "diagnostic_card"])
@pytest.mark.parametrize("falsey_card", [False, True])
def test_explicit_card_reaches_the_real_feature_extractor(
    card_field, falsey_card, monkeypatch,
):
    """Observe routing without pinning a synthetic classifier prediction."""
    if falsey_card:
        monkeypatch.setattr(CardData, "__bool__", lambda self: False, raising=False)
    pump = ALL_GENERATORS["SAND_ABRASION"](seed=0)
    results = AnalysisResults(downhole_card=ALL_GENERATORS["NORMAL"](seed=0))
    setattr(results, card_field, pump)
    observed = []
    extract = FeatureExtractor.extract_classifier_vector

    def capture(card, *args, **kwargs):
        observed.append(card)
        return extract(card, *args, **kwargs)

    monkeypatch.setattr(FeatureExtractor, "extract_classifier_vector", capture)
    PumpDiagnostics().classify_with_context(results)
    assert observed == [getattr(results, card_field)]


def test_failure_precedes_warning_through_public_router(tmp_path, monkeypatch):
    """Controlled labels check status mapping and ordering, not field accuracy.

    Both inputs use the same NORMAL generator; the classifier double supplies
    one failure label and one warning label independently of card morphology.
    """
    labels = {"SYNTHETIC_WARNING": "BENT_BARREL", "SYNTHETIC_FAILURE": "ROD_PARTING"}

    def controlled_classification(self, results):
        return DiagnosticResult(classification=labels[results.ctx.api14], confidence=1.0)

    monkeypatch.setattr(PumpDiagnostics, "classify_with_context", controlled_classification)
    settings = {
        "output_dir": tmp_path.as_posix(),
        "well_defaults": {
            "rod": {"diameter": 1.0, "length": 5000.0},
            "pump": {"diameter": 1.75, "depth": 5000.0},
            "spm": 10.0,
        },
        "wells": [
            {"api14": key, "synthetic_card": {"mode": "NORMAL", "seed": 0}}
            for key in labels
        ],
    }
    output = field_health.router({"artificial_lift_field_health": settings})
    summary = output["artificial_lift_field_health"]
    assert [row["health_status"] for row in summary["worst_wells"]] == [
        "failure", "warning",
    ]
    assert summary["screening_status"] == output["screening_status"] == "fail"
    for path in output["outputs"].values():
        assert Path(path).resolve().is_relative_to(tmp_path.resolve())
        assert Path(path).is_file()


@pytest.mark.parametrize("use_diagnostic_card", [False, True])
@pytest.mark.parametrize("falsey_card", [False, True])
def test_report_plots_the_card_used_for_classification(
    tmp_path, monkeypatch, use_diagnostic_card, falsey_card,
):
    """The plotting boundary receives the selected card and matching disclosure."""
    if falsey_card:
        monkeypatch.setattr(CardData, "__bool__", lambda self: False, raising=False)
    plotted = []

    def capture(self, card, *args):
        plotted.append(card)
        return "<svg></svg>"

    monkeypatch.setattr(DiagnosticAnnotator, "render", capture)
    pump = ALL_GENERATORS["SAND_ABRASION"](seed=0)
    ctx = context_for(pump)
    ctx.surface_card = surface_card_from_pump_card(pump, ctx)
    workflow = DynacardWorkflow(ctx)
    if use_diagnostic_card:
        # Explicit solver selection; configuration-driven runs retain defaults.
        workflow.solver = EverittJenningsContextSolver(ctx, use_diagnostic_card=True)
    output = workflow.router({
        "report": {"html": True},
        "Analysis": {"result_folder": tmp_path.as_posix()},
    })
    selected = "diagnostic_card" if use_diagnostic_card else "downhole_card"
    assert plotted[0].model_dump() == output["results"][selected]
    html = Path(output["outputs"]["html_report"]).read_text(encoding="utf-8")
    assert ("unsmoothed solver strain card" in html) is use_diagnostic_card
    if use_diagnostic_card:
        assert "field accuracy: NOT_EVALUATED" in html
    else:
        assert output["results"]["diagnostic_card"] is None


@pytest.mark.parametrize("options", [{}, {"use_diagnostic_card": False}])
def test_context_solver_defaults_to_filtered_classification(options):
    pump = ALL_GENERATORS["SAND_ABRASION"](seed=0)
    ctx = context_for(pump)
    ctx.surface_card = surface_card_from_pump_card(pump, ctx)
    results = EverittJenningsContextSolver(ctx, **options).solve()
    assert results.diagnostic_card is None
    assert (
        PumpDiagnostics().classify_with_context(results).classification
        == PumpDiagnostics.classify_card(results.downhole_card)
    )


def test_full_workflow_defaults_to_filtered_classification():
    pump = ALL_GENERATORS["SAND_ABRASION"](seed=0)
    ctx = context_for(pump)
    ctx.surface_card = surface_card_from_pump_card(pump, ctx)
    results = DynacardWorkflow(ctx).run_full_analysis()
    assert results.diagnostic_card is None
    expected = PumpDiagnostics.classify_card(results.downhole_card)
    assert results.diagnostic_message.startswith(f"Classification: {expected}.")


@pytest.mark.parametrize("invalid", [None, 0, 1, "true", np.bool_(True)])
def test_diagnostic_option_requires_a_boolean_at_construction(invalid):
    ctx = context_for(ALL_GENERATORS["NORMAL"](seed=0))
    with pytest.raises(ValueError, match="use_diagnostic_card must be a boolean"):
        EverittJenningsContextSolver(ctx, use_diagnostic_card=invalid)


@pytest.mark.parametrize("invalid", [None, 0, 1, "true", np.bool_(True)])
def test_diagnostic_option_requires_a_boolean_after_mutation(invalid):
    solver = EverittJenningsContextSolver(
        context_for(ALL_GENERATORS["NORMAL"](seed=0)),
    )
    solver.use_diagnostic_card = invalid
    with pytest.raises(ValueError, match="use_diagnostic_card must be a boolean"):
        solver.solve()


def test_disabling_diagnostic_option_clears_previous_card():
    pump = ALL_GENERATORS["SAND_ABRASION"](seed=0)
    ctx = context_for(pump)
    ctx.surface_card = surface_card_from_pump_card(pump, ctx)
    solver = EverittJenningsContextSolver(ctx, use_diagnostic_card=True)
    assert solver.solve().diagnostic_card is not None
    solver.use_diagnostic_card = False
    assert solver.solve().diagnostic_card is None


def test_filtered_seed_zero_predictions_at_the_stored_model():
    """Pin the lost labels, not only the retained count; no field claim follows."""
    lost = {}
    for mode, generate in ALL_GENERATORS.items():
        pump = generate(seed=0)
        ctx = context_for(pump)
        ctx.surface_card = surface_card_from_pump_card(pump, ctx)
        results = EverittJenningsContextSolver(ctx).solve()
        assert results.diagnostic_card is None
        label = PumpDiagnostics().classify_with_context(results).classification
        if label != mode:
            lost[mode] = label
    assert lost == {
        "TUBING_MOVEMENT": "PARAFFIN_RESTRICTION",
        "PLUNGER_UNDERTRAVEL": "NORMAL",
        "VALVE_LEAK_SV": "NORMAL",
        "DELAYED_TV_CLOSURE": "VALVE_LEAK_TV",
        "SAND_ABRASION": "BENT_BARREL",
        "EXCESSIVE_VIBRATION": "NORMAL",
    }
    assert len(ALL_GENERATORS) - len(lost) == 14


@pytest.mark.parametrize("use_diagnostic_card", [False, True])
def test_load_basis_survives_beside_optional_diagnostic_card(use_diagnostic_card):
    """Schema and rendering check only; no combined physical basis is qualified."""
    # Arbitrary schema fixtures, not qualified engineering coefficients.
    basis = {"buoyant_rod_weight_lbf": 1.0, "attenuation_factor": 1.0}
    card = CardData(position=[0, 1, 0], load=[0, 1, 0])
    diagnostic = CardData(position=[0, 1, 0], load=[2, 3, 2])
    results = AnalysisResults(
        downhole_card=card,
        downhole_load_basis=basis,
        diagnostic_card=diagnostic if use_diagnostic_card else None,
    )
    restored = AnalysisResults.model_validate_json(results.model_dump_json())
    assert restored.downhole_load_basis.buoyant_rod_weight_lbf == 1.0
    assert restored.downhole_load_basis.attenuation_factor == 1.0
    assert restored.diagnostic_card == results.diagnostic_card
    assert restored.downhole_card == card
    html = build_diagnostic_report_html(
        {}, restored, "<svg></svg>",
        {"screening_status": "pass", "classification": "NORMAL", "severity": "none"},
        {"setpoints": {}, "note": "", "alarms": []},
    )
    assert "Downhole load basis" in html
    assert restored.downhole_load_basis.method in html
    assert restored.downhole_load_basis.validation_status in html
    assert restored.downhole_load_basis.limitations in html
    assert ("unsmoothed solver strain card" in html) is use_diagnostic_card
