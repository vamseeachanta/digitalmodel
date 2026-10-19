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
    surface_card_from_pump_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import PumpDiagnostics
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import (
    EverittJenningsContextSolver,
    solve_downhole_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    AnalysisResults,
    CardData,
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
)
from digitalmodel.marine_ops.artificial_lift import field_health
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
def test_round_trip_preserves_input_label(mode, seed):
    """Expected answer is the supplied label, independent of classifier output.

    This tests input -> forward surface -> production inverse -> classification.
    It must not substitute classification of the generated card as the oracle.
    """
    pump = ALL_GENERATORS[mode](seed=seed)
    ctx = context_for(pump)
    ctx.surface_card = surface_card_from_pump_card(pump, ctx)
    results = EverittJenningsContextSolver(ctx).solve()
    assert PumpDiagnostics().classify_with_context(results).classification == mode


def test_calculation_card_is_unchanged_and_diagnostic_card_survives_serialization():
    """The new card must not silently change existing calculation inputs."""
    pump = ALL_GENERATORS["SAND_ABRASION"](seed=0)
    ctx = context_for(pump)
    ctx.surface_card = surface_card_from_pump_card(pump, ctx)
    results = EverittJenningsContextSolver(ctx).solve()
    assert results.downhole_card == solve_downhole_card(ctx, n_nodes=200)
    restored = AnalysisResults.model_validate_json(results.model_dump_json())
    assert restored.diagnostic_card == results.diagnostic_card
    assert restored.diagnostic_card != restored.downhole_card
    assert restored.diagnostic_card.load[-1] == restored.diagnostic_card.load[0]
    assert restored.diagnostic_card.position[-1] == restored.diagnostic_card.position[0]


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
    results = EverittJenningsContextSolver(ctx).solve()
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
    results = EverittJenningsContextSolver(ctx).solve()
    assert (
        PumpDiagnostics().classify_with_context(results).classification
        == PumpDiagnostics.classify_card(closed)
    )


def test_context_without_diagnostic_card_retains_explicit_card_behavior():
    """Older stored results and comparison solvers still use downhole_card."""
    pump = ALL_GENERATORS["ROD_PARTING"](seed=0)
    results = AnalysisResults(downhole_card=pump)
    assert PumpDiagnostics().classify_with_context(results).classification == "ROD_PARTING"


def test_explicit_diagnostic_card_is_used_even_without_calculation_card():
    """Classification uses the disclosed diagnostic input, not a placeholder."""
    pump = ALL_GENERATORS["SAND_ABRASION"](seed=0)
    results = AnalysisResults(diagnostic_card=pump)
    assert PumpDiagnostics().classify_with_context(results).classification == "SAND_ABRASION"


def test_rod_parting_is_failure_and_precedes_warning(tmp_path):
    """Independent ordering: FAILURE_CLASSIFICATIONS contains ROD_PARTING;
    STATUS_RANK assigns failure greater rank than warning (field_health.py).
    """
    settings = {
        "well_defaults": {
            "rod": {"diameter": 1.0, "length": 5000.0},
            "pump": {"diameter": 1.75, "depth": 5000.0},
            "spm": 10.0,
        },
        "wells": [
            {"api14": mode, "synthetic_card": {"mode": mode, "seed": 0}}
            for mode in ("BENT_BARREL", "ROD_PARTING")
        ],
    }
    rows = field_health.run_field_troubleshooter(settings)
    by_label = {row["api14"]: row for row in rows}
    assert by_label["ROD_PARTING"]["health_status"] == "failure"
    assert by_label["BENT_BARREL"]["health_status"] == "warning"
    summary = field_health._field_summary(
        rows, tmp_path / "wells.csv", tmp_path / "summary.json", settings,
    )
    assert summary["worst_wells"][0]["diagnostic_classification"] == "ROD_PARTING"
    assert summary["screening_status"] == "fail"


def test_report_plots_the_card_used_for_classification(tmp_path, monkeypatch):
    """The plotting boundary receives diagnostic data and discloses its basis."""
    plotted = []

    def capture(self, card, *args):
        plotted.append(card)
        return "<svg></svg>"

    monkeypatch.setattr(DiagnosticAnnotator, "render", capture)
    cfg = {
        "synthetic_card": {"mode": "SAND_ABRASION", "seed": 0},
        "well": {
            "api14": "SYNTHETIC",
            "rod": {"diameter": 1.0, "length": 5000.0},
            "pump": {"diameter": 1.75, "depth": 5000.0},
            "spm": 10.0,
        },
        "report": {"html": True},
        "Analysis": {"result_folder": str(tmp_path)},
    }
    output = DynacardWorkflow().router(cfg)
    assert plotted[0].model_dump() == output["results"]["diagnostic_card"]
    html = Path(output["outputs"]["html_report"]).read_text()
    assert "unsmoothed solver strain card" in html
    assert "field accuracy: NOT_EVALUATED" in html
