import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    AnalysisResults,
    CardData,
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
    SurfaceUnit,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
    PumpDiagnostics,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import (
    DynacardWorkflow,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.report_sections import (
    _build_alarm_block,
    _classification_verdict,
    build_diagnostic_report_html,
)


@pytest.mark.parametrize(
    ("classification", "severity", "screening_status"),
    [
        ("NORMAL", "normal", "pass"),
        ("PUMP_TAGGING", "critical", "fail"),
        ("VALVE_LEAK", "warning", "pass"),
    ],
)
def test_classification_verdict_maps_severity_to_screening_status(
    classification,
    severity,
    screening_status,
):
    verdict = _classification_verdict(classification)

    assert verdict["classification"] == classification
    assert verdict["severity"] == severity
    assert verdict["screening_status"] == screening_status


def test_alarm_block_treats_zero_beam_rating_as_no_structure_cap():
    ctx = DynacardAnalysisContext(
        api14="SIM-REPORT-UNIT",
        surface_card=CardData(
            position=[0.0, 144.0, 144.0, 0.0],
            load=[1000.0, 1200.0, 1100.0, 900.0],
        ),
        rod_string=[RodSection(diameter=1.0, length=5000.0)],
        pump=PumpProperties(diameter=1.75, depth=5000.0),
        surface_unit=SurfaceUnit(stroke_length=144.0, beam_rating=0.0),
        spm=10.0,
    )

    alarm_block = _build_alarm_block(ctx, "NORMAL")

    assert alarm_block["setpoints"]["structure_rating_capped"] is False
    assert alarm_block["setpoints"]["high_load_shutdown_lbs"] == pytest.approx(1440.0)


def test_legacy_valve_leak_report_renders_troubleshooting_actions():
    html = build_diagnostic_report_html(
        cfg={},
        results=AnalysisResults(diagnostic_message="Classification: VALVE_LEAK."),
        svg="",
        verdict={
            "classification": "VALVE_LEAK",
            "severity": "warning",
            "screening_status": "pass",
        },
        alarm_block={"note": "single-card report", "setpoints": {}, "alarms": []},
    )

    assert "Traveling valve leak" in html
    assert "Symptom:" in html
    assert "Mechanism:" in html
    assert "Confirm with a traveling valve test" in html


def test_router_reuses_one_diagnostic_classification(monkeypatch):
    calls = 0
    original = PumpDiagnostics.classify_with_context

    def counting_classifier(self, results):
        nonlocal calls
        calls += 1
        return original(self, results)

    monkeypatch.setattr(
        PumpDiagnostics,
        "classify_with_context",
        counting_classifier,
    )
    cfg = {
        "synthetic_card": {"mode": "PUMP_TAGGING", "seed": 711},
        "well": {
            "api14": "SIM-PUMP-TAGGING-711",
            "rod": {"diameter": 1.0, "length": 5000.0},
            "pump": {"diameter": 1.75, "depth": 5000.0},
            "surface_unit": {"stroke_length": 144.0},
            "spm": 10.0,
        },
        "report": {"html": False},
    }

    result = DynacardWorkflow().router(cfg)

    assert calls == 1
    assert result["results"]["diagnostic_message"].startswith(
        "Classification: PUMP_TAGGING."
    )
    assert result["artificial_lift"]["classification"] == "PUMP_TAGGING"
