# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic survey tests; no field measurements or standards data."""

import csv
import json
import math
import os
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.cathodic_protection.cp_survey import (
    ACVGBands,
    ACVGIndication,
    CISSurveyPoint,
    DCVGIndication,
    PCMConfig,
    PCMSurveyPoint,
    analyze_pcm_survey,
    classify_acvg_indication,
)
from digitalmodel.cathodic_protection.ecda import ECDAConfig, correlate_surveys
from digitalmodel.cathodic_protection.ecda_reporting import build_chainage_chart


def pcm(distance, current):
    return PCMSurveyPoint(distance_m=distance, signal_current_mA=current)


def cis(distance, potential=-0.75, off=True):
    return CISSurveyPoint(
        distance_m=distance, on_potential_V=potential,
        off_potential_V=potential if off else None,
    )


def dcvg(distance, percentage=50):
    return DCVGIndication(
        distance_m=distance, voltage_gradient_mV=10,
        ir_drop_percentage=percentage,
    )


def acvg(distance, reading=50, unit="relative", direction=None):
    return ACVGIndication(
        distance_m=distance, reading=reading, unit=unit, direction=direction,
    )


def test_pcm_signed_losses_sorted_and_threshold_inclusive():
    result = analyze_pcm_survey(
        [pcm(20, 10), pcm(0, 30), pcm(10, 20)],
        PCMConfig(loss_unit="mA/m", loss_threshold=1),
    )
    assert [s.start_m for s in result] == [0, 10]
    assert [s.end_m for s in result] == [10, 20]
    assert [s.loss_per_m for s in result] == [1, 1]
    assert all(s.flagged for s in result)
    gain = analyze_pcm_survey([pcm(0, 10), pcm(10, 20)])[0]
    assert gain.loss_per_m < 0
    assert not gain.flagged


def test_pcm_amplitude_decibels_and_sparse_input():
    segment = analyze_pcm_survey(
        [pcm(0, 100), pcm(10, 10)],
        PCMConfig(loss_unit="dB/m", loss_threshold=2),
    )[0]
    assert segment.loss_per_m == pytest.approx(2)
    assert segment.flagged
    assert analyze_pcm_survey([]) == []
    assert analyze_pcm_survey([pcm(0, 100)]) == []
    with pytest.raises(ValueError, match="duplicate"):
        analyze_pcm_survey([pcm(0, 100), pcm(0, 80)])


@pytest.mark.parametrize("value", [0, -1, math.nan, math.inf])
def test_pcm_rejects_invalid_current(value):
    with pytest.raises(ValidationError):
        pcm(0, value)


@pytest.mark.parametrize("reading, expected", [
    (19.9, "minor"), (20, "moderate"), (40, "severe"), (60, "very_severe"),
])
def test_acvg_band_boundaries_and_direction(reading, expected):
    result = classify_acvg_indication(acvg(5, reading, direction="upstream"))
    assert result.severity == expected
    assert result.direction == "upstream"


def test_acvg_custom_bands_and_units():
    bands = ACVGBands(unit="dB_uV", moderate_min=5, severe_min=10, very_severe_min=15)
    assert classify_acvg_indication(acvg(0, 10, "dB_uV"), bands).severity == "severe"
    assert classify_acvg_indication(acvg(0, -10, "dB_uV"), bands).severity == "minor"
    with pytest.raises(ValueError, match="unit"):
        classify_acvg_indication(acvg(0), bands)


@pytest.mark.parametrize("kwargs", [
    {"moderate_min": 40}, {"severe_min": math.inf}, {"unit": "mV"},
])
def test_acvg_rejects_invalid_bands(kwargs):
    with pytest.raises(ValidationError):
        ACVGBands(**kwargs)


@pytest.mark.parametrize("kwargs", [
    {"alignment_tolerance_m": -1}, {"alignment_tolerance_m": math.nan},
    {"high_priority_min_surveys": 1}, {"dcvg_min_ir_percentage": 101},
    {"cis_overprotection_limit_V": -0.7}, {"unexpected_threshold": 5},
])
def test_ecda_config_rejects_invalid_criteria(kwargs):
    with pytest.raises(ValidationError):
        ECDAConfig(**kwargs)


def test_correlation_three_classes_ranking_and_all_evidence(tmp_path):
    result = correlate_surveys(
        cis=[cis(10), cis(100, -0.95), cis(200)],
        dcvg=[dcvg(12), dcvg(102)],
        pcm=[pcm(7, 100), pcm(17, 10)],
        acvg=[acvg(11), acvg(101), acvg(300)],
    )
    frame = result.to_dataframe()
    assert list(frame["rank"]) == [1, 2, 3, 4]
    assert list(frame["classification"]) == [
        "high priority", "further assessment", "further assessment", "conflicting signals",
    ]
    assert list(frame["chainage_m"]) == [10, 200, 300, 100]
    first = result.anomalies[0]
    assert first.rule == "corroborated_anomaly"
    assert first.positive_surveys == 4
    assert set(first.evidence) == {"cis", "dcvg", "pcm", "acvg"}
    assert first.evidence["pcm"][0]["start_m"] == 7
    assert first.evidence["dcvg"][0]["offset_m"] == 2
    assert result.criteria["alignment_tolerance_m"] == 5
    assert "user to confirm against the governing code" in result.criteria_note
    target = tmp_path / "digs.csv"
    result.to_csv(target)
    with target.open(newline="") as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 4
    assert json.loads(rows[0]["acvg_evidence"])[0]["reading"] == 50
    assert json.loads(rows[0]["criteria"])["dcvg_min_ir_percentage"] == 35
    assert frame.loc[3, "rule"] == "cis_disagrees"


def test_alignment_inclusive_no_transitive_bridging_or_reuse():
    result = correlate_surveys(
        cis=[cis(0)], dcvg=[dcvg(5)], acvg=[acvg(10)],
        config=ECDAConfig(alignment_tolerance_m=5),
    )
    assert [a.chainage_m for a in result.anomalies] == [0, 10]
    assert result.anomalies[0].positive_surveys == 2
    assert sum(len(a.evidence["acvg"]) for a in result.anomalies) == 1
    separate = correlate_surveys(cis=[cis(0)], dcvg=[dcvg(5.1)])
    assert len(separate.anomalies) == 2


def test_duplicate_survey_evidence_is_not_extra_votes_and_order_is_stable():
    points = [cis(2), cis(0), cis(1)]
    first = correlate_surveys(cis=points)
    second = correlate_surveys(cis=list(reversed(points)))
    assert first.model_dump() == second.model_dump()
    assert first.anomalies[0].positive_surveys == 1
    assert first.anomalies[0].classification == "further assessment"
    assert len(first.anomalies[0].evidence["cis"]) == 3


def test_unknown_cis_is_not_protected_or_implicitly_on_potential():
    result = correlate_surveys(cis=[cis(0, off=False)], dcvg=[dcvg(0)])
    assert result.anomalies[0].classification == "further assessment"
    assert result.anomalies[0].evidence["cis"][0]["state"] == "unknown"
    explicit_on = correlate_surveys(
        cis=[cis(0, off=False)], dcvg=[dcvg(0)], config=ECDAConfig(cis_potential="on"),
    )
    assert explicit_on.anomalies[0].classification == "high priority"


def test_criteria_overrides_change_decisions_without_mutating_inputs():
    point = acvg(0, 40)
    result = correlate_surveys(
        acvg=[point], dcvg=[dcvg(0, 40)],
        config=ECDAConfig(require_cis_for_high=False),
    )
    assert result.anomalies[0].classification == "high priority"
    assert point.severity == ""
    result = correlate_surveys(
        acvg=[point], dcvg=[dcvg(0, 40)],
        config=ECDAConfig(dcvg_min_ir_percentage=50, acvg_min_severity="very_severe"),
    )
    assert result.anomalies == []
    assert correlate_surveys(cis=[cis(0, -1.25)]).anomalies[0].rule == "isolated_anomaly"


def test_cis_mixed_states_conflict_and_threshold_equality():
    result = correlate_surveys(cis=[cis(0, -0.85), cis(1)], dcvg=[dcvg(0, 35)])
    assert result.anomalies[0].classification == "conflicting signals"
    assert correlate_surveys(cis=[cis(0, -0.85)]).anomalies == []
    assert correlate_surveys(cis=[cis(0, -1.2)]).anomalies


@pytest.mark.parametrize("point", [cis(math.nan), cis(-1), dcvg(math.inf)])
def test_correlation_rejects_invalid_existing_survey_locations(point):
    name = "cis" if isinstance(point, CISSurveyPoint) else "dcvg"
    with pytest.raises(ValueError, match="finite|non-negative"):
        correlate_surveys(**{name: [point]})


def test_empty_outputs_have_stable_schema_and_chart():
    result = correlate_surveys()
    frame = result.to_dataframe()
    assert frame.empty
    assert {"rank", "rule", "cis_evidence", "criteria"} <= set(frame.columns)
    assert len(build_chainage_chart(result).data) >= 4


def test_chart_has_four_shared_panels_classes_and_rank_annotations(tmp_path):
    result = correlate_surveys(cis=[cis(0), cis(100, -0.95)], dcvg=[dcvg(0), dcvg(100)])
    chart = build_chainage_chart(result)
    assert {t.name for t in chart.data} >= {"CIS OFF", "DCVG", "PCM", "ACVG"}
    assert chart.layout.xaxis.matches == "x4"
    assert chart.layout.xaxis2.matches == "x4"
    assert chart.layout.xaxis3.matches == "x4"
    texts = [a.text for a in chart.layout.annotations]
    assert "Dig 1: high priority" in texts
    assert "Dig 2: conflicting signals" in texts
    target = tmp_path / "chart.html"
    from digitalmodel.cathodic_protection.ecda_reporting import write_chainage_html
    write_chainage_html(result, target)
    html = target.read_text()
    assert "Plotly.newPlot" in html
    assert result.criteria_note in html
    assert "Revision history" in html
    assert "alignment_tolerance_m" in html


def test_worked_yaml_routes_through_existing_adapter(tmp_path):
    from digitalmodel.cathodic_protection.engine_adapter import (
        CALCULATION_TYPES, run_cathodic_protection,
    )
    root = Path(__file__).resolve().parents[2]
    cfg = yaml.safe_load((root / "examples/cathodic_protection/ecda_synthetic.yml").read_text())
    cfg["Analysis"] = {"result_folder": str(tmp_path)}
    assert "ECDA" in CALCULATION_TYPES
    assert run_cathodic_protection(cfg) is cfg
    results = cfg["results"]
    assert results["status"]["use_status"] == "engineering-validation-required"
    assert results["status"]["result"] == "ASSESSMENT"
    assert len(results["ecda"]["anomalies"]) == 4
    assert (tmp_path / "ecda_dig_list.csv").is_file()
    assert (tmp_path / "ecda_chainage.html").is_file()
    json.dumps(cfg, allow_nan=False)


def test_new_text_excludes_optional_denylist():
    configured = os.environ.get("ECDA_POST_DENYLIST")
    if not configured:
        pytest.skip("ECDA_POST_DENYLIST is unset")
    phrases = [p.strip().casefold() for p in Path(configured).read_text().splitlines() if p.strip()]
    root = Path(__file__).resolve().parents[2]
    from digitalmodel.cathodic_protection.ecda_reporting import chainage_html
    texts = [
        (root / "docs/domains/cathodic_protection/ecda.md").read_text(),
        (root / "examples/cathodic_protection/ecda_synthetic.yml").read_text(),
        chainage_html(correlate_surveys(cis=[cis(0)], dcvg=[dcvg(0)])),
    ]
    for index, text in enumerate(texts):
        if any(phrase in text.casefold() for phrase in phrases):
            pytest.fail(f"text {index}: denied phrase", pytrace=False)
