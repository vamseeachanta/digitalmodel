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


@pytest.mark.parametrize("survey", ["pcm", "acvg", "correlation"])
def test_supplied_surveys_require_explicit_criteria(survey):
    with pytest.raises(ValueError, match="explicit"):
        if survey == "pcm":
            analyze_pcm_survey([pcm(0, 100), pcm(10, 10)])
        elif survey == "acvg":
            classify_acvg_indication(acvg(0))
        else:
            correlate_surveys(cis=[cis(0)], dcvg=[dcvg(0)])


@pytest.mark.parametrize("survey", ["cis", "dcvg", "pcm", "acvg"])
def test_yaml_supplied_surveys_require_criteria_before_output(survey, tmp_path):
    from digitalmodel.cathodic_protection.ecda_workflow import run_ecda

    observations = {
        "cis": [cis(0).model_dump()], "dcvg": [dcvg(0).model_dump()],
        "pcm": [pcm(0, 100).model_dump(), pcm(10, 10).model_dump()],
        "acvg": [acvg(0).model_dump()],
    }
    cfg = {
        "inputs": {"ecda": {survey: observations[survey]}},
        "Analysis": {"result_folder": str(tmp_path / "output")},
    }
    with pytest.raises(ValidationError, match="criteria"):
        run_ecda(cfg)
    assert "results" not in cfg
    assert not (tmp_path / "output").exists()


@pytest.mark.parametrize("model", [PCMConfig, ACVGBands, ECDAConfig])
def test_criteria_models_have_no_implicit_thresholds(model):
    with pytest.raises(ValidationError):
        model()


def test_thresholds_require_provenance():
    with pytest.raises(ValidationError, match="provenance"):
        PCMConfig(loss_unit="mA/m", loss_threshold=1)
    with pytest.raises(ValidationError, match="provenance"):
        ACVGBands(unit="relative", moderate_min=20, severe_min=40, very_severe_min=60)


def criteria_data():
    root = Path(__file__).resolve().parents[2]
    return yaml.safe_load(
        (root / "examples/cathodic_protection/ecda_synthetic.yml").read_text(encoding="utf-8")
    )["inputs"]["ecda"]["criteria"]


def example_config(**overrides):
    data = criteria_data()
    data.update(overrides)
    return ECDAConfig.model_validate(data)


@pytest.mark.parametrize("group, field", [
    ("correlation", field) for field in (
        "alignment_tolerance_m", "cis_potential", "cis_protection_criterion_V",
        "cis_overprotection_limit_V", "dcvg_min_ir_percentage", "pcm", "acvg",
        "acvg_min_severity", "high_priority_min_surveys", "require_cis_for_high", "provenance",
    )
] + [
    ("pcm", field) for field in ("loss_unit", "loss_threshold", "provenance")
] + [
    ("acvg", field) for field in ("unit", "moderate_min", "severe_min", "very_severe_min", "provenance")
])
def test_every_criterion_is_required(group, field):
    data = criteria_data()
    target = data if group == "correlation" else data[group]
    del target[field]
    with pytest.raises(ValidationError) as caught:
        ECDAConfig.model_validate(data)
    expected = (field,) if group == "correlation" else (group, field)
    assert any(error["loc"] == expected and error["type"] == "missing" for error in caught.value.errors())


def provenance_data(kind="demonstration", confirmed=False):
    return {
        "kind": kind, "reference": "Synthetic test basis, revision 1, section 2",
        "confirmed": confirmed,
    }


def criteria_with_provenance(kind="demonstration", confirmed=False):
    data = criteria_data()
    for group in (data, data["pcm"], data["acvg"]):
        group["provenance"] = provenance_data(kind, confirmed)
    return data


@pytest.mark.parametrize("group", ["correlation", "pcm", "acvg"])
def test_engineering_criteria_require_confirmation(group):
    data = criteria_with_provenance()
    target = data if group == "correlation" else data[group]
    target["provenance"] = provenance_data("engineering", False)
    with pytest.raises(ValidationError, match="engineering criteria require confirmation"):
        ECDAConfig.model_validate(data)


@pytest.mark.parametrize("reference", ["", "   "])
def test_empty_criterion_source_is_rejected(reference):
    data = criteria_with_provenance()
    data["provenance"]["reference"] = reference
    with pytest.raises(ValidationError, match="reference must not be blank"):
        ECDAConfig.model_validate(data)


def test_engineering_criteria_cannot_hide_demonstration_thresholds():
    data = criteria_with_provenance("engineering", True)
    data["pcm"]["provenance"] = provenance_data()
    with pytest.raises(ValidationError, match="engineering correlation requires engineering"):
        ECDAConfig.model_validate(data)


def test_provenance_survives_results_csv_and_html(tmp_path):
    from digitalmodel.cathodic_protection.ecda_reporting import chainage_html

    for kind, confirmed in (("demonstration", False), ("engineering", True)):
        data = criteria_with_provenance(kind, confirmed)
        config = ECDAConfig.model_validate(data)
        result = correlate_surveys(cis=[cis(0)], dcvg=[dcvg(0)], config=config)
        assert result.criteria == data
        assert result.surveys["dcvg"][0]["state"] == "positive"
        target = tmp_path / f"{kind}.csv"
        result.to_csv(target)
        with target.open(encoding="utf-8", newline="") as stream:
            exported = json.loads(next(csv.DictReader(stream))["criteria"])
        assert exported == data
        assert kind in result.criteria_note
        html = chainage_html(result)
        assert "Synthetic test basis, revision 1, section 2" in html
        assert "confirmed" in html


def test_pcm_signed_losses_sorted_and_threshold_inclusive():
    result = analyze_pcm_survey(
        [pcm(20, 10), pcm(0, 30), pcm(10, 20)],
        PCMConfig(loss_unit="mA/m", loss_threshold=1, provenance=provenance_data()),
    )
    assert [s.start_m for s in result] == [0, 10]
    assert [s.end_m for s in result] == [10, 20]
    # (30 - 20) / 10 = 1 mA/m; (20 - 10) / 10 = 1 mA/m.
    assert [s.loss_per_m for s in result] == [1, 1]
    assert all(s.flagged for s in result)
    gain = analyze_pcm_survey([pcm(0, 10), pcm(10, 20)], example_config().pcm)[0]
    assert gain.loss_per_m < 0
    assert not gain.flagged


def test_pcm_amplitude_decibels_and_sparse_input():
    segment = analyze_pcm_survey(
        [pcm(0, 100), pcm(10, 10)],
        PCMConfig(loss_unit="dB/m", loss_threshold=2, provenance=provenance_data()),
    )[0]
    # NIST SP 811 (2008), section 8.7: 20 log10(100 / 10) / 10 = 2 dB/m.
    assert segment.loss_per_m == pytest.approx(2)
    # Midpoint: 0 + (10 - 0) / 2 = 5 m.
    assert segment.distance_m == pytest.approx(5)
    assert segment.provenance.model_dump() == provenance_data()
    assert segment.flagged
    assert analyze_pcm_survey([], example_config().pcm) == []
    assert analyze_pcm_survey([pcm(0, 100)], example_config().pcm) == []
    with pytest.raises(ValueError, match="duplicate"):
        analyze_pcm_survey([pcm(0, 100), pcm(0, 80)], example_config().pcm)


@pytest.mark.parametrize("value", [0, -1, math.nan, math.inf])
def test_pcm_rejects_invalid_current(value):
    with pytest.raises(ValidationError):
        pcm(0, value)


@pytest.mark.parametrize("reading, expected", [
    (19.9, "minor"), (20, "moderate"), (40, "severe"), (60, "very_severe"),
])
def test_acvg_band_boundaries_and_direction(reading, expected):
    result = classify_acvg_indication(acvg(5, reading, direction="upstream"), example_config().acvg)
    assert result.severity == expected
    assert result.direction == "upstream"
    assert result.provenance == example_config().acvg.provenance


def test_acvg_custom_bands_and_units():
    bands = ACVGBands(
        unit="dB_uV", moderate_min=5, severe_min=10, very_severe_min=15,
        provenance=provenance_data(),
    )
    assert classify_acvg_indication(acvg(0, 10, "dB_uV"), bands).severity == "severe"
    assert classify_acvg_indication(acvg(0, -10, "dB_uV"), bands).severity == "minor"
    with pytest.raises(ValueError, match="unit"):
        classify_acvg_indication(acvg(0), bands)


def test_acvg_reading_requires_an_explicit_unit():
    with pytest.raises(ValidationError, match="unit"):
        ACVGIndication(distance_m=0, reading=50)


@pytest.mark.parametrize("kwargs", [
    {"moderate_min": 40}, {"severe_min": math.inf}, {"unit": "mV"},
])
def test_acvg_rejects_invalid_bands(kwargs):
    data = example_config().acvg.model_dump()
    data.update(kwargs)
    with pytest.raises(ValidationError):
        ACVGBands.model_validate(data)


@pytest.mark.parametrize("kwargs", [
    {"alignment_tolerance_m": -1}, {"alignment_tolerance_m": math.nan},
    {"high_priority_min_surveys": 1}, {"dcvg_min_ir_percentage": 101},
    {"cis_overprotection_limit_V": -0.7}, {"unexpected_threshold": 5},
])
def test_ecda_config_rejects_invalid_criteria(kwargs):
    with pytest.raises(ValidationError):
        example_config(**kwargs)


def test_correlation_three_classes_ranking_and_all_evidence(tmp_path):
    result = correlate_surveys(
        cis=[cis(10), cis(100, -0.95), cis(200)],
        dcvg=[dcvg(12), dcvg(102)],
        pcm=[pcm(7, 100), pcm(17, 10)],
        acvg=[acvg(11), acvg(101), acvg(300)],
        config=example_config(),
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
    with target.open(encoding="utf-8", newline="") as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 4
    assert json.loads(rows[0]["acvg_evidence"])[0]["reading"] == 50
    assert json.loads(rows[0]["criteria"])["dcvg_min_ir_percentage"] == 35
    assert frame.loc[3, "rule"] == "cis_disagrees"


def test_alignment_inclusive_no_transitive_bridging_or_reuse():
    result = correlate_surveys(
        cis=[cis(0)], dcvg=[dcvg(5)], acvg=[acvg(10)],
        config=example_config(alignment_tolerance_m=5),
    )
    assert [a.chainage_m for a in result.anomalies] == [0, 10]
    assert result.anomalies[0].positive_surveys == 2
    assert sum(len(a.evidence["acvg"]) for a in result.anomalies) == 1
    separate = correlate_surveys(cis=[cis(0)], dcvg=[dcvg(5.1)], config=example_config())
    assert len(separate.anomalies) == 2


def test_duplicate_survey_evidence_is_not_extra_votes_and_order_is_stable():
    points = [cis(2), cis(0), cis(1)]
    first = correlate_surveys(cis=points, config=example_config())
    second = correlate_surveys(cis=list(reversed(points)), config=example_config())
    assert first.model_dump() == second.model_dump()
    assert first.anomalies[0].positive_surveys == 1
    assert first.anomalies[0].classification == "further assessment"
    assert len(first.anomalies[0].evidence["cis"]) == 3


def test_unknown_cis_is_not_protected_or_implicitly_on_potential():
    result = correlate_surveys(cis=[cis(0, off=False)], dcvg=[dcvg(0)], config=example_config())
    assert result.anomalies[0].classification == "further assessment"
    assert result.anomalies[0].evidence["cis"][0]["state"] == "unknown"
    explicit_on = correlate_surveys(
        cis=[cis(0, off=False)], dcvg=[dcvg(0)], config=example_config(cis_potential="on"),
    )
    assert explicit_on.anomalies[0].classification == "high priority"


def test_criteria_overrides_change_decisions_without_mutating_inputs():
    point = acvg(0, 40)
    result = correlate_surveys(
        acvg=[point], dcvg=[dcvg(0, 40)],
        config=example_config(require_cis_for_high=False),
    )
    assert result.anomalies[0].classification == "high priority"
    assert point.severity == ""
    result = correlate_surveys(
        acvg=[point], dcvg=[dcvg(0, 40)],
        config=example_config(dcvg_min_ir_percentage=50, acvg_min_severity="very_severe"),
    )
    assert result.anomalies == []
    assert correlate_surveys(
        cis=[cis(0, -1.25)], config=example_config(),
    ).anomalies[0].rule == "isolated_anomaly"


def test_cis_mixed_states_conflict_and_threshold_equality():
    result = correlate_surveys(
        cis=[cis(0, -0.85), cis(1)], dcvg=[dcvg(0, 35)], config=example_config(),
    )
    assert result.anomalies[0].classification == "conflicting signals"
    assert correlate_surveys(cis=[cis(0, -0.85)], config=example_config()).anomalies == []
    assert correlate_surveys(cis=[cis(0, -1.2)], config=example_config()).anomalies


@pytest.mark.parametrize("point", [cis(math.nan), cis(-1), dcvg(math.inf)])
def test_correlation_rejects_invalid_existing_survey_locations(point):
    name = "cis" if isinstance(point, CISSurveyPoint) else "dcvg"
    with pytest.raises(ValueError, match="finite|non-negative"):
        correlate_surveys(**{name: [point]}, config=example_config())


def test_empty_outputs_have_stable_schema_and_chart():
    result = correlate_surveys(config=example_config())
    frame = result.to_dataframe()
    assert frame.empty
    assert {"rank", "rule", "cis_evidence", "criteria"} <= set(frame.columns)
    assert len(build_chainage_chart(result).data) >= 4


def test_chart_has_four_shared_panels_classes_and_rank_annotations(tmp_path):
    result = correlate_surveys(
        cis=[cis(0), cis(100, -0.95)], dcvg=[dcvg(0), dcvg(100)], config=example_config(),
    )
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
    html = target.read_text(encoding="utf-8")
    assert "Plotly.newPlot" in html
    assert result.criteria_note in html
    assert "Revision history" in html
    assert "alignment_tolerance_m" in html


def test_worked_yaml_routes_through_existing_adapter(tmp_path):
    from digitalmodel.cathodic_protection.engine_adapter import (
        CALCULATION_TYPES, run_cathodic_protection,
    )
    root = Path(__file__).resolve().parents[2]
    cfg = yaml.safe_load(
        (root / "examples/cathodic_protection/ecda_synthetic.yml").read_text(encoding="utf-8")
    )
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
    phrases = [p.strip().casefold() for p in Path(configured).read_text(encoding="utf-8").splitlines() if p.strip()]
    root = Path(__file__).resolve().parents[2]
    from digitalmodel.cathodic_protection.ecda_reporting import chainage_html
    texts = [
        (root / "docs/domains/cathodic_protection/ecda.md").read_text(encoding="utf-8"),
        (root / "examples/cathodic_protection/ecda_synthetic.yml").read_text(encoding="utf-8"),
        chainage_html(correlate_surveys(cis=[cis(0)], dcvg=[dcvg(0)], config=example_config())),
    ]
    for index, text in enumerate(texts):
        if any(phrase in text.casefold() for phrase in phrases):
            pytest.fail(f"text {index}: denied phrase", pytrace=False)
