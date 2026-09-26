from copy import deepcopy
from io import BytesIO

import pytest

from digitalmodel.workflows.installation_envelope_pdf import (
    plot_bounds, render_pdf, select_snapshot,
)


def payload():
    channel = dict(id="wave", label="Offshore wave elevation", units="m",
                   history=dict(times=[240, 300, 360], values=[-1, 1, 0]),
                   forecast=dict(times=[361, 420, 480], values=[0, 0.5, -0.5]),
                   truth=dict(times=[361, 420, 480], values=[0, 999, 0]),
                   assumed_limit=None, fit_status="fitted")
    scenario = dict(case_index=4, hs_m=2, tp_s=10, source_label="Simulated case",
                    frames=[dict(now_s=360, channels=[channel])])
    return dict(title="Installation capability screening", created_utc="2026-09-18",
                criteria=["Illustrative criteria only"],
                cases=[dict(index=4, hs_m=2, tp_s=10,
                            status="WITHIN_ASSUMPTIONS", reason="Screening", metrics={})],
                demo=dict(scenarios=[scenario]), limitations=["No operating approval"],
                provenance={"model": "synthetic test fixture"})


def test_selection_is_exact_and_returns_record_identity():
    data = payload()
    alternative = deepcopy(data["demo"]["scenarios"][0])
    alternative.update(case_index=7, hs_m=1)
    data["demo"]["scenarios"].insert(0, alternative)
    scenario, frame = select_snapshot(data)
    assert scenario["case_index"] == 4 and frame["now_s"] == 360


@pytest.mark.parametrize("change", ["missing", "duplicate", "wrong_origin"])
def test_snapshot_errors_do_not_choose_nearest_case(change):
    data = payload()
    if change == "missing":
        data["demo"]["scenarios"][0]["hs_m"] = 1
    elif change == "duplicate":
        data["demo"]["scenarios"] *= 2
    else:
        data["demo"]["scenarios"][0]["frames"][0]["now_s"] = 359
    with pytest.raises(ValueError):
        select_snapshot(data)


def test_plot_bounds_include_limit_and_exclude_hidden_truth():
    channel = payload()["demo"]["scenarios"][0]["frames"][0]["channels"][0]
    channel["assumed_limit"] = 15
    xmin, xmax, ymin, ymax = plot_bounds(channel, 360)
    assert xmin == 240 and xmax == 480
    assert ymin < -1 and 15 < ymax < 20


def test_invalid_prediction_fails_before_any_pdf_bytes():
    data = payload()
    data["demo"]["scenarios"][0]["frames"][0]["channels"][0]["forecast"]["times"][-1] = 479
    stream = BytesIO()
    with pytest.raises(ValueError):
        render_pdf(data, stream)
    assert stream.getvalue() == b""


def test_pdf_contains_three_pages_and_traceable_snapshot():
    from pypdf import PdfReader
    stream = BytesIO()
    render_pdf(payload(), stream)
    reader = PdfReader(stream)
    assert len(reader.pages) == 3
    text = "\n".join(page.extract_text() for page in reader.pages)
    for expected in ["SIMULATED", "NOW", "120 s", "case 4", "360 s",
                     "Assumed criteria", "edge-censored", "No operating approval"]:
        assert expected in text


def test_three_channels_keep_high_limit_visible_with_response_zoom():
    from pypdf import PdfReader
    data = payload()
    channels = data["demo"]["scenarios"][0]["frames"][0]["channels"]
    for name in ["Soft sling", "Lower sling"]:
        load = deepcopy(channels[0])
        load.update(id=name, label=name, units="kN", assumed_limit=800)
        channels.append(load)
    stream = BytesIO()
    render_pdf(data, stream)
    page = PdfReader(stream).pages[1].extract_text()
    assert page.count("Response zoom") == 2
    assert page.count("Full limit context") == 2
    assert "Assumed limit: 800 kN" in page


def test_history_must_end_at_now():
    data = payload()
    channel = data["demo"]["scenarios"][0]["frames"][0]["channels"][0]
    channel["history"]["times"][-1] = 359
    with pytest.raises(ValueError):
        render_pdf(data, BytesIO())


def test_report_uses_generic_study_edge_and_displays_forecast_comparator():
    from pypdf import PdfReader
    data = payload()
    channel = data["demo"]["scenarios"][0]["frames"][0]["channels"][0]
    channel["metrics"] = {"autoregression": {"rmse": 2},
                          "persistence": {"rmse": 3}, "history_mean": {"rmse": 1}}
    stream = BytesIO()
    render_pdf(data, stream)
    reader = PdfReader(stream)
    text = "\n".join(p.extract_text() for p in reader.pages)
    assert "3 m Hs study edge" not in text
    assert "RMSE 2 m" in text and "best naive 1 m" in text


def preview_payload():
    data = payload()
    data["demo"]["default_mode"] = "wave_preview"
    channel = data["demo"]["scenarios"][0]["frames"][0]["channels"][0]
    channel["wave_preview"] = deepcopy(channel["truth"])
    channel["preview_fit_status"] = "supplied simulated wave input"
    return data


def test_wave_preview_default_is_labelled_and_does_not_mutate_payload():
    from pypdf import PdfReader
    data = preview_payload()
    original = deepcopy(data)
    stream = BytesIO()
    result = render_pdf(data, stream)
    assert result["mode"] == "wave_preview"
    assert data == original
    text = PdfReader(stream).pages[1].extract_text()
    assert "SIMULATED WAVE PREVIEW INPUT" in text
    assert "conditional load forecast" in text
    assert "offshore wave prediction not validated" in text


def test_history_only_mode_can_override_preview_default():
    data = preview_payload()
    del data["demo"]["scenarios"][0]["frames"][0]["channels"][0]["wave_preview"]
    assert render_pdf(data, BytesIO(), mode="history_only")["mode"] == "history_only"
    with pytest.raises(ValueError, match="preview"):
        render_pdf(data, BytesIO())


def test_wave_preview_reports_conditional_load_rmse():
    from pypdf import PdfReader
    data = preview_payload()
    channel = data["demo"]["scenarios"][0]["frames"][0]["channels"][0]
    channel["id"] = "load"
    channel["wave_preview_metrics"] = {"oracle_wave_fir": {"rmse": 0.4},
                                       "persistence": {"rmse": 3}, "history_mean": {"rmse": 1}}
    stream = BytesIO()
    render_pdf(data, stream)
    text = PdfReader(stream).pages[2].extract_text()
    assert "RMSE 0.4 m" in text
    assert "Conditional oracle comparison" in text
