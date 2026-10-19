"""Integration of the worked example against independent arithmetic."""

import importlib.util
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[4]
SPEC = importlib.util.spec_from_file_location(
    "surface_undulations_example",
    ROOT / "examples/domains/artificial_lift/surface_undulations_example.py",
)
EXAMPLE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(EXAMPLE)


def test_worked_field_example():
    """Inputs: issue #1855, corrected rod length and peak-position error.

    Independent rational arithmetic: T=4*4200/16300=168/163 s;
    No=60*163/168 SPM; half-cycle=30/6.4=75/16 s;
    oscillations=(75/16)/(168/163). This uses wave speed, not a rounded
    design constant. Expected card area uses the CSV's 0.5 in steps:
    first/last branch differences 780.14/1063.05 lb, interior sum
    138978.47 lb. Composite trapezoid area is
    0.5*(780.14/2+138978.47+1063.05/2)=69950.0325 in-lb.
    hp=area/12*6.4/33000 (existing card_metrics.py unit convention).
    Arithmetic is independent of the implementation under test.
    """
    matches = sorted((ROOT / "docs").rglob("dynacard_digitized.csv"))
    assert len(matches) == 1, "expected exactly one digitized card CSV"
    card_csv = matches[0]
    result = EXAMPLE.run_example(card_csv)
    ringing, card = result["ringing"], result["card"]
    assert ringing.period_s == pytest.approx(168 / 163)
    assert ringing.natural_frequency_spm == pytest.approx(60 * 163 / 168)
    assert ringing.undulations_per_half_stroke == pytest.approx((75 / 16) / (168 / 163))
    assert ringing.timing_status == "COMPATIBLE"
    assert card.card_area_in_lb == pytest.approx(69950.0325)
    assert card.polished_rod_hp == pytest.approx(69950.0325 / 12 * 6.4 / 33000)
    assert result["coverage_in"] == (0.5, 40.0)
    assert len(ringing.not_evaluated) == 3


def test_synthetic_image_to_card_example(tmp_path):
    """Synthetic rectangle: x=0..8; loads=85,25 => area=8*60=480."""
    card = EXAMPLE.digitization_example(tmp_path)
    assert card.position_in == (0, 2, 4, 6, 8)
    assert card.upstroke_load_lb == (85,) * 5
    assert card.downstroke_load_lb == (25,) * 5
    assert (tmp_path / "synthetic-card.csv").is_file()


@pytest.mark.parametrize(
    "text",
    [
        "position_in,upstroke_load_lb,downstroke_load_lb\n0,nan,1\n1,2,1\n",
        "position_in,upstroke_load_lb,downstroke_load_lb\n1,2,1\n0,2,1\n",
        "position_in,upstroke_load_lb,downstroke_load_lb\n0,1,2\n1,1,2\n",
        "position_in,upstroke_load_lb,downstroke_load_lb\n0,2,1\n",
    ],
)
def test_example_rejects_invalid_csv(tmp_path, text):
    target = tmp_path / "invalid.csv"
    target.write_text(text)
    with pytest.raises(ValueError):
        EXAMPLE.run_example(target)


@pytest.mark.parametrize(
    "overrides,status",
    [
        ({}, "COMPATIBLE"),
        ({"wave_speed_ft_s": 400}, "INCOMPATIBLE"),
        ({"peak_positions_in": ()}, "NOT_EVALUATED"),
    ],
)
def test_printed_conclusion_follows_timing_criterion(
    tmp_path, monkeypatch, capsys, overrides, status
):
    """A slower wave gives a period outside the selected timing bounds."""
    card_csv = tmp_path / "card.csv"
    card_csv.write_text(
        "position_in,upstroke_load_lb,downstroke_load_lb\n0,2,1\n1,2,1\n"
    )
    analyse = EXAMPLE.analyse_undulations

    def changed_inputs(**inputs):
        return analyse(**{**inputs, **overrides})

    monkeypatch.setattr(EXAMPLE, "analyse_undulations", changed_inputs)
    monkeypatch.setattr("sys.argv", ["example", str(card_csv)])
    EXAMPLE.main()
    output = capsys.readouterr().out
    assert f"Peak timing bounds: {status}" in output
    conclusion = "Ringing is consistent with the surface-card humps"
    assert (conclusion in output) == (status == "COMPATIBLE")
    if status != "COMPATIBLE":
        assert f"{status}: check inputs and assumptions" in output
