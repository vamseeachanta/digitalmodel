"""Independent arithmetic examples; all engineering inputs are artificial.

Paris law source: AFGROW DTD Handbook (2002), section 2.2.5, eq. 2.2.7.
Secondary stress method: TWI, Quantification of Residual Stress Effects in
SINTAP Defect Assessment (1999), multiplicative V method. No code coefficients
or published material properties are reproduced in these examples.
"""

from copy import deepcopy
import json
import math
import subprocess
import sys

import pytest

from digitalmodel.asset_integrity.assessment.fe_crack_screen import (
    assess_fe_crack,
    report_html,
)
from digitalmodel.asset_integrity.assessment.fe_crack_growth import growth_life


def case():
    """Constructed inputs, selected for hand arithmetic rather than design use."""
    return {
        "basis": "Artificial arithmetic example; not engineering data",
        "k_mat_mpa_sqrt_m": 100.0,
        "yield_mpa": 200.0,
        "lr_max": 1.5,
        "envelope": [[0.0, 1.0], [1.0, 0.5], [1.5, 0.0]],
        "envelope_basis": "Artificial piecewise-linear envelope, not API 579",
        "secondary_factor": 2.0,
        "secondary_basis": "Artificial V multiplier; not a code-derived factor",
        "states": [
            {
                "a_m": 0.001,
                "k_primary": 20.0,
                "k_secondary": 5.0,
                "k_residual": 5.0,
                "reference_mpa": 100.0,
                "delta_k": 10.0,
            },
            {
                "a_m": 0.002,
                "k_primary": 30.0,
                "k_secondary": 5.0,
                "k_residual": 5.0,
                "reference_mpa": 150.0,
                "delta_k": 10.0,
            },
        ],
        "growth": {
            "c": 1e-10,
            "m": 2.0,
            "threshold": 2.0,
            "e_ratio": 1.0,
            "threshold_rule": "unchanged",
            "basis": "Artificial Paris parameters in m/cycle and MPa sqrt(m)",
        },
    }


def test_secondary_and_residual_change_envelope_disposition():
    """Lr=0.75; f=0.625; Kr=(30+2*(5+5))/100=0.5.

    Raising residual K to 15 gives Kr=0.7 > 0.625; primary-only Kr=0.3.
    """
    data = case()
    result = assess_fe_crack(data)
    assert result["states"][1]["lr"] == 0.75
    assert result["states"][1]["kr"] == 0.5
    assert result["states"][1]["f_lr"] == 0.625
    assert result["screen"] == "INSIDE_SUPPLIED_ENVELOPE"
    assert result["passes"] is False
    assert result["code_conformity"] == "NOT_EVALUATED"
    data["states"][1]["k_residual"] = 15.0
    assert assess_fe_crack(data)["screen"] == "OUTSIDE_SUPPLIED_ENVELOPE"


def test_collapse_cutoff_even_with_zero_k():
    data = case()
    data["states"][0].update(
        reference_mpa=301, k_primary=0, k_secondary=0, k_residual=0
    )
    assert assess_fe_crack(data)["states"][0]["inside"] is False


def test_growth_constant_and_linear_closed_forms():
    """N=0.001/(1e-10*10^2)=100000 cycles for constant DeltaK.

    For DeltaK=10+10000*(a-.001), m=2:
    N=(1/10-1/20)/(1e-10*10000)=50000 cycles.
    For m=1: N=ln(20/10)/(1e-10*10000).
    """
    law = case()["growth"]
    assert growth_life([[0.001, 10], [0.002, 10]], law)["cycles"] == pytest.approx(
        100000
    )
    assert growth_life([[0.001, 10], [0.002, 20]], law)["cycles"] == pytest.approx(
        50000
    )
    law["m"] = 1
    assert growth_life([[0.001, 10], [0.002, 20]], law)["cycles"] == pytest.approx(
        math.log(2) / 1e-6
    )


def test_threshold_crossing_and_equality_arrest():
    """Linear DeltaK 4 -> 0 crosses threshold 2 at a=0.0015 m."""
    law = case()["growth"]
    result = growth_life([[0.001, 4], [0.002, 0]], law)
    assert result["status"] == "ARRESTED"
    assert result["cycles"] is None
    assert result["arrest_a_m"] == pytest.approx(0.0015)
    assert growth_life([[0.001, 2], [0.002, 4]], law)["arrest_a_m"] == 0.001


def test_e_ratio_and_threshold_rule_are_separate_inputs():
    """E ratio 0.5 scales DeltaK 4 to 2; unchanged threshold 2 arrests.

    Scaling the threshold to 1 instead gives N=.001/(1e-10*2^2)=2.5e6.
    """
    law = case()["growth"]
    law["e_ratio"] = 0.5
    assert growth_life([[0.001, 4], [0.002, 4]], law)["status"] == "ARRESTED"
    law["threshold_rule"] = "e_ratio"
    assert growth_life([[0.001, 4], [0.002, 4]], law)["cycles"] == pytest.approx(
        2500000
    )


def test_near_threshold_sensitivity():
    data = case()
    data["growth"]["threshold"] = 9
    data["delta_k_multipliers"] = [0.8, 1.2]
    result = assess_fe_crack(data)
    assert result["growth"]["minimum_threshold_ratio"] == pytest.approx(10 / 9)
    assert result["sensitivities"][0]["status"] == "ARRESTED"
    assert result["sensitivities"][1]["cycles"] == pytest.approx(100000 / 1.2**2)


def test_history_consistency_and_missing_checks():
    """Static law predicts 100000 cycles; SMART supplies 10000, ratio 0.1.

    Hand tolerance 0.05 on observed/predicted - 1 detects the discrepancy.
    """
    data = case()
    assert assess_fe_crack(data)["checks"]["history"]["status"] == "NOT_EVALUATED"
    data["history"] = [[0.001, 0], [0.002, 10000]]
    data["history_tolerance_fraction"] = 0.05
    data["history_basis"] = "Artificial history and tolerance"
    check = assess_fe_crack(data)["checks"]["history"]
    assert check["status"] == "FLAG"
    assert check["cycle_ratios"] == pytest.approx([0.1])
    data["history"][1][1] = 100000
    assert assess_fe_crack(data)["checks"]["history"]["status"] == "PASS"


def test_k_reference_consistency():
    """K=Y*sigma*sqrt(pi*a). Construct Y=1, K=100*sqrt(pi*.001).

    Implied/reference stress = 100/100 = 1, inside supplied band [0.9,1.1].
    This compares nominal opening stress only; it does not qualify reference stress.
    """
    data = case()
    data["consistency"] = {
        "geometry_factor": 1,
        "lower_ratio": 0.9,
        "upper_ratio": 1.1,
        "basis": "Artificial diagnostic band",
    }
    data["states"][0]["k_primary"] = 100 * math.sqrt(math.pi * 0.001)
    result = assess_fe_crack(data)
    assert result["checks"]["k_reference"]["ratios"][0] == pytest.approx(1)
    assert result["checks"]["k_reference"]["status"] == "FLAG"


@pytest.mark.parametrize(
    "key", ["k_mat_mpa_sqrt_m", "yield_mpa", "lr_max", "secondary_factor"]
)
@pytest.mark.parametrize("value", [float("nan"), float("inf"), True, -1, 0])
def test_invalid_scalars(key, value):
    data = case()
    data[key] = value
    with pytest.raises(ValueError):
        assess_fe_crack(data)


@pytest.mark.parametrize(
    "mutation",
    [
        lambda d: d.update(envelope=[[0.2, 1], [1.5, 0]]),
        lambda d: d.update(envelope=[[0, 1], [1, 0.5], [1, 0]]),
        lambda d: d.update(envelope=[[0, 0.5], [1.5, 1]]),
        lambda d: d["states"].reverse(),
        lambda d: d["states"][0].update(delta_k=-1),
        lambda d: d["states"][0].update(k_secondary=float("nan")),
        lambda d: d.update(basis=" "),
        lambda d: d["growth"].update(threshold_rule="guess"),
        lambda d: d["growth"].update(threshold=float("nan")),
    ],
)
def test_invalid_tables_and_bases(mutation):
    data = deepcopy(case())
    mutation(data)
    with pytest.raises(ValueError):
        assess_fe_crack(data)


def test_report_has_limits_revision_inputs_and_escaped_basis():
    data = case()
    data["basis"] = "<script>alert(1)</script>"
    result = assess_fe_crack(data)
    report = report_html(result)
    assert "<script>" not in report
    assert "&lt;script&gt;" in report
    assert (
        report.index("Summary")
        < report.index("Revision history")
        < report.index("Inputs")
    )
    assert "NOT_EVALUATED" in report
    assert "allowable service life" in report
    assert "100000.0" in report
    assert "0.001" in report  # crack-depth inputs retain engineering precision


def test_cli_readback_and_refuses_overwrite(tmp_path):
    input_path = tmp_path / "input.json"
    output_path = tmp_path / "report.html"
    input_path.write_text(json.dumps(case()))
    command = [
        sys.executable,
        "-m",
        "digitalmodel.asset_integrity.assessment.fe_crack_screen",
        str(input_path),
        str(output_path),
    ]
    completed = subprocess.run(command, capture_output=True, text=True)
    assert completed.returncode == 0, completed.stderr
    assert "Crack-like flaw analysis" in output_path.read_text()
    original = input_path.read_bytes()
    assert subprocess.run(command, capture_output=True).returncode != 0
    assert input_path.read_bytes() == original


def test_documented_example_executes():
    """The published worked example is executable and asserts its own answer."""
    from pathlib import Path

    document = Path("docs/domains/asset-integrity/fe-crack-screen.md").read_text()
    example = document.split("```python\n", 1)[1].split("```", 1)[0]
    exec(compile(example, "worked-example", "exec"), {})


def test_history_preserves_intermediate_static_knots():
    """DeltaK 10->20->10: each half gives (1/10-1/20)/(C*20000).

    Total N=50000. Ignoring the middle knot incorrectly gives 100000.
    """
    data = case()
    middle = dict(data["states"][0], a_m=0.0015, delta_k=20)
    data["states"].insert(1, middle)
    data.update(
        history=[[0.001, 0], [0.002, 50000]],
        history_tolerance_fraction=0.001,
        history_basis="Hand integration",
    )
    assert assess_fe_crack(data)["checks"]["history"]["cycle_ratios"] == pytest.approx(
        [1]
    )


def test_history_growth_below_threshold_is_flagged():
    data = case()
    data["growth"]["threshold"] = 10
    data.update(
        history=[[0.001, 0], [0.002, 10000]],
        history_tolerance_fraction=0.05,
        history_basis="Artificial",
    )
    assert assess_fe_crack(data)["checks"]["history"]["status"] == "FLAG"


@pytest.mark.parametrize(
    "history",
    [
        [[0.0005, 0], [0.002, 100000]],
        [[0.001, 0], [0.003, 100000]],
        [[0.001, 100], [0.002, 99]],
        [[0.001, 100], [0.002, 100]],
    ],
)
def test_invalid_history(history):
    data = case()
    data.update(
        history=history, history_tolerance_fraction=0.05, history_basis="Artificial"
    )
    with pytest.raises(ValueError):
        assess_fe_crack(data)


def test_growth_near_constant_and_near_unit_exponent():
    """Continuous limits: K1/K0 approaches 1; m approaches 1.

    Constant K=10 yields 100000 for m=2; linear K 10->20 yields
    ln(2)/(C*10000) for m=1. Expected limits follow the test above.
    """
    law = case()["growth"]
    assert growth_life([[0.001, 10], [0.002, 10 + 1e-12]], law)[
        "cycles"
    ] == pytest.approx(100000)
    law["m"] = 1 + 1e-12
    assert growth_life([[0.001, 10], [0.002, 20]], law)["cycles"] == pytest.approx(
        math.log(2) / 1e-6
    )


@pytest.mark.parametrize("key", ["c", "m", "e_ratio"])
@pytest.mark.parametrize("value", [0, -1, True, float("nan"), float("inf")])
def test_growth_invalid_coefficients(key, value):
    law = case()["growth"]
    law[key] = value
    with pytest.raises(ValueError):
        growth_life([[0.001, 10], [0.002, 20]], law)


def test_cli_duplicate_keys_do_not_create_output(tmp_path):
    source = tmp_path / "invalid.json"
    destination = tmp_path / "absent.html"
    source.write_text('{"basis":"one", "basis":"two"}')
    command = [
        sys.executable,
        "-m",
        "digitalmodel.asset_integrity.assessment.fe_crack_screen",
        str(source),
        str(destination),
    ]
    assert subprocess.run(command, capture_output=True).returncode != 0
    assert not destination.exists()


def test_result_keeps_an_independent_input_snapshot():
    """Editing the caller's input after assessment must not rewrite report provenance."""
    data = case()
    result = assess_fe_crack(data)
    data["states"][0]["a_m"] = 0.5
    assert result["inputs"]["states"][0]["a_m"] == 0.001


def test_orphan_history_controls_are_not_silently_ignored():
    data = case()
    data["history_tolerance_fraction"] = 0.05
    with pytest.raises(ValueError):
        assess_fe_crack(data)


def test_report_compares_unrounded_values_and_formats_diagnostics():
    data = case()
    data.update(
        history=[[0.001, 0], [0.002, 100000 / 3]],
        history_tolerance_fraction=0.05,
        history_basis="Artificial",
    )
    report = report_html(assess_fe_crack(data))
    assert "0.333333" not in report
    assert "unrounded" in report


def test_diagnostic_numerical_underflow_is_rejected():
    data = case()
    data["consistency"] = {
        "geometry_factor": 1e-300,
        "lower_ratio": 0.9,
        "upper_ratio": 1.1,
        "basis": "Numerical range probe",
    }
    data["states"][0]["reference_mpa"] = 1e-300
    with pytest.raises(ValueError):
        assess_fe_crack(data)
