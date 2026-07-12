import json
import math
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine
from tests.workflows.field_dev_production_assertions import (
    FIELD_DEV_PRODUCTION_WORKFLOWS,
    assert_field_dev_production_workflow,
)

REPO_ROOT = Path(__file__).resolve().parents[2]
REGISTRY_PATH = REPO_ROOT / "docs" / "registry" / "workflows.yaml"


def _load_registry():
    with REGISTRY_PATH.open() as stream:
        registry = yaml.safe_load(stream)
    return registry["workflows"]


@pytest.mark.parametrize(
    "workflow",
    _load_registry(),
    ids=lambda workflow: workflow["id"],
)
def test_workflow_registry(workflow, monkeypatch):
    if workflow.get("runtime", "offline") != "offline":
        pytest.skip(f"{workflow['id']} requires runtime={workflow['runtime']}")

    input_path = REPO_ROOT / workflow["input"]
    if workflow["id"] == "cathodic-protection-pipeline":
        monkeypatch.delenv("LLM_WIKI_PATH", raising=False)
        monkeypatch.delenv("DIGITALMODEL_REPO_ROOT", raising=False)

    cfg = engine(inputfile=str(input_path))

    assert isinstance(cfg, dict)
    assert cfg["basename"] == workflow["basename"]

    for output in workflow["outputs"]:
        assert (REPO_ROOT / output).exists()

    if workflow["id"] == "cathodic-protection":
        cp = cfg["cathodic_protection"]
        assert cp["current_demand_A"]["totals"]["mean"] == pytest.approx(196.667146)
        assert cp["anode_requirements"]["total_mass_kg"] == pytest.approx(5067.071173)
        assert cp["anode_requirements"]["anode_count"] > 180
    elif workflow["id"] == "cathodic-protection-jacket":
        results = cfg["results"]
        assert results["standard"] == "DNV-RP-B401-2021"
        assert results["current_demand_A"]["total_mean_A"] == pytest.approx(101.85)
        assert results["current_demand_A"]["total_final_A"] == pytest.approx(168.6)
        assert results["anode_requirements"]["total_mass_kg"] == pytest.approx(13120.68)
        assert results["anode_requirements"]["anode_count"] == 66
        verification = results["current_output_verification"]
        assert verification["driving_voltage_V"] == pytest.approx(0.25)
        assert verification["recommended_anode_count"] == 109
    elif workflow["id"] == "cathodic-protection-pipeline":
        results = cfg["results"]
        densities = results["current_densities_mA_m2"]
        coating = results["coating_breakdown_factors"]
        demand = results["current_demand_A"]
        anodes = results["anode_requirements"]
        spacing = results["anode_spacing_m"]
        attenuation = results["attenuation_analysis"]

        assert densities["mean_current_density_A_m2"] == pytest.approx(0.06)
        assert densities["temperature_band"] == ">50-80"
        assert coating["mean_factor"] == pytest.approx(0.0145)
        assert demand["mean_current_demand_A"] == pytest.approx(1.328)
        assert demand["final_current_demand_A"] == pytest.approx(1.74)
        assert anodes["total_anode_mass_kg"] == pytest.approx(218.111)
        assert anodes["actual_total_mass_kg"] == pytest.approx(250.0)
        assert anodes["anode_count"] == 10
        assert spacing["spacing_m"] == pytest.approx(166.667)
        assert spacing["spacing_valid"] is True
        assert attenuation["protection_reach_m"] == pytest.approx(96.559)
        assert attenuation["protection_adequate"] is True

        expected_mass = demand["total_charge_Ah"] / (
            anodes["anode_capacity_Ah_kg"] * anodes["utilization_factor"]
        )
        assert anodes["total_anode_mass_kg"] == pytest.approx(expected_mass, abs=1.0e-3)
        assert anodes["anode_count"] == math.ceil(
            anodes["total_anode_mass_kg"]
            * anodes["contingency_factor"]
            / anodes["individual_anode_mass_kg"]
        )
        citation = results["citations"][0]
        assert citation["code_id"] == "dnv-rp-f103"
        assert citation["publisher"] == "DNV"
        assert citation["revision"] == "2010"

        from digitalmodel.citations.resolver import resolve_wiki_path

        assert resolve_wiki_path(citation["wiki_path"]) == (
            REPO_ROOT / "knowledge" / citation["wiki_path"]
        )
    elif workflow["id"] == "cathodic-protection-manifold":
        results = cfg["results"]
        assert results["standard"] == "DNV-RP-B401-2021"
        assert results["current_demand_A"]["total_mean_A"] == pytest.approx(29.75)
        assert results["anode_requirements"]["total_mass_kg"] == pytest.approx(3832.5)
        assert results["anode_requirements"]["anode_count"] == 48
        assert results["current_output_verification"]["adequate"] is True
    elif workflow["id"] == "cathodic-protection-monopile":
        results = cfg["results"]
        assert results["standard"] == "DNV-RP-B401-2021"
        assert results["current_demand_A"]["total_mean_A"] == pytest.approx(27.72)
        assert results["anode_requirements"]["total_mass_kg"] == pytest.approx(4285.19)
        assert results["anode_requirements"]["anode_count"] == 29
        assert results["current_output_verification"]["recommended_anode_count"] == 32
    elif workflow["id"] == "cathodic-protection-fpso":
        results = cfg["results"]
        assert results["current_demand_A"]["mean"] == pytest.approx(96.0)
        assert results["current_demand_A"]["final"] == pytest.approx(248.0)
        assert results["anode_current_capacity_Ah_kg"] == pytest.approx(1865.0)
        assert results["anode_mass_kg"] == pytest.approx(14091.153)
    elif workflow["id"] == "catenary":
        assert cfg["S"] == pytest.approx(173.2050808)
        assert cfg["X"] == pytest.approx(131.6957897)
        assert cfg["BendRadius"] == pytest.approx(100.0)
    elif workflow["id"] == "pipe-capacity":
        result = cfg["pipe_capacity"]["Outer_Pipe"]["internal_pressure"][
            "API STD 2RD-2013 Section 5"
        ]
        assert result["Design_WT_Max_Pressure"]["Zero Corrosion Allowance"] == (
            pytest.approx(3149.178164)
        )
        assert result["minimum_thickness"]["With Corrosion Allowance"] == (
            pytest.approx(0.358704)
        )
        assert result["thickness"]["With Corrosion Allowance"] == pytest.approx(0.375)
    elif workflow["id"] == "fatigue-analysis":
        damage = cfg["fatigue_analysis"]["damage"]
        assert damage == pytest.approx(7.418780212e-11)
        assert 0 < damage < 1.0e-9
    elif workflow["id"] == "jumper-installation":
        summary = cfg["jumper_installation"]["summary"]
        results_path = Path(summary["cases_csv"])
        summary_path = Path(summary["summary_json"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        if not summary_path.is_absolute():
            summary_path = REPO_ROOT / summary_path

        cases = pd.read_csv(results_path)
        summary_json = yaml.safe_load(summary_path.read_text())

        assert summary["screening_status"] in {"pass", "fail"}
        assert summary["screening_status"] == "pass"
        assert summary["governing_phase"] == "splash_zone"
        assert summary["max_utilisation"] == pytest.approx(
            cases["max_utilisation"].max()
        )
        assert len(cases) == 3
        assert cases["hs_m"].tolist() == [1.0, 2.0, 3.0]
        assert cases["max_utilisation"].is_monotonic_increasing
        assert set(cases["governing_phase"]) == {"splash_zone"}
        assert summary_json["summary"]["screening_status"] == "pass"
        assert summary_json["summary"]["governing_phase"] == "splash_zone"
    elif workflow["id"] == "mooring-fatigue":
        summary = cfg["mooring_fatigue"]
        results_path = Path(summary["results_csv"])
        summary_path = Path(summary["summary_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        if not summary_path.is_absolute():
            summary_path = REPO_ROOT / summary_path

        results = pd.read_csv(results_path)
        line_summary = pd.read_csv(summary_path)

        assert summary["governing_line"] == "wire-01"
        assert summary["governing_damage"] > 0.0
        assert summary["governing_fatigue_life_years"] == pytest.approx(
            summary["design_life_years"] / summary["governing_damage"]
        )
        assert summary["governing_dff_margin"] == pytest.approx(
            summary["governing_fatigue_life_years"]
            / (summary["design_life_years"] * summary["dff"])
        )
        assert len(results) == 6
        assert set(line_summary["line_id"]) == {"chain-01", "wire-01"}
        assert (results["stress_range_MPa"] > 0.0).all()
        assert (results["damage"] > 0.0).all()
        assert results.sort_values("stress_range_MPa")[
            "allowable_cycles"
        ].is_monotonic_decreasing
    elif workflow["id"] == "riser-fatigue":
        summary = cfg["riser_fatigue"]
        results_path = Path(summary["results_csv"])
        summary_path = Path(summary["summary_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        if not summary_path.is_absolute():
            summary_path = REPO_ROOT / summary_path

        results = pd.read_csv(results_path)
        seg_summary = pd.read_csv(summary_path)

        assert summary["governing_segment"] == "TDZ-sandwave"
        assert 0.0 < summary["governing_damage"] < 1.0
        assert summary["governing_wave_damage"] > 0.0
        assert summary["governing_viv_damage"] > 0.0
        assert summary["governing_damage"] == pytest.approx(
            summary["governing_wave_damage"] + summary["governing_viv_damage"]
        )
        assert summary["governing_fatigue_life_years"] == pytest.approx(
            summary["design_life_years"] / summary["governing_damage"]
        )
        assert summary["screening_status"] == (
            "pass" if summary["governing_dff_margin"] >= 1.0 else "fail"
        )
        assert set(seg_summary["segment_id"]) == {"TDZ-sandwave", "hangoff"}
        assert set(results["contribution"]) == {"wave", "viv"}
        assert (results["stress_range_MPa"] > 0.0).all()
    elif workflow["id"] in {
        "mooring-fatigue-atlas-query",
        "synthetic-rope-atlas-query",
        "spectral-fatigue-atlas-query",
    }:
        result = cfg["parametric_query"]["result"]
        # in-range query returns an interpolated screening estimate, not a verdict
        assert result["in_range"] is True
        assert result["response"] == "fatigue_life_years"
        assert result["value"] > 0.0
        assert result["screening_status"] in {"pass", "fail"}
        lo, hi = result["confidence"]["band"]
        assert lo <= result["value"] <= hi
        assert "screening estimate" in result["disclaimer"].lower()
        assert result["provenance"]["atlas_id"]
    elif workflow["id"] == "code-check-atlas-query":
        result = cfg["parametric_query"]["result"]
        # boundary class: interpolated utilisation + derived verdict
        assert result["in_range"] is True
        assert result["response"] == "utilisation"
        assert result["value"] > 0.0
        assert result["screening_status"] in {"pass", "fail"}
        lo, hi = result["confidence"]["band"]
        # in-range answers must NOT straddle the code limit (else they escalate)
        assert not (lo < 1.0 < hi)
    elif workflow["id"] == "rao-atlas-query":
        result = cfg["parametric_query"]["result"]
        # linear class: direct interpolated response
        assert result["in_range"] is True
        assert result["response"] == "heave_m"
        assert result["value"] > 0.0
        lo, hi = result["confidence"]["band"]
        assert lo <= result["value"] <= hi
    elif workflow["id"] == "fpso-mooring-atlas-query":
        result = cfg["parametric_query"]["result"]
        # value class: interpolated max line tension
        assert result["in_range"] is True
        assert result["response"] == "max_line_tension_N"
        assert result["value"] > 0.0
        lo, hi = result["confidence"]["band"]
        assert lo <= result["value"] <= hi
    elif workflow["id"] == "viv-atlas-query":
        result = cfg["parametric_query"]["result"]
        # value class: interpolated in-line VIV safety factor
        assert result["in_range"] is True
        assert result["response"] == "safety_factor_inline"
        assert result["value"] > 0.0
        lo, hi = result["confidence"]["band"]
        assert lo <= result["value"] <= hi
    elif workflow["id"] == "diffraction-library-query":
        result = cfg["parametric_query"]["result"]
        # licensed-solver sparse library: covered case interpolates within
        assert result["in_range"] is True
        assert result["response"] == "heave_rao_m_per_m"
        assert result["value"] > 0.0
        lo, hi = result["confidence"]["band"]
        assert lo <= result["value"] <= hi
    elif workflow["id"] == "orcaflex-library-query":
        result = cfg["parametric_query"]["result"]
        # licensed-solver sparse library: covered load case interpolates heading
        assert result["in_range"] is True
        assert result["response"] == "max_effective_tension_N"
        assert result["value"] > 0.0
        lo, hi = result["confidence"]["band"]
        assert lo <= result["value"] <= hi
    elif workflow["id"] == "drilling-riser-envelope-library-query":
        result = cfg["parametric_query"]["result"]
        # licensed-solver sparse library: covered operating mode interpolates
        # offset x current x Hs x Tp; the DAF is a dimensionless multiplier >= 1.
        assert result["in_range"] is True
        assert result["response"] == "von_mises_daf"
        assert result["value"] >= 1.0
        lo, hi = result["confidence"]["band"]
        assert lo <= result["value"] <= hi
        # the STUB must self-identify at the query surface (#1346 governance fix)
        assert result["provenance"]["solver"]["licensed"] is False
        assert result["provenance"]["solver"]["version"] == "STUB"
    elif workflow["id"] == "drilling-riser-operability-query":
        result = cfg["parametric_query"]["result"]
        # exact operability node-cache (#1283): governing utilisation thresholded at
        # 1.0; the example point is operable (a clean pass, not a boundary escalation).
        assert result["in_range"] is True
        assert result["response"] == "utilisation"
        assert result["value"] > 0.0
        assert result["screening_status"] in {"pass", "fail"}
        lo, hi = result["confidence"]["band"]
        assert lo <= result["value"] <= hi
        assert result["provenance"]["atlas_id"]
    elif workflow["id"] == "free-span-atlas-query":
        result = cfg["parametric_query"]["result"]
        # boundary class: utilisation predicted directly
        assert result["in_range"] is True
        assert result["response"] == "utilisation"
        assert result["value"] > 0.0
        assert result["screening_status"] in {"pass", "fail"}
        lo, hi = result["confidence"]["band"]
        assert not (lo < 1.0 < hi)
    elif workflow["id"] in {"pile-capacity-atlas-query", "anchor-capacity-atlas-query"}:
        result = cfg["parametric_query"]["result"]
        # capacity-demand class: atlas predicts capacity, load case applied here
        assert result["in_range"] is True
        assert result["response"] == "utilisation"
        assert result["capacity_kN"] > 0.0
        assert result["screening_status"] in {"pass", "fail"}
        lo, hi = result["confidence"]["band"]
        assert not (lo < 1.0 < hi)
    elif workflow["id"] == "synthetic-rope-mooring-fatigue":
        summary = cfg["synthetic_rope_mooring_fatigue"]
        results_path = Path(summary["results_csv"])
        summary_path = Path(summary["summary_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        if not summary_path.is_absolute():
            summary_path = REPO_ROOT / summary_path
        results = pd.read_csv(results_path)
        line_summary = pd.read_csv(summary_path)

        assert cfg["screening_status"] in {"pass", "fail"}
        assert cfg["screening_status"] == summary["screening_status"]
        assert summary["screening_status"] == "fail"
        assert summary["governing_mechanism"] == "creep"
        assert summary["governing_line"] == "polyester-02"
        assert set(line_summary["line_id"]) == {"polyester-01", "polyester-02"}
        assert (results["damage"] > 0.0).all()
        assert (results["normalised_range"] > 0.0).all()
        assert (results["allowable_cycles"] > 0.0).all()
        by_line = {row["line_id"]: row for _, row in line_summary.iterrows()}
        assert bool(by_line["polyester-01"]["passes"]) is True
        assert bool(by_line["polyester-02"]["passes"]) is False
        assert by_line["polyester-02"]["governing_mechanism"] == "creep"
        # steeper range -> fewer allowable cycles (monotonic T-N). Checked
        # per-line: the mean-load knockdown makes a_eff line-specific, so the
        # T-N curve is only monotonic within a single line, not across lines.
        for _, group in results.groupby("line_id"):
            assert group.sort_values("normalised_range")[
                "allowable_cycles"
            ].is_monotonic_decreasing
    elif workflow["id"] == "spectral-fatigue":
        summary = cfg["spectral_fatigue"]
        results_path = Path(summary["results_csv"])
        summary_path = Path(summary["summary_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        if not summary_path.is_absolute():
            summary_path = REPO_ROOT / summary_path
        results = pd.read_csv(results_path)
        location_summary = pd.read_csv(summary_path)

        assert cfg["screening_status"] == summary["screening_status"]
        assert summary["screening_status"] == "fail"  # splash zone fails
        assert summary["method"] == "dirlik"
        assert summary["governing_location"] == "riser-splash-zone"
        assert set(location_summary["location_id"]) == {
            "riser-splash-zone",
            "riser-keel-joint",
        }
        by_loc = {row["location_id"]: row for _, row in location_summary.iterrows()}
        assert bool(by_loc["riser-splash-zone"]["passes"]) is False
        assert bool(by_loc["riser-keel-joint"]["passes"]) is True
        # life = 1 / accumulated annual damage; governing = smallest margin
        for _, row in location_summary.iterrows():
            assert row["fatigue_life_years"] == pytest.approx(
                1.0 / row["annual_damage"]
            )
        assert summary["governing_margin"] == pytest.approx(
            location_summary["margin"].min()
        )
        # multi-sea-state weighting: splash zone has 2 states, keel 1; each
        # weighted contribution = full damage * occurrence fraction.
        assert (results["damage_per_year_full"] > 0.0).all()
        assert len(results[results["location_id"] == "riser-splash-zone"]) == 2
        for _, row in results.iterrows():
            assert row["damage_per_year_weighted"] == pytest.approx(
                row["damage_per_year_full"] * row["occurrence_fraction"]
            )
    elif workflow["id"] == "rao-spectral-fatigue":
        summary = cfg["rao_spectral_fatigue"]
        # the RAO -> stress-PSD transfer is recorded on the result
        assert (
            summary["wave_to_stress_transfer"] == "S_stress(f) = |H(f)|**2 * S_wave(f)"
        )
        summary_path = Path(summary["summary_csv"])
        if not summary_path.is_absolute():
            summary_path = REPO_ROOT / summary_path
        location_summary = pd.read_csv(summary_path)

        assert cfg["screening_status"] == summary["screening_status"]
        assert summary["screening_status"] == "fail"  # splash zone fails
        assert summary["method"] == "dirlik"
        assert summary["governing_location"] == "riser-splash-zone"
        assert set(location_summary["location_id"]) == {
            "riser-splash-zone",
            "riser-keel-joint",
        }
        by_loc = {row["location_id"]: row for _, row in location_summary.iterrows()}
        assert bool(by_loc["riser-splash-zone"]["passes"]) is False
        assert bool(by_loc["riser-keel-joint"]["passes"]) is True
        for _, row in location_summary.iterrows():
            assert row["fatigue_life_years"] == pytest.approx(
                1.0 / row["annual_damage"]
            )
        assert summary["governing_margin"] == pytest.approx(
            location_summary["margin"].min()
        )
    elif workflow["id"] == "vessel-seakeeping":
        summary = cfg["vessel_seakeeping"]
        assert summary["response_transfer"] == "S_response(w) = |RAO(w)|^2 * S_wave(w)"
        summary_path = Path(summary["summary_csv"])
        if not summary_path.is_absolute():
            summary_path = REPO_ROOT / summary_path
        dof_summary = pd.read_csv(summary_path)

        assert cfg["screening_status"] == summary["screening_status"]
        assert summary["screening_status"] == "fail"  # roll resonance fails operability
        assert summary["governing_dof"] == "roll"
        assert set(dof_summary["dof"]) == {"heave", "roll", "pitch"}
        by_dof = {row["dof"]: row for _, row in dof_summary.iterrows()}
        assert bool(by_dof["heave"]["passes"]) is True
        assert bool(by_dof["pitch"]["passes"]) is True
        assert bool(by_dof["roll"]["passes"]) is False
        # governing DOF carries the lowest operability percentage
        assert summary["governing_operability_pct"] == pytest.approx(
            dof_summary["operability_pct"].min()
        )
        # operability percentage is bounded [0, 100]
        for _, row in dof_summary.iterrows():
            assert 0.0 <= row["operability_pct"] <= 100.0
    elif workflow["id"] == "time-series":
        fft_path = Path(cfg["time_series"]["csv"]["signal_fft"])
        fft = pd.read_csv(fft_path)
        dominant = fft.sort_values("power", ascending=False).iloc[0]
        assert dominant["fft_freq"] == pytest.approx(0.20)
        assert dominant["power"] > 250000.0
    elif workflow["id"] == "viv-analysis":
        result_dir = Path(cfg["Analysis"]["result_folder"])
        natural = pd.read_csv(result_dir / "input_natural_frequencies.csv")
        shedding = pd.read_csv(result_dir / "input_vs_frequencies.csv")
        safety = pd.read_csv(result_dir / "input_safety_factors.csv")

        assert natural.loc[0, "mode 1"] == pytest.approx(2.623697)
        assert shedding.loc[0, "shredding_frequency_inline"] == pytest.approx(0.564706)
        assert safety.loc[0, "safety_factor_inline"] == pytest.approx(4.646130)
    elif workflow["id"] == "viv-parametric":
        cases = cfg["parametric_run"]["cases"]
        manifest = pd.read_csv(
            REPO_ROOT / "examples/workflows/viv-parametric/results/cases.csv"
        )
        case_0 = yaml.safe_load(
            (
                REPO_ROOT / "examples/workflows/viv-parametric/results/case_0.yml"
            ).read_text()
        )

        assert len(cases) == 6
        assert manifest["status"].tolist() == ["completed"] * 6
        assert case_0["basename"] == "viv_analysis"
        assert case_0["pipeline"]["span_length"][0] == pytest.approx(40.0)
        assert case_0["pipeline"]["crossection"][0]["Design_WT"] == pytest.approx(0.5)
    elif workflow["id"] == "stress-strain":
        summary = cfg["stress_strain"]
        curve = pd.read_csv(REPO_ROOT / summary["curve_csv"])

        assert summary["points"] == 21
        assert summary["elastic_modulus"] == pytest.approx(207000.0)
        assert summary["yield_strength"] == pytest.approx(448.0)
        assert len(curve) == 21
        assert curve["stress"].iloc[0] == pytest.approx(0.0, abs=1.0e-6)
        assert curve["stress"].iloc[-1] > curve["stress"].iloc[1]
        assert curve["stress"].is_monotonic_increasing
    elif workflow["id"] == "compare-tool":
        summary = cfg["compare_tool"]
        comparison_path = Path(summary["comparison_csv"])
        if not comparison_path.is_absolute():
            comparison_path = REPO_ROOT / comparison_path
        comparison = pd.read_csv(comparison_path)
        row_x2 = comparison.loc[comparison["x"] == 2].iloc[0]

        assert summary["n_sources"] == 2
        assert summary["n_rows"] == 6
        assert len(comparison) == 6
        assert row_x2["run_b_minus_run_a"] == pytest.approx(3.0)
        assert row_x2["run_b_ratio"] == pytest.approx(1.1)
    elif workflow["id"] in {
        "api-rp-2rd-riser",
        "dnv-os-f201-riser",
        "api-2sk-mooring",
    }:
        expected_codes = {
            "api-rp-2rd-riser": "API_RP_2RD",
            "dnv-os-f201-riser": "DNV_OS_F201",
            "api-2sk-mooring": "API_RP_2SK",
        }
        summary = cfg["code_check"]
        results_path = Path(summary["results_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        results = pd.read_csv(results_path)

        assert summary["code"] == expected_codes[workflow["id"]]
        assert summary["governing_utilisation"] > 0.0
        assert math.isfinite(summary["governing_utilisation"])
        assert summary["governing_utilisation"] == pytest.approx(
            results["utilisation"].max()
        )
        assert (results["code"] == expected_codes[workflow["id"]]).all()
        assert (results["utilisation"] > 0.0).all()
        assert results["utilisation"].map(math.isfinite).all()
    elif workflow["id"] == "pipe-ovality":
        summary = cfg["pipe_ovality"]
        results_path = Path(summary["results_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        results = pd.read_csv(results_path)
        expected_out_of_roundness = (325.0 - 322.7) / 323.85

        assert summary["out_of_roundness"] == pytest.approx(expected_out_of_roundness)
        assert summary["ovality_percent"] == pytest.approx(
            expected_out_of_roundness * 100.0
        )
        assert summary["passes"] is True
        assert results.loc[0, "out_of_roundness"] == pytest.approx(
            expected_out_of_roundness
        )
        assert bool(results.loc[0, "passes"]) is True
    elif workflow["id"] == "wave-spectrum":
        summary = cfg["wave_spectrum"]
        results_path = Path(summary["spectrum_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        spectrum = pd.read_csv(results_path)
        omega_p = 2.0 * math.pi / summary["Tp"]

        assert summary["spectrum"] == "jonswap"
        assert summary["Hs_input"] == pytest.approx(3.0)
        assert summary["Hs_check"] == pytest.approx(3.0, rel=0.05)
        assert summary["peak_frequency"] == pytest.approx(omega_p, abs=0.02)
        assert len(spectrum) == 181
        assert (spectrum["S"] >= 0.0).all()
    elif workflow["id"] == "response-spectrum":
        summary = cfg["response_spectrum"]["summary"]
        results_path = Path(cfg["response_spectrum"]["timeseries_csv"])
        report_path = Path(cfg["response_spectrum"]["report_html"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        if not report_path.is_absolute():
            report_path = REPO_ROOT / report_path
        timeseries = pd.read_csv(results_path)

        assert cfg["screening_status"] == "pass"
        assert cfg["response_spectrum"]["method"] == "kinematic_cumtrapz"
        assert summary["npts"] == 1560
        assert summary["dt_s"] == pytest.approx(0.02)
        assert summary["pga_g"] == pytest.approx(0.31882, rel=1.0e-4)
        assert len(timeseries) == 1560
        assert {"velocity_m_s", "displacement_m"}.issubset(timeseries.columns)
        html = report_path.read_text(encoding="utf-8")
        assert "Data provenance" in html
        assert "Kinematic acceleration integration" in html
        assert html.find("<script") >= 0
        assert html.find("cdn.plot") == -1
    elif workflow["id"] == "rao-tabulation":
        summary = cfg["rao_tabulation"]
        results_path = Path(summary["rao_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        results = pd.read_csv(results_path)

        assert summary["n_query_points"] == 3
        assert summary["dofs"] == ["surge", "sway", "heave", "roll", "pitch", "yaw"]
        assert summary["peak_heave_amplitude"] == pytest.approx(1.02)
        assert summary["peak_heave_period_s"] == pytest.approx(18.0)
        assert summary["peak_heave_heading_deg"] == pytest.approx(0.0)
        assert len(results) == 3
        assert (results["heave"] > 0.0).all()
        assert results["heave"].map(math.isfinite).all()

        interpolated_heave = results.loc[0, "heave"]
        assert interpolated_heave == pytest.approx(0.575)
        assert 0.30 < interpolated_heave < 0.75
    elif workflow["id"] == "von-mises":
        summary = cfg["von_mises"]
        results_path = Path(summary["results_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        element_results = pd.read_csv(results_path)

        assert summary["status"] == "completed"
        assert summary["max_stress"] > 0.0
        assert math.isfinite(summary["max_stress"])
        assert summary["min_safety_factor"] > 0.0
        assert summary["min_safety_factor"] == pytest.approx(
            summary["yield_strength"] / summary["max_stress"]
        )
        assert summary["n_critical"] == 0
        assert len(element_results) == summary["num_elements"]
        assert (element_results["von_mises_stress"] > 0.0).all()
        assert (element_results["safety_factor"] > 0.0).all()
    elif workflow["id"] == "elastic-buckling":
        summary = cfg["elastic_buckling"]
        results_path = Path(summary["results_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        mode_results = pd.read_csv(results_path)

        assert summary["status"] == "completed"
        assert summary["first_critical_load"] > 0.0
        assert math.isfinite(summary["first_critical_load"])
        assert summary["num_modes"] >= 2
        assert len(mode_results) == summary["num_modes"]
        assert mode_results["critical_load"].iloc[0] == pytest.approx(
            summary["first_critical_load"]
        )
        assert (
            mode_results["critical_load"].iloc[1]
            > mode_results["critical_load"].iloc[0]
        )
        assert not math.isclose(
            mode_results["critical_load"].iloc[0],
            mode_results["critical_load"].iloc[1],
        )
        assert summary["first_critical_load"] == pytest.approx(
            summary["euler_reference_load"],
            rel=0.05,
        )
    elif workflow["id"] == "stress-strain-parametric":
        cases = cfg["parametric_run"]["cases"]
        manifest = pd.read_csv(
            REPO_ROOT / "examples/workflows/stress-strain-parametric/results/cases.csv"
        )
        case_0 = yaml.safe_load(
            (
                REPO_ROOT
                / "examples/workflows/stress-strain-parametric/results/case_0.yml"
            ).read_text()
        )
        case_2 = yaml.safe_load(
            (
                REPO_ROOT
                / "examples/workflows/stress-strain-parametric/results/case_2.yml"
            ).read_text()
        )
        low_yield = pd.read_csv(
            REPO_ROOT / "examples/workflows/stress-strain-parametric/results/"
            "results/case_0_stress_strain.csv"
        )
        high_yield = pd.read_csv(
            REPO_ROOT / "examples/workflows/stress-strain-parametric/results/"
            "results/case_2_stress_strain.csv"
        )

        assert len(cases) == 3
        assert manifest["status"].tolist() == ["completed"] * 3
        assert case_0["basename"] == "stress_strain"
        assert case_0["stress_strain"]["material"]["yield_strength"] == pytest.approx(
            359.0
        )
        assert case_2["stress_strain"]["material"]["yield_strength"] == pytest.approx(
            552.0
        )
        assert high_yield["stress"].iloc[-1] > low_yield["stress"].iloc[-1]
    elif workflow["id"] == "riser-stackup":
        summary = cfg["riser_stackup"]
        profile_path = Path(summary["profile_csv"])
        if not profile_path.is_absolute():
            profile_path = REPO_ROOT / profile_path
        profile = pd.read_csv(profile_path)

        assert summary["top_tension_required_kn"] > 0
        assert summary["top_tension_required_kn"] >= summary["submerged_weight_kn"]
        assert summary["wall_thickness_required_mm"] > 0
        assert summary["points"] == 5
        assert len(profile) == 5
        assert profile["effective_tension_kn"].iloc[0] > (
            profile["effective_tension_kn"].iloc[-1]
        )
    elif workflow["id"] == "tsj-sizing":
        summary = cfg["tsj_sizing"]
        results_path = Path(summary["results_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        results = pd.read_csv(results_path)

        assert summary["max_utilisation"] > 0.0
        assert math.isfinite(summary["max_utilisation"])
        assert summary["passes"] is True
        assert summary["governing_section"]["position_m"] == pytest.approx(30.0)
        assert len(results) == 4
        assert (results["utilisation"] > 0.0).all()
        assert results["utilisation"].map(math.isfinite).all()
        assert results["utilisation"].max() == pytest.approx(summary["max_utilisation"])
        assert results["vm_stress_Pa"].iloc[-1] > results["vm_stress_Pa"].iloc[0]
    elif workflow["id"] == "riser-stackup-parametric":
        cases = cfg["parametric_run"]["cases"]
        manifest = pd.read_csv(
            REPO_ROOT / "examples/workflows/riser-stackup-parametric/results/cases.csv"
        )
        case_0 = yaml.safe_load(
            (
                REPO_ROOT
                / "examples/workflows/riser-stackup-parametric/results/case_0.yml"
            ).read_text()
        )

        assert len(cases) == 10
        assert manifest["status"].tolist() == ["completed"] * 10
        assert case_0["basename"] == "riser_stackup"
        assert case_0["riser_stackup"]["submerged_weight_kn"] == pytest.approx(
            4268.0377
        )
    elif workflow["id"] == "sn-curve":
        summary = cfg["sn_curve"]
        curve_path = Path(summary["curve_csv"])
        if not curve_path.is_absolute():
            curve_path = REPO_ROOT / curve_path
        curve = pd.read_csv(curve_path)
        cycles = curve["allowable_cycles_n"]

        assert summary["points"] == 6
        assert len(curve) == 6
        assert curve["stress_range_mpa"].is_monotonic_increasing
        assert (cycles > 0).all()
        assert cycles.map(math.isfinite).all()
        assert all(
            cycles.iloc[index] < cycles.iloc[index - 1]
            for index in range(1, len(cycles))
        )
    elif workflow["id"] == "sn-curve-comparison":
        summary = cfg["sn_curve"]
        curve_path = Path(summary["curve_csv"])
        if not curve_path.is_absolute():
            curve_path = REPO_ROOT / curve_path
        curve = pd.read_csv(curve_path)
        curve_ids = {
            "DNV-RP-C203:D:air",
            "DNV-RP-C203:E:air",
            "DNV-RP-C203:F:air",
            "DNV-RP-C203:F1:air",
        }

        assert summary["points"] == 24
        assert len(summary["curves"]) == 4
        assert len(curve) == 24
        assert set(curve["curve_id"]) == curve_ids
        for _, group in curve.groupby("curve_id"):
            assert group["stress_range_mpa"].is_monotonic_increasing
            cycles = group["allowable_cycles_n"]
            assert all(
                cycles.iloc[index] < cycles.iloc[index - 1]
                for index in range(1, len(cycles))
            )
    elif workflow["id"] == "plate-buckling":
        result = cfg["plate_buckling"][0]
        assert result["dnv_rp_usage_factor"]["usage_longtudinal"] == pytest.approx(
            0.1578
        )
        assert result["dnv_rp_usage_factor"]["usage_bi_axial_with_shear"] == (
            pytest.approx(0.236)
        )
        assert result["usage_factor_ultimate_check"]["usage_equivalent"] < 1
    elif workflow["id"] == "on-bottom-stability-f109":
        result = cfg["on_bottom_stability"]["result"]
        assert result["submerged_weight_N_m"] == pytest.approx(2133.496806)
        assert result["horizontal_load_N_m"] == pytest.approx(715.325862)
        assert result["lift_load_N_m"] == pytest.approx(502.197469)
        assert result["required_submerged_weight_N_m"] == pytest.approx(2075.914365)
        assert result["lateral_utilization"] == pytest.approx(0.964701)
        assert result["is_laterally_stable"] is True
    elif workflow["id"] == "free-span-f105":
        result = cfg["free_span"]["result"]
        assert result["fn_IL_hz"] == pytest.approx(0.427094447363)
        assert result["Ks"] == pytest.approx(0.396584516808)
        assert result["Ur_IL"] == pytest.approx(6.858740341547)
        assert result["il_viv_onset"] is True
        assert result["cf_viv_onset"] is True
        assert result["allowable_span_m"] == pytest.approx(12.378282602856)
        assert result["span_utilization"] == pytest.approx(2.746746143294)
        assert result["cf_stress_mpa"] == pytest.approx(97.942915033638)
    elif workflow["id"] == "pipeline-lateral-buckling":
        result = cfg["pipeline"]["lateral_buckling"]
        assert result["lateral_buckling_check"] == "Pass"
        assert result["anchor_length"]["start"] == pytest.approx(1368.5)
        assert result["anchor_length"]["end"] == pytest.approx(1368.5)
        assert result["min_critical_buckling_load"] == pytest.approx(-64701.045412)
        assert result["effective_axial_load"]["anchor_start"] == pytest.approx(-33.26)
        assert result["fully_restrained_axial_force"]["L=0"] == pytest.approx(-253.575)
    elif workflow["id"] == "pipeline-upheaval-buckling":
        result = cfg["pipeline"]["upheaval_buckling"]
        assert result["cover_check"] == "Pass"
        assert result["available_download_N_m"] == pytest.approx(33.048855)
        assert result["required_download_N_m"] == pytest.approx(30.0)
        assert result["download_margin_N_m"] == pytest.approx(3.048855)
        assert result["level_1_analysis"]["N"] == pytest.approx(184397.093676)
    elif workflow["id"] == "orcawave-input-prep":
        generated = Path(cfg["diffraction"]["outputs"]["orcawave"])
        data = yaml.safe_load(generated.read_text())
        body = data["Bodies"][0]

        assert generated.name == "RegistryShip.yml"
        assert data["WaterDepth"] == pytest.approx(120.0)
        assert data["WaterDensity"] == pytest.approx(1.025)
        assert data["PeriodOrFrequency"] == pytest.approx([5.236, 7.854, 15.708])
        assert data["WaveHeading"] == [0.0, 90.0, 180.0]
        assert body["BodyName"] == "RegistryShip"
        assert body["BodyMeshFileName"] == "sample_box.gdf"
    elif workflow["id"] == "aqwa-diffraction-deck-prep":
        generated = Path(cfg["diffraction"]["outputs"]["aqwa"])
        content = generated.read_text()

        assert generated.name == "registry_ship_diffraction.dat"
        assert "JOB AQWA  LINE" in content
        assert "TITLE               registry_ship_diffraction" in content
        assert "      DPTH       120" in content
        assert content.count("HRTZ") == 3
        assert content.count("DIRN") == 5
        assert content.count("QPPL") == 5
        assert "could not be loaded" not in content
    elif workflow["id"] == "dynacard-diagnostics":
        results = cfg["results"]
        assert results["diagnostic_message"].startswith("Classification: PUMP_TAGGING.")
        assert results["pump_fillage"] == pytest.approx(75.909653)
        assert results["inferred_production"] == pytest.approx(338.041470)

        html_report = Path(cfg["outputs"]["html_report"])
        if not html_report.is_absolute():
            html_report = REPO_ROOT / html_report
        html = html_report.read_text()
        assert "Pump Tagging" in html
        assert "SIM-PUMP-TAGGING-711" in html
    elif workflow["id"] == "artificial-lift-field-health":
        summary = cfg["artificial_lift_field_health"]
        statuses = {row["api14"]: row["health_status"] for row in summary["wells"]}

        assert cfg["screening_status"] == "fail"
        assert summary["screening_status"] == "fail"
        assert summary["n_wells"] == 3
        assert summary["field_status_counts"] == {
            "warning": 1,
            "critical": 1,
            "failure": 1,
        }
        assert statuses == {
            "SIM-FIELD-RESTRICTION-711": "warning",
            "SIM-FIELD-PUMP-TAGGING-711": "critical",
            "SIM-FIELD-ROD-PARTING-711": "failure",
        }
        assert summary["worst_wells"][0]["api14"] == "SIM-FIELD-ROD-PARTING-711"

        wells_csv = Path(cfg["outputs"]["well_status_csv"])
        if not wells_csv.is_absolute():
            wells_csv = REPO_ROOT / wells_csv
        well_rows = pd.read_csv(wells_csv)
        assert len(well_rows) == 3
        assert set(well_rows["health_status"]) == {"warning", "critical", "failure"}
    elif workflow["id"] == "orcaflex-6dbuoy-dnvrph103":
        props = cfg["code_dnvrph103"]["properties"]
        translational = props["translational"]
        assert translational["ca"]["x"] == pytest.approx(0.703132)
        assert translational["area_drag"]["x"] == pytest.approx(0.427716)
        assert props["I"]["Ix"] == pytest.approx(19.471869)

        model_path = (
            REPO_ROOT / "examples/workflows/orcaflex-6dbuoy-dnvrph103/results/"
            "dnvrph103_demo_6dbuoy_deep.yml"
        )
        model = yaml.safe_load(model_path.read_text())
        buoy = model["6DBuoys"]["dnvrph103_demo_6dbuoy"]
        assert buoy["Mass"] == pytest.approx(0.169)
        assert buoy["Volume"] == pytest.approx(0.28)
        assert buoy["DragForceCoefficient"] == [
            pytest.approx(1.15),
            pytest.approx(1.15),
            pytest.approx(1.15),
        ]
    elif workflow["id"] == "fpso-spread-mooring":
        result = cfg["fpso_mooring"]
        summary = result["summary"]
        assert summary["overall_status"] == "PASS"
        assert summary["n_results"] == 8
        assert summary["min_safety_factor"] == pytest.approx(2.08)
        assert summary["max_utilization"] == pytest.approx(0.48)
        assert result["environmental_loads"]["total_force"] == pytest.approx(2280.94225)

        summary_path = Path(cfg["outputs"]["summary_json"])
        if not summary_path.is_absolute():
            summary_path = REPO_ROOT / summary_path
        summary_json = yaml.safe_load(summary_path.read_text())
        assert summary_json["summary"]["critical_line"]["line_id"] == "ML1"
    elif workflow["id"] == "fpso-mooring-full":
        result = cfg["fpso_mooring_full"]
        summary = result["summary"]
        lines = result["line_tensions"]

        assert summary["n_lines"] == 8
        assert result["environmental_forces"]["total_force_N"] > 0.0
        assert result["environmental_forces"]["wave_drift_force_N"] > 0.0
        assert result["static_equilibrium"]["offset_m"] > 0.0
        assert len(lines) == 8
        assert summary["max_line_tension_N"] == pytest.approx(
            max(line["top_tension_N"] for line in lines)
        )
        assert summary["max_line_tension_N"] > summary["pretension_N"]

        summary_path = Path(cfg["outputs"]["summary_json"])
        tensions_path = Path(cfg["outputs"]["line_tensions_csv"])
        if not summary_path.is_absolute():
            summary_path = REPO_ROOT / summary_path
        if not tensions_path.is_absolute():
            tensions_path = REPO_ROOT / tensions_path
        summary_json = yaml.safe_load(summary_path.read_text())
        tensions = pd.read_csv(tensions_path)
        assert summary_json["summary"]["n_lines"] == 8
        assert len(tensions) == 8
    elif workflow["id"] == "wall-thickness-quickcheck":
        result = cfg["wall_thickness"]["quickcheck"]
        selection = result["selection"]["with_arrestors"]
        assert selection["selected_standard_label"] == "SCH 80"
        assert selection["selected_standard_wall_mm"] == pytest.approx(17.475)
        assert selection["nonstandard_minimum_wall_mm"] == pytest.approx(14.909)
        assert selection["governing_check"] == "DNV-ST-F101 collapse"
        assert selection["governing_utilisation"] == pytest.approx(0.629046)
        arrestor = result["buckle_arrestor_sizing"]
        assert arrestor["arrestor_wall_mm"] == pytest.approx(25.5)
    elif workflow["id"] == "api579-pipe-ffs-b314":
        gml = cfg["Result"]["Circumference"][0]
        gml_mawp = cfg["Result"]["GML_MAWP"][0][0]
        lml = cfg["Result"]["LML"][0][0]
        assert gml["Min WT (inch)"] == pytest.approx(0.312)
        assert gml["Avg. WT (inch)"] == pytest.approx(0.329727273)
        assert gml_mawp["MAWP"] == pytest.approx(930.994652)
        assert cfg["Result"]["GML_Acceptable_FCA"][0] == pytest.approx(0.06)
        assert lml["MAWP"] == pytest.approx(970.588235)
        assert lml["RSF, L2"] == pytest.approx(1.0)
        assert lml["RSF, L2"] >= cfg["API579Parameters"]["RSFa"]
        assert lml["MAWPr, L2"] == pytest.approx(970.588235)
        # The FFS verdict: measured-lattice MAWP must cover the design pressure
        design_pressure = cfg["Design"][0]["InternalPressure"]["Outer_Pipe"]
        assert gml_mawp["MAWP"] >= design_pressure
        assert lml["MAWPr, L2"] >= design_pressure
    elif workflow["id"] == "api579-pipe-ffs-b318":
        gml = cfg["Result"]["Circumference"][0]
        gml_mawp = cfg["Result"]["GML_MAWP"][0][0]
        lml = cfg["Result"]["LML"][0][0]
        assert gml["Min WT (inch)"] == pytest.approx(0.548)
        assert gml["Avg. WT (inch)"] == pytest.approx(0.584454545)
        assert gml_mawp["MAWP"] == pytest.approx(3548.686981)
        assert cfg["Result"]["GML_Acceptable_FCA"][0] == pytest.approx(0.16)
        assert lml["MAWP"] == pytest.approx(3726.454203)
        assert lml["RSF, L2"] == pytest.approx(1.0)
        assert lml["RSF, L2"] >= cfg["API579Parameters"]["RSFa"]
        assert lml["MAWPr, L2"] == pytest.approx(3726.454203)
        # The FFS verdict: measured-lattice MAWP must cover the design pressure
        design_pressure = cfg["Design"][0]["InternalPressure"]["Outer_Pipe"]
        assert gml_mawp["MAWP"] >= design_pressure
        assert lml["MAWPr, L2"] >= design_pressure
    elif workflow["id"] == "jacket-member-joint-checks":
        result = cfg["jacket_checks"]
        members = {item["member_id"]: item for item in result["members"]}
        joints = {item["joint_id"]: item for item in result["joints"]}

        assert result["overall_status"] == "PASS"
        assert members["JKT-LEG-01"]["governing_check"] == "combined"
        assert members["JKT-LEG-01"]["governing_uc"] == pytest.approx(0.416667)
        assert members["TSD-BRACE-01"]["governing_uc"] == pytest.approx(0.574074)
        assert joints["JNT-T-01"]["joint_type"] == "T/Y"
        assert joints["JNT-T-01"]["unity_check"] == pytest.approx(0.062164331)
        assert joints["JNT-T-01"]["allowable_vp"] == pytest.approx(17.810291)
    elif workflow["id"] == "ocimf-tanker-loads":
        loads = cfg["ocimf"]["results"]["loads"]
        wind = loads["wind"]
        current = loads["current"]
        total = loads["total"]

        assert wind["fx_N"] == pytest.approx(73500.000472)
        assert wind["fy_N"] == pytest.approx(665172.461887)
        assert wind["mz_Nm"] == pytest.approx(57764978.137437)
        assert current["fx_N"] == pytest.approx(1394305.164879)
        assert current["fy_N"] == pytest.approx(4185843.706462)
        assert total["fx_N"] == pytest.approx(1467805.165352)
        assert total["fy_N"] == pytest.approx(4851016.168349)
        assert total["mz_Nm"] == pytest.approx(727673679.141325)
    elif workflow["id"] == "naval-arch-yaw-moment":
        rows = cfg["naval_arch"]["yaw_moment"]["result"]["rows"]
        row = rows[0]

        assert len(rows) == 1
        assert row["speed_m_s"] == pytest.approx(5.0)
        assert row["rudder_angle_deg"] == pytest.approx(10.0)
        assert row["scalar_normal_force_N"] == pytest.approx(97417.402886)
        assert row["transverse_force_N"] == pytest.approx(97417.402886)
        assert row["yaw_moment_Nm"] == pytest.approx(-4383783.129880)
        assert row["sign_convention"] == "port"
    elif workflow["id"] == "naval-arch-maneuvering-envelope":
        block = cfg["naval_arch"]["maneuvering_envelope"]
        assert cfg["naval_arch"]["calculation"] == "maneuvering_envelope"
        meta = block["result"]["metadata"]
        rows = block["result"]["rows"]

        # Full-form tanker is marginally course-unstable; IMO turning passes.
        assert meta["course_stability_discriminant"] < 0
        assert meta["rudder_lift_slope_per_rad"] == pytest.approx(3.77, abs=0.02)
        assert meta["tactical_diameter_over_L"] == pytest.approx(3.2, abs=0.1)
        assert meta["imo_turning"]["overall_pass"] is True

        assert len(rows) == 2  # 1 loading x 2 current speeds
        r3 = next(r for r in rows if r["current_speed_kn"] == pytest.approx(3.0))
        r5 = next(r for r in rows if r["current_speed_kn"] == pytest.approx(5.0))

        # 3 kn beam current holdable on the engine; 5 kn beyond authority -> tug.
        assert r3["can_hold_heading"] is True
        assert r5["can_hold_heading"] is False
        assert r5["required_rudder_angle_deg"] is None
        assert r3["utilisation"] < 1.0 < r5["utilisation"]
        # Current yaw moment is quadratic in current speed.
        assert r5["current_yaw_moment_MNm"] / r3["current_yaw_moment_MNm"] == pytest.approx(
            (5.0 / 3.0) ** 2, rel=1e-6
        )
        # Frozen deterministic values.
        assert r3["threshold_speed_engine_on_kn"] == pytest.approx(1.9054868442)
        assert r3["current_yaw_moment_MNm"] == pytest.approx(37.864787106557)
        assert r3["required_rudder_angle_deg"] == pytest.approx(33.407753387878)
    elif workflow["id"] == "rudder-stock-torque":
        row = cfg["naval_arch"]["rudder_stock_torque"]["result"]["rows"][0]
        assert cfg["naval_arch"]["calculation"] == "rudder_stock_torque"
        assert len(cfg["naval_arch"]["rudder_stock_torque"]["result"]["rows"]) == 1
        assert row["speed_kn"] == pytest.approx(12.0)
        assert row["rudder_angle_deg"] == pytest.approx(35.0)
        assert row["scalar_normal_force_N"] == pytest.approx(722870.905140)
        assert row["hydrodynamic_rudder_stock_torque_Nm"] == pytest.approx(
            542153.178855
        )
        assert row["required_steering_gear_holding_torque_Nm"] == pytest.approx(
            -542153.178855
        )
        assert row["rudder_stock_torque_abs_kNm"] == pytest.approx(542.153178855)
    elif workflow["id"] == "pile-axial-capacity":
        block = cfg["geotechnical"]["pile_axial_capacity"]
        result = block["result"]
        design = block["design"]
        assert block["standard"] == "API RP 2GEO"
        assert result["total_capacity_kn"] == pytest.approx(16137.983938)
        assert result["skin_friction_kn"] == pytest.approx(14305.807103)
        assert result["end_bearing_kn"] == pytest.approx(1832.176836)
        assert result["alpha"] == pytest.approx(0.790569415)
        assert design["allowable_capacity_kn"] == pytest.approx(8068.991969)
        assert design["utilization"] == pytest.approx(0.619656088)
        assert design["status"] == "PASS"
    elif workflow["id"] == "drag-anchor-holding":
        block = cfg["geotechnical"]["drag_anchor"]
        result = block["result"]
        design = block["design"]
        assert block["standard"] == "DNV-RP-E302"
        assert result["holding_capacity_kn"] == pytest.approx(3600.0)
        assert result["efficiency"] == pytest.approx(30.0)
        assert design["allowable_holding_kn"] == pytest.approx(2400.0)
        assert design["status"] == "PASS"
    elif workflow["id"] == "nodal-analysis-ipr-vlp":
        block = cfg["production"]["nodal_analysis"]
        op = block["operating_point"]
        assert block["ipr_model"] == "vogel"
        assert op["q_bopd"] == pytest.approx(996.727220, rel=1e-4)
        assert op["pwf_psi"] == pytest.approx(2458.137610, rel=1e-4)
        assert op["confidence"] == "Green"
        assert op["q_uncertainty_fraction"] == pytest.approx(0.05)
        assert op["q_low_bopd"] < op["q_bopd"] < op["q_high_bopd"]
    elif workflow["id"] == "anchor-capacity":
        result = cfg["anchor_capacity"]["results"]
        assert result["drag_anchor"]["holding_capacity_kn"] == pytest.approx(3600.0)
        assert result["suction_anchor"]["total_capacity_kn"] == pytest.approx(
            14417.082847
        )
        assert cfg["anchor_capacity"]["design"]["status"] == "PASS"
    elif workflow["id"] == "pile-capacity":
        result = cfg["pile_capacity"]["result"]
        assert result["total_capacity_kn"] == pytest.approx(4894.679728)
        assert result["alpha"] == pytest.approx(0.790569415)
        assert cfg["pile_capacity"]["design"]["status"] == "PASS"
    elif workflow["id"] == "scour-assessment":
        result = cfg["scour"]["results"]
        assert result["pipeline"]["scour_depth_m"] == pytest.approx(0.225)
        assert result["monopile"]["scour_depth_m"] == pytest.approx(7.8)
        assert result["rock_armour"]["thickness_m"] == pytest.approx(0.6)
    elif workflow["id"] == "mudmat-bearing-capacity":
        block = cfg["mudmat_bearing_capacity"]
        result = block["result"]
        design = block["design"]
        # undrained: Nc = 2 + pi (= 5.14), Nq = 1, Ngamma = 0
        assert result["Nc"] == pytest.approx(2.0 + math.pi)
        assert result["Nq"] == pytest.approx(1.0)
        assert result["Ngamma"] == pytest.approx(0.0)
        # q_ult = su*Nc*sc*dc + gamma'*D  (Nq=1, Ngamma=0)
        sc, dc = result["shape_factors"]["sc"], result["depth_factors"]["dc"]
        expected_qult = 15.0 * (2.0 + math.pi) * sc * dc + 6.0 * 0.5
        assert result["q_ult_kpa"] == pytest.approx(expected_qult, rel=1e-9)
        assert result["vertical_capacity_kn"] == pytest.approx(
            expected_qult * result["effective_area_m2"], rel=1e-9
        )
        # bearing governs and the screen fails (utilisation ~ 1.10)
        assert design["governing_check"] == "bearing"
        assert design["bearing_utilization"] == pytest.approx(1.1005587885, rel=1e-6)
        assert design["sliding_utilization"] < 1.0
        assert design["screening_status"] == "fail"
        assert cfg["screening_status"] == "fail"
    elif workflow["id"] == "liquefaction-triggering":
        result = cfg["liquefaction"]["result"]
        layers = {round(L["depth_m"]): L for L in result["layers"]}
        # shallow loose sand (N=8) liquefies; deep dense sand (N=28) does not
        assert layers[3]["liquefies"] is True
        assert layers[9]["liquefies"] is False
        assert layers[3]["factor_of_safety"] == pytest.approx(0.454, abs=0.01)
        assert layers[9]["factor_of_safety"] == pytest.approx(1.503, abs=0.01)
        # governing = the lowest-FS (shallow) layer; overall fails
        assert result["governing_depth_m"] == pytest.approx(3.0)
        assert result["governing_factor_of_safety"] == pytest.approx(
            min(L["factor_of_safety"] for L in result["layers"])
        )
        assert result["screening_status"] == "fail"
        assert cfg["screening_status"] == "fail"
    elif workflow["id"] == "casing-design":
        summary = cfg["casing_design"]["summary"]
        assert summary["product_count"] == 8
        assert summary["passing_products"] == [
            '5.5" 20# P110',
            '5.5" 23# P110',
            '5.5" 23# Q125',
        ]
        golden = next(p for p in summary["products"]
                      if p["label"] == '5.5" 23# P110')
        # Barlow worked example from the source deck: 14,520 psi API-rounded.
        assert golden["burst_rating_psi"] == pytest.approx(14520.0)
        assert golden["collapse_rating_psi"] == pytest.approx(14540.0)
        assert golden["body_yield_lbf"] == pytest.approx(729000.0)
        assert golden["max_frac_surface_pressure_psi"] == pytest.approx(
            11616.0)
        assert golden["passes_all"] is True
        sour = summary["sour_service"]
        # 100 ppm at 8,500 psia -> 0.85 psia partial pressure -> sour.
        assert sour["is_sour"] is True
        assert sour["h2s_partial_psia"] == pytest.approx(0.85)
        assert "P110" in sour["acceptable_grades"]  # 180 F >= 175 F window
        assert "Q125" not in sour["acceptable_grades"]  # needs >= 225 F
    elif workflow["id"] == "well-bore-design":
        block = cfg["well_bore_design"]
        summary = block["summary"]
        assert block["hole_type"] == "standard_hole"
        assert summary["casing_count"] == 5
        assert summary["total_cost_usd"] == pytest.approx(21500000.0)
        assert summary["production_potential_bopd"] == pytest.approx(15000.0)
        assert summary["risk"]["overall_score"] == pytest.approx(4.0)
        assert summary["recommended_hole_type"] == "standard_hole"
    elif workflow["id"] == "well-hydraulics":
        result = cfg["well_hydraulics"]["result"]
        assert result["annular_velocity_ft_min"] == pytest.approx(188.538461538)
        assert result["pressure_drop_annulus_psi"] == pytest.approx(127.087912088)
        assert result["ecd_ppg"] == pytest.approx(10.805499789)
        assert result["cuttings_transport_ratio"] == pytest.approx(1.0)
    elif workflow["id"] == "wellpath":
        summary = cfg["wellpath"]
        results_path = Path(summary["wellpath_csv"])
        if not results_path.is_absolute():
            results_path = REPO_ROOT / results_path
        survey = pd.read_csv(results_path)

        assert summary["total_depth"] == pytest.approx(400.0)
        assert summary["total_tvd"] < summary["total_depth"]
        assert summary["max_dls"] > 0.0
        assert summary["max_dls_depth"] == pytest.approx(200.0)
        assert len(survey) == 5
        assert survey["tvd"].iloc[-1] < survey["md"].iloc[-1]
        assert survey["dls"].iloc[2] > 0.0
    elif workflow["id"] == "rop-analysis":
        result = cfg["rop_analysis"]["result"]
        by = result["bourgoyne_young"]
        warren = result["warren"]
        assert by["rop_ft_hr"] == pytest.approx(5.636893157)
        assert by["sensitivity_wob_ft_hr_per_klb"] == pytest.approx(0.140933341)
        assert by["sensitivity_rpm_ft_hr_per_rpm"] == pytest.approx(0.033829259)
        assert warren["rop_ft_hr"] == pytest.approx(167.096226327)
    elif workflow["id"] == "tubular-design-envelope":
        summary = cfg["tubular_design"]["summary"]
        assert summary["geometry"]["yield_force_lbf"] == pytest.approx(603928.713320)
        assert summary["envelopes"]["vme_isotropic"]["max_pressure_psi"] == (
            pytest.approx(7224.808854)
        )
        assert summary["envelopes"]["vme_isotropic"]["min_pressure_psi"] == (
            pytest.approx(-7224.808854)
        )
        assert summary["envelopes"]["api_ellipse"]["max_pressure_psi"] == (
            pytest.approx(4984.071753)
        )
        assert summary["envelopes"]["api_ellipse"]["min_pressure_psi"] == (
            pytest.approx(-5340.076878)
        )
        envelope_csv = Path(cfg["outputs"]["envelope_csv"])
        if not envelope_csv.is_absolute():
            envelope_csv = REPO_ROOT / envelope_csv
        envelope = pd.read_csv(envelope_csv)
        assert len(envelope) == 160
        assert set(envelope["envelope"]) == {"vme_isotropic", "api_ellipse"}
    elif workflow["id"] == "damage-stability":
        result = cfg["naval_arch"]["damage_stability"]["result"]
        assert cfg["naval_arch"]["calculation"] == "damage_stability"
        assert result["imo_intact_stability"]["overall_pass"] is True
        assert result["area_to_30_m_rad"] == pytest.approx(0.236055781)
        assert result["area_to_40_m_rad"] == pytest.approx(0.434237918)
        assert result["gz_at_30_m"] == pytest.approx(0.945)
        assert result["angle_of_max_gz_deg"] == pytest.approx(50.0)
        assert result["sinkage_m"] == pytest.approx(0.2125)
        assert result["flooded_gm_m"] == pytest.approx(1.0)
    elif workflow["id"] == "platform-stability":
        result = cfg["naval_arch"]["platform_stability"]["result"]
        assert cfg["naval_arch"]["calculation"] == "platform_stability"
        assert result["platform_type"] == "fpso"
        assert result["intact"] is True
        assert result["gm_m"] == pytest.approx(2.784602183)
        assert result["gz_at_30deg"] == pytest.approx(1.392301092)
        assert result["area_0_to_30"] == pytest.approx(0.372829168)
        assert result["area_0_to_40"] == pytest.approx(0.651059664)
        assert result["wind_heel_angle_deg"] == pytest.approx(0.655465685)
        assert result["wind_criterion_ok"] is True
    elif workflow["id"] == "hull-resistance":
        result = cfg["naval_arch"]["hull_resistance"]["result"]
        coefficients = result["coefficients"]
        assert cfg["naval_arch"]["calculation"] == "hull_resistance"
        assert coefficients["cb"] == pytest.approx(0.60)
        assert coefficients["cm"] == pytest.approx(0.977)
        assert coefficients["cwp"] == pytest.approx(0.70)
        assert coefficients["cp_from_cb_cm"] == pytest.approx(0.614124872)
        assert result["froude_number"] == pytest.approx(0.223226279)
        assert result["wetted_surface_m2"] == pytest.approx(2429.246060393)
        assert result["total_resistance_N"] == pytest.approx(195598.466626663)
        assert result["effective_power_W"] == pytest.approx(1510020.162357836)
    elif workflow["id"] == "hull-seakeeping":
        result = cfg["naval_arch"]["hull_seakeeping"]["result"]
        assert cfg["naval_arch"]["calculation"] == "hull_seakeeping"
        assert result["natural_roll_period_s"] == pytest.approx(24.072800169)
        assert result["natural_heave_period_s"] == pytest.approx(3.617618822)
        assert result["natural_pitch_period_s"] == pytest.approx(6.142299697)
        assert result["encounter_frequency_rad_s"] == pytest.approx(1.321916412)
        assert result["simple_heave_rao"] == pytest.approx(1.267131324)
        assert result["motion_sickness_incidence_pct"] == pytest.approx(8.485281374)
        assert result["significant_motion_m"] == pytest.approx(2.0)
    elif workflow["id"] == "propeller-rudder-interaction":
        block = cfg["propeller_rudder"]
        rows = block["result"]["rows"]
        assert block["result"]["metadata"]["method"] == "soding"
        assert len(rows) == 4

        zero = rows[0]
        assert zero["rudder_angle_deg"] == pytest.approx(0.0)
        assert zero["F_sway_N"] == pytest.approx(0.0, abs=1e-9)
        assert zero["F_yaw_Nm"] == pytest.approx(0.0, abs=1e-9)

        full = rows[-1]
        assert full["method"] == "soding"
        assert full["ship_speed_kn"] == pytest.approx(12.0)
        assert full["shaft_speed_rev_s"] == pytest.approx(2.0)
        assert full["rudder_angle_deg"] == pytest.approx(35.0)
        assert full["F_surge_N"] == pytest.approx(-23199.667135705)
        assert full["F_sway_N"] == pytest.approx(-73579.943704159)
        assert full["F_yaw_Nm"] == pytest.approx(-3678997.185207926)
        # F_yaw == F_sway * lever arm (x_R = 50 m) for the vertical rudder
        assert full["F_yaw_Nm"] == pytest.approx(full["F_sway_N"] * 50.0)

        # Force magnitude grows monotonically with rudder deflection.
        sway_mag = [abs(row["F_sway_N"]) for row in rows]
        assert sway_mag == sorted(sway_mag)

        sweep_csv = REPO_ROOT / (
            "examples/workflows/propeller-rudder-interaction/results/"
            "propeller_rudder/propeller_rudder_sweep.csv"
        )
        table = pd.read_csv(sweep_csv)
        assert len(table) == 4
        assert table["F_sway_N"].iloc[-1] == pytest.approx(-73579.943704159)
    elif workflow["id"] in {
        "hydro-coefficients",
        "wave-spectra",
        "passing-ship",
    }:
        from tests.workflows.hydrodynamics_assertions import (
            assert_hydrodynamics_workflow,
        )

        assert_hydrodynamics_workflow(workflow["id"], cfg)
    elif workflow["id"] == "ct-hydraulics":
        results = cfg["ct_hydraulics"]["results"]
        summary = results["summary"]
        assert summary["hydrostatic_pressure_psi"] == pytest.approx(4376.0)
        assert summary["pump_pressure_psi"] == pytest.approx(2258.6)
        assert summary["downhole_pressure_psi"] == pytest.approx(6476.1)
        assert summary["equivalent_circulating_density_ppg"] == pytest.approx(8.567)
        ct = results["coiled_tubing"]
        assert ct["id_in"] == pytest.approx(1.482)
        assert ct["reynolds_number"] == pytest.approx(131177.7)
        assert ct["pressure_loss_psi"] == pytest.approx(1300.1)
    elif workflow["id"] == "rigging":
        groups = cfg["rigging"]["resolved_groups"]
        assert len(groups) == 1
        group = groups[0]
        assert group["label"] == "uta1_clump_weight"

        elements = {element["part_number"]: element for element in group["elements"]}
        assert set(elements) == {2038614, 1500, 1019533}

        # Shackle G2100 part 2038614 resolves to its crosby workbook row.
        g2100 = elements[2038614]
        assert g2100["category"] == "shackle"
        assert g2100["subcategory"] == "G2100"
        assert g2100["catalog_record"]["wll_te"] == pytest.approx(25.0)
        assert g2100["catalog_record"]["w_lb"] == pytest.approx(38.6)
        assert g2100["catalog_record"]["design_factor"] == 6

        # Sling 1500 (polyester endless round) resolves to its rated
        # capacities, width range, unit weight, and design factor.
        sling = elements[1500]
        assert sling["category"] == "sling"
        model = sling["model"]
        assert model["wll_vertical_lb"] == pytest.approx(15000.0)
        assert model["wll_choker_lb"] == pytest.approx(12000.0)
        assert model["wll_vertical_basket_90_deg_lb"] == pytest.approx(30000.0)
        assert model["min_width_in"] == pytest.approx(1.5)
        assert model["max_width_in"] == pytest.approx(3.0)
        assert model["weight_lb_per_ft"] == pytest.approx(0.45)
        assert model["design_factor"] == 5

        # Shackle G2130 part 1019533 resolves to its bolt-type anchor row.
        g2130 = elements[1019533]
        assert g2130["subcategory"] == "G2130"
        assert g2130["catalog_record"]["wll_te"] == pytest.approx(6.5)
        assert g2130["catalog_record"]["size_in"] == pytest.approx(0.875)

        summary_path = Path(cfg["rigging"]["summary_json"])
        if not summary_path.is_absolute():
            summary_path = REPO_ROOT / summary_path
        summary = yaml.safe_load(summary_path.read_text())
        assert summary["groups"][0]["label"] == "uta1_clump_weight"
    elif workflow["id"] == "lifting-lug-design":
        block = cfg["lifting_lug"]
        # P = static * DAF * skew = 420 * 2.0 * 1.1
        assert block["design_load_kN"] == pytest.approx(924.0)
        checks = {c["name"]: c for c in block["checks"]}
        assert set(checks) == {"pin_bearing", "net_tension", "shear_tearout"}
        # shear tear-out governs (0.40 Fy allowable) and fails
        assert block["governing_check"] == "shear_tearout"
        assert checks["shear_tearout"]["utilization"] == pytest.approx(
            1.16197183, rel=1e-6
        )
        assert checks["shear_tearout"]["passes"] is False
        assert bool(checks["pin_bearing"]["passes"]) is True
        assert bool(checks["net_tension"]["passes"]) is True
        # shear demand equals tension demand (same area), lower allowable
        assert checks["shear_tearout"]["demand_mpa"] == pytest.approx(
            checks["net_tension"]["demand_mpa"]
        )
        assert block["screening_status"] == "fail"
        assert cfg["screening_status"] == "fail"
    elif workflow["id"] == "inspection-planning":
        result = cfg["inspection_planning"]["result"]
        # CR = (11.0 - 8.5)/5 = 0.5 mm/yr; allowance = 8.5 - 6.0 = 2.5 mm
        assert result["corrosion_rate_mm_yr"] == pytest.approx(0.5)
        assert result["corrosion_allowance_remaining_mm"] == pytest.approx(2.5)
        # remaining life = 2.5 / 0.5 = 5.0 yr; half-life interval = 2.5 yr
        assert result["remaining_life_years"] == pytest.approx(5.0)
        assert result["half_life_interval_years"] == pytest.approx(2.5)
        # half-life (2.5) < code max (10) -> governs
        assert result["next_inspection_interval_years"] == pytest.approx(2.5)
        # 5.0 yr remaining < 10 yr required -> fail
        assert result["screening_status"] == "fail"
        assert cfg["screening_status"] == "fail"
    elif workflow["id"] == "span-rectification":
        result = cfg["span_rectification"]["result"]
        allowable = result["allowable_span_m"]
        # as-built 60 m span exceeds the ~12.4 m allowable -> rectification required
        assert result["actual_span_m"] == pytest.approx(60.0)
        assert result["rectification_required"] is True
        assert result["screening_status"] == "fail"
        assert cfg["screening_status"] == "fail"
        # n_supports = ceil(L_a / L_allow) - 1; equal sub-spans within allowable
        expected_subspans = math.ceil(60.0 / allowable)
        assert result["sub_span_count"] == expected_subspans
        assert result["n_supports"] == expected_subspans - 1
        assert result["resulting_sub_span_m"] == pytest.approx(60.0 / expected_subspans)
        assert result["resulting_sub_span_m"] <= allowable + 1e-9
    elif workflow["id"] == "weather-window":
        result = cfg["weather_window"]["result"]
        # 56 steps x 3 h = 168 h record; 41 steps below the 2.0 m limit
        assert result["record_hours"] == pytest.approx(168.0)
        assert result["workability_pct"] == pytest.approx(100.0 * 41.0 / 56.0)
        # four windows: 36, 30, 24, 33 h -> longest 36 h, mean 30.8 h
        assert result["longest_window_hours"] == pytest.approx(36.0)
        assert result["num_windows"] == 4
        # operation needs 48 h; longest window is 36 h -> no fit, fails
        assert result["operation_fits_window"] is False
        assert result["num_viable_windows"] == 0
        assert result["screening_status"] == "fail"
        assert cfg["screening_status"] == "fail"
        # workability (73%) clears the 50% requirement -> the fail is window-fit
        assert result["workability_pct"] >= result["required_workability_pct"]
    elif workflow["id"] == "esp-pump-hydraulics":
        result = cfg["esp_pump_hydraulics"]["result"]
        # TDH = net lift + friction + discharge head
        assert result["net_lift_m"] == pytest.approx(1800.0)
        assert result["total_dynamic_head_m"] == pytest.approx(
            result["net_lift_m"]
            + result["friction_head_m"]
            + result["discharge_head_m"],
            rel=1e-12,
        )
        # stages = ceil(TDH / head_per_stage) = ceil(2425.4 / 6.0) = 405
        assert result["stages_required"] == math.ceil(
            result["total_dynamic_head_m"] / 6.0
        )
        assert result["stages_required"] == 405
        # brake power = stages * bhp_per_stage * SG
        assert result["brake_power_hp"] == pytest.approx(405 * 0.5 * 0.85)
        # stages exceed the 400 housing -> governing fail; motor + rate pass
        checks = {c["name"]: c for c in result["checks"]}
        assert checks["pump_stages"]["passes"] is False
        assert checks["motor_power"]["passes"] is True
        assert checks["rate_within_range"]["passes"] is True
        assert result["governing_check"] == "pump_stages"
        assert result["screening_status"] == "fail"
        assert cfg["screening_status"] == "fail"
    elif workflow["id"] == "fowt-mooring":
        result = cfg["fowt_mooring"]["result"]
        assert result["mbr_limit_m"] == pytest.approx(4.5)
        # governing bend radius comfortably exceeds the MBR limit -> passes
        assert result["governing_bend_radius_m"] > result["mbr_limit_m"]
        assert result["passes"] is True
        assert result["margin_m"] == pytest.approx(
            result["governing_bend_radius_m"] - result["mbr_limit_m"]
        )
    elif workflow["id"] == "fowt-mooring-atlas-query":
        result = cfg["parametric_query"]["result"]
        # in-range point -> served interpolated utilisation, no escalation
        assert result["in_range"] is True
        assert result["response"] == "utilisation"
        assert result["screening_status"] == "pass"
        assert 0.0 < result["value"] < 1.0
    elif workflow["id"] == "lifting-lug-atlas-query":
        result = cfg["parametric_query"]["result"]
        # in-range light-load / thick-plate point -> served utilisation, passes
        assert result["in_range"] is True
        assert result["response"] == "utilisation"
        assert result["screening_status"] == "pass"
        assert 0.0 < result["value"] < 1.0
    elif workflow["id"] == "esp-pump-atlas-query":
        result = cfg["parametric_query"]["result"]
        # in-range moderate-rate / shallow-lift point -> served utilisation, passes
        assert result["in_range"] is True
        assert result["response"] == "utilisation"
        assert result["screening_status"] == "pass"
        assert 0.0 < result["value"] < 1.0
    elif workflow["id"] == "weather-window-atlas-query":
        result = cfg["parametric_query"]["result"]
        # value class: interpolated planned-operability percentage, no verdict
        assert result["in_range"] is True
        assert result["response"] == "operability_pct"
        # operability is a percentage in (0, 100)
        assert 0.0 < result["value"] < 100.0
        lo, hi = result["confidence"]["band"]
        assert lo <= result["value"] <= hi
        assert "screening estimate" in result["disclaimer"].lower()
        assert result["provenance"]["atlas_id"]
    elif workflow["id"] == "span-rectification-atlas-query":
        result = cfg["parametric_query"]["result"]
        # in-range short-span / moderate-current point -> span within allowable,
        # served utilisation < 1 -> screen passes (no rectification needed)
        assert result["in_range"] is True
        assert result["response"] == "utilisation"
        assert result["screening_status"] == "pass"
        assert 0.0 < result["value"] < 1.0
    elif workflow["id"] == "mudmat-bearing-atlas-query":
        result = cfg["parametric_query"]["result"]
        # in-range moderate-load / firm-clay point -> served utilisation, passes
        assert result["in_range"] is True
        assert result["response"] == "utilisation"
        assert result["screening_status"] == "pass"
        assert 0.0 < result["value"] < 1.0
    elif workflow["id"] == "inspection-planning-atlas-query":
        result = cfg["parametric_query"]["result"]
        # in-range point -> served interpolated remaining life (plain value).
        # _handle_value has no screening_status; assert what it actually returns.
        assert result["in_range"] is True
        assert result["response"] == "remaining_life_years"
        assert "screening_status" not in result
        assert math.isfinite(result["value"]) and result["value"] > 0.0
        band = result["confidence"]["band"]
        assert band[0] <= result["value"] <= band[1]
    elif workflow["id"] == "riser-fatigue-atlas-query":
        result = cfg["parametric_query"]["result"]
        # annual-damage class: interpolate annual damage -> life + DFF margin.
        assert result["in_range"] is True
        assert result["response"] == "fatigue_life_years"
        assert result["annual_damage"] > 0.0
        assert result["value"] > 0.0  # fatigue life (years)
        assert result["screening_status"] in {"pass", "fail"}
    elif workflow["id"] == "ffs-metal-loss":
        # workflow-API adoption row (workspace-hub#3285): the `ffs` route parks the
        # #1066 indexed 16-key FFSAssessmentResult.to_dict() on cfg["ffs"].
        result = cfg["ffs"]
        assert result["component_id"] == "LINE-001"
        assert result["assessment_type"] == "LML"  # 2-cell deep local thin spot
        assert result["verdict"] == "ACCEPT"
        # the measurement-sufficiency catch: an L1-ACCEPT that is under-measured
        assert result["sufficiency_status"] == "TAKE_MORE"
        assert result["passes"] is True
        assert result["t_nominal_in"] == pytest.approx(0.5)
        assert result["t_measured_min_in"] == pytest.approx(0.3)
        assert result["folias_factor"] >= 1.0
        assert set(result) == {
            "component_id", "assessment_type", "level_reached", "t_nominal_in",
            "t_min_in", "t_measured_min_in", "t_measured_avg_in", "fca_in", "rsf",
            "rsf_a", "folias_factor", "remaining_life_yr", "verdict",
            "rerated_pressure_psi", "sufficiency_status", "passes", "code_reference",
        }
    elif workflow["id"] == "buckling-parametric":
        # workflow-API adoption row (workspace-hub#3285-OWNED): the
        # buckling_parametric route writes a byte-stable results.json.
        result = cfg["buckling_parametric"]
        assert result["standard"] == "DNV-RP-C201"
        assert result["n_cases"] == 8  # 2 grades x 2 thk x 1 width x 1 length x 2 loads
        results_json = Path(result["outputs"]["results_json"])
        if not results_json.is_absolute():
            results_json = REPO_ROOT / results_json
        payload = json.loads(results_json.read_text())
        assert payload["meta"]["n_cases"] == 8
        assert payload["meta"]["standard"] == "DNV-RP-C201"
        # timestamp=None => meta.generated_at omitted => byte-stable golden
        assert "generated_at" not in payload["meta"]
        assert len(payload["index"]) == 8
        assert set(payload["meta"]["grades"]) == {"Grade A", "AH36"}
    elif workflow["id"] == "mooring-design-mbl":
        # workflow-API adoption row (workspace-hub#3285): NEW `mooring_mbl` basename
        # (the reserved `mooring` arm is untouched). DNV-OS-E301 SF + citation sidecar.
        result = cfg["mooring_mbl"]
        assert result["condition"] == "intact"
        assert result["safety_factor"] == pytest.approx(1.67)
        assert set(result["results"]) == {"R4_84mm_chain", "160mm_polyester"}
        for seg in result["results"].values():
            assert seg["utilisation_with_sf"] == pytest.approx(
                seg["utilisation_no_sf"] * result["safety_factor"], rel=1e-3
            )
        citations = result["citations"]
        assert len(citations) == 1
        assert citations[0]["code_id"] == "DNV-OS-E301"
        assert citations[0]["publisher"] == "DNV"
    elif workflow["id"] in FIELD_DEV_PRODUCTION_WORKFLOWS:
        assert_field_dev_production_workflow(workflow["id"], cfg)
    elif workflow["id"] == "viv-parametric-screening":
        # Synthetic DNV-RP-C205 VIV screening base case (#1505): D=0.2032 m, V=1.0 m/s,
        # span=60 m -> cross-flow lock-in at mode 2, A/D ~= 0.98.
        res = cfg["viv_parametric_screening"]
        assert res["standard"].startswith("DNV-RP-C205")
        assert res["lock_in"] is True and res["screening_pass"] is False
        assert res["critical_mode"] == 2
        assert res["a_d_ratio"] == pytest.approx(0.9789, abs=1e-3)
        assert res["fatigue_proxy"] > 0.0
    else:
        raise AssertionError(f"Missing workflow assertion for {workflow['id']}")


def test_wellpath_minimum_curvature_textbook_case(tmp_path):
    input_path = tmp_path / "wellpath.yml"
    input_path.write_text(
        yaml.safe_dump(
            {
                "basename": "wellpath",
                "wellpath": {
                    "stations": [
                        {
                            "measured_depth_m": 0.0,
                            "inclination_deg": 0.0,
                            "azimuth_deg": 0.0,
                        },
                        {
                            "measured_depth_m": 100.0,
                            "inclination_deg": 30.0,
                            "azimuth_deg": 0.0,
                        },
                    ],
                    "output_dir": "results",
                },
                "default": {
                    "log_level": "INFO",
                    "config": {"overwrite": {"output": True}},
                },
            }
        )
    )

    cfg = engine(inputfile=str(input_path))
    survey = pd.read_csv(tmp_path / "results" / "wellpath_wellpath.csv")

    assert cfg["wellpath"]["total_depth"] == pytest.approx(100.0)
    assert survey.loc[1, "tvd"] == pytest.approx(95.4929658551)
    assert survey.loc[1, "north"] == pytest.approx(25.5872630837)
    assert survey.loc[1, "east"] == pytest.approx(0.0, abs=1.0e-12)
    assert survey.loc[1, "dls"] == pytest.approx(9.0)


def test_wave_spectrum_pierson_moskowitz_engine_textbook_check(tmp_path):
    input_path = tmp_path / "wave_spectrum.yml"
    input_path.write_text(
        yaml.safe_dump(
            {
                "basename": "wave_spectrum",
                "wave_spectrum": {
                    "spectrum": "pierson_moskowitz",
                    "Hs": 2.5,
                    "Tp": 8.0,
                    "frequency_grid": {
                        "units": "rad_s",
                        "start": 0.2,
                        "stop": 2.5,
                        "num": 231,
                    },
                    "output_dir": "results",
                },
                "default": {
                    "log_level": "INFO",
                    "config": {"overwrite": {"output": True}},
                },
            }
        )
    )

    cfg = engine(inputfile=str(input_path))
    spectrum = pd.read_csv(tmp_path / "results" / "wave_spectrum_spectrum.csv")
    omega_p = 2.0 * math.pi / 8.0

    assert cfg["wave_spectrum"]["spectrum"] == "pierson_moskowitz"
    assert cfg["wave_spectrum"]["Hs_check"] == pytest.approx(2.5, rel=0.05)
    assert cfg["wave_spectrum"]["peak_frequency"] == pytest.approx(omega_p, abs=0.02)
    assert spectrum.loc[spectrum["S"].idxmax(), "frequency"] == pytest.approx(
        cfg["wave_spectrum"]["peak_frequency"]
    )


def test_synthetic_rope_tn_curve_matches_dnv_os_e301_table_f3(tmp_path):
    """AC6 validation gate: the polyester tension-tension T-N leg reproduces the
    DNV-OS-E301 (Oct 2010) Table F3 design curve  N = aD * R^(-m), with
    aD = 0.259, m = 13.46 and R = tension range / characteristic strength (MBL).
    The curve is range-only (mean_load_knockdown = 0), so allowable_cycles must
    equal aD * R^(-m) exactly for each bin."""
    a_d, m = 0.259, 13.46
    mbl = 10000.0
    ranges = [500.0, 1000.0, 2000.0]  # R = 0.05, 0.10, 0.20
    input_path = tmp_path / "synthetic_rope.yml"
    input_path.write_text(
        yaml.safe_dump(
            {
                "basename": "synthetic_rope_mooring_fatigue",
                "synthetic_rope_mooring_fatigue": {
                    "design_life_years": 20.0,
                    "dff": 60.0,
                    "creep_safety_factor": 3.0,
                    "tn_curve": {
                        "intercept": a_d,
                        "slope": m,
                        "mean_load_knockdown": 0.0,
                    },
                    "creep": {
                        "reference_life_years": 100.0,
                        "load_ratio_ref": 0.30,
                        "decades_per_load_ratio": 6.0,
                    },
                    "min_tension": {"min_ratio_allow": 0.05},
                    "output_dir": "results",
                    "lines": [
                        {
                            "id": "polyester-val",
                            "material": "POLYESTER",
                            "MBL_kN": mbl,
                            "mean_tension_kN": 2000.0,
                            "min_tension_kN": 1000.0,
                            "tension_range_bins": [
                                {"tension_range_kN": r, "n_cycles": 1000.0}
                                for r in ranges
                            ],
                        }
                    ],
                },
                "default": {
                    "log_level": "INFO",
                    "config": {"overwrite": {"output": True}},
                },
            }
        )
    )

    engine(inputfile=str(input_path))
    results = pd.read_csv(
        tmp_path / "results" / "synthetic_rope_synthetic_rope_mooring_fatigue.csv"
    )

    for _, row in results.iterrows():
        expected_n = a_d * row["normalised_range"] ** (-m)
        assert row["allowable_cycles"] == pytest.approx(expected_n, rel=1e-9)
    # Published anchor: R = 0.10 -> N = 0.259 * 10^13.46
    anchor = results.loc[results["normalised_range"].round(6) == 0.10].iloc[0]
    assert anchor["allowable_cycles"] == pytest.approx(a_d * 10**13.46, rel=1e-9)


def test_spectral_fatigue_workflow_matches_narrow_band_closed_form(tmp_path):
    """AC6 validation gate: the spectral-fatigue workflow faithfully wires the
    validated digitalmodel.fatigue.spectral_fatigue library — the per-location
    annual damage equals a direct narrow-band (Rayleigh, Bendat 1964) library
    call on the same PSD and S-N curve."""
    from digitalmodel.fatigue.spectral_fatigue import (
        compute_spectral_moments,
        narrow_band_damage,
    )

    frequency = [0.04, 0.08, 0.10, 0.12, 0.20, 0.40]
    psd = [40.0, 900.0, 1800.0, 1100.0, 120.0, 8.0]
    sn_slope, sn_log_intercept = 3.0, 12.164

    input_path = tmp_path / "spectral.yml"
    input_path.write_text(
        yaml.safe_dump(
            {
                "basename": "spectral_fatigue",
                "spectral_fatigue": {
                    "design_life_years": 20.0,
                    "dff": 3.0,
                    "method": "narrow_band",
                    "sn_curve": {
                        "slope": sn_slope,
                        "log_intercept": sn_log_intercept,
                    },
                    "output_dir": "results",
                    "locations": [
                        {
                            "id": "detail-a",
                            "sea_states": [
                                {
                                    "occurrence_fraction": 1.0,
                                    "frequency_Hz": frequency,
                                    "stress_psd_MPa2_Hz": psd,
                                }
                            ],
                        }
                    ],
                },
                "default": {
                    "log_level": "INFO",
                    "config": {"overwrite": {"output": True}},
                },
            }
        )
    )

    cfg = engine(inputfile=str(input_path))

    moments = compute_spectral_moments(frequency, psd)
    expected = narrow_band_damage(moments, sn_slope, sn_log_intercept).damage_per_year

    location = cfg["spectral_fatigue"]["locations"][0]
    assert location["annual_damage"] == pytest.approx(expected, rel=1e-9)
    assert location["fatigue_life_years"] == pytest.approx(1.0 / expected, rel=1e-9)


def test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening(tmp_path):
    """AC6 validation gate for rao-spectral-fatigue: a *constant* stress RAO
    H(f) = g must reduce the RAO transfer S_stress(f) = |H(f)|^2 * S_wave(f) to
    the validated fixed-gain atlas screening model
    parametric.generate._spectral_fatigue_annual_damage (constant
    stress_gain_MPa_per_m). The generalisation must agree exactly with the
    special case it generalises."""
    from digitalmodel.parametric.generate import _spectral_fatigue_annual_damage

    gain = 18.0  # MPa per m of wave amplitude (constant RAO)
    hs, tp, gamma = 3.5, 10.0, 3.3
    sn_slope, sn_log_intercept = 3.0, 12.164

    input_path = tmp_path / "rao_spectral.yml"
    input_path.write_text(
        yaml.safe_dump(
            {
                "basename": "rao_spectral_fatigue",
                "rao_spectral_fatigue": {
                    "design_life_years": 25.0,
                    "dff": 3.0,
                    "method": "dirlik",
                    "sn_curve": {"slope": sn_slope, "log_intercept": sn_log_intercept},
                    "output_dir": "results",
                    "locations": [
                        {
                            "id": "detail-a",
                            # constant RAO -> H(f) = gain everywhere
                            "rao_frequency_Hz": [0.0, 0.1, 0.2, 0.4],
                            "rao_stress_MPa_per_m": [gain, gain, gain, gain],
                            "sea_states": [
                                {
                                    "occurrence_fraction": 1.0,
                                    "wave_spectrum": {
                                        "type": "jonswap",
                                        "Hs": hs,
                                        "Tp": tp,
                                        "gamma": gamma,
                                    },
                                }
                            ],
                        }
                    ],
                },
                "default": {
                    "log_level": "INFO",
                    "config": {"overwrite": {"output": True}},
                },
            }
        )
    )

    cfg = engine(inputfile=str(input_path))

    expected = _spectral_fatigue_annual_damage(
        {"Hs": hs, "Tp": tp},
        gamma=gamma,
        stress_gain_MPa_per_m=gain,
        sn_slope=sn_slope,
        sn_intercept=sn_log_intercept,
    )

    location = cfg["rao_spectral_fatigue"]["locations"][0]
    assert location["annual_damage"] == pytest.approx(expected, rel=1e-9)
    assert location["fatigue_life_years"] == pytest.approx(1.0 / expected, rel=1e-9)
    assert cfg["rao_spectral_fatigue"]["wave_to_stress_transfer"] == (
        "S_stress(f) = |H(f)|**2 * S_wave(f)"
    )


def test_vessel_seakeeping_significant_amplitude_matches_library(tmp_path):
    """AC6 validation gate for vessel-seakeeping: the workflow's per-sea-state
    significant motion amplitude must equal an independent computation through
    the validated digitalmodel.hydrodynamics.seakeeping primitives
    (compute_response_spectrum -> spectral_moments -> significant_amplitude) on
    the same RAO and JONSWAP spectrum -- i.e. the workflow faithfully wires the
    tested library, not a re-implementation."""
    import numpy as np

    from digitalmodel.hydrodynamics.seakeeping import (
        compute_response_spectrum,
        significant_amplitude,
        spectral_moments,
    )
    from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra

    freqs = [0.3, 0.5, 0.7, 0.9, 1.1]
    rao = [1.0, 0.8, 0.5, 0.25, 0.1]
    hs, tp, gamma = 3.0, 11.0, 3.3

    input_path = tmp_path / "vessel_seakeeping.yml"
    input_path.write_text(
        yaml.safe_dump(
            {
                "basename": "vessel_seakeeping",
                "vessel_seakeeping": {
                    "vessel": "unit-test",
                    "spectrum_type": "jonswap",
                    "gamma": gamma,
                    "required_operability_pct": 95.0,
                    "output_dir": "results",
                    "rao_frequency_rad_s": freqs,
                    "sea_states": [{"hs": hs, "tp": tp, "probability": 1.0}],
                    "dofs": [
                        {
                            "name": "heave",
                            "unit": "m",
                            "rao_amplitude": rao,
                            "criterion": 2.0,
                        }
                    ],
                },
                "default": {
                    "log_level": "INFO",
                    "config": {"overwrite": {"output": True}},
                },
            }
        )
    )

    cfg = engine(inputfile=str(input_path))

    # independent recomputation through the library primitives
    rao_freqs = np.asarray(freqs, dtype=float)
    omega, s_wave = WaveSpectra().jonswap(
        hs=hs,
        tp=tp,
        gamma=gamma,
        freq_min=float(rao_freqs[0]),
        freq_max=float(rao_freqs[-1]),
        n_points=len(rao_freqs),
    )
    s_resp = compute_response_spectrum(np.asarray(rao, dtype=float), s_wave)
    m0 = spectral_moments(omega, s_resp, orders=[0])[0]
    expected_sig = significant_amplitude(m0)

    results_path = Path(cfg["vessel_seakeeping"]["results_csv"])
    if not results_path.is_absolute():
        results_path = REPO_ROOT / results_path
    results = pd.read_csv(results_path)
    heave_row = results[results["dof"] == "heave"].iloc[0]
    assert heave_row["significant_amplitude"] == pytest.approx(expected_sig, rel=1e-9)
    assert cfg["vessel_seakeeping"]["response_transfer"] == (
        "S_response(w) = |RAO(w)|^2 * S_wave(w)"
    )


def test_mudmat_bearing_capacity_factors_match_brinch_hansen():
    """AC6 validation gate for mudmat-bearing-capacity: the bearing-capacity
    factors reproduce the closed-form Brinch Hansen / DNV-RP-C212 expressions,
    and a drained strip-footing q_ult equals a direct hand computation through
    those factors and the general bearing-capacity equation."""
    from digitalmodel.geotechnical.mudmat import (
        bearing_capacity_factors,
        mudmat_bearing_capacity,
    )

    # undrained limit: Nc = 2 + pi, Nq = 1, Ngamma = 0
    nc0, nq0, ng0 = bearing_capacity_factors(0.0)
    assert nc0 == pytest.approx(2.0 + math.pi)
    assert nq0 == pytest.approx(1.0)
    assert ng0 == pytest.approx(0.0)

    # drained: closed-form factors at phi = 30 deg
    phi = 30.0
    nc, nq, ng = bearing_capacity_factors(phi)
    phir = math.radians(phi)
    exp_nq = math.exp(math.pi * math.tan(phir)) * math.tan(math.pi / 4 + phir / 2) ** 2
    assert nq == pytest.approx(exp_nq, rel=1e-12)
    assert nc == pytest.approx((exp_nq - 1.0) / math.tan(phir), rel=1e-12)
    assert ng == pytest.approx(1.5 * (exp_nq - 1.0) * math.tan(phir), rel=1e-12)

    # drained square footing, cohesionless: q_ult reproduced by hand from the
    # general equation with the workflow's own shape/depth factors.
    res = mudmat_bearing_capacity(
        width_b_m=5.0,
        length_l_m=5.0,
        embedment_depth_m=1.0,
        condition="drained",
        submerged_unit_weight_kn_m3=9.0,
        vertical_load_kn=1000.0,
        friction_angle_deg=phi,
        effective_cohesion_kpa=0.0,
    )
    sc, sq, sg = (res.shape_factors[k] for k in ("sc", "sq", "sgamma"))
    dc, dq, dg = (res.depth_factors[k] for k in ("dc", "dq", "dgamma"))
    p0 = 9.0 * 1.0
    expected = (
        0.0 * nc * sc * dc
        + p0 * nq * sq * dq
        + 0.5 * 9.0 * res.effective_width_m * ng * sg * dg
    )
    assert res.q_ult_kpa == pytest.approx(expected, rel=1e-12)


def test_lifting_lug_checks_match_closed_form():
    """AC6 validation gate for lifting-lug-design: the bearing, net-tension and
    shear tear-out demands equal the closed-form AISC padeye expressions, and
    the allowables are the AISC factors on yield (0.90 / 0.60 / 0.40 Fy)."""
    from digitalmodel.lifting_lug.workflow import design_load_kn, lug_checks

    fy = 355.0
    t = 40.0
    d_pin, d_hole, r_outer = 75.0, 80.0, 110.0
    p = design_load_kn(420.0, 2.0, 1.1)
    assert p == pytest.approx(924.0)

    checks = {c.name: c for c in lug_checks(p, t, d_pin, d_hole, r_outer, fy)}
    p_n = p * 1.0e3
    ligament = r_outer - d_hole / 2.0

    assert checks["pin_bearing"].demand_mpa == pytest.approx(
        p_n / (d_pin * t), rel=1e-12
    )
    assert checks["pin_bearing"].allowable_mpa == pytest.approx(0.90 * fy, rel=1e-12)
    assert checks["net_tension"].demand_mpa == pytest.approx(
        p_n / (2.0 * t * ligament), rel=1e-12
    )
    assert checks["net_tension"].allowable_mpa == pytest.approx(0.60 * fy, rel=1e-12)
    assert checks["shear_tearout"].demand_mpa == pytest.approx(
        p_n / (2.0 * t * ligament), rel=1e-12
    )
    assert checks["shear_tearout"].allowable_mpa == pytest.approx(0.40 * fy, rel=1e-12)
    # utilisation = demand / allowable
    for c in checks.values():
        assert c.utilization == pytest.approx(c.demand_mpa / c.allowable_mpa, rel=1e-12)


def test_inspection_planning_half_life_rule_closed_form():
    """AC6 validation gate for inspection-planning: remaining life and the
    next-inspection interval reproduce the closed-form API 510/570/653 rules,
    including the half-life cap and the t<=t_min repair flag."""
    from digitalmodel.asset_integrity.inspection_planning import (
        corrosion_rate_mm_yr,
        plan_inspection,
    )

    # corrosion rate from two readings
    assert corrosion_rate_mm_yr(11.0, 8.5, 5.0) == pytest.approx(0.5)

    # half-life governs (remaining life / 2 < code max)
    p = plan_inspection(
        current_mm=8.5,
        required_mm=6.0,
        code_max_interval_years=10.0,
        previous_mm=11.0,
        years_between=5.0,
    )
    assert p.corrosion_rate_mm_yr == pytest.approx(0.5)
    assert p.remaining_life_years == pytest.approx((8.5 - 6.0) / 0.5)
    assert p.half_life_interval_years == pytest.approx(p.remaining_life_years / 2.0)
    assert p.next_inspection_interval_years == pytest.approx(
        min(p.remaining_life_years / 2.0, 10.0)
    )

    # code maximum governs when half the remaining life exceeds it
    p2 = plan_inspection(
        current_mm=20.0,
        required_mm=6.0,
        code_max_interval_years=10.0,
        corrosion_rate=0.1,
    )
    assert p2.remaining_life_years == pytest.approx(140.0)
    assert p2.next_inspection_interval_years == pytest.approx(10.0)  # capped
    assert p2.screening_status == "pass"

    # already below t_min -> immediate repair flag
    p3 = plan_inspection(
        current_mm=5.5,
        required_mm=6.0,
        code_max_interval_years=10.0,
        corrosion_rate=0.2,
    )
    assert p3.next_inspection_interval_years == 0.0
    assert p3.screening_status == "fail"


def test_span_rectification_support_count_closed_form():
    """AC6 validation gate for span-rectification: the equal-sub-span support
    count reproduces the closed-form rule n_supports = ceil(L_a/L_allow) - 1,
    every resulting sub-span is within the allowable, and L_a <= L_allow needs
    no rectification."""
    from digitalmodel.subsea.pipeline.span_rectification import design_rectification

    # over-long span: 60 m vs 12.378 m allowable -> 5 sub-spans, 4 supports
    allowable = 12.378282602855869
    r = design_rectification(60.0, allowable)
    assert r["rectification_required"] is True
    assert r["sub_span_count"] == math.ceil(60.0 / allowable)  # = 5
    assert r["n_supports"] == r["sub_span_count"] - 1  # = 4
    assert r["resulting_sub_span_m"] == pytest.approx(60.0 / r["sub_span_count"])
    assert r["resulting_sub_span_m"] <= allowable + 1e-9

    # exact multiple: 24 m vs 12 m -> 2 sub-spans, 1 support, sub-span == allowable
    r2 = design_rectification(24.0, 12.0)
    assert r2["sub_span_count"] == 2
    assert r2["n_supports"] == 1
    assert r2["resulting_sub_span_m"] == pytest.approx(12.0)

    # within allowable: no rectification
    r3 = design_rectification(10.0, 12.0)
    assert r3["rectification_required"] is False
    assert r3["n_supports"] == 0
    assert r3["resulting_sub_span_m"] == pytest.approx(10.0)


def test_liquefaction_seed_idriss_closed_form():
    """AC6 validation gate for liquefaction-triggering: rd, MSF, CSR, CRR7.5 and
    the factor of safety reproduce the closed-form Seed-Idriss / Youd 2001
    expressions, and (N1)60cs >= 30 is treated as non-liquefiable."""
    from digitalmodel.geotechnical.liquefaction import (
        assess_liquefaction,
        cyclic_resistance_ratio_75,
        cyclic_stress_ratio,
        magnitude_scaling_factor,
        stress_reduction_factor,
    )

    # rd piecewise (Liao & Whitman 1986)
    assert stress_reduction_factor(3.0) == pytest.approx(1.0 - 0.00765 * 3.0)
    assert stress_reduction_factor(15.0) == pytest.approx(1.174 - 0.0267 * 15.0)
    # MSF = 174 / Mw^2.56
    assert magnitude_scaling_factor(7.0) == pytest.approx(174.0 / 7.0**2.56)
    # CSR closed form
    rd = stress_reduction_factor(3.0)
    assert cyclic_stress_ratio(0.25, 54.0, 34.0, rd) == pytest.approx(
        0.65 * 0.25 * (54.0 / 34.0) * rd
    )
    # CRR7.5 closed form for N=8
    n = 8.0
    expected_crr = (
        1.0 / (34.0 - n) + n / 135.0 + 50.0 / (10.0 * n + 45.0) ** 2 - 1.0 / 200.0
    )
    assert cyclic_resistance_ratio_75(n) == pytest.approx(expected_crr)
    # (N1)60cs >= 30 -> non-liquefiable cap
    assert cyclic_resistance_ratio_75(35.0) == pytest.approx(2.0)

    # full layer FS = (CRR/CSR) * MSF
    res = assess_liquefaction(
        pga_g=0.25,
        magnitude=7.0,
        layers=[
            {
                "depth_m": 3.0,
                "sigma_v_kpa": 54.0,
                "sigma_v_eff_kpa": 34.0,
                "N1_60cs": 8.0,
            }
        ],
        required_factor_of_safety=1.2,
    )
    layer = res.layers[0]
    expected_fs = (expected_crr / layer.csr) * magnitude_scaling_factor(7.0)
    assert layer.factor_of_safety == pytest.approx(expected_fs, rel=1e-12)
    assert layer.liquefies is True
    assert res.screening_status == "fail"


def test_weather_window_persistence_and_fit_closed_form():
    """AC6 validation gate for weather-window: window extraction, workability and
    the window-fit verdict match a direct hand computation, and the workflow
    reuses the tested analyse_persistence for the window statistics."""
    import numpy as np

    from digitalmodel.orcaflex.weather_window import analyse_persistence
    from digitalmodel.weather_window.workflow import (
        assess_weather_window,
        window_lengths_hours,
    )

    dt = 3.0
    limit = 2.0
    # below-limit runs of 3 and 2 steps separated by an above-limit step
    series = [1.0, 1.0, 1.0, 3.0, 1.5, 1.5, 3.0]
    windows = window_lengths_hours(series, limit, dt)
    assert windows == [9.0, 6.0]  # 3*3 h and 2*3 h

    res = assess_weather_window(
        series, hs_limit=limit, required_duration_hours=9.0, time_step_hours=dt
    )
    # workability = 5 below-limit steps / 7 total
    assert res["workability_pct"] == pytest.approx(100.0 * 5.0 / 7.0)
    assert res["longest_window_hours"] == pytest.approx(9.0)
    assert res["num_viable_windows"] == 1  # only the 9 h window meets 9 h
    assert res["operation_fits_window"] is True
    assert res["screening_status"] == "pass"

    # reuses analyse_persistence: window stats agree with a direct library call
    persistence = analyse_persistence(np.asarray(series, dtype=float), limit, dt)
    assert res["longest_window_hours"] == pytest.approx(persistence.max_window_hours)
    assert res["num_windows"] == persistence.num_windows

    # a longer required duration than any window -> fail
    res2 = assess_weather_window(
        series, hs_limit=limit, required_duration_hours=12.0, time_step_hours=dt
    )
    assert res2["operation_fits_window"] is False
    assert res2["screening_status"] == "fail"


def test_esp_pump_hydraulics_tdh_and_sizing_closed_form():
    """AC6 validation gate for esp-pump-hydraulics: the Hazen-Williams friction
    head, the total dynamic head decomposition, the stage count (ceil rule) and
    the brake power reproduce their closed-form expressions."""
    from digitalmodel.production_engineering.esp_pump_hydraulics import (
        hazen_williams_head_m,
        size_esp,
    )

    # Hazen-Williams closed form: hf = 10.67 L Q^1.852 / (C^1.852 d^4.87)
    q = 1200.0 / 86400.0
    expected_hf = 10.67 * 2500.0 * q**1.852 / (120.0**1.852 * 0.076**4.87)
    assert hazen_williams_head_m(1200.0, 2500.0, 0.076, 120.0) == pytest.approx(
        expected_hf, rel=1e-12
    )

    r = size_esp(
        flow_rate_m3_per_day=1200.0,
        dynamic_fluid_level_m=1800.0,
        pump_setting_depth_m=2500.0,
        wellhead_pressure_kpa=2000.0,
        specific_gravity=0.85,
        tubing_inner_diameter_m=0.076,
        head_per_stage_m=6.0,
        bhp_per_stage_hp=0.5,
        max_stages=400,
        motor_rating_hp=250.0,
        min_rate_m3_per_day=600.0,
        max_rate_m3_per_day=2000.0,
        hazen_williams_c=120.0,
    )
    expected_discharge = 2000.0 * 1.0e3 / (0.85 * 1000.0 * 9.81)
    expected_tdh = 1800.0 + expected_hf + expected_discharge
    assert r.friction_head_m == pytest.approx(expected_hf, rel=1e-12)
    assert r.discharge_head_m == pytest.approx(expected_discharge, rel=1e-12)
    assert r.total_dynamic_head_m == pytest.approx(expected_tdh, rel=1e-12)
    assert r.stages_required == math.ceil(expected_tdh / 6.0)
    assert r.brake_power_hp == pytest.approx(r.stages_required * 0.5 * 0.85, rel=1e-12)
    assert r.screening_status == "fail"  # 405 stages > 400 housing
