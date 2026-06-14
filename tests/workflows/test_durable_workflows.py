import math
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine


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
def test_workflow_registry(workflow):
    input_path = REPO_ROOT / workflow["input"]
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
            (REPO_ROOT / "examples/workflows/viv-parametric/results/case_0.yml")
            .read_text()
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
        assert result["fully_restrained_axial_force"]["L=0"] == pytest.approx(
            -253.575
        )
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
        assert results["diagnostic_message"].startswith(
            "Classification: PUMP_TAGGING."
        )
        assert results["pump_fillage"] == pytest.approx(75.909653)
        assert results["inferred_production"] == pytest.approx(338.041470)

        html_report = Path(cfg["outputs"]["html_report"])
        if not html_report.is_absolute():
            html_report = REPO_ROOT / html_report
        html = html_report.read_text()
        assert "Pump Tagging" in html
        assert "SIM-PUMP-TAGGING-711" in html
    elif workflow["id"] == "orcaflex-6dbuoy-dnvrph103":
        props = cfg["code_dnvrph103"]["properties"]
        translational = props["translational"]
        assert translational["ca"]["x"] == pytest.approx(0.703132)
        assert translational["area_drag"]["x"] == pytest.approx(0.427716)
        assert props["I"]["Ix"] == pytest.approx(19.471869)

        model_path = (
            REPO_ROOT
            / "examples/workflows/orcaflex-6dbuoy-dnvrph103/results/"
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
        assert result["environmental_loads"]["total_force"] == pytest.approx(
            2280.94225
        )

        summary_path = Path(cfg["outputs"]["summary_json"])
        if not summary_path.is_absolute():
            summary_path = REPO_ROOT / summary_path
        summary_json = yaml.safe_load(summary_path.read_text())
        assert summary_json["summary"]["critical_line"]["line_id"] == "ML1"
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
    elif workflow["id"] == "rudder-stock-torque":
        row = cfg["naval_arch"]["rudder_stock_torque"]["result"]["rows"][0]
        assert cfg["naval_arch"]["calculation"] == "rudder_stock_torque"
        assert len(cfg["naval_arch"]["rudder_stock_torque"]["result"]["rows"]) == 1
        assert row["speed_kn"] == pytest.approx(12.0)
        assert row["rudder_angle_deg"] == pytest.approx(35.0)
        assert row["scalar_normal_force_N"] == pytest.approx(722870.905140)
        assert row["hydrodynamic_rudder_stock_torque_Nm"] == pytest.approx(542153.178855)
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
    else:
        raise AssertionError(f"Missing workflow assertion for {workflow['id']}")
