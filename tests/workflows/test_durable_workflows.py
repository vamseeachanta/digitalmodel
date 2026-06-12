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
        assert cp["current_demand_A"]["totals"]["mean"] == pytest.approx(
            196.667146
        )
        assert cp["anode_requirements"]["total_mass_kg"] == pytest.approx(
            5067.071173
        )
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
        assert shedding.loc[0, "shredding_frequency_inline"] == pytest.approx(
            0.564706
        )
        assert safety.loc[0, "safety_factor_inline"] == pytest.approx(4.646130)
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
    else:
        raise AssertionError(f"Missing workflow assertion for {workflow['id']}")
