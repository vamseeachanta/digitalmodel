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
    elif workflow["id"] == "plate-buckling":
        result = cfg["plate_buckling"][0]
        assert result["dnv_rp_usage_factor"]["usage_longtudinal"] == pytest.approx(
            0.1578
        )
        assert result["dnv_rp_usage_factor"]["usage_bi_axial_with_shear"] == (
            pytest.approx(0.236)
        )
        assert result["usage_factor_ultimate_check"]["usage_equivalent"] < 1
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
    else:
        raise AssertionError(f"Missing workflow assertion for {workflow['id']}")
