from pathlib import Path

import pytest
import yaml


FIXTURES_DIR = Path(__file__).parent / "fixtures"


def _workflow_cfg(tmp_path: Path, solver: str) -> dict:
    return {
        "basename": "diffraction",
        "_config_dir_path": str(FIXTURES_DIR),
        "Analysis": {"result_folder": str(tmp_path / "results")},
        "diffraction": {
            "operation": "convert_spec",
            "spec": "spec_ship_raos.yml",
            "solver": solver,
            "format": "single",
            "output_directory": "prepared",
        },
    }


def test_diffraction_workflow_generates_orcawave_input(tmp_path: Path) -> None:
    from digitalmodel.hydrodynamics.diffraction.workflow import (
        DiffractionWorkflow,
    )

    cfg = DiffractionWorkflow().router(_workflow_cfg(tmp_path, "orcawave"))

    generated = Path(cfg["diffraction"]["outputs"]["orcawave"])
    data = yaml.safe_load(generated.read_text())
    body = data["Bodies"][0]

    assert generated == tmp_path / "results" / "prepared" / "Ship_001.yml"
    assert data["WaterDepth"] == pytest.approx(500.0)
    assert data["WaterDensity"] == pytest.approx(1.025)
    assert len(data["PeriodOrFrequency"]) == 15
    assert data["PeriodOrFrequency"][0] == pytest.approx(4.1888)
    assert data["WaveHeading"] == [0.0, 45.0, 90.0, 135.0, 180.0]
    assert body["BodyName"] == "Ship_001"
    assert body["BodyMeshFileName"] == "sample_box.gdf"


def test_diffraction_workflow_generates_aqwa_deck(tmp_path: Path) -> None:
    from digitalmodel.hydrodynamics.diffraction.workflow import (
        DiffractionWorkflow,
    )

    cfg = DiffractionWorkflow().router(_workflow_cfg(tmp_path, "aqwa"))

    generated = Path(cfg["diffraction"]["outputs"]["aqwa"])
    content = generated.read_text()

    assert generated == tmp_path / "results" / "prepared" / "test_ship_raos.dat"
    assert "JOB AQWA  LINE" in content
    assert "      DPTH       500" in content
    assert content.count("HRTZ") == 15
    assert content.count("QPPL") == 5
    assert "could not be loaded" not in content
