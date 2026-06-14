import math
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.engine import engine


def test_elastic_buckling_workflow_matches_euler_column(tmp_path):
    length = 3.0
    elements = 8
    youngs_modulus = 2.10e11
    inertia = 8.10e-6
    area = 1.0e-2
    euler_load = math.pi**2 * youngs_modulus * inertia / length**2
    nodes = [
        {"id": index, "x": length * index / elements, "y": 0.0}
        for index in range(elements + 1)
    ]
    beam_elements = [
        {
            "id": index,
            "node1": index,
            "node2": index + 1,
            "E": youngs_modulus,
            "I": inertia,
            "A": area,
        }
        for index in range(elements)
    ]
    cfg = {
        "basename": "elastic_buckling",
        "elastic_buckling": {
            "nodes": nodes,
            "elements": beam_elements,
            "boundary_conditions": {"pinned_nodes": [0, elements]},
            "loads": [{"node": elements, "direction": "x", "magnitude": -1.0}],
            "material": {
                "E": youngs_modulus,
                "nu": 0.3,
                "rho": 7850.0,
                "sigma_y": 4.48e8,
            },
            "axial_force": 1.0,
            "num_modes": 3,
            "reference": {
                "effective_length_factor": 1.0,
                "euler_tolerance": 0.05,
            },
            "output_dir": "results",
        },
        "default": {"log_level": "INFO", "config": {"overwrite": {"output": True}}},
    }
    input_path = tmp_path / "case.yml"
    input_path.write_text(yaml.safe_dump(cfg, sort_keys=False))

    result = engine(inputfile=str(input_path))

    summary = result["elastic_buckling"]
    csv_path = Path(summary["results_csv"])
    if not csv_path.is_absolute():
        csv_path = Path.cwd() / csv_path
    mode_results = pd.read_csv(csv_path)

    assert csv_path.resolve() == (
        tmp_path / "results" / "case_elastic_buckling.csv"
    ).resolve()
    assert summary["status"] == "completed"
    assert summary["num_modes"] == 3
    assert summary["first_critical_load"] > 0.0
    assert summary["euler_reference_load"] == pytest.approx(euler_load)
    assert summary["first_critical_load"] == pytest.approx(euler_load, rel=0.05)

    assert len(mode_results) == 3
    assert mode_results["critical_load"].iloc[0] == pytest.approx(
        summary["first_critical_load"]
    )
    assert mode_results["critical_load"].iloc[1] > mode_results["critical_load"].iloc[0]
    assert mode_results["critical_load"].iloc[1] == pytest.approx(
        4.0 * euler_load,
        rel=0.05,
    )
    assert not math.isclose(
        mode_results["critical_load"].iloc[0],
        mode_results["critical_load"].iloc[1],
    )
