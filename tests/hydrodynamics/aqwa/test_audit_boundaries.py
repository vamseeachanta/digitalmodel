"""Adversarial boundary cases for damping and gate provenance."""
from pathlib import Path
import hashlib
import tomllib

import numpy as np
import pytest

from digitalmodel.hydrodynamics.aqwa.damping_physics import critical_dof
from digitalmodel.hydrodynamics.aqwa import qa_gates
from tests.hydrodynamics.aqwa.test_audit_workflow import case


def test_frequency_dependent_mass_solves_between_samples():
    # E4: I=2, A(w)=2w, C=.75 gives w=.5, mass=3.
    # E5: interpolated B=6, critical B=3, zeta=2.
    result = critical_dof(np.array([.2, .8]), [2., 10.], 2., .75, [.4, 1.6], "roll")
    assert result["natural_frequency_rad_s"] == pytest.approx(.5)
    assert result["damping_at_natural_frequency"] == pytest.approx(6.)
    assert result["zeta_at_natural_frequency"] == pytest.approx(2.)


@pytest.mark.parametrize("stiffness,added", [(1000., [1., 1., 1.]), (3., [99., 0., 99.])])
def test_outside_or_multiple_resonances_raise(stiffness, added):
    with pytest.raises(ValueError, match="resonance"):
        critical_dof(np.array([.2, .5, .8]), [1., 1., 1.], 1., stiffness, added, "roll")


def test_zero_dat_inertia_falls_back_per_dof(tmp_path):
    module, dataset, config = case(tmp_path)
    dat = dataset["manifest"][0]["dat"]
    with dat.open("a") as stream:
        stream.write("\nDECK 4\n0 0 0 0 0 0\n")
    record = module._process_case(dataset, config)["records"][0]
    assert record["inertia"]["I44"] == 1e9
    assert record["inertia"]["I55"] == 1e9


def test_report_rejects_changed_source(tmp_path):
    module, dataset, config = case(tmp_path)
    result = module._process_case(dataset, config)
    dataset["manifest"][0]["dat"].write_text("tampered")
    with pytest.raises(ValueError, match="digest"):
        module._generate_case_reports(result, tmp_path)


def test_foreign_manual_gate_inputs_rejected(tmp_path):
    artifact = tmp_path / "evidence.json"
    artifact.write_text("synthetic")
    gate = "AQ-05"
    record = {"tool": qa_gates.EXTERNAL_TOOLS[gate], "tool_version": "test",
              "reviewer_role": "maintainer", "decision": "PASS", "criteria_revision": "AQG-r0",
              "artifact": str(artifact), "sha256": hashlib.sha256(artifact.read_bytes()).hexdigest(),
              "input_sha256": {"dat": "a"*64},
              "checks": {key: True for key in qa_gates.EXTERNAL_CHECKS[gate]}}
    with pytest.raises(ValueError, match="input"):
        qa_gates.validate_external_gate(gate, record, expected_inputs={"dat": "b"*64})


def test_citation_basis_is_packaged():
    root = Path(__file__).resolve().parents[3]
    config = tomllib.loads((root / "pyproject.toml").read_text())
    patterns = config["tool"]["setuptools"]["package-data"]["digitalmodel"]
    assert "hydrodynamics/aqwa/docs/damping-basis.md" in patterns


def test_reader_cache_refreshes_changed_bytes(tmp_path):
    from digitalmodel.marine_ops.marine_analysis.aqwa_enhanced_parser import AQWAEnhancedParser
    path = tmp_path / "input.lis"
    path.write_text("first")
    parser = AQWAEnhancedParser()
    assert parser._read_file_content(str(path)) == "first"
    path.write_text("second")
    assert parser._read_file_content(str(path)) == "second"


def test_missing_dof_coefficient_raises(tmp_path):
    module, dataset, config = case(tmp_path)
    del config["hydrostatics"]["inertia"]["I55"]
    with pytest.raises(ValueError, match="I55"):
        module._process_case(dataset, config)


def test_report_separates_resonance_from_grid_diagnostics(tmp_path):
    import pandas as pd
    module, dataset, config = case(tmp_path)
    result = module._process_case(dataset, config)
    module._generate_case_reports(result, tmp_path)
    module._aggregate_case_summaries([result], tmp_path)
    table = pd.read_csv(tmp_path / "case/data/record_critical.csv")
    assert table["zeta_roll_at_natural_frequency_inf"].iloc[0] == pytest.approx(0.0102062073)
    assert table["natural_frequency_roll_inf_rad_s"].iloc[0] == pytest.approx(0.40824829)
    assert "diagnostic_grid_zeta_roll_inf" in table
    summary = pd.read_csv(tmp_path / "viscous_damping_master_summary.csv")
    assert "max_zeta_roll" not in summary
    assert summary["zeta_roll_at_natural_frequency"].iloc[0] == pytest.approx(0.0102062073)


def test_empty_lis_table_is_an_error(tmp_path):
    from digitalmodel.hydrodynamics.aqwa.aqwa_lis_files import AqwaLISFiles
    from digitalmodel.marine_ops.marine_analysis.aqwa_enhanced_parser import AQWAFileError
    path = tmp_path / "empty.lis"
    path.write_text("R.A.O.S-VARIATION WITH WAVE DIRECTION\n")
    with pytest.raises(AQWAFileError, match="no data"):
        AqwaLISFiles().get_data(
            {"file_management": {"input_files": {"LIS": [str(path)]}}},
            {"category": "displacement_raos"})
