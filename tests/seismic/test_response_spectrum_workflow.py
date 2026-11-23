from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from digitalmodel.engine import engine
from digitalmodel.reporting import ProvenanceError
from digitalmodel.seismic.readers.ascii import AccelerogramRecord
from digitalmodel.seismic.response_spectrum.calculations import compute_kinematics
from digitalmodel.seismic.response_spectrum.render import render_report
from digitalmodel.seismic.response_spectrum.workflow import router


def _record() -> AccelerogramRecord:
    return AccelerogramRecord(
        time_s=np.array([0.0, 1.0, 2.0]),
        acceleration_m_s2=np.array([0.0, 1.0, 1.0]),
        units="m_s2",
        source_path=Path("synthetic.txt"),
        source_digest="synthetic-digest",
    )


def test_compute_kinematics_keeps_eom_solver_out_of_pr1_scope():
    result = compute_kinematics(
        _record(),
        baseline={"method": "none"},
        frequency_filter={"enabled": False},
    )

    assert result.method == "kinematic_cumtrapz"
    assert result.summary["npts"] == 3
    assert result.summary["pga_m_s2"] == pytest.approx(1.0)
    assert "pgv_m_s" not in result.summary
    assert "pgd_m" not in result.summary
    np.testing.assert_allclose(result.timeseries["velocity_m_s"], [0.0, 0.5, 1.5])
    np.testing.assert_allclose(
        result.timeseries["displacement_m"],
        [0.0, 0.25, 1.25],
    )


def test_report_requires_declared_data_source(tmp_path):
    result = compute_kinematics(
        _record(),
        baseline={"method": "none"},
        frequency_filter={"enabled": False},
    )

    with pytest.raises(ProvenanceError):
        render_report(result, output_path=tmp_path / "report.html", data_source=None)


def test_router_writes_timeseries_and_provenance_report(tmp_path):
    record_path = tmp_path / "motion.txt"
    record_path.write_text("0.0 0.0\n1.0 1.0\n2.0 1.0\n", encoding="utf-8")
    input_path = tmp_path / "input.yml"
    cfg = {
        "basename": "response_spectrum",
        "_config_file_path": str(input_path),
        "response_spectrum": {
            "record": {"file": str(record_path), "units": "m_s2"},
            "baseline": {"method": "none"},
            "filter": {"enabled": False},
            "output_dir": "results",
            "generated_label": "pytest deterministic label",
        },
    }

    routed = router(cfg)

    summary = routed["response_spectrum"]["summary"]
    assert routed["screening_status"] == "pass"
    assert summary["npts"] == 3
    assert summary["pga_m_s2"] == pytest.approx(1.0)

    timeseries_path = tmp_path / "results" / "input_timeseries.csv"
    report_path = tmp_path / "results" / "input_report.html"
    assert timeseries_path.exists()
    assert report_path.exists()
    timeseries = pd.read_csv(timeseries_path)
    assert timeseries["velocity_m_s"].tolist() == pytest.approx([0.0, 0.5, 1.5])
    html = report_path.read_text(encoding="utf-8")
    assert "Data provenance" in html
    assert "Kinematic acceleration integration" in html
    assert "PGV" not in html
    assert "PGD" not in html
    assert html.find("<script") >= 0
    assert html.find("cdn.plot") == -1
    assert str(tmp_path) not in html
    assert "motion.txt" in html


def test_engine_dispatches_registered_response_spectrum_workflow(tmp_path):
    data_dir = tmp_path / "data"
    data_dir.mkdir()
    (data_dir / "motion.txt").write_text("0.0 0.0\n1.0 1.0\n2.0 1.0\n", encoding="utf-8")
    input_path = tmp_path / "input.yml"
    input_path.write_text(
        """
basename: response_spectrum
response_spectrum:
  record:
    file: data/motion.txt
    units: m_s2
  baseline:
    method: none
  filter:
    enabled: false
  output_dir: results
  generated_label: pytest deterministic label
default:
  log_level: INFO
  config:
    overwrite:
      output: true
""",
        encoding="utf-8",
    )

    cfg = engine(inputfile=str(input_path))

    assert cfg["basename"] == "response_spectrum"
    assert cfg["response_spectrum"]["summary"]["pga_m_s2"] == pytest.approx(1.0)
    assert (tmp_path / "results" / "input_timeseries.csv").exists()
    assert (tmp_path / "results" / "input_report.html").exists()
