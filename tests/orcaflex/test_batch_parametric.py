"""Tests for digitalmodel.orcaflex.batch_parametric module."""

import pytest

from digitalmodel.orcaflex.batch_parametric import (
    CaseResult,
    ParameterSweep,
    ParametricResultsSummary,
    ParametricStudy,
)


class TestParameterSweep:
    """Tests for parameter sweep definition."""

    def test_explicit_values(self):
        """Explicit values should be returned as-is."""
        ps = ParameterSweep(name="hs", values=[1.0, 2.0, 3.0])
        assert ps.get_values() == [1.0, 2.0, 3.0]

    def test_range_values(self):
        """Range should generate correct values."""
        ps = ParameterSweep(name="heading", start=0, stop=360, step=90)
        vals = ps.get_values()
        assert len(vals) == 5  # 0, 90, 180, 270, 360
        assert vals[0] == pytest.approx(0.0)
        assert vals[-1] == pytest.approx(360.0)

    def test_missing_values_raises(self):
        """Missing both values and range should raise."""
        ps = ParameterSweep(name="bad")
        with pytest.raises(ValueError, match="provide either"):
            ps.get_values()

    def test_single_value(self):
        """Single value sweep should work."""
        ps = ParameterSweep(name="single", values=[42.0])
        assert ps.get_values() == [42.0]


class TestParametricStudy:
    """Tests for parametric study framework."""

    def test_case_matrix_size(self):
        """Full-factorial matrix size should be product of param lengths."""
        study = ParametricStudy(
            name="test",
            parameters=[
                ParameterSweep(name="hs", values=[1.0, 2.0, 3.0]),
                ParameterSweep(name="heading", values=[0, 90, 180, 270]),
            ],
        )
        assert study.num_cases == 12
        df = study.generate_case_matrix()
        assert len(df) == 12
        assert "case_id" in df.columns

    def test_case_configs_have_parameters(self):
        """Each case config should contain parameter values."""
        study = ParametricStudy(
            parameters=[
                ParameterSweep(name="wave_hs", values=[2.0, 4.0]),
            ],
            base_config={"model": "test_model"},
        )
        configs = study.generate_case_configs()
        assert len(configs) == 2
        assert configs[0]["wave_hs"] in [2.0, 4.0]
        assert configs[0]["model"] == "test_model"

    def test_single_parameter_study(self):
        """Single parameter sweep should work."""
        study = ParametricStudy(
            parameters=[ParameterSweep(name="current", values=[0.5, 1.0, 1.5])],
        )
        df = study.generate_case_matrix()
        assert len(df) == 3

    def test_export_csv(self, tmp_path):
        """CSV export should create a file."""
        study = ParametricStudy(
            parameters=[ParameterSweep(name="hs", values=[1.0, 2.0])],
            output_dir=str(tmp_path),
        )
        filepath = study.export_case_matrix_csv()
        import os
        assert os.path.exists(filepath)


class TestParametricResults:
    """Tests for parametric results summary."""

    def test_to_dataframe(self):
        """Results should convert to DataFrame."""
        summary = ParametricResultsSummary(
            results=[
                CaseResult(case_id="c001", parameters={"hs": 2.0}, max_tension_kN=500.0, status="completed"),
                CaseResult(case_id="c002", parameters={"hs": 4.0}, max_tension_kN=800.0, status="completed"),
            ],
        )
        df = summary.to_dataframe()
        assert len(df) == 2
        assert "max_tension_kN" in df.columns

    def test_critical_case(self):
        """Critical case should have highest metric value."""
        summary = ParametricResultsSummary(
            results=[
                CaseResult(case_id="c001", max_tension_kN=500.0, status="completed"),
                CaseResult(case_id="c002", max_tension_kN=900.0, status="completed"),
                CaseResult(case_id="c003", max_tension_kN=700.0, status="completed"),
            ],
        )
        critical = summary.get_critical_case("max_tension_kN")
        assert critical is not None
        assert critical.case_id == "c002"

    def test_summary_statistics(self):
        """Summary stats should include min, max, mean, std."""
        summary = ParametricResultsSummary(
            results=[
                CaseResult(case_id="c001", max_tension_kN=500.0, status="completed"),
                CaseResult(case_id="c002", max_tension_kN=800.0, status="completed"),
            ],
        )
        stats = summary.summary_statistics()
        assert "max_tension_kN" in stats
        assert stats["max_tension_kN"]["min"] == 500.0
        assert stats["max_tension_kN"]["max"] == 800.0
