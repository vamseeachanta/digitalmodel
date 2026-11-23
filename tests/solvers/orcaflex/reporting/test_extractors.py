"""
Unit tests for Phase 4 extractors:
  - loads_extractor
  - materials_extractor
  - boundary_conditions_extractor
  - mooring_extractor
  - aggregator
"""
import sys
from unittest.mock import MagicMock, patch, PropertyMock
import pytest

# ---------------------------------------------------------------------------
# Helpers to create mock OrcaFlex objects
# ---------------------------------------------------------------------------

def _mock_line(name="TestLine", n_segs=5, seg_length=10.0, lt_name="8in_Pipe"):
    """Return a MagicMock mimicking an OrcaFlex Line object."""
    line = MagicMock()
    line.Name = name
    line.NumberOfSegments = n_segs
    line.SegmentLength = [seg_length] * n_segs
    line.NodeArclengths = [i * seg_length for i in range(n_segs + 1)]
    line.LineType = lt_name
    line.EndAConnection = "Anchored"
    line.EndBConnection = "VESSEL_A"
    line.EndAX = 0.0
    line.EndAY = 0.0
    line.EndAZ = -500.0
    line.EndBX = 100.0
    line.EndBY = 0.0
    line.EndBZ = -10.0
    line.SeabedNormalStiffness = 100.0
    line.SeabedFrictionCoefficient = 0.3

    # StaticResult returns a float
    line.StaticResult = MagicMock(return_value=1000.0)

    # RangeGraph returns object with .X, .Max, .Min, .Mean
    rg = MagicMock()
    rg.X = [i * seg_length for i in range(n_segs + 1)]
    rg.Max = [1100.0] * (n_segs + 1)
    rg.Min = [900.0] * (n_segs + 1)
    rg.Mean = [1000.0] * (n_segs + 1)
    line.RangeGraph = MagicMock(return_value=rg)

    # TimeHistory returns list of floats
    line.TimeHistory = MagicMock(return_value=[1000.0] * 10)

    # Model reference
    model = _mock_model()
    line.model = model
    # Support item lookup for line type
    lt_obj = MagicMock()
    lt_obj.OD = 0.2032
    lt_obj.ID = 0.1778
    lt_obj.MassPerUnitLength = 150.0
    lt_obj.EA = 1e6
    lt_obj.EIx = 5e4
    model.__getitem__ = MagicMock(return_value=lt_obj)

    return line


def _mock_model():
    """Return a MagicMock mimicking an OrcaFlex Model object."""
    model = MagicMock()
    model.version = "11.4"

    env = MagicMock()
    env.WaveHeight = 4.5
    env.WavePeriod = 12.0
    env.WaterDepth = 500.0
    env.CurrentVelocity = 0.5
    env.CurrentDirection = 180.0
    model.environment = env

    general = MagicMock()
    general.RampDuration = 30.0
    model.general = general

    model.SampleTimes = MagicMock(return_value=list(range(10)))
    return model


# ===========================================================================
# loads_extractor
# ===========================================================================

class TestLoadsExtractor:
    def test_extract_loads_basic(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.loads_extractor import (
            extract_loads,
        )
        from digitalmodel.solvers.orcaflex.reporting.models.loads import EnvironmentData

        mock_ofx = MagicMock()
        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.loads_extractor.ofx",
            mock_ofx,
        ):
            model = _mock_model()
            result = extract_loads(model)

        assert isinstance(result, EnvironmentData)
        assert len(result.load_cases) == 1
        lc = result.load_cases[0]
        assert lc.case_id == "Active Load Case"
        assert lc.hs_m == pytest.approx(4.5)
        assert lc.tp_s == pytest.approx(12.0)

    def test_extract_loads_raises_without_ofxapi(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.loads_extractor import (
            extract_loads,
        )

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.loads_extractor.ofx",
            None,
        ):
            with pytest.raises(ImportError, match="OrcFxAPI"):
                extract_loads(_mock_model())

    def test_extract_loads_missing_attrs(self):
        """Extractor must not crash when environment attributes are absent."""
        from digitalmodel.solvers.orcaflex.reporting.extractors.loads_extractor import (
            extract_loads,
        )

        mock_ofx = MagicMock()
        model = MagicMock()
        env = MagicMock(spec=[])  # empty spec — all attrs raise AttributeError
        model.environment = env

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.loads_extractor.ofx",
            mock_ofx,
        ):
            result = extract_loads(model)

        assert result.load_cases[0].hs_m is None


# ===========================================================================
# materials_extractor
# ===========================================================================

class TestMaterialsExtractor:
    def test_extract_materials_basic(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.materials_extractor import (
            extract_materials,
        )
        from digitalmodel.solvers.orcaflex.reporting.models.materials import MaterialData

        mock_ofx = MagicMock()
        line = _mock_line()

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.materials_extractor.ofx",
            mock_ofx,
        ):
            result = extract_materials(line)

        assert isinstance(result, MaterialData)
        assert len(result.line_types) == 1
        assert result.line_types[0].name == "8in_Pipe"

    def test_extract_materials_raises_without_ofxapi(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.materials_extractor import (
            extract_materials,
        )

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.materials_extractor.ofx",
            None,
        ):
            with pytest.raises(ImportError, match="OrcFxAPI"):
                extract_materials(_mock_line())

    def test_extract_materials_for_lines_deduplicates(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.materials_extractor import (
            extract_materials_for_lines,
        )

        mock_ofx = MagicMock()
        lines = [_mock_line(lt_name="TypeA"), _mock_line(lt_name="TypeA"), _mock_line(lt_name="TypeB")]

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.materials_extractor.ofx",
            mock_ofx,
        ):
            result = extract_materials_for_lines(lines)

        names = {lt.name for lt in result.line_types}
        assert "TypeA" in names
        assert "TypeB" in names
        assert len(result.line_types) == 2

    def test_extract_materials_for_lines_raises_without_ofxapi(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.materials_extractor import (
            extract_materials_for_lines,
        )

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.materials_extractor.ofx",
            None,
        ):
            with pytest.raises(ImportError):
                extract_materials_for_lines([_mock_line()])


# ===========================================================================
# boundary_conditions_extractor
# ===========================================================================

class TestBCExtractor:
    def test_extract_bc_basic(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.boundary_conditions_extractor import (
            extract_boundary_conditions,
        )
        from digitalmodel.solvers.orcaflex.reporting.models.boundary_conditions import BCData

        mock_ofx = MagicMock()
        line = _mock_line()

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.boundary_conditions_extractor.ofx",
            mock_ofx,
        ):
            result = extract_boundary_conditions(line)

        assert isinstance(result, BCData)
        assert result.end_a is not None
        assert result.end_b is not None
        assert result.end_a.type == "Fixed"
        assert result.end_a.name == "End A"

    def test_extract_bc_raises_without_ofxapi(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.boundary_conditions_extractor import (
            extract_boundary_conditions,
        )

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.boundary_conditions_extractor.ofx",
            None,
        ):
            with pytest.raises(ImportError, match="OrcFxAPI"):
                extract_boundary_conditions(_mock_line())

    def test_extract_bc_vessel_connection(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.boundary_conditions_extractor import (
            extract_boundary_conditions,
        )

        mock_ofx = MagicMock()
        line = _mock_line()
        line.EndBConnection = "FPSO_Hull"

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.boundary_conditions_extractor.ofx",
            mock_ofx,
        ):
            result = extract_boundary_conditions(line)

        assert result.end_b.type == "Vessel"
        assert result.end_b.connected_to == "FPSO_Hull"

    def test_seabed_model_populated(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.boundary_conditions_extractor import (
            extract_boundary_conditions,
        )

        mock_ofx = MagicMock()
        line = _mock_line()

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.boundary_conditions_extractor.ofx",
            mock_ofx,
        ):
            result = extract_boundary_conditions(line)

        assert result.seabed is not None
        assert result.seabed.stiffness_kn_m2 == pytest.approx(100.0)
        assert result.seabed.type == "Linear"


# ===========================================================================
# mooring_extractor
# ===========================================================================

class TestMooringExtractor:
    def _make_comprehensive_results(self):
        """Build a minimal ComprehensiveResults-like mock."""
        from unittest.mock import MagicMock

        cr = MagicMock()
        cr.individual_results = {}
        cr.overall_summary = None
        cr.errors = []
        cr.warnings = []

        ar = MagicMock()
        pm = MagicMock()
        pm.tension_distribution = {"Line001": 1200.0, "Line002": 1300.0}
        pm.recommendations = ["Check pretension"]
        ar.pretension_metrics = pm

        fm = MagicMock()
        fm.utilization_rates = {"Fender_P1": 75.0, "Fender_S1": 82.0}
        fm.recommendations = []
        ar.fender_metrics = fm

        sm = MagicMock()
        sm.natural_periods = {"surge": 120.0, "sway": 118.0}
        sm.recommendations = []
        ar.stiffness_metrics = sm

        cr.individual_results["run_001"] = ar
        return cr

    def test_extract_mooring_report_basic(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.mooring_extractor import (
            extract_mooring_report,
        )
        from digitalmodel.solvers.orcaflex.reporting.models.report import OrcaFlexAnalysisReport

        cr = self._make_comprehensive_results()
        result = extract_mooring_report(cr, project_name="Test Project", structure_id="MOOR-001")

        assert isinstance(result, OrcaFlexAnalysisReport)
        assert result.structure_type == "mooring"
        assert result.project_name == "Test Project"

    def test_extract_mooring_report_static_results(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.mooring_extractor import (
            extract_mooring_report,
        )

        cr = self._make_comprehensive_results()
        result = extract_mooring_report(cr)

        assert result.static_results is not None
        per_line = result.static_results.per_line_tensions
        assert "run_001/Line001" in per_line
        assert per_line["run_001/Line001"] == pytest.approx(1200.0)

    def test_extract_mooring_report_design_checks(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.mooring_extractor import (
            extract_mooring_report,
        )

        cr = self._make_comprehensive_results()
        result = extract_mooring_report(cr)

        assert result.design_checks is not None
        uc_names = [c.name for c in result.design_checks.checks]
        assert any("Fender_P1" in n for n in uc_names)

    def test_extract_mooring_report_summary_notes(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.mooring_extractor import (
            extract_mooring_report,
        )

        cr = self._make_comprehensive_results()
        result = extract_mooring_report(cr)

        assert result.summary_notes is not None
        assert "surge" in result.summary_notes

    def test_extract_mooring_report_recommendations(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.mooring_extractor import (
            extract_mooring_report,
        )

        cr = self._make_comprehensive_results()
        result = extract_mooring_report(cr)

        assert "Check pretension" in result.recommendations

    def test_extract_mooring_report_empty(self):
        """Empty ComprehensiveResults should return a minimal valid report."""
        from digitalmodel.solvers.orcaflex.reporting.extractors.mooring_extractor import (
            extract_mooring_report,
        )

        cr = MagicMock()
        cr.individual_results = {}
        cr.overall_summary = None

        result = extract_mooring_report(cr)
        assert result.structure_type == "mooring"
        assert result.static_results is None
        assert result.design_checks is None


# ===========================================================================
# aggregator
# ===========================================================================

class TestAggregator:
    def test_extract_mesh_all_lines(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            extract_mesh_all_lines,
        )
        from digitalmodel.solvers.orcaflex.reporting.models.mesh import MeshData

        mock_ofx = MagicMock()
        lines = [_mock_line(n_segs=4, seg_length=5.0), _mock_line(n_segs=3, seg_length=10.0)]

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.aggregator.ofx",
            mock_ofx,
        ), patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.mesh_extractor.ofx",
            mock_ofx,
        ):
            result = extract_mesh_all_lines(lines)

        assert isinstance(result, MeshData)
        assert result.total_segment_count == 7  # 4 + 3

    def test_extract_mesh_all_lines_empty(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            extract_mesh_all_lines,
        )

        # All lines raise on extract_mesh — should return empty MeshData
        lines = [MagicMock(spec=[])]
        result = extract_mesh_all_lines(lines)
        assert result.total_segment_count == 0

    def test_build_report_from_model_raises_without_ofxapi(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            build_report_from_model,
        )

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.aggregator.ofx",
            None,
        ):
            with pytest.raises(ImportError):
                build_report_from_model(MagicMock())

    def test_build_report_from_model_no_lines_raises(self):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            build_report_from_model,
        )

        mock_ofx = MagicMock()
        model = _mock_model()
        model.objects = []

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.aggregator.ofx",
            mock_ofx,
        ):
            with pytest.raises(ValueError, match="no Line objects"):
                build_report_from_model(model)


class TestExportRangegraphCsvs:
    def test_export_rangegraph_csvs_basic(self, tmp_path):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            export_rangegraph_csvs,
        )

        mock_ofx = MagicMock()
        line = _mock_line(name="pipeline", n_segs=4, seg_length=10.0)

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.aggregator.ofx",
            mock_ofx,
        ):
            paths = export_rangegraph_csvs(
                lines=[line],
                variables=["Effective Tension", "Max Bending Stress"],
                period=mock_ofx.pnDynamic,
                output_dir=tmp_path,
            )

        assert len(paths) == 1
        assert paths[0].name == "pipeline_rangegraph.csv"
        assert paths[0].exists()

        import csv
        with open(paths[0]) as f:
            reader = csv.DictReader(f)
            headers = reader.fieldnames
        assert "ArcLength_m" in headers
        assert "Effective_Tension_Min" in headers
        assert "Effective_Tension_Max" in headers
        assert "Effective_Tension_Mean" in headers
        assert "Max_Bending_Stress_Min" in headers

    def test_export_rangegraph_csvs_raises_without_ofxapi(self, tmp_path):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            export_rangegraph_csvs,
        )

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.aggregator.ofx",
            None,
        ):
            with pytest.raises(ImportError, match="OrcFxAPI"):
                export_rangegraph_csvs(
                    lines=[_mock_line()],
                    variables=["Effective Tension"],
                    period=None,
                    output_dir=tmp_path,
                )

    def test_export_rangegraph_csvs_creates_output_dir(self, tmp_path):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            export_rangegraph_csvs,
        )

        mock_ofx = MagicMock()
        nested_dir = tmp_path / "sub" / "dir"

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.aggregator.ofx",
            mock_ofx,
        ):
            paths = export_rangegraph_csvs(
                lines=[_mock_line(name="pipe")],
                variables=["Effective Tension"],
                period=mock_ofx.pnDynamic,
                output_dir=nested_dir,
            )

        assert nested_dir.exists()
        assert len(paths) == 1

    def test_export_rangegraph_csvs_row_count_matches_arc_length(self, tmp_path):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            export_rangegraph_csvs,
        )

        mock_ofx = MagicMock()
        line = _mock_line(name="pipeline", n_segs=5, seg_length=10.0)

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.aggregator.ofx",
            mock_ofx,
        ):
            paths = export_rangegraph_csvs(
                lines=[line],
                variables=["Effective Tension"],
                period=mock_ofx.pnDynamic,
                output_dir=tmp_path,
            )

        import csv
        with open(paths[0]) as f:
            rows = list(csv.DictReader(f))
        # _mock_line n_segs=5 -> RangeGraph returns 6 points (n_segs+1)
        assert len(rows) == 6
