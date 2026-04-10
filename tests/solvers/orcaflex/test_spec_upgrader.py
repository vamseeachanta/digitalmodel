"""Tests for spec_upgrader model type detection."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.spec_upgrader import (
    DetectedModelType,
    DetectionResult,
    detect_model_type,
    upgrade_spec,
)

REPO_ROOT = Path(__file__).resolve().parents[3]
LIBRARY = REPO_ROOT / "docs" / "domains" / "orcaflex" / "library" / "model_library"


# ---------------------------------------------------------------------------
# Real spec file tests
# ---------------------------------------------------------------------------

@pytest.fixture
def riser_spec():
    path = LIBRARY / "a01_catenary_riser" / "spec.yml"
    if not path.exists():
        pytest.skip(f"Spec file not found: {path}")
    with open(path) as f:
        return yaml.safe_load(f)


@pytest.fixture
def mooring_spec():
    path = LIBRARY / "c03_turret_moored_fpso" / "spec.yml"
    if not path.exists():
        pytest.skip(f"Spec file not found: {path}")
    with open(path) as f:
        return yaml.safe_load(f)


@pytest.fixture
def pipeline_spec():
    path = LIBRARY / "m01_pipeline_lateral_buckling" / "spec.yml"
    if not path.exists():
        pytest.skip(f"Spec file not found: {path}")
    with open(path) as f:
        return yaml.safe_load(f)


class TestRiserDetection:
    def test_detects_riser_type(self, riser_spec):
        result = detect_model_type(riser_spec)
        assert result.model_type == DetectedModelType.RISER

    def test_riser_confidence_above_threshold(self, riser_spec):
        result = detect_model_type(riser_spec)
        assert result.confidence >= 0.3

    def test_riser_has_evidence(self, riser_spec):
        result = detect_model_type(riser_spec)
        assert len(result.evidence) > 0


class TestMooringDetection:
    def test_detects_mooring_type(self, mooring_spec):
        result = detect_model_type(mooring_spec)
        assert result.model_type == DetectedModelType.MOORING

    def test_mooring_confidence_above_threshold(self, mooring_spec):
        result = detect_model_type(mooring_spec)
        assert result.confidence >= 0.3

    def test_mooring_has_evidence(self, mooring_spec):
        result = detect_model_type(mooring_spec)
        assert len(result.evidence) > 0


class TestPipelineDetection:
    def test_detects_pipeline_type(self, pipeline_spec):
        result = detect_model_type(pipeline_spec)
        assert result.model_type == DetectedModelType.PIPELINE

    def test_pipeline_confidence_above_threshold(self, pipeline_spec):
        result = detect_model_type(pipeline_spec)
        assert result.confidence >= 0.3

    def test_pipeline_has_evidence(self, pipeline_spec):
        result = detect_model_type(pipeline_spec)
        assert len(result.evidence) > 0


class TestGenericDetection:
    def test_empty_spec_returns_generic(self):
        result = detect_model_type({})
        assert result.model_type == DetectedModelType.GENERIC
        assert result.confidence == 0.0

    def test_empty_generic_returns_generic(self):
        result = detect_model_type({"generic": {}})
        assert result.model_type == DetectedModelType.GENERIC

    def test_ambiguous_spec_returns_generic(self):
        """A spec with no distinguishing features should return generic."""
        spec = {
            "generic": {
                "lines": [],
                "line_types": [],
                "vessels": [],
            }
        }
        result = detect_model_type(spec)
        assert result.model_type == DetectedModelType.GENERIC

    def test_no_generic_section(self):
        spec = {"metadata": {"name": "test"}, "environment": {}}
        result = detect_model_type(spec)
        assert result.model_type == DetectedModelType.GENERIC
        assert "No 'generic' section" in result.evidence[0]


class TestDetectionResult:
    def test_result_fields(self):
        result = DetectionResult(
            model_type=DetectedModelType.RISER,
            confidence=0.8,
            evidence=["has flex joint"],
        )
        assert result.model_type == DetectedModelType.RISER
        assert result.confidence == 0.8
        assert result.evidence == ["has flex joint"]

    def test_enum_values(self):
        assert DetectedModelType.RISER.value == "riser"
        assert DetectedModelType.MOORING.value == "mooring"
        assert DetectedModelType.PIPELINE.value == "pipeline"
        assert DetectedModelType.GENERIC.value == "generic"


class TestUpgradeSpec:
    def test_upgrade_not_implemented(self):
        with pytest.raises(NotImplementedError, match="planned for v2"):
            upgrade_spec({}, DetectedModelType.RISER)


class TestSyntheticSpecs:
    """Test with minimal synthetic specs to validate heuristic logic."""

    def test_synthetic_riser(self):
        spec = {
            "generic": {
                "vessels": [{"name": "FPSO", "vessel_type": "Tanker"}],
                "lines": [
                    {
                        "name": "Production Riser",
                        "properties": {
                            "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo": [
                                ["FPSO", 0, 0, -10.0, 0, 90, 0, None],
                                ["Anchored", 100, 0, 0.15, 0, 90, 0, None],
                            ],
                            "LineType, Length, TargetSegmentLength": [
                                ["Flex Riser", 120, 2],
                            ],
                        },
                    }
                ],
                "line_types": [
                    {"name": "Flex Riser", "category": "General"},
                ],
                "flex_joints": [{"name": "FJ1"}],
            }
        }
        result = detect_model_type(spec)
        assert result.model_type == DetectedModelType.RISER
        assert result.confidence >= 0.6

    def test_synthetic_mooring(self):
        spec = {
            "generic": {
                "vessels": [{"name": "Vessel1"}],
                "lines": [
                    {
                        "name": f"Leg{i}",
                        "properties": {
                            "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo": [
                                ["Vessel1", 0, 0, -5, 0, 90, 0, None],
                                ["Anchored", 500, 0, 0, 0, 90, 0, None],
                            ],
                        },
                    }
                    for i in range(1, 7)
                ],
                "line_types": [
                    {"name": "12in Chain", "category": "Chain"},
                    {"name": "Wire Rope", "category": "General"},
                ],
            }
        }
        result = detect_model_type(spec)
        assert result.model_type == DetectedModelType.MOORING
        assert result.confidence >= 0.5

    def test_synthetic_pipeline(self):
        spec = {
            "generic": {
                "vessels": [],
                "lines": [
                    {
                        "name": "Pipeline",
                        "properties": {
                            "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo": [
                                ["Anchored", 0, 0, 0.1, 0, 90, 0, None],
                                ["Anchored", 1000, 0, 0.1, 0, 90, 0, None],
                            ],
                            "LineType, Length, TargetSegmentLength": [
                                ["Pipeline type", 1000, 2.5],
                            ],
                        },
                    }
                ],
                "line_types": [
                    {
                        "name": "Pipeline type",
                        "properties": {"CoatingThickness": 0.03},
                    },
                ],
                "support_types": [{"name": "Sleeper"}],
            }
        }
        result = detect_model_type(spec)
        assert result.model_type == DetectedModelType.PIPELINE
        assert result.confidence >= 0.6
