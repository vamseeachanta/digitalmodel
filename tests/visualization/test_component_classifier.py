"""
Unit tests for the OrcaFlex component classifier service.

Tests cover:
- Enum values (ComponentType, ParameterType)
- ComponentInfo and ClassificationResult dataclass creation
- classify_column: component and parameter pattern matching
- OrcaFlex column naming conventions (space-separated tokens match word boundaries)
- Unit-based and data-pattern-based classification
- Edge cases: empty columns, unknown columns, confidence range
- classify_file: grouping, sorting, empty DataFrame
- get_component_summary, validate_classification, get_component_hierarchy

Note on word-boundary matching:
    The classifier uses regex \\b which treats underscore as a word character.
    Therefore 'fst1_fx' does NOT match r'fst1\\b', but 'fst1 fx' DOES.
    Tests use space-separated column names to exercise component patterns.

Note on unit-based classification:
    _classify_by_unit uses substring matching ('m' in column_name).
    Since 'm' appears in many words, columns containing the letter 'm'
    get unit-classified as DISPLACEMENT_X at 0.8 confidence, which can
    override weaker pattern matches. Tests reflect this actual behavior.
"""

import importlib.util
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

# ---------------------------------------------------------------------------
# Dynamic import: the parent directory "orcaflex-dashboard" contains a hyphen,
# which blocks normal Python imports. Use spec_from_file_location instead.
# ---------------------------------------------------------------------------
_MOD_PATH = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "visualization"
    / "orcaflex-dashboard"
    / "backend"
    / "app"
    / "services"
    / "component_classifier.py"
)

_spec = importlib.util.spec_from_file_location("component_classifier", _MOD_PATH)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

ComponentType = _mod.ComponentType
ParameterType = _mod.ParameterType
ComponentInfo = _mod.ComponentInfo
ClassificationResult = _mod.ClassificationResult
ComponentClassifier = _mod.ComponentClassifier
get_component_hierarchy = _mod.get_component_hierarchy


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def classifier():
    """Return a fresh ComponentClassifier instance."""
    return ComponentClassifier()


@pytest.fixture
def sample_components():
    """Return a list of ComponentInfo objects for summary/validation tests."""
    return [
        ComponentInfo(
            name="fst1",
            component_type=ComponentType.FST1,
            columns=["fst1 fx", "fst1 fy", "fst1 fz"],
            parameters={
                "fst1 fx": ParameterType.FORCE_X,
                "fst1 fy": ParameterType.FORCE_Y,
                "fst1 fz": ParameterType.FORCE_Z,
            },
            confidence_score=0.85,
        ),
        ComponentInfo(
            name="strut",
            component_type=ComponentType.STRUT,
            columns=["strut tension", "strut curvature"],
            parameters={
                "strut tension": ParameterType.TENSION,
                "strut curvature": ParameterType.CURVATURE,
            },
            confidence_score=0.75,
        ),
    ]


# ===========================================================================
# 1. Enum values
# ===========================================================================

class TestComponentTypeEnum:
    def test_all_expected_members_exist(self):
        expected = {"FST1", "FST2", "STRUT", "JACKET", "LNGC", "UNKNOWN"}
        assert set(ComponentType.__members__.keys()) == expected

    def test_enum_values(self):
        assert ComponentType.FST1.value == "fst1"
        assert ComponentType.FST2.value == "fst2"
        assert ComponentType.STRUT.value == "strut"
        assert ComponentType.JACKET.value == "jacket"
        assert ComponentType.LNGC.value == "lngc"
        assert ComponentType.UNKNOWN.value == "unknown"


class TestParameterTypeEnum:
    def test_all_expected_members_exist(self):
        expected = {
            "FORCE_X", "FORCE_Y", "FORCE_Z",
            "MOMENT_X", "MOMENT_Y", "MOMENT_Z",
            "DISPLACEMENT_X", "DISPLACEMENT_Y", "DISPLACEMENT_Z",
            "ROTATION_X", "ROTATION_Y", "ROTATION_Z",
            "VELOCITY", "ACCELERATION", "TENSION", "CURVATURE", "OTHER",
        }
        assert set(ParameterType.__members__.keys()) == expected

    def test_force_values(self):
        assert ParameterType.FORCE_X.value == "force_x"
        assert ParameterType.FORCE_Y.value == "force_y"
        assert ParameterType.FORCE_Z.value == "force_z"

    def test_other_value(self):
        assert ParameterType.OTHER.value == "other"


# ===========================================================================
# 2. Dataclass creation
# ===========================================================================

class TestComponentInfo:
    def test_basic_creation(self):
        info = ComponentInfo(
            name="fst1",
            component_type=ComponentType.FST1,
            columns=["col_a"],
            parameters={"col_a": ParameterType.TENSION},
            confidence_score=0.9,
        )
        assert info.name == "fst1"
        assert info.component_type == ComponentType.FST1
        assert info.columns == ["col_a"]
        assert info.confidence_score == 0.9

    def test_metadata_defaults_to_empty_dict(self):
        info = ComponentInfo(
            name="test",
            component_type=ComponentType.UNKNOWN,
            columns=[],
            parameters={},
            confidence_score=0.0,
        )
        assert info.metadata == {}

    def test_metadata_preserves_explicit_value(self):
        meta = {"source": "manual"}
        info = ComponentInfo(
            name="test",
            component_type=ComponentType.FST1,
            columns=[],
            parameters={},
            confidence_score=0.5,
            metadata=meta,
        )
        assert info.metadata == {"source": "manual"}


class TestClassificationResult:
    def test_creation(self):
        result = ClassificationResult(
            components=[],
            unclassified_columns=["abc"],
            classification_confidence=0.7,
            total_columns=10,
            classified_columns=9,
        )
        assert result.total_columns == 10
        assert result.classified_columns == 9
        assert result.unclassified_columns == ["abc"]


# ===========================================================================
# 3. classify_column -- component patterns (space-separated names)
# ===========================================================================

class TestClassifyColumnComponent:
    """Verify component pattern matching via classify_column.

    Word-boundary regex (\\b) does not fire between word chars and underscores,
    so space-separated column names are used to exercise the patterns.
    """

    @pytest.mark.parametrize("col,expected_type", [
        ("fst1 fx", ComponentType.FST1),
        ("FST1 MomentX", ComponentType.FST1),
        ("floater 1 disp_x", ComponentType.FST1),
        ("hull 1 tension", ComponentType.FST1),
    ])
    def test_fst1_patterns(self, classifier, col, expected_type):
        comp, _, _ = classifier.classify_column(col)
        assert comp == expected_type

    @pytest.mark.parametrize("col,expected_type", [
        ("fst2 fy", ComponentType.FST2),
        ("floater 2 mz", ComponentType.FST2),
        ("hull 2 tension", ComponentType.FST2),
        ("platform 2 vel", ComponentType.FST2),
    ])
    def test_fst2_patterns(self, classifier, col, expected_type):
        comp, _, _ = classifier.classify_column(col)
        assert comp == expected_type

    @pytest.mark.parametrize("col,expected_type", [
        ("strut tension", ComponentType.STRUT),
        ("connector fx", ComponentType.STRUT),
        ("connection moment_x", ComponentType.STRUT),
    ])
    def test_strut_patterns(self, classifier, col, expected_type):
        comp, _, _ = classifier.classify_column(col)
        assert comp == expected_type

    @pytest.mark.parametrize("col,expected_type", [
        ("jacket fz", ComponentType.JACKET),
        ("foundation mz", ComponentType.JACKET),
        ("pile tension", ComponentType.JACKET),
        ("suction fx", ComponentType.JACKET),
    ])
    def test_jacket_patterns(self, classifier, col, expected_type):
        comp, _, _ = classifier.classify_column(col)
        assert comp == expected_type

    @pytest.mark.parametrize("col,expected_type", [
        ("lngc fx", ComponentType.LNGC),
        ("vessel disp_y", ComponentType.LNGC),
        ("ship moment_z", ComponentType.LNGC),
        ("carrier velocity", ComponentType.LNGC),
        ("tanker rz", ComponentType.LNGC),
    ])
    def test_lngc_patterns(self, classifier, col, expected_type):
        comp, _, _ = classifier.classify_column(col)
        assert comp == expected_type

    def test_underscore_names_do_not_match_component(self, classifier):
        """Underscore-separated names fail \\b matching, yielding UNKNOWN."""
        comp, _, _ = classifier.classify_column("fst1_fx")
        assert comp == ComponentType.UNKNOWN


# ===========================================================================
# 4. classify_column -- parameter patterns
# ===========================================================================

class TestClassifyColumnParameter:
    """Verify parameter pattern matching via classify_column."""

    @pytest.mark.parametrize("col,expected_param", [
        ("fst1 fx", ParameterType.FORCE_X),
        ("strut fy", ParameterType.FORCE_Y),
        ("jacket fz", ParameterType.FORCE_Z),
        ("fst1 force_x", ParameterType.FORCE_X),
    ])
    def test_force_parameters(self, classifier, col, expected_param):
        _, param, _ = classifier.classify_column(col)
        assert param == expected_param

    @pytest.mark.parametrize("col,expected_param", [
        # 'moment_x' pattern matches via r'moment.*x\b'
        ("fst1 moment_x", ParameterType.MOMENT_X),
        ("ship moment_z", ParameterType.MOMENT_Z),
    ])
    def test_moment_parameters_explicit(self, classifier, col, expected_param):
        _, param, _ = classifier.classify_column(col)
        assert param == expected_param

    def test_short_mx_overridden_by_unit_m(self, classifier):
        """'fst1 mx' -- pattern mx\\b matches MOMENT_X, but the 'm' in
        the column triggers unit-based DISPLACEMENT_X at 0.8 confidence,
        which is higher, so DISPLACEMENT_X wins."""
        _, param, _ = classifier.classify_column("fst1 mx")
        assert param == ParameterType.DISPLACEMENT_X

    @pytest.mark.parametrize("col,expected_param", [
        ("fst1 rx", ParameterType.ROTATION_X),
        ("vessel ry", ParameterType.ROTATION_Y),
        ("jacket rz", ParameterType.ROTATION_Z),
        ("fst1 rot_x", ParameterType.ROTATION_X),
    ])
    def test_rotation_parameters(self, classifier, col, expected_param):
        _, param, _ = classifier.classify_column(col)
        assert param == expected_param

    def test_tension_parameter(self, classifier):
        _, param, _ = classifier.classify_column("strut tension")
        assert param == ParameterType.TENSION

    def test_velocity_parameter(self, classifier):
        _, param, _ = classifier.classify_column("vessel velocity")
        assert param == ParameterType.VELOCITY

    def test_acceleration_parameter(self, classifier):
        _, param, _ = classifier.classify_column("lngc acceleration")
        assert param == ParameterType.ACCELERATION

    def test_curvature_parameter(self, classifier):
        _, param, _ = classifier.classify_column("strut curvature")
        assert param == ParameterType.CURVATURE

    @pytest.mark.parametrize("col,expected_param", [
        ("fst1 disp_x", ParameterType.DISPLACEMENT_X),
        ("fst1 disp_y", ParameterType.DISPLACEMENT_Y),
        ("fst1 disp_z", ParameterType.DISPLACEMENT_Z),
    ])
    def test_displacement_parameters(self, classifier, col, expected_param):
        _, param, _ = classifier.classify_column(col)
        assert param == expected_param


# ===========================================================================
# 5. Edge cases
# ===========================================================================

class TestEdgeCases:
    def test_unknown_column_no_m_returns_other(self, classifier):
        """A column with no recognizable patterns and no letter 'm'
        (which would trigger unit-based displacement) returns OTHER."""
        comp, param, conf = classifier.classify_column("zzzz")
        assert comp == ComponentType.UNKNOWN
        assert param == ParameterType.OTHER

    def test_unknown_column_with_m_gets_displacement(self, classifier):
        """Columns containing the letter 'm' get unit-classified as
        DISPLACEMENT_X at 0.8 confidence due to substring unit matching."""
        _, param, _ = classifier.classify_column("some_random_metric")
        assert param == ParameterType.DISPLACEMENT_X

    def test_empty_string_column(self, classifier):
        comp, param, _ = classifier.classify_column("")
        assert comp == ComponentType.UNKNOWN

    def test_whitespace_column(self, classifier):
        comp, _, _ = classifier.classify_column("   ")
        assert comp == ComponentType.UNKNOWN

    def test_generic_float_defaults_to_fst1(self, classifier):
        """Generic 'float' without a number should fall back to FST1 at low confidence."""
        comp, _, _ = classifier.classify_column("float something")
        assert comp == ComponentType.FST1

    def test_case_insensitivity(self, classifier):
        """Patterns are compiled with re.IGNORECASE."""
        comp, param, _ = classifier.classify_column("FST1 FX")
        assert comp == ComponentType.FST1
        assert param == ParameterType.FORCE_X

    def test_confidence_in_valid_range(self, classifier):
        """Confidence should always be between 0 and 1."""
        _, _, conf = classifier.classify_column("fst1 tension")
        assert 0.0 <= conf <= 1.0


# ===========================================================================
# 6. Unit-based classification (_classify_by_unit)
# ===========================================================================

class TestUnitBasedClassification:
    def test_kn_unit_implies_force(self, classifier):
        param_type, conf = classifier._classify_by_unit("force (kN)")
        assert param_type == ParameterType.FORCE_X
        assert conf == 0.8

    def test_knm_still_matches_kn_first(self, classifier):
        """'kNm' contains 'kN' as a substring; dict iteration hits 'kN' first,
        so the result is FORCE_X rather than MOMENT_X."""
        param_type, conf = classifier._classify_by_unit("moment (kNm)")
        assert param_type == ParameterType.FORCE_X
        assert conf == 0.8

    def test_deg_unit_implies_rotation(self, classifier):
        param_type, conf = classifier._classify_by_unit("heading (deg)")
        assert param_type == ParameterType.ROTATION_X
        assert conf == 0.8

    def test_m_unit_implies_displacement(self, classifier):
        param_type, conf = classifier._classify_by_unit("displacement (m)")
        assert param_type == ParameterType.DISPLACEMENT_X
        assert conf == 0.8

    def test_no_unit_returns_other(self, classifier):
        """A column name that contains no unit substring returns OTHER at 0.0."""
        param_type, conf = classifier._classify_by_unit("zzzz")
        assert param_type == ParameterType.OTHER
        assert conf == 0.0


# ===========================================================================
# 7. Data-pattern-based classification (_classify_by_data_pattern)
# ===========================================================================

class TestDataPatternClassification:
    def test_rotation_like_data(self, classifier):
        """Data in 0-360 range with range <= 360 triggers rotation heuristic."""
        np.random.seed(42)
        data = pd.Series(np.random.uniform(0, 180, size=100))
        param, conf = classifier._classify_by_data_pattern(data)
        assert param == ParameterType.ROTATION_Z
        assert conf == 0.6

    def test_force_like_data(self, classifier):
        """Large-magnitude data (range > 1000, abs(mean) > 100) suggests forces."""
        np.random.seed(42)
        data = pd.Series(np.random.uniform(500, 5000, size=100))
        param, conf = classifier._classify_by_data_pattern(data)
        assert param == ParameterType.FORCE_Z
        assert conf == 0.5

    def test_small_range_data_matches_rotation_first(self, classifier):
        """Small displacement-like values still satisfy the rotation heuristic
        (0 <= abs(mean) <= 360, range <= 360) which is checked first."""
        np.random.seed(42)
        data = pd.Series(np.random.uniform(-2, 2, size=100))
        param, _ = classifier._classify_by_data_pattern(data)
        assert param == ParameterType.ROTATION_Z

    def test_non_numeric_data_returns_other(self, classifier):
        data = pd.Series(["abc", "def", "ghi"])
        param, conf = classifier._classify_by_data_pattern(data)
        assert param == ParameterType.OTHER
        assert conf == 0.0

    def test_empty_series_returns_other(self, classifier):
        data = pd.Series([], dtype=float)
        param, conf = classifier._classify_by_data_pattern(data)
        assert param == ParameterType.OTHER
        assert conf == 0.0

    def test_data_sample_used_in_classify_column(self, classifier):
        """When classify_column receives data with large forces, the data-based
        classification influences the result for an otherwise-unknown column."""
        np.random.seed(42)
        force_data = pd.Series(np.random.uniform(500, 5000, size=100))
        _, param, _ = classifier.classify_column("zzzz", data_sample=force_data)
        assert param == ParameterType.FORCE_Z


# ===========================================================================
# 8. classify_file
# ===========================================================================

class TestClassifyFile:
    def test_classify_file_groups_by_component(self, classifier):
        """Space-separated column names match component patterns."""
        df = pd.DataFrame({
            "fst1 fx": [100, 200, 300],
            "fst1 fy": [110, 210, 310],
            "strut tension": [50, 60, 70],
        })
        parse_result = {"dataframe": df, "column_metadata": {}}
        components = classifier.classify_file(parse_result)

        comp_types = {c.component_type for c in components}
        assert ComponentType.FST1 in comp_types
        assert ComponentType.STRUT in comp_types

    def test_classify_file_empty_dataframe(self, classifier):
        df = pd.DataFrame()
        parse_result = {"dataframe": df, "column_metadata": {}}
        components = classifier.classify_file(parse_result)
        assert components == []

    def test_classify_file_sorted_by_confidence(self, classifier):
        """Components should be sorted by confidence, highest first."""
        df = pd.DataFrame({
            "fst1 fx": [1, 2],
            "fst1 fy": [3, 4],
            "zzzz_a": [5, 6],
            "zzzz_b": [7, 8],
            "zzzz_c": [9, 10],
        })
        parse_result = {"dataframe": df, "column_metadata": {}}
        components = classifier.classify_file(parse_result)

        if len(components) > 1:
            for i in range(len(components) - 1):
                assert components[i].confidence_score >= components[i + 1].confidence_score

    def test_classify_file_small_unknown_groups_skipped(self, classifier):
        """Unknown groups with fewer than 3 columns are skipped."""
        df = pd.DataFrame({
            "fst1 fx": [1, 2],
            "zzz1": [3, 4],
        })
        parse_result = {"dataframe": df, "column_metadata": {}}
        components = classifier.classify_file(parse_result)

        comp_types = {c.component_type for c in components}
        assert ComponentType.UNKNOWN not in comp_types


# ===========================================================================
# 9. get_component_summary
# ===========================================================================

class TestGetComponentSummary:
    def test_summary_empty_list(self, classifier):
        summary = classifier.get_component_summary([])
        assert summary["total_components"] == 0
        assert summary["classification_rate"] == 0.0

    def test_summary_populated(self, classifier, sample_components):
        summary = classifier.get_component_summary(sample_components)
        assert summary["total_components"] == 2
        assert summary["total_columns"] == 5
        assert 0.0 < summary["average_confidence"] <= 1.0
        assert "fst1" in summary["component_types"]
        assert "strut" in summary["component_types"]
        assert summary["highest_confidence"] == 0.85
        assert summary["lowest_confidence"] == 0.75

    def test_summary_parameter_types(self, classifier, sample_components):
        summary = classifier.get_component_summary(sample_components)
        assert "force_x" in summary["parameter_types"]
        assert "tension" in summary["parameter_types"]


# ===========================================================================
# 10. validate_classification
# ===========================================================================

class TestValidateClassification:
    def test_valid_when_no_expected_set(self, classifier, sample_components):
        result = classifier.validate_classification(sample_components)
        assert result["is_valid"] is True

    def test_missing_components_detected(self, classifier, sample_components):
        expected = {"fst1", "strut", "jacket"}
        result = classifier.validate_classification(sample_components, expected)
        assert result["is_valid"] is False
        assert "jacket" in result["missing_components"]

    def test_unexpected_components_detected(self, classifier, sample_components):
        expected = {"fst1"}
        result = classifier.validate_classification(sample_components, expected)
        assert "strut" in result["unexpected_components"]

    def test_coverage_calculation(self, classifier, sample_components):
        expected = {"fst1", "strut"}
        result = classifier.validate_classification(sample_components, expected)
        assert result["coverage"] == 1.0

    def test_low_confidence_flagged(self, classifier):
        low_conf = [
            ComponentInfo(
                name="weak",
                component_type=ComponentType.UNKNOWN,
                columns=["a", "b", "c"],
                parameters={
                    "a": ParameterType.OTHER,
                    "b": ParameterType.OTHER,
                    "c": ParameterType.OTHER,
                },
                confidence_score=0.2,
            )
        ]
        result = classifier.validate_classification(low_conf)
        issue_text = " ".join(result["issues"])
        assert "Low confidence" in issue_text

    def test_sparse_component_flagged(self, classifier):
        sparse = [
            ComponentInfo(
                name="sparse",
                component_type=ComponentType.JACKET,
                columns=["single_col"],
                parameters={"single_col": ParameterType.TENSION},
                confidence_score=0.9,
            )
        ]
        result = classifier.validate_classification(sparse)
        issue_text = " ".join(result["issues"])
        assert "few columns" in issue_text


# ===========================================================================
# 11. get_component_hierarchy utility function
# ===========================================================================

class TestGetComponentHierarchy:
    def test_hierarchy_structure(self, sample_components):
        hierarchy = get_component_hierarchy(sample_components)
        assert "fst1" in hierarchy
        assert "strut" in hierarchy
        assert hierarchy["fst1"]["type"] == "fst1"
        assert hierarchy["fst1"]["confidence"] == 0.85
        assert "force_x" in hierarchy["fst1"]["parameters"]

    def test_hierarchy_empty_list(self):
        hierarchy = get_component_hierarchy([])
        assert hierarchy == {}

    def test_hierarchy_parameter_grouping(self, sample_components):
        hierarchy = get_component_hierarchy(sample_components)
        strut_params = hierarchy["strut"]["parameters"]
        assert "tension" in strut_params
        assert "curvature" in strut_params
