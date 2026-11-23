#!/usr/bin/env python3
"""
ABOUTME: Tests for OpenFOAM parametric case generation covering parameter
matrix expansion, case naming conventions, and directory management.
"""

import pytest
from pathlib import Path

from digitalmodel.solvers.openfoam.models import CaseType
from digitalmodel.solvers.openfoam.parametric import ParametricStudy, StudyParameter


# ============================================================================
# StudyParameter tests
# ============================================================================


class TestStudyParameter:
    """Test StudyParameter definition model."""

    def test_study_parameter_creation(self):
        """StudyParameter stores name and values."""
        sp = StudyParameter(name="wave_height", values=[1.0, 2.0, 3.0])
        assert sp.name == "wave_height"
        assert len(sp.values) == 3

    def test_study_parameter_from_range(self):
        """StudyParameter can be created from a range spec."""
        sp = StudyParameter.from_range(
            name="current_speed", start=0.5, stop=2.0, step=0.5
        )
        assert len(sp.values) > 0
        assert sp.values[0] == pytest.approx(0.5)

    def test_study_parameter_n_values(self):
        """n_values returns correct count."""
        sp = StudyParameter(name="heading", values=[0.0, 45.0, 90.0, 135.0])
        assert sp.n_values == 4


# ============================================================================
# ParametricStudy tests
# ============================================================================


class TestParametricStudy:
    """Test ParametricStudy case matrix generation."""

    def test_single_parameter_generates_correct_count(self):
        """Single parameter with N values generates N cases."""
        study = ParametricStudy(case_type=CaseType.CURRENT_LOADING)
        study.add_parameter(
            StudyParameter(name="current_speed", values=[0.5, 1.0, 1.5])
        )
        cases = study.generate_cases()
        assert len(cases) == 3

    def test_two_parameters_generate_product_count(self):
        """Two parameters with M and N values generate M*N cases."""
        study = ParametricStudy(case_type=CaseType.CURRENT_LOADING)
        study.add_parameter(
            StudyParameter(name="current_speed", values=[0.5, 1.0])
        )
        study.add_parameter(
            StudyParameter(name="heading", values=[0.0, 45.0, 90.0])
        )
        cases = study.generate_cases()
        assert len(cases) == 6

    def test_no_parameters_raises_error(self):
        """Generating cases without parameters raises ValueError."""
        study = ParametricStudy(case_type=CaseType.CURRENT_LOADING)
        with pytest.raises(ValueError, match="parameter"):
            study.generate_cases()

    def test_case_names_are_unique(self):
        """All generated case names are distinct."""
        study = ParametricStudy(case_type=CaseType.CURRENT_LOADING)
        study.add_parameter(
            StudyParameter(name="current_speed", values=[0.5, 1.0, 1.5])
        )
        cases = study.generate_cases()
        names = [c.name for c in cases]
        assert len(names) == len(set(names))

    def test_case_names_contain_parameter_values(self):
        """Generated case names encode the parameter values."""
        study = ParametricStudy(case_type=CaseType.CURRENT_LOADING)
        study.add_parameter(
            StudyParameter(name="speed", values=[1.0])
        )
        cases = study.generate_cases()
        assert "1" in cases[0].name or "speed" in cases[0].name.lower()

    def test_case_type_propagated(self):
        """All generated cases have the study's case_type."""
        study = ParametricStudy(case_type=CaseType.WAVE_LOADING)
        study.add_parameter(
            StudyParameter(name="wave_height", values=[2.0, 4.0])
        )
        cases = study.generate_cases()
        assert all(c.case_type == CaseType.WAVE_LOADING for c in cases)

    def test_generate_directories(self, tmp_path):
        """generate_directories builds one subdirectory per case."""
        study = ParametricStudy(
            case_type=CaseType.CURRENT_LOADING,
            study_name="speed_sweep",
        )
        study.add_parameter(
            StudyParameter(name="speed", values=[1.0, 2.0])
        )
        dirs = study.generate_directories(base_dir=tmp_path)
        assert len(dirs) == 2
        for d in dirs:
            assert d.is_dir()

    def test_study_name_prefix_in_directory_names(self, tmp_path):
        """Study name appears in generated directory names when provided."""
        study = ParametricStudy(
            case_type=CaseType.CURRENT_LOADING,
            study_name="current_sweep",
        )
        study.add_parameter(
            StudyParameter(name="speed", values=[1.0])
        )
        dirs = study.generate_directories(base_dir=tmp_path)
        assert any("current_sweep" in str(d) for d in dirs)
