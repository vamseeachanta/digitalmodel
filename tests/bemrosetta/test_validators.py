"""
Tests for BEMRosetta validators.

TDD: Tests written first to drive implementation of:
- CoefficientValidator: Hydrodynamic coefficient validation
- CausalityChecker: Kramers-Kronig causality validation
"""

import pytest
import numpy as np
from datetime import datetime


class TestCoefficientValidator:
    """Tests for CoefficientValidator class."""

    @pytest.fixture
    def valid_diffraction_results(self):
        """Create valid diffraction results for testing."""
        from digitalmodel.diffraction import (
            DiffractionResults,
            RAOSet,
            RAOComponent,
            AddedMassSet,
            DampingSet,
            HydrodynamicMatrix,
            FrequencyData,
            HeadingData,
            DOF,
        )

        # Setup frequency and heading data
        frequencies = np.linspace(0.1, 2.0, 5)
        headings = np.array([0.0, 90.0, 180.0])
        n_freq = len(frequencies)
        n_head = len(headings)

        freq_data = FrequencyData(
            values=frequencies,
            periods=2 * np.pi / frequencies,
            count=n_freq,
            min_freq=frequencies.min(),
            max_freq=frequencies.max(),
        )

        head_data = HeadingData(
            values=headings,
            count=n_head,
            min_heading=headings.min(),
            max_heading=headings.max(),
        )

        # Create RAO components
        def create_rao_component(dof: DOF) -> RAOComponent:
            return RAOComponent(
                dof=dof,
                magnitude=np.random.rand(n_freq, n_head) * 2.0,
                phase=np.random.rand(n_freq, n_head) * 180.0,
                frequencies=freq_data,
                headings=head_data,
                unit="m/m" if dof.value <= 3 else "deg/m",
            )

        rao_set = RAOSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            surge=create_rao_component(DOF.SURGE),
            sway=create_rao_component(DOF.SWAY),
            heave=create_rao_component(DOF.HEAVE),
            roll=create_rao_component(DOF.ROLL),
            pitch=create_rao_component(DOF.PITCH),
            yaw=create_rao_component(DOF.YAW),
            created_date=datetime.now().isoformat(),
        )

        # Create added mass matrices (symmetric, positive diagonal)
        added_mass_matrices = []
        for freq in frequencies:
            # Create symmetric positive semi-definite matrix
            base = np.random.rand(6, 6) * 1000
            symmetric = (base + base.T) / 2
            # Ensure positive diagonal
            np.fill_diagonal(symmetric, np.abs(np.diag(symmetric)) + 100)
            added_mass_matrices.append(
                HydrodynamicMatrix(
                    matrix=symmetric,
                    frequency=freq,
                    matrix_type="added_mass",
                    units={"linear": "kg", "angular": "kg.m^2"},
                )
            )

        added_mass = AddedMassSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            matrices=added_mass_matrices,
            frequencies=freq_data,
            created_date=datetime.now().isoformat(),
        )

        # Create damping matrices (symmetric, non-negative diagonal)
        damping_matrices = []
        for freq in frequencies:
            # Create symmetric non-negative matrix
            base = np.random.rand(6, 6) * 100
            symmetric = (base + base.T) / 2
            # Ensure non-negative diagonal
            np.fill_diagonal(symmetric, np.abs(np.diag(symmetric)) + 10)
            damping_matrices.append(
                HydrodynamicMatrix(
                    matrix=symmetric,
                    frequency=freq,
                    matrix_type="damping",
                    units={"linear": "N.s/m", "angular": "N.m.s/rad"},
                )
            )

        damping = DampingSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            matrices=damping_matrices,
            frequencies=freq_data,
            created_date=datetime.now().isoformat(),
        )

        return DiffractionResults(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            raos=rao_set,
            added_mass=added_mass,
            damping=damping,
            created_date=datetime.now().isoformat(),
        )

    def test_coefficient_validator_can_instantiate(self):
        """Test that CoefficientValidator can be instantiated."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        validator = CoefficientValidator()
        assert validator is not None

    def test_coefficient_validator_with_default_options(self):
        """Test CoefficientValidator uses sensible defaults."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        validator = CoefficientValidator()
        assert validator.check_symmetry is True
        assert validator.check_positive_definite is True
        assert validator.check_physical_limits is True
        assert validator.tolerance == pytest.approx(0.01)

    def test_coefficient_validator_with_custom_options(self):
        """Test CoefficientValidator accepts custom options."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        validator = CoefficientValidator(
            check_symmetry=False,
            check_positive_definite=False,
            check_physical_limits=False,
            tolerance=0.05,
        )
        assert validator.check_symmetry is False
        assert validator.check_positive_definite is False
        assert validator.check_physical_limits is False
        assert validator.tolerance == pytest.approx(0.05)

    def test_coefficient_validator_implements_interface(self):
        """Test CoefficientValidator implements ValidatorInterface."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator
        from digitalmodel.bemrosetta.core.interfaces import ValidatorInterface

        validator = CoefficientValidator()
        assert isinstance(validator, ValidatorInterface)

    def test_validate_returns_validation_report(self, valid_diffraction_results):
        """Test validate() returns ValidationReport."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator
        from digitalmodel.bemrosetta.core.interfaces import ValidationReport

        validator = CoefficientValidator()
        report = validator.validate(valid_diffraction_results)

        assert isinstance(report, ValidationReport)

    def test_validate_valid_data_passes(self, valid_diffraction_results):
        """Test that valid coefficients pass validation."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        validator = CoefficientValidator()
        report = validator.validate(valid_diffraction_results)

        assert report.is_valid is True
        assert len(report.errors) == 0

    def test_is_valid_returns_bool(self, valid_diffraction_results):
        """Test is_valid() returns boolean."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        validator = CoefficientValidator()
        result = validator.is_valid(valid_diffraction_results)

        assert isinstance(result, bool)
        assert result is True

    def test_validate_detects_asymmetric_added_mass(self, valid_diffraction_results):
        """Test that asymmetric added mass matrix is flagged."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        # Make added mass asymmetric
        valid_diffraction_results.added_mass.matrices[0].matrix[0, 1] += 1000
        valid_diffraction_results.added_mass.matrices[0].matrix[1, 0] -= 1000

        validator = CoefficientValidator(check_symmetry=True)
        report = validator.validate(valid_diffraction_results)

        # Should have warning about asymmetry
        assert any("symmetric" in w.lower() for w in report.warnings)

    def test_validate_detects_negative_added_mass_diagonal(self, valid_diffraction_results):
        """Test that negative added mass diagonal is flagged as error."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        # Make diagonal negative
        valid_diffraction_results.added_mass.matrices[0].matrix[0, 0] = -100

        validator = CoefficientValidator(check_positive_definite=True)
        report = validator.validate(valid_diffraction_results)

        # Should have error about negative diagonal
        assert report.is_valid is False
        assert any("negative" in e.lower() for e in report.errors)

    def test_validate_detects_nan_in_added_mass(self, valid_diffraction_results):
        """Test that NaN in added mass is flagged as error."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        # Insert NaN
        valid_diffraction_results.added_mass.matrices[0].matrix[0, 0] = np.nan

        validator = CoefficientValidator(check_physical_limits=True)
        report = validator.validate(valid_diffraction_results)

        assert report.is_valid is False
        assert any("nan" in e.lower() or "inf" in e.lower() for e in report.errors)

    def test_validate_detects_inf_in_damping(self, valid_diffraction_results):
        """Test that Inf in damping is flagged as error."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        # Insert Inf
        valid_diffraction_results.damping.matrices[0].matrix[0, 0] = np.inf

        validator = CoefficientValidator(check_physical_limits=True)
        report = validator.validate(valid_diffraction_results)

        assert report.is_valid is False
        assert any("nan" in e.lower() or "inf" in e.lower() for e in report.errors)

    def test_validate_detects_nan_in_rao(self, valid_diffraction_results):
        """Test that NaN in RAO magnitude is flagged as error."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        # Insert NaN in RAO
        valid_diffraction_results.raos.surge.magnitude[0, 0] = np.nan

        validator = CoefficientValidator(check_physical_limits=True)
        report = validator.validate(valid_diffraction_results)

        assert report.is_valid is False
        assert any("nan" in e.lower() or "inf" in e.lower() for e in report.errors)

    def test_validate_detects_negative_rao_magnitude(self, valid_diffraction_results):
        """Test that negative RAO magnitude is flagged as warning."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        # Set negative magnitude
        valid_diffraction_results.raos.surge.magnitude[0, 0] = -1.0

        validator = CoefficientValidator()
        report = validator.validate(valid_diffraction_results)

        assert any("negative" in w.lower() for w in report.warnings)

    def test_validate_handles_missing_added_mass(self, valid_diffraction_results):
        """Test validation handles missing added mass gracefully."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        valid_diffraction_results.added_mass = None

        validator = CoefficientValidator()
        report = validator.validate(valid_diffraction_results)

        # Should have warning but not fail
        assert any("added mass" in w.lower() for w in report.warnings)

    def test_validate_handles_missing_damping(self, valid_diffraction_results):
        """Test validation handles missing damping gracefully."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        valid_diffraction_results.damping = None

        validator = CoefficientValidator()
        report = validator.validate(valid_diffraction_results)

        # Should have warning but not fail
        assert any("damping" in w.lower() for w in report.warnings)


class TestCoefficientValidatorConvenienceFunction:
    """Tests for validate_coefficients convenience function."""

    @pytest.fixture
    def valid_diffraction_results(self):
        """Reuse fixture from TestCoefficientValidator."""
        from digitalmodel.diffraction import (
            DiffractionResults,
            RAOSet,
            RAOComponent,
            AddedMassSet,
            DampingSet,
            HydrodynamicMatrix,
            FrequencyData,
            HeadingData,
            DOF,
        )

        frequencies = np.linspace(0.1, 2.0, 5)
        headings = np.array([0.0, 90.0, 180.0])
        n_freq = len(frequencies)
        n_head = len(headings)

        freq_data = FrequencyData(
            values=frequencies,
            periods=2 * np.pi / frequencies,
            count=n_freq,
            min_freq=frequencies.min(),
            max_freq=frequencies.max(),
        )

        head_data = HeadingData(
            values=headings,
            count=n_head,
            min_heading=headings.min(),
            max_heading=headings.max(),
        )

        def create_rao_component(dof: DOF) -> RAOComponent:
            return RAOComponent(
                dof=dof,
                magnitude=np.random.rand(n_freq, n_head) * 2.0,
                phase=np.random.rand(n_freq, n_head) * 180.0,
                frequencies=freq_data,
                headings=head_data,
                unit="m/m" if dof.value <= 3 else "deg/m",
            )

        rao_set = RAOSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            surge=create_rao_component(DOF.SURGE),
            sway=create_rao_component(DOF.SWAY),
            heave=create_rao_component(DOF.HEAVE),
            roll=create_rao_component(DOF.ROLL),
            pitch=create_rao_component(DOF.PITCH),
            yaw=create_rao_component(DOF.YAW),
            created_date=datetime.now().isoformat(),
        )

        added_mass_matrices = []
        for freq in frequencies:
            base = np.random.rand(6, 6) * 1000
            symmetric = (base + base.T) / 2
            np.fill_diagonal(symmetric, np.abs(np.diag(symmetric)) + 100)
            added_mass_matrices.append(
                HydrodynamicMatrix(
                    matrix=symmetric,
                    frequency=freq,
                    matrix_type="added_mass",
                    units={"linear": "kg", "angular": "kg.m^2"},
                )
            )

        added_mass = AddedMassSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            matrices=added_mass_matrices,
            frequencies=freq_data,
            created_date=datetime.now().isoformat(),
        )

        damping_matrices = []
        for freq in frequencies:
            base = np.random.rand(6, 6) * 100
            symmetric = (base + base.T) / 2
            np.fill_diagonal(symmetric, np.abs(np.diag(symmetric)) + 10)
            damping_matrices.append(
                HydrodynamicMatrix(
                    matrix=symmetric,
                    frequency=freq,
                    matrix_type="damping",
                    units={"linear": "N.s/m", "angular": "N.m.s/rad"},
                )
            )

        damping = DampingSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            matrices=damping_matrices,
            frequencies=freq_data,
            created_date=datetime.now().isoformat(),
        )

        return DiffractionResults(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            raos=rao_set,
            added_mass=added_mass,
            damping=damping,
            created_date=datetime.now().isoformat(),
        )

    def test_validate_coefficients_function_exists(self):
        """Test validate_coefficients function is importable."""
        from digitalmodel.bemrosetta.validators import validate_coefficients

        assert callable(validate_coefficients)

    def test_validate_coefficients_returns_report(self, valid_diffraction_results):
        """Test validate_coefficients returns ValidationReport."""
        from digitalmodel.bemrosetta.validators import validate_coefficients
        from digitalmodel.bemrosetta.core.interfaces import ValidationReport

        report = validate_coefficients(valid_diffraction_results)
        assert isinstance(report, ValidationReport)

    def test_validate_coefficients_strict_mode(self, valid_diffraction_results):
        """Test validate_coefficients strict mode enables positive definite check."""
        from digitalmodel.bemrosetta.validators import validate_coefficients

        # Make diagonal slightly negative (would pass in non-strict)
        valid_diffraction_results.added_mass.matrices[0].matrix[0, 0] = -50

        report_strict = validate_coefficients(valid_diffraction_results, strict=True)
        # In strict mode, negative diagonal should cause error
        assert report_strict.is_valid is False


class TestCausalityChecker:
    """Tests for CausalityChecker class (Kramers-Kronig validation)."""

    @pytest.fixture
    def causal_diffraction_results(self):
        """Create diffraction results that satisfy causality."""
        from digitalmodel.diffraction import (
            DiffractionResults,
            RAOSet,
            RAOComponent,
            AddedMassSet,
            DampingSet,
            HydrodynamicMatrix,
            FrequencyData,
            HeadingData,
            DOF,
        )

        # Use more frequencies for better integration
        frequencies = np.linspace(0.1, 3.0, 20)
        headings = np.array([0.0, 90.0, 180.0])
        n_freq = len(frequencies)
        n_head = len(headings)

        freq_data = FrequencyData(
            values=frequencies,
            periods=2 * np.pi / frequencies,
            count=n_freq,
            min_freq=frequencies.min(),
            max_freq=frequencies.max(),
        )

        head_data = HeadingData(
            values=headings,
            count=n_head,
            min_heading=headings.min(),
            max_heading=headings.max(),
        )

        def create_rao_component(dof: DOF) -> RAOComponent:
            return RAOComponent(
                dof=dof,
                magnitude=np.random.rand(n_freq, n_head) * 2.0,
                phase=np.random.rand(n_freq, n_head) * 180.0,
                frequencies=freq_data,
                headings=head_data,
                unit="m/m" if dof.value <= 3 else "deg/m",
            )

        rao_set = RAOSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            surge=create_rao_component(DOF.SURGE),
            sway=create_rao_component(DOF.SWAY),
            heave=create_rao_component(DOF.HEAVE),
            roll=create_rao_component(DOF.ROLL),
            pitch=create_rao_component(DOF.PITCH),
            yaw=create_rao_component(DOF.YAW),
            created_date=datetime.now().isoformat(),
        )

        # Create physically consistent added mass and damping
        # A(w) = A_inf + integral of B(w')/w' (simplified K-K relation)
        added_mass_matrices = []
        damping_matrices = []

        # Base infinite frequency added mass
        a_inf = np.eye(6) * 1000

        for freq in frequencies:
            # Simple damping model: B = B0 / (1 + (w/w0)^2)
            w0 = 1.0
            b0 = 500.0
            damping_factor = b0 / (1 + (freq / w0) ** 2)
            damping_matrix = np.eye(6) * damping_factor

            # Added mass follows from K-K relation (simplified)
            added_mass_factor = 1000 + 200 * np.arctan(1.0 / freq)
            added_mass_matrix = np.eye(6) * added_mass_factor

            added_mass_matrices.append(
                HydrodynamicMatrix(
                    matrix=added_mass_matrix,
                    frequency=freq,
                    matrix_type="added_mass",
                    units={"linear": "kg", "angular": "kg.m^2"},
                )
            )
            damping_matrices.append(
                HydrodynamicMatrix(
                    matrix=damping_matrix,
                    frequency=freq,
                    matrix_type="damping",
                    units={"linear": "N.s/m", "angular": "N.m.s/rad"},
                )
            )

        added_mass = AddedMassSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            matrices=added_mass_matrices,
            frequencies=freq_data,
            created_date=datetime.now().isoformat(),
        )

        damping = DampingSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            matrices=damping_matrices,
            frequencies=freq_data,
            created_date=datetime.now().isoformat(),
        )

        return DiffractionResults(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            raos=rao_set,
            added_mass=added_mass,
            damping=damping,
            created_date=datetime.now().isoformat(),
        )

    def test_causality_checker_can_instantiate(self):
        """Test that CausalityChecker can be instantiated."""
        from digitalmodel.bemrosetta.validators import CausalityChecker

        checker = CausalityChecker()
        assert checker is not None

    def test_causality_checker_with_default_options(self):
        """Test CausalityChecker uses sensible defaults."""
        from digitalmodel.bemrosetta.validators import CausalityChecker

        checker = CausalityChecker()
        assert checker.tolerance == pytest.approx(0.1)
        assert checker.n_integration_points == 1000

    def test_causality_checker_with_custom_options(self):
        """Test CausalityChecker accepts custom options."""
        from digitalmodel.bemrosetta.validators import CausalityChecker

        checker = CausalityChecker(tolerance=0.2, n_integration_points=500)
        assert checker.tolerance == pytest.approx(0.2)
        assert checker.n_integration_points == 500

    def test_causality_checker_implements_interface(self):
        """Test CausalityChecker implements ValidatorInterface."""
        from digitalmodel.bemrosetta.validators import CausalityChecker
        from digitalmodel.bemrosetta.core.interfaces import ValidatorInterface

        checker = CausalityChecker()
        assert isinstance(checker, ValidatorInterface)

    def test_validate_returns_validation_report(self, causal_diffraction_results):
        """Test validate() returns ValidationReport."""
        from digitalmodel.bemrosetta.validators import CausalityChecker
        from digitalmodel.bemrosetta.core.interfaces import ValidationReport

        checker = CausalityChecker()
        report = checker.validate(causal_diffraction_results)

        assert isinstance(report, ValidationReport)

    def test_is_valid_returns_bool(self, causal_diffraction_results):
        """Test is_valid() returns boolean."""
        from digitalmodel.bemrosetta.validators import CausalityChecker

        checker = CausalityChecker()
        result = checker.is_valid(causal_diffraction_results)

        assert isinstance(result, bool)

    def test_validate_handles_missing_added_mass(self, causal_diffraction_results):
        """Test validation handles missing added mass gracefully."""
        from digitalmodel.bemrosetta.validators import CausalityChecker

        causal_diffraction_results.added_mass = None

        checker = CausalityChecker()
        report = checker.validate(causal_diffraction_results)

        # Should have warning but not crash
        assert any("added mass" in w.lower() or "damping" in w.lower() for w in report.warnings)

    def test_validate_handles_missing_damping(self, causal_diffraction_results):
        """Test validation handles missing damping gracefully."""
        from digitalmodel.bemrosetta.validators import CausalityChecker

        causal_diffraction_results.damping = None

        checker = CausalityChecker()
        report = checker.validate(causal_diffraction_results)

        # Should have warning but not crash
        assert any("added mass" in w.lower() or "damping" in w.lower() for w in report.warnings)

    def test_validate_provides_kk_error_metrics(self, causal_diffraction_results):
        """Test validation provides Kramers-Kronig error metrics."""
        from digitalmodel.bemrosetta.validators import CausalityChecker

        checker = CausalityChecker()
        report = checker.validate(causal_diffraction_results)

        # Should have KK error metrics in info
        assert any("kk_error" in str(k) for k in report.metrics.keys())

    def test_validate_detects_non_causal_data(self, causal_diffraction_results):
        """Test detection of non-causal (physically inconsistent) data."""
        from digitalmodel.bemrosetta.validators import CausalityChecker

        # Make data non-causal by introducing inconsistency
        # Reverse the trend of added mass (should decrease with freq, make it increase)
        for i, matrix in enumerate(causal_diffraction_results.added_mass.matrices):
            # Reverse the natural trend
            matrix.matrix = np.eye(6) * (500 + i * 200)

        checker = CausalityChecker(tolerance=0.05)
        report = checker.validate(causal_diffraction_results)

        # Should have warnings about K-K violation
        # Note: may or may not fail depending on severity
        # At least metrics should show higher error
        has_high_kk_error = any(
            isinstance(v, float) and v > 0.05
            for k, v in report.metrics.items()
            if "kk_error" in k
        )
        has_kk_warning = any("kramers" in w.lower() for w in report.warnings)

        assert has_high_kk_error or has_kk_warning


class TestCausalityCheckerConvenienceFunction:
    """Tests for check_causality convenience function."""

    @pytest.fixture
    def causal_diffraction_results(self):
        """Create minimal diffraction results for testing."""
        from digitalmodel.diffraction import (
            DiffractionResults,
            RAOSet,
            RAOComponent,
            AddedMassSet,
            DampingSet,
            HydrodynamicMatrix,
            FrequencyData,
            HeadingData,
            DOF,
        )

        frequencies = np.linspace(0.1, 2.0, 10)
        headings = np.array([0.0, 90.0, 180.0])
        n_freq = len(frequencies)
        n_head = len(headings)

        freq_data = FrequencyData(
            values=frequencies,
            periods=2 * np.pi / frequencies,
            count=n_freq,
            min_freq=frequencies.min(),
            max_freq=frequencies.max(),
        )

        head_data = HeadingData(
            values=headings,
            count=n_head,
            min_heading=headings.min(),
            max_heading=headings.max(),
        )

        def create_rao_component(dof: DOF) -> RAOComponent:
            return RAOComponent(
                dof=dof,
                magnitude=np.random.rand(n_freq, n_head) * 2.0,
                phase=np.random.rand(n_freq, n_head) * 180.0,
                frequencies=freq_data,
                headings=head_data,
                unit="m/m" if dof.value <= 3 else "deg/m",
            )

        rao_set = RAOSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            surge=create_rao_component(DOF.SURGE),
            sway=create_rao_component(DOF.SWAY),
            heave=create_rao_component(DOF.HEAVE),
            roll=create_rao_component(DOF.ROLL),
            pitch=create_rao_component(DOF.PITCH),
            yaw=create_rao_component(DOF.YAW),
            created_date=datetime.now().isoformat(),
        )

        added_mass_matrices = []
        damping_matrices = []

        for freq in frequencies:
            added_mass_matrices.append(
                HydrodynamicMatrix(
                    matrix=np.eye(6) * 1000,
                    frequency=freq,
                    matrix_type="added_mass",
                    units={"linear": "kg"},
                )
            )
            damping_matrices.append(
                HydrodynamicMatrix(
                    matrix=np.eye(6) * 100,
                    frequency=freq,
                    matrix_type="damping",
                    units={"linear": "N.s/m"},
                )
            )

        added_mass = AddedMassSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            matrices=added_mass_matrices,
            frequencies=freq_data,
            created_date=datetime.now().isoformat(),
        )

        damping = DampingSet(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            matrices=damping_matrices,
            frequencies=freq_data,
            created_date=datetime.now().isoformat(),
        )

        return DiffractionResults(
            vessel_name="TestVessel",
            analysis_tool="OrcaWave",
            water_depth=100.0,
            raos=rao_set,
            added_mass=added_mass,
            damping=damping,
            created_date=datetime.now().isoformat(),
        )

    def test_check_causality_function_exists(self):
        """Test check_causality function is importable."""
        from digitalmodel.bemrosetta.validators import check_causality

        assert callable(check_causality)

    def test_check_causality_returns_report(self, causal_diffraction_results):
        """Test check_causality returns ValidationReport."""
        from digitalmodel.bemrosetta.validators import check_causality
        from digitalmodel.bemrosetta.core.interfaces import ValidationReport

        report = check_causality(causal_diffraction_results)
        assert isinstance(report, ValidationReport)


class TestValidatorsModuleExports:
    """Tests for validators module exports."""

    def test_coefficient_validator_exported(self):
        """Test CoefficientValidator is exported from validators module."""
        from digitalmodel.bemrosetta.validators import CoefficientValidator

        assert CoefficientValidator is not None

    def test_causality_checker_exported(self):
        """Test CausalityChecker is exported from validators module."""
        from digitalmodel.bemrosetta.validators import CausalityChecker

        assert CausalityChecker is not None

    def test_validate_coefficients_exported(self):
        """Test validate_coefficients is exported from validators module."""
        from digitalmodel.bemrosetta.validators import validate_coefficients

        assert validate_coefficients is not None

    def test_check_causality_exported(self):
        """Test check_causality is exported from validators module."""
        from digitalmodel.bemrosetta.validators import check_causality

        assert check_causality is not None
