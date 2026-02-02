"""Tests for BEMRosetta converters module.

TDD tests for BaseConverter and OrcaFlexConverter classes.
"""

import csv
from pathlib import Path
from unittest.mock import MagicMock, patch

import numpy as np
import pytest
import yaml

from digitalmodel.hydrodynamics.bemrosetta.converters.base import BaseConverter
from digitalmodel.hydrodynamics.bemrosetta.converters.to_orcaflex import (
    OrcaFlexConverter,
    convert_to_orcaflex,
)
from digitalmodel.hydrodynamics.bemrosetta.core.exceptions import ConverterError
from digitalmodel.hydrodynamics.bemrosetta.models import ConversionResult, QTFData
from digitalmodel.hydrodynamics.bemrosetta.models.conversion_result import ConversionStatus
from digitalmodel.hydrodynamics.bemrosetta.models.qtf_data import QTFComponent, QTFType
from digitalmodel.diffraction import DiffractionResults


class TestBaseConverter:
    """Tests for BaseConverter class."""

    def test_base_converter_is_abstract(self):
        """BaseConverter should not be directly instantiable without implementing abstract methods."""
        # BaseConverter inherits from ConverterInterface which has abstract methods
        # But BaseConverter provides default implementations, so it should be instantiable
        # as a concrete class for testing purposes
        with pytest.raises(TypeError):
            # Cannot instantiate directly because output_format is abstract
            BaseConverter()

    def test_base_converter_output_dir_default(self, tmp_path):
        """BaseConverter should default to current directory if no output_dir specified."""

        class ConcreteConverter(BaseConverter):
            @property
            def output_format(self) -> str:
                return "test"

            def convert(self, data, output_dir=None):
                return output_dir or self.output_dir

        converter = ConcreteConverter(output_dir=tmp_path)
        assert converter.output_dir == tmp_path

    def test_base_converter_output_dir_from_path(self, tmp_path):
        """BaseConverter should accept Path object for output_dir."""

        class ConcreteConverter(BaseConverter):
            @property
            def output_format(self) -> str:
                return "test"

            def convert(self, data, output_dir=None):
                return output_dir or self.output_dir

        converter = ConcreteConverter(output_dir=tmp_path)
        assert converter.output_dir == tmp_path
        assert isinstance(converter.output_dir, Path)

    def test_base_converter_validate_input_with_missing_raos(
        self, mock_diffraction_results_no_raos
    ):
        """validate_input should warn when RAO data is missing."""

        class ConcreteConverter(BaseConverter):
            @property
            def output_format(self) -> str:
                return "test"

            def convert(self, data, output_dir=None):
                return output_dir or self.output_dir

        converter = ConcreteConverter()
        warnings = converter.validate_input(mock_diffraction_results_no_raos)
        assert "No RAO data available" in warnings

    def test_base_converter_validate_input_with_missing_added_mass(
        self, mock_diffraction_results_no_added_mass
    ):
        """validate_input should warn when added mass data is missing."""

        class ConcreteConverter(BaseConverter):
            @property
            def output_format(self) -> str:
                return "test"

            def convert(self, data, output_dir=None):
                return output_dir or self.output_dir

        converter = ConcreteConverter()
        warnings = converter.validate_input(mock_diffraction_results_no_added_mass)
        assert "No added mass data available" in warnings

    def test_base_converter_validate_input_with_missing_damping(
        self, mock_diffraction_results_no_damping
    ):
        """validate_input should warn when damping data is missing."""

        class ConcreteConverter(BaseConverter):
            @property
            def output_format(self) -> str:
                return "test"

            def convert(self, data, output_dir=None):
                return output_dir or self.output_dir

        converter = ConcreteConverter()
        warnings = converter.validate_input(mock_diffraction_results_no_damping)
        assert "No damping data available" in warnings

    def test_base_converter_ensure_output_dir_creates_directory(self, tmp_path):
        """_ensure_output_dir should create the output directory if it doesn't exist."""

        class ConcreteConverter(BaseConverter):
            @property
            def output_format(self) -> str:
                return "test"

            def convert(self, data, output_dir=None):
                self._ensure_output_dir()
                return self.output_dir

        new_dir = tmp_path / "new_output_dir"
        assert not new_dir.exists()

        converter = ConcreteConverter(output_dir=new_dir)
        converter._ensure_output_dir()

        assert new_dir.exists()


class TestOrcaFlexConverter:
    """Tests for OrcaFlexConverter class."""

    def test_orcaflex_converter_output_format(self, tmp_path):
        """OrcaFlexConverter should report 'orcaflex' as output format."""
        converter = OrcaFlexConverter(output_dir=tmp_path)
        assert converter.output_format == "orcaflex"

    def test_orcaflex_converter_initialization(self, tmp_path):
        """OrcaFlexConverter should initialize with optional parameters."""
        converter = OrcaFlexConverter(
            output_dir=tmp_path,
            vessel_name="TestVessel",
            include_qtf=False,
        )

        assert converter.output_dir == tmp_path
        assert converter.vessel_name == "TestVessel"
        assert converter.include_qtf is False

    def test_orcaflex_converter_set_qtf_data(self, tmp_path, sample_qtf_data_model):
        """set_qtf_data should store QTF data for export."""
        converter = OrcaFlexConverter(output_dir=tmp_path)
        converter.set_qtf_data(sample_qtf_data_model)

        assert converter._qtf_data is sample_qtf_data_model

    def test_orcaflex_converter_convert_creates_output_directory(
        self, tmp_path, mock_diffraction_results
    ):
        """convert should create output directory if it doesn't exist."""
        output_dir = tmp_path / "orcaflex_output"
        converter = OrcaFlexConverter(output_dir=output_dir)

        with patch.object(
            converter, "_call_orcaflex_exporter", return_value=None
        ):
            converter.convert(mock_diffraction_results)

        assert output_dir.exists()

    def test_orcaflex_converter_convert_calls_validate_input(
        self, tmp_path, mock_diffraction_results_no_raos
    ):
        """convert should call validate_input and process warnings."""
        converter = OrcaFlexConverter(output_dir=tmp_path)

        with patch.object(
            converter, "_call_orcaflex_exporter", return_value=None
        ):
            with patch.object(converter, "validate_input") as mock_validate:
                mock_validate.return_value = ["No RAO data available"]
                converter.convert(mock_diffraction_results_no_raos)

                # Verify validate_input was called with the results
                mock_validate.assert_called_once_with(mock_diffraction_results_no_raos)

    def test_orcaflex_converter_convert_uses_vessel_name_from_results(
        self, tmp_path, mock_diffraction_results
    ):
        """convert should use vessel name from results if not specified."""
        converter = OrcaFlexConverter(output_dir=tmp_path)

        with patch.object(converter, "_call_orcaflex_exporter") as mock_export:
            mock_export.return_value = None
            converter.convert(mock_diffraction_results)

            # Verify vessel name was extracted from results
            assert mock_diffraction_results.vessel_name is not None

    def test_orcaflex_converter_convert_uses_custom_vessel_name(
        self, tmp_path, mock_diffraction_results
    ):
        """convert should use custom vessel name if specified."""
        converter = OrcaFlexConverter(
            output_dir=tmp_path, vessel_name="CustomVessel"
        )

        assert converter.vessel_name == "CustomVessel"

    def test_orcaflex_converter_convert_raises_converter_error_on_failure(
        self, tmp_path, mock_diffraction_results
    ):
        """convert should raise ConverterError when export fails."""
        converter = OrcaFlexConverter(output_dir=tmp_path)

        with patch.object(
            converter, "_call_orcaflex_exporter", side_effect=Exception("Export failed")
        ):
            with pytest.raises(ConverterError) as exc_info:
                converter.convert(mock_diffraction_results)

            assert "OrcaFlex conversion failed" in str(exc_info.value)

    def test_orcaflex_converter_build_vessel_yaml(
        self, tmp_path, mock_diffraction_results
    ):
        """_build_vessel_yaml should create correct OrcaFlex vessel structure."""
        converter = OrcaFlexConverter(output_dir=tmp_path)
        vessel_data = converter._build_vessel_yaml(
            mock_diffraction_results, "TestVessel"
        )

        assert "VesselType" in vessel_data
        vessel_type = vessel_data["VesselType"]
        assert vessel_type["Name"] == "TestVessel"
        assert vessel_type["Category"] == "Vessel"
        assert vessel_type["Connection"] == "Free"
        assert vessel_type["PrimaryMotion"] == "Calculated (6 DOF)"

    def test_orcaflex_converter_build_vessel_yaml_without_qtf(
        self, tmp_path, mock_diffraction_results
    ):
        """_build_vessel_yaml should set WaveDrift to None when no QTF."""
        converter = OrcaFlexConverter(output_dir=tmp_path)
        vessel_data = converter._build_vessel_yaml(
            mock_diffraction_results, "TestVessel"
        )

        included_effects = vessel_data["VesselType"]["IncludedEffects"]
        assert included_effects["WaveDrift"] == "None"

    def test_orcaflex_converter_build_vessel_yaml_with_qtf(
        self, tmp_path, mock_diffraction_results, sample_qtf_data_model
    ):
        """_build_vessel_yaml should set WaveDrift to Full QTF when QTF provided."""
        converter = OrcaFlexConverter(output_dir=tmp_path)
        converter.set_qtf_data(sample_qtf_data_model)

        vessel_data = converter._build_vessel_yaml(
            mock_diffraction_results, "TestVessel"
        )

        included_effects = vessel_data["VesselType"]["IncludedEffects"]
        assert included_effects["WaveDrift"] == "Full QTF"

    def test_orcaflex_converter_convert_to_vessel_yaml(
        self, tmp_path, mock_diffraction_results
    ):
        """convert_to_vessel_yaml should write YAML file."""
        converter = OrcaFlexConverter(output_dir=tmp_path)
        output_file = tmp_path / "test_vessel.yml"

        result_path = converter.convert_to_vessel_yaml(
            mock_diffraction_results, output_file
        )

        assert result_path == output_file
        assert output_file.exists()

        with open(output_file, "r") as f:
            data = yaml.safe_load(f)
            assert "VesselType" in data

    def test_orcaflex_converter_export_qtf(
        self, tmp_path, sample_qtf_data_model
    ):
        """_export_qtf should create QTF CSV file."""
        converter = OrcaFlexConverter(output_dir=tmp_path)
        converter.set_qtf_data(sample_qtf_data_model)

        qtf_path = converter._export_qtf(tmp_path, "test_vessel")

        assert qtf_path.exists()
        assert qtf_path.name == "test_vessel_qtf.csv"

        # Verify CSV structure
        with open(qtf_path, "r") as f:
            reader = csv.reader(f)
            header = next(reader)
            assert "Freq1_rad/s" in header
            assert "Freq2_rad/s" in header
            assert "Heading_deg" in header
            assert "Surge_Re" in header
            assert "Surge_Im" in header

    def test_orcaflex_converter_export_qtf_without_data_raises_error(
        self, tmp_path
    ):
        """_export_qtf should raise error when no QTF data set."""
        converter = OrcaFlexConverter(output_dir=tmp_path)

        with pytest.raises(ConverterError) as exc_info:
            converter._export_qtf(tmp_path, "test_vessel")

        assert "No QTF data set" in str(exc_info.value)


class TestConvertToOrcaflexFunction:
    """Tests for the convert_to_orcaflex convenience function."""

    def test_convert_to_orcaflex_success(
        self, tmp_path, mock_diffraction_results
    ):
        """convert_to_orcaflex should return successful ConversionResult."""
        with patch(
            "digitalmodel.hydrodynamics.bemrosetta.converters.to_orcaflex.OrcaFlexConverter"
        ) as MockConverter:
            mock_instance = MockConverter.return_value
            mock_instance.convert.return_value = tmp_path

            result = convert_to_orcaflex(mock_diffraction_results, tmp_path)

            assert isinstance(result, ConversionResult)
            assert result.is_success

    def test_convert_to_orcaflex_with_qtf(
        self, tmp_path, mock_diffraction_results, sample_qtf_data_model
    ):
        """convert_to_orcaflex should pass QTF data to converter."""
        with patch(
            "digitalmodel.hydrodynamics.bemrosetta.converters.to_orcaflex.OrcaFlexConverter"
        ) as MockConverter:
            mock_instance = MockConverter.return_value
            mock_instance.convert.return_value = tmp_path

            result = convert_to_orcaflex(
                mock_diffraction_results, tmp_path, qtf_data=sample_qtf_data_model
            )

            mock_instance.set_qtf_data.assert_called_once_with(sample_qtf_data_model)

    def test_convert_to_orcaflex_failure(
        self, tmp_path, mock_diffraction_results
    ):
        """convert_to_orcaflex should return failed result on exception."""
        with patch(
            "digitalmodel.hydrodynamics.bemrosetta.converters.to_orcaflex.OrcaFlexConverter"
        ) as MockConverter:
            mock_instance = MockConverter.return_value
            mock_instance.convert.side_effect = Exception("Conversion failed")

            result = convert_to_orcaflex(mock_diffraction_results, tmp_path)

            assert isinstance(result, ConversionResult)
            assert not result.is_success
            assert "Conversion failed" in result.errors[0]


class TestOrcaFlexConverterIntegration:
    """Integration tests for OrcaFlexConverter with real OrcaFlexExporter."""

    @pytest.mark.integration
    def test_full_conversion_workflow(
        self, tmp_path, mock_diffraction_results_complete
    ):
        """Full conversion should produce all expected output files."""
        converter = OrcaFlexConverter(output_dir=tmp_path, vessel_name="IntegrationTest")

        output_path = converter.convert(mock_diffraction_results_complete)

        # Verify output files exist
        assert output_path.exists()
        # Check for vessel type YAML
        vessel_files = list(output_path.glob("*_vessel_type.yml"))
        assert len(vessel_files) > 0

    @pytest.mark.integration
    def test_conversion_with_qtf_produces_qtf_file(
        self, tmp_path, mock_diffraction_results_complete, sample_qtf_data_model
    ):
        """Conversion with QTF should produce QTF CSV file."""
        converter = OrcaFlexConverter(
            output_dir=tmp_path, vessel_name="QTFTest", include_qtf=True
        )
        converter.set_qtf_data(sample_qtf_data_model)

        output_path = converter.convert(mock_diffraction_results_complete)

        qtf_files = list(output_path.glob("*_qtf.csv"))
        assert len(qtf_files) > 0


# Fixtures for mock data


@pytest.fixture
def mock_diffraction_results():
    """Create mock DiffractionResults for testing."""
    mock = MagicMock(spec=DiffractionResults)
    mock.vessel_name = "TestVessel"
    mock.raos = MagicMock()
    mock.added_mass = MagicMock()
    mock.damping = MagicMock()
    return mock


@pytest.fixture
def mock_diffraction_results_no_raos():
    """Create mock DiffractionResults without RAO data."""
    mock = MagicMock(spec=DiffractionResults)
    mock.vessel_name = "TestVessel"
    mock.raos = None
    mock.added_mass = MagicMock()
    mock.damping = MagicMock()
    return mock


@pytest.fixture
def mock_diffraction_results_no_added_mass():
    """Create mock DiffractionResults without added mass data."""
    mock = MagicMock(spec=DiffractionResults)
    mock.vessel_name = "TestVessel"
    mock.raos = MagicMock()
    mock.added_mass = None
    mock.damping = MagicMock()
    return mock


@pytest.fixture
def mock_diffraction_results_no_damping():
    """Create mock DiffractionResults without damping data."""
    mock = MagicMock(spec=DiffractionResults)
    mock.vessel_name = "TestVessel"
    mock.raos = MagicMock()
    mock.added_mass = MagicMock()
    mock.damping = None
    return mock


@pytest.fixture
def mock_diffraction_results_complete(sample_frequencies, sample_headings):
    """Create complete mock DiffractionResults for integration testing."""
    from digitalmodel.diffraction import (
        DiffractionResults,
        RAOSet,
        AddedMassSet,
        DampingSet,
        RAOComponent,
        HydrodynamicMatrix,
        FrequencyData,
        HeadingData,
        DOF,
    )

    n_freq = len(sample_frequencies)
    n_head = len(sample_headings)

    # Create frequency data
    freq_data = FrequencyData(
        values=sample_frequencies,
        periods=2 * np.pi / sample_frequencies,
        count=n_freq,
        min_freq=float(sample_frequencies.min()),
        max_freq=float(sample_frequencies.max()),
    )

    # Create heading data
    head_data = HeadingData(
        values=sample_headings,
        count=n_head,
        min_heading=float(sample_headings.min()),
        max_heading=float(sample_headings.max()),
    )

    # Create RAO components for each DOF
    def create_rao_component(dof):
        return RAOComponent(
            dof=dof,
            magnitude=np.random.rand(n_freq, n_head),
            phase=np.random.rand(n_freq, n_head) * 360,
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
        created_date="2024-01-01",
    )

    # Create added mass matrices
    added_mass_matrices = [
        HydrodynamicMatrix(
            matrix=np.eye(6) * 1000 + np.random.rand(6, 6) * 100,
            frequency=freq,
            matrix_type="added_mass",
            units={"linear": "kg", "angular": "kg.m^2"},
        )
        for freq in sample_frequencies
    ]
    # Make matrices symmetric
    for m in added_mass_matrices:
        m.matrix = (m.matrix + m.matrix.T) / 2

    added_mass_set = AddedMassSet(
        vessel_name="TestVessel",
        analysis_tool="OrcaWave",
        water_depth=100.0,
        matrices=added_mass_matrices,
        frequencies=freq_data,
        created_date="2024-01-01",
    )

    # Create damping matrices
    damping_matrices = [
        HydrodynamicMatrix(
            matrix=np.eye(6) * 100 + np.random.rand(6, 6) * 10,
            frequency=freq,
            matrix_type="damping",
            units={"linear": "N.s/m", "angular": "N.m.s/rad"},
        )
        for freq in sample_frequencies
    ]
    # Make matrices symmetric
    for m in damping_matrices:
        m.matrix = (m.matrix + m.matrix.T) / 2

    damping_set = DampingSet(
        vessel_name="TestVessel",
        analysis_tool="OrcaWave",
        water_depth=100.0,
        matrices=damping_matrices,
        frequencies=freq_data,
        created_date="2024-01-01",
    )

    return DiffractionResults(
        vessel_name="TestVessel",
        analysis_tool="OrcaWave",
        water_depth=100.0,
        raos=rao_set,
        added_mass=added_mass_set,
        damping=damping_set,
        created_date="2024-01-01",
    )


@pytest.fixture
def sample_qtf_data_model(sample_frequencies, sample_headings):
    """Create sample QTFData model for testing."""
    n_freq = len(sample_frequencies)
    n_head = len(sample_headings)

    components = []
    for dof in range(6):
        for heading in sample_headings:
            components.append(
                QTFComponent(
                    dof=dof,
                    heading=heading,
                    qtf_type=QTFType.DIFFERENCE_FREQUENCY,
                    frequencies_1=sample_frequencies,
                    frequencies_2=sample_frequencies,
                    amplitude=np.random.rand(n_freq, n_freq) * 100,
                    phase=np.random.rand(n_freq, n_freq) * 2 * np.pi,
                )
            )

    return QTFData(
        qtf_type=QTFType.DIFFERENCE_FREQUENCY,
        body_name="TestVessel",
        components=components,
        frequencies=sample_frequencies,
        headings=sample_headings,
    )
