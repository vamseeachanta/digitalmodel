"""
ABOUTME: Level 2 OrcaFlex API validation
ABOUTME: Validates YAML files by loading them through OrcaFlex API and running static analysis
"""

from pathlib import Path
from typing import Optional, Tuple
import sys
from .models import Level2Result, ValidationStatus, SoftwareAvailability
from .utils import load_yaml_file


class Level2OrcaFlexValidator:
    """
    Level 2 validator using OrcaFlex API.

    Validates:
    - OrcaFlex software availability
    - File loading via OrcFxAPI
    - Static analysis and warnings
    - Model compatibility
    """

    def __init__(self):
        """Initialize Level 2 validator."""
        self.orcaflex_available = False
        self.orcaflex_version = None
        self.OrcFxAPI = None

        # Check OrcaFlex availability
        self._check_orcaflex_availability()

    def _check_orcaflex_availability(self) -> None:
        """
        Check if OrcaFlex is available and detect version.

        Sets:
            self.orcaflex_available: True if OrcaFlex can be imported
            self.orcaflex_version: Version string if available
            self.OrcFxAPI: Module reference if available
        """
        try:
            import OrcFxAPI
            self.OrcFxAPI = OrcFxAPI
            self.orcaflex_available = True

            # Try to get version
            try:
                self.orcaflex_version = OrcFxAPI.Version()
            except Exception:
                self.orcaflex_version = "Unknown"

        except ImportError:
            self.orcaflex_available = False
            self.orcaflex_version = None
        except Exception as e:
            self.orcaflex_available = False
            self.orcaflex_version = None
            print(f"Warning: OrcaFlex import failed: {e}")

    def validate(self, file_path: Path) -> Level2Result:
        """
        Run Level 2 OrcaFlex API validation.

        Args:
            file_path: Path to YAML file to validate

        Returns:
            Level2Result with validation findings
        """
        file_path = Path(file_path)

        result = Level2Result(
            orcaflex_loadable=False,
            orcaflex_available=SoftwareAvailability.UNKNOWN
        )

        # Check file existence
        if not file_path.exists():
            result.orcaflex_available = SoftwareAvailability.NO
            result.load_errors.append(f"File not found: {file_path}")
            result.status = ValidationStatus.FAIL
            return result

        # Check OrcaFlex availability
        if not self.orcaflex_available:
            result.orcaflex_available = SoftwareAvailability.NO
            result.orcaflex_version = None
            result.comments = "OrcaFlex not available - skipping API validation"
            result.status = ValidationStatus.SKIPPED
            return result

        # OrcaFlex is available
        result.orcaflex_available = SoftwareAvailability.YES
        result.orcaflex_version = self.orcaflex_version

        # Try to load the file with OrcaFlex
        success, model_data, error = self._load_with_orcaflex(file_path)

        if not success:
            result.orcaflex_loadable = False
            result.load_errors.append(error)
            result.status = ValidationStatus.FAIL
            return result

        # File loaded successfully
        result.orcaflex_loadable = True

        # Extract model information
        if model_data:
            result.model_stage = model_data.get('stage', 'Unknown')
            result.analysis_mode = model_data.get('mode', 'Unknown')
            result.warnings = model_data.get('warnings', [])
            result.object_count = model_data.get('object_count', 0)

        # Run static analysis if model loaded
        if result.orcaflex_loadable:
            analysis_result = self._run_static_analysis(file_path, model_data)
            result.static_analysis_passed = analysis_result['passed']
            result.warnings.extend(analysis_result.get('warnings', []))

            if analysis_result.get('errors'):
                result.load_errors.extend(analysis_result['errors'])

        # Determine overall status
        if result.orcaflex_loadable and result.static_analysis_passed:
            if result.warnings:
                result.status = ValidationStatus.WARN
                result.comments = f"{len(result.warnings)} warnings found"
            else:
                result.status = ValidationStatus.PASS
                result.comments = "File loaded successfully with no warnings"
        elif result.orcaflex_loadable and not result.static_analysis_passed:
            result.status = ValidationStatus.WARN
            result.comments = "File loaded but static analysis found issues"
        else:
            result.status = ValidationStatus.FAIL
            result.comments = "Failed to load file in OrcaFlex"

        return result

    def _load_with_orcaflex(self, file_path: Path) -> Tuple[bool, Optional[dict], Optional[str]]:
        """
        Load YAML file using OrcaFlex API.

        Args:
            file_path: Path to YAML file

        Returns:
            Tuple of (success, model_data, error_message)
        """
        if not self.orcaflex_available:
            return False, None, "OrcaFlex not available"

        try:
            # Create new model
            model = self.OrcFxAPI.Model()

            # Load the YAML file
            # Note: OrcaFlex expects .yml or .dat files
            # For modular YAML, we need to load it as a data file
            model.LoadData(str(file_path))

            # Extract model information
            model_data = {
                'stage': self._get_model_stage(model),
                'mode': self._get_analysis_mode(model),
                'warnings': self._get_model_warnings(model),
                'object_count': self._count_model_objects(model)
            }

            return True, model_data, None

        except Exception as e:
            error_msg = f"OrcaFlex loading error: {str(e)}"
            return False, None, error_msg

    def _get_model_stage(self, model) -> str:
        """
        Get model calculation stage.

        Args:
            model: OrcaFlex model object

        Returns:
            Stage name (e.g., 'Whole Simulation', 'Statics', 'Unknown')
        """
        try:
            # Get the model state
            state = model.state
            if state == self.OrcFxAPI.ModelState.Reset:
                return "Reset"
            elif state == self.OrcFxAPI.ModelState.CalculatingStatics:
                return "Calculating Statics"
            elif state == self.OrcFxAPI.ModelState.InStaticState:
                return "Static State"
            elif state == self.OrcFxAPI.ModelState.RunningSimulation:
                return "Running Simulation"
            elif state == self.OrcFxAPI.ModelState.SimulationStopped:
                return "Simulation Complete"
            else:
                return "Unknown"
        except Exception:
            return "Unknown"

    def _get_analysis_mode(self, model) -> str:
        """
        Get analysis mode from model.

        Args:
            model: OrcaFlex model object

        Returns:
            Analysis mode (e.g., 'Time Domain', 'Frequency Domain')
        """
        try:
            # Check general parameters
            general = model.general

            # OrcaFlex uses different analysis types
            # This is a simplified check - extend as needed
            if hasattr(general, 'StaticsMinDamping'):
                return "Time Domain (Dynamic)"
            else:
                return "Time Domain"

        except Exception:
            return "Unknown"

    def _get_model_warnings(self, model) -> list:
        """
        Extract warnings from model.

        Args:
            model: OrcaFlex model object

        Returns:
            List of warning messages
        """
        warnings = []

        try:
            # Try to calculate statics to generate warnings
            model.CalculateStatics()

            # Get warnings from calculation
            # Note: OrcaFlex warnings are typically in model.WarningMessages
            if hasattr(model, 'WarningMessages'):
                for warning in model.WarningMessages:
                    warnings.append(str(warning))

        except Exception as e:
            # If statics calculation fails, that's a warning itself
            warnings.append(f"Statics calculation issue: {str(e)}")

        return warnings

    def _count_model_objects(self, model) -> int:
        """
        Count objects in the model.

        Args:
            model: OrcaFlex model object

        Returns:
            Total object count
        """
        try:
            # Get all objects in the model
            objects = model.objects
            return len(objects) if objects else 0
        except Exception:
            return 0

    def _run_static_analysis(self, file_path: Path, model_data: Optional[dict]) -> dict:
        """
        Run static analysis on the model.

        Args:
            file_path: Path to YAML file
            model_data: Model data from loading

        Returns:
            Dictionary with analysis results
        """
        result = {
            'passed': True,
            'warnings': [],
            'errors': []
        }

        try:
            # Static analysis checks
            # 1. Check if model has minimum required objects
            if model_data and model_data.get('object_count', 0) == 0:
                result['warnings'].append("Model contains no objects")
                result['passed'] = False

            # 2. Check for stage appropriateness
            stage = model_data.get('stage', 'Unknown') if model_data else 'Unknown'
            if stage == 'Unknown':
                result['warnings'].append("Could not determine model stage")

            # 3. Check for existing warnings from OrcaFlex
            if model_data and model_data.get('warnings'):
                result['warnings'].extend(model_data['warnings'])

            # If we have critical errors, mark as not passed
            if result['errors']:
                result['passed'] = False

        except Exception as e:
            result['errors'].append(f"Static analysis error: {str(e)}")
            result['passed'] = False

        return result

    def get_software_info(self) -> dict:
        """
        Get OrcaFlex software information.

        Returns:
            Dictionary with software details
        """
        return {
            'available': self.orcaflex_available,
            'version': self.orcaflex_version,
            'python_version': f"{sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}"
        }
