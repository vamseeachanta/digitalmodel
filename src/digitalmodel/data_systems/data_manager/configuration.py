"""
Configuration Management Module for API STD 2RD Analysis

This module provides secure configuration loading and management capabilities
with YAML safe loading and cross-platform path handling.

Author: Digitally Modernized from Legacy Code
Date: 2025-01-15
"""

import yaml
from pathlib import Path
from typing import Dict, Any, Optional, Union, List
import logging
import pandas as pd
from copy import deepcopy

logger = logging.getLogger(__name__)


class ConfigurationManager:
    """
    Manages configuration loading, validation, and updates for API STD 2RD analysis.
    """

    def __init__(self, default_config_file: Union[str, Path, None] = None):
        """
        Initialize configuration manager.

        Args:
            default_config_file: Path to default configuration file
        """
        self.default_config_file = Path(default_config_file) if default_config_file else None
        self.config: Dict[str, Any] = {}
        self._original_config: Dict[str, Any] = {}

    def load_configuration(
        self,
        default_file: Union[str, Path],
        update_file: Optional[Union[str, Path]] = None
    ) -> Dict[str, Any]:
        """
        Load configuration from YAML files with secure loading.

        Args:
            default_file: Path to default configuration file
            update_file: Optional path to update configuration file

        Returns:
            Merged configuration dictionary

        Raises:
            FileNotFoundError: If default file doesn't exist
            yaml.YAMLError: If YAML parsing fails
        """
        default_path = Path(default_file)

        if not default_path.exists():
            raise FileNotFoundError(f"Default configuration file not found: {default_path}")

        # Load default configuration with safe loading
        try:
            with open(default_path, 'r', encoding='utf-8') as file:
                self.config = yaml.safe_load(file) or {}
                self._original_config = deepcopy(self.config)
                logger.info(f"Loaded default configuration from: {default_path}")
        except yaml.YAMLError as e:
            logger.error(f"Error parsing default YAML file {default_path}: {e}")
            raise

        # Load and merge update configuration if provided
        if update_file:
            update_path = Path(update_file)
            if update_path.exists():
                try:
                    with open(update_path, 'r', encoding='utf-8') as file:
                        update_config = yaml.safe_load(file) or {}
                        self._merge_configurations(update_config)
                        logger.info(f"Merged update configuration from: {update_path}")
                except yaml.YAMLError as e:
                    logger.error(f"Error parsing update YAML file {update_path}: {e}")
                    logger.warning("Continuing with default configuration only")
            else:
                logger.warning(f"Update configuration file not found: {update_path}")

        return self.config

    def _merge_configurations(self, update_config: Dict[str, Any]) -> None:
        """
        Recursively merge update configuration into base configuration.

        Args:
            update_config: Configuration updates to merge
        """
        def deep_merge(base: Dict[str, Any], update: Dict[str, Any]) -> None:
            for key, value in update.items():
                if key in base and isinstance(base[key], dict) and isinstance(value, dict):
                    deep_merge(base[key], value)
                else:
                    base[key] = value

        deep_merge(self.config, update_config)

    def get_configured_values(self) -> Dict[str, Any]:
        """
        Get the current configuration values.

        Returns:
            Current configuration dictionary
        """
        return self.config

    def update_geometry_properties(self) -> None:
        """
        Update geometry properties based on available parameters.

        Calculates missing diameter when two of three parameters (OD, ID, WT) are provided.
        """
        if 'geometry' not in self.config:
            return

        geometry = self.config['geometry']

        # Calculate missing diameter
        if geometry.get('NominalOD') is not None and geometry.get('DesignWT') is not None:
            if geometry.get('NominalID') is None:
                geometry['NominalID'] = geometry['NominalOD'] - 2 * geometry['DesignWT']
                logger.debug("Calculated NominalID from OD and WT")
        elif geometry.get('NominalID') is not None and geometry.get('DesignWT') is not None:
            if geometry.get('NominalOD') is None:
                geometry['NominalOD'] = geometry['NominalID'] + 2 * geometry['DesignWT']
                logger.debug("Calculated NominalOD from ID and WT")

    def update_plot_settings(self) -> None:
        """
        Update plot settings based on geometry configuration.
        """
        if not all(key in self.config for key in ['geometry', 'plotSettings']):
            return

        geometry = self.config['geometry']
        plot_settings = self.config.get('plotSettings', {}).get('variableWTBurst', {})

        if not plot_settings:
            return

        # Determine diameter type and value for plot naming
        if geometry.get('NominalOD') is not None:
            diameter_type = 'OD'
            diameter_value = geometry['NominalOD']
        elif geometry.get('NominalID') is not None:
            diameter_type = 'ID'
            diameter_value = geometry['NominalID']
        else:
            return

        corrosion_allowance = geometry.get('CorrosionAllowance', 0)

        # Update plot filename
        if 'plotFileName' in plot_settings:
            filename_template = plot_settings['plotFileName']
            try:
                plot_settings['plotFileName'] = filename_template.format(
                    diameter_type, diameter_value, corrosion_allowance
                ).replace('.', 'p')
            except (KeyError, ValueError) as e:
                logger.warning(f"Could not format plot filename: {e}")

        # Update plot title
        if 'pltTitle' in plot_settings:
            title_template = plot_settings['pltTitle']
            try:
                plot_settings['pltTitle'] = title_template.format(
                    diameter_value, corrosion_allowance, diameter_type
                )
            except (KeyError, ValueError) as e:
                logger.warning(f"Could not format plot title: {e}")

    def get_analysis_input_data(self, method_index: int = 0) -> Dict[str, Any]:
        """
        Prepare input data for API STD 2RD analysis.

        Args:
            method_index: Index for method-specific data (for multiple load cases)

        Returns:
            Dictionary containing formatted input data for analysis

        Raises:
            KeyError: If required configuration keys are missing
        """
        try:
            # Extract geometry data
            geometry = self.config['geometry']
            material = self.config['material']
            design_factors = self.config['designFactors']

            # Get method-specific data
            method_data_key = 'nominalWTAPISTD2RDMethod1'
            if method_data_key in self.config and 'data' in self.config[method_data_key]:
                method_data = self.config[method_data_key]['data'][method_index]
            else:
                method_data = {}

            input_data = {
                "OD": geometry.get('NominalOD'),
                "ID": geometry.get('NominalID'),
                "t": geometry.get('DesignWT'),
                "E": material.get('E'),
                "Poissionsratio": material.get('Poissionsratio'),
                "alphafab": material.get('alphafab'),
                "k": material.get('k'),
                "S": material.get('SMYS'),
                "U": material.get('SMUS'),
                "CorrosionAllowance": method_data.get('CorrosionAllowance', 0),
                "designFactors": design_factors,
                "externalPressure": method_data.get('ExternalPressure', 0),
                "internalPressure": method_data.get('InternalPressure', 0),
                "LimitState": method_data.get('LimitState', 'design')
            }

            # Validate required fields
            required_fields = ['OD', 'ID', 't', 'E', 'S', 'U']
            missing_fields = [field for field in required_fields if input_data[field] is None]

            if missing_fields:
                raise KeyError(f"Missing required configuration fields: {missing_fields}")

            return input_data

        except KeyError as e:
            logger.error(f"Configuration error when preparing input data: {e}")
            raise

    def extract_data_from_files(
        self,
        file_list: List[Union[str, Path]],
        extraction_config: Dict[str, Any]
    ) -> pd.DataFrame:
        """
        Extract data from multiple YAML files into a DataFrame.

        Args:
            file_list: List of file paths to process
            extraction_config: Configuration for data extraction

        Returns:
            DataFrame containing extracted data

        Raises:
            FileNotFoundError: If a file doesn't exist
            yaml.YAMLError: If YAML parsing fails
        """
        if 'columns' not in extraction_config or 'data' not in extraction_config:
            raise ValueError("Extraction config must contain 'columns' and 'data' keys")

        columns = extraction_config['columns']
        data_config = extraction_config['data']

        # Initialize DataFrame
        df = pd.DataFrame(columns=columns)

        for file_path in file_list:
            file_path = Path(file_path)

            if not file_path.exists():
                logger.warning(f"File not found: {file_path}")
                continue

            try:
                with open(file_path, 'r', encoding='utf-8') as file:
                    file_data = yaml.safe_load(file) or {}

                # Extract data according to configuration
                row_data = [str(file_path)]  # First column is filename

                for column_config in data_config:
                    try:
                        value = self._extract_nested_value(file_data, column_config)
                        row_data.append(value)
                    except KeyError:
                        logger.warning(f"Could not extract data for config {column_config} from {file_path}")
                        row_data.append(None)

                # Add row to DataFrame
                df.loc[len(df)] = row_data

            except yaml.YAMLError as e:
                logger.error(f"Error parsing YAML file {file_path}: {e}")
                continue

        return df

    def _extract_nested_value(self, data: Dict[str, Any], config: Dict[str, Any]) -> Any:
        """
        Extract nested value from data using hierarchical configuration.

        Args:
            data: Source data dictionary
            config: Configuration with L1, L2, L3 keys for nesting levels

        Returns:
            Extracted value

        Raises:
            KeyError: If path doesn't exist in data
        """
        current = data

        for level in ['L1', 'L2', 'L3']:
            if config.get(level) is not None:
                current = current[config[level]]
            else:
                break

        return current

    def validate_configuration(self) -> List[str]:
        """
        Validate the current configuration for completeness and consistency.

        Returns:
            List of validation warnings/errors
        """
        warnings = []

        # Check required sections
        required_sections = ['geometry', 'material', 'designFactors']
        for section in required_sections:
            if section not in self.config:
                warnings.append(f"Missing required configuration section: {section}")

        # Check geometry consistency
        if 'geometry' in self.config:
            geometry = self.config['geometry']
            od = geometry.get('NominalOD')
            id_val = geometry.get('NominalID')
            wt = geometry.get('DesignWT')

            if od is not None and id_val is not None and wt is not None:
                expected_wt = (od - id_val) / 2
                if abs(wt - expected_wt) > 1e-6:
                    warnings.append(f"Geometry inconsistency: WT={wt}, but calculated from OD/ID is {expected_wt}")

        # Check material properties
        if 'material' in self.config:
            material = self.config['material']
            required_props = ['E', 'SMYS', 'SMUS']
            for prop in required_props:
                if material.get(prop) is None or material.get(prop) <= 0:
                    warnings.append(f"Invalid or missing material property: {prop}")

        return warnings

    def reset_to_defaults(self) -> None:
        """Reset configuration to original default values."""
        self.config = deepcopy(self._original_config)
        logger.info("Configuration reset to default values")

    def save_configuration(self, file_path: Union[str, Path]) -> None:
        """
        Save current configuration to a YAML file.

        Args:
            file_path: Path where to save the configuration
        """
        file_path = Path(file_path)

        try:
            file_path.parent.mkdir(parents=True, exist_ok=True)
            with open(file_path, 'w', encoding='utf-8') as file:
                yaml.safe_dump(self.config, file, default_flow_style=False, indent=2)
            logger.info(f"Configuration saved to: {file_path}")
        except Exception as e:
            logger.error(f"Error saving configuration to {file_path}: {e}")
            raise


# Legacy compatibility functions
def ymlInput(default_yml: Union[str, Path], update_yml: Optional[Union[str, Path]] = None) -> Dict[str, Any]:
    """
    Legacy function for loading YAML configuration with secure loading.

    Args:
        default_yml: Path to default YAML file
        update_yml: Optional path to update YAML file

    Returns:
        Merged configuration dictionary
    """
    manager = ConfigurationManager()
    return manager.load_configuration(default_yml, update_yml)


def customUpdate(cfg: Dict[str, Any]) -> Dict[str, Any]:
    """
    Legacy function for custom configuration updates.

    Args:
        cfg: Configuration dictionary to update

    Returns:
        Updated configuration dictionary
    """
    manager = ConfigurationManager()
    manager.config = cfg
    manager.update_geometry_properties()
    manager.update_plot_settings()
    return manager.config


def loadConfiguration(file: Union[str, Path]) -> Dict[str, Any]:
    """
    Legacy function for loading configuration.

    Args:
        file: Configuration file path

    Returns:
        Configuration dictionary
    """
    manager = ConfigurationManager(file)
    return manager.load_configuration(file)