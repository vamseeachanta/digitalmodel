"""
API STD 2RD Analysis Module

This module provides the main analysis class for API STD 2RD (American Petroleum Institute
Standard 2RD) pipe stress analysis, including burst pressure, collapse pressure, and
interaction analysis methods.

Author: Digitally Modernized from Legacy Code
Date: 2025-01-15
"""

import sys
import logging
from pathlib import Path
from typing import Dict, Any, List, Optional, Union, Tuple
import pandas as pd
import numpy as np

from digitalmodel.calculations.pipe_properties import calculate_geometric_properties
from digitalmodel.calculations.stress_calculations import (
    APISTD2RDCalculations,
    calculate_utilization,
    apply_temperature_derating
)
from digitalmodel.data_manager.configuration import ConfigurationManager

logger = logging.getLogger(__name__)


class APISTD2RDAnalyzer:
    """
    Main class for API STD 2RD stress analysis.

    This class provides comprehensive analysis capabilities including:
    - Variable wall thickness burst analysis
    - Nominal wall thickness Method 1 analysis
    - Collapse pressure analysis
    - Load interaction analysis
    """

    def __init__(self, config_manager: Optional[ConfigurationManager] = None):
        """
        Initialize the API STD 2RD analyzer.

        Args:
            config_manager: Optional configuration manager instance
        """
        self.config_manager = config_manager or ConfigurationManager()
        self.config: Dict[str, Any] = {}
        self.results: Dict[str, Any] = {}
        self.calculator = APISTD2RDCalculations()

    def load_configuration(
        self,
        default_config: Union[str, Path],
        update_config: Optional[Union[str, Path]] = None
    ) -> None:
        """
        Load analysis configuration from files.

        Args:
            default_config: Path to default configuration file
            update_config: Optional path to update configuration file
        """
        self.config = self.config_manager.load_configuration(default_config, update_config)
        self.config_manager.update_geometry_properties()
        self.config_manager.update_plot_settings()

        # Validate configuration
        warnings = self.config_manager.validate_configuration()
        if warnings:
            for warning in warnings:
                logger.warning(warning)

    def run_variable_wt_burst_analysis(self) -> Tuple[pd.DataFrame, Dict[str, Any]]:
        """
        Run variable wall thickness burst analysis.

        Returns:
            Tuple containing results DataFrame and updated configuration

        Raises:
            ValueError: If required configuration is missing
        """
        if not self.config:
            raise ValueError("Configuration must be loaded before running analysis")

        logger.info("Starting variable wall thickness burst analysis")

        # Prepare DataFrame
        header_columns = ['OD', 'ID', 't', 'Pd', 'Pa', 'Pt', 'Pb']
        results_df = pd.DataFrame(columns=header_columns)

        geometry = self.config.get('geometry', {})
        material = self.config.get('material', {})
        design_factors = self.config.get('designFactors', {})

        nominal_wt_list = geometry.get('NominalWT', [])
        if not nominal_wt_list:
            raise ValueError("NominalWT list is required for variable WT analysis")

        # Generate data for each wall thickness
        if geometry.get('NominalOD') is not None:
            # OD-based analysis
            nominal_od = geometry['NominalOD']
            for wt in nominal_wt_list:
                nominal_id = nominal_od - 2 * wt
                row_data = self._calculate_burst_pressures_for_geometry(
                    nominal_od, nominal_id, wt, material, design_factors
                )
                results_df.loc[len(results_df)] = [nominal_od, nominal_id, wt] + list(row_data.values())

        elif geometry.get('NominalID') is not None:
            # ID-based analysis
            nominal_id = geometry['NominalID']
            for wt in nominal_wt_list:
                nominal_od = nominal_id + 2 * wt
                row_data = self._calculate_burst_pressures_for_geometry(
                    nominal_od, nominal_id, wt, material, design_factors
                )
                results_df.loc[len(results_df)] = [nominal_od, nominal_id, wt] + list(row_data.values())

        else:
            raise ValueError("Either NominalOD or NominalID must be specified")

        logger.info(f"Variable WT burst analysis completed with {len(results_df)} cases")
        self.results['variable_wt_burst'] = results_df

        return results_df, self.config

    def _calculate_burst_pressures_for_geometry(
        self,
        od: float,
        id_val: float,
        wt: float,
        material: Dict[str, Any],
        design_factors: Dict[str, Any]
    ) -> Dict[str, float]:
        """
        Calculate burst pressures for specific geometry.

        Args:
            od: Outer diameter
            id_val: Inner diameter
            wt: Wall thickness
            material: Material properties
            design_factors: Design factors

        Returns:
            Dictionary with calculated pressures
        """
        # Apply temperature derating if needed
        yield_strength = material['SMYS']
        ultimate_strength = material['SMUS']

        if 'temperature' in self.config.get('design', {}):
            temp_celsius = (self.config['design']['temperature'] - 32) * 5/9
            derated_props = apply_temperature_derating(yield_strength, ultimate_strength, temp_celsius)
            yield_strength = derated_props['yield_strength']
            ultimate_strength = derated_props['ultimate_strength']

        corrosion_allowance = self.config.get('geometry', {}).get('CorrosionAllowance', 0)

        # Calculate burst pressure without corrosion (for test pressure)
        burst_no_corrosion = self.calculator.calculate_burst_pressure(
            od, id_val, yield_strength, ultimate_strength, 0, include_corrosion=False
        )

        # Calculate burst pressure with corrosion (for design pressures)
        burst_with_corrosion = self.calculator.calculate_burst_pressure(
            od, id_val, yield_strength, ultimate_strength, corrosion_allowance, include_corrosion=True
        )

        # Calculate design pressures
        pressures = {
            'Pb': burst_with_corrosion,
            'Pt': design_factors['internalPressure']['hydroStaticTest'] * burst_no_corrosion,
            'Pd': design_factors['internalPressure']['design'] * burst_with_corrosion,
            'Pa': design_factors['internalPressure']['incidentalPressure'] * burst_with_corrosion
        }

        return pressures

    def run_nominal_wt_method1_analysis(self) -> List[pd.DataFrame]:
        """
        Run nominal wall thickness Method 1 analysis.

        Returns:
            List of DataFrames containing utilization results for each case

        Raises:
            ValueError: If required configuration is missing
        """
        if not self.config:
            raise ValueError("Configuration must be loaded before running analysis")

        logger.info("Starting nominal WT Method 1 analysis")

        method1_config = self.config.get('nominalWTAPISTD2RDMethod1', {})
        data_cases = method1_config.get('data', [])

        if not data_cases:
            raise ValueError("No data cases found for Method 1 analysis")

        utilization_dfs = []

        for case_index, case_data in enumerate(data_cases):
            logger.debug(f"Processing Method 1 case {case_index + 1}")

            # Prepare input data for this case
            input_data = self.config_manager.get_analysis_input_data(case_index)

            # Calculate geometric properties
            geom_props = calculate_geometric_properties(
                input_data['OD'], input_data['ID'], input_data['t']
            )

            # Update input data with geometric properties
            input_data.update({
                'A': geom_props['A'],
                'Ai': geom_props['Ai'],
                'Ao': geom_props['Ao'],
                'I': geom_props['I']
            })

            # Calculate burst pressure
            burst_pressure = self.calculator.calculate_burst_pressure(
                input_data['OD'],
                input_data['ID'],
                input_data['S'],
                input_data['U'],
                input_data['CorrosionAllowance']
            )

            # Calculate collapse pressures
            collapse_data = self.calculator.calculate_collapse_pressure(
                input_data['OD'],
                input_data['ID'],
                input_data['t'],
                input_data['S'],
                input_data['E'],
                input_data['Poissionsratio'],
                input_data.get('alphafab', 1.0)
            )

            # Calculate Method 1 limits and interaction curves
            method1_results = self.calculator.calculate_method1_limits(
                input_data['OD'],
                input_data['ID'],
                input_data['t'],
                input_data['S'],
                burst_pressure,
                input_data['designFactors'],
                input_data['LimitState'],
                input_data['internalPressure'],
                input_data['externalPressure']
            )

            # Generate interaction curves
            interaction_df = self.calculator.generate_interaction_curves(
                method1_results['yield_tension'],
                method1_results['yield_moment'],
                method1_results['plastic_moment'],
                method1_results['pressure_corrected_factor']
            )

            utilization_dfs.append(interaction_df)

            # Store results for this case
            case_results = {
                'input_data': input_data,
                'burst_pressure': burst_pressure,
                'collapse_data': collapse_data,
                'method1_results': method1_results,
                'interaction_curves': interaction_df
            }

            # Store in main results
            if 'method1_cases' not in self.results:
                self.results['method1_cases'] = []
            self.results['method1_cases'].append(case_results)

        logger.info(f"Method 1 analysis completed for {len(data_cases)} cases")
        return utilization_dfs

    def calculate_load_utilization(
        self,
        loading_conditions: List[Dict[str, Any]],
        case_index: int = 0
    ) -> List[Dict[str, Any]]:
        """
        Calculate utilization for applied loading conditions.

        Args:
            loading_conditions: List of loading condition dictionaries
            case_index: Index of the analysis case to use

        Returns:
            List of utilization results

        Raises:
            ValueError: If case results not available
        """
        if 'method1_cases' not in self.results or case_index >= len(self.results['method1_cases']):
            raise ValueError(f"Method 1 results for case {case_index} not available")

        case_results = self.results['method1_cases'][case_index]
        method1_data = case_results['method1_results']

        utilization_results = []

        for i, condition in enumerate(loading_conditions):
            # Determine pressure condition
            internal_pressure = condition.get('InternalPressure', 0)
            external_pressure = condition.get('ExternalPressure', 0)
            load_category = condition.get('LoadCategory', 'design')

            if internal_pressure >= external_pressure:
                pressure_condition = "Internal OverPressure"
                pressure_capacity = case_results['burst_pressure']
            else:
                pressure_condition = "External OverPressure"
                pressure_capacity = case_results['collapse_data']['combined_collapse']

            # Calculate pressure-corrected design factor
            design_factor = case_results['input_data']['designFactors']['internalPressure'][load_category]
            pressure_ratio = (internal_pressure - external_pressure) / pressure_capacity
            fd_pressure_corrected = np.sqrt(design_factor**2 - pressure_ratio**2)

            # Calculate utilization for applied loads
            applied_tension = condition.get('Tension', 0)
            applied_moment = condition.get('BendingMoment', 0)

            utilization_method1 = calculate_utilization(
                applied_tension,
                applied_moment,
                method1_data['yield_tension'],
                method1_data['yield_moment'],
                fd_pressure_corrected,
                method="method1"
            )

            utilization_method2 = calculate_utilization(
                applied_tension,
                applied_moment,
                method1_data['yield_tension'],
                method1_data['yield_moment'],
                fd_pressure_corrected,
                method="method2"
            )

            result = {
                'condition_index': i,
                'pressure_condition': pressure_condition,
                'design_factor': design_factor,
                'pressure_corrected_factor': fd_pressure_corrected,
                'utilization_method1': utilization_method1,
                'utilization_method2': utilization_method2,
                'applied_tension': applied_tension,
                'applied_moment': applied_moment,
                **condition
            }

            utilization_results.append(result)

        return utilization_results

    def run_collapse_analysis(self) -> Dict[str, Any]:
        """
        Run collapse pressure analysis.

        Returns:
            Dictionary containing collapse analysis results

        Raises:
            ValueError: If required configuration is missing
        """
        if not self.config:
            raise ValueError("Configuration must be loaded before running analysis")

        logger.info("Starting collapse pressure analysis")

        # Get input data
        input_data = self.config_manager.get_analysis_input_data()

        # Calculate collapse pressures
        collapse_results = self.calculator.calculate_collapse_pressure(
            input_data['OD'],
            input_data['ID'],
            input_data['t'],
            input_data['S'],
            input_data['E'],
            input_data['Poissionsratio'],
            input_data.get('alphafab', 1.0)
        )

        self.results['collapse_analysis'] = collapse_results
        logger.info("Collapse analysis completed")

        return collapse_results

    def get_analysis_summary(self) -> Dict[str, Any]:
        """
        Get a comprehensive summary of all analysis results.

        Returns:
            Dictionary containing analysis summary
        """
        summary = {
            'configuration': self.config,
            'results_available': list(self.results.keys()),
            'analysis_timestamp': pd.Timestamp.now().isoformat()
        }

        # Add summaries for each analysis type
        if 'variable_wt_burst' in self.results:
            df = self.results['variable_wt_burst']
            summary['variable_wt_burst_summary'] = {
                'num_cases': len(df),
                'wall_thickness_range': [df['t'].min(), df['t'].max()],
                'burst_pressure_range': [df['Pb'].min(), df['Pb'].max()]
            }

        if 'method1_cases' in self.results:
            num_cases = len(self.results['method1_cases'])
            summary['method1_analysis_summary'] = {
                'num_cases': num_cases,
                'cases_analyzed': True
            }

        if 'collapse_analysis' in self.results:
            collapse = self.results['collapse_analysis']
            summary['collapse_analysis_summary'] = {
                'yield_collapse': collapse['yield_collapse'],
                'elastic_collapse': collapse['elastic_collapse'],
                'combined_collapse': collapse['combined_collapse']
            }

        return summary

    def export_results(self, output_dir: Union[str, Path]) -> None:
        """
        Export analysis results to files.

        Args:
            output_dir: Directory where to save results
        """
        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)

        # Export variable WT burst results
        if 'variable_wt_burst' in self.results:
            df = self.results['variable_wt_burst']
            df.to_csv(output_path / 'variable_wt_burst_results.csv', index=False)
            df.to_excel(output_path / 'variable_wt_burst_results.xlsx', index=False)

        # Export Method 1 results
        if 'method1_cases' in self.results:
            for i, case in enumerate(self.results['method1_cases']):
                case_df = case['interaction_curves']
                case_df.to_csv(output_path / f'method1_case_{i+1}_interaction_curves.csv', index=False)

        # Export summary
        summary = self.get_analysis_summary()
        with open(output_path / 'analysis_summary.yaml', 'w') as f:
            import yaml
            yaml.safe_dump(summary, f, default_flow_style=False)

        logger.info(f"Results exported to: {output_path}")


def main():
    """
    Main function for command-line execution.
    """
    # Set up logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )

    # Parse command line arguments
    default_config = Path("config") / "APISTD2RD.yml"
    update_config = None

    if len(sys.argv) > 1:
        update_config = Path("config") / sys.argv[1]
        logger.info(f"Using update configuration: {update_config}")
    else:
        logger.info("No update configuration provided, using defaults")

    try:
        # Initialize analyzer
        analyzer = APISTD2RDAnalyzer()

        # Load configuration
        analyzer.load_configuration(default_config, update_config)

        # Run analyses based on configuration
        config = analyzer.config

        if config.get('default', {}).get('Analysis', {}).get('variableWTBurst', False):
            logger.info("Running variable WT burst analysis")
            results_df, _ = analyzer.run_variable_wt_burst_analysis()
            logger.info(f"Variable WT burst analysis completed with {len(results_df)} cases")

        if config.get('default', {}).get('Analysis', {}).get('nominalWTAPISTD2RDMethod1', False):
            logger.info("Running Method 1 analysis")
            utilization_dfs = analyzer.run_nominal_wt_method1_analysis()
            logger.info(f"Method 1 analysis completed with {len(utilization_dfs)} cases")

        if config.get('default', {}).get('Analysis', {}).get('variableWTCollapse', False):
            logger.info("Running collapse analysis")
            collapse_results = analyzer.run_collapse_analysis()
            logger.info("Collapse analysis completed")

        # Export results
        output_dir = Path("results") / "apistd2rd"
        analyzer.export_results(output_dir)

        logger.info("Analysis completed successfully")

    except Exception as e:
        logger.error(f"Analysis failed: {e}")
        raise


if __name__ == "__main__":
    main()