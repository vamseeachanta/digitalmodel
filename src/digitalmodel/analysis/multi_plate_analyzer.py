"""
Multi-Plate Buckling Analyzer

This module consolidates the functionality from the original plateBucklingCal_*.py files
(G, H, I, J, K) into a unified multi-plate analysis system. It provides batch processing
capabilities for analyzing multiple plates with different configurations.

Author: Migrated and modernized from original work by Sai Venkatesh
Date: 2025-01-15
Standards: DNV-RP-C201
"""

from typing import Dict, List, Optional, Tuple, Any, Union
from dataclasses import dataclass, field
import logging
from pathlib import Path
import json
import csv

from .plate_capacity import (
    PlateProperties, AppliedLoads, BucklingConstants, BucklingResults,
    PlateBucklingAnalyzer, BoundaryCondition, create_plate_from_legacy_data
)

logger = logging.getLogger(__name__)


@dataclass
class PlateConfiguration:
    """Configuration for a single plate in multi-plate analysis."""

    plate_id: str
    plate_properties: PlateProperties
    applied_loads: AppliedLoads
    buckling_constants: BucklingConstants
    boundary_condition: BoundaryCondition = BoundaryCondition.SIMPLY_SUPPORTED
    description: str = ""


@dataclass
class MultiPlateResults:
    """Results from multi-plate analysis."""

    plate_results: Dict[str, BucklingResults] = field(default_factory=dict)
    summary_statistics: Dict[str, float] = field(default_factory=dict)
    critical_plates: List[str] = field(default_factory=list)
    overall_status: str = "SAFE"


class MultiPlateAnalyzer:
    """
    Analyzer for multiple plates with different configurations.

    This class provides batch processing capabilities for analyzing multiple plates
    and generates comprehensive reports comparing results across all plates.
    """

    def __init__(self):
        """Initialize the multi-plate analyzer."""
        self.plates: Dict[str, PlateConfiguration] = {}
        self.results: Optional[MultiPlateResults] = None

        logger.info("Initialized multi-plate analyzer")

    def add_plate(self, config: PlateConfiguration) -> None:
        """
        Add a plate configuration to the analysis.

        Args:
            config: Plate configuration to add
        """
        if config.plate_id in self.plates:
            logger.warning(f"Plate {config.plate_id} already exists, replacing")

        self.plates[config.plate_id] = config
        logger.debug(f"Added plate {config.plate_id}")

    def add_plate_from_legacy_data(self, plate_id: str, legacy_data: Dict[str, Any],
                                 description: str = "") -> None:
        """
        Add a plate from legacy parameter format.

        Args:
            plate_id: Unique identifier for the plate
            legacy_data: Legacy parameter dictionary
            description: Optional description
        """
        try:
            plate_props, applied_loads, buckling_constants = create_plate_from_legacy_data(legacy_data)

            config = PlateConfiguration(
                plate_id=plate_id,
                plate_properties=plate_props,
                applied_loads=applied_loads,
                buckling_constants=buckling_constants,
                description=description
            )

            self.add_plate(config)
            logger.info(f"Added plate {plate_id} from legacy data")

        except Exception as e:
            logger.error(f"Failed to add plate {plate_id} from legacy data: {e}")
            raise

    def remove_plate(self, plate_id: str) -> bool:
        """
        Remove a plate from the analysis.

        Args:
            plate_id: ID of plate to remove

        Returns:
            True if plate was removed, False if not found
        """
        if plate_id in self.plates:
            del self.plates[plate_id]
            logger.debug(f"Removed plate {plate_id}")
            return True
        else:
            logger.warning(f"Plate {plate_id} not found for removal")
            return False

    def get_plate_count(self) -> int:
        """Get the number of plates in the analysis."""
        return len(self.plates)

    def analyze_all_plates(self) -> MultiPlateResults:
        """
        Analyze all plates and generate comprehensive results.

        Returns:
            MultiPlateResults with analysis for all plates
        """
        if not self.plates:
            raise ValueError("No plates defined for analysis")

        logger.info(f"Starting analysis of {len(self.plates)} plates")

        results = MultiPlateResults()
        usage_factors = []
        von_mises_stresses = []

        # Analyze each plate
        for plate_id, config in self.plates.items():
            try:
                analyzer = PlateBucklingAnalyzer(
                    plate_props=config.plate_properties,
                    applied_loads=config.applied_loads,
                    buckling_constants=config.buckling_constants,
                    boundary_condition=config.boundary_condition
                )

                plate_result = analyzer.perform_analysis()
                results.plate_results[plate_id] = plate_result

                # Collect statistics
                max_usage = max([
                    plate_result.usage_factor_longitudinal,
                    plate_result.usage_factor_transverse,
                    plate_result.usage_factor_shear,
                    plate_result.usage_factor_biaxial
                ])
                usage_factors.append(max_usage)
                von_mises_stresses.append(plate_result.von_mises_stress)

                # Track critical plates
                if not plate_result.is_safe:
                    results.critical_plates.append(plate_id)

                logger.debug(f"Analyzed plate {plate_id}, max usage factor: {max_usage:.3f}")

            except Exception as e:
                logger.error(f"Failed to analyze plate {plate_id}: {e}")
                # Continue with other plates
                continue

        # Calculate summary statistics
        if usage_factors:
            results.summary_statistics = {
                'max_usage_factor': max(usage_factors),
                'min_usage_factor': min(usage_factors),
                'avg_usage_factor': sum(usage_factors) / len(usage_factors),
                'max_von_mises': max(von_mises_stresses),
                'min_von_mises': min(von_mises_stresses),
                'avg_von_mises': sum(von_mises_stresses) / len(von_mises_stresses),
                'critical_plate_count': len(results.critical_plates),
                'total_plate_count': len(self.plates)
            }

            # Determine overall status
            results.overall_status = "UNSAFE" if results.critical_plates else "SAFE"

        self.results = results
        logger.info(f"Multi-plate analysis complete. Status: {results.overall_status}")

        return results

    def get_critical_plates(self) -> List[str]:
        """Get list of plates that failed the safety check."""
        if self.results is None:
            raise ValueError("No analysis results available. Run analyze_all_plates() first.")

        return self.results.critical_plates

    def get_plate_ranking(self, sort_by: str = "usage_factor") -> List[Tuple[str, float]]:
        """
        Get plates ranked by specified criteria.

        Args:
            sort_by: Criteria to sort by ('usage_factor', 'von_mises', 'safety_margin')

        Returns:
            List of (plate_id, value) tuples sorted by criteria
        """
        if self.results is None:
            raise ValueError("No analysis results available. Run analyze_all_plates() first.")

        ranking = []

        for plate_id, result in self.results.plate_results.items():
            if sort_by == "usage_factor":
                value = max([
                    result.usage_factor_longitudinal,
                    result.usage_factor_transverse,
                    result.usage_factor_shear,
                    result.usage_factor_biaxial
                ])
            elif sort_by == "von_mises":
                value = result.von_mises_stress
            elif sort_by == "safety_margin":
                max_usage = max([
                    result.usage_factor_longitudinal,
                    result.usage_factor_transverse,
                    result.usage_factor_shear,
                    result.usage_factor_biaxial
                ])
                value = 1.0 - max_usage  # Safety margin
            else:
                raise ValueError(f"Unknown sort criteria: {sort_by}")

            ranking.append((plate_id, value))

        # Sort in descending order for usage_factor and von_mises, ascending for safety_margin
        reverse = sort_by != "safety_margin"
        ranking.sort(key=lambda x: x[1], reverse=reverse)

        return ranking

    def export_results_to_csv(self, filepath: Union[str, Path]) -> None:
        """
        Export analysis results to CSV file.

        Args:
            filepath: Path to output CSV file
        """
        if self.results is None:
            raise ValueError("No analysis results available. Run analyze_all_plates() first.")

        filepath = Path(filepath)

        with open(filepath, 'w', newline='', encoding='utf-8') as csvfile:
            writer = csv.writer(csvfile)

            # Write header
            header = [
                'Plate_ID', 'Description', 'Length', 'Breadth', 'Thickness',
                'Young_Modulus', 'Yield_Strength', 'Longitudinal_Stress',
                'Transverse_Stress', 'Shear_Stress', 'Von_Mises_Stress',
                'Usage_Factor_Longitudinal', 'Usage_Factor_Transverse',
                'Usage_Factor_Shear', 'Usage_Factor_Biaxial', 'Max_Usage_Factor',
                'Safety_Status', 'Failure_Mode'
            ]
            writer.writerow(header)

            # Write data rows
            for plate_id, result in self.results.plate_results.items():
                config = self.plates[plate_id]
                props = config.plate_properties
                loads = config.applied_loads

                max_usage = max([
                    result.usage_factor_longitudinal,
                    result.usage_factor_transverse,
                    result.usage_factor_shear,
                    result.usage_factor_biaxial
                ])

                row = [
                    plate_id,
                    config.description,
                    props.length,
                    props.breadth,
                    props.thickness,
                    props.youngs_modulus,
                    props.yield_strength,
                    loads.longitudinal_stress,
                    loads.transverse_stress,
                    loads.shear_stress,
                    result.von_mises_stress,
                    result.usage_factor_longitudinal,
                    result.usage_factor_transverse,
                    result.usage_factor_shear,
                    result.usage_factor_biaxial,
                    max_usage,
                    'SAFE' if result.is_safe else 'UNSAFE',
                    result.failure_mode or 'None'
                ]
                writer.writerow(row)

        logger.info(f"Results exported to {filepath}")

    def export_summary_report(self, filepath: Union[str, Path]) -> None:
        """
        Export summary report to text file.

        Args:
            filepath: Path to output text file
        """
        if self.results is None:
            raise ValueError("No analysis results available. Run analyze_all_plates() first.")

        filepath = Path(filepath)

        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("MULTI-PLATE BUCKLING ANALYSIS SUMMARY REPORT\n")
            f.write("=" * 50 + "\n\n")

            # Overall status
            f.write(f"Overall Status: {self.results.overall_status}\n")
            f.write(f"Total Plates Analyzed: {self.results.summary_statistics.get('total_plate_count', 0)}\n")
            f.write(f"Critical Plates: {self.results.summary_statistics.get('critical_plate_count', 0)}\n\n")

            # Summary statistics
            f.write("SUMMARY STATISTICS\n")
            f.write("-" * 20 + "\n")
            stats = self.results.summary_statistics
            f.write(f"Maximum Usage Factor: {stats.get('max_usage_factor', 0):.3f}\n")
            f.write(f"Average Usage Factor: {stats.get('avg_usage_factor', 0):.3f}\n")
            f.write(f"Minimum Usage Factor: {stats.get('min_usage_factor', 0):.3f}\n")
            f.write(f"Maximum Von Mises Stress: {stats.get('max_von_mises', 0):.2e}\n")
            f.write(f"Average Von Mises Stress: {stats.get('avg_von_mises', 0):.2e}\n\n")

            # Critical plates
            if self.results.critical_plates:
                f.write("CRITICAL PLATES\n")
                f.write("-" * 15 + "\n")
                for plate_id in self.results.critical_plates:
                    result = self.results.plate_results[plate_id]
                    max_usage = max([
                        result.usage_factor_longitudinal,
                        result.usage_factor_transverse,
                        result.usage_factor_shear,
                        result.usage_factor_biaxial
                    ])
                    f.write(f"{plate_id}: Usage Factor = {max_usage:.3f}, "
                           f"Failure Mode = {result.failure_mode or 'Multiple'}\n")
                f.write("\n")

            # Plate ranking
            f.write("PLATE RANKING (by Usage Factor)\n")
            f.write("-" * 30 + "\n")
            ranking = self.get_plate_ranking("usage_factor")
            for i, (plate_id, usage_factor) in enumerate(ranking, 1):
                status = "UNSAFE" if plate_id in self.results.critical_plates else "SAFE"
                f.write(f"{i:2d}. {plate_id:10s} - Usage Factor: {usage_factor:.3f} ({status})\n")

        logger.info(f"Summary report exported to {filepath}")

    def load_plates_from_json(self, filepath: Union[str, Path]) -> None:
        """
        Load plate configurations from JSON file.

        Args:
            filepath: Path to JSON configuration file
        """
        filepath = Path(filepath)

        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                data = json.load(f)

            plates_data = data.get('plates', [])

            for plate_data in plates_data:
                plate_id = plate_data.get('plate_id')
                if not plate_id:
                    logger.warning("Skipping plate without ID")
                    continue

                # Create plate properties
                props_data = plate_data.get('properties', {})
                plate_props = PlateProperties(
                    length=props_data.get('length', 0),
                    breadth=props_data.get('breadth', 0),
                    thickness=props_data.get('thickness', 0),
                    youngs_modulus=props_data.get('youngs_modulus', 0),
                    poisson_ratio=props_data.get('poisson_ratio', 0.3),
                    yield_strength=props_data.get('yield_strength', 0),
                    water_depth=props_data.get('water_depth', 0),
                    length_unit=props_data.get('length_unit', 'm'),
                    stress_unit=props_data.get('stress_unit', 'Pa')
                )

                # Create applied loads
                loads_data = plate_data.get('loads', {})
                applied_loads = AppliedLoads(
                    longitudinal_stress=loads_data.get('longitudinal_stress', 0),
                    transverse_stress=loads_data.get('transverse_stress', 0),
                    shear_stress=loads_data.get('shear_stress', 0)
                )

                # Create buckling constants
                constants_data = plate_data.get('constants', {})
                buckling_constants = BucklingConstants(
                    material_factor=constants_data.get('material_factor', 1.15)
                )

                # Create configuration
                config = PlateConfiguration(
                    plate_id=plate_id,
                    plate_properties=plate_props,
                    applied_loads=applied_loads,
                    buckling_constants=buckling_constants,
                    description=plate_data.get('description', '')
                )

                self.add_plate(config)

            logger.info(f"Loaded {len(plates_data)} plates from {filepath}")

        except Exception as e:
            logger.error(f"Failed to load plates from {filepath}: {e}")
            raise


def create_legacy_multi_plate_analyzer() -> MultiPlateAnalyzer:
    """
    Create a multi-plate analyzer with the original legacy plate data (G, H, I, J, K).

    Returns:
        MultiPlateAnalyzer with legacy plate configurations
    """
    analyzer = MultiPlateAnalyzer()

    # Legacy plate data from original parameters_Col_All.py

    # Plate G data
    plate_g_data = {
        'PlateLength': 2.69, 'PlateLength_unit': 'm',
        'PlateBreadth': 0.70, 'PlateBreadth_unit': 'm',
        'PlateThickness': 0.014, 'PlateThickness_unit': 'm',
        'AverageWaterDepth': 40, 'AverageWaterDepth_unit': 'm',
        'YieldStrength': 34, 'YieldStrength_unit': 'ksi',
        'PoissionsRatio': 0.30,
        'YoungsModulus': 30450, 'YoungsModulus_unit': 'ksi',
        'LongtudinalStress': 0.5, 'LongtudinalStress_unit': 'ksi',
        'TransverseStress': 0.5, 'TransverseStress_unit': 'ksi',
        'ShearStress': 0.7, 'ShearStress_unit': 'ksi',
        'Resulting material factor': 1.15
    }

    # Plate H data
    plate_h_data = {
        'PlateLength': 2.64, 'PlateLength_unit': 'm',
        'PlateBreadth': 0.70, 'PlateBreadth_unit': 'm',
        'PlateThickness': 0.010, 'PlateThickness_unit': 'm',
        'AverageWaterDepth': 40, 'AverageWaterDepth_unit': 'm',
        'YieldStrength': 34, 'YieldStrength_unit': 'ksi',
        'PoissionsRatio': 0.30,
        'YoungsModulus': 30450, 'YoungsModulus_unit': 'ksi',
        'LongtudinalStress': 0.3, 'LongtudinalStress_unit': 'ksi',
        'TransverseStress': 1.9, 'TransverseStress_unit': 'ksi',
        'ShearStress': 1.0, 'ShearStress_unit': 'ksi',
        'Resulting material factor': 1.15
    }

    # Plate I data
    plate_i_data = {
        'PlateLength': 2.74, 'PlateLength_unit': 'm',
        'PlateBreadth': 0.70, 'PlateBreadth_unit': 'm',
        'PlateThickness': 0.010, 'PlateThickness_unit': 'm',
        'AverageWaterDepth': 40, 'AverageWaterDepth_unit': 'm',
        'YieldStrength': 34, 'YieldStrength_unit': 'ksi',
        'PoissionsRatio': 0.30,
        'YoungsModulus': 30450, 'YoungsModulus_unit': 'ksi',
        'LongtudinalStress': 0.6, 'LongtudinalStress_unit': 'ksi',
        'TransverseStress': 3.0, 'TransverseStress_unit': 'ksi',
        'ShearStress': 2.2, 'ShearStress_unit': 'ksi',
        'Resulting material factor': 1.15
    }

    # Plate J data
    plate_j_data = {
        'PlateLength': 1.65, 'PlateLength_unit': 'm',
        'PlateBreadth': 0.70, 'PlateBreadth_unit': 'm',
        'PlateThickness': 0.013, 'PlateThickness_unit': 'm',
        'AverageWaterDepth': 40, 'AverageWaterDepth_unit': 'm',
        'YieldStrength': 34, 'YieldStrength_unit': 'ksi',
        'PoissionsRatio': 0.30,
        'YoungsModulus': 30450, 'YoungsModulus_unit': 'ksi',
        'LongtudinalStress': 0.8, 'LongtudinalStress_unit': 'ksi',
        'TransverseStress': 4.5, 'TransverseStress_unit': 'ksi',
        'ShearStress': 0.7, 'ShearStress_unit': 'ksi',
        'Resulting material factor': 1.15
    }

    # Plate K data
    plate_k_data = {
        'PlateLength': 1.84, 'PlateLength_unit': 'm',
        'PlateBreadth': 0.70, 'PlateBreadth_unit': 'm',
        'PlateThickness': 0.012, 'PlateThickness_unit': 'm',
        'AverageWaterDepth': 40, 'AverageWaterDepth_unit': 'm',
        'YieldStrength': 34, 'YieldStrength_unit': 'ksi',
        'PoissionsRatio': 0.30,
        'YoungsModulus': 30450, 'YoungsModulus_unit': 'ksi',
        'LongtudinalStress': 0.0, 'LongtudinalStress_unit': 'ksi',
        'TransverseStress': 5.8, 'TransverseStress_unit': 'ksi',
        'ShearStress': 0.8, 'ShearStress_unit': 'ksi',
        'Resulting material factor': 1.15
    }

    # Add all plates to analyzer
    analyzer.add_plate_from_legacy_data('Plate_G', plate_g_data, 'Legacy Plate G')
    analyzer.add_plate_from_legacy_data('Plate_H', plate_h_data, 'Legacy Plate H')
    analyzer.add_plate_from_legacy_data('Plate_I', plate_i_data, 'Legacy Plate I')
    analyzer.add_plate_from_legacy_data('Plate_J', plate_j_data, 'Legacy Plate J')
    analyzer.add_plate_from_legacy_data('Plate_K', plate_k_data, 'Legacy Plate K')

    return analyzer


# Example usage
if __name__ == "__main__":
    # Set up logging
    logging.basicConfig(level=logging.INFO)

    # Create legacy multi-plate analyzer
    analyzer = create_legacy_multi_plate_analyzer()

    # Run analysis
    results = analyzer.analyze_all_plates()

    # Print summary
    print(f"Multi-Plate Analysis Results:")
    print(f"Overall Status: {results.overall_status}")
    print(f"Total Plates: {analyzer.get_plate_count()}")
    print(f"Critical Plates: {len(results.critical_plates)}")

    if results.critical_plates:
        print(f"Critical Plate IDs: {', '.join(results.critical_plates)}")

    # Show ranking
    print(f"\nPlate Ranking (by Usage Factor):")
    ranking = analyzer.get_plate_ranking("usage_factor")
    for i, (plate_id, usage_factor) in enumerate(ranking, 1):
        status = "UNSAFE" if plate_id in results.critical_plates else "SAFE"
        print(f"{i}. {plate_id}: {usage_factor:.3f} ({status})")