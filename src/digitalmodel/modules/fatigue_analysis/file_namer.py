#!/usr/bin/env python3
"""
File Naming Convention Handler for Fatigue Analysis

Implements the agreed naming convention:
{config}_FC{###}_Strut{#}_{type}.csv
"""

from pathlib import Path
from typing import Optional, Union


class FatigueFileNamer:
    """Handles all fatigue analysis file naming according to agreed convention"""
    
    def __init__(self, config: str, output_base_dir: Union[str, Path]):
        """
        Initialize file namer with configuration
        
        Args:
            config: Configuration name (e.g., 'fsts_l015')
            output_base_dir: Base output directory
        """
        self.config = config
        self.base_dir = Path(output_base_dir) / config
        
        # Create directory structure
        self.dirs = {
            'scaled_references': self.base_dir / 'scaled_references',
            'combined_tensions': self.base_dir / 'combined_tensions',
            'rainflow_results': self.base_dir / 'rainflow_results',
            'damage_results': self.base_dir / 'damage_results',
            'summaries': self.base_dir / 'summaries'
        }
        
        # Create all directories
        for directory in self.dirs.values():
            directory.mkdir(parents=True, exist_ok=True)
    
    def get_scaled_ref_filename(self, condition_id: str, ref_case: str, 
                               scale_factor: float, strut_num: int, 
                               ref_type: str = 'wind') -> Path:
        """
        Get filename for scaled reference file (intermediate)
        
        Args:
            condition_id: Condition ID (legacy: FC001, new: SS001)
            ref_case: Reference case (e.g., 'wind09', 'REF_WIND01')
            scale_factor: Scaling factor applied
            strut_num: Strut number
            ref_type: 'wind' or 'wave'
            
        Returns:
            Path to scaled reference file
        """
        scale_str = f"s{int(scale_factor*100):03d}"  # e.g., 0.25 -> s025
        
        # Support both legacy FC### and new SS### formats
        if isinstance(condition_id, int):
            condition_str = f"FC{condition_id:03d}"  # Legacy format
        else:
            condition_str = condition_id  # New format (SS001, etc.)
            
        filename = f"{self.config}_{condition_str}_{ref_case}_{scale_str}_Strut{strut_num}_scaled.csv"
        return self.dirs['scaled_references'] / filename
    
    def get_combined_filename(self, condition_id, strut_num: int) -> Path:
        """
        Get filename for combined effective tension file
        
        Args:
            condition_id: Condition ID (legacy: int for FC###, new: str for SS###)
            strut_num: Strut number
            
        Returns:
            Path to combined tension file
        """
        # Support both legacy FC### and new SS### formats
        if isinstance(condition_id, int):
            condition_str = f"FC{condition_id:03d}"  # Legacy format
        else:
            condition_str = condition_id  # New format (SS001, etc.)
            
        filename = f"{self.config}_{condition_str}_Strut{strut_num}_combined.csv"
        return self.dirs['combined_tensions'] / filename
    
    def get_rainflow_filename(self, condition_id, strut_num: int) -> Path:
        """
        Get filename for rainflow results file
        
        Args:
            condition_id: Condition ID (legacy: int for FC###, new: str for SS###)
            strut_num: Strut number
            
        Returns:
            Path to rainflow results file
        """
        # Support both legacy FC### and new SS### formats
        if isinstance(condition_id, int):
            condition_str = f"FC{condition_id:03d}"  # Legacy format
        else:
            condition_str = condition_id  # New format (SS001, etc.)
            
        filename = f"{self.config}_{condition_str}_Strut{strut_num}_rainflow.csv"
        return self.dirs['rainflow_results'] / filename
    
    def get_damage_filename(self, condition_id, strut_num: int) -> Path:
        """
        Get filename for damage results file
        
        Args:
            condition_id: Condition ID (legacy: int for FC###, new: str for SS###)
            strut_num: Strut number
            
        Returns:
            Path to damage results file
        """
        # Support both legacy FC### and new SS### formats
        if isinstance(condition_id, int):
            condition_str = f"FC{condition_id:03d}"  # Legacy format
        else:
            condition_str = condition_id  # New format (SS001, etc.)
            
        filename = f"{self.config}_{condition_str}_Strut{strut_num}_damage.csv"
        return self.dirs['damage_results'] / filename
    
    def get_summary_filename(self) -> Path:
        """
        Get filename for configuration summary file
        
        Returns:
            Path to summary file
        """
        filename = f"{self.config}_fatigue_summary.csv"
        return self.dirs['summaries'] / filename
    
    def get_processing_map_filename(self) -> Path:
        """
        Get filename for processing map file
        
        Returns:
            Path to processing map file
        """
        filename = f"{self.config}_processing_map.csv"
        return self.dirs['summaries'] / filename


class OverallFileNamer:
    """Handles overall summary file naming"""
    
    def __init__(self, output_base_dir: Union[str, Path]):
        """
        Initialize overall file namer
        
        Args:
            output_base_dir: Base output directory
        """
        self.base_dir = Path(output_base_dir) / 'overall'
        self.base_dir.mkdir(parents=True, exist_ok=True)
    
    def get_overall_summary_filename(self) -> Path:
        """Get overall fatigue summary filename"""
        return self.base_dir / "overall_fatigue_summary.csv"
    
    def get_configuration_comparison_filename(self) -> Path:
        """Get configuration comparison filename"""
        return self.base_dir / "configuration_comparison.csv"
    
    def get_weighted_life_filename(self) -> Path:
        """Get weighted fatigue life filename"""
        return self.base_dir / "weighted_fatigue_life.json"


def create_output_structure(output_base_dir: Union[str, Path], 
                          configs: list = None) -> dict:
    """
    Create complete output directory structure
    
    Args:
        output_base_dir: Base output directory
        configs: List of configuration names
        
    Returns:
        Dictionary of file namers by configuration
    """
    if configs is None:
        configs = ['fsts_l015', 'fsts_l095', 
                  'fsts_l015_125km3_l100_pb', 'fsts_l095_125km3_l000_pb']
    
    namers = {}
    for config in configs:
        namers[config] = FatigueFileNamer(config, output_base_dir)
    
    # Create overall namer
    namers['overall'] = OverallFileNamer(output_base_dir)
    
    return namers


# Example usage
if __name__ == "__main__":
    # Create file naming structure
    namers = create_output_structure('output')
    
    # Example for fsts_l015
    namer = namers['fsts_l015']
    
    print("=== Legacy FC### Format ===")
    # Get various filenames (legacy format)
    print("Scaled wind:", namer.get_scaled_ref_filename(23, 'wind09', 3.24, 1, 'wind'))
    print("Scaled wave:", namer.get_scaled_ref_filename(23, 'wave15', 1.60, 1, 'wave'))
    print("Combined:", namer.get_combined_filename(23, 1))
    print("Rainflow:", namer.get_rainflow_filename(23, 1))
    print("Damage:", namer.get_damage_filename(23, 1))
    
    print("\n=== New SS### Format ===")
    # Get various filenames (new format)
    print("Scaled wind:", namer.get_scaled_ref_filename('SS001', 'REF_WIND01', 2.25, 1, 'wind'))
    print("Scaled wave:", namer.get_scaled_ref_filename('SS001', 'REF_WAVE01', 1.50, 1, 'wave'))
    print("Combined:", namer.get_combined_filename('SS001', 1))
    print("Rainflow:", namer.get_rainflow_filename('SS001', 1))
    print("Damage:", namer.get_damage_filename('SS001', 1))
    
    print("\n=== Common Files ===")
    print("Summary:", namer.get_summary_filename())
    
    # Overall files
    overall = namers['overall']
    print("Overall summary:", overall.get_overall_summary_filename())
    print("Configuration comparison:", overall.get_configuration_comparison_filename())