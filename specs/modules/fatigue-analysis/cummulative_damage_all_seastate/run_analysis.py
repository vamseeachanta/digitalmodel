#!/usr/bin/env python3
"""
Quick runner script for cumulative damage analysis
"""

import sys
import os
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent))

from cumulative_damage_analysis import CumulativeDamageAnalyzer
import logging

def run_sample_analysis():
    """Run analysis with sample data"""
    
    # Set up logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    
    logger = logging.getLogger(__name__)
    
    # Configuration file
    config_file = 'cumulative_damage_config.yml'
    
    # Check if config exists
    if not Path(config_file).exists():
        logger.error(f"Configuration file {config_file} not found")
        return 1
    
    try:
        # Initialize and run analyzer
        logger.info("="*60)
        logger.info("Starting Cumulative Fatigue Damage Analysis")
        logger.info("="*60)
        
        analyzer = CumulativeDamageAnalyzer(config_file)
        analyzer.run()
        
        logger.info("="*60)
        logger.info("Analysis completed successfully!")
        logger.info("Check the output folder for results:")
        logger.info("  - Individual results: output/individual_results/")
        logger.info("  - Plots: output/plots/")
        logger.info("  - Summary: output/fatigue_life_summary.csv")
        logger.info("  - Report: output/analysis_report.md")
        logger.info("="*60)
        
        return 0
        
    except Exception as e:
        logger.error(f"Analysis failed: {str(e)}")
        import traceback
        traceback.print_exc()
        return 1

if __name__ == "__main__":
    sys.exit(run_sample_analysis())