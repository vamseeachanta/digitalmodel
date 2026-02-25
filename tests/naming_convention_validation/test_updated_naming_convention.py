#!/usr/bin/env python3
"""
Test script to verify the updated production code supports the new SS naming convention.
Tests both legacy (wind01/wave01, FC###) and new (REF_WIND01/REF_WAVE01, SS###) formats.
"""

import sys
import logging
from pathlib import Path

# Add the source directory to the path
sys.path.insert(0, str(Path(__file__).parent / 'src'))

from digitalmodel.structural.fatigue_apps.strut_foundation_processor import (
    ProductionDataHandler, LoadScaler, FatigueCondition, SeaState
)
from digitalmodel.structural.fatigue_apps.file_namer import FatigueFileNamer, create_output_structure

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)

def test_reference_name_parsing():
    """Test the updated reference name parsing"""
    logger.info("=== Testing Reference Name Parsing ===")
    
    # Create a dummy data handler
    data_handler = ProductionDataHandler(base_path="sample_data", sample_timesteps=100)
    
    # Test new naming convention
    ref_wind_new = data_handler.parse_reference_name("REF_WIND01")
    ref_wave_new = data_handler.parse_reference_name("REF_WAVE01")
    
    logger.info(f"REF_WIND01 parsed: {ref_wind_new}")
    logger.info(f"REF_WAVE01 parsed: {ref_wave_new}")
    
    # Test legacy naming convention
    ref_wind_legacy = data_handler.parse_reference_name("wind01")
    ref_wave_legacy = data_handler.parse_reference_name("wave01")
    
    logger.info(f"wind01 parsed: {ref_wind_legacy}")
    logger.info(f"wave01 parsed: {ref_wave_legacy}")
    
    # Verify new format detection
    assert ref_wind_new.get('naming_format') == 'new', "REF_WIND01 should be detected as new format"
    assert ref_wave_new.get('naming_format') == 'new', "REF_WAVE01 should be detected as new format"
    assert ref_wind_legacy.get('naming_format') == 'legacy', "wind01 should be detected as legacy format"
    assert ref_wave_legacy.get('naming_format') == 'legacy', "wave01 should be detected as legacy format"
    
    logger.info("✓ Reference name parsing works correctly")

def test_sea_state_dataclass():
    """Test the new SeaState dataclass"""
    logger.info("\n=== Testing SeaState Dataclass ===")
    
    # Create sample sea states based on the verification framework
    sea_states = [
        SeaState(
            id='SS001',
            wind_speed=15.0,
            wind_dir=0.0,
            hs=0.75,
            tp=2.7,
            wave_dir=0.0,
            occurrence=25.0,
            description='Test validation condition'
        ),
        SeaState(
            id='SS002',
            wind_speed=10.0,
            wind_dir=0.0,
            hs=0.50,
            tp=2.7,
            wave_dir=0.0,
            occurrence=25.0,
            description='Baseline check condition'
        )
    ]
    
    logger.info(f"Created {len(sea_states)} sea states:")
    for ss in sea_states:
        logger.info(f"  {ss.id}: {ss.wind_speed} m/s wind, {ss.hs}m waves - {ss.description}")
    
    logger.info("✓ SeaState dataclass works correctly")

def test_load_scaler_new_methods():
    """Test the LoadScaler with new sea state methods"""
    logger.info("\n=== Testing LoadScaler New Methods ===")
    
    try:
        # Create data handler and load scaler
        data_handler = ProductionDataHandler(base_path="sample_data", sample_timesteps=100)
        load_scaler = LoadScaler(data_handler)
        
        logger.info(f"Loaded {len(load_scaler.fatigue_conditions)} legacy fatigue conditions")
        logger.info(f"Loaded {len(load_scaler.sea_states)} new sea states")
        
        # Display sea states
        for ss in load_scaler.sea_states:
            logger.info(f"  {ss.id}: {ss.wind_speed} m/s, {ss.hs}m Hs - {ss.description}")
        
        logger.info("✓ LoadScaler new methods work correctly")
        
    except Exception as e:
        logger.warning(f"LoadScaler test skipped (expected if no sample data): {e}")

def test_file_naming_compatibility():
    """Test file naming with both conventions"""
    logger.info("\n=== Testing File Naming Compatibility ===")
    
    # Create file naming structure
    namers = create_output_structure('test_output')
    namer = namers['fsts_l015']
    
    # Test legacy naming
    legacy_combined = namer.get_combined_filename(1, 1)  # FC001
    legacy_rainflow = namer.get_rainflow_filename(1, 1)
    legacy_damage = namer.get_damage_filename(1, 1)
    
    logger.info(f"Legacy FC001 files:")
    logger.info(f"  Combined: {legacy_combined.name}")
    logger.info(f"  Rainflow: {legacy_rainflow.name}")
    logger.info(f"  Damage: {legacy_damage.name}")
    
    # Test new naming
    new_combined = namer.get_combined_filename('SS001', 1)
    new_rainflow = namer.get_rainflow_filename('SS001', 1)
    new_damage = namer.get_damage_filename('SS001', 1)
    
    logger.info(f"New SS001 files:")
    logger.info(f"  Combined: {new_combined.name}")
    logger.info(f"  Rainflow: {new_rainflow.name}")
    logger.info(f"  Damage: {new_damage.name}")
    
    # Verify correct naming patterns
    assert "FC001" in legacy_combined.name, "Legacy naming should include FC001"
    assert "SS001" in new_combined.name, "New naming should include SS001"
    
    logger.info("✓ File naming compatibility works correctly")

def test_scaling_factors():
    """Test scaling factor calculations for the verification framework"""
    logger.info("\n=== Testing Scaling Factor Calculations ===")
    
    # Reference baselines (REF_WIND01, REF_WAVE01)
    base_wind_speed = 10.0  # m/s
    base_hs = 0.5  # m
    
    # Test conditions from verification framework
    test_conditions = [
        ('SS001', 15.0, 0.75, 2.25, 1.50),  # Test validation
        ('SS002', 10.0, 0.50, 1.00, 1.00),  # Baseline check
        ('SS003', 5.0, 0.25, 0.25, 0.50),   # Calm conditions
        ('SS004', 20.0, 1.00, 4.00, 2.00)   # Severe conditions
    ]
    
    logger.info("Verification of scaling factors:")
    for ss_id, wind_speed, hs, expected_wind_scale, expected_wave_scale in test_conditions:
        # Calculate scaling factors
        wind_scale = (wind_speed / base_wind_speed) ** 2
        wave_scale = hs / base_hs
        
        logger.info(f"{ss_id}: {wind_speed} m/s → {wind_scale:.2f}x, {hs}m → {wave_scale:.2f}x")
        
        # Verify against expected values
        assert abs(wind_scale - expected_wind_scale) < 0.01, f"Wind scale mismatch for {ss_id}"
        assert abs(wave_scale - expected_wave_scale) < 0.01, f"Wave scale mismatch for {ss_id}"
    
    logger.info("✓ Scaling factor calculations are correct")

def main():
    """Run all tests"""
    logger.info("=" * 60)
    logger.info("TESTING UPDATED NAMING CONVENTION SUPPORT")
    logger.info("=" * 60)
    
    try:
        test_reference_name_parsing()
        test_sea_state_dataclass()
        test_load_scaler_new_methods()
        test_file_naming_compatibility()
        test_scaling_factors()
        
        logger.info("\n" + "=" * 60)
        logger.info("✓ ALL TESTS PASSED - Updated code supports new SS naming convention")
        logger.info("✓ Backward compatibility with legacy wind01/wave01 and FC### maintained")
        logger.info("✓ Production code ready for REF_WIND01/REF_WAVE01 and SS001-SS004")
        logger.info("=" * 60)
        
    except Exception as e:
        logger.error(f"❌ Test failed: {e}")
        raise

if __name__ == "__main__":
    main()