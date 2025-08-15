#!/usr/bin/env python
"""
Test to demonstrate OrcaFlex license detection.
This test distinguishes between environments with and without OrcaFlex licenses.
"""

import sys
from pathlib import Path

def check_orcaflex_environment():
    """
    Comprehensive check of OrcaFlex environment.
    Returns detailed status for test planning.
    """
    status = {
        'module_installed': False,
        'module_version': None,
        'license_valid': False,
        'can_create_model': False,
        'can_run_analysis': False,
        'environment_type': 'NO_INSTALLATION',
        'available_tests': []
    }
    
    # Step 1: Check if OrcFxAPI module is installed
    try:
        import OrcFxAPI
        status['module_installed'] = True
        
        # Try to get version
        try:
            status['module_version'] = OrcFxAPI.Version()
        except:
            status['module_version'] = 'Unknown'
        
        # Step 2: Check if license is valid
        try:
            model = OrcFxAPI.Model()
            status['license_valid'] = True
            status['can_create_model'] = True
            
            # Step 3: Check if we can run analysis
            try:
                # Try a simple operation
                general = model.general
                # Just reading should work
                duration = general.StageDuration[0]
                status['can_run_analysis'] = True
                status['environment_type'] = 'FULL_LICENSE'
                
            except Exception as e:
                status['environment_type'] = 'LIMITED_LICENSE'
                
        except Exception as e:
            # Module exists but no valid license
            status['environment_type'] = 'NO_LICENSE'
            status['license_error'] = str(e)
            
    except ImportError as e:
        # Module not installed
        status['environment_type'] = 'NO_INSTALLATION'
        status['import_error'] = str(e)
    
    # Determine available tests based on environment
    if status['environment_type'] == 'FULL_LICENSE':
        status['available_tests'] = [
            'Unit tests',
            'Integration tests', 
            'DAT to SIM conversion',
            'Static analysis',
            'Dynamic simulation',
            'Post-processing',
            'Results extraction'
        ]
    elif status['environment_type'] == 'NO_LICENSE':
        status['available_tests'] = [
            'Unit tests',
            'Configuration validation',
            'YAML parsing',
            'Mock OrcaFlex tests'
        ]
    else:  # NO_INSTALLATION
        status['available_tests'] = [
            'Configuration tests',
            'Utility function tests',
            'Mock-only tests'
        ]
    
    return status


def print_environment_report(status):
    """Print a formatted environment report."""
    
    print("="*60)
    print("OrcaFlex Environment Detection Report")
    print("="*60)
    
    # Module status
    print("\n[MODULE STATUS]")
    if status['module_installed']:
        print(f"  [OK] OrcFxAPI module installed")
        if status['module_version']:
            print(f"       Version: {status['module_version']}")
    else:
        print(f"  [X] OrcFxAPI module NOT installed")
        if 'import_error' in status:
            print(f"      Error: {status['import_error'][:50]}...")
    
    # License status
    print("\n[LICENSE STATUS]")
    if status['license_valid']:
        print(f"  [OK] Valid OrcaFlex license detected")
    else:
        print(f"  [X] No valid OrcaFlex license")
        if 'license_error' in status:
            print(f"      Error: {status['license_error'][:50]}...")
    
    # Capabilities
    print("\n[CAPABILITIES]")
    capabilities = [
        ('Create models', status['can_create_model']),
        ('Run analysis', status['can_run_analysis'])
    ]
    for capability, available in capabilities:
        symbol = "[OK]" if available else "[X]"
        print(f"  {symbol} {capability}")
    
    # Environment type
    print("\n[ENVIRONMENT TYPE]")
    env_descriptions = {
        'FULL_LICENSE': 'Full OrcaFlex installation with valid license',
        'LIMITED_LICENSE': 'OrcaFlex installed with limited license',
        'NO_LICENSE': 'OrcaFlex installed but no valid license',
        'NO_INSTALLATION': 'OrcaFlex not installed'
    }
    print(f"  {status['environment_type']}")
    print(f"  {env_descriptions.get(status['environment_type'], 'Unknown')}")
    
    # Available tests
    print("\n[AVAILABLE TESTS]")
    if status['available_tests']:
        for test in status['available_tests']:
            print(f"  - {test}")
    else:
        print("  No tests available")
    
    print("\n" + "="*60)


def generate_test_strategy(status):
    """Generate appropriate test strategy based on environment."""
    
    print("\n[RECOMMENDED TEST STRATEGY]")
    print("-"*40)
    
    if status['environment_type'] == 'FULL_LICENSE':
        print("You have full OrcaFlex capabilities!")
        print("\nRun all tests:")
        print("  python -m pytest tests/modules/orcaflex/ -v")
        print("\nDirect OrcaFlex tests:")
        print("  python test_direct_orcaflex.py")
        print("  python test_sim_verification.py")
        
    elif status['environment_type'] in ['NO_LICENSE', 'LIMITED_LICENSE']:
        print("OrcaFlex is installed but license is limited/missing.")
        print("\nRun unit tests only:")
        print("  python -m pytest tests/modules/orcaflex/ -v -m 'not requires_license'")
        print("\nConfiguration tests:")
        print("  python test_config_validation.py")
        
    else:  # NO_INSTALLATION
        print("OrcaFlex is not installed.")
        print("\nRun mock tests only:")
        print("  python -m pytest tests/modules/orcaflex/ -v -m 'mock_only'")
        print("\nConsider:")
        print("  - Installing OrcFxAPI for development")
        print("  - Using Docker with OrcaFlex for CI/CD")
    
    print("\n" + "-"*40)


if __name__ == "__main__":
    print("OrcaFlex License Detection Test")
    print("="*60)
    
    # Check environment
    status = check_orcaflex_environment()
    
    # Print report
    print_environment_report(status)
    
    # Generate test strategy
    generate_test_strategy(status)
    
    # Summary
    print("\n[SUMMARY]")
    if status['environment_type'] == 'FULL_LICENSE':
        print("[SUCCESS] Full testing capabilities available!")
        exit_code = 0
    elif status['environment_type'] in ['NO_LICENSE', 'LIMITED_LICENSE']:
        print("[WARNING] Limited testing - no valid license")
        exit_code = 1
    else:
        print("[INFO] Mock testing only - OrcaFlex not installed")
        exit_code = 2
    
    print("="*60)
    sys.exit(exit_code)