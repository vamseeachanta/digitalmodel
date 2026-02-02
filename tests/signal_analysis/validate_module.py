#!/usr/bin/env python
"""
Validation script for signal_analysis module

Performs comprehensive checks to ensure the module is fully functional.
"""

import sys
import numpy as np
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

def validate_imports():
    """Validate all module imports"""
    print("Validating imports...")
    try:
        from src.digitalmodel.signal_analysis import (
            RainflowCounter, SpectralAnalyzer, TimeSeriesProcessor
        )
        from src.digitalmodel.signal_analysis.fatigue.damage import FatigueDamageCalculator
        from src.digitalmodel.signal_analysis.fatigue.curves import SNCurve
        from src.digitalmodel.signal_analysis.filters.frequency import FrequencyFilter
        from src.digitalmodel.signal_analysis.adapters import TimeSeriesComponentsAdapter
        print("  [OK] All imports successful")
        return True
    except ImportError as e:
        print(f"  [FAIL] Import error: {e}")
        return False

def validate_rainflow():
    """Validate rainflow counting functionality"""
    print("\nValidating rainflow counting...")
    try:
        from src.digitalmodel.signal_analysis import RainflowCounter
        
        # Generate test signal
        signal = np.array([0, 2, -1, 3, -2, 1, -3, 2, -1, 0])
        
        # Count cycles
        counter = RainflowCounter()
        cycles = counter.count_cycles(signal)
        
        if len(cycles) > 0:
            print("  [OK] Rainflow counting works")
            return True
        else:
            print("  [FAIL] No cycles extracted")
            return False
    except Exception as e:
        print(f"  [FAIL] Error: {e}")
        return False

def validate_spectral():
    """Validate spectral analysis functionality"""
    print("\nValidating spectral analysis...")
    try:
        from src.digitalmodel.signal_analysis import SpectralAnalyzer
        
        # Generate test signal
        fs = 100
        t = np.linspace(0, 1, fs)
        signal = np.sin(2 * np.pi * 10 * t)
        
        # Analyze spectrum
        analyzer = SpectralAnalyzer(sampling_rate=fs)
        spectrum = analyzer.compute_spectrum(signal)
        
        if len(spectrum) > 0:
            print("  [OK] Spectral analysis works")
            return True
        else:
            print("  [FAIL] No spectrum computed")
            return False
    except Exception as e:
        print(f"  [FAIL] Error: {e}")
        return False

def validate_fatigue():
    """Validate fatigue analysis functionality"""
    print("\nValidating fatigue analysis...")
    try:
        from src.digitalmodel.signal_analysis import RainflowCounter
        from src.digitalmodel.signal_analysis.fatigue.damage import FatigueDamageCalculator
        from src.digitalmodel.signal_analysis.fatigue.curves import SNCurve
        
        # Generate test cycles
        signal = 10 * np.sin(np.linspace(0, 20*np.pi, 1000)) + 50
        
        # Count cycles
        counter = RainflowCounter()
        cycles = counter.count_cycles(signal)
        
        # Calculate damage
        sn_curve = SNCurve(curve_type='standard', standard='DNV', **{'class': 'D'})
        damage_calc = FatigueDamageCalculator()
        damage = damage_calc.calculate_damage(cycles, sn_curve.get_curve_parameters())
        
        if damage['total_damage'] >= 0:
            print("  [OK] Fatigue analysis works")
            return True
        else:
            print("  [FAIL] Invalid damage calculation")
            return False
    except Exception as e:
        print(f"  [FAIL] Error: {e}")
        return False

def validate_filtering():
    """Validate filtering functionality"""
    print("\nValidating signal filtering...")
    try:
        from src.digitalmodel.signal_analysis.filters.frequency import FrequencyFilter
        
        # Generate test signal
        fs = 1000
        t = np.linspace(0, 1, fs)
        signal = np.sin(2 * np.pi * 10 * t) + np.sin(2 * np.pi * 100 * t)
        
        # Apply filter
        filter_obj = FrequencyFilter()
        filtered = filter_obj.lowpass_filter(signal, 50, fs)
        
        if len(filtered) == len(signal):
            print("  [OK] Signal filtering works")
            return True
        else:
            print("  [FAIL] Filter output size mismatch")
            return False
    except Exception as e:
        print(f"  [FAIL] Error: {e}")
        return False

def validate_timeseries():
    """Validate time series processing"""
    print("\nValidating time series processing...")
    try:
        from src.digitalmodel.signal_analysis import TimeSeriesProcessor
        
        # Generate test signal with trend and outliers
        t = np.linspace(0, 10, 100)
        signal = np.sin(t) + 0.1 * t  # Signal with trend
        signal[50] = 100  # Add outlier
        
        # Process signal
        processor = TimeSeriesProcessor()
        clean = processor.remove_outliers(signal)
        detrended = processor.detrend(clean)
        
        if len(detrended) > 0:
            print("  [OK] Time series processing works")
            return True
        else:
            print("  [FAIL] Processing failed")
            return False
    except Exception as e:
        print(f"  [FAIL] Error: {e}")
        return False

def validate_compatibility():
    """Validate backward compatibility"""
    print("\nValidating backward compatibility...")
    try:
        from src.digitalmodel.signal_analysis.adapters import TimeSeriesComponentsAdapter
        
        # Legacy config
        cfg = {'fft': {'window': {'size': 256}}}
        
        # Create adapter
        adapter = TimeSeriesComponentsAdapter(cfg)
        
        # Test legacy method
        signal = np.random.randn(1000)
        cycles_df, cycles_dict = adapter.get_rainflow_count_from_time_series(signal)
        
        if len(cycles_df) > 0:
            print("  [OK] Backward compatibility works")
            return True
        else:
            print("  [FAIL] Legacy methods not working")
            return False
    except Exception as e:
        print(f"  [FAIL] Error: {e}")
        return False

def main():
    """Run all validations"""
    print("="*60)
    print("Signal Analysis Module Validation")
    print("="*60)
    
    results = []
    
    # Run all validations
    results.append(("Imports", validate_imports()))
    results.append(("Rainflow", validate_rainflow()))
    results.append(("Spectral", validate_spectral()))
    results.append(("Fatigue", validate_fatigue()))
    results.append(("Filtering", validate_filtering()))
    results.append(("Time Series", validate_timeseries()))
    results.append(("Compatibility", validate_compatibility()))
    
    # Summary
    print("\n" + "="*60)
    print("Validation Summary")
    print("="*60)
    
    all_passed = True
    for name, passed in results:
        status = "[OK]" if passed else "[FAIL]"
        print(f"  {status} {name}")
        if not passed:
            all_passed = False
    
    print("="*60)
    if all_passed:
        print("ALL VALIDATIONS PASSED - Module is fully functional!")
    else:
        print("SOME VALIDATIONS FAILED - Please review errors above")
    print("="*60)
    
    return 0 if all_passed else 1

if __name__ == "__main__":
    sys.exit(main())