"""
OrcaFlex Time Trace Signal Processing Example

Demonstrates best practices for processing OrcaFlex time traces
using the integrated signal analysis module.
"""

from pathlib import Path
import sys

# Add project to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.digitalmodel.orcaflex.time_trace_processor import (
    OrcaFlexTimeTraceProcessor,
    TimeTraceConfig,
    create_default_config
)


def example_1_simple_analysis():
    """
    Example 1: Simple fatigue analysis of mooring lines
    """
    print("\n" + "="*60)
    print("Example 1: Simple Mooring Line Fatigue Analysis")
    print("="*60)
    
    # Create configuration
    config = create_default_config(
        file_path=Path("simulations/mooring_system.sim"),
        objects=["Line1", "Line2", "Line3", "Line4"],
        variables=["Tension"],
        output_dir=Path("./results/mooring_fatigue")
    )
    
    # Customize fatigue parameters for mooring lines
    config.fatigue_analysis.update({
        'sn_standard': 'DNV',
        'sn_class': 'F',  # Typical for chain
        'scf': 1.15,  # Stress concentration factor
        'design_life_years': 25
    })
    
    # Process
    processor = OrcaFlexTimeTraceProcessor(config)
    results = processor.process()
    
    # Display summary
    print("\nFatigue Analysis Results:")
    for line_name, line_results in results['fatigue'].items():
        print(f"\n{line_name}:")
        print(f"  Total cycles: {line_results['statistics']['total_cycles']:.0f}")
        print(f"  Max tension range: {line_results['statistics']['max_range']:.1f} kN")
        print(f"  Design life damage: {line_results['damage_design_life']:.3f}")
        print(f"  Estimated life: {line_results['life_years']:.1f} years")
        
        if line_results['damage_design_life'] > 1.0:
            print(f"  WARNING: Fails design life requirement!")
    
    return results


def example_2_spectral_analysis():
    """
    Example 2: Spectral analysis for VIV assessment
    """
    print("\n" + "="*60)
    print("Example 2: Riser VIV Spectral Analysis")
    print("="*60)
    
    # Configuration for riser analysis
    config = TimeTraceConfig(
        source_type='orcaflex',
        file_path=Path("simulations/riser_system.sim"),
        objects=["Riser"],
        variables=["X", "Y", "Vx", "Vy"],  # Position and velocity
        spectral_analysis={
            'method': 'welch',
            'window': 'hann',
            'nperseg': 1024,
            'n_peaks': 10  # More peaks for VIV
        },
        fatigue_analysis={},  # Skip fatigue for this example
        output_dir=Path("./results/riser_viv")
    )
    
    # Process
    processor = OrcaFlexTimeTraceProcessor(config)
    results = processor.process()
    
    # Display VIV frequencies
    print("\nDominant Frequencies (potential VIV):")
    for var_name, var_results in results['spectral'].items():
        print(f"\n{var_name}:")
        peaks = var_results['peaks']
        for idx, peak in peaks.iterrows():
            if idx < 3:  # Show top 3 peaks
                print(f"  {peak['frequency']:.3f} Hz (T = {1/peak['frequency']:.2f} s)")
    
    return results


def example_3_batch_processing():
    """
    Example 3: Batch processing multiple load cases
    """
    print("\n" + "="*60)
    print("Example 3: Batch Processing Multiple Load Cases")
    print("="*60)
    
    # Define load cases
    load_cases = [
        "LC01_operational.sim",
        "LC02_storm.sim",
        "LC03_survival.sim",
        "LC04_installation.sim"
    ]
    
    all_results = {}
    
    for load_case in load_cases:
        print(f"\nProcessing {load_case}...")
        
        config = create_default_config(
            file_path=Path(f"simulations/{load_case}"),
            objects=["Line1", "Line2", "Line3", "Line4"],
            variables=["Tension"],
            output_dir=Path(f"./results/{load_case.split('.')[0]}")
        )
        
        # Adjust for different load case durations
        if "storm" in load_case or "survival" in load_case:
            config.fatigue_analysis['design_life_years'] = 1  # Storm events
        
        processor = OrcaFlexTimeTraceProcessor(config)
        results = processor.process()
        all_results[load_case] = results
    
    # Combine results
    print("\n" + "-"*40)
    print("Combined Fatigue Damage Summary:")
    print("-"*40)
    
    total_damage = {}
    for load_case, results in all_results.items():
        print(f"\n{load_case}:")
        for line_name, line_results in results['fatigue'].items():
            damage = line_results['damage_design_life']
            print(f"  {line_name}: {damage:.4f}")
            
            # Accumulate damage
            if line_name not in total_damage:
                total_damage[line_name] = 0
            
            # Weight by occurrence probability (simplified)
            if "operational" in load_case:
                weight = 0.95  # 95% of time
            elif "storm" in load_case:
                weight = 0.04  # 4% of time
            elif "survival" in load_case:
                weight = 0.01  # 1% of time
            else:
                weight = 0.0
            
            total_damage[line_name] += damage * weight
    
    print("\n" + "-"*40)
    print("Total Weighted Damage:")
    for line_name, damage in total_damage.items():
        print(f"  {line_name}: {damage:.4f}")
        if damage > 1.0:
            print(f"    WARNING: Exceeds allowable damage!")
    
    return all_results


def example_4_advanced_pipeline():
    """
    Example 4: Advanced processing pipeline with custom preprocessing
    """
    print("\n" + "="*60)
    print("Example 4: Advanced Processing Pipeline")
    print("="*60)
    
    # Advanced configuration
    config = TimeTraceConfig(
        source_type='orcaflex',
        file_path=Path("simulations/complex_system.sim"),
        objects=["FPSO", "Riser1", "Riser2", "Line1", "Line2"],
        variables=["Tension", "BendMoment", "X", "Y", "Z"],
        time_range=(100, 10900),  # Skip initial transients
        preprocessing={
            'remove_outliers': True,
            'outlier_method': 'isolation_forest',
            'detrend': True,
            'detrend_method': 'polynomial',
            'detrend_order': 2,
            'smooth': True,
            'smooth_method': 'savgol',
            'smooth_params': {
                'window_size': 51,
                'polyorder': 3
            }
        },
        fatigue_analysis={
            'sn_standard': 'API',
            'sn_class': 'X',
            'scf': 1.2,
            'mean_stress_correction': 'goodman',
            'design_life_years': 20
        },
        spectral_analysis={
            'method': 'window_averaged',
            'window': 'blackman',
            'nperseg': 2048,
            'n_peaks': 15
        },
        output_dir=Path("./results/advanced_analysis"),
        save_intermediate=True,
        generate_report=True,
        parallel_processing=True,
        n_workers=8
    )
    
    # Process with detailed logging
    import logging
    logging.basicConfig(level=logging.INFO)
    
    processor = OrcaFlexTimeTraceProcessor(config)
    results = processor.process()
    
    # Perform additional analysis
    print("\nCritical Component Identification:")
    
    max_damage = 0
    critical_component = None
    critical_variable = None
    
    for trace_name, trace_results in results['fatigue'].items():
        damage = trace_results['damage_design_life']
        if damage > max_damage:
            max_damage = damage
            parts = trace_name.split('.')
            critical_component = parts[0]
            critical_variable = parts[1] if len(parts) > 1 else 'Unknown'
    
    print(f"  Most critical: {critical_component}")
    print(f"  Variable: {critical_variable}")
    print(f"  Damage ratio: {max_damage:.3f}")
    
    if max_damage > 0.5:
        print(f"  Recommendation: Consider design modification or inspection plan")
    
    return results


def example_5_csv_input():
    """
    Example 5: Process exported CSV time traces
    """
    print("\n" + "="*60)
    print("Example 5: CSV Time Trace Processing")
    print("="*60)
    
    # Configuration for CSV input
    config = TimeTraceConfig(
        source_type='csv',
        file_path=Path("exports/time_traces.csv"),
        preprocessing={
            'detrend': True,
            'smooth': True
        },
        fatigue_analysis={
            'sn_standard': 'BS',
            'sn_class': 'D',
            'scf': 1.1,
            'design_life_years': 30
        },
        spectral_analysis={
            'method': 'periodogram',
            'n_peaks': 5
        },
        output_dir=Path("./results/csv_analysis")
    )
    
    processor = OrcaFlexTimeTraceProcessor(config)
    results = processor.process()
    
    print("\nProcessing complete. Results saved to ./results/csv_analysis/")
    
    return results


def example_6_custom_workflow():
    """
    Example 6: Custom workflow with manual intervention
    """
    print("\n" + "="*60)
    print("Example 6: Custom Workflow with Manual Processing")
    print("="*60)
    
    # Step 1: Load and preprocess only
    config = TimeTraceConfig(
        source_type='orcaflex',
        file_path=Path("simulations/test_case.sim"),
        objects=["Line1"],
        variables=["Tension"],
        preprocessing={
            'detrend': True,
            'remove_outliers': True
        },
        fatigue_analysis={},  # Disable automatic fatigue
        spectral_analysis={},  # Disable automatic spectral
        generate_report=False
    )
    
    processor = OrcaFlexTimeTraceProcessor(config)
    
    # Load traces manually
    time_traces = processor._load_time_traces()
    print(f"Loaded {len(time_traces.columns)-1} time traces")
    
    # Preprocess
    processed_traces = processor._preprocess_signals(time_traces)
    print("Preprocessing complete")
    
    # Custom analysis using signal_analysis module directly
    from src.digitalmodel.modules.signal_analysis import (
        RainflowCounter, SpectralAnalyzer
    )
    
    # Custom rainflow with specific parameters
    counter = RainflowCounter(method='astm')
    tension = processed_traces['Line1.Tension'].values
    
    # Apply custom filter before counting
    from src.digitalmodel.modules.signal_analysis.filters import FrequencyFilter
    filter_obj = FrequencyFilter(filter_type='butterworth', order=4)
    
    # Remove high-frequency noise
    fs = processor._get_sampling_rate(processed_traces)
    filtered_tension = filter_obj.lowpass_filter(tension, cutoff=1.0, fs=fs)
    
    # Count cycles on filtered signal
    cycles = counter.count_cycles(filtered_tension)
    stats = counter.get_statistics(cycles)
    
    print(f"\nCustom Analysis Results:")
    print(f"  Cycles found: {stats['total_cycles']:.0f}")
    print(f"  Max range: {stats['max_range']:.1f}")
    print(f"  Mean range: {stats['mean_range']:.1f}")
    
    # Custom spectral analysis
    analyzer = SpectralAnalyzer(sampling_rate=fs, method='welch')
    spectrum = analyzer.compute_spectrum(filtered_tension, nperseg=512)
    peaks = analyzer.find_peaks(spectrum, n_peaks=3)
    
    print(f"\nDominant frequencies:")
    for _, peak in peaks.iterrows():
        print(f"  {peak['frequency']:.4f} Hz")
    
    return cycles, spectrum


def main():
    """Run all examples"""
    print("\n" + "#"*60)
    print("#" + " "*15 + "ORCAFLEX SIGNAL PROCESSING" + " "*16 + "#")
    print("#" + " "*18 + "Integration Examples" + " "*20 + "#")
    print("#"*60)
    
    # Note: These examples require actual OrcaFlex files
    # They are shown here for demonstration purposes
    
    try:
        # Example 1: Simple fatigue analysis
        # results1 = example_1_simple_analysis()
        print("\nExample 1: Would process mooring line fatigue")
        
        # Example 2: Spectral analysis for VIV
        # results2 = example_2_spectral_analysis()
        print("\nExample 2: Would perform VIV spectral analysis")
        
        # Example 3: Batch processing
        # results3 = example_3_batch_processing()
        print("\nExample 3: Would batch process multiple load cases")
        
        # Example 4: Advanced pipeline
        # results4 = example_4_advanced_pipeline()
        print("\nExample 4: Would run advanced processing pipeline")
        
        # Example 5: CSV input
        # results5 = example_5_csv_input()
        print("\nExample 5: Would process CSV time traces")
        
        # Example 6: Custom workflow
        # results6 = example_6_custom_workflow()
        print("\nExample 6: Would demonstrate custom workflow")
        
    except Exception as e:
        print(f"\nNote: Examples require OrcaFlex files to run")
        print(f"This demonstration shows the architecture and usage patterns")
    
    print("\n" + "#"*60)
    print("# Architecture Benefits:")
    print("#   - Clean separation of OrcaFlex I/O and analysis")
    print("#   - Reusable signal processing components")
    print("#   - Parallel processing for large datasets")
    print("#   - Configuration-driven for reproducibility")
    print("#   - Extensible for custom workflows")
    print("#"*60)


if __name__ == "__main__":
    main()