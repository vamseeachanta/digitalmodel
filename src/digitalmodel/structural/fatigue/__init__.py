"""
Digital Model Fatigue Analysis Module
====================================

This module provides comprehensive fatigue analysis capabilities for structural
engineering applications, supporting both time-domain and frequency-domain
analysis methods.

Key Features:
- S-N curve models for various international standards (DNV, API, BS, AWS)
- Multiple damage accumulation methods (linear and nonlinear)
- ASTM E1049-85 compliant rainflow counting
- Frequency domain methods (Dirlik, Tovo-Benasciutti, etc.)
- Mean stress corrections and thickness effects
- Critical plane analysis for multiaxial loading
- Statistical analysis and safety factor calculations

Modules:
--------
sn_curves : S-N curve models and standard libraries
damage_accumulation : Fatigue damage calculation methods
rainflow : Rainflow cycle counting algorithms
frequency_domain : Spectral fatigue analysis methods

Quick Start:
-----------
>>> from digitalmodel.structural.fatigue import get_dnv_curve, rainflow_count, LinearDamageAccumulation
>>>
>>> # Get standard S-N curve
>>> sn_curve = get_dnv_curve('D')
>>>
>>> # Count cycles from time series
>>> cycles = rainflow_count(stress_time_series)
>>>
>>> # Calculate fatigue damage
>>> damage_calc = LinearDamageAccumulation()
>>> result = damage_calc.calculate_damage(cycles, sn_curve)
>>> print(f"Total damage: {result['total_damage']:.4f}")

Author: Digital Model Team
Version: 2.0.0
"""

# Import main classes and functions for easy access
from .sn_curves import (
    # Base classes
    SNCurveBase,
    PowerLawSNCurve,
    BilinearSNCurve,

    # Standard curves
    StandardSNCurves,
    get_dnv_curve,
    get_api_curve,
    get_bs_curve,

    # Material properties and corrections
    MaterialProperties,
    MeanStressCorrection,
    ThicknessCorrection,

    # Curve fitting
    SNDataFitting,
    plot_sn_curve
)

from .damage_accumulation import (
    # Base classes
    DamageAccumulationBase,

    # Damage methods
    LinearDamageAccumulation,
    ModifiedMinersRule,
    NonlinearDamageAccumulation,

    # Data structures
    LoadCycle,
    DamageState,

    # Multiaxial analysis
    CriticalPlaneAnalysis,

    # Utilities
    compare_damage_methods
)

from .rainflow import (
    # Main classes
    RainflowCounter,
    RainflowBatch,

    # Configuration
    RainflowParameters,
    CycleStatistics,

    # Convenience functions
    rainflow_count,
    rainflow_with_means
)

# Frequency domain imports (optional, requires scipy)
try:
    from .frequency_domain import (
        # Base classes
        FrequencyDomainBase,

        # Methods
        NarrowBandMethod,
        DirlikMethod,
        TovoBenasciuttiMethod,
        SingleMomentMethod,

        # Data structures
        SpectralMoments,
        FrequencyDomainResult,

        # Response analysis
        ResponsePSDCalculator,

        # Utilities
        compare_frequency_methods
    )
    FREQUENCY_DOMAIN_AVAILABLE = True
except ImportError:
    FREQUENCY_DOMAIN_AVAILABLE = False

from .analysis import (
    # Main analysis engine
    FatigueAnalysisEngine,
    FatigueAnalysisConfig,
    MultislopeSNCurve,

    # Validation and results
    FatigueValidationResults,

    # Convenience functions
    quick_time_domain_analysis,
    quick_frequency_domain_analysis
)

# Version information
__version__ = "2.0.0"
__author__ = "Digital Model Team"

# Module metadata - base components always available
__all__ = [
    # S-N Curves
    'SNCurveBase', 'PowerLawSNCurve', 'BilinearSNCurve', 'MultislopeSNCurve',
    'StandardSNCurves', 'get_dnv_curve', 'get_api_curve', 'get_bs_curve',
    'MaterialProperties', 'MeanStressCorrection', 'ThicknessCorrection',
    'SNDataFitting', 'plot_sn_curve',

    # Damage Accumulation
    'DamageAccumulationBase', 'LinearDamageAccumulation', 'ModifiedMinersRule',
    'NonlinearDamageAccumulation', 'LoadCycle', 'DamageState',
    'CriticalPlaneAnalysis', 'compare_damage_methods',

    # Rainflow Counting
    'RainflowCounter', 'RainflowBatch', 'RainflowParameters',
    'CycleStatistics', 'rainflow_count', 'rainflow_with_means',

    # Analysis Engine
    'FatigueAnalysisEngine', 'FatigueAnalysisConfig', 'FatigueValidationResults',
    'quick_time_domain_analysis', 'quick_frequency_domain_analysis'
]

# Add frequency domain components if available
if FREQUENCY_DOMAIN_AVAILABLE:
    __all__.extend([
        'FrequencyDomainBase', 'NarrowBandMethod', 'DirlikMethod',
        'TovoBenasciuttiMethod', 'SingleMomentMethod', 'SpectralMoments',
        'FrequencyDomainResult', 'ResponsePSDCalculator', 'compare_frequency_methods'
    ])

# Compatibility imports for legacy code
try:
    from ..modules.signal_analysis.fatigue.curves import SNCurve as LegacySNCurve
    from ..modules.signal_analysis.fatigue.damage import FatigueDamageCalculator as LegacyDamageCalculator

    # Add to all for backward compatibility
    __all__.extend(['LegacySNCurve', 'LegacyDamageCalculator'])

except ImportError:
    # Legacy modules not available
    pass

def quick_fatigue_analysis(stress_time_series,
                          sn_curve_standard='DNV',
                          sn_curve_class='D',
                          mean_stress_correction=None,
                          gate_value=0.05):
    """
    Quick fatigue analysis function for rapid assessment

    Parameters
    ----------
    stress_time_series : array-like
        Stress time series data
    sn_curve_standard : str, default='DNV'
        S-N curve standard ('DNV', 'API', 'BS', 'AWS')
    sn_curve_class : str, default='D'
        S-N curve class
    mean_stress_correction : str, optional
        Mean stress correction method
    gate_value : float, default=0.05
        Gate filter for small cycles (relative)

    Returns
    -------
    dict
        Quick analysis results including damage, life, and cycles
    """
    import numpy as np

    # Get S-N curve
    if sn_curve_standard.upper() == 'DNV':
        sn_curve = get_dnv_curve(sn_curve_class)
    elif sn_curve_standard.upper() == 'API':
        sn_curve = get_api_curve(sn_curve_class)
    elif sn_curve_standard.upper() == 'BS':
        sn_curve = get_bs_curve(sn_curve_class)
    else:
        raise ValueError(f"Unknown standard: {sn_curve_standard}")

    # Perform rainflow counting
    params = RainflowParameters(gate_value=gate_value, gate_type='relative')
    counter = RainflowCounter(params)

    if mean_stress_correction:
        cycles = counter.extract_cycles_with_means(stress_time_series)
    else:
        result = counter.count_cycles(stress_time_series)
        cycles = result['cycles'].copy()
        cycles['mean'] = 0.0

    # Calculate damage
    damage_calc = LinearDamageAccumulation(mean_stress_correction=mean_stress_correction)
    damage_result = damage_calc.calculate_damage(cycles, sn_curve)

    # Format results
    return {
        'sn_curve': f"{sn_curve_standard}-{sn_curve_class}",
        'total_cycles': float(cycles['count'].sum()) if len(cycles) > 0 else 0,
        'unique_ranges': len(cycles),
        'max_stress_range': float(cycles['range'].max()) if len(cycles) > 0 else 0,
        'total_damage': damage_result['total_damage'],
        'safety_factor': damage_result['safety_factor'],
        'life_fraction_used': damage_result['life_fraction_used'],
        'estimated_life_cycles': damage_result.get('estimated_remaining_cycles', np.inf)
    }

def create_fatigue_report(analysis_results,
                         output_path=None,
                         include_plots=True):
    """
    Generate comprehensive fatigue analysis report

    Parameters
    ----------
    analysis_results : dict
        Results from fatigue analysis
    output_path : str, optional
        Path to save report (if None, returns string)
    include_plots : bool, default=True
        Whether to include plots in report

    Returns
    -------
    str or None
        Report content if output_path is None, otherwise saves to file
    """
    from datetime import datetime

    report = []
    report.append("=" * 80)
    report.append("FATIGUE ANALYSIS REPORT")
    report.append("=" * 80)
    report.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    report.append(f"Digital Model Fatigue Module v{__version__}")
    report.append("")

    # Analysis summary
    report.append("ANALYSIS SUMMARY")
    report.append("-" * 40)

    if 'sn_curve' in analysis_results:
        report.append(f"S-N Curve: {analysis_results['sn_curve']}")
    if 'total_cycles' in analysis_results:
        report.append(f"Total Cycles: {analysis_results['total_cycles']:,.1f}")
    if 'unique_ranges' in analysis_results:
        report.append(f"Unique Stress Ranges: {analysis_results['unique_ranges']}")
    if 'max_stress_range' in analysis_results:
        report.append(f"Maximum Stress Range: {analysis_results['max_stress_range']:.2f} MPa")

    report.append("")

    # Damage results
    report.append("DAMAGE ASSESSMENT")
    report.append("-" * 40)

    if 'total_damage' in analysis_results:
        damage = analysis_results['total_damage']
        report.append(f"Total Damage: {damage:.6f}")

        if damage < 0.1:
            assessment = "Low damage - structure is safe"
        elif damage < 0.5:
            assessment = "Moderate damage - monitor condition"
        elif damage < 1.0:
            assessment = "High damage - inspection recommended"
        else:
            assessment = "Critical damage - immediate action required"

        report.append(f"Assessment: {assessment}")

    if 'safety_factor' in analysis_results:
        sf = analysis_results['safety_factor']
        if sf == float('inf'):
            report.append("Safety Factor: Infinite (no damage)")
        else:
            report.append(f"Safety Factor: {sf:.2f}")

    if 'life_fraction_used' in analysis_results:
        life_used = analysis_results['life_fraction_used'] * 100
        report.append(f"Life Fraction Used: {life_used:.2f}%")

    report.append("")

    # Recommendations
    report.append("RECOMMENDATIONS")
    report.append("-" * 40)

    if 'total_damage' in analysis_results:
        damage = analysis_results['total_damage']
        if damage < 0.1:
            report.append("- Structure is in good condition")
            report.append("- Continue normal operation")
            report.append("- Regular inspection per schedule")
        elif damage < 0.5:
            report.append("- Increase inspection frequency")
            report.append("- Monitor for crack initiation")
            report.append("- Consider load reduction if possible")
        elif damage < 1.0:
            report.append("- Detailed inspection required")
            report.append("- Non-destructive testing recommended")
            report.append("- Consider structural modifications")
        else:
            report.append("- IMMEDIATE INSPECTION REQUIRED")
            report.append("- Consider load restrictions")
            report.append("- Repair or replacement may be necessary")

    report_text = "\n".join(report)

    if output_path:
        with open(output_path, 'w') as f:
            f.write(report_text)
        return None
    else:
        return report_text

# Module configuration
import logging

# Set up module logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

# Add console handler if none exists
if not logger.handlers:
    handler = logging.StreamHandler()
    formatter = logging.Formatter('%(name)s - %(levelname)s - %(message)s')
    handler.setFormatter(formatter)
    logger.addHandler(handler)

# Module information
def get_module_info():
    """Get information about the fatigue module"""
    return {
        'name': 'Digital Model Fatigue Analysis',
        'version': __version__,
        'author': __author__,
        'description': 'Comprehensive fatigue analysis for structural engineering',
        'standards_supported': ['DNV-RP-C203', 'API RP 2A', 'BS 7608', 'AWS D1.1'],
        'methods_supported': [
            'Rainflow counting (ASTM E1049-85)',
            'Linear damage accumulation (Palmgren-Miner)',
            'Nonlinear damage models',
            'Frequency domain methods',
            'Mean stress corrections',
            'Critical plane analysis'
        ]
    }

# Print module info when imported
logger.info(f"Digital Model Fatigue Module v{__version__} loaded successfully")