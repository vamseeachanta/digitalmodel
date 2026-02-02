"""
Hydrodynamic Coefficients Module

Comprehensive management and visualization of frequency-dependent hydrodynamic
coefficients including added mass and damping matrices.

Classes:
    CoefficientDatabase: Main database for hydrodynamic coefficients
    FrequencyDependentMatrix: Container for 6×6 matrices at specific frequencies
    KramersKronigValidator: Validates causality relationships
    AQWAParser: Parser for ANSYS AQWA .LIS files
    HydrodynamicPlotter: Visualization interface for coefficients

Constants:
    DOF_NAMES: List of degree of freedom names
    DOF_INDEX: Mapping from DOF names to indices

Example:
    >>> from digitalmodel.marine_engineering.hydrodynamic_coefficients import CoefficientDatabase
    >>>
    >>> # Load from CSV files
    >>> db = CoefficientDatabase.from_csv("data/hydrodynamic/")
    >>>
    >>> # Get coefficient at specific frequency
    >>> A33 = db.get_added_mass(frequency=0.8, dof_i=2, dof_j=2)
    >>>
    >>> # Create visualizations
    >>> from digitalmodel.marine_engineering.hydrodynamic_coefficients import HydrodynamicPlotter
    >>> plotter = HydrodynamicPlotter(db)
    >>> plotter.plot_frequency_response(dof='Heave', save_path='heave_response.png')
"""

from .coefficients import (
    CoefficientDatabase,
    FrequencyDependentMatrix,
    KramersKronigValidator,
    DOF_NAMES,
    DOF_INDEX
)

from .aqwa_parser import (
    AQWAParser,
    AQWASection,
    AQWADataExtractor
)

from .plotting import HydrodynamicPlotter

__all__ = [
    # Core classes
    'CoefficientDatabase',
    'FrequencyDependentMatrix',
    'KramersKronigValidator',

    # AQWA parsing
    'AQWAParser',
    'AQWASection',
    'AQWADataExtractor',

    # Visualization
    'HydrodynamicPlotter',

    # Constants
    'DOF_NAMES',
    'DOF_INDEX',
]

__version__ = '1.0.0'
__author__ = 'Marine Engineering Team'
__description__ = 'Hydrodynamic coefficient management and visualization'
