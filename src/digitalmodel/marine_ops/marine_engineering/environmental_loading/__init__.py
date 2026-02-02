"""
Environmental Loading Module for Marine Engineering.

This module provides tools for calculating environmental loads on marine structures,
including OCIMF wind and current coefficients.

Main classes:
    - OCIMFDatabase: OCIMF coefficient database with interpolation
    - EnvironmentalForces: Force calculation engine
    - OCIMFCoefficients: Dataclass for coefficients
    - EnvironmentalConditions: Environmental parameters
    - VesselGeometry: Vessel geometry parameters
    - EnvironmentalForceResults: Force calculation results

Example:
    >>> from digitalmodel.marine_engineering.environmental_loading import (
    ...     OCIMFDatabase, EnvironmentalForces, EnvironmentalConditions,
    ...     VesselGeometry
    ... )
    >>>
    >>> # Load database
    >>> db = OCIMFDatabase('data/ocimf/ocimf_coefficients.csv')
    >>>
    >>> # Get coefficients
    >>> coeffs = db.get_coefficients(heading=45, displacement=250000)
    >>>
    >>> # Calculate forces
    >>> calc = EnvironmentalForces(db)
    >>> conditions = EnvironmentalConditions(
    ...     wind_speed=20.0,
    ...     wind_direction=45,
    ...     current_speed=1.5,
    ...     current_direction=30
    ... )
    >>> geometry = VesselGeometry(loa=330, beam=60, draft=22)
    >>> results = calc.calculate_total_forces(conditions, geometry, 250000)
"""

from .ocimf import (
    OCIMFCoefficients,
    OCIMFDatabase,
    EnvironmentalConditions,
    VesselGeometry,
    EnvironmentalForceResults,
    EnvironmentalForces,
    create_sample_database,
)

__all__ = [
    'OCIMFCoefficients',
    'OCIMFDatabase',
    'EnvironmentalConditions',
    'VesselGeometry',
    'EnvironmentalForceResults',
    'EnvironmentalForces',
    'create_sample_database',
]

__version__ = '0.1.0'
