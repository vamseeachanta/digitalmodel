"""
Pipe Properties Calculation Module for API STD 2RD Analysis

This module provides functions for calculating pipe geometric and section properties
used in API STD 2RD analysis.

Author: Digitally Modernized from Legacy Code
Date: 2025-01-15
"""

import math
from typing import Dict, Any, Union
import logging

logger = logging.getLogger(__name__)


def calculate_section_properties(outer_diameter: float, inner_diameter: float) -> Dict[str, float]:
    """
    Calculate pipe section properties including areas and moment of inertia.

    Args:
        outer_diameter: Outer diameter of the pipe
        inner_diameter: Inner diameter of the pipe

    Returns:
        Dictionary containing calculated section properties:
        - A: Cross-sectional area of pipe wall
        - Ai: Internal area
        - Ao: External area
        - I: Second moment of area

    Raises:
        ValueError: If diameters are invalid
    """
    if outer_diameter <= 0 or inner_diameter <= 0:
        raise ValueError("Diameters must be positive values")

    if inner_diameter >= outer_diameter:
        raise ValueError("Inner diameter must be less than outer diameter")

    # Cross-sectional area of pipe wall
    area_wall = (math.pi / 4) * (outer_diameter**2 - inner_diameter**2)

    # Internal area
    area_internal = (math.pi / 4) * (inner_diameter**2)

    # External area
    area_external = (math.pi / 4) * (outer_diameter**2)

    # Second moment of area
    moment_inertia = (math.pi / 64) * (outer_diameter**4 - inner_diameter**4)

    properties = {
        "A": area_wall,
        "Ai": area_internal,
        "Ao": area_external,
        "I": moment_inertia
    }

    logger.debug(f"Calculated section properties for OD={outer_diameter}, ID={inner_diameter}: {properties}")

    return properties


def calculate_wall_thickness(outer_diameter: float, inner_diameter: float) -> float:
    """
    Calculate wall thickness from outer and inner diameters.

    Args:
        outer_diameter: Outer diameter of the pipe
        inner_diameter: Inner diameter of the pipe

    Returns:
        Wall thickness

    Raises:
        ValueError: If diameters are invalid
    """
    if outer_diameter <= 0 or inner_diameter <= 0:
        raise ValueError("Diameters must be positive values")

    if inner_diameter >= outer_diameter:
        raise ValueError("Inner diameter must be less than outer diameter")

    return (outer_diameter - inner_diameter) / 2


def calculate_diameter_from_thickness(
    known_diameter: float,
    wall_thickness: float,
    diameter_type: str
) -> float:
    """
    Calculate the other diameter when one diameter and wall thickness are known.

    Args:
        known_diameter: The known diameter (outer or inner)
        wall_thickness: Wall thickness of the pipe
        diameter_type: Type of known diameter ('outer' or 'inner')

    Returns:
        The calculated diameter

    Raises:
        ValueError: If inputs are invalid
    """
    if known_diameter <= 0 or wall_thickness <= 0:
        raise ValueError("Diameter and wall thickness must be positive values")

    if diameter_type.lower() == 'outer':
        inner_diameter = known_diameter - 2 * wall_thickness
        if inner_diameter <= 0:
            raise ValueError("Wall thickness too large for given outer diameter")
        return inner_diameter
    elif diameter_type.lower() == 'inner':
        return known_diameter + 2 * wall_thickness
    else:
        raise ValueError("diameter_type must be 'outer' or 'inner'")


def calculate_geometric_properties(
    outer_diameter: Union[float, None] = None,
    inner_diameter: Union[float, None] = None,
    wall_thickness: Union[float, None] = None
) -> Dict[str, float]:
    """
    Calculate all geometric properties from available parameters.

    Args:
        outer_diameter: Outer diameter (optional if other parameters provided)
        inner_diameter: Inner diameter (optional if other parameters provided)
        wall_thickness: Wall thickness (optional if both diameters provided)

    Returns:
        Dictionary containing all geometric and section properties

    Raises:
        ValueError: If insufficient parameters provided or values are invalid
    """
    # Determine missing parameter
    if outer_diameter is not None and inner_diameter is not None:
        # Both diameters provided
        od = outer_diameter
        id_calc = inner_diameter
        wt = calculate_wall_thickness(od, id_calc)
    elif outer_diameter is not None and wall_thickness is not None:
        # OD and WT provided
        od = outer_diameter
        wt = wall_thickness
        id_calc = calculate_diameter_from_thickness(od, wt, 'outer')
    elif inner_diameter is not None and wall_thickness is not None:
        # ID and WT provided
        id_calc = inner_diameter
        wt = wall_thickness
        od = calculate_diameter_from_thickness(id_calc, wt, 'inner')
    else:
        raise ValueError("At least two of the three parameters (OD, ID, WT) must be provided")

    # Calculate section properties
    section_props = calculate_section_properties(od, id_calc)

    # Combine all properties
    properties = {
        "outer_diameter": od,
        "inner_diameter": id_calc,
        "wall_thickness": wt,
        **section_props
    }

    return properties


def validate_pipe_geometry(
    outer_diameter: float,
    inner_diameter: float,
    wall_thickness: float,
    tolerance: float = 1e-6
) -> bool:
    """
    Validate that pipe geometry parameters are consistent.

    Args:
        outer_diameter: Outer diameter
        inner_diameter: Inner diameter
        wall_thickness: Wall thickness
        tolerance: Tolerance for validation check

    Returns:
        True if geometry is valid, False otherwise
    """
    expected_wall_thickness = (outer_diameter - inner_diameter) / 2
    return abs(wall_thickness - expected_wall_thickness) <= tolerance