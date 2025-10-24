# ABOUTME: Unified riser client for accessing all riser component databases
# ABOUTME: System-level operations, property calculations, and OrcaFlex export

"""
Riser Client
=============

Unified interface for riser component data procurement.

Features:
- Access pipe specification database
- Calculate complete riser properties
- OrcaFlex riser export
- Configuration-driven

Example:
    from digitalmodel.data_procurement import RiserClient

    client = RiserClient()

    # Get pipe specification
    pipe = client.get_pipe_specification(diameter=10, schedule='SCH 80', grade='X52')

    # Calculate properties with coatings
    props = client.calculate_properties(
        pipe=pipe,
        coatings=[
            {'type': '3LPE', 'thickness': 3.2, 'density': 940},
            {'type': 'insulation', 'thickness': 50, 'density': 500}
        ],
        contents_density=850  # oil
    )

    # Export to OrcaFlex
    yaml_output = client.to_orcaflex_line_type(props)
"""

import logging
from typing import Dict, Any, List, Optional

from .database_clients import PipeSpecificationClient

logger = logging.getLogger(__name__)


class RiserClient:
    """
    Unified riser client.

    Provides access to all riser component databases and system-level operations.
    """

    def __init__(self, config: Dict[str, Any] = None):
        """
        Initialize riser client.

        Args:
            config: Configuration dictionary (optional)
        """
        self.config = config or {}

        # Initialize component database clients
        self.pipe_client = PipeSpecificationClient()

        logger.info("Initialized RiserClient with pipe database")

    def get_pipe_specification(self, diameter: float, schedule: str = 'STD',
                              grade: str = 'X52') -> Dict[str, Any]:
        """
        Get pipe specification with calculated properties.

        Args:
            diameter: Nominal diameter in inches
            schedule: Schedule designation (default 'STD')
            grade: API 5L grade (default 'X52')

        Returns:
            Pipe specification dict

        Raises:
            ValueError: If pipe not found

        Example:
            pipe = client.get_pipe_specification(diameter=10, schedule='SCH 80', grade='X52')
            print(f"OD: {pipe['outer_diameter_mm']:.1f} mm")
            print(f"Wall: {pipe['wall_thickness_mm']:.1f} mm")
            print(f"Mass: {pipe['mass_per_meter_kg']:.2f} kg/m")
        """
        pipes = self.pipe_client.find_by_diameter(diameter, schedule, grade)

        if not pipes:
            raise ValueError(f"No pipe found for {diameter}\" {schedule} {grade}")

        return pipes[0]

    def calculate_properties(self, pipe: Dict[str, Any],
                            coatings: List[Dict[str, Any]] = None,
                            contents_density: float = 0) -> Dict[str, Any]:
        """
        Calculate complete riser properties with coatings and contents.

        Args:
            pipe: Base pipe specification
            coatings: List of coating dicts with 'type', 'thickness' (mm), 'density' (kg/m³)
            contents_density: Contents density in kg/m³ (default 0 for empty)

        Returns:
            Complete riser properties dict including mass, EA, EI, GJ

        Example:
            pipe = client.get_pipe_specification(10, 'SCH 80', 'X52')
            props = client.calculate_properties(
                pipe=pipe,
                coatings=[
                    {'type': '3LPE', 'thickness': 3.2, 'density': 940},
                    {'type': 'insulation', 'thickness': 50, 'density': 500}
                ],
                contents_density=850  # oil
            )
            print(f"EA: {props['axial_stiffness_ea_n']/1e9:.2f} GN")
            print(f"EI: {props['bending_stiffness_ei_nm2']/1e6:.2f} MN·m²")
        """
        return self.pipe_client.calculate_properties(pipe, coatings, contents_density)

    def to_orcaflex_line_type(self, pipe_props: Dict[str, Any],
                             name: str = None) -> str:
        """
        Convert riser properties to OrcaFlex line type YAML.

        Args:
            pipe_props: Pipe properties dict (from calculate_properties)
            name: Line type name (optional)

        Returns:
            OrcaFlex line type YAML string

        Example:
            pipe = client.get_pipe_specification(10, 'SCH 80', 'X52')
            props = client.calculate_properties(pipe, coatings=[...])
            yaml_output = client.to_orcaflex_line_type(props, name="Production_Riser")
        """
        return self.pipe_client.to_orcaflex_line_type(pipe_props, name)
