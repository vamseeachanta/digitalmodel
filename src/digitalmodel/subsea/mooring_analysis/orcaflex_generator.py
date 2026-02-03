#!/usr/bin/env python3
"""
ABOUTME: OrcaFlex model generator for mooring systems, creating YAML configurations
for lines, line types, anchors, and vessel connections.
"""

from typing import Dict, List, Set
import yaml
from pathlib import Path
import logging

from .models import (
    MooringSystem,
    MooringLine,
    MooringLineProperties,
    LineType
)

logger = logging.getLogger(__name__)


class OrcaFlexModelGenerator:
    """Generate OrcaFlex model files for mooring analysis."""

    def __init__(self, system: MooringSystem):
        """
        Initialize OrcaFlex model generator.

        Args:
            system: Complete mooring system configuration
        """
        self.system = system

    def _get_line_type_name(self, seg: MooringLineProperties) -> str:
        """
        Generate unique line type name for OrcaFlex.

        Args:
            seg: Mooring line segment properties

        Returns:
            Line type name string
        """
        return f"{seg.line_type.value}_{int(seg.diameter)}mm"

    def generate_line_type(self, seg: MooringLineProperties) -> Dict:
        """
        Generate OrcaFlex line type definition.

        Args:
            seg: Mooring line segment properties

        Returns:
            Dictionary with OrcaFlex line type parameters
        """
        import numpy as np

        # Convert units for OrcaFlex (SI units)
        od_m = seg.diameter / 1000.0  # mm to m

        # Mass per unit length in air (kg/m)
        # For chain: weight_water + displaced water weight
        # For synthetic: weight_water is negative (buoyant)
        if seg.line_type == LineType.CHAIN:
            # Chain density approximately 7850 kg/m³
            volume = np.pi * (od_m / 2)**2  # m³/m for solid cylinder
            displaced_mass = 1025 * volume
            mass_air = seg.weight_water + displaced_mass
        else:
            # For wire/polyester, weight_water accounts for buoyancy
            displaced_mass = 1025 * np.pi * (od_m / 2)**2
            mass_air = seg.weight_water + displaced_mass

        return {
            'Name': self._get_line_type_name(seg),
            'Category': seg.line_type.value.capitalize(),
            'OD': round(od_m, 4),
            'MassPerUnitLength': round(mass_air, 2),
            'EA': round(seg.ea * 1000, 0),  # kN to N
            'EI': 0.0,  # Flexible line (no bending stiffness)
            'NormalDragCoefficient': seg.drag_coeff,
            'AxialDragCoefficient': 0.1,
            'NormalAddedMassCoefficient': 1.0,
            'AxialAddedMassCoefficient': 0.0,
            'MBL': round(seg.mbl * 1000, 0),  # kN to N
        }

    def generate_line_data(self, line: MooringLine) -> Dict:
        """
        Generate OrcaFlex line data for a mooring line.

        Args:
            line: Mooring line configuration

        Returns:
            Dictionary with OrcaFlex line parameters
        """
        line_data = {
            'Name': line.line_id,
            'LineType': [],
            'Length': [],
            'TargetSegmentLength': [],
            'EndAConnection': 'Fixed',  # Anchor
            'EndAX': round(line.anchor.location[0], 2),
            'EndAY': round(line.anchor.location[1], 2),
            'EndAZ': round(line.anchor.location[2], 2),
            'EndBConnection': 'Vessel',  # Fairlead
            'EndBX': round(line.fairlead_location[0], 2),
            'EndBY': round(line.fairlead_location[1], 2),
            'EndBZ': round(line.fairlead_location[2], 2),
            'IncludeTorsion': 'No',
            'StaticsStep1': 'User specified profile',
        }

        # Add segments
        for seg in line.segments:
            line_data['LineType'].append(self._get_line_type_name(seg))
            line_data['Length'].append(round(seg.length, 2))
            # Target segment length: ~20 segments per line section
            target_seg_length = min(10.0, seg.length / 20)
            line_data['TargetSegmentLength'].append(round(target_seg_length, 2))

        return line_data

    def generate_vessel_data(self) -> Dict:
        """
        Generate vessel data for OrcaFlex.

        Returns:
            Dictionary with vessel parameters
        """
        vessel = self.system.vessel

        return {
            'Name': 'Vessel',
            'VesselType': vessel.vessel_type.upper(),
            'Length': round(vessel.length, 2),
            'Width': round(vessel.beam, 2),
            'Draft': round(vessel.draft, 2),
            'Displacement': round(vessel.displacement, 0),
            'Connection': 'Free',
            'InitialX': 0.0,
            'InitialY': 0.0,
            'InitialZ': 0.0,
            'InitialHeading': 0.0,
        }

    def generate_environment_data(self) -> Dict:
        """
        Generate environment data for OrcaFlex.

        Returns:
            Dictionary with environment parameters
        """
        return {
            'WaterDepth': round(self.system.water_depth, 2),
            'WaterDensity': 1025.0,
            'KinematicViscosity': 1.19e-6,
            'SeabedModel': 'Flat',
            'SeabedOriginX': 0.0,
            'SeabedOriginY': 0.0,
            'SeabedOriginZ': -round(self.system.water_depth, 2),
        }

    def generate_model_yml(self, output_path: str) -> str:
        """
        Generate complete YAML configuration for OrcaFlex model.

        Args:
            output_path: Path to save YAML file

        Returns:
            Path to generated file
        """
        # Collect unique line types
        line_types_seen: Set[str] = set()
        line_types = []

        for line in self.system.lines:
            for seg in line.segments:
                type_name = self._get_line_type_name(seg)
                if type_name not in line_types_seen:
                    line_types.append(self.generate_line_type(seg))
                    line_types_seen.add(type_name)

        # Generate lines
        lines = [self.generate_line_data(line) for line in self.system.lines]

        # Build complete model
        model = {
            'General': {
                'UnitsSystem': 'SI',
                'StageDuration': [
                    {'StaticState': 100.0},
                    {'InstantaneousChange': 0.0},
                    {'Simulation': 3600.0}  # 1 hour simulation
                ]
            },
            'Environment': self.generate_environment_data(),
            'Vessel': self.generate_vessel_data(),
            'LineTypes': line_types,
            'Lines': lines,
        }

        # Write YAML file
        output_file = Path(output_path)
        output_file.parent.mkdir(parents=True, exist_ok=True)

        with open(output_file, 'w') as f:
            yaml.dump(model, f, default_flow_style=False, sort_keys=False)

        logger.info(f"Generated OrcaFlex model: {output_file}")
        logger.info(f"  - {len(line_types)} line types")
        logger.info(f"  - {len(lines)} mooring lines")

        return str(output_file)

    def generate_model_dict(self) -> Dict:
        """
        Generate complete model as dictionary (without writing to file).

        Returns:
            Dictionary with complete OrcaFlex model
        """
        # Collect unique line types
        line_types_seen: Set[str] = set()
        line_types = []

        for line in self.system.lines:
            for seg in line.segments:
                type_name = self._get_line_type_name(seg)
                if type_name not in line_types_seen:
                    line_types.append(self.generate_line_type(seg))
                    line_types_seen.add(type_name)

        # Generate lines
        lines = [self.generate_line_data(line) for line in self.system.lines]

        return {
            'General': {
                'UnitsSystem': 'SI',
            },
            'Environment': self.generate_environment_data(),
            'Vessel': self.generate_vessel_data(),
            'LineTypes': line_types,
            'Lines': lines,
        }
