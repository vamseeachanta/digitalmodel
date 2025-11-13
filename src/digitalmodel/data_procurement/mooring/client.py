# ABOUTME: Unified mooring client for accessing all mooring component databases
# ABOUTME: System-level operations, standards validation, and OrcaFlex export

"""
Mooring Client
==============

Unified interface for mooring component data procurement.

Features:
- Access all component databases (chains, ropes, anchors, connectors)
- Design mooring lines with automatic component selection
- Standards validation (API RP 2SK, DNV-OS-E301)
- OrcaFlex mooring system export (in-memory)
- Configuration-driven

Example:
    # Initialize from config
    client = MooringClient.from_config("path/to/config.yml")

    # Design mooring line
    line = client.design_mooring_line(
        design_load=5000,  # kN
        water_depth=1500,  # m
        mooring_type='catenary'
    )

    # Export to OrcaFlex
    orcaflex_yaml = client.to_orcaflex_mooring(line)
"""

import logging
from typing import Dict, Any, List, Optional
from datetime import datetime
from pathlib import Path
import yaml

from ..common.config_loader import ConfigLoader
from .database_clients import (
    ChainDatabaseClient,
    WireRopeDatabaseClient,
    SyntheticRopeDatabaseClient,
    AnchorDatabaseClient,
    ConnectorDatabaseClient
)

logger = logging.getLogger(__name__)


class MooringClient:
    """
    Unified mooring client.

    Provides access to all mooring component databases and system-level operations.

    Architecture:
    - Component-based queries (chains, ropes, anchors, connectors)
    - System design (complete mooring line design)
    - Standards validation (API RP 2SK, DNV-OS-E301)
    - Direct consumption (OrcaFlex mooring YAML)
    """

    def __init__(self, config: Dict[str, Any] = None):
        """
        Initialize mooring client.

        Args:
            config: Configuration dictionary (typically loaded from YAML)
        """
        self.config = config or {}

        # Initialize component database clients
        self.chain_client = ChainDatabaseClient()
        self.wire_rope_client = WireRopeDatabaseClient()
        self.synthetic_rope_client = SyntheticRopeDatabaseClient()
        self.anchor_client = AnchorDatabaseClient()
        self.connector_client = ConnectorDatabaseClient()

        # Get standards configuration
        self.standards = self.config.get('standards', {})
        self.safety_factors = self.standards.get('safety_factors', {
            'intact': 1.67,  # API RP 2SK default
            'damaged': 1.25
        })

        logger.info("Initialized MooringClient with all component databases")

    @classmethod
    def from_config(cls, config_path: str) -> 'MooringClient':
        """
        Create client from YAML configuration file.

        Args:
            config_path: Path to YAML config file

        Returns:
            Initialized MooringClient

        Example:
            client = MooringClient.from_config(
                "specs/modules/data-procurement/mooring-systems/configs/example_config.yml"
            )
        """
        config_loader = ConfigLoader(config_path)

        if not config_loader.validate_schema():
            raise ValueError(f"Invalid configuration: {config_path}")

        return cls(config_loader.to_dict())

    # Component database access methods

    def get_chain(self, grade: str, diameter: float) -> Dict[str, Any]:
        """Get chain specification."""
        return self.chain_client.get_chain(grade, diameter)

    def get_wire_rope(self, construction: str, diameter: float) -> Dict[str, Any]:
        """Get wire rope specification."""
        return self.wire_rope_client.get_wire_rope(construction, diameter)

    def get_synthetic_rope(self, fiber_type: str, diameter: float) -> Dict[str, Any]:
        """Get synthetic rope specification."""
        return self.synthetic_rope_client.get_synthetic_rope(fiber_type, diameter)

    def get_anchor(self, anchor_type: str, weight: float) -> Dict[str, Any]:
        """Get anchor specification."""
        return self.anchor_client.get_anchor(anchor_type, weight)

    def get_connector(self, connector_type: str, pin_diameter: float) -> Dict[str, Any]:
        """Get connector specification."""
        return self.connector_client.get_connector(connector_type, pin_diameter)

    # System-level design methods

    def design_mooring_line(self, design_load: float,
                           water_depth: float,
                           mooring_type: str = 'catenary',
                           soil_type: str = 'clay',
                           safety_factor: float = None) -> Dict[str, Any]:
        """
        Design complete mooring line with automatic component selection.

        Args:
            design_load: Design load in kN
            water_depth: Water depth in m
            mooring_type: 'catenary', 'taut_leg', or 'semi_taut'
            soil_type: 'clay' or 'sand'
            safety_factor: Safety factor (default from config)

        Returns:
            Complete mooring line design dict

        Example:
            line = client.design_mooring_line(
                design_load=5000,
                water_depth=1500,
                mooring_type='catenary'
            )
        """
        if safety_factor is None:
            safety_factor = self.safety_factors.get('intact', 1.67)

        logger.info(f"Designing {mooring_type} mooring line: {design_load}kN @ {water_depth}m depth")

        # Design based on mooring type
        if mooring_type == 'catenary':
            line = self._design_catenary_mooring(design_load, water_depth,
                                                 soil_type, safety_factor)
        elif mooring_type == 'taut_leg':
            line = self._design_taut_leg_mooring(design_load, water_depth,
                                                 soil_type, safety_factor)
        elif mooring_type == 'semi_taut':
            line = self._design_semi_taut_mooring(design_load, water_depth,
                                                  soil_type, safety_factor)
        else:
            raise ValueError(f"Unknown mooring type: {mooring_type}")

        # Validate design
        validation = self.validate_design(line, safety_factor)
        line['validation'] = validation

        return line

    def _design_catenary_mooring(self, design_load: float, water_depth: float,
                                 soil_type: str, safety_factor: float) -> Dict[str, Any]:
        """
        Design catenary mooring (chain-only or chain-wire-chain).

        Typical configuration:
        - Upper chain: 100-200m (fairlead to touchdown)
        - Wire rope: Mid-section (if deepwater)
        - Lower chain: 100-200m (ground chain)
        - Anchor: Drag embedment
        """
        # Select upper chain
        upper_chains = self.chain_client.find_by_design_load(design_load,
                                                             safety_factor=safety_factor)

        if not upper_chains:
            raise ValueError(f"No chain found for design load {design_load} kN")

        upper_chain = upper_chains[0]  # Smallest suitable chain

        # Estimate line lengths
        upper_length = min(200, water_depth * 0.2)  # 20% of depth, max 200m

        # For deepwater (>500m), use wire rope in middle
        if water_depth > 500:
            # Select wire rope
            wire_ropes = self.wire_rope_client.find_by_design_load(design_load,
                                                                   safety_factor=safety_factor)
            if wire_ropes:
                wire_rope = wire_ropes[0]
                wire_length = water_depth - upper_length - 200  # Remainder after chains
            else:
                wire_rope = None
                wire_length = 0
        else:
            wire_rope = None
            wire_length = 0

        # Lower chain (ground chain)
        lower_chain = upper_chain.copy()  # Same grade as upper
        lower_length = min(200, water_depth * 0.2)

        # Total line length (catenary requires ~1.5-2x depth)
        if wire_rope:
            total_length = upper_length + wire_length + lower_length
        else:
            total_length = water_depth * 1.5  # All-chain catenary
            lower_length = total_length - upper_length

        # Select anchor
        # Holding capacity ≈ 2 × design_load (typical for catenary)
        required_anchor_capacity = design_load * 2

        anchors = self.anchor_client.find_by_holding_capacity(
            required_anchor_capacity,
            soil=soil_type,
            safety_factor=1.5
        )

        if not anchors:
            raise ValueError(f"No anchor found for capacity {required_anchor_capacity} kN")

        anchor = anchors[0]  # Lightest suitable anchor

        # Select connectors
        # Upper connector (fairlead to chain)
        upper_connector = self.connector_client.find_for_chain(
            upper_chain['grade'],
            upper_chain['diameter']
        )[0]

        # Lower connector (chain to anchor)
        lower_connector = self.connector_client.find_for_chain(
            lower_chain['grade'],
            lower_chain['diameter']
        )[0]

        # Build mooring line
        segments = [
            {
                'type': 'chain',
                'component': upper_chain,
                'length': upper_length,
                'name': 'Upper Chain'
            }
        ]

        if wire_rope:
            segments.append({
                'type': 'wire_rope',
                'component': wire_rope,
                'length': wire_length,
                'name': 'Middle Wire Rope'
            })

        segments.append({
            'type': 'chain',
            'component': lower_chain,
            'length': lower_length,
            'name': 'Lower Chain (Ground Chain)'
        })

        return {
            'mooring_type': 'catenary',
            'design_load': design_load,
            'water_depth': water_depth,
            'total_length': total_length,
            'segments': segments,
            'anchor': anchor,
            'connectors': [upper_connector, lower_connector],
            'soil_type': soil_type,
            'safety_factor': safety_factor
        }

    def _design_taut_leg_mooring(self, design_load: float, water_depth: float,
                                 soil_type: str, safety_factor: float) -> Dict[str, Any]:
        """
        Design taut-leg mooring (synthetic rope or wire rope).

        Typical configuration:
        - Synthetic rope (polyester or HMPE for deepwater)
        - Minimal ground chain
        - High holding capacity anchor
        """
        # Select synthetic rope (preferred for deepwater)
        if water_depth > 1000:
            fiber_type = 'hmpe'  # Ultra-deepwater, lightweight
        else:
            fiber_type = 'polyester'  # Standard deepwater

        synthetic_ropes = self.synthetic_rope_client.find_by_design_load(
            design_load,
            fiber_type=fiber_type,
            safety_factor=safety_factor
        )

        if not synthetic_ropes:
            raise ValueError(f"No synthetic rope found for design load {design_load} kN")

        synthetic_rope = synthetic_ropes[0]

        # Taut-leg length ≈ 1.05-1.1 × depth (low pretension)
        rope_length = water_depth * 1.07

        # Small ground chain (for abrasion protection)
        ground_chains = self.chain_client.find_by_design_load(design_load,
                                                              safety_factor=safety_factor)
        ground_chain = ground_chains[0] if ground_chains else None
        ground_length = 50  # Short ground chain

        # High holding capacity anchor (taut-leg has higher anchor loads)
        required_anchor_capacity = design_load * 2.5

        anchors = self.anchor_client.find_by_holding_capacity(
            required_anchor_capacity,
            soil=soil_type,
            safety_factor=1.5
        )

        anchor = anchors[0] if anchors else None

        # Connectors
        connector = self.connector_client.find_by_working_load(design_load)[0]

        segments = [
            {
                'type': 'synthetic_rope',
                'component': synthetic_rope,
                'length': rope_length,
                'name': f'{fiber_type.capitalize()} Taut-Leg Rope'
            }
        ]

        if ground_chain:
            segments.append({
                'type': 'chain',
                'component': ground_chain,
                'length': ground_length,
                'name': 'Ground Chain'
            })

        return {
            'mooring_type': 'taut_leg',
            'design_load': design_load,
            'water_depth': water_depth,
            'total_length': rope_length + (ground_length if ground_chain else 0),
            'segments': segments,
            'anchor': anchor,
            'connectors': [connector],
            'soil_type': soil_type,
            'safety_factor': safety_factor
        }

    def _design_semi_taut_mooring(self, design_load: float, water_depth: float,
                                  soil_type: str, safety_factor: float) -> Dict[str, Any]:
        """
        Design semi-taut mooring (chain-polyester-chain).

        Typical configuration:
        - Upper chain
        - Polyester rope
        - Lower chain
        - Moderate anchor capacity
        """
        # Select chain
        chains = self.chain_client.find_by_design_load(design_load,
                                                       safety_factor=safety_factor)
        chain = chains[0] if chains else None

        # Select polyester rope
        polyester_ropes = self.synthetic_rope_client.find_by_design_load(
            design_load,
            fiber_type='polyester',
            safety_factor=safety_factor
        )
        polyester_rope = polyester_ropes[0] if polyester_ropes else None

        # Lengths (semi-taut: ~1.2-1.3x depth)
        upper_length = 100
        polyester_length = water_depth * 1.2 - 200
        lower_length = 100

        # Anchor
        required_capacity = design_load * 2.2
        anchors = self.anchor_client.find_by_holding_capacity(required_capacity,
                                                              soil=soil_type,
                                                              safety_factor=1.5)
        anchor = anchors[0] if anchors else None

        # Connectors
        connector = self.connector_client.find_for_chain(chain['grade'],
                                                         chain['diameter'])[0]

        segments = [
            {
                'type': 'chain',
                'component': chain,
                'length': upper_length,
                'name': 'Upper Chain'
            },
            {
                'type': 'synthetic_rope',
                'component': polyester_rope,
                'length': polyester_length,
                'name': 'Polyester Rope'
            },
            {
                'type': 'chain',
                'component': chain,
                'length': lower_length,
                'name': 'Lower Chain'
            }
        ]

        return {
            'mooring_type': 'semi_taut',
            'design_load': design_load,
            'water_depth': water_depth,
            'total_length': upper_length + polyester_length + lower_length,
            'segments': segments,
            'anchor': anchor,
            'connectors': [connector],
            'soil_type': soil_type,
            'safety_factor': safety_factor
        }

    def validate_design(self, mooring_line: Dict[str, Any],
                       safety_factor: float) -> Dict[str, Any]:
        """
        Validate mooring line design against standards.

        Args:
            mooring_line: Mooring line design dict
            safety_factor: Required safety factor

        Returns:
            Validation results dict

        Example:
            validation = client.validate_design(line, safety_factor=1.67)
            print(f"Valid: {validation['valid']}")
        """
        issues = []
        warnings = []

        # Check all components meet design load
        design_load = mooring_line['design_load']

        for segment in mooring_line['segments']:
            component = segment['component']

            if segment['type'] == 'chain':
                component_capacity = component['minimum_breaking_load'] / safety_factor

                if component_capacity < design_load:
                    issues.append(f"Chain {component['grade']} {component['diameter']}mm insufficient: "
                                f"{component_capacity:.0f}kN < {design_load:.0f}kN")

            elif segment['type'] in ['wire_rope', 'synthetic_rope']:
                component_capacity = component['minimum_breaking_load'] / safety_factor

                if component_capacity < design_load:
                    issues.append(f"Rope insufficient: {component_capacity:.0f}kN < {design_load:.0f}kN")

        # Check anchor capacity
        anchor = mooring_line.get('anchor')
        if anchor:
            # For catenary: anchor load ≈ design_load
            # For taut-leg: anchor load ≈ 1.5 × design_load
            if mooring_line['mooring_type'] == 'taut_leg':
                required_anchor_capacity = design_load * 1.5
            else:
                required_anchor_capacity = design_load

            if anchor['design_capacity_kn'] < required_anchor_capacity:
                issues.append(f"Anchor capacity insufficient: "
                            f"{anchor['design_capacity_kn']:.0f}kN < {required_anchor_capacity:.0f}kN")

        # Warnings
        if mooring_line['mooring_type'] == 'catenary':
            if mooring_line['total_length'] < mooring_line['water_depth'] * 1.3:
                warnings.append("Catenary length may be too short (typical: 1.5-2x depth)")

        return {
            'valid': len(issues) == 0,
            'issues': issues,
            'warnings': warnings,
            'safety_factor': safety_factor,
            'standard': 'API RP 2SK'
        }

    def to_orcaflex_mooring(self, mooring_line: Dict[str, Any],
                           name: str = None) -> str:
        """
        Convert mooring line to OrcaFlex mooring YAML (in-memory).

        Args:
            mooring_line: Mooring line design dict
            name: Mooring line name

        Returns:
            OrcaFlex mooring YAML string (in-memory, not saved)

        Example:
            line = client.design_mooring_line(design_load=5000, water_depth=1500)
            yaml = client.to_orcaflex_mooring(line)
            # Pass directly to OrcaFlex API (no file I/O)
        """
        if name is None:
            name = f"MooringLine_{mooring_line['mooring_type']}"

        # Build line type definitions for each segment
        line_types = []

        for i, segment in enumerate(mooring_line['segments']):
            component = segment['component']
            segment_name = f"{name}_Segment{i+1}"

            if segment['type'] == 'chain':
                line_type_yaml = self.chain_client.to_orcaflex_line_type(
                    component,
                    name=segment_name
                )
            elif segment['type'] == 'wire_rope':
                line_type_yaml = self.wire_rope_client.to_orcaflex_line_type(
                    component,
                    name=segment_name
                )
            elif segment['type'] == 'synthetic_rope':
                line_type_yaml = self.synthetic_rope_client.to_orcaflex_line_type(
                    component,
                    name=segment_name,
                    include_nonlinear=True
                )

            line_types.append(line_type_yaml)

        # Build mooring line definition
        segments_list = []

        for i, segment in enumerate(mooring_line['segments']):
            segments_list.append({
                'LineType': f"{name}_Segment{i+1}",
                'Length': segment['length'],
                'TargetSegmentLength': 10.0  # 10m segments typical
            })

        orcaflex_mooring = {
            'Line': {
                'Name': name,
                'LineType': 'Multisegment',
                'Segments': segments_list
            }
        }

        # Combine line types and mooring definition
        all_yaml = "\n\n".join(line_types)
        all_yaml += "\n\n" + yaml.dump(orcaflex_mooring,
                                      default_flow_style=False,
                                      sort_keys=False)

        return all_yaml
