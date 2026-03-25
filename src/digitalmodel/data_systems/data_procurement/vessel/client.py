# ABOUTME: Unified vessel data procurement client across multiple FREE APIs and databases
# ABOUTME: Automatic provider selection, fallbacks, and streaming vessel-based queries

"""
Vessel Client
=============

Unified interface for vessel data procurement from multiple FREE sources.

Features:
- Automatic provider selection (MarineTraffic, ShipXplorer, Generic RAO DB)
- Fallback mechanism (if primary fails, try fallback)
- Provider-agnostic vessel queries (IMO/MMSI/name → data)
- Streaming architecture with zero storage
- Configuration-driven (YAML)

Example:
    # Initialize from config
    client = VesselClient.from_config("path/to/config.yml")

    # Get vessel data by IMO
    vessel = client.get_vessel("9321483")
    print(f"{vessel['name']}: {vessel['length']}m × {vessel['beam']}m")

    # Get RAOs for vessel
    raos = client.get_vessel_raos("VLCC", draft=20.0)

    # Stream to OrcaFlex vessel YAML
    yaml_output = client.to_orcaflex_vessel(vessel, raos)

Configuration:
    Uses YAML config from specs/modules/data-procurement/vessel-systems/configs/
    Supports primary API + fallbacks for resilience
"""

import logging
from typing import Dict, Any, Iterator, Optional, List
from datetime import datetime
from pathlib import Path
import numpy as np

from ..common.config_loader import ConfigLoader
from ..common.stream_handler import StreamHandler
from .api_clients import MarineTrafficClient, ShipXplorerClient, GenericRAOClient

logger = logging.getLogger(__name__)


class VesselClient:
    """
    Unified vessel data procurement client.

    Automatically selects appropriate API/database based on configuration and data type.
    Falls back to alternative providers if primary fails.

    Architecture:
    - Vessel-based queries (IMO/MMSI/name → vessel data)
    - Streaming processing (zero storage footprint)
    - Direct consumption (OrcaFlex vessel YAML, AQWA)
    """

    def __init__(self, config: Dict[str, Any]):
        """
        Initialize vessel client from configuration.

        Args:
            config: Configuration dictionary (typically loaded from YAML)
        """
        self.config = config

        # Initialize API clients based on configuration
        self.clients: Dict[str, Any] = {}
        self._initialize_clients()

        # Initialize stream handler for processing
        self.stream_handler = StreamHandler()

        logger.info("Initialized VesselClient")

    @classmethod
    def from_config(cls, config_path: str) -> 'VesselClient':
        """
        Create client from YAML configuration file.

        Args:
            config_path: Path to YAML config file

        Returns:
            Initialized VesselClient

        Example:
            client = VesselClient.from_config(
                "specs/modules/data-procurement/vessel-systems/configs/example_config.yml"
            )
        """
        config_loader = ConfigLoader(config_path)

        if not config_loader.validate_schema():
            raise ValueError(f"Invalid configuration: {config_path}")

        return cls(config_loader.to_dict())

    def _initialize_clients(self) -> None:
        """Initialize API clients based on configuration."""
        apis = self.config.get('apis', {})

        # MarineTraffic
        if 'MarineTraffic' in apis and apis['MarineTraffic'].get('enabled', False):
            api_config = apis['MarineTraffic']
            auth_config = api_config.get('authentication', {})

            if auth_config.get('key'):
                self.clients['MarineTraffic'] = MarineTrafficClient(
                    api_key=auth_config['key']
                )
                logger.info("Initialized MarineTraffic client")

        # ShipXplorer
        if 'ShipXplorer' in apis and apis['ShipXplorer'].get('enabled', False):
            api_config = apis['ShipXplorer']
            auth_config = api_config.get('authentication', {})

            self.clients['ShipXplorer'] = ShipXplorerClient(
                api_key=auth_config.get('key')
            )
            logger.info("Initialized ShipXplorer client")

        # Generic RAO Database
        if 'GenericRAO' in apis and apis['GenericRAO'].get('enabled', True):
            rao_config = apis['GenericRAO']
            rao_path = rao_config.get('path')

            self.clients['GenericRAO'] = GenericRAOClient(
                rao_database_path=rao_path
            )
            logger.info("Initialized GenericRAO client")

    def get_vessel(self, identifier: str,
                  identifier_type: str = 'auto') -> Dict[str, Any]:
        """
        Get vessel data by identifier with automatic provider selection.

        Args:
            identifier: Vessel identifier (IMO, MMSI, or name)
            identifier_type: Type of identifier:
                - 'auto': Automatically detect (default)
                - 'imo': IMO number
                - 'mmsi': MMSI number
                - 'name': Vessel name

        Returns:
            Vessel data dict

        Example:
            vessel = client.get_vessel("9321483")  # Auto-detects IMO
            vessel = client.get_vessel("Front Altair", identifier_type='name')
        """
        # Auto-detect identifier type
        if identifier_type == 'auto':
            identifier_type = self._detect_identifier_type(identifier)

        # Try providers in sequence
        providers = ['MarineTraffic', 'ShipXplorer']
        last_error = None

        for provider_name in providers:
            if provider_name not in self.clients:
                continue

            try:
                client = self.clients[provider_name]

                if identifier_type == 'imo':
                    return client.get_vessel_by_imo(identifier)
                elif identifier_type == 'mmsi':
                    return client.get_vessel_by_mmsi(identifier)
                elif identifier_type == 'name':
                    results = list(client.search_vessels_by_name(identifier))
                    if results:
                        return results[0]  # Return first match
                    raise ValueError(f"No vessel found for name: {identifier}")

            except Exception as e:
                last_error = e
                logger.warning(f"{provider_name} failed: {e}")
                continue

        # All providers failed
        raise RuntimeError(f"Failed to retrieve vessel data. Last error: {last_error}")

    def get_vessel_raos(self, vessel_type: str = None,
                       vessel_imo: str = None,
                       draft: float = None,
                       wave_directions: List[float] = None,
                       frequencies: np.ndarray = None) -> Dict[str, Any]:
        """
        Get vessel RAOs (Response Amplitude Operators).

        Args:
            vessel_type: Generic vessel type (e.g., "VLCC", "FPSO")
            vessel_imo: Specific vessel IMO (for custom RAOs if available)
            draft: Operating draft in meters
            wave_directions: Wave directions in degrees
            frequencies: Wave frequencies in rad/s

        Returns:
            RAO data dict

        Example:
            # Get RAOs for generic VLCC
            raos = client.get_vessel_raos(vessel_type="VLCC", draft=20.0)

            # Get RAOs for specific vessel (if custom RAOs available)
            raos = client.get_vessel_raos(vessel_imo="9321483", draft=20.0)
        """
        if 'GenericRAO' not in self.clients:
            raise RuntimeError("Generic RAO client not initialized")

        rao_client = self.clients['GenericRAO']

        # If specific vessel requested, try to get vessel type
        if vessel_imo and not vessel_type:
            try:
                vessel = self.get_vessel(vessel_imo)
                vessel_type = self._map_vessel_type(vessel.get('type'))
            except Exception as e:
                logger.warning(f"Could not get vessel type for IMO {vessel_imo}: {e}")
                vessel_type = "VLCC"  # Default fallback

        # Get RAOs
        return rao_client.get_raos(
            vessel_type=vessel_type,
            draft=draft,
            wave_directions=wave_directions,
            frequencies=frequencies
        )

    def calculate_hydrostatics(self, vessel: Dict[str, Any],
                              draft: float = None) -> Dict[str, Any]:
        """
        Calculate hydrostatic properties for vessel.

        Args:
            vessel: Vessel data dict (from get_vessel)
            draft: Operating draft (uses vessel draft if not specified)

        Returns:
            Hydrostatic properties dict:
                - displacement: tonnes
                - block_coefficient: Cb
                - waterplane_coefficient: Cwp
                - gm_metacentric_height: m
                - kb_center_buoyancy: m from keel
                - kg_center_gravity: m from keel
                - radii_of_gyration: dict (roll, pitch, yaw)

        Example:
            vessel = client.get_vessel("9321483")
            hydro = client.calculate_hydrostatics(vessel, draft=20.0)
            print(f"GM: {hydro['gm_metacentric_height']:.2f} m")
        """
        draft = draft or vessel.get('draft', 20.0)
        loa = vessel['length']
        beam = vessel['beam']
        depth = vessel.get('depth', draft * 1.5)  # Estimate if not available
        dwt = vessel.get('dwt', 0)

        # Calculate displacement (simplified)
        # Displacement ≈ DWT / 0.85 (assuming 85% cargo capacity)
        displacement = dwt / 0.85 if dwt > 0 else loa * beam * draft * 0.85 * 1.025

        # Block coefficient
        submerged_volume = displacement / 1.025  # m³ (seawater density)
        cb = submerged_volume / (loa * beam * draft) if loa * beam * draft > 0 else 0.8

        # Waterplane coefficient (typical relationship to Cb)
        cwp = cb + 0.06

        # Vertical center of buoyancy
        kb = draft * (0.5 - cb / 6)

        # Vertical center of gravity (estimated)
        kg = draft + (depth - draft) * 0.3  # Typical for cargo vessels

        # Metacentric height
        i_t = (loa * beam**3) / 12  # Transverse moment of inertia (simplified)
        bm = i_t / submerged_volume  # Metacentric radius
        gm = kb + bm - kg

        # Radii of gyration (as fraction of dimensions)
        kxx = 0.38 * beam  # Roll
        kyy = 0.25 * loa   # Pitch
        kzz = 0.25 * loa   # Yaw

        return {
            'displacement': displacement / 1000,  # tonnes
            'block_coefficient': cb,
            'waterplane_coefficient': cwp,
            'midship_coefficient': cb / cwp if cwp > 0 else 0.98,
            'gm_metacentric_height': gm,
            'kb_center_buoyancy': kb,
            'kg_center_gravity': kg,
            'bm_metacentric_radius': bm,
            'radii_of_gyration': {
                'roll': kxx,
                'pitch': kyy,
                'yaw': kzz
            }
        }

    def calculate_natural_periods(self, vessel: Dict[str, Any],
                                  hydrostatics: Dict[str, Any]) -> Dict[str, float]:
        """
        Calculate natural periods for vessel.

        Args:
            vessel: Vessel data dict
            hydrostatics: Hydrostatic properties (from calculate_hydrostatics)

        Returns:
            Natural periods dict (seconds):
                - roll: Roll period
                - pitch: Pitch period
                - heave: Heave period

        Example:
            periods = client.calculate_natural_periods(vessel, hydro)
            print(f"Roll period: {periods['roll']:.1f} s")
        """
        gm = hydrostatics['gm_metacentric_height']
        kxx = hydrostatics['radii_of_gyration']['roll']
        kyy = hydrostatics['radii_of_gyration']['pitch']
        draft = vessel.get('draft', 20.0)

        # Natural periods (simplified formulas)
        # Roll: T = 2π * sqrt(Kxx / (g * GM))
        t_roll = 2 * np.pi * np.sqrt(kxx / (9.81 * gm)) if gm > 0 else 12.0

        # Pitch: T = 2π * sqrt(Kyy / (g * 0.25 * LOA))
        t_pitch = 2 * np.pi * np.sqrt(kyy / (9.81 * vessel['length'] * 0.25))

        # Heave: T = 2π * sqrt(draft / (g * 0.5))
        t_heave = 2 * np.pi * np.sqrt(draft / (9.81 * 0.5))

        return {
            'roll': t_roll,
            'pitch': t_pitch,
            'heave': t_heave
        }

    def to_orcaflex_vessel(self, vessel: Dict[str, Any],
                          raos: Dict[str, Any] = None,
                          hydrostatics: Dict[str, Any] = None,
                          draft: float = None) -> str:
        """
        Convert vessel data to OrcaFlex vessel YAML (in-memory).

        Args:
            vessel: Vessel data dict
            raos: RAO data (optional)
            hydrostatics: Hydrostatic properties (optional, calculated if not provided)
            draft: Operating draft

        Returns:
            OrcaFlex vessel YAML string (in-memory, not saved)

        Example:
            vessel = client.get_vessel("9321483")
            raos = client.get_vessel_raos("VLCC", draft=20.0)
            yaml = client.to_orcaflex_vessel(vessel, raos)
            # Pass directly to OrcaFlex API (no file I/O)
        """
        import yaml

        draft = draft or vessel.get('draft', 20.0)

        # Calculate hydrostatics if not provided
        if hydrostatics is None:
            hydrostatics = self.calculate_hydrostatics(vessel, draft)

        # Calculate natural periods
        periods = self.calculate_natural_periods(vessel, hydrostatics)

        # Build OrcaFlex vessel definition
        orcaflex_vessel = {
            'Vessel': {
                'Name': vessel.get('name', 'Vessel'),
                'Type': 'Vessel',
                'Length': vessel['length'],
                'Breadth': vessel['beam'],
                'Draft': draft,
                'Displacement': hydrostatics['displacement'],
                'DistributedMass': 'Auto',
                'Xcg': vessel['length'] / 2,  # Midship
                'Ycg': 0.0,
                'Zcg': hydrostatics['kg_center_gravity'],
                'RadiusOfGyrationAboutX': hydrostatics['radii_of_gyration']['roll'],
                'RadiusOfGyrationAboutY': hydrostatics['radii_of_gyration']['pitch'],
                'RadiusOfGyrationAboutZ': hydrostatics['radii_of_gyration']['yaw'],
                'GM': hydrostatics['gm_metacentric_height'],
                'NaturalRollPeriod': periods['roll'],
                'NaturalPitchPeriod': periods['pitch'],
                'NaturalHeavePeriod': periods['heave'],
            }
        }

        # Add RAOs if provided
        if raos:
            orcaflex_vessel['VesselRAOs'] = {
                'Direction': 'TowardsVessel',
                'RAOType': 'Frequency',
                'RAOData': []
            }

            # Convert RAOs to OrcaFlex format
            for record in self.clients['GenericRAO'].stream_raos_orcaflex(raos):
                orcaflex_vessel['VesselRAOs']['RAOData'].append({
                    'Motion': record['motion'].capitalize(),
                    'WaveDirection': record['wave_direction'],
                    'Frequency': record['frequency'],
                    'Amplitude': float(record['amplitude']),
                    'Phase': float(record['phase'])
                })

        # Convert to YAML
        return yaml.dump(orcaflex_vessel, default_flow_style=False, sort_keys=False)

    def _detect_identifier_type(self, identifier: str) -> str:
        """
        Automatically detect identifier type.

        Args:
            identifier: Vessel identifier

        Returns:
            Identifier type ('imo', 'mmsi', or 'name')
        """
        # IMO: 7 digits starting with specific prefixes
        if identifier.isdigit() and len(identifier) == 7:
            return 'imo'

        # MMSI: 9 digits
        if identifier.isdigit() and len(identifier) == 9:
            return 'mmsi'

        # Otherwise assume name
        return 'name'

    def _map_vessel_type(self, vessel_type_str: str) -> str:
        """
        Map API vessel type to generic RAO database type.

        Args:
            vessel_type_str: Vessel type from API (e.g., "Crude Oil Tanker")

        Returns:
            Generic vessel type (e.g., "VLCC")
        """
        vessel_type_str = vessel_type_str.upper() if vessel_type_str else ""

        # Simple mapping (can be extended)
        if 'VLCC' in vessel_type_str or 'VERY LARGE' in vessel_type_str:
            return 'VLCC'
        elif 'SUEZMAX' in vessel_type_str:
            return 'Suezmax'
        elif 'AFRAMAX' in vessel_type_str:
            return 'Aframax'
        elif 'FPSO' in vessel_type_str or 'PRODUCTION' in vessel_type_str:
            return 'FPSO'
        elif 'SEMI' in vessel_type_str or 'SUBMERSIBLE' in vessel_type_str:
            return 'Semi-sub'
        elif 'DRILL' in vessel_type_str:
            return 'Drillship'
        elif 'LNG' in vessel_type_str:
            return 'LNG'
        elif 'TANKER' in vessel_type_str or 'CRUDE' in vessel_type_str:
            return 'VLCC'  # Default tanker to VLCC
        else:
            return 'VLCC'  # Default fallback
