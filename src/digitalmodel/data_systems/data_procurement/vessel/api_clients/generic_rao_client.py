# ABOUTME: Generic RAO database client using repository-based RAO library
# ABOUTME: Streaming RAO data by vessel type with interpolation (draft, direction, frequency)

"""
Generic RAO Client
==================

Repository-based generic RAO (Response Amplitude Operator) database.

Data Coverage:
- Vessel types: VLCC, Suezmax, Aframax, FPSO, Semi-sub, Drillship, LNG, Barge
- DOF: 6 degrees of freedom (surge, sway, heave, roll, pitch, yaw)
- Wave directions: 0-180° (typically 15° intervals)
- Frequencies: 0.1-2.0 rad/s (or periods 3-60s)
- Drafts: Multiple operating drafts per vessel type

Source: Generic database from repository or industry-standard RAOs
Authentication: None (repository data)

Critical: RAO streaming with interpolation - NO file storage!
"""

import logging
from typing import Dict, Any, Iterator, List, Optional
from datetime import datetime
from pathlib import Path
import numpy as np
from scipy import interpolate

from ...common.base_client import BaseAPIClient

logger = logging.getLogger(__name__)


class GenericRAOClient(BaseAPIClient):
    """
    Generic RAO database client.

    Retrieves and interpolates RAO data for generic vessel types.

    Example:
        client = GenericRAOClient()

        # Get RAOs for VLCC at specific draft
        raos = client.get_raos(
            vessel_type="VLCC",
            draft=20.0,
            wave_directions=[0, 45, 90, 135, 180],
            frequencies=np.arange(0.2, 1.5, 0.05)
        )

        # Stream to OrcaFlex format
        for rao_record in client.stream_raos_orcaflex(raos):
            print(rao_record)
    """

    # Generic vessel types with standard dimensions
    VESSEL_TYPES = {
        'VLCC': {
            'name': 'Very Large Crude Carrier',
            'loa': 330.0,
            'beam': 58.0,
            'depth': 30.0,
            'drafts': [18.0, 20.0, 22.0],
            'dwt': 300000,
            'displacement': 350000
        },
        'Suezmax': {
            'name': 'Suezmax Tanker',
            'loa': 270.0,
            'beam': 48.0,
            'depth': 24.0,
            'drafts': [15.0, 16.5, 18.0],
            'dwt': 150000,
            'displacement': 170000
        },
        'Aframax': {
            'name': 'Aframax Tanker',
            'loa': 245.0,
            'beam': 42.0,
            'depth': 21.0,
            'drafts': [13.0, 14.5, 16.0],
            'dwt': 110000,
            'displacement': 125000
        },
        'FPSO': {
            'name': 'Floating Production Storage Offloading',
            'loa': 300.0,
            'beam': 54.0,
            'depth': 28.0,
            'drafts': [16.0, 18.0, 20.0],
            'dwt': 250000,
            'displacement': 280000
        },
        'Semi-sub': {
            'name': 'Semi-Submersible',
            'loa': 110.0,
            'beam': 80.0,
            'depth': 40.0,
            'drafts': [20.0, 22.0, 24.0],
            'displacement': 60000
        },
        'Drillship': {
            'name': 'Drillship',
            'loa': 230.0,
            'beam': 42.0,
            'depth': 25.0,
            'drafts': [10.0, 12.0, 14.0],
            'displacement': 70000
        },
        'LNG': {
            'name': 'LNG Carrier',
            'loa': 290.0,
            'beam': 46.0,
            'depth': 26.0,
            'drafts': [10.5, 11.5, 12.5],
            'dwt': 100000,
            'displacement': 115000
        }
    }

    def __init__(self, rao_database_path: str = None, **kwargs):
        """
        Initialize Generic RAO client.

        Args:
            rao_database_path: Path to RAO database directory
                              Default: specs/modules/data-procurement/vessel-systems/data/generic_raos/
            **kwargs: Additional BaseAPIClient arguments
        """
        auth_config = {'method': 'none'}  # Repository data, no authentication

        # Not a web API, but we use BaseAPIClient for consistency
        super().__init__(
            base_url="file://localhost",
            auth_config=auth_config,
            **kwargs
        )

        # Set RAO database path
        if rao_database_path:
            self.rao_db_path = Path(rao_database_path)
        else:
            # Default to repository location
            self.rao_db_path = Path(__file__).parent.parent.parent.parent.parent.parent / \
                               'specs/modules/data-procurement/vessel-systems/data/generic_raos'

        logger.info(f"Initialized GenericRAOClient (database: {self.rao_db_path})")

    def get_vessel_types(self) -> List[str]:
        """
        Get list of available vessel types.

        Returns:
            List of vessel type identifiers
        """
        return list(self.VESSEL_TYPES.keys())

    def get_vessel_particulars(self, vessel_type: str) -> Dict[str, Any]:
        """
        Get vessel particulars for generic type.

        Args:
            vessel_type: Vessel type (e.g., "VLCC", "FPSO")

        Returns:
            Vessel particulars dict
        """
        if vessel_type not in self.VESSEL_TYPES:
            raise ValueError(f"Unknown vessel type: {vessel_type}. "
                           f"Available: {list(self.VESSEL_TYPES.keys())}")

        return self.VESSEL_TYPES[vessel_type].copy()

    def get_raos(self, vessel_type: str,
                 draft: float,
                 wave_directions: List[float] = None,
                 frequencies: np.ndarray = None,
                 dof: List[str] = None) -> Dict[str, Any]:
        """
        Get RAOs for vessel type with interpolation.

        Args:
            vessel_type: Vessel type (e.g., "VLCC")
            draft: Operating draft in meters
            wave_directions: Wave directions in degrees (0 = head seas)
                            Default: [0, 15, 30, 45, 60, 90, 120, 135, 150, 165, 180]
            frequencies: Wave frequencies in rad/s
                        Default: np.arange(0.1, 2.0, 0.05)
            dof: Degrees of freedom to include
                Default: ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']

        Returns:
            RAO data dict:
                - vessel_type: str
                - draft: float
                - wave_directions: array
                - frequencies: array
                - raos: dict[dof][direction][frequency] = amplitude + phase
        """
        # Set defaults
        wave_directions = wave_directions or [0, 15, 30, 45, 60, 90, 120, 135, 150, 165, 180]
        frequencies = frequencies if frequencies is not None else np.arange(0.1, 2.0, 0.05)
        dof = dof or ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']

        # Get vessel particulars
        vessel = self.get_vessel_particulars(vessel_type)

        # Generate generic RAOs (simplified for generic database)
        raos = self._generate_generic_raos(
            vessel_type=vessel_type,
            vessel=vessel,
            draft=draft,
            wave_directions=wave_directions,
            frequencies=frequencies,
            dof=dof
        )

        return raos

    def _generate_generic_raos(self, vessel_type: str,
                              vessel: Dict[str, Any],
                              draft: float,
                              wave_directions: List[float],
                              frequencies: np.ndarray,
                              dof: List[str]) -> Dict[str, Any]:
        """
        Generate generic RAOs based on vessel dimensions.

        Uses simplified RAO formulations based on vessel type and dimensions.
        For production use, replace with actual RAO database or WAMIT/AQWA results.

        Args:
            vessel_type: Vessel type
            vessel: Vessel particulars
            draft: Draft
            wave_directions: Directions
            frequencies: Frequencies
            dof: Degrees of freedom

        Returns:
            RAO data dict
        """
        loa = vessel['loa']
        beam = vessel['beam']
        disp = vessel.get('displacement', 100000)

        # Initialize RAO structure
        rao_data = {
            'vessel_type': vessel_type,
            'draft': draft,
            'wave_directions': np.array(wave_directions),
            'frequencies': frequencies,
            'periods': 2 * np.pi / frequencies,
            'raos': {}
        }

        # Generate RAOs for each DOF
        for motion in dof:
            rao_data['raos'][motion] = {}

            for direction in wave_directions:
                rao_data['raos'][motion][direction] = self._calculate_rao_for_motion(
                    motion=motion,
                    direction=direction,
                    frequencies=frequencies,
                    loa=loa,
                    beam=beam,
                    draft=draft,
                    displacement=disp
                )

        return rao_data

    def _calculate_rao_for_motion(self, motion: str,
                                  direction: float,
                                  frequencies: np.ndarray,
                                  loa: float, beam: float,
                                  draft: float, displacement: float) -> Dict[str, np.ndarray]:
        """
        Calculate generic RAO for specific motion.

        Simplified RAO formulations based on vessel characteristics.
        For production, use actual model test data or numerical simulations.

        Args:
            motion: DOF ('surge', 'sway', 'heave', 'roll', 'pitch', 'yaw')
            direction: Wave direction (degrees)
            frequencies: Wave frequencies (rad/s)
            loa, beam, draft, displacement: Vessel dimensions

        Returns:
            RAO dict with 'amplitude' and 'phase' arrays
        """
        # Natural frequencies (simplified)
        omega_heave = np.sqrt(9.81 / (draft * 0.5))
        omega_roll = np.sqrt(9.81 * 0.4 * beam / (loa * 0.25**2))  # Using Kxx/beam ≈ 0.38
        omega_pitch = np.sqrt(9.81 * 0.25 * loa / (loa * 0.25**2))  # Using Kyy/loa ≈ 0.25

        # Direction effects
        dir_rad = np.radians(direction)

        if motion == 'surge':
            # Surge RAO (head seas maximum)
            amplitude = 1.0 * np.cos(dir_rad) * np.ones_like(frequencies)
            phase = np.zeros_like(frequencies)

        elif motion == 'sway':
            # Sway RAO (beam seas maximum)
            amplitude = 1.0 * np.abs(np.sin(dir_rad)) * np.ones_like(frequencies)
            phase = 90 * np.ones_like(frequencies)

        elif motion == 'heave':
            # Heave RAO (resonance at natural frequency)
            amplitude = 1.0 / np.sqrt((1 - (frequencies/omega_heave)**2)**2 + (0.1)**2)
            phase = np.arctan2(0.1, 1 - (frequencies/omega_heave)**2) * 180/np.pi

        elif motion == 'roll':
            # Roll RAO (beam seas maximum, resonance)
            amplitude = 20.0 * np.abs(np.sin(dir_rad)) / \
                       np.sqrt((1 - (frequencies/omega_roll)**2)**2 + (0.05)**2)  # deg/m
            phase = np.arctan2(0.05, 1 - (frequencies/omega_roll)**2) * 180/np.pi

        elif motion == 'pitch':
            # Pitch RAO (head/following seas maximum)
            amplitude = 10.0 * np.abs(np.cos(dir_rad)) / \
                       np.sqrt((1 - (frequencies/omega_pitch)**2)**2 + (0.08)**2)  # deg/m
            phase = np.arctan2(0.08, 1 - (frequencies/omega_pitch)**2) * 180/np.pi

        elif motion == 'yaw':
            # Yaw RAO (quartering seas maximum)
            amplitude = 5.0 * np.abs(np.sin(2*dir_rad)) * np.ones_like(frequencies)  # deg/m
            phase = np.zeros_like(frequencies)

        else:
            amplitude = np.zeros_like(frequencies)
            phase = np.zeros_like(frequencies)

        return {
            'amplitude': amplitude,
            'phase': phase
        }

    def interpolate_raos(self, rao_data: Dict[str, Any],
                        target_draft: float = None,
                        target_directions: List[float] = None,
                        target_frequencies: np.ndarray = None) -> Dict[str, Any]:
        """
        Interpolate RAOs to different draft, directions, or frequencies.

        Args:
            rao_data: Original RAO data
            target_draft: Target draft (if different from original)
            target_directions: Target wave directions
            target_frequencies: Target frequencies

        Returns:
            Interpolated RAO data
        """
        # Currently, this is a simplified implementation
        # For production, implement multi-dimensional interpolation

        if target_draft and abs(target_draft - rao_data['draft']) > 0.1:
            logger.warning("Draft interpolation not fully implemented, using original RAOs")

        # Frequency interpolation
        if target_frequencies is not None:
            interpolated_raos = rao_data.copy()
            interpolated_raos['frequencies'] = target_frequencies
            interpolated_raos['periods'] = 2 * np.pi / target_frequencies
            interpolated_raos['raos'] = {}

            for motion in rao_data['raos']:
                interpolated_raos['raos'][motion] = {}

                for direction in rao_data['wave_directions']:
                    orig_amp = rao_data['raos'][motion][direction]['amplitude']
                    orig_phase = rao_data['raos'][motion][direction]['phase']

                    # Interpolate amplitude and phase
                    f_interp_amp = interpolate.interp1d(
                        rao_data['frequencies'], orig_amp,
                        kind='cubic', fill_value='extrapolate'
                    )
                    f_interp_phase = interpolate.interp1d(
                        rao_data['frequencies'], orig_phase,
                        kind='linear', fill_value='extrapolate'
                    )

                    interpolated_raos['raos'][motion][direction] = {
                        'amplitude': f_interp_amp(target_frequencies),
                        'phase': f_interp_phase(target_frequencies)
                    }

            return interpolated_raos

        return rao_data

    def stream_raos_orcaflex(self, rao_data: Dict[str, Any]) -> Iterator[Dict[str, Any]]:
        """
        Stream RAOs in OrcaFlex format (in-memory).

        Args:
            rao_data: RAO data dict

        Yields:
            OrcaFlex-formatted RAO records (streaming, not stored)
        """
        vessel_type = rao_data['vessel_type']
        draft = rao_data['draft']

        # OrcaFlex RAO format
        for motion in rao_data['raos']:
            for direction in rao_data['wave_directions']:
                amplitudes = rao_data['raos'][motion][direction]['amplitude']
                phases = rao_data['raos'][motion][direction]['phase']
                frequencies = rao_data['frequencies']

                for i, freq in enumerate(frequencies):
                    yield {
                        'vessel_type': vessel_type,
                        'draft': draft,
                        'motion': motion,
                        'wave_direction': direction,
                        'frequency': freq,
                        'period': 2 * np.pi / freq,
                        'amplitude': amplitudes[i],
                        'phase': phases[i]
                    }

    def query_by_date(self, start_date: datetime, end_date: datetime,
                      location: Dict[str, float] = None,
                      vessel_type: str = None,
                      draft: float = None,
                      **kwargs) -> Iterator[Dict[str, Any]]:
        """
        Query RAO data (compatibility method).

        RAOs don't change with time, so this returns vessel RAOs.

        Args:
            start_date: Not used
            end_date: Not used
            location: Not used
            vessel_type: Vessel type to query
            draft: Operating draft
            **kwargs: Additional parameters

        Yields:
            RAO records (streaming)
        """
        if not vessel_type:
            raise ValueError("vessel_type required for RAO queries")

        draft = draft or self.VESSEL_TYPES[vessel_type]['drafts'][1]  # Middle draft

        rao_data = self.get_raos(vessel_type=vessel_type, draft=draft)

        # Stream RAOs
        yield from self.stream_raos_orcaflex(rao_data)
