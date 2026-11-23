"""
Natural Period Calculator for Mooring Analysis

This module calculates the natural periods of a moored vessel system
considering the mooring stiffness matrix and vessel mass properties.
"""

import numpy as np
from typing import Dict, Optional, Tuple, List
from dataclasses import dataclass
from loguru import logger


@dataclass
class VesselMassProperties:
    """Vessel mass and inertia properties."""
    mass: float  # tonnes
    inertia_xx: float  # tonne.m^2 (roll)
    inertia_yy: float  # tonne.m^2 (pitch)
    inertia_zz: float  # tonne.m^2 (yaw)
    cog_x: float = 0.0  # m
    cog_y: float = 0.0  # m
    cog_z: float = 0.0  # m
    added_mass_factor: float = 1.1  # Default 10% added mass


@dataclass
class NaturalPeriods:
    """Natural periods for all 6 DOF."""
    surge: float  # seconds
    sway: float  # seconds
    heave: float  # seconds
    roll: float  # seconds
    pitch: float  # seconds
    yaw: float  # seconds
    
    def to_dict(self) -> Dict[str, float]:
        """Convert to dictionary format."""
        return {
            'surge': self.surge,
            'sway': self.sway,
            'heave': self.heave,
            'roll': self.roll,
            'pitch': self.pitch,
            'yaw': self.yaw
        }
    
    def validate_ranges(self, vessel_type: str = 'FPSO') -> Dict[str, bool]:
        """
        Validate natural periods against typical ranges.
        
        Args:
            vessel_type: Type of vessel ('FPSO', 'LNGC', 'FSO')
            
        Returns:
            Dictionary of validation results for each DOF
        """
        # Typical ranges for different vessel types (seconds)
        typical_ranges = {
            'FPSO': {
                'surge': (100, 300),
                'sway': (100, 300),
                'heave': (8, 20),
                'roll': (10, 30),
                'pitch': (8, 20),
                'yaw': (50, 150)
            },
            'LNGC': {
                'surge': (80, 250),
                'sway': (80, 250),
                'heave': (6, 15),
                'roll': (8, 25),
                'pitch': (6, 15),
                'yaw': (40, 120)
            },
            'FSO': {
                'surge': (120, 350),
                'sway': (120, 350),
                'heave': (10, 25),
                'roll': (12, 35),
                'pitch': (10, 25),
                'yaw': (60, 180)
            }
        }
        
        ranges = typical_ranges.get(vessel_type, typical_ranges['FPSO'])
        validation = {}
        
        for dof, (min_val, max_val) in ranges.items():
            period = getattr(self, dof)
            validation[dof] = min_val <= period <= max_val
            
        return validation


class NaturalPeriodAnalyzer:
    """
    Analyzer for calculating natural periods of moored vessel systems.
    
    This class calculates the natural periods considering:
    - Mooring stiffness matrix (6x6)
    - Vessel mass and inertia properties
    - Added mass effects
    - Cross-coupling terms
    """
    
    def __init__(self):
        """Initialize the natural period analyzer."""
        self.vessel_properties: Optional[VesselMassProperties] = None
        self.stiffness_matrix: Optional[np.ndarray] = None
        self.mass_matrix: Optional[np.ndarray] = None
        
    def set_vessel_properties(self, properties: VesselMassProperties) -> None:
        """
        Set vessel mass and inertia properties.
        
        Args:
            properties: Vessel mass properties
        """
        self.vessel_properties = properties
        self._build_mass_matrix()
        logger.info(f"Set vessel properties: mass={properties.mass:.1f} tonnes")
        
    def set_stiffness_matrix(self, stiffness: np.ndarray) -> None:
        """
        Set the mooring stiffness matrix.
        
        Args:
            stiffness: 6x6 stiffness matrix
        """
        if stiffness.shape != (6, 6):
            raise ValueError(f"Stiffness matrix must be 6x6, got {stiffness.shape}")
        
        self.stiffness_matrix = stiffness
        logger.info("Set 6x6 stiffness matrix")
        
    def _build_mass_matrix(self) -> None:
        """Build the 6x6 mass matrix including added mass effects."""
        if not self.vessel_properties:
            raise ValueError("Vessel properties not set")
        
        props = self.vessel_properties
        
        # Initialize diagonal mass matrix
        self.mass_matrix = np.zeros((6, 6))
        
        # Translational masses (including added mass)
        mass_with_added = props.mass * props.added_mass_factor * 1000  # Convert to kg
        self.mass_matrix[0, 0] = mass_with_added  # Surge
        self.mass_matrix[1, 1] = mass_with_added  # Sway
        self.mass_matrix[2, 2] = mass_with_added  # Heave
        
        # Rotational inertias (including added mass effects)
        self.mass_matrix[3, 3] = props.inertia_xx * props.added_mass_factor * 1000  # Roll
        self.mass_matrix[4, 4] = props.inertia_yy * props.added_mass_factor * 1000  # Pitch
        self.mass_matrix[5, 5] = props.inertia_zz * props.added_mass_factor * 1000  # Yaw
        
    def calculate_natural_periods(self, 
                                 include_coupling: bool = True,
                                 frequency_range: Optional[Tuple[float, float]] = None) -> NaturalPeriods:
        """
        Calculate natural periods for all 6 DOF.
        
        Args:
            include_coupling: Include cross-coupling terms in calculation
            frequency_range: Optional frequency range for filtering (Hz)
            
        Returns:
            Natural periods for all DOF
        """
        if self.stiffness_matrix is None:
            raise ValueError("Stiffness matrix not set")
        if self.mass_matrix is None:
            raise ValueError("Mass matrix not set")
        
        periods = {}
        
        if include_coupling:
            # Full eigenvalue analysis for coupled system
            periods = self._calculate_coupled_periods(frequency_range)
        else:
            # Simple uncoupled calculation
            periods = self._calculate_uncoupled_periods()
        
        return NaturalPeriods(**periods)
    
    def _calculate_uncoupled_periods(self) -> Dict[str, float]:
        """Calculate natural periods assuming no coupling between DOF."""
        periods = {}
        dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        
        for i, dof in enumerate(dof_names):
            k = self.stiffness_matrix[i, i]  # Diagonal stiffness
            m = self.mass_matrix[i, i]  # Diagonal mass/inertia
            
            if k > 0 and m > 0:
                omega = np.sqrt(k / m)  # Natural frequency (rad/s)
                period = 2 * np.pi / omega  # Natural period (seconds)
                periods[dof] = period
            else:
                periods[dof] = float('inf')  # No restoring force
                
        return periods
    
    def _calculate_coupled_periods(self, 
                                  frequency_range: Optional[Tuple[float, float]] = None) -> Dict[str, float]:
        """
        Calculate natural periods considering coupling between DOF.
        
        Uses eigenvalue analysis of the coupled mass-stiffness system.
        """
        # Solve generalized eigenvalue problem: K*x = omega^2*M*x
        try:
            eigenvalues, eigenvectors = np.linalg.eig(
                np.linalg.inv(self.mass_matrix) @ self.stiffness_matrix
            )
            
            # Convert eigenvalues to natural frequencies
            natural_frequencies = np.sqrt(np.abs(eigenvalues))  # rad/s
            
            # Apply frequency range filter if specified
            if frequency_range:
                min_freq, max_freq = frequency_range
                min_omega = 2 * np.pi * min_freq
                max_omega = 2 * np.pi * max_freq
                mask = (natural_frequencies >= min_omega) & (natural_frequencies <= max_omega)
                natural_frequencies = natural_frequencies[mask]
                eigenvectors = eigenvectors[:, mask]
            
            # Sort by frequency
            idx = np.argsort(natural_frequencies)
            natural_frequencies = natural_frequencies[idx]
            eigenvectors = eigenvectors[:, idx]
            
            # Map modes to DOF based on eigenvector dominance
            periods = self._map_modes_to_dof(natural_frequencies, eigenvectors)
            
        except np.linalg.LinAlgError:
            logger.warning("Eigenvalue calculation failed, using uncoupled approximation")
            periods = self._calculate_uncoupled_periods()
            
        return periods
    
    def _map_modes_to_dof(self, 
                         frequencies: np.ndarray, 
                         eigenvectors: np.ndarray) -> Dict[str, float]:
        """
        Map vibration modes to DOF based on eigenvector analysis.
        
        Args:
            frequencies: Natural frequencies (rad/s)
            eigenvectors: Mode shapes
            
        Returns:
            Dictionary mapping DOF names to periods
        """
        dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        periods = {}
        assigned_modes = set()
        
        for i, dof in enumerate(dof_names):
            # Find the mode with maximum participation for this DOF
            max_participation = 0
            selected_mode = None
            
            for j in range(min(len(frequencies), 6)):
                if j not in assigned_modes:
                    participation = abs(eigenvectors[i, j])
                    if participation > max_participation:
                        max_participation = participation
                        selected_mode = j
            
            if selected_mode is not None and frequencies[selected_mode] > 0:
                period = 2 * np.pi / frequencies[selected_mode]
                periods[dof] = period
                assigned_modes.add(selected_mode)
            else:
                periods[dof] = float('inf')
                
        return periods
    
    def calculate_frequency_response(self, 
                                    frequency_range: Tuple[float, float] = (0.01, 2.0),
                                    num_points: int = 100) -> Tuple[np.ndarray, Dict[str, np.ndarray]]:
        """
        Calculate frequency response for all DOF.
        
        Args:
            frequency_range: Frequency range in Hz (min, max)
            num_points: Number of frequency points
            
        Returns:
            Tuple of (frequencies, response_amplitudes)
        """
        if self.stiffness_matrix is None or self.mass_matrix is None:
            raise ValueError("Stiffness and mass matrices must be set")
        
        frequencies = np.linspace(frequency_range[0], frequency_range[1], num_points)
        omega = 2 * np.pi * frequencies
        
        response = {
            'surge': np.zeros(num_points),
            'sway': np.zeros(num_points),
            'heave': np.zeros(num_points),
            'roll': np.zeros(num_points),
            'pitch': np.zeros(num_points),
            'yaw': np.zeros(num_points)
        }
        
        dof_names = list(response.keys())
        
        for i, w in enumerate(omega):
            # Dynamic stiffness matrix: K - omega^2*M
            dynamic_stiffness = self.stiffness_matrix - w**2 * self.mass_matrix
            
            try:
                # Calculate response (simplified - unit force excitation)
                inv_dynamic = np.linalg.inv(dynamic_stiffness)
                
                for j, dof in enumerate(dof_names):
                    response[dof][i] = abs(inv_dynamic[j, j])
                    
            except np.linalg.LinAlgError:
                # Singular matrix at resonance
                for dof in dof_names:
                    response[dof][i] = 1e6  # Large value to indicate resonance
                    
        return frequencies, response


def calculate_natural_periods_from_stiffness(
    stiffness_matrix: np.ndarray,
    vessel_mass: float,
    vessel_type: str = 'FPSO',
    added_mass_factor: float = 1.1
) -> NaturalPeriods:
    """
    Convenience function to calculate natural periods from stiffness matrix.
    
    Args:
        stiffness_matrix: 6x6 mooring stiffness matrix
        vessel_mass: Vessel mass in tonnes
        vessel_type: Type of vessel for inertia estimation
        added_mass_factor: Factor for added mass effects
        
    Returns:
        Natural periods for all DOF
    """
    # Estimate inertias based on vessel type and mass
    inertia_factors = {
        'FPSO': {'xx': 0.3, 'yy': 0.25, 'zz': 0.25},
        'LNGC': {'xx': 0.25, 'yy': 0.2, 'zz': 0.2},
        'FSO': {'xx': 0.35, 'yy': 0.3, 'zz': 0.3}
    }
    
    factors = inertia_factors.get(vessel_type, inertia_factors['FPSO'])
    
    # Estimate vessel dimensions (simplified)
    vessel_length = (vessel_mass / 50) ** (1/3) * 10  # Rough estimate
    
    properties = VesselMassProperties(
        mass=vessel_mass,
        inertia_xx=vessel_mass * (vessel_length * factors['xx']) ** 2,
        inertia_yy=vessel_mass * (vessel_length * factors['yy']) ** 2,
        inertia_zz=vessel_mass * (vessel_length * factors['zz']) ** 2,
        added_mass_factor=added_mass_factor
    )
    
    analyzer = NaturalPeriodAnalyzer()
    analyzer.set_vessel_properties(properties)
    analyzer.set_stiffness_matrix(stiffness_matrix)
    
    return analyzer.calculate_natural_periods()