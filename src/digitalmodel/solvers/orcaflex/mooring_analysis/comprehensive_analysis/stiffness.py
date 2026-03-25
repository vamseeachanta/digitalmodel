"""Stiffness analysis module for mooring systems."""

from pathlib import Path
from typing import Optional, Dict, List, Tuple
import pandas as pd
import numpy as np
import logging
from datetime import datetime

from .models import (
    StiffnessLineData,
    StiffnessData,
    StiffnessMatrix,
    StiffnessMetrics
)
from .exceptions import DataParsingError, StiffnessCalculationError

logger = logging.getLogger(__name__)


class StiffnessAnalyzer:
    """Analyzes stiffness characteristics of mooring systems."""
    
    def __init__(self, include_rotational: bool = True):
        """Initialize stiffness analyzer.
        
        Args:
            include_rotational: Include rotational DOF in analysis
        """
        self.include_rotational = include_rotational
    
    def parse_stiffness_csv(self, file: Path) -> StiffnessData:
        """Parse stiffness CSV file.
        
        Args:
            file: Path to CSV file
            
        Returns:
            Parsed stiffness data
            
        Raises:
            DataParsingError: If CSV cannot be parsed
        """
        try:
            df = pd.read_csv(file)
            
            # Validate required columns
            required_columns = [
                'ObjectName', 'k_x', 'k_y', 'k_z'
            ]
            
            missing_columns = set(required_columns) - set(df.columns)
            if missing_columns:
                raise DataParsingError(
                    str(file),
                    f"Missing required columns: {missing_columns}"
                )
            
            # Parse line data
            lines = []
            for _, row in df.iterrows():
                line_data = StiffnessLineData(
                    object_name=row['ObjectName'],
                    k_axial=float(row.get('k_axial', 0)),
                    k_x=float(row['k_x']),
                    k_y=float(row['k_y']),
                    k_z=float(row['k_z']),
                    k_xy=float(row.get('k_xy', 0)),
                    k_xz=float(row.get('k_xz', 0)),
                    k_yz=float(row.get('k_yz', 0)),
                    direction_cosines=(
                        float(row.get('cos_x', 0)),
                        float(row.get('cos_y', 0)),
                        float(row.get('cos_z', 0))
                    ),
                    forces=(
                        float(row.get('Fx', 0)),
                        float(row.get('Fy', 0)),
                        float(row.get('Fz', 0))
                    )
                )
                lines.append(line_data)
            
            return StiffnessData(
                filename=file,
                timestamp=datetime.now(),
                lines=lines,
                raw_dataframe=df
            )
            
        except Exception as e:
            raise DataParsingError(str(file), str(e))
    
    def compute_stiffness_matrix(self, data: StiffnessData) -> np.ndarray:
        """Compute 6-DOF stiffness matrix.
        
        Args:
            data: Stiffness data
            
        Returns:
            6x6 stiffness matrix
        """
        df = data.raw_dataframe
        
        # Initialize 6-DOF matrix
        k_matrix = np.zeros((6, 6))
        
        # Sum translational stiffnesses
        k_matrix[0, 0] = df['k_x'].sum() if 'k_x' in df else 0
        k_matrix[1, 1] = df['k_y'].sum() if 'k_y' in df else 0
        k_matrix[2, 2] = df['k_z'].sum() if 'k_z' in df else 0
        
        # Add cross-coupling terms if available
        if 'k_xy' in df:
            k_matrix[0, 1] = k_matrix[1, 0] = df['k_xy'].sum()
        if 'k_xz' in df:
            k_matrix[0, 2] = k_matrix[2, 0] = df['k_xz'].sum()
        if 'k_yz' in df:
            k_matrix[1, 2] = k_matrix[2, 1] = df['k_yz'].sum()
        
        # Add rotational stiffnesses if available and requested
        if self.include_rotational:
            if 'k_rx' in df:
                k_matrix[3, 3] = df['k_rx'].sum()
            if 'k_ry' in df:
                k_matrix[4, 4] = df['k_ry'].sum()
            if 'k_rz' in df:
                k_matrix[5, 5] = df['k_rz'].sum()
        
        return k_matrix
    
    def analyze_characteristics(self, matrix: np.ndarray) -> StiffnessMetrics:
        """Analyze stiffness characteristics.
        
        Args:
            matrix: Stiffness matrix
            
        Returns:
            Stiffness metrics
        """
        # Create StiffnessMatrix object
        stiffness_matrix = StiffnessMatrix(
            matrix=matrix,
            translational=matrix[:3, :3],
            rotational=matrix[3:, 3:] if matrix.shape[0] > 3 else None
        )
        
        # Compute eigenvalues and eigenvectors
        try:
            eigenvalues, eigenvectors = np.linalg.eig(matrix[:3, :3])
            stiffness_matrix.eigenvalues = eigenvalues
            stiffness_matrix.eigenvectors = eigenvectors
            stiffness_matrix.condition_number = np.linalg.cond(matrix)
        except np.linalg.LinAlgError as e:
            logger.warning(f"Failed to compute eigenvalues: {e}")
        
        # Find dominant stiffness direction
        if stiffness_matrix.eigenvectors is not None:
            max_idx = np.argmax(np.abs(stiffness_matrix.eigenvalues))
            dominant_direction = tuple(stiffness_matrix.eigenvectors[:, max_idx].real)
        else:
            dominant_direction = (1.0, 0.0, 0.0)
        
        # Calculate stiffness ratios
        k_x = matrix[0, 0]
        k_y = matrix[1, 1]
        k_z = matrix[2, 2]
        
        stiffness_ratios = {
            'x/y': k_x / k_y if k_y > 0 else np.inf,
            'x/z': k_x / k_z if k_z > 0 else np.inf,
            'y/z': k_y / k_z if k_z > 0 else np.inf
        }
        
        # Calculate anisotropy factor
        anisotropy_factor = max(stiffness_ratios.values()) / min(
            [v for v in stiffness_ratios.values() if v != np.inf]
        ) if any(v != np.inf for v in stiffness_ratios.values()) else 1.0
        
        return StiffnessMetrics(
            stiffness_matrix=stiffness_matrix,
            dominant_direction=dominant_direction,
            stiffness_ratios=stiffness_ratios,
            anisotropy_factor=anisotropy_factor,
            critical_lines=[],  # To be implemented
            natural_periods=None,  # To be calculated if vessel mass provided
            system_characteristics={
                'total_stiffness_x': k_x,
                'total_stiffness_y': k_y,
                'total_stiffness_z': k_z
            }
        )
    
    def estimate_natural_periods(self, matrix: np.ndarray, mass: float) -> Dict[str, float]:
        """Estimate natural periods of the system.
        
        Args:
            matrix: Stiffness matrix
            mass: Vessel mass in tonnes
            
        Returns:
            Natural periods in seconds for each DOF
        """
        periods = {}
        
        # Convert mass to kg
        mass_kg = mass * 1000
        
        # Calculate periods for translational DOF
        # T = 2π√(m/k)
        for i, dof in enumerate(['surge', 'sway', 'heave']):
            k = matrix[i, i]
            if k > 0:
                periods[dof] = 2 * np.pi * np.sqrt(mass_kg / (k * 1000))  # Convert kN/m to N/m
            else:
                periods[dof] = np.inf
        
        # Add rotational periods if available
        if matrix.shape[0] > 3:
            # Would need moments of inertia for proper calculation
            periods['roll'] = 0.0  # Placeholder
            periods['pitch'] = 0.0  # Placeholder
            periods['yaw'] = 0.0  # Placeholder
        
        return periods