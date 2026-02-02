"""
Configuration management for mooring tension iteration system.

This module handles YAML configuration parsing, validation, and data models
for the mooring tension iteration process.
"""

import os
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
import yaml
from pathlib import Path
import pandas as pd
from loguru import logger


@dataclass
class ConvergenceSettings:
    """Convergence criteria and algorithm parameters."""
    tolerance: float = 1.0  # Percentage tolerance
    max_iterations: int = 10
    damping_factor: float = 0.8  # For stability
    min_tolerance: float = 0.1  # Minimum absolute tolerance in kN
    
    def validate(self) -> None:
        """Validate convergence settings."""
        if self.tolerance <= 0:
            raise ValueError("Tolerance must be positive")
        if self.max_iterations < 1:
            raise ValueError("Max iterations must be at least 1")
        if not 0 < self.damping_factor <= 1:
            raise ValueError("Damping factor must be between 0 and 1")


@dataclass
class JacobianSettings:
    """Settings for Jacobian matrix calculation."""
    method: str = "finite_difference"  # or "analytical"
    perturbation_factor: float = 0.01  # 1% of line length
    min_perturbation: float = 0.1  # meters
    parallel_evaluation: bool = True
    
    def validate(self) -> None:
        """Validate Jacobian settings."""
        if self.method not in ["finite_difference", "analytical"]:
            raise ValueError(f"Invalid Jacobian method: {self.method}")
        if self.perturbation_factor <= 0:
            raise ValueError("Perturbation factor must be positive")
        if self.min_perturbation <= 0:
            raise ValueError("Minimum perturbation must be positive")


@dataclass
class LineProperties:
    """Properties for a single mooring line."""
    name: str
    EA: float  # Axial stiffness in kN
    sections: List[Optional[float]]  # Section lengths, None for variable
    target_tension: float  # Target tension in kN
    min_length: Optional[float] = None  # Minimum allowable length
    max_length: Optional[float] = None  # Maximum allowable length
    
    def validate(self) -> None:
        """Validate line properties."""
        if self.EA <= 0:
            raise ValueError(f"EA must be positive for line {self.name}")
        if self.target_tension <= 0:
            raise ValueError(f"Target tension must be positive for line {self.name}")
        if self.min_length and self.max_length:
            if self.min_length >= self.max_length:
                raise ValueError(f"Min length must be less than max length for line {self.name}")


@dataclass
class VesselConfig:
    """Vessel fixing configuration for static analysis."""
    fix_vessels: bool = True
    vessels_to_fix: List[str] = field(default_factory=list)
    fix_degrees_of_freedom: List[str] = field(default_factory=lambda: ["x", "y", "z", "rx", "ry", "rz"])
    
    def validate(self) -> None:
        """Validate vessel configuration."""
        valid_dofs = ["x", "y", "z", "rx", "ry", "rz"]
        for dof in self.fix_degrees_of_freedom:
            if dof not in valid_dofs:
                raise ValueError(f"Invalid degree of freedom: {dof}")


@dataclass
class OutputConfig:
    """Output configuration for results and reporting."""
    save_iterations: bool = True
    iteration_folder: str = "iterations"
    final_report: str = "tension_iteration_report.pdf"
    convergence_plot: str = "convergence_history.png"
    export_csv: bool = True
    export_excel: bool = False
    
    def create_output_directories(self, base_path: Path) -> None:
        """Create necessary output directories."""
        if self.save_iterations:
            iteration_path = base_path / self.iteration_folder
            iteration_path.mkdir(parents=True, exist_ok=True)


class IterationConfig:
    """
    Main configuration class for mooring tension iteration.
    
    Handles loading and validation of YAML configuration files.
    """
    
    def __init__(self, config_path: Optional[str] = None, config_dict: Optional[Dict] = None):
        """
        Initialize configuration from file or dictionary.
        
        Args:
            config_path: Path to YAML configuration file
            config_dict: Configuration dictionary (for testing)
        """
        self.config_path = config_path
        self.raw_config = {}
        
        # Initialize settings with defaults
        self.convergence = ConvergenceSettings()
        self.jacobian = JacobianSettings()
        self.vessel_config = VesselConfig()
        self.output_config = OutputConfig()
        self.lines: List[LineProperties] = []
        self.method = "scipy"  # Default optimization method
        
        if config_path:
            self.load_from_file(config_path)
        elif config_dict:
            self.load_from_dict(config_dict)
    
    def load_from_file(self, config_path: str) -> None:
        """Load configuration from YAML file."""
        logger.info(f"Loading configuration from {config_path}")
        
        if not os.path.exists(config_path):
            raise FileNotFoundError(f"Configuration file not found: {config_path}")
        
        with open(config_path, 'r') as f:
            self.raw_config = yaml.safe_load(f)
        
        self._parse_config()
        self._validate()
    
    def load_from_dict(self, config_dict: Dict) -> None:
        """Load configuration from dictionary."""
        self.raw_config = config_dict
        self._parse_config()
        self._validate()
    
    def load_target_tensions_from_csv(self, csv_path: str) -> Dict[str, float]:
        """
        Load target tensions from CSV file.
        
        Expected CSV format:
        ObjectName,target_tension
        Line1,1500.0
        Line2,1500.0
        """
        logger.info(f"Loading target tensions from {csv_path}")
        
        if not os.path.exists(csv_path):
            raise FileNotFoundError(f"CSV file not found: {csv_path}")
        
        df = pd.read_csv(csv_path)
        
        if 'ObjectName' not in df.columns or 'target_tension' not in df.columns:
            raise ValueError("CSV must have 'ObjectName' and 'target_tension' columns")
        
        return dict(zip(df['ObjectName'], df['target_tension']))
    
    def _parse_config(self) -> None:
        """Parse raw configuration into structured objects."""
        # Get the mooring tension iteration section
        if 'orcaflex_analysis' in self.raw_config:
            mti_config = self.raw_config['orcaflex_analysis'].get('mooring_tension_iteration', {})
        else:
            mti_config = self.raw_config.get('mooring_tension_iteration', {})
        
        # Parse method
        self.method = mti_config.get('method', 'scipy')
        
        # Parse convergence settings
        if 'convergence' in mti_config:
            conv = mti_config['convergence']
            self.convergence = ConvergenceSettings(
                tolerance=conv.get('tolerance', 1.0),
                max_iterations=conv.get('max_iterations', 10),
                damping_factor=conv.get('damping_factor', 0.8),
                min_tolerance=conv.get('min_tolerance', 0.1)
            )
        
        # Parse Jacobian settings
        if 'jacobian' in mti_config:
            jac = mti_config['jacobian']
            self.jacobian = JacobianSettings(
                method=jac.get('method', 'finite_difference'),
                perturbation_factor=jac.get('perturbation_factor', 0.01),
                min_perturbation=jac.get('min_perturbation', 0.1),
                parallel_evaluation=jac.get('parallel_evaluation', True)
            )
        
        # Parse vessel configuration
        if 'groups' in mti_config and mti_config['groups']:
            group = mti_config['groups'][0]  # Use first group
            if 'vessel_config' in group:
                vc = group['vessel_config']
                self.vessel_config = VesselConfig(
                    fix_vessels=vc.get('fix_vessels', True),
                    vessels_to_fix=vc.get('vessels_to_fix', []),
                    fix_degrees_of_freedom=vc.get('fix_degrees_of_freedom', ["x", "y", "z", "rx", "ry", "rz"])
                )
            
            # Parse line properties and target tensions
            self._parse_lines(group)
        
        # Parse output configuration
        if 'output' in mti_config:
            out = mti_config['output']
            self.output_config = OutputConfig(
                save_iterations=out.get('save_iterations', True),
                iteration_folder=out.get('iteration_folder', 'iterations'),
                final_report=out.get('final_report', 'tension_iteration_report.pdf'),
                convergence_plot=out.get('convergence_plot', 'convergence_history.png'),
                export_csv=out.get('export_csv', True),
                export_excel=out.get('export_excel', False)
            )
    
    def _parse_lines(self, group: Dict) -> None:
        """Parse line properties and target tensions."""
        self.lines = []
        
        # Get target tensions
        target_tensions = {}
        if 'target_pretension' in group:
            tp = group['target_pretension']
            if tp.get('type') == 'csv':
                target_tensions = self.load_target_tensions_from_csv(tp['filename'])
            elif tp.get('type') == 'yaml' and 'tensions' in tp:
                target_tensions = tp['tensions']
        
        # Get line properties
        line_props = group.get('line_properties', {})
        
        # Create LineProperties objects
        for line_name, tension in target_tensions.items():
            props = line_props.get(line_name, {})
            line = LineProperties(
                name=line_name,
                EA=props.get('EA', 850000),  # Default EA
                sections=props.get('sections', [None]),
                target_tension=tension,
                min_length=props.get('min_length'),
                max_length=props.get('max_length')
            )
            self.lines.append(line)
    
    def _validate(self) -> None:
        """Validate all configuration settings."""
        logger.info("Validating configuration")
        
        # Validate settings objects
        self.convergence.validate()
        self.jacobian.validate()
        self.vessel_config.validate()
        
        # Validate lines
        if not self.lines:
            raise ValueError("No mooring lines configured")
        
        for line in self.lines:
            line.validate()
        
        # Check for duplicate line names
        line_names = [line.name for line in self.lines]
        if len(line_names) != len(set(line_names)):
            raise ValueError("Duplicate line names found")
        
        logger.info(f"Configuration validated: {len(self.lines)} lines configured")
    
    def get_line_by_name(self, name: str) -> Optional[LineProperties]:
        """Get line properties by name."""
        for line in self.lines:
            if line.name == name:
                return line
        return None
    
    def get_target_tensions_dict(self) -> Dict[str, float]:
        """Get dictionary of line names to target tensions."""
        return {line.name: line.target_tension for line in self.lines}
    
    def get_line_ea_dict(self) -> Dict[str, float]:
        """Get dictionary of line names to EA values."""
        return {line.name: line.EA for line in self.lines}
    
    def summary(self) -> str:
        """Generate configuration summary string."""
        summary = [
            f"Mooring Tension Iteration Configuration",
            f"Method: {self.method}",
            f"Convergence Tolerance: {self.convergence.tolerance}%",
            f"Max Iterations: {self.convergence.max_iterations}",
            f"Number of Lines: {len(self.lines)}",
            f"Lines: {', '.join([line.name for line in self.lines])}"
        ]
        return "\n".join(summary)