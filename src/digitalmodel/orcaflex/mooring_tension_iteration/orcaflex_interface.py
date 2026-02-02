"""
OrcaFlex model interface with mock API support.

This module provides a wrapper around the OrcaFlex API with a mock implementation
for testing without requiring a commercial license.
"""

import os
import numpy as np
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
from loguru import logger
from abc import ABC, abstractmethod


@dataclass
class MooringLine:
    """Data class representing a mooring line."""
    name: str
    length: List[float]  # Section lengths
    target_segment_length: float = 2.0
    line_type: str = "Chain"
    end_a_connection: str = "Fixed"
    end_b_connection: str = "Anchored"
    
    @property
    def total_length(self) -> float:
        """Calculate total line length."""
        return sum(l for l in self.length if l is not None)


@dataclass
class StaticResult:
    """Data class for static analysis results."""
    converged: bool
    iterations: int
    tensions: Dict[str, float]  # Line name to tension
    positions: Dict[str, Tuple[float, float, float]]  # Line name to (x, y, z)
    
    
class OrcaFlexAPIBase(ABC):
    """Abstract base class for OrcaFlex API."""
    
    @abstractmethod
    def create_model(self) -> Any:
        """Create a new OrcaFlex model."""
        pass
    
    @abstractmethod
    def load_model(self, filepath: str) -> Any:
        """Load an OrcaFlex model from file."""
        pass
    
    @abstractmethod
    def save_model(self, filepath: str) -> None:
        """Save the current model to file."""
        pass
    
    @abstractmethod
    def get_line_names(self) -> List[str]:
        """Get list of all line names in the model."""
        pass
    
    @abstractmethod
    def get_line_properties(self, line_name: str) -> MooringLine:
        """Get properties of a specific line."""
        pass
    
    @abstractmethod
    def set_line_length(self, line_name: str, sections: List[float]) -> None:
        """Set the section lengths of a line."""
        pass
    
    @abstractmethod
    def fix_vessels(self, vessel_names: List[str], dofs: List[str]) -> None:
        """Fix specified vessels in given degrees of freedom."""
        pass
    
    @abstractmethod
    def run_static_analysis(self) -> StaticResult:
        """Run static analysis and return results."""
        pass
    
    @abstractmethod
    def get_line_tension(self, line_name: str) -> float:
        """Get effective tension for a line from last static analysis."""
        pass


class MockOrcaFlexAPI(OrcaFlexAPIBase):
    """
    Mock implementation of OrcaFlex API for testing.
    
    Simulates mooring line behavior using simplified catenary equations.
    """
    
    def __init__(self):
        """Initialize mock API."""
        self.model = None
        self.lines: Dict[str, MooringLine] = {}
        self.last_result: Optional[StaticResult] = None
        self.vessels_fixed = False
        self._iteration_count = 0
        
        # Mock parameters for tension calculation
        self.water_depth = 100.0  # meters
        self.line_weight = 1.0  # kN/m in water
        self.horizontal_span = 200.0  # meters
        
        logger.info("Initialized Mock OrcaFlex API for testing")
    
    def create_model(self) -> Dict:
        """Create a new mock model."""
        self.model = {"type": "mock", "lines": {}, "vessels": {}}
        self.lines = {}
        return self.model
    
    def load_model(self, filepath: str) -> Dict:
        """Load a mock model (creates test configuration)."""
        logger.info(f"Mock loading model from {filepath}")
        
        self.create_model()
        
        # Create default test lines
        for i in range(1, 5):
            line_name = f"Line{i}"
            self.lines[line_name] = MooringLine(
                name=line_name,
                length=[100.0, None, 100.0],  # Variable middle section
                target_segment_length=2.0
            )
            self.model["lines"][line_name] = self.lines[line_name]
        
        return self.model
    
    def save_model(self, filepath: str) -> None:
        """Mock save operation."""
        logger.info(f"Mock saving model to {filepath}")
    
    def get_line_names(self) -> List[str]:
        """Get list of all line names."""
        return list(self.lines.keys())
    
    def get_line_properties(self, line_name: str) -> MooringLine:
        """Get properties of a specific line."""
        if line_name not in self.lines:
            raise ValueError(f"Line {line_name} not found in model")
        return self.lines[line_name]
    
    def set_line_length(self, line_name: str, sections: List[float]) -> None:
        """Set the section lengths of a line."""
        if line_name not in self.lines:
            raise ValueError(f"Line {line_name} not found in model")
        
        self.lines[line_name].length = sections
        logger.debug(f"Set {line_name} lengths to {sections}")
    
    def fix_vessels(self, vessel_names: List[str], dofs: List[str]) -> None:
        """Mock vessel fixing."""
        self.vessels_fixed = True
        logger.info(f"Fixed vessels {vessel_names} in DOFs {dofs}")
    
    def run_static_analysis(self) -> StaticResult:
        """
        Run mock static analysis.
        
        Uses simplified catenary equations to estimate tensions.
        """
        logger.debug("Running mock static analysis")
        
        tensions = {}
        positions = {}
        
        for line_name, line in self.lines.items():
            # Calculate mock tension based on catenary approximation
            L = line.total_length
            h = self.horizontal_span
            d = self.water_depth
            w = self.line_weight
            
            # Simplified catenary tension calculation
            # T_h = horizontal tension component
            # T_v = vertical tension component (weight of suspended line)
            excess_length = L - np.sqrt(h**2 + d**2)
            sag = max(0.1, excess_length / 10)  # Simplified sag estimation
            
            T_h = w * h**2 / (8 * sag) if sag > 0 else 1000.0
            T_v = w * d
            T_effective = np.sqrt(T_h**2 + T_v**2)
            
            # Add some variation based on line number for testing
            line_number = int(line_name.replace("Line", ""))
            variation = 1.0 + 0.1 * np.sin(line_number + self._iteration_count * 0.5)
            T_effective *= variation
            
            tensions[line_name] = T_effective
            positions[line_name] = (h * line_number / 4, 0, -d)
        
        self._iteration_count += 1
        
        self.last_result = StaticResult(
            converged=True,
            iterations=5,  # Mock iteration count
            tensions=tensions,
            positions=positions
        )
        
        return self.last_result
    
    def get_line_tension(self, line_name: str) -> float:
        """Get effective tension for a line."""
        if not self.last_result:
            raise RuntimeError("No static analysis results available")
        
        if line_name not in self.last_result.tensions:
            raise ValueError(f"No tension data for line {line_name}")
        
        return self.last_result.tensions[line_name]


class OrcaFlexAPI(OrcaFlexAPIBase):
    """
    Real OrcaFlex API implementation.
    
    Requires OrcaFlex to be installed and licensed.
    """
    
    def __init__(self):
        """Initialize OrcaFlex API."""
        try:
            import OrcFxAPI
            self.OrcFxAPI = OrcFxAPI
            self.model = None
            logger.info("Initialized OrcaFlex API")
        except ImportError:
            logger.warning("OrcaFlex API not available, using mock implementation")
            raise ImportError("OrcaFlex API not available. Use MockOrcaFlexAPI for testing.")
    
    def create_model(self) -> Any:
        """Create a new OrcaFlex model."""
        self.model = self.OrcFxAPI.Model()
        return self.model
    
    def load_model(self, filepath: str) -> Any:
        """Load an OrcaFlex model from file."""
        if not os.path.exists(filepath):
            raise FileNotFoundError(f"Model file not found: {filepath}")
        
        self.model = self.OrcFxAPI.Model(filepath)
        logger.info(f"Loaded OrcaFlex model from {filepath}")
        return self.model
    
    def save_model(self, filepath: str) -> None:
        """Save the current model to file."""
        if not self.model:
            raise RuntimeError("No model loaded")
        
        self.model.SaveData(filepath)
        logger.info(f"Saved model to {filepath}")
    
    def get_line_names(self) -> List[str]:
        """Get list of all line names in the model."""
        if not self.model:
            raise RuntimeError("No model loaded")
        
        lines = []
        for obj in self.model.objects:
            if obj.type == self.OrcFxAPI.ObjectType.Line:
                lines.append(obj.name)
        
        return lines
    
    def get_line_properties(self, line_name: str) -> MooringLine:
        """Get properties of a specific line."""
        if not self.model:
            raise RuntimeError("No model loaded")
        
        line_obj = self.model[line_name]
        
        # Extract section lengths
        sections = []
        for i in range(line_obj.NumberOfSections):
            sections.append(line_obj.Length[i])
        
        return MooringLine(
            name=line_name,
            length=sections,
            target_segment_length=line_obj.TargetSegmentLength[0],
            line_type=line_obj.LineType[0],
            end_a_connection=line_obj.EndAConnection,
            end_b_connection=line_obj.EndBConnection
        )
    
    def set_line_length(self, line_name: str, sections: List[float]) -> None:
        """Set the section lengths of a line."""
        if not self.model:
            raise RuntimeError("No model loaded")
        
        line_obj = self.model[line_name]
        
        for i, length in enumerate(sections):
            if length is not None:
                line_obj.Length[i] = length
        
        logger.debug(f"Updated {line_name} section lengths")
    
    def fix_vessels(self, vessel_names: List[str], dofs: List[str]) -> None:
        """Fix specified vessels in given degrees of freedom."""
        if not self.model:
            raise RuntimeError("No model loaded")
        
        dof_mapping = {
            'x': 'FixedX',
            'y': 'FixedY', 
            'z': 'FixedZ',
            'rx': 'FixedRotationX',
            'ry': 'FixedRotationY',
            'rz': 'FixedRotationZ'
        }
        
        for vessel_name in vessel_names:
            vessel = self.model[vessel_name]
            for dof in dofs:
                if dof in dof_mapping:
                    setattr(vessel, dof_mapping[dof], 'Yes')
        
        logger.info(f"Fixed vessels {vessel_names} in DOFs {dofs}")
    
    def run_static_analysis(self) -> StaticResult:
        """Run static analysis and return results."""
        if not self.model:
            raise RuntimeError("No model loaded")
        
        logger.info("Running OrcaFlex static analysis")
        self.model.CalculateStatics()
        
        # Extract results
        tensions = {}
        positions = {}
        
        for line_name in self.get_line_names():
            line = self.model[line_name]
            
            # Get effective tension at end A
            tensions[line_name] = line.StaticResult('Effective Tension', 
                                                   OrcFxAPI.oeEndA)
            
            # Get position of end A
            x = line.StaticResult('X', OrcFxAPI.oeEndA)
            y = line.StaticResult('Y', OrcFxAPI.oeEndA)
            z = line.StaticResult('Z', OrcFxAPI.oeEndA)
            positions[line_name] = (x, y, z)
        
        return StaticResult(
            converged=True,  # Check actual convergence status
            iterations=self.model.StaticsIterationCount,
            tensions=tensions,
            positions=positions
        )
    
    def get_line_tension(self, line_name: str) -> float:
        """Get effective tension for a line from last static analysis."""
        if not self.model:
            raise RuntimeError("No model loaded")
        
        line = self.model[line_name]
        return line.StaticResult('Effective Tension', self.OrcFxAPI.oeEndA)


class OrcaFlexModelInterface:
    """
    High-level interface for OrcaFlex model manipulation.
    
    Automatically selects between real and mock API based on availability.
    """
    
    def __init__(self, use_mock: bool = False):
        """
        Initialize the interface.
        
        Args:
            use_mock: Force use of mock API for testing
        """
        self.api: Optional[OrcaFlexAPIBase] = None
        self.use_mock = use_mock
        self._initialize_api()
    
    def _initialize_api(self) -> None:
        """Initialize the appropriate API implementation."""
        if self.use_mock:
            self.api = MockOrcaFlexAPI()
        else:
            try:
                self.api = OrcaFlexAPI()
            except ImportError:
                logger.warning("OrcaFlex not available, using mock API")
                self.api = MockOrcaFlexAPI()
                self.use_mock = True
    
    def load_model(self, filepath: str) -> None:
        """Load an OrcaFlex model."""
        self.api.load_model(filepath)
    
    def save_model(self, filepath: str) -> None:
        """Save the current model."""
        self.api.save_model(filepath)
    
    def get_mooring_lines(self) -> List[str]:
        """Get list of mooring line names."""
        return self.api.get_line_names()
    
    def get_line_lengths(self) -> Dict[str, List[float]]:
        """Get current lengths for all mooring lines."""
        lengths = {}
        for line_name in self.get_mooring_lines():
            line = self.api.get_line_properties(line_name)
            lengths[line_name] = line.length
        return lengths
    
    def set_line_lengths(self, lengths: Dict[str, List[float]]) -> None:
        """Set lengths for multiple lines."""
        for line_name, sections in lengths.items():
            self.api.set_line_length(line_name, sections)
    
    def fix_vessels(self, vessel_names: List[str], dofs: Optional[List[str]] = None) -> None:
        """Fix vessels for static analysis."""
        if dofs is None:
            dofs = ['x', 'y', 'z', 'rx', 'ry', 'rz']
        self.api.fix_vessels(vessel_names, dofs)
    
    def run_static_analysis(self) -> StaticResult:
        """Run static analysis."""
        return self.api.run_static_analysis()
    
    def get_tensions(self) -> Dict[str, float]:
        """Get current tensions for all mooring lines."""
        result = self.api.run_static_analysis()
        return result.tensions
    
    def validate_model(self) -> bool:
        """Validate that the model is ready for iteration."""
        try:
            lines = self.get_mooring_lines()
            if not lines:
                logger.error("No mooring lines found in model")
                return False
            
            # Try to run a static analysis
            result = self.run_static_analysis()
            if not result.converged:
                logger.warning("Initial static analysis did not converge")
            
            return True
            
        except Exception as e:
            logger.error(f"Model validation failed: {e}")
            return False