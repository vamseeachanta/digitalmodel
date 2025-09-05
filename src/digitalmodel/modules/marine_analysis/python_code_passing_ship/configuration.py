"""
Configuration system for passing ship force calculations.

Provides YAML-based configuration with Pydantic validation, unit conversion,
expression evaluation, and configuration merging capabilities.
"""

import yaml
import re
import ast
import operator
from pathlib import Path
from typing import Dict, Any, Optional, List, Union, Tuple
from pydantic import BaseModel, Field, field_validator, model_validator
import numpy as np


class VesselConfig(BaseModel):
    """Configuration for vessel dimensions and properties."""
    
    length: float = Field(..., gt=0, description="Vessel length [m]")
    beam: float = Field(..., gt=0, description="Vessel beam [m]")
    draft: float = Field(..., gt=0, description="Vessel draft [m]")
    block_coefficient: float = Field(..., gt=0, le=1, description="Block coefficient [-]")
    name: Optional[str] = Field(None, description="Vessel name")
    
    @property
    def midship_area(self) -> float:
        """Calculate midship sectional area."""
        return self.beam * self.draft * self.block_coefficient
    
    @property
    def displacement(self) -> float:
        """Calculate approximate displacement [tonnes]."""
        # Simplified calculation - actual would need more detail
        volume = self.length * self.beam * self.draft * self.block_coefficient
        return volume * 1.025  # Assuming seawater density
    
    model_config = {"validate_assignment": True}


class EnvironmentalConfig(BaseModel):
    """Configuration for environmental conditions."""
    
    water_depth: Optional[float] = Field(None, gt=0, description="Water depth [m], None for infinite")
    water_density: float = Field(1025.0, gt=0, description="Water density [kg/m³]")
    current_velocity: Optional[float] = Field(0.0, ge=0, description="Current velocity [m/s]")
    
    @property
    def is_infinite_depth(self) -> bool:
        """Check if infinite depth condition."""
        return self.water_depth is None
    
    @property
    def is_shallow_water(self) -> bool:
        """Check if shallow water condition (h/T < 1.5)."""
        # This would need vessel draft to determine properly
        if self.water_depth is None:
            return False
        # Simplified check - actual would need draft
        return self.water_depth < 20.0  # Arbitrary threshold
    
    model_config = {"validate_assignment": True}


class CalculationConfig(BaseModel):
    """Configuration for calculation parameters."""
    
    stagger_distance: Optional[float] = Field(None, description="Longitudinal stagger distance [m]")
    lateral_separation: Optional[float] = Field(None, description="Lateral separation [m]")
    passing_velocity: Optional[float] = Field(None, description="Passing vessel velocity [m/s]")
    num_points: int = Field(50, gt=10, le=500, description="Number of integration points")
    integration_tolerance: float = Field(1e-6, gt=0, le=1e-2, description="Integration tolerance")
    max_iterations: int = Field(1000, gt=100, le=10000, description="Maximum iterations")
    cache_size: int = Field(1000, gt=100, le=10000, description="Result cache size")
    depth_modes: int = Field(10, gt=1, le=50, description="Number of depth correction modes")
    
    @field_validator('lateral_separation')
    @classmethod
    def validate_separation(cls, v):
        """Ensure lateral separation is reasonable."""
        if v is not None and v < 10.0:
            raise ValueError("Lateral separation too small for safety")
        return v
    
    model_config = {"validate_assignment": True}


class PassingShipConfig(BaseModel):
    """Complete passing ship calculation configuration."""
    
    moored_vessel: VesselConfig
    passing_vessel: VesselConfig
    environment: EnvironmentalConfig
    calculation: CalculationConfig
    output_units: str = Field("SI", description="Output unit system (SI/Imperial)")
    description: Optional[str] = Field(None, description="Configuration description")
    
    @model_validator(mode='after')
    def validate_configuration(self):
        """Validate overall configuration consistency."""
        if self.environment and self.moored_vessel:
            env = self.environment
            vessel = self.moored_vessel
            
            # Check water depth vs draft
            if env.water_depth is not None and env.water_depth < vessel.draft:
                raise ValueError(f"Water depth ({env.water_depth}m) less than vessel draft ({vessel.draft}m)")
        
        return self
    
    model_config = {"validate_assignment": True}


class YAMLConfigParser:
    """Parser for YAML configuration files with expression evaluation."""
    
    def __init__(self):
        """Initialize parser with custom YAML tags."""
        self.variables = {}
        self._setup_yaml_constructors()
    
    def _setup_yaml_constructors(self):
        """Set up custom YAML tag constructors."""
        yaml.add_constructor('!eval', self._eval_constructor)
        yaml.add_constructor('!var', self._var_constructor)
    
    def _eval_constructor(self, loader, node):
        """Evaluate mathematical expressions in YAML."""
        expression = loader.construct_scalar(node)
        return self._safe_eval(expression)
    
    def _var_constructor(self, loader, node):
        """Variable reference constructor."""
        var_name = loader.construct_scalar(node)
        if var_name in self.variables:
            return self.variables[var_name]
        raise ValueError(f"Variable '{var_name}' not defined")
    
    def _safe_eval(self, expression: str) -> float:
        """Safely evaluate mathematical expressions."""
        # Replace variable references
        pattern = r'var:(\w+)'
        
        def replace_var(match):
            var_name = match.group(1)
            if var_name in self.variables:
                return str(self.variables[var_name])
            raise ValueError(f"Variable '{var_name}' not found")
        
        expression = re.sub(pattern, replace_var, expression)
        
        # Safe evaluation using ast
        try:
            node = ast.parse(expression, mode='eval')
            
            # Check for safe operations only
            for n in ast.walk(node):
                if isinstance(n, (ast.Call, ast.Import, ast.ImportFrom, 
                                 ast.Attribute, ast.Subscript)):
                    raise ValueError(f"Unsafe operation in expression: {expression}")
            
            # Evaluate with limited operators
            safe_operators = {
                ast.Add: operator.add,
                ast.Sub: operator.sub,
                ast.Mult: operator.mul,
                ast.Div: operator.truediv,
                ast.Pow: operator.pow,
                ast.USub: operator.neg,
            }
            
            def _eval_node(node):
                if isinstance(node, ast.Expression):
                    return _eval_node(node.body)
                elif isinstance(node, ast.Constant):
                    return node.value
                elif isinstance(node, ast.Num):  # For Python < 3.8
                    return node.n
                elif isinstance(node, ast.BinOp):
                    left = _eval_node(node.left)
                    right = _eval_node(node.right)
                    op = safe_operators.get(type(node.op))
                    if op:
                        return op(left, right)
                    raise ValueError(f"Unsupported operator: {type(node.op)}")
                elif isinstance(node, ast.UnaryOp):
                    operand = _eval_node(node.operand)
                    op = safe_operators.get(type(node.op))
                    if op:
                        return op(operand)
                    raise ValueError(f"Unsupported unary operator: {type(node.op)}")
                else:
                    raise ValueError(f"Unsupported node type: {type(node)}")
            
            return _eval_node(node)
            
        except Exception as e:
            raise ValueError(f"Error evaluating expression '{expression}': {e}")
    
    def parse_file(self, filepath: str) -> PassingShipConfig:
        """Parse YAML configuration file."""
        with open(filepath, 'r') as f:
            data = yaml.safe_load(f)
        
        # Extract variables if present
        if 'variables' in data:
            self.variables = data['variables']
            del data['variables']  # Remove from main config
        
        # Parse configuration
        return PassingShipConfig(**data)
    
    def parse_string(self, yaml_string: str) -> PassingShipConfig:
        """Parse YAML configuration from string."""
        data = yaml.safe_load(yaml_string)
        
        # Extract variables if present
        if 'variables' in data:
            self.variables = data['variables']
            del data['variables']
        
        return PassingShipConfig(**data)


class UnitConverter:
    """Unit conversion utilities for marine engineering calculations."""
    
    # Conversion factors to SI base units
    LENGTH_TO_SI = {
        'm': 1.0,
        'meter': 1.0,
        'metres': 1.0,
        'ft': 0.3048,
        'feet': 0.3048,
        'foot': 0.3048,
    }
    
    FORCE_TO_SI = {
        'N': 1.0,
        'newton': 1.0,
        'kN': 1000.0,
        'kilonewton': 1000.0,
        'MN': 1e6,
        'meganewton': 1e6,
        'lbf': 4.44822,
        'pound-force': 4.44822,
        'kip': 4448.22,
    }
    
    MOMENT_TO_SI = {
        'N.m': 1.0,
        'N*m': 1.0,
        'newton-meter': 1.0,
        'kN.m': 1000.0,
        'kN*m': 1000.0,
        'kilonewton-meter': 1000.0,
        'MN.m': 1e6,
        'MN*m': 1e6,
        'meganewton-meter': 1e6,
    }
    
    VELOCITY_TO_SI = {
        'm/s': 1.0,
        'meter/second': 1.0,
        'knots': 0.514444,
        'knot': 0.514444,
        'kt': 0.514444,
        'ft/s': 0.3048,
        'feet/second': 0.3048,
    }
    
    DENSITY_TO_SI = {
        'kg/m3': 1.0,
        'kg/m^3': 1.0,
        'sg': 1000.0,  # Specific gravity to kg/m³
        'specific_gravity': 1000.0,
    }
    
    def convert(self, value: float, from_unit: str, to_unit: str) -> float:
        """Convert value from one unit to another."""
        # Determine unit type
        unit_type = self._determine_unit_type(from_unit)
        
        if unit_type == 'length':
            return self._convert_length(value, from_unit, to_unit)
        elif unit_type == 'force':
            return self._convert_force(value, from_unit, to_unit)
        elif unit_type == 'moment':
            return self._convert_moment(value, from_unit, to_unit)
        elif unit_type == 'velocity':
            return self._convert_velocity(value, from_unit, to_unit)
        elif unit_type == 'density':
            return self._convert_density(value, from_unit, to_unit)
        else:
            # No conversion if units not recognized
            return value
    
    def _determine_unit_type(self, unit: str) -> str:
        """Determine the type of unit."""
        if unit in self.LENGTH_TO_SI:
            return 'length'
        elif unit in self.FORCE_TO_SI:
            return 'force'
        elif unit in self.MOMENT_TO_SI:
            return 'moment'
        elif unit in self.VELOCITY_TO_SI:
            return 'velocity'
        elif unit in self.DENSITY_TO_SI:
            return 'density'
        else:
            return 'unknown'
    
    def _convert_length(self, value: float, from_unit: str, to_unit: str) -> float:
        """Convert length units."""
        # Convert to SI
        si_value = value * self.LENGTH_TO_SI.get(from_unit, 1.0)
        # Convert from SI to target
        return si_value / self.LENGTH_TO_SI.get(to_unit, 1.0)
    
    def _convert_force(self, value: float, from_unit: str, to_unit: str) -> float:
        """Convert force units."""
        si_value = value * self.FORCE_TO_SI.get(from_unit, 1.0)
        return si_value / self.FORCE_TO_SI.get(to_unit, 1.0)
    
    def _convert_moment(self, value: float, from_unit: str, to_unit: str) -> float:
        """Convert moment units."""
        si_value = value * self.MOMENT_TO_SI.get(from_unit, 1.0)
        return si_value / self.MOMENT_TO_SI.get(to_unit, 1.0)
    
    def _convert_velocity(self, value: float, from_unit: str, to_unit: str) -> float:
        """Convert velocity units."""
        si_value = value * self.VELOCITY_TO_SI.get(from_unit, 1.0)
        return si_value / self.VELOCITY_TO_SI.get(to_unit, 1.0)
    
    def _convert_density(self, value: float, from_unit: str, to_unit: str) -> float:
        """Convert density units."""
        si_value = value * self.DENSITY_TO_SI.get(from_unit, 1.0)
        return si_value / self.DENSITY_TO_SI.get(to_unit, 1.0)
    
    def detect_units(self, data: Dict[str, Any]) -> str:
        """Detect unit system from configuration data."""
        # Look for unit indicators
        for key, value in data.items():
            if isinstance(value, str):
                if 'lbf' in value or 'ft' in value or 'kip' in value:
                    return 'Imperial'
                elif 'kN' in value or 'MN' in value or 'kg' in value:
                    return 'SI'
        
        # Default to SI if no clear indicators
        return 'SI'
    
    def convert_config(self, config: Dict[str, float], 
                      from_system: str, to_system: str) -> Dict[str, float]:
        """Convert configuration between unit systems."""
        if from_system == to_system:
            return config.copy()
        
        converted = {}
        
        # Define unit mappings for each parameter type
        conversions = {
            'length': ('m', 'ft'),
            'beam': ('m', 'ft'),
            'draft': ('m', 'ft'),
            'velocity': ('m/s', 'knots'),
            'force': ('N', 'lbf'),
            'moment': ('N.m', 'lbf.ft'),
        }
        
        for key, value in config.items():
            if key in conversions:
                if from_system == 'SI' and to_system == 'Imperial':
                    from_unit, to_unit = conversions[key]
                else:
                    to_unit, from_unit = conversions[key]
                
                converted[key] = self.convert(value, from_unit, to_unit)
            else:
                converted[key] = value
        
        return converted


class ConfigurationMerger:
    """Merge multiple configuration sources with override capabilities."""
    
    def merge(self, base: Dict[str, Any], override: Dict[str, Any]) -> Dict[str, Any]:
        """Merge two configuration dictionaries."""
        result = base.copy()
        
        for key, value in override.items():
            if key in result:
                if isinstance(result[key], dict) and isinstance(value, dict):
                    # Recursive merge for nested dicts
                    result[key] = self.merge(result[key], value)
                else:
                    # Override value
                    result[key] = value
            else:
                # Add new key
                result[key] = value
        
        return result
    
    def merge_multiple(self, configs: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Merge multiple configuration dictionaries in order."""
        if not configs:
            return {}
        
        result = configs[0].copy()
        for config in configs[1:]:
            result = self.merge(result, config)
        
        return result
    
    def apply_overrides(self, config: PassingShipConfig, 
                       overrides: Dict[str, Any]) -> PassingShipConfig:
        """Apply overrides to a Pydantic configuration object."""
        # Convert to dict, merge, and reconstruct
        config_dict = config.dict()
        merged_dict = self.merge(config_dict, overrides)
        return PassingShipConfig(**merged_dict)