"""
Parameter Override Service for OrcaFlex Browser Interface
Handles parameter validation, pattern generation, and configuration management
"""

import re
import json
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, asdict
from enum import Enum
from pathlib import Path
import logging

logger = logging.getLogger(__name__)


class VesselType(Enum):
    FSTS = "fsts"
    FLNG = "flng"
    LNGC = "lngc"


class LoadingCondition(Enum):
    L015 = "l015"  # 15% LNG
    L050 = "l050"  # 50% LNG
    L095 = "l095"  # 95% LNG


class TideLevel(Enum):
    HWL = "hwl"  # High Water Level
    MWL = "mwl"  # Mean Water Level
    LWL = "lwl"  # Low Water Level


class ReturnPeriod(Enum):
    YR0001 = "0001yr"  # 1 Year
    YR0010 = "0010yr"  # 10 Year
    YR0100 = "0100yr"  # 100 Year


@dataclass
class ParameterConfig:
    """Configuration for OrcaFlex analysis parameters"""
    vessel_type: str
    loading_condition: str
    tide_level: str
    return_period: str
    wave_direction: str
    analysis_type: str
    auto_max: bool = True
    custom_basename: Optional[str] = None
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization"""
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'ParameterConfig':
        """Create instance from dictionary"""
        return cls(**data)
    
    def validate(self) -> Tuple[bool, List[str]]:
        """
        Validate parameter configuration
        
        Returns:
            Tuple of (is_valid, list_of_errors)
        """
        errors = []
        
        # Validate vessel type
        if not self.vessel_type:
            errors.append("Vessel type is required")
        elif self.vessel_type not in [v.value for v in VesselType]:
            errors.append(f"Invalid vessel type: {self.vessel_type}")
        
        # Validate required fields when not in auto-max mode
        if not self.auto_max and not self.custom_basename:
            if not self.loading_condition:
                errors.append("Loading condition is required in manual mode")
            elif self.loading_condition not in [l.value for l in LoadingCondition]:
                errors.append(f"Invalid loading condition: {self.loading_condition}")
            
            if not self.tide_level:
                errors.append("Tide level is required in manual mode")
            elif self.tide_level not in [t.value for t in TideLevel]:
                errors.append(f"Invalid tide level: {self.tide_level}")
            
            if not self.return_period:
                errors.append("Return period is required in manual mode")
            elif self.return_period not in [r.value for r in ReturnPeriod]:
                errors.append(f"Invalid return period: {self.return_period}")
        
        # Validate wave direction format
        if self.wave_direction and not re.match(r'^\d{3}deg$', self.wave_direction):
            errors.append(f"Invalid wave direction format: {self.wave_direction}")
        
        # Validate analysis type format
        if self.analysis_type and not re.match(r'^\d{2}[a-z]$', self.analysis_type):
            errors.append(f"Invalid analysis type format: {self.analysis_type}")
        
        # Validate custom basename
        if self.custom_basename:
            if len(self.custom_basename) < 3:
                errors.append("Custom basename must be at least 3 characters")
            if not re.match(r'^[a-zA-Z0-9_\-]+$', self.custom_basename):
                errors.append("Custom basename can only contain letters, numbers, underscore and hyphen")
        
        return len(errors) == 0, errors


class ParameterService:
    """Service for managing parameter configurations and file patterns"""
    
    def __init__(self, base_path: Optional[Path] = None):
        """
        Initialize parameter service
        
        Args:
            base_path: Base directory path for OrcaFlex files
        """
        self.base_path = base_path or Path("D:/1522/ctr7/orcaflex/rev_a08")
        self.config_cache: Dict[str, ParameterConfig] = {}
        
    def generate_pattern(self, config: ParameterConfig) -> str:
        """
        Generate file pattern from configuration
        
        Args:
            config: Parameter configuration
            
        Returns:
            Generated file pattern string
        """
        if config.custom_basename:
            return config.custom_basename
        
        # Build pattern from parameters
        parts = []
        
        if config.vessel_type:
            parts.append(config.vessel_type)
        
        if config.analysis_type:
            parts.append(config.analysis_type)
        
        if config.return_period:
            parts.append(config.return_period)
        
        if config.loading_condition:
            parts.append(config.loading_condition)
        
        if config.tide_level:
            parts.append(config.tide_level)
        
        # Add wave direction if specified
        if config.wave_direction and config.wave_direction != "000deg":
            parts.append(config.wave_direction)
        
        return "_".join(filter(None, parts))
    
    def parse_filename_to_config(self, filename: str) -> Optional[ParameterConfig]:
        """
        Parse filename to extract configuration parameters
        
        Args:
            filename: File basename to parse
            
        Returns:
            ParameterConfig if successfully parsed, None otherwise
        """
        # Remove file extension
        basename = Path(filename).stem
        
        # Common patterns to match
        patterns = [
            # Full pattern: vessel_analysis_return_loading_tide_wave
            r'^(?P<vessel>fsts|flng|lngc)_(?P<analysis>\d{2}[a-z])_(?P<return>\d{4}yr)_(?P<loading>l\d{3})_(?P<tide>hwl|mwl|lwl)(?:_(?P<wave>\d{3}deg))?',
            # Simplified pattern: vessel_loading_tide
            r'^(?P<vessel>fsts|flng|lngc)_(?P<loading>l\d{3})_(?P<tide>hwl|mwl|lwl)',
            # Summary file pattern: dm_vessel_analysis_return_loading_tide
            r'^dm_(?P<vessel>fsts|flng|lngc)_(?P<analysis>\d{2}[a-z])_(?P<return>\d{4}yr)_(?P<loading>l\d{3})_(?P<tide>hwl|mwl|lwl)'
        ]
        
        for pattern in patterns:
            match = re.match(pattern, basename.lower())
            if match:
                groups = match.groupdict()
                
                config = ParameterConfig(
                    vessel_type=groups.get('vessel', 'fsts'),
                    loading_condition=groups.get('loading', 'l015'),
                    tide_level=groups.get('tide', 'hwl'),
                    return_period=groups.get('return', '0100yr'),
                    wave_direction=groups.get('wave', '000deg'),
                    analysis_type=groups.get('analysis', '03c'),
                    auto_max=False
                )
                
                is_valid, _ = config.validate()
                if is_valid:
                    return config
        
        return None
    
    def modify_pattern_for_component(self, base_pattern: str, component: str) -> str:
        """
        Modify base pattern to include component suffix
        
        Args:
            base_pattern: Base file pattern
            component: Component name (e.g., 'Strut1', 'Jacket2')
            
        Returns:
            Modified pattern with component
        """
        return f"{base_pattern}_{component}"
    
    def find_matching_files(self, pattern: str, folder: Path) -> List[Path]:
        """
        Find files matching the generated pattern
        
        Args:
            pattern: File pattern to match
            folder: Directory to search in
            
        Returns:
            List of matching file paths
        """
        matching_files = []
        
        if not folder.exists():
            logger.warning(f"Folder does not exist: {folder}")
            return matching_files
        
        # Create regex pattern for matching
        # Convert pattern to regex (handle wildcards)
        regex_pattern = pattern.replace('*', '.*')
        regex_pattern = f"^{regex_pattern}"
        
        # Search for matching files
        for file_path in folder.glob("*.csv"):
            if re.match(regex_pattern, file_path.stem, re.IGNORECASE):
                matching_files.append(file_path)
        
        return matching_files
    
    def save_configuration(self, config: ParameterConfig, name: str) -> bool:
        """
        Save configuration to cache/storage
        
        Args:
            config: Configuration to save
            name: Configuration name/identifier
            
        Returns:
            True if saved successfully
        """
        try:
            is_valid, errors = config.validate()
            if not is_valid:
                logger.error(f"Cannot save invalid configuration: {errors}")
                return False
            
            self.config_cache[name] = config
            
            # Also persist to file if needed
            config_file = self.base_path / f"config_{name}.json"
            with open(config_file, 'w') as f:
                json.dump(config.to_dict(), f, indent=2)
            
            logger.info(f"Configuration saved: {name}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to save configuration: {e}")
            return False
    
    def load_configuration(self, name: str) -> Optional[ParameterConfig]:
        """
        Load configuration from cache/storage
        
        Args:
            name: Configuration name/identifier
            
        Returns:
            ParameterConfig if found, None otherwise
        """
        # Check cache first
        if name in self.config_cache:
            return self.config_cache[name]
        
        # Try loading from file
        config_file = self.base_path / f"config_{name}.json"
        if config_file.exists():
            try:
                with open(config_file, 'r') as f:
                    data = json.load(f)
                config = ParameterConfig.from_dict(data)
                self.config_cache[name] = config
                return config
            except Exception as e:
                logger.error(f"Failed to load configuration: {e}")
        
        return None
    
    def get_available_options(self) -> Dict[str, List[Dict[str, str]]]:
        """
        Get all available parameter options
        
        Returns:
            Dictionary of parameter options
        """
        return {
            "vessel_types": [
                {"value": v.value, "label": v.name} for v in VesselType
            ],
            "loading_conditions": [
                {"value": l.value, "label": f"{l.name.replace('L', '')}% LNG"} 
                for l in LoadingCondition
            ],
            "tide_levels": [
                {"value": t.value, "label": t.name.replace('_', ' ').title()} 
                for t in TideLevel
            ],
            "return_periods": [
                {"value": r.value, "label": r.name.replace('YR', '').replace('_', ' ') + " Year"} 
                for r in ReturnPeriod
            ],
            "wave_directions": [
                {"value": f"{deg:03d}deg", "label": f"{deg}°"} 
                for deg in [0, 45, 90, 135, 180, 225, 270, 315]
            ],
            "analysis_types": [
                {"value": f"{i:02d}{c}", "label": f"Configuration {i}{c.upper()}"} 
                for i, c in [(3, 'c'), (4, 'a'), (5, 'b')]
            ]
        }


# Example usage
if __name__ == "__main__":
    # Configure logging
    logging.basicConfig(level=logging.INFO)
    
    # Create service instance
    service = ParameterService()
    
    # Create a configuration
    config = ParameterConfig(
        vessel_type="fsts",
        loading_condition="l015",
        tide_level="hwl",
        return_period="0100yr",
        wave_direction="000deg",
        analysis_type="03c",
        auto_max=False
    )
    
    # Validate configuration
    is_valid, errors = config.validate()
    print(f"Configuration valid: {is_valid}")
    if errors:
        print(f"Errors: {errors}")
    
    # Generate pattern
    pattern = service.generate_pattern(config)
    print(f"Generated pattern: {pattern}")
    
    # Save configuration
    service.save_configuration(config, "default")
    
    # Load configuration
    loaded_config = service.load_configuration("default")
    if loaded_config:
        print(f"Loaded configuration: {loaded_config.to_dict()}")
    
    # Get available options
    options = service.get_available_options()
    print(f"Available options: {json.dumps(options, indent=2)}")