"""
Loading Condition Decoder

Extracts loading conditions from OrcaFlex filenames and data.
Decodes: hwl/lwl, 125km3/180km3, pb/sb, environmental conditions.
"""

import re
import logging
from typing import Dict, List, Optional, Tuple, Union
from dataclasses import dataclass
from pathlib import Path
import pandas as pd
from enum import Enum

logger = logging.getLogger(__name__)


class WaterLevel(Enum):
    """Water level conditions"""
    HWL = "hwl"  # High Water Level
    LWL = "lwl"  # Low Water Level
    UNKNOWN = "unknown"


class VolumeCondition(Enum):
    """LNG volume conditions"""
    FULL_125 = "125km3"      # 125,000 m³ (partial load)
    FULL_180 = "180km3"      # 180,000 m³ (full load)
    BALLAST = "ballast"      # Ballast condition
    UNKNOWN = "unknown"


class SideConfiguration(Enum):
    """Side configuration"""
    PORT = "pb"              # Port side
    STARBOARD = "sb"         # Starboard side
    BOTH = "both"            # Both sides
    UNKNOWN = "unknown"


class LoadingPhase(Enum):
    """Loading operation phase"""
    CONNECTED = "connected"   # Vessel connected
    APPROACH = "approach"     # Approach phase
    DEPARTURE = "departure"   # Departure phase
    TRANSFER = "transfer"     # Transfer operation
    EMERGENCY = "emergency"   # Emergency disconnect
    UNKNOWN = "unknown"


@dataclass
class EnvironmentalConditions:
    """Environmental loading conditions"""
    wave_height: Optional[float] = None      # Significant wave height (m)
    wave_period: Optional[float] = None      # Wave period (s)
    wind_speed: Optional[float] = None       # Wind speed (m/s)
    current_speed: Optional[float] = None    # Current speed (m/s)
    wave_direction: Optional[float] = None   # Wave direction (degrees)
    wind_direction: Optional[float] = None   # Wind direction (degrees)
    current_direction: Optional[float] = None # Current direction (degrees)


@dataclass
class LoadingConditions:
    """Complete loading condition specification"""
    water_level: WaterLevel
    volume_condition: VolumeCondition
    side_config: SideConfiguration
    loading_phase: LoadingPhase
    environmental: Optional[EnvironmentalConditions] = None
    additional_info: Dict[str, any] = None
    confidence_score: float = 0.0
    source: str = "filename"  # filename, data, manual
    
    def __post_init__(self):
        if self.additional_info is None:
            self.additional_info = {}


class LoadingDecoder:
    """
    Decodes loading conditions from OrcaFlex analysis files.
    
    Extracts loading conditions from:
    - Filenames using pattern matching
    - Column headers and data values
    - File metadata and directory structure
    """
    
    # Filename patterns for loading conditions
    FILENAME_PATTERNS = {
        'water_level': {
            WaterLevel.HWL: [r'\bhwl\b', r'high.*water', r'hwl_', r'_hwl'],
            WaterLevel.LWL: [r'\blwl\b', r'low.*water', r'lwl_', r'_lwl']
        },
        'volume': {
            VolumeCondition.FULL_125: [
                r'125\s*k?m3', r'125000\s*m3', r'125\s*km3', r'125k'
            ],
            VolumeCondition.FULL_180: [
                r'180\s*k?m3', r'180000\s*m3', r'180\s*km3', r'180k'
            ],
            VolumeCondition.BALLAST: [
                r'\bballast\b', r'\bbal\b', r'empty', r'0\s*km3'
            ]
        },
        'side': {
            SideConfiguration.PORT: [r'\bpb\b', r'\bport\b', r'portside', r'_pb_', r'_pb$'],
            SideConfiguration.STARBOARD: [r'\bsb\b', r'\bstbd\b', r'starboard', r'_sb_', r'_sb$']
        },
        'phase': {
            LoadingPhase.CONNECTED: [r'connected', r'conn', r'loading', r'transfer'],
            LoadingPhase.APPROACH: [r'approach', r'appr', r'incoming'],
            LoadingPhase.DEPARTURE: [r'departure', r'dept', r'leaving'],
            LoadingPhase.EMERGENCY: [r'emergency', r'emerg', r'disconnect', r'esd']
        }
    }
    
    # Environmental condition patterns
    ENV_PATTERNS = {
        'wave_height': [
            r'hs?\s*(\d+(?:\.\d+)?)\s*m',  # Hs 2.5m
            r'h\s*(\d+(?:\.\d+)?)',        # H 2.5
            r'wave.*(\d+(?:\.\d+)?)m'      # wave 2.5m
        ],
        'wave_period': [
            r'tp?\s*(\d+(?:\.\d+)?)\s*s',  # Tp 8.0s
            r't\s*(\d+(?:\.\d+)?)',        # T 8.0
            r'period.*(\d+(?:\.\d+)?)s'    # period 8.0s
        ],
        'wind_speed': [
            r'w\s*(\d+(?:\.\d+)?)\s*m/?s', # W 15 m/s
            r'wind.*(\d+(?:\.\d+)?)m'      # wind 15m/s
        ],
        'current': [
            r'curr?.*(\d+(?:\.\d+)?)\s*m/?s',  # curr 1.5 m/s
            r'c\s*(\d+(?:\.\d+)?)'             # C 1.5
        ],
        'direction': [
            r'(\d+)\s*deg',                # 180 deg
            r'dir.*(\d+)',                 # dir 180
            r'(\d+)°'                      # 180°
        ]
    }
    
    def __init__(self):
        """Initialize loading decoder"""
        self.logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")
        self._compile_patterns()
    
    def _compile_patterns(self):
        """Compile regex patterns for performance"""
        self.compiled_patterns = {}
        
        for category, conditions in self.FILENAME_PATTERNS.items():
            self.compiled_patterns[category] = {}
            for condition, patterns in conditions.items():
                self.compiled_patterns[category][condition] = [
                    re.compile(pattern, re.IGNORECASE) for pattern in patterns
                ]
        
        self.compiled_env_patterns = {}
        for env_type, patterns in self.ENV_PATTERNS.items():
            self.compiled_env_patterns[env_type] = [
                re.compile(pattern, re.IGNORECASE) for pattern in patterns
            ]
    
    def decode_from_filename(self, file_path: Path) -> LoadingConditions:
        """
        Decode loading conditions from filename.
        
        Args:
            file_path: Path to the file
            
        Returns:
            LoadingConditions object with decoded information
        """
        filename = file_path.name.lower()
        directory = file_path.parent.name.lower()
        full_path = str(file_path).lower()
        
        # Initialize conditions
        water_level = WaterLevel.UNKNOWN
        volume_condition = VolumeCondition.UNKNOWN
        side_config = SideConfiguration.UNKNOWN
        loading_phase = LoadingPhase.UNKNOWN
        
        confidence_scores = []
        
        # Decode water level
        for level, patterns in self.compiled_patterns['water_level'].items():
            for pattern in patterns:
                if pattern.search(filename) or pattern.search(directory):
                    water_level = level
                    confidence_scores.append(0.9)
                    break
            if water_level != WaterLevel.UNKNOWN:
                break
        
        # Decode volume condition
        for volume, patterns in self.compiled_patterns['volume'].items():
            for pattern in patterns:
                if pattern.search(filename) or pattern.search(directory):
                    volume_condition = volume
                    confidence_scores.append(0.9)
                    break
            if volume_condition != VolumeCondition.UNKNOWN:
                break
        
        # Decode side configuration
        for side, patterns in self.compiled_patterns['side'].items():
            for pattern in patterns:
                if pattern.search(filename) or pattern.search(directory):
                    side_config = side
                    confidence_scores.append(0.8)
                    break
            if side_config != SideConfiguration.UNKNOWN:
                break
        
        # Decode loading phase
        for phase, patterns in self.compiled_patterns['phase'].items():
            for pattern in patterns:
                if pattern.search(filename) or pattern.search(directory):
                    loading_phase = phase
                    confidence_scores.append(0.7)
                    break
            if loading_phase != LoadingPhase.UNKNOWN:
                break
        
        # Extract environmental conditions
        environmental = self._extract_environmental_conditions(full_path)
        if any(getattr(environmental, attr) is not None 
               for attr in ['wave_height', 'wave_period', 'wind_speed', 'current_speed']):
            confidence_scores.append(0.8)
        
        # Calculate overall confidence
        overall_confidence = np.mean(confidence_scores) if confidence_scores else 0.0
        
        # Add heuristic adjustments
        if water_level != WaterLevel.UNKNOWN and volume_condition != VolumeCondition.UNKNOWN:
            overall_confidence += 0.1  # Bonus for multiple conditions found
        
        loading_conditions = LoadingConditions(
            water_level=water_level,
            volume_condition=volume_condition,
            side_config=side_config,
            loading_phase=loading_phase,
            environmental=environmental,
            confidence_score=min(1.0, overall_confidence),
            source="filename"
        )
        
        self.logger.debug(f"Decoded from filename {file_path.name}: "
                         f"{water_level.value}/{volume_condition.value}/{side_config.value} "
                         f"(confidence: {overall_confidence:.2f})")
        
        return loading_conditions
    
    def _extract_environmental_conditions(self, text: str) -> EnvironmentalConditions:
        """Extract environmental conditions from text"""
        env_conditions = EnvironmentalConditions()
        
        try:
            # Extract wave height
            for pattern in self.compiled_env_patterns['wave_height']:
                match = pattern.search(text)
                if match:
                    env_conditions.wave_height = float(match.group(1))
                    break
            
            # Extract wave period
            for pattern in self.compiled_env_patterns['wave_period']:
                match = pattern.search(text)
                if match:
                    env_conditions.wave_period = float(match.group(1))
                    break
            
            # Extract wind speed
            for pattern in self.compiled_env_patterns['wind_speed']:
                match = pattern.search(text)
                if match:
                    env_conditions.wind_speed = float(match.group(1))
                    break
            
            # Extract current speed
            for pattern in self.compiled_env_patterns['current']:
                match = pattern.search(text)
                if match:
                    env_conditions.current_speed = float(match.group(1))
                    break
            
            # Extract directions (simplified - would need more context)
            direction_matches = []
            for pattern in self.compiled_env_patterns['direction']:
                matches = pattern.findall(text)
                direction_matches.extend([float(m) for m in matches])
            
            # Assign directions heuristically (first found goes to wave)
            if len(direction_matches) > 0:
                env_conditions.wave_direction = direction_matches[0]
            if len(direction_matches) > 1:
                env_conditions.wind_direction = direction_matches[1]
            if len(direction_matches) > 2:
                env_conditions.current_direction = direction_matches[2]
        
        except (ValueError, AttributeError) as e:
            self.logger.debug(f"Error extracting environmental conditions: {e}")
        
        return env_conditions
    
    def decode_from_data(self, df: pd.DataFrame) -> LoadingConditions:
        """
        Decode loading conditions from DataFrame column names and values.
        
        Args:
            df: DataFrame to analyze
            
        Returns:
            LoadingConditions object with decoded information
        """
        # Initialize conditions
        water_level = WaterLevel.UNKNOWN
        volume_condition = VolumeCondition.UNKNOWN
        side_config = SideConfiguration.UNKNOWN
        loading_phase = LoadingPhase.CONNECTED  # Default assumption for data files
        
        column_text = ' '.join(df.columns).lower()
        confidence_scores = []
        
        # Check column names for loading conditions
        for level, patterns in self.compiled_patterns['water_level'].items():
            for pattern in patterns:
                if pattern.search(column_text):
                    water_level = level
                    confidence_scores.append(0.6)
                    break
            if water_level != WaterLevel.UNKNOWN:
                break
        
        for volume, patterns in self.compiled_patterns['volume'].items():
            for pattern in patterns:
                if pattern.search(column_text):
                    volume_condition = volume
                    confidence_scores.append(0.6)
                    break
            if volume_condition != VolumeCondition.UNKNOWN:
                break
        
        for side, patterns in self.compiled_patterns['side'].items():
            for pattern in patterns:
                if pattern.search(column_text):
                    side_config = side
                    confidence_scores.append(0.5)
                    break
            if side_config != SideConfiguration.UNKNOWN:
                break
        
        # Try to infer from data ranges (heuristic)
        env_conditions = self._infer_environmental_from_data(df)
        if any(getattr(env_conditions, attr) is not None 
               for attr in ['wave_height', 'wave_period', 'wind_speed']):
            confidence_scores.append(0.4)
        
        overall_confidence = np.mean(confidence_scores) if confidence_scores else 0.0
        
        loading_conditions = LoadingConditions(
            water_level=water_level,
            volume_condition=volume_condition,
            side_config=side_config,
            loading_phase=loading_phase,
            environmental=env_conditions,
            confidence_score=overall_confidence,
            source="data"
        )
        
        return loading_conditions
    
    def _infer_environmental_from_data(self, df: pd.DataFrame) -> EnvironmentalConditions:
        """Infer environmental conditions from data patterns"""
        env_conditions = EnvironmentalConditions()
        
        try:
            # Look for wave-related columns
            wave_cols = [col for col in df.columns 
                        if any(wave_word in col.lower() 
                              for wave_word in ['wave', 'hs', 'tp', 'elevation'])]
            
            if wave_cols:
                # Try to extract typical wave parameters
                for col in wave_cols:
                    data = pd.to_numeric(df[col], errors='coerce').dropna()
                    if len(data) > 0:
                        data_range = data.max() - data.min()
                        data_mean = abs(data.mean())
                        
                        # Heuristic classification
                        if 'hs' in col.lower() or 'height' in col.lower():
                            if 0.1 <= data_mean <= 10:  # Reasonable wave height range
                                env_conditions.wave_height = round(data_mean, 1)
                        
                        elif 'tp' in col.lower() or 'period' in col.lower():
                            if 3 <= data_mean <= 20:  # Reasonable wave period range
                                env_conditions.wave_period = round(data_mean, 1)
            
            # Look for wind columns
            wind_cols = [col for col in df.columns if 'wind' in col.lower()]
            if wind_cols:
                for col in wind_cols:
                    data = pd.to_numeric(df[col], errors='coerce').dropna()
                    if len(data) > 0:
                        data_mean = abs(data.mean())
                        if 0 <= data_mean <= 50:  # Reasonable wind speed range
                            env_conditions.wind_speed = round(data_mean, 1)
                            break
            
            # Look for current columns
            current_cols = [col for col in df.columns 
                           if any(curr_word in col.lower() 
                                 for curr_word in ['current', 'curr', 'flow'])]
            if current_cols:
                for col in current_cols:
                    data = pd.to_numeric(df[col], errors='coerce').dropna()
                    if len(data) > 0:
                        data_mean = abs(data.mean())
                        if 0 <= data_mean <= 5:  # Reasonable current speed range
                            env_conditions.current_speed = round(data_mean, 2)
                            break
        
        except Exception as e:
            self.logger.debug(f"Error inferring environmental conditions: {e}")
        
        return env_conditions
    
    def decode_loading_conditions(self, file_path: Path, 
                                 df: Optional[pd.DataFrame] = None) -> LoadingConditions:
        """
        Decode loading conditions using all available sources.
        
        Args:
            file_path: Path to the file
            df: Optional DataFrame for data-based decoding
            
        Returns:
            LoadingConditions with best available information
        """
        # Start with filename decoding
        filename_conditions = self.decode_from_filename(file_path)
        
        # If DataFrame provided, try data-based decoding
        if df is not None:
            data_conditions = self.decode_from_data(df)
            
            # Combine results, preferring filename over data when available
            combined_conditions = self._combine_conditions(filename_conditions, data_conditions)
        else:
            combined_conditions = filename_conditions
        
        # Add additional metadata
        combined_conditions.additional_info = {
            'file_name': file_path.name,
            'file_size': file_path.stat().st_size if file_path.exists() else 0,
            'analysis_timestamp': file_path.stat().st_mtime if file_path.exists() else None
        }
        
        return combined_conditions
    
    def _combine_conditions(self, filename_cond: LoadingConditions, 
                          data_cond: LoadingConditions) -> LoadingConditions:
        """Combine loading conditions from multiple sources"""
        
        # Use filename conditions as base (usually more reliable)
        combined = LoadingConditions(
            water_level=filename_cond.water_level if filename_cond.water_level != WaterLevel.UNKNOWN 
                       else data_cond.water_level,
            volume_condition=filename_cond.volume_condition if filename_cond.volume_condition != VolumeCondition.UNKNOWN 
                           else data_cond.volume_condition,
            side_config=filename_cond.side_config if filename_cond.side_config != SideConfiguration.UNKNOWN 
                       else data_cond.side_config,
            loading_phase=filename_cond.loading_phase if filename_cond.loading_phase != LoadingPhase.UNKNOWN 
                         else data_cond.loading_phase,
            confidence_score=max(filename_cond.confidence_score, data_cond.confidence_score),
            source="combined"
        )
        
        # Combine environmental conditions
        if filename_cond.environmental and data_cond.environmental:
            combined.environmental = EnvironmentalConditions(
                wave_height=filename_cond.environmental.wave_height or data_cond.environmental.wave_height,
                wave_period=filename_cond.environmental.wave_period or data_cond.environmental.wave_period,
                wind_speed=filename_cond.environmental.wind_speed or data_cond.environmental.wind_speed,
                current_speed=filename_cond.environmental.current_speed or data_cond.environmental.current_speed,
                wave_direction=filename_cond.environmental.wave_direction or data_cond.environmental.wave_direction,
                wind_direction=filename_cond.environmental.wind_direction or data_cond.environmental.wind_direction,
                current_direction=filename_cond.environmental.current_direction or data_cond.environmental.current_direction
            )
        elif filename_cond.environmental:
            combined.environmental = filename_cond.environmental
        elif data_cond.environmental:
            combined.environmental = data_cond.environmental
        
        # Combine additional info
        combined.additional_info = {}
        if filename_cond.additional_info:
            combined.additional_info.update(filename_cond.additional_info)
        if data_cond.additional_info:
            combined.additional_info.update(data_cond.additional_info)
        
        return combined
    
    def validate_conditions(self, conditions: LoadingConditions) -> Dict[str, any]:
        """
        Validate decoded loading conditions for completeness and consistency.
        
        Args:
            conditions: LoadingConditions to validate
            
        Returns:
            Dictionary with validation results
        """
        validation = {
            'is_valid': True,
            'completeness_score': 0.0,
            'issues': [],
            'warnings': []
        }
        
        # Check completeness
        completeness_factors = []
        
        if conditions.water_level != WaterLevel.UNKNOWN:
            completeness_factors.append(1.0)
        else:
            validation['issues'].append("Water level not identified")
            completeness_factors.append(0.0)
        
        if conditions.volume_condition != VolumeCondition.UNKNOWN:
            completeness_factors.append(1.0)
        else:
            validation['warnings'].append("Volume condition not identified")
            completeness_factors.append(0.0)
        
        if conditions.side_config != SideConfiguration.UNKNOWN:
            completeness_factors.append(1.0)
        else:
            validation['warnings'].append("Side configuration not identified")
            completeness_factors.append(0.0)
        
        # Environmental conditions are optional but add to completeness
        if conditions.environmental:
            env_completeness = sum([
                1 if getattr(conditions.environmental, attr) is not None else 0
                for attr in ['wave_height', 'wave_period', 'wind_speed', 'current_speed']
            ]) / 4.0
            completeness_factors.append(env_completeness)
        
        validation['completeness_score'] = np.mean(completeness_factors)
        
        # Check for inconsistencies
        if conditions.volume_condition == VolumeCondition.BALLAST and conditions.loading_phase == LoadingPhase.TRANSFER:
            validation['issues'].append("Inconsistent: ballast condition with transfer phase")
            validation['is_valid'] = False
        
        # Check environmental consistency
        if conditions.environmental:
            env = conditions.environmental
            if env.wave_height and env.wave_height > 10:
                validation['warnings'].append(f"Very high wave height: {env.wave_height}m")
            
            if env.wind_speed and env.wind_speed > 25:
                validation['warnings'].append(f"Very high wind speed: {env.wind_speed}m/s")
        
        # Overall validity check
        if validation['completeness_score'] < 0.3:
            validation['is_valid'] = False
            validation['issues'].append("Insufficient loading condition information")
        
        return validation
    
    def get_condition_summary(self, conditions: LoadingConditions) -> str:
        """
        Generate human-readable summary of loading conditions.
        
        Args:
            conditions: LoadingConditions to summarize
            
        Returns:
            String summary
        """
        parts = []
        
        if conditions.water_level != WaterLevel.UNKNOWN:
            parts.append(conditions.water_level.value.upper())
        
        if conditions.volume_condition != VolumeCondition.UNKNOWN:
            parts.append(conditions.volume_condition.value)
        
        if conditions.side_config != SideConfiguration.UNKNOWN:
            parts.append(conditions.side_config.value.upper())
        
        if conditions.loading_phase != LoadingPhase.UNKNOWN:
            parts.append(conditions.loading_phase.value)
        
        base_summary = " / ".join(parts) if parts else "Unknown conditions"
        
        # Add environmental info if available
        if conditions.environmental:
            env_parts = []
            env = conditions.environmental
            
            if env.wave_height:
                env_parts.append(f"Hs={env.wave_height}m")
            if env.wave_period:
                env_parts.append(f"Tp={env.wave_period}s")
            if env.wind_speed:
                env_parts.append(f"Wind={env.wind_speed}m/s")
            if env.current_speed:
                env_parts.append(f"Current={env.current_speed}m/s")
            
            if env_parts:
                base_summary += f" ({', '.join(env_parts)})"
        
        return base_summary


# Utility functions
def quick_decode(file_path: Union[str, Path]) -> LoadingConditions:
    """Quick decode with default settings"""
    decoder = LoadingDecoder()
    return decoder.decode_loading_conditions(Path(file_path))


def batch_decode(file_paths: List[Union[str, Path]]) -> List[LoadingConditions]:
    """Decode multiple files in batch"""
    decoder = LoadingDecoder()
    return [decoder.decode_loading_conditions(Path(fp)) for fp in file_paths]


# Import numpy at module level to avoid issues
import numpy as np