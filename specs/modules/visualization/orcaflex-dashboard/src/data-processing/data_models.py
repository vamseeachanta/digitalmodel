"""
Data models for OrcaFlex data processing
"""

from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

import pandas as pd
from pydantic import BaseModel, Field, validator


@dataclass
class ComponentClassification:
    """Component classification result"""
    type: str  # fst1, fst2, strut, jacket, lngc, unknown
    name: str
    confidence: float  # 0.0 to 1.0


@dataclass
class LoadingConditionInfo:
    """Loading condition information"""
    water_level: str  # hwl, lwl
    volume: str  # 125km3, 180km3
    ballast: str  # pb, sb
    combined: str  # Combined string representation


@dataclass
class PolarDataPoint:
    """Single polar data point"""
    heading: float  # 0 to 360 degrees
    value: float
    unit: str


@dataclass
class ParsedData:
    """Complete parsed data from a file"""
    file_path: Path
    case_name: str
    component: ComponentClassification
    loading_condition: LoadingConditionInfo
    polar_data: Optional[List[PolarDataPoint]]
    time_trace_data: Optional[pd.DataFrame]
    statistics: Dict[str, float]
    raw_data: pd.DataFrame


class ValidationResult(BaseModel):
    """Data validation result"""
    is_valid: bool
    errors: List[str] = Field(default_factory=list)
    warnings: List[str] = Field(default_factory=list)
    data_quality_score: float = Field(ge=0.0, le=1.0)


class ProcessedDataset(BaseModel):
    """Processed dataset ready for database storage"""
    case_id: str
    component_id: str
    loading_condition: str
    timestamp: datetime = Field(default_factory=datetime.now)
    
    # Polar data
    polar_headings: Optional[List[float]] = None
    polar_values: Optional[List[float]] = None
    polar_unit: Optional[str] = None
    
    # Time trace data
    time_points: Optional[List[float]] = None
    time_values: Optional[List[float]] = None
    time_unit: Optional[str] = None
    
    # Statistics
    max_value: float
    min_value: float
    mean_value: float
    std_deviation: float
    percentile_95: float
    percentile_99: float
    
    # Metadata
    source_file: str
    processing_time: float  # seconds
    validation_result: ValidationResult
    
    @validator("polar_headings", "polar_values")
    def validate_polar_arrays(cls, v, values):
        """Ensure polar arrays have same length"""
        if v is not None:
            if "polar_headings" in values and values["polar_headings"] is not None:
                if len(v) != len(values["polar_headings"]):
                    raise ValueError("Polar headings and values must have same length")
        return v


class UnitConverter:
    """Unit conversion utilities"""
    
    FORCE_CONVERSIONS = {
        "N": 1.0,
        "kN": 1000.0,
        "MN": 1e6,
        "lbf": 4.44822,
        "kip": 4448.22,
    }
    
    MOMENT_CONVERSIONS = {
        "Nm": 1.0,
        "kNm": 1000.0,
        "MNm": 1e6,
        "lbf-ft": 1.35582,
        "kip-ft": 1355.82,
    }
    
    LENGTH_CONVERSIONS = {
        "m": 1.0,
        "mm": 0.001,
        "cm": 0.01,
        "ft": 0.3048,
        "in": 0.0254,
    }
    
    @classmethod
    def convert(
        cls,
        value: float,
        from_unit: str,
        to_unit: str,
        unit_type: str = "force",
    ) -> float:
        """
        Convert value between units
        
        Args:
            value: Value to convert
            from_unit: Source unit
            to_unit: Target unit
            unit_type: Type of unit (force, moment, length)
        
        Returns:
            Converted value
        """
        conversions = {
            "force": cls.FORCE_CONVERSIONS,
            "moment": cls.MOMENT_CONVERSIONS,
            "length": cls.LENGTH_CONVERSIONS,
        }.get(unit_type, {})
        
        if from_unit not in conversions or to_unit not in conversions:
            raise ValueError(f"Unknown unit for {unit_type}: {from_unit} or {to_unit}")
        
        # Convert to SI first, then to target
        si_value = value * conversions[from_unit]
        return si_value / conversions[to_unit]


class DataQualityChecker:
    """Data quality validation utilities"""
    
    @staticmethod
    def check_completeness(data: pd.DataFrame) -> float:
        """
        Check data completeness
        
        Args:
            data: DataFrame to check
        
        Returns:
            Completeness score (0.0 to 1.0)
        """
        total_cells = data.size
        non_null_cells = data.count().sum()
        
        if total_cells == 0:
            return 0.0
        
        return non_null_cells / total_cells
    
    @staticmethod
    def check_consistency(data: pd.DataFrame) -> List[str]:
        """
        Check data consistency
        
        Args:
            data: DataFrame to check
        
        Returns:
            List of consistency issues
        """
        issues = []
        
        # Check for duplicate indices
        if data.index.duplicated().any():
            issues.append("Duplicate indices found")
        
        # Check for negative values in typically positive columns
        force_cols = [col for col in data.columns if "force" in col.lower()]
        for col in force_cols:
            if (data[col] < 0).any():
                issues.append(f"Negative values in force column: {col}")
        
        # Check for outliers (values > 3 std dev from mean)
        numeric_cols = data.select_dtypes(include=["float64", "int64"]).columns
        for col in numeric_cols:
            mean = data[col].mean()
            std = data[col].std()
            outliers = ((data[col] - mean).abs() > 3 * std).sum()
            if outliers > 0:
                issues.append(f"Found {outliers} outliers in column: {col}")
        
        return issues
    
    @staticmethod
    def validate_polar_data(
        headings: List[float],
        values: List[float],
    ) -> ValidationResult:
        """
        Validate polar data
        
        Args:
            headings: List of heading angles
            values: List of corresponding values
        
        Returns:
            ValidationResult
        """
        errors = []
        warnings = []
        
        # Check array lengths
        if len(headings) != len(values):
            errors.append("Heading and value arrays have different lengths")
        
        # Check heading range
        if any(h < 0 or h > 360 for h in headings):
            errors.append("Heading values must be between 0 and 360 degrees")
        
        # Check for expected 24 points
        if len(headings) != 24:
            warnings.append(f"Expected 24 heading points, got {len(headings)}")
        
        # Check for uniform spacing
        if len(headings) > 1:
            spacing = [headings[i+1] - headings[i] for i in range(len(headings)-1)]
            if not all(abs(s - 15) < 0.1 for s in spacing):
                warnings.append("Non-uniform heading spacing detected")
        
        # Calculate quality score
        quality_score = 1.0
        quality_score -= 0.2 * len(errors)
        quality_score -= 0.1 * len(warnings)
        quality_score = max(0.0, quality_score)
        
        return ValidationResult(
            is_valid=len(errors) == 0,
            errors=errors,
            warnings=warnings,
            data_quality_score=quality_score,
        )