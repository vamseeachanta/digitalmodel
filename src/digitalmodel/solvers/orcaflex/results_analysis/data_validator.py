"""
Data Quality Validator

Validates OrcaFlex analysis data for engineering constraints, consistency, and completeness.
Implements domain-specific validation rules for offshore engineering data.
"""

import numpy as np
import pandas as pd
import logging
from typing import Dict, List, Optional, Tuple, Union, Set
from dataclasses import dataclass, field
from enum import Enum
import math
import warnings

logger = logging.getLogger(__name__)


class ValidationLevel(Enum):
    """Validation strictness levels"""
    MINIMAL = "minimal"      # Basic data integrity only
    STANDARD = "standard"    # Standard engineering checks
    STRICT = "strict"        # Comprehensive validation


class ValidationCategory(Enum):
    """Categories of validation checks"""
    DATA_INTEGRITY = "data_integrity"
    ENGINEERING_LIMITS = "engineering_limits"
    UNIT_CONSISTENCY = "unit_consistency"
    POLAR_DATA = "polar_data"
    TIME_SERIES = "time_series"
    COMPONENT_CONSISTENCY = "component_consistency"


class Severity(Enum):
    """Validation issue severity"""
    ERROR = "error"          # Critical issues that prevent use
    WARNING = "warning"      # Issues that may affect accuracy
    INFO = "info"           # Informational notes


@dataclass
class ValidationIssue:
    """Individual validation issue"""
    category: ValidationCategory
    severity: Severity
    message: str
    column: Optional[str] = None
    value_range: Optional[Tuple[float, float]] = None
    affected_rows: Optional[List[int]] = None
    metadata: Dict = field(default_factory=dict)


@dataclass
class ValidationResult:
    """Complete validation results"""
    overall_score: float          # 0.0 to 1.0
    category_scores: Dict[ValidationCategory, float]
    issues: List[ValidationIssue]
    statistics: Dict[str, any]
    validation_level: ValidationLevel
    total_checks: int
    passed_checks: int
    
    def is_valid(self, min_score: float = 0.7) -> bool:
        """Check if data passes validation threshold"""
        return self.overall_score >= min_score and not any(
            issue.severity == Severity.ERROR for issue in self.issues
        )


class DataValidator:
    """
    Comprehensive data quality validator for OrcaFlex analysis results.
    
    Validates:
    - Data integrity (completeness, types, ranges)
    - Engineering constraints (forces, moments, displacements)
    - Unit consistency and conversions
    - Polar data completeness and continuity
    - Time series data quality
    - Component data consistency
    """
    
    # Engineering limits for offshore structures (SI units)
    ENGINEERING_LIMITS = {
        'forces': {
            'min': -1e8,  # -100 MN (extreme compression)
            'max': 1e8,   # 100 MN (extreme tension)
            'typical_max': 1e6,  # 1 MN typical maximum
            'units': ['N', 'kN', 'MN']
        },
        'moments': {
            'min': -1e9,  # -1000 MN⋅m
            'max': 1e9,   # 1000 MN⋅m  
            'typical_max': 1e7,  # 10 MN⋅m typical
            'units': ['Nm', 'kNm', 'MNm', 'N.m', 'kN.m', 'MN.m']
        },
        'displacements': {
            'min': -100,  # -100 m
            'max': 100,   # 100 m
            'typical_max': 10,  # 10 m typical
            'units': ['m', 'mm', 'cm']
        },
        'rotations': {
            'min': -360,  # -360 degrees
            'max': 360,   # 360 degrees
            'typical_max': 45,  # 45 degrees typical
            'units': ['deg', 'rad', '°']
        },
        'velocities': {
            'min': -50,   # -50 m/s
            'max': 50,    # 50 m/s
            'typical_max': 10,  # 10 m/s typical
            'units': ['m/s', 'ft/s']
        },
        'accelerations': {
            'min': -100,  # -100 m/s²
            'max': 100,   # 100 m/s²
            'typical_max': 20,  # 20 m/s² typical
            'units': ['m/s2', 'm/s²', 'ft/s2', 'ft/s²']
        }
    }
    
    # Expected polar data points (standard OrcaFlex output)
    STANDARD_POLAR_HEADINGS = np.arange(0, 360, 15)  # 0° to 345° in 15° steps
    
    def __init__(self, validation_level: ValidationLevel = ValidationLevel.STANDARD):
        """
        Initialize data validator.
        
        Args:
            validation_level: Level of validation strictness
        """
        self.validation_level = validation_level
        self.logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")
        
        # Validation statistics
        self.validation_stats = {
            'total_validations': 0,
            'total_issues_found': 0,
            'validation_times': []
        }
    
    def validate_analysis_data(self, df: pd.DataFrame, 
                             components: Optional[List] = None,
                             validation_level: Optional[ValidationLevel] = None) -> ValidationResult:
        """
        Validate complete analysis data.
        
        Args:
            df: DataFrame with analysis results
            components: Optional component classification results
            validation_level: Override default validation level
            
        Returns:
            ValidationResult with comprehensive validation results
        """
        import time
        start_time = time.time()
        
        level = validation_level or self.validation_level
        issues = []
        category_scores = {}
        total_checks = 0
        passed_checks = 0
        
        self.logger.info(f"Validating analysis data: {df.shape[0]} rows, {df.shape[1]} columns")
        
        # 1. Data Integrity Validation
        if level in [ValidationLevel.MINIMAL, ValidationLevel.STANDARD, ValidationLevel.STRICT]:
            integrity_issues, integrity_stats = self._validate_data_integrity(df)
            issues.extend(integrity_issues)
            category_scores[ValidationCategory.DATA_INTEGRITY] = integrity_stats['score']
            total_checks += integrity_stats['total_checks']
            passed_checks += integrity_stats['passed_checks']
        
        # 2. Engineering Limits Validation
        if level in [ValidationLevel.STANDARD, ValidationLevel.STRICT]:
            limits_issues, limits_stats = self._validate_engineering_limits(df)
            issues.extend(limits_issues)
            category_scores[ValidationCategory.ENGINEERING_LIMITS] = limits_stats['score']
            total_checks += limits_stats['total_checks']
            passed_checks += limits_stats['passed_checks']
        
        # 3. Unit Consistency Validation
        if level in [ValidationLevel.STANDARD, ValidationLevel.STRICT]:
            units_issues, units_stats = self._validate_unit_consistency(df)
            issues.extend(units_issues)
            category_scores[ValidationCategory.UNIT_CONSISTENCY] = units_stats['score']
            total_checks += units_stats['total_checks']
            passed_checks += units_stats['passed_checks']
        
        # 4. Polar Data Validation
        polar_issues, polar_stats = self._validate_polar_data(df)
        issues.extend(polar_issues)
        category_scores[ValidationCategory.POLAR_DATA] = polar_stats['score']
        total_checks += polar_stats['total_checks']
        passed_checks += polar_stats['passed_checks']
        
        # 5. Time Series Validation
        if self._has_time_series_data(df):
            ts_issues, ts_stats = self._validate_time_series(df)
            issues.extend(ts_issues)
            category_scores[ValidationCategory.TIME_SERIES] = ts_stats['score']
            total_checks += ts_stats['total_checks']
            passed_checks += ts_stats['passed_checks']
        
        # 6. Component Consistency Validation
        if components and level == ValidationLevel.STRICT:
            comp_issues, comp_stats = self._validate_component_consistency(df, components)
            issues.extend(comp_issues)
            category_scores[ValidationCategory.COMPONENT_CONSISTENCY] = comp_stats['score']
            total_checks += comp_stats['total_checks']
            passed_checks += comp_stats['passed_checks']
        
        # Calculate overall score
        overall_score = passed_checks / total_checks if total_checks > 0 else 0.0
        
        # Adjust score based on error severity
        error_penalty = sum(0.2 for issue in issues if issue.severity == Severity.ERROR)
        warning_penalty = sum(0.05 for issue in issues if issue.severity == Severity.WARNING)
        overall_score = max(0.0, overall_score - error_penalty - warning_penalty)
        
        # Compile validation statistics
        validation_time = time.time() - start_time
        statistics = {
            'validation_time_seconds': validation_time,
            'data_shape': df.shape,
            'memory_usage_mb': df.memory_usage(deep=True).sum() / 1024**2,
            'numeric_columns': len(df.select_dtypes(include=[np.number]).columns),
            'non_numeric_columns': len(df.select_dtypes(exclude=[np.number]).columns),
            'missing_data_percentage': (df.isnull().sum().sum() / df.size) * 100,
            'issues_by_severity': {
                severity.value: sum(1 for issue in issues if issue.severity == severity)
                for severity in Severity
            }
        }
        
        # Update validator statistics
        self.validation_stats['total_validations'] += 1
        self.validation_stats['total_issues_found'] += len(issues)
        self.validation_stats['validation_times'].append(validation_time)
        
        result = ValidationResult(
            overall_score=overall_score,
            category_scores=category_scores,
            issues=issues,
            statistics=statistics,
            validation_level=level,
            total_checks=total_checks,
            passed_checks=passed_checks
        )
        
        self.logger.info(f"Validation completed: score={overall_score:.2f}, "
                        f"{len(issues)} issues found in {validation_time:.2f}s")
        
        return result
    
    def _validate_data_integrity(self, df: pd.DataFrame) -> Tuple[List[ValidationIssue], Dict]:
        """Validate basic data integrity"""
        issues = []
        total_checks = 0
        passed_checks = 0
        
        # Check for empty DataFrame
        total_checks += 1
        if df.empty:
            issues.append(ValidationIssue(
                category=ValidationCategory.DATA_INTEGRITY,
                severity=Severity.ERROR,
                message="DataFrame is empty"
            ))
        else:
            passed_checks += 1
        
        # Check for duplicate columns
        total_checks += 1
        duplicate_cols = df.columns[df.columns.duplicated()].tolist()
        if duplicate_cols:
            issues.append(ValidationIssue(
                category=ValidationCategory.DATA_INTEGRITY,
                severity=Severity.WARNING,
                message=f"Duplicate columns found: {duplicate_cols}"
            ))
        else:
            passed_checks += 1
        
        # Check for completely empty columns
        total_checks += len(df.columns)
        for col in df.columns:
            if df[col].isna().all():
                issues.append(ValidationIssue(
                    category=ValidationCategory.DATA_INTEGRITY,
                    severity=Severity.WARNING,
                    message=f"Column '{col}' is completely empty",
                    column=col
                ))
            else:
                passed_checks += 1
        
        # Check for excessive missing data
        missing_threshold = 0.5  # 50%
        total_checks += len(df.columns)
        for col in df.columns:
            missing_pct = df[col].isna().mean()
            if missing_pct > missing_threshold:
                issues.append(ValidationIssue(
                    category=ValidationCategory.DATA_INTEGRITY,
                    severity=Severity.WARNING,
                    message=f"Column '{col}' has {missing_pct:.1%} missing data",
                    column=col
                ))
            else:
                passed_checks += 1
        
        # Check for infinite values in numeric columns
        numeric_cols = df.select_dtypes(include=[np.number]).columns
        total_checks += len(numeric_cols)
        for col in numeric_cols:
            if np.isinf(df[col]).any():
                inf_count = np.isinf(df[col]).sum()
                issues.append(ValidationIssue(
                    category=ValidationCategory.DATA_INTEGRITY,
                    severity=Severity.ERROR,
                    message=f"Column '{col}' contains {inf_count} infinite values",
                    column=col
                ))
            else:
                passed_checks += 1
        
        # Check for constant values (may indicate sensor failure)
        total_checks += len(numeric_cols)
        for col in numeric_cols:
            if len(df[col].dropna()) > 1:
                if df[col].nunique() == 1:
                    issues.append(ValidationIssue(
                        category=ValidationCategory.DATA_INTEGRITY,
                        severity=Severity.INFO,
                        message=f"Column '{col}' has constant value: {df[col].iloc[0]}",
                        column=col
                    ))
                else:
                    passed_checks += 1
            else:
                passed_checks += 1  # Skip check for insufficient data
        
        return issues, {
            'score': passed_checks / total_checks if total_checks > 0 else 1.0,
            'total_checks': total_checks,
            'passed_checks': passed_checks
        }
    
    def _validate_engineering_limits(self, df: pd.DataFrame) -> Tuple[List[ValidationIssue], Dict]:
        """Validate engineering parameter limits"""
        issues = []
        total_checks = 0
        passed_checks = 0
        
        numeric_cols = df.select_dtypes(include=[np.number]).columns
        
        for col in numeric_cols:
            col_lower = col.lower()
            data = df[col].dropna()
            
            if len(data) == 0:
                continue
            
            # Determine parameter type from column name
            param_type = None
            if any(word in col_lower for word in ['fx', 'fy', 'fz', 'force']):
                param_type = 'forces'
            elif any(word in col_lower for word in ['mx', 'my', 'mz', 'moment']):
                param_type = 'moments'
            elif any(word in col_lower for word in ['x', 'y', 'z', 'disp', 'pos']):
                param_type = 'displacements'
            elif any(word in col_lower for word in ['rx', 'ry', 'rz', 'rot', 'deg']):
                param_type = 'rotations'
            elif 'vel' in col_lower:
                param_type = 'velocities'
            elif 'acc' in col_lower:
                param_type = 'accelerations'
            
            if param_type and param_type in self.ENGINEERING_LIMITS:
                limits = self.ENGINEERING_LIMITS[param_type]
                
                # Check absolute limits
                total_checks += 2
                min_val, max_val = data.min(), data.max()
                
                if min_val < limits['min']:
                    issues.append(ValidationIssue(
                        category=ValidationCategory.ENGINEERING_LIMITS,
                        severity=Severity.ERROR,
                        message=f"Column '{col}' has values below engineering limit: "
                               f"{min_val:.2e} < {limits['min']:.2e}",
                        column=col,
                        value_range=(min_val, max_val)
                    ))
                else:
                    passed_checks += 1
                
                if max_val > limits['max']:
                    issues.append(ValidationIssue(
                        category=ValidationCategory.ENGINEERING_LIMITS,
                        severity=Severity.ERROR,
                        message=f"Column '{col}' has values above engineering limit: "
                               f"{max_val:.2e} > {limits['max']:.2e}",
                        column=col,
                        value_range=(min_val, max_val)
                    ))
                else:
                    passed_checks += 1
                
                # Check typical limits (warnings only)
                if self.validation_level == ValidationLevel.STRICT:
                    total_checks += 1
                    typical_max = limits['typical_max']
                    if abs(max_val) > typical_max or abs(min_val) > typical_max:
                        issues.append(ValidationIssue(
                            category=ValidationCategory.ENGINEERING_LIMITS,
                            severity=Severity.WARNING,
                            message=f"Column '{col}' has unusually high values: "
                                   f"range [{min_val:.2e}, {max_val:.2e}], "
                                   f"typical max: {typical_max:.2e}",
                            column=col,
                            value_range=(min_val, max_val)
                        ))
                    else:
                        passed_checks += 1
        
        return issues, {
            'score': passed_checks / total_checks if total_checks > 0 else 1.0,
            'total_checks': total_checks,
            'passed_checks': passed_checks
        }
    
    def _validate_unit_consistency(self, df: pd.DataFrame) -> Tuple[List[ValidationIssue], Dict]:
        """Validate unit consistency across columns"""
        issues = []
        total_checks = 0
        passed_checks = 0
        
        # Group columns by parameter type
        param_groups = {
            'forces': [],
            'moments': [], 
            'displacements': [],
            'rotations': []
        }
        
        for col in df.columns:
            col_lower = col.lower()
            if any(word in col_lower for word in ['fx', 'fy', 'fz', 'force']):
                param_groups['forces'].append(col)
            elif any(word in col_lower for word in ['mx', 'my', 'mz', 'moment']):
                param_groups['moments'].append(col)
            elif any(word in col_lower for word in ['x', 'y', 'z', 'disp', 'pos']):
                param_groups['displacements'].append(col)
            elif any(word in col_lower for word in ['rx', 'ry', 'rz', 'rot']):
                param_groups['rotations'].append(col)
        
        # Check unit consistency within parameter groups
        for param_type, columns in param_groups.items():
            if len(columns) < 2:
                continue
                
            # Extract units from column names
            units_found = set()
            for col in columns:
                for unit_list in self.ENGINEERING_LIMITS.get(param_type, {}).get('units', []):
                    if unit_list.lower() in col.lower():
                        units_found.add(unit_list)
                        break
            
            total_checks += 1
            if len(units_found) > 1:
                issues.append(ValidationIssue(
                    category=ValidationCategory.UNIT_CONSISTENCY,
                    severity=Severity.WARNING,
                    message=f"Inconsistent units in {param_type}: {units_found}",
                    metadata={'columns': columns, 'units': list(units_found)}
                ))
            else:
                passed_checks += 1
        
        # Check for unit indicators in column names
        total_checks += len(df.select_dtypes(include=[np.number]).columns)
        for col in df.select_dtypes(include=[np.number]).columns:
            has_unit = any(
                unit in col.lower() 
                for unit_list in [limits.get('units', []) for limits in self.ENGINEERING_LIMITS.values()]
                for unit in unit_list
            )
            
            if not has_unit and self.validation_level == ValidationLevel.STRICT:
                issues.append(ValidationIssue(
                    category=ValidationCategory.UNIT_CONSISTENCY,
                    severity=Severity.INFO,
                    message=f"Column '{col}' missing unit indicator",
                    column=col
                ))
            else:
                passed_checks += 1
        
        return issues, {
            'score': passed_checks / total_checks if total_checks > 0 else 1.0,
            'total_checks': total_checks,
            'passed_checks': passed_checks
        }
    
    def _validate_polar_data(self, df: pd.DataFrame) -> Tuple[List[ValidationIssue], Dict]:
        """Validate polar data completeness and consistency"""
        issues = []
        total_checks = 0
        passed_checks = 0
        
        # Find polar columns (containing degree information)
        polar_columns = []
        for col in df.columns:
            if any(pattern in col.lower() for pattern in ['deg', '°']):
                # Extract heading value
                import re
                match = re.search(r'(\d+(?:\.\d+)?)\s*(?:deg|°)', col, re.IGNORECASE)
                if match:
                    heading = float(match.group(1))
                    if 0 <= heading < 360:
                        polar_columns.append((col, heading))
        
        if not polar_columns:
            # No polar data found - this might be normal for some files
            return issues, {
                'score': 1.0,
                'total_checks': 0,
                'passed_checks': 0
            }
        
        # Group polar columns by parameter (everything before the heading)
        param_groups = {}
        for col, heading in polar_columns:
            import re
            param_match = re.search(r'^(.+?)\s+\d+(?:\.\d+)?\s*(?:deg|°)', col, re.IGNORECASE)
            if param_match:
                param_name = param_match.group(1).strip()
                if param_name not in param_groups:
                    param_groups[param_name] = []
                param_groups[param_name].append((col, heading))
        
        # Validate each parameter group
        for param_name, columns in param_groups.items():
            headings = [heading for _, heading in columns]
            headings.sort()
            
            total_checks += 1
            
            # Check for standard polar data completeness (24 points expected)
            expected_headings = set(self.STANDARD_POLAR_HEADINGS)
            actual_headings = set(headings)
            
            completeness = len(actual_headings & expected_headings) / len(expected_headings)
            
            if completeness < 0.8:  # Less than 80% complete
                missing_headings = expected_headings - actual_headings
                issues.append(ValidationIssue(
                    category=ValidationCategory.POLAR_DATA,
                    severity=Severity.WARNING,
                    message=f"Polar data incomplete for '{param_name}': "
                           f"{completeness:.1%} complete, missing {len(missing_headings)} headings",
                    metadata={
                        'parameter': param_name,
                        'completeness': completeness,
                        'missing_headings': sorted(missing_headings)
                    }
                ))
            else:
                passed_checks += 1
            
            # Check for data continuity (no large gaps)
            total_checks += 1
            if len(headings) >= 3:
                gaps = np.diff(sorted(headings))
                max_gap = np.max(gaps)
                
                if max_gap > 45:  # Gap larger than 45 degrees
                    issues.append(ValidationIssue(
                        category=ValidationCategory.POLAR_DATA,
                        severity=Severity.WARNING,
                        message=f"Large gap in polar data for '{param_name}': {max_gap}°",
                        metadata={'parameter': param_name, 'max_gap': max_gap}
                    ))
                else:
                    passed_checks += 1
            else:
                passed_checks += 1  # Skip check for insufficient data
            
            # Check for reasonable data values
            total_checks += 1
            try:
                values = []
                for col, _ in columns:
                    col_data = pd.to_numeric(df[col], errors='coerce').dropna()
                    if len(col_data) > 0:
                        values.extend(col_data.tolist())
                
                if values:
                    values = np.array(values)
                    if np.any(np.isnan(values)) or np.any(np.isinf(values)):
                        issues.append(ValidationIssue(
                            category=ValidationCategory.POLAR_DATA,
                            severity=Severity.ERROR,
                            message=f"Invalid values in polar data for '{param_name}'",
                            metadata={'parameter': param_name}
                        ))
                    else:
                        passed_checks += 1
                else:
                    issues.append(ValidationIssue(
                        category=ValidationCategory.POLAR_DATA,
                        severity=Severity.ERROR,
                        message=f"No valid data values found for polar parameter '{param_name}'",
                        metadata={'parameter': param_name}
                    ))
            except Exception as e:
                issues.append(ValidationIssue(
                    category=ValidationCategory.POLAR_DATA,
                    severity=Severity.ERROR,
                    message=f"Error validating polar data for '{param_name}': {str(e)}",
                    metadata={'parameter': param_name}
                ))
        
        return issues, {
            'score': passed_checks / total_checks if total_checks > 0 else 1.0,
            'total_checks': total_checks,
            'passed_checks': passed_checks
        }
    
    def _validate_time_series(self, df: pd.DataFrame) -> Tuple[List[ValidationIssue], Dict]:
        """Validate time series data quality"""
        issues = []
        total_checks = 0
        passed_checks = 0
        
        # Find time column
        time_col = None
        for col in df.columns:
            if 'time' in col.lower() or col.lower() == 't':
                time_col = col
                break
        
        if time_col is None:
            return issues, {
                'score': 1.0,
                'total_checks': 0,
                'passed_checks': 0
            }
        
        time_data = pd.to_numeric(df[time_col], errors='coerce').dropna()
        
        # Check time series properties
        total_checks += 1
        if len(time_data) < 2:
            issues.append(ValidationIssue(
                category=ValidationCategory.TIME_SERIES,
                severity=Severity.ERROR,
                message=f"Insufficient time series data: {len(time_data)} points",
                column=time_col
            ))
        else:
            passed_checks += 1
        
        if len(time_data) >= 2:
            # Check for monotonically increasing time
            total_checks += 1
            if not time_data.is_monotonic_increasing:
                issues.append(ValidationIssue(
                    category=ValidationCategory.TIME_SERIES,
                    severity=Severity.WARNING,
                    message="Time series is not monotonically increasing",
                    column=time_col
                ))
            else:
                passed_checks += 1
            
            # Check for consistent time step
            total_checks += 1
            time_diffs = np.diff(time_data)
            dt_std = np.std(time_diffs)
            dt_mean = np.mean(time_diffs)
            
            if dt_std / dt_mean > 0.1:  # 10% variation
                issues.append(ValidationIssue(
                    category=ValidationCategory.TIME_SERIES,
                    severity=Severity.INFO,
                    message=f"Irregular time step: std/mean = {dt_std/dt_mean:.3f}",
                    column=time_col,
                    metadata={'dt_mean': dt_mean, 'dt_std': dt_std}
                ))
            else:
                passed_checks += 1
            
            # Check for reasonable time range
            total_checks += 1
            time_range = time_data.max() - time_data.min()
            if time_range <= 0:
                issues.append(ValidationIssue(
                    category=ValidationCategory.TIME_SERIES,
                    severity=Severity.ERROR,
                    message="Invalid time range: range <= 0",
                    column=time_col
                ))
            elif time_range > 365 * 24 * 3600:  # More than a year
                issues.append(ValidationIssue(
                    category=ValidationCategory.TIME_SERIES,
                    severity=Severity.WARNING,
                    message=f"Very long time series: {time_range/3600:.1f} hours",
                    column=time_col
                ))
                passed_checks += 1
            else:
                passed_checks += 1
        
        return issues, {
            'score': passed_checks / total_checks if total_checks > 0 else 1.0,
            'total_checks': total_checks,
            'passed_checks': passed_checks
        }
    
    def _validate_component_consistency(self, df: pd.DataFrame, 
                                      components: List) -> Tuple[List[ValidationIssue], Dict]:
        """Validate consistency between classified components"""
        issues = []
        total_checks = 0
        passed_checks = 0
        
        # Check that all columns are classified
        classified_columns = set()
        for component in components:
            classified_columns.update(component.columns if hasattr(component, 'columns') else [])
        
        unclassified_columns = set(df.columns) - classified_columns
        
        total_checks += 1
        if unclassified_columns:
            issues.append(ValidationIssue(
                category=ValidationCategory.COMPONENT_CONSISTENCY,
                severity=Severity.INFO,
                message=f"Unclassified columns found: {list(unclassified_columns)[:5]}",
                metadata={'unclassified_count': len(unclassified_columns)}
            ))
        else:
            passed_checks += 1
        
        # Check component data consistency
        for component in components:
            if not hasattr(component, 'columns') or not hasattr(component, 'component_type'):
                continue
                
            component_columns = [col for col in component.columns if col in df.columns]
            
            if not component_columns:
                continue
            
            # Check for consistent data ranges within component
            total_checks += 1
            try:
                component_data = df[component_columns].select_dtypes(include=[np.number])
                if not component_data.empty:
                    # Check for outliers using IQR method
                    Q1 = component_data.quantile(0.25)
                    Q3 = component_data.quantile(0.75)
                    IQR = Q3 - Q1
                    
                    outlier_threshold = 3.0
                    outliers = ((component_data < (Q1 - outlier_threshold * IQR)) | 
                              (component_data > (Q3 + outlier_threshold * IQR))).sum().sum()
                    
                    total_outliers = component_data.count().sum()
                    outlier_rate = outliers / total_outliers if total_outliers > 0 else 0
                    
                    if outlier_rate > 0.05:  # More than 5% outliers
                        issues.append(ValidationIssue(
                            category=ValidationCategory.COMPONENT_CONSISTENCY,
                            severity=Severity.WARNING,
                            message=f"Component '{component.name}' has high outlier rate: {outlier_rate:.1%}",
                            metadata={'component': component.name, 'outlier_rate': outlier_rate}
                        ))
                    else:
                        passed_checks += 1
                else:
                    passed_checks += 1  # No numeric data to check
            except Exception as e:
                issues.append(ValidationIssue(
                    category=ValidationCategory.COMPONENT_CONSISTENCY,
                    severity=Severity.ERROR,
                    message=f"Error checking component consistency for '{component.name}': {str(e)}",
                    metadata={'component': component.name}
                ))
        
        return issues, {
            'score': passed_checks / total_checks if total_checks > 0 else 1.0,
            'total_checks': total_checks,
            'passed_checks': passed_checks
        }
    
    def _has_time_series_data(self, df: pd.DataFrame) -> bool:
        """Check if DataFrame contains time series data"""
        time_indicators = ['time', 't', 'timestamp', 'datetime']
        return any(
            any(indicator in col.lower() for indicator in time_indicators)
            for col in df.columns
        )
    
    def get_validation_summary(self, result: ValidationResult) -> str:
        """Generate human-readable validation summary"""
        summary_parts = [
            f"Validation Score: {result.overall_score:.2f}/1.00",
            f"Checks: {result.passed_checks}/{result.total_checks} passed"
        ]
        
        if result.issues:
            error_count = sum(1 for issue in result.issues if issue.severity == Severity.ERROR)
            warning_count = sum(1 for issue in result.issues if issue.severity == Severity.WARNING)
            info_count = sum(1 for issue in result.issues if issue.severity == Severity.INFO)
            
            issue_summary = []
            if error_count > 0:
                issue_summary.append(f"{error_count} errors")
            if warning_count > 0:
                issue_summary.append(f"{warning_count} warnings")
            if info_count > 0:
                issue_summary.append(f"{info_count} info")
            
            summary_parts.append(f"Issues: {', '.join(issue_summary)}")
        else:
            summary_parts.append("No issues found")
        
        return " | ".join(summary_parts)
    
    def get_validator_stats(self) -> Dict:
        """Get validator performance statistics"""
        stats = self.validation_stats.copy()
        
        if stats['validation_times']:
            stats['average_validation_time'] = np.mean(stats['validation_times'])
            stats['total_validation_time'] = sum(stats['validation_times'])
        
        return stats


# Utility functions
def quick_validate(df: pd.DataFrame, 
                  validation_level: ValidationLevel = ValidationLevel.STANDARD) -> ValidationResult:
    """Quick validation with default settings"""
    validator = DataValidator(validation_level)
    return validator.validate_analysis_data(df)


def validate_file_data(file_path: str, **kwargs) -> ValidationResult:
    """Validate data directly from CSV file"""
    import pandas as pd
    from pathlib import Path
    
    df = pd.read_csv(Path(file_path))
    return quick_validate(df, **kwargs)