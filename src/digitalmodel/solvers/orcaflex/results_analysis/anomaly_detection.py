"""
Anomaly Detection Service for OrcaFlex Results Dashboard

Provides automated anomaly detection capabilities including:
- Statistical outlier detection (IQR, Z-score, Modified Z-score)
- Engineering limit violations
- Time series anomaly detection
- Multi-variate anomaly detection
- Machine learning-based detection
"""

import numpy as np
import pandas as pd
from scipy import stats
from sklearn.ensemble import IsolationForest
from sklearn.preprocessing import StandardScaler
from sklearn.covariance import EllipticEnvelope
from typing import Dict, List, Tuple, Optional, Any, Union
import logging
from dataclasses import dataclass
from enum import Enum
from datetime import datetime
import warnings

warnings.filterwarnings('ignore')
logger = logging.getLogger(__name__)


class AnomalyType(Enum):
    OUTLIER = "outlier"
    ENGINEERING_LIMIT = "engineering_limit"
    TREND_ANOMALY = "trend_anomaly"
    MULTIVARIATE = "multivariate"
    TEMPORAL = "temporal"


class DetectionMethod(Enum):
    IQR = "iqr"
    Z_SCORE = "z_score"
    MODIFIED_Z_SCORE = "modified_z_score"
    ISOLATION_FOREST = "isolation_forest"
    ELLIPTIC_ENVELOPE = "elliptic_envelope"
    ENGINEERING_LIMITS = "engineering_limits"


@dataclass
class AnomalyResult:
    """Container for anomaly detection results"""
    index: int
    value: float
    anomaly_type: str
    detection_method: str
    severity: str  # 'low', 'medium', 'high', 'critical'
    score: float  # Anomaly score (higher = more anomalous)
    threshold: float
    description: str
    recommendations: List[str]


@dataclass
class EngineeringLimits:
    """Container for engineering design limits"""
    parameter: str
    min_value: Optional[float] = None
    max_value: Optional[float] = None
    warning_min: Optional[float] = None
    warning_max: Optional[float] = None
    units: Optional[str] = None
    description: Optional[str] = None


class AnomalyDetectionService:
    """Service for automated anomaly detection in OrcaFlex results"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.engineering_limits = self._initialize_engineering_limits()
    
    def _initialize_engineering_limits(self) -> Dict[str, EngineeringLimits]:
        """Initialize standard engineering limits for marine structures"""
        return {
            'tension': EngineeringLimits(
                parameter='tension',
                min_value=0.0,
                max_value=None,  # Set based on breaking strength
                warning_max=None,  # Set based on working load limit
                units='N',
                description='Line tension'
            ),
            'bend_radius': EngineeringLimits(
                parameter='bend_radius',
                min_value=None,  # Set based on minimum bend radius
                max_value=None,
                warning_min=None,
                units='m',
                description='Bend radius'
            ),
            'vessel_offset': EngineeringLimits(
                parameter='vessel_offset',
                min_value=0.0,
                max_value=None,  # Set based on watch circle
                warning_max=None,  # Set based on operational limits
                units='m',
                description='Vessel horizontal offset'
            ),
            'heave': EngineeringLimits(
                parameter='heave',
                min_value=None,
                max_value=None,
                warning_max=None,  # Set based on operational limits
                units='m',
                description='Vessel heave motion'
            ),
            'pitch': EngineeringLimits(
                parameter='pitch',
                min_value=None,
                max_value=None,
                warning_max=15.0,  # degrees
                units='deg',
                description='Vessel pitch angle'
            ),
            'roll': EngineeringLimits(
                parameter='roll',
                min_value=None,
                max_value=None,
                warning_max=15.0,  # degrees
                units='deg',
                description='Vessel roll angle'
            )
        }
    
    def detect_statistical_outliers(
        self,
        data: Union[pd.Series, np.ndarray],
        method: str = 'iqr',
        threshold_factor: float = 1.5,
        return_scores: bool = True
    ) -> Dict[str, Any]:
        """
        Detect statistical outliers using various methods
        
        IQR Method: Outliers are values outside Q1 - 1.5*IQR and Q3 + 1.5*IQR
        Z-Score Method: Outliers are values with |z-score| > threshold (typically 3)
        Modified Z-Score: Uses median absolute deviation instead of standard deviation
        
        Args:
            data: Input data (Series or array)
            method: Detection method ('iqr', 'z_score', 'modified_z_score')
            threshold_factor: Threshold multiplier for outlier detection
            return_scores: Whether to return anomaly scores
            
        Returns:
            Dictionary with outlier indices, values, and scores
        """
        try:
            # Convert to numpy array for processing
            if isinstance(data, pd.Series):
                values = data.values
                original_index = data.index
            else:
                values = np.array(data)
                original_index = np.arange(len(values))
            
            # Remove NaN values
            valid_mask = ~np.isnan(values)
            valid_values = values[valid_mask]
            valid_indices = original_index[valid_mask]
            
            if len(valid_values) < 3:
                raise ValueError("Need at least 3 valid data points for outlier detection")
            
            outlier_mask = np.zeros(len(valid_values), dtype=bool)
            scores = np.zeros(len(valid_values))
            
            if method == 'iqr':
                # IQR method: Q1 - 1.5*IQR < x < Q3 + 1.5*IQR
                Q1 = np.percentile(valid_values, 25)
                Q3 = np.percentile(valid_values, 75)
                IQR = Q3 - Q1
                
                lower_bound = Q1 - threshold_factor * IQR
                upper_bound = Q3 + threshold_factor * IQR
                
                outlier_mask = (valid_values < lower_bound) | (valid_values > upper_bound)
                
                # Calculate scores (distance from nearest bound)
                scores = np.maximum(
                    np.maximum(lower_bound - valid_values, 0),
                    np.maximum(valid_values - upper_bound, 0)
                ) / IQR
                
                threshold_info = {
                    'lower_bound': lower_bound,
                    'upper_bound': upper_bound,
                    'Q1': Q1,
                    'Q3': Q3,
                    'IQR': IQR
                }
            
            elif method == 'z_score':
                # Z-score method: |z| > threshold
                mean_val = np.mean(valid_values)
                std_val = np.std(valid_values)
                
                if std_val == 0:
                    # All values are the same
                    scores = np.zeros(len(valid_values))
                    outlier_mask = np.zeros(len(valid_values), dtype=bool)
                else:
                    z_scores = np.abs((valid_values - mean_val) / std_val)
                    outlier_mask = z_scores > threshold_factor
                    scores = z_scores
                
                threshold_info = {
                    'mean': mean_val,
                    'std': std_val,
                    'threshold': threshold_factor
                }
            
            elif method == 'modified_z_score':
                # Modified Z-score using median absolute deviation
                median_val = np.median(valid_values)
                mad = np.median(np.abs(valid_values - median_val))
                
                if mad == 0:
                    # All values are the same
                    scores = np.zeros(len(valid_values))
                    outlier_mask = np.zeros(len(valid_values), dtype=bool)
                else:
                    modified_z_scores = 0.6745 * (valid_values - median_val) / mad
                    outlier_mask = np.abs(modified_z_scores) > threshold_factor
                    scores = np.abs(modified_z_scores)
                
                threshold_info = {
                    'median': median_val,
                    'mad': mad,
                    'threshold': threshold_factor
                }
            
            else:
                raise ValueError(f"Unknown method: {method}")
            
            # Create anomaly results
            anomalies = []
            outlier_indices = valid_indices[outlier_mask]
            outlier_values = valid_values[outlier_mask]
            outlier_scores = scores[outlier_mask]
            
            for idx, value, score in zip(outlier_indices, outlier_values, outlier_scores):
                severity = self._classify_anomaly_severity(score, method)
                
                anomaly = AnomalyResult(
                    index=idx,
                    value=value,
                    anomaly_type=AnomalyType.OUTLIER.value,
                    detection_method=method,
                    severity=severity,
                    score=score,
                    threshold=threshold_factor,
                    description=f"Statistical outlier detected using {method} method",
                    recommendations=self._generate_outlier_recommendations(value, method, severity)
                )
                anomalies.append(anomaly)
            
            return {
                'anomalies': anomalies,
                'total_outliers': len(anomalies),
                'outlier_percentage': len(anomalies) / len(valid_values) * 100,
                'method': method,
                'threshold_info': threshold_info,
                'all_scores': scores if return_scores else None
            }
            
        except Exception as e:
            self.logger.error(f"Error in statistical outlier detection: {str(e)}")
            raise
    
    def detect_engineering_limit_violations(
        self,
        data: pd.DataFrame,
        custom_limits: Optional[Dict[str, EngineeringLimits]] = None
    ) -> Dict[str, Any]:
        """
        Detect violations of engineering design limits
        
        Args:
            data: DataFrame with engineering parameters
            custom_limits: Custom engineering limits to override defaults
            
        Returns:
            Dictionary with limit violation results
        """
        try:
            # Merge custom limits with defaults
            limits = self.engineering_limits.copy()
            if custom_limits:
                limits.update(custom_limits)
            
            violations = []
            
            for column in data.columns:
                if column not in limits:
                    continue
                
                limit = limits[column]
                column_data = data[column].dropna()
                
                if len(column_data) == 0:
                    continue
                
                # Check for violations
                for idx, value in column_data.items():
                    violation_type = None
                    severity = 'low'
                    description = ""
                    
                    # Check critical limits
                    if limit.min_value is not None and value < limit.min_value:
                        violation_type = 'critical_min'
                        severity = 'critical'
                        description = f"{column} below minimum limit ({limit.min_value} {limit.units or ''})"
                    
                    elif limit.max_value is not None and value > limit.max_value:
                        violation_type = 'critical_max'
                        severity = 'critical'
                        description = f"{column} above maximum limit ({limit.max_value} {limit.units or ''})"
                    
                    # Check warning limits
                    elif limit.warning_min is not None and value < limit.warning_min:
                        violation_type = 'warning_min'
                        severity = 'medium'
                        description = f"{column} below warning limit ({limit.warning_min} {limit.units or ''})"
                    
                    elif limit.warning_max is not None and value > limit.warning_max:
                        violation_type = 'warning_max'
                        severity = 'medium'
                        description = f"{column} above warning limit ({limit.warning_max} {limit.units or ''})"
                    
                    if violation_type:
                        # Calculate severity score
                        if violation_type.startswith('critical'):
                            score = 1.0
                        else:
                            score = 0.5
                        
                        anomaly = AnomalyResult(
                            index=idx,
                            value=value,
                            anomaly_type=AnomalyType.ENGINEERING_LIMIT.value,
                            detection_method=DetectionMethod.ENGINEERING_LIMITS.value,
                            severity=severity,
                            score=score,
                            threshold=limit.max_value or limit.min_value or 0,
                            description=description,
                            recommendations=self._generate_limit_violation_recommendations(
                                column, value, violation_type, limit
                            )
                        )
                        violations.append(anomaly)
            
            # Categorize violations
            critical_violations = [v for v in violations if v.severity == 'critical']
            warning_violations = [v for v in violations if v.severity == 'medium']
            
            return {
                'violations': violations,
                'critical_violations': len(critical_violations),
                'warning_violations': len(warning_violations),
                'total_violations': len(violations),
                'violated_parameters': list(set([v.description.split()[0] for v in violations])),
                'limits_checked': list(limits.keys())
            }
            
        except Exception as e:
            self.logger.error(f"Error in engineering limit detection: {str(e)}")
            raise
    
    def detect_multivariate_anomalies(
        self,
        data: pd.DataFrame,
        method: str = 'isolation_forest',
        contamination: float = 0.1,
        random_state: int = 42
    ) -> Dict[str, Any]:
        """
        Detect multivariate anomalies using machine learning methods
        
        Args:
            data: DataFrame with multiple variables
            method: Detection method ('isolation_forest', 'elliptic_envelope')
            contamination: Expected proportion of outliers
            random_state: Random seed for reproducibility
            
        Returns:
            Dictionary with multivariate anomaly results
        """
        try:
            # Prepare data
            numeric_data = data.select_dtypes(include=[np.number])
            if numeric_data.empty:
                raise ValueError("No numeric columns found for multivariate analysis")
            
            # Remove rows with any NaN values
            clean_data = numeric_data.dropna()
            if len(clean_data) < 10:
                raise ValueError("Need at least 10 complete observations for multivariate analysis")
            
            # Standardize data
            scaler = StandardScaler()
            scaled_data = scaler.fit_transform(clean_data)
            
            # Choose detection algorithm
            if method == 'isolation_forest':
                detector = IsolationForest(
                    contamination=contamination,
                    random_state=random_state,
                    n_estimators=100
                )
            elif method == 'elliptic_envelope':
                detector = EllipticEnvelope(
                    contamination=contamination,
                    random_state=random_state
                )
            else:
                raise ValueError(f"Unknown method: {method}")
            
            # Fit detector and predict
            detector.fit(scaled_data)
            predictions = detector.predict(scaled_data)
            scores = detector.decision_function(scaled_data)
            
            # Convert predictions (-1 for outliers, 1 for inliers)
            is_outlier = predictions == -1
            
            # Create anomaly results
            anomalies = []
            outlier_indices = clean_data.index[is_outlier]
            outlier_scores = np.abs(scores[is_outlier])  # Use absolute value for scoring
            
            for idx, score in zip(outlier_indices, outlier_scores):
                severity = self._classify_anomaly_severity(score, 'multivariate')
                
                # Get the actual values for this row
                row_values = clean_data.loc[idx].to_dict()
                
                anomaly = AnomalyResult(
                    index=idx,
                    value=score,  # Use anomaly score as value
                    anomaly_type=AnomalyType.MULTIVARIATE.value,
                    detection_method=method,
                    severity=severity,
                    score=score,
                    threshold=contamination,
                    description=f"Multivariate anomaly detected using {method}",
                    recommendations=self._generate_multivariate_recommendations(row_values, method)
                )
                anomalies.append(anomaly)
            
            return {
                'anomalies': anomalies,
                'total_anomalies': len(anomalies),
                'anomaly_percentage': len(anomalies) / len(clean_data) * 100,
                'method': method,
                'contamination': contamination,
                'features_used': list(clean_data.columns),
                'all_scores': scores.tolist()
            }
            
        except Exception as e:
            self.logger.error(f"Error in multivariate anomaly detection: {str(e)}")
            raise
    
    def detect_time_series_anomalies(
        self,
        time_series_data: pd.DataFrame,
        time_column: str,
        value_columns: List[str],
        window_size: int = 10,
        threshold: float = 3.0
    ) -> Dict[str, Any]:
        """
        Detect anomalies in time series data using rolling statistics
        
        Args:
            time_series_data: DataFrame with time series data
            time_column: Name of time column
            value_columns: List of value column names to analyze
            window_size: Rolling window size for anomaly detection
            threshold: Threshold for anomaly detection (in standard deviations)
            
        Returns:
            Dictionary with time series anomaly results
        """
        try:
            if time_column not in time_series_data.columns:
                raise ValueError(f"Time column '{time_column}' not found")
            
            # Sort by time
            sorted_data = time_series_data.sort_values(time_column)
            
            all_anomalies = {}
            
            for column in value_columns:
                if column not in sorted_data.columns:
                    self.logger.warning(f"Column '{column}' not found in data")
                    continue
                
                column_data = sorted_data[column].dropna()
                if len(column_data) < window_size * 2:
                    self.logger.warning(f"Insufficient data for time series analysis of {column}")
                    continue
                
                # Calculate rolling statistics
                rolling_mean = column_data.rolling(window=window_size, center=True).mean()
                rolling_std = column_data.rolling(window=window_size, center=True).std()
                
                # Calculate Z-scores relative to rolling statistics
                z_scores = np.abs((column_data - rolling_mean) / rolling_std)
                
                # Find anomalies
                anomaly_mask = z_scores > threshold
                anomaly_indices = column_data.index[anomaly_mask]
                
                anomalies = []
                for idx in anomaly_indices:
                    if not np.isnan(z_scores.loc[idx]):
                        severity = self._classify_anomaly_severity(z_scores.loc[idx], 'time_series')
                        
                        anomaly = AnomalyResult(
                            index=idx,
                            value=column_data.loc[idx],
                            anomaly_type=AnomalyType.TEMPORAL.value,
                            detection_method='rolling_z_score',
                            severity=severity,
                            score=z_scores.loc[idx],
                            threshold=threshold,
                            description=f"Time series anomaly in {column}",
                            recommendations=self._generate_temporal_recommendations(
                                column, column_data.loc[idx], severity
                            )
                        )
                        anomalies.append(anomaly)
                
                all_anomalies[column] = {
                    'anomalies': anomalies,
                    'total_anomalies': len(anomalies),
                    'anomaly_percentage': len(anomalies) / len(column_data) * 100,
                    'rolling_stats': {
                        'window_size': window_size,
                        'threshold': threshold
                    }
                }
            
            return {
                'time_series_anomalies': all_anomalies,
                'summary': self._generate_temporal_summary(all_anomalies),
                'parameters': {
                    'window_size': window_size,
                    'threshold': threshold,
                    'time_column': time_column,
                    'value_columns': value_columns
                }
            }
            
        except Exception as e:
            self.logger.error(f"Error in time series anomaly detection: {str(e)}")
            raise
    
    def _classify_anomaly_severity(self, score: float, method: str) -> str:
        """Classify anomaly severity based on score and method"""
        if method == 'iqr':
            if score > 5.0:
                return 'critical'
            elif score > 3.0:
                return 'high'
            elif score > 1.5:
                return 'medium'
            else:
                return 'low'
        
        elif method in ['z_score', 'modified_z_score', 'time_series']:
            if score > 5.0:
                return 'critical'
            elif score > 4.0:
                return 'high'
            elif score > 3.0:
                return 'medium'
            else:
                return 'low'
        
        elif method == 'multivariate':
            if score > 2.0:
                return 'critical'
            elif score > 1.0:
                return 'high'
            elif score > 0.5:
                return 'medium'
            else:
                return 'low'
        
        else:
            return 'medium'  # Default
    
    def _generate_outlier_recommendations(
        self,
        value: float,
        method: str,
        severity: str
    ) -> List[str]:
        """Generate recommendations for statistical outliers"""
        recommendations = []
        
        if severity in ['critical', 'high']:
            recommendations.append("Investigate data source and measurement accuracy")
            recommendations.append("Check for equipment malfunction or calibration issues")
            recommendations.append("Consider excluding from analysis if confirmed as error")
        elif severity == 'medium':
            recommendations.append("Review measurement conditions")
            recommendations.append("Consider as potential edge case for design")
        else:
            recommendations.append("Monitor for recurring patterns")
        
        recommendations.append(f"Value detected using {method} method")
        
        return recommendations
    
    def _generate_limit_violation_recommendations(
        self,
        parameter: str,
        value: float,
        violation_type: str,
        limit: EngineeringLimits
    ) -> List[str]:
        """Generate recommendations for engineering limit violations"""
        recommendations = []
        
        if violation_type.startswith('critical'):
            recommendations.append("IMMEDIATE ACTION REQUIRED")
            recommendations.append("Stop operations and investigate cause")
            recommendations.append("Review design assumptions and safety factors")
            
            if 'tension' in parameter.lower():
                recommendations.append("Check line condition and connections")
                recommendations.append("Verify load calculations")
            elif 'offset' in parameter.lower():
                recommendations.append("Check positioning system")
                recommendations.append("Review environmental conditions")
            elif 'motion' in parameter.lower() or 'pitch' in parameter.lower() or 'roll' in parameter.lower():
                recommendations.append("Check vessel stability")
                recommendations.append("Review sea state conditions")
        
        else:  # Warning violation
            recommendations.append("Monitor closely for trend")
            recommendations.append("Consider operational adjustments")
            recommendations.append("Plan maintenance if condition persists")
        
        return recommendations
    
    def _generate_multivariate_recommendations(
        self,
        row_values: Dict[str, float],
        method: str
    ) -> List[str]:
        """Generate recommendations for multivariate anomalies"""
        recommendations = [
            "Review combination of parameters simultaneously",
            "Check for systematic measurement errors",
            "Investigate unusual operating conditions",
            f"Anomaly detected using {method} across multiple variables"
        ]
        
        # Find the most extreme values
        most_extreme = max(row_values.items(), key=lambda x: abs(x[1]))
        recommendations.append(f"Focus investigation on {most_extreme[0]} parameter")
        
        return recommendations
    
    def _generate_temporal_recommendations(
        self,
        parameter: str,
        value: float,
        severity: str
    ) -> List[str]:
        """Generate recommendations for temporal anomalies"""
        recommendations = []
        
        if severity in ['critical', 'high']:
            recommendations.append("Investigate sudden change in conditions")
            recommendations.append("Check for equipment failure or malfunction")
            recommendations.append("Review environmental data for corresponding changes")
        else:
            recommendations.append("Monitor for sustained trend changes")
            recommendations.append("Consider normal operational variation")
        
        recommendations.append(f"Temporal anomaly in {parameter} detected")
        
        return recommendations
    
    def _generate_temporal_summary(self, all_anomalies: Dict[str, Any]) -> Dict[str, Any]:
        """Generate summary of temporal anomaly analysis"""
        total_anomalies = sum(
            results['total_anomalies'] 
            for results in all_anomalies.values()
        )
        
        # Find parameter with most anomalies
        most_anomalous_param = max(
            all_anomalies.items(),
            key=lambda x: x[1]['total_anomalies']
        )[0] if all_anomalies else None
        
        return {
            'total_parameters_analyzed': len(all_anomalies),
            'total_anomalies_found': total_anomalies,
            'most_anomalous_parameter': most_anomalous_param,
            'parameters_with_anomalies': [
                param for param, results in all_anomalies.items()
                if results['total_anomalies'] > 0
            ]
        }
    
    def comprehensive_anomaly_scan(
        self,
        data: pd.DataFrame,
        time_column: Optional[str] = None,
        custom_limits: Optional[Dict[str, EngineeringLimits]] = None,
        methods: List[str] = None
    ) -> Dict[str, Any]:
        """
        Perform comprehensive anomaly detection using multiple methods
        
        Args:
            data: Input DataFrame
            time_column: Name of time column for temporal analysis
            custom_limits: Custom engineering limits
            methods: List of detection methods to use
            
        Returns:
            Comprehensive anomaly detection results
        """
        try:
            if methods is None:
                methods = ['iqr', 'z_score', 'engineering_limits', 'multivariate']
            
            results = {
                'timestamp': datetime.now().isoformat(),
                'data_shape': data.shape,
                'methods_used': methods,
                'results': {}
            }
            
            numeric_columns = data.select_dtypes(include=[np.number]).columns.tolist()
            
            # Statistical outlier detection
            if any(method in ['iqr', 'z_score', 'modified_z_score'] for method in methods):
                statistical_results = {}
                
                for column in numeric_columns:
                    column_results = {}
                    
                    if 'iqr' in methods:
                        column_results['iqr'] = self.detect_statistical_outliers(
                            data[column], method='iqr'
                        )
                    
                    if 'z_score' in methods:
                        column_results['z_score'] = self.detect_statistical_outliers(
                            data[column], method='z_score'
                        )
                    
                    if 'modified_z_score' in methods:
                        column_results['modified_z_score'] = self.detect_statistical_outliers(
                            data[column], method='modified_z_score'
                        )
                    
                    statistical_results[column] = column_results
                
                results['results']['statistical_outliers'] = statistical_results
            
            # Engineering limit violations
            if 'engineering_limits' in methods:
                results['results']['engineering_violations'] = self.detect_engineering_limit_violations(
                    data, custom_limits
                )
            
            # Multivariate anomalies
            if 'multivariate' in methods:
                try:
                    results['results']['multivariate'] = self.detect_multivariate_anomalies(data)
                except Exception as e:
                    self.logger.warning(f"Multivariate detection failed: {str(e)}")
                    results['results']['multivariate'] = {'error': str(e)}
            
            # Time series anomalies
            if 'time_series' in methods and time_column:
                try:
                    results['results']['time_series'] = self.detect_time_series_anomalies(
                        data, time_column, numeric_columns
                    )
                except Exception as e:
                    self.logger.warning(f"Time series detection failed: {str(e)}")
                    results['results']['time_series'] = {'error': str(e)}
            
            # Generate comprehensive summary
            results['summary'] = self._generate_comprehensive_summary(results['results'])
            
            return results
            
        except Exception as e:
            self.logger.error(f"Error in comprehensive anomaly scan: {str(e)}")
            raise
    
    def _generate_comprehensive_summary(self, all_results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate comprehensive summary across all detection methods"""
        summary = {
            'total_anomalies': 0,
            'critical_anomalies': 0,
            'high_severity_anomalies': 0,
            'anomaly_types': {},
            'most_problematic_parameters': [],
            'detection_methods_used': []
        }
        
        # Count anomalies from all methods
        for method, results in all_results.items():
            if isinstance(results, dict) and 'error' not in results:
                summary['detection_methods_used'].append(method)
                
                # Statistical outliers
                if method == 'statistical_outliers':
                    for param, param_results in results.items():
                        for sub_method, sub_results in param_results.items():
                            anomalies = sub_results.get('anomalies', [])
                            summary['total_anomalies'] += len(anomalies)
                            
                            for anomaly in anomalies:
                                if anomaly.severity == 'critical':
                                    summary['critical_anomalies'] += 1
                                elif anomaly.severity == 'high':
                                    summary['high_severity_anomalies'] += 1
                
                # Engineering violations
                elif method == 'engineering_violations':
                    violations = results.get('violations', [])
                    summary['total_anomalies'] += len(violations)
                    summary['critical_anomalies'] += results.get('critical_violations', 0)
                
                # Multivariate
                elif method == 'multivariate':
                    anomalies = results.get('anomalies', [])
                    summary['total_anomalies'] += len(anomalies)
                    
                    for anomaly in anomalies:
                        if anomaly.severity in ['critical', 'high']:
                            summary['critical_anomalies'] += 1
                
                # Time series
                elif method == 'time_series':
                    ts_results = results.get('time_series_anomalies', {})
                    for param, param_results in ts_results.items():
                        anomalies = param_results.get('anomalies', [])
                        summary['total_anomalies'] += len(anomalies)
                        
                        for anomaly in anomalies:
                            if anomaly.severity in ['critical', 'high']:
                                summary['critical_anomalies'] += 1
        
        return summary