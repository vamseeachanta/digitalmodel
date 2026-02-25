"""
Statistical Analysis Service for OrcaFlex Results Dashboard

Provides comprehensive statistical analysis capabilities including:
- Correlation analysis
- Regression analysis
- Confidence intervals
- Trend analysis
- Distribution analysis
"""

import numpy as np
import pandas as pd
from scipy import stats
from scipy.stats import pearsonr, spearmanr
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import r2_score
from typing import Dict, List, Tuple, Optional, Any
import logging
from dataclasses import dataclass
from enum import Enum

logger = logging.getLogger(__name__)


class AnalysisType(Enum):
    CORRELATION = "correlation"
    REGRESSION = "regression"
    TREND = "trend"
    DISTRIBUTION = "distribution"
    CONFIDENCE_INTERVAL = "confidence_interval"


@dataclass
class StatisticalResult:
    """Container for statistical analysis results"""
    analysis_type: str
    value: float
    p_value: Optional[float] = None
    confidence_interval: Optional[Tuple[float, float]] = None
    r_squared: Optional[float] = None
    std_error: Optional[float] = None
    metadata: Optional[Dict[str, Any]] = None


@dataclass
class RegressionResult:
    """Container for regression analysis results"""
    slope: float
    intercept: float
    r_squared: float
    p_value: float
    std_error: float
    confidence_intervals: Dict[str, Tuple[float, float]]
    predicted_values: np.ndarray
    residuals: np.ndarray


class StatisticalAnalysisService:
    """Service for performing statistical analysis on OrcaFlex results"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
    
    def calculate_correlation_matrix(
        self,
        data: pd.DataFrame,
        method: str = 'pearson',
        confidence_level: float = 0.95
    ) -> Dict[str, Any]:
        """
        Calculate correlation matrix with statistical significance
        
        Args:
            data: DataFrame with numerical columns
            method: 'pearson' or 'spearman'
            confidence_level: Confidence level for significance testing
            
        Returns:
            Dict containing correlation matrix, p-values, and significance flags
        """
        try:
            # Validate input
            if data.empty:
                raise ValueError("Input data is empty")
            
            numeric_data = data.select_dtypes(include=[np.number])
            if numeric_data.empty:
                raise ValueError("No numeric columns found in data")
            
            n_samples = len(numeric_data)
            columns = numeric_data.columns.tolist()
            
            # Initialize result matrices
            correlation_matrix = np.zeros((len(columns), len(columns)))
            p_value_matrix = np.zeros((len(columns), len(columns)))
            significance_matrix = np.zeros((len(columns), len(columns)), dtype=bool)
            
            # Calculate correlations
            for i, col1 in enumerate(columns):
                for j, col2 in enumerate(columns):
                    if i == j:
                        correlation_matrix[i, j] = 1.0
                        p_value_matrix[i, j] = 0.0
                        significance_matrix[i, j] = True
                    else:
                        if method == 'pearson':
                            corr, p_val = pearsonr(numeric_data[col1], numeric_data[col2])
                        else:
                            corr, p_val = spearmanr(numeric_data[col1], numeric_data[col2])
                        
                        correlation_matrix[i, j] = corr
                        p_value_matrix[i, j] = p_val
                        significance_matrix[i, j] = p_val < (1 - confidence_level)
            
            return {
                'correlation_matrix': correlation_matrix.tolist(),
                'p_value_matrix': p_value_matrix.tolist(),
                'significance_matrix': significance_matrix.tolist(),
                'column_names': columns,
                'method': method,
                'n_samples': n_samples,
                'confidence_level': confidence_level
            }
            
        except Exception as e:
            self.logger.error(f"Error calculating correlation matrix: {str(e)}")
            raise
    
    def perform_linear_regression(
        self,
        x_data: np.ndarray,
        y_data: np.ndarray,
        confidence_level: float = 0.95
    ) -> RegressionResult:
        """
        Perform linear regression with confidence intervals
        
        Mathematical formula: y = mx + b
        Correlation coefficient: r = Σ(xi - x̄)(yi - ȳ) / √[Σ(xi - x̄)²Σ(yi - ȳ)²]
        
        Args:
            x_data: Independent variable data
            y_data: Dependent variable data
            confidence_level: Confidence level for intervals
            
        Returns:
            RegressionResult object with comprehensive analysis
        """
        try:
            # Validate input
            if len(x_data) != len(y_data):
                raise ValueError("X and Y data must have the same length")
            
            if len(x_data) < 3:
                raise ValueError("Need at least 3 data points for regression")
            
            # Remove NaN values
            valid_indices = ~(np.isnan(x_data) | np.isnan(y_data))
            x_clean = x_data[valid_indices]
            y_clean = y_data[valid_indices]
            
            # Perform regression
            model = LinearRegression()
            X_reshaped = x_clean.reshape(-1, 1)
            model.fit(X_reshaped, y_clean)
            
            slope = model.coef_[0]
            intercept = model.intercept_
            y_pred = model.predict(X_reshaped)
            r_squared = r2_score(y_clean, y_pred)
            
            # Calculate statistics
            n = len(x_clean)
            residuals = y_clean - y_pred
            mse = np.mean(residuals ** 2)
            std_error = np.sqrt(mse)
            
            # Calculate t-statistic and p-value
            x_mean = np.mean(x_clean)
            ss_xx = np.sum((x_clean - x_mean) ** 2)
            slope_se = std_error / np.sqrt(ss_xx)
            t_stat = slope / slope_se
            p_value = 2 * (1 - stats.t.cdf(abs(t_stat), n - 2))
            
            # Calculate confidence intervals
            alpha = 1 - confidence_level
            t_critical = stats.t.ppf(1 - alpha/2, n - 2)
            
            slope_ci = (
                slope - t_critical * slope_se,
                slope + t_critical * slope_se
            )
            
            intercept_se = std_error * np.sqrt(1/n + x_mean**2/ss_xx)
            intercept_ci = (
                intercept - t_critical * intercept_se,
                intercept + t_critical * intercept_se
            )
            
            return RegressionResult(
                slope=slope,
                intercept=intercept,
                r_squared=r_squared,
                p_value=p_value,
                std_error=std_error,
                confidence_intervals={
                    'slope': slope_ci,
                    'intercept': intercept_ci
                },
                predicted_values=y_pred,
                residuals=residuals
            )
            
        except Exception as e:
            self.logger.error(f"Error performing linear regression: {str(e)}")
            raise
    
    def calculate_confidence_interval(
        self,
        data: np.ndarray,
        confidence_level: float = 0.95
    ) -> Dict[str, Any]:
        """
        Calculate confidence interval for mean
        
        Mathematical formula: CI = x̄ ± z(σ/√n)
        
        Args:
            data: Numerical data array
            confidence_level: Confidence level (default 0.95)
            
        Returns:
            Dict with confidence interval bounds and statistics
        """
        try:
            # Remove NaN values
            clean_data = data[~np.isnan(data)]
            
            if len(clean_data) == 0:
                raise ValueError("No valid data points found")
            
            n = len(clean_data)
            mean = np.mean(clean_data)
            std_dev = np.std(clean_data, ddof=1)  # Sample standard deviation
            std_error = std_dev / np.sqrt(n)
            
            # Use t-distribution for small samples or when population std is unknown
            alpha = 1 - confidence_level
            if n > 30:
                # Normal distribution approximation
                z_critical = stats.norm.ppf(1 - alpha/2)
                margin_error = z_critical * std_error
            else:
                # t-distribution
                t_critical = stats.t.ppf(1 - alpha/2, n - 1)
                margin_error = t_critical * std_error
            
            lower_bound = mean - margin_error
            upper_bound = mean + margin_error
            
            return {
                'mean': mean,
                'std_dev': std_dev,
                'std_error': std_error,
                'confidence_level': confidence_level,
                'lower_bound': lower_bound,
                'upper_bound': upper_bound,
                'margin_error': margin_error,
                'n_samples': n
            }
            
        except Exception as e:
            self.logger.error(f"Error calculating confidence interval: {str(e)}")
            raise
    
    def perform_trend_analysis(
        self,
        time_series_data: pd.DataFrame,
        time_column: str,
        value_column: str,
        trend_method: str = 'linear'
    ) -> Dict[str, Any]:
        """
        Perform trend analysis on time series data
        
        Args:
            time_series_data: DataFrame with time and value columns
            time_column: Name of time column
            value_column: Name of value column
            trend_method: 'linear', 'polynomial', or 'moving_average'
            
        Returns:
            Dict with trend analysis results
        """
        try:
            # Validate input
            if time_column not in time_series_data.columns:
                raise ValueError(f"Time column '{time_column}' not found")
            
            if value_column not in time_series_data.columns:
                raise ValueError(f"Value column '{value_column}' not found")
            
            # Clean data
            clean_data = time_series_data.dropna(subset=[time_column, value_column])
            
            if len(clean_data) < 3:
                raise ValueError("Need at least 3 data points for trend analysis")
            
            # Convert time to numeric if needed
            if pd.api.types.is_datetime64_any_dtype(clean_data[time_column]):
                time_numeric = pd.to_numeric(clean_data[time_column])
            else:
                time_numeric = clean_data[time_column]
            
            values = clean_data[value_column].values
            
            if trend_method == 'linear':
                # Linear trend
                regression_result = self.perform_linear_regression(
                    time_numeric.values, values
                )
                
                trend_strength = abs(regression_result.slope)
                trend_direction = 'increasing' if regression_result.slope > 0 else 'decreasing'
                
                result = {
                    'method': 'linear',
                    'slope': regression_result.slope,
                    'intercept': regression_result.intercept,
                    'r_squared': regression_result.r_squared,
                    'p_value': regression_result.p_value,
                    'trend_strength': trend_strength,
                    'trend_direction': trend_direction,
                    'predicted_values': regression_result.predicted_values.tolist(),
                    'residuals': regression_result.residuals.tolist()
                }
                
            elif trend_method == 'moving_average':
                # Moving average trend
                window_size = min(10, len(values) // 3)
                moving_avg = clean_data[value_column].rolling(
                    window=window_size, center=True
                ).mean()
                
                # Calculate trend from moving average
                ma_regression = self.perform_linear_regression(
                    time_numeric.values[~moving_avg.isna()],
                    moving_avg.dropna().values
                )
                
                result = {
                    'method': 'moving_average',
                    'window_size': window_size,
                    'slope': ma_regression.slope,
                    'r_squared': ma_regression.r_squared,
                    'moving_average': moving_avg.tolist(),
                    'trend_direction': 'increasing' if ma_regression.slope > 0 else 'decreasing'
                }
            
            else:
                raise ValueError(f"Unknown trend method: {trend_method}")
            
            return result
            
        except Exception as e:
            self.logger.error(f"Error performing trend analysis: {str(e)}")
            raise
    
    def analyze_distribution(
        self,
        data: np.ndarray,
        distribution_tests: List[str] = None
    ) -> Dict[str, Any]:
        """
        Analyze data distribution and test normality
        
        Args:
            data: Numerical data array
            distribution_tests: List of tests to perform
            
        Returns:
            Dict with distribution analysis results
        """
        try:
            if distribution_tests is None:
                distribution_tests = ['shapiro', 'kstest', 'jarque_bera']
            
            # Remove NaN values
            clean_data = data[~np.isnan(data)]
            
            if len(clean_data) == 0:
                raise ValueError("No valid data points found")
            
            # Basic statistics
            basic_stats = {
                'mean': np.mean(clean_data),
                'median': np.median(clean_data),
                'std_dev': np.std(clean_data, ddof=1),
                'variance': np.var(clean_data, ddof=1),
                'skewness': stats.skew(clean_data),
                'kurtosis': stats.kurtosis(clean_data),
                'min': np.min(clean_data),
                'max': np.max(clean_data),
                'q25': np.percentile(clean_data, 25),
                'q75': np.percentile(clean_data, 75),
                'n_samples': len(clean_data)
            }
            
            # Distribution tests
            test_results = {}
            
            if 'shapiro' in distribution_tests and len(clean_data) <= 5000:
                stat, p_val = stats.shapiro(clean_data)
                test_results['shapiro'] = {
                    'statistic': stat,
                    'p_value': p_val,
                    'is_normal': p_val > 0.05
                }
            
            if 'kstest' in distribution_tests:
                stat, p_val = stats.kstest(clean_data, 'norm', 
                                         args=(basic_stats['mean'], basic_stats['std_dev']))
                test_results['kolmogorov_smirnov'] = {
                    'statistic': stat,
                    'p_value': p_val,
                    'is_normal': p_val > 0.05
                }
            
            if 'jarque_bera' in distribution_tests:
                stat, p_val = stats.jarque_bera(clean_data)
                test_results['jarque_bera'] = {
                    'statistic': stat,
                    'p_value': p_val,
                    'is_normal': p_val > 0.05
                }
            
            return {
                'basic_statistics': basic_stats,
                'normality_tests': test_results,
                'recommended_analysis': self._recommend_analysis_method(test_results)
            }
            
        except Exception as e:
            self.logger.error(f"Error analyzing distribution: {str(e)}")
            raise
    
    def _recommend_analysis_method(self, test_results: Dict[str, Any]) -> str:
        """
        Recommend analysis method based on distribution tests
        
        Args:
            test_results: Results from normality tests
            
        Returns:
            Recommended analysis method
        """
        if not test_results:
            return "parametric"  # Default recommendation
        
        # Count how many tests suggest normal distribution
        normal_count = sum(
            1 for test in test_results.values() 
            if test.get('is_normal', False)
        )
        
        total_tests = len(test_results)
        
        if normal_count >= total_tests * 0.5:
            return "parametric"
        else:
            return "non_parametric"
    
    def generate_statistical_report(
        self,
        analysis_results: Dict[str, Any],
        title: str = "Statistical Analysis Report"
    ) -> Dict[str, Any]:
        """
        Generate comprehensive statistical report
        
        Args:
            analysis_results: Dictionary of analysis results
            title: Report title
            
        Returns:
            Formatted statistical report
        """
        try:
            report = {
                'title': title,
                'timestamp': pd.Timestamp.now().isoformat(),
                'summary': {},
                'detailed_results': analysis_results,
                'recommendations': []
            }
            
            # Generate summary
            if 'correlation_matrix' in analysis_results:
                corr_data = analysis_results['correlation_matrix']
                max_corr = np.max(np.abs(np.array(corr_data)))
                report['summary']['max_correlation'] = max_corr
            
            if 'regression' in analysis_results:
                reg_data = analysis_results['regression']
                report['summary']['r_squared'] = reg_data.get('r_squared', 0)
                report['summary']['trend_direction'] = (
                    'positive' if reg_data.get('slope', 0) > 0 else 'negative'
                )
            
            # Generate recommendations
            if 'distribution' in analysis_results:
                dist_data = analysis_results['distribution']
                recommended_method = dist_data.get('recommended_analysis', 'parametric')
                report['recommendations'].append(
                    f"Use {recommended_method} statistical methods based on distribution analysis"
                )
            
            return report
            
        except Exception as e:
            self.logger.error(f"Error generating statistical report: {str(e)}")
            raise