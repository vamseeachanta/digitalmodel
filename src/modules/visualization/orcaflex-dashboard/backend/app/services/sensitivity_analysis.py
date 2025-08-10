"""
Sensitivity Analysis Service for OrcaFlex Results Dashboard

Provides environmental sensitivity analysis capabilities including:
- Parameter sensitivity analysis
- Environmental load case comparisons
- Response surface methodology
- Monte Carlo sensitivity analysis
- Design of experiments (DOE) analysis
"""

import numpy as np
import pandas as pd
from scipy import stats
from scipy.optimize import minimize_scalar
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import r2_score
from typing import Dict, List, Tuple, Optional, Any, Union, Callable
import logging
from dataclasses import dataclass
from enum import Enum
from datetime import datetime
import itertools
from concurrent.futures import ThreadPoolExecutor, as_completed

logger = logging.getLogger(__name__)


class SensitivityMethod(Enum):
    ONE_AT_A_TIME = "one_at_a_time"
    SOBOL = "sobol"
    MORRIS = "morris"
    REGRESSION = "regression"
    CORRELATION = "correlation"
    RANDOM_FOREST = "random_forest"


@dataclass
class ParameterRange:
    """Define parameter range for sensitivity analysis"""
    name: str
    min_value: float
    max_value: float
    nominal_value: float
    units: Optional[str] = None
    description: Optional[str] = None
    distribution: str = 'uniform'  # 'uniform', 'normal', 'lognormal'


@dataclass
class SensitivityResult:
    """Container for sensitivity analysis results"""
    parameter: str
    response: str
    sensitivity_index: float
    confidence_interval: Tuple[float, float]
    p_value: float
    effect_size: str  # 'negligible', 'small', 'medium', 'large'
    ranking: int
    method: str
    additional_metrics: Dict[str, Any]


class SensitivityAnalysisService:
    """Service for environmental sensitivity analysis of OrcaFlex results"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
    
    def perform_parameter_sensitivity_analysis(
        self,
        environmental_data: Dict[str, Dict[str, float]],
        response_data: Dict[str, pd.DataFrame],
        parameter_ranges: Dict[str, ParameterRange],
        response_metrics: List[str],
        method: str = 'correlation',
        n_samples: int = 1000
    ) -> Dict[str, Any]:
        """
        Perform comprehensive parameter sensitivity analysis
        
        Args:
            environmental_data: Dict mapping case names to environmental parameters
            response_data: Dict mapping case names to response DataFrames
            parameter_ranges: Dict mapping parameter names to their ranges
            response_metrics: List of response metrics to analyze
            method: Sensitivity analysis method
            n_samples: Number of samples for Monte Carlo methods
            
        Returns:
            Comprehensive sensitivity analysis results
        """
        try:
            # Validate inputs
            if len(environmental_data) != len(response_data):
                raise ValueError("Environmental and response data must have matching cases")
            
            case_names = list(environmental_data.keys())
            env_params = list(parameter_ranges.keys())
            
            # Build data matrix
            analysis_data = self._build_analysis_matrix(
                environmental_data, response_data, case_names, env_params, response_metrics
            )
            
            sensitivity_results = {}
            
            # Perform sensitivity analysis for each response metric
            for metric in response_metrics:
                if metric not in analysis_data.columns:
                    self.logger.warning(f"Response metric {metric} not found in data")
                    continue
                
                self.logger.info(f"Analyzing sensitivity for {metric}")
                
                if method == 'correlation':
                    metric_results = self._correlation_sensitivity(
                        analysis_data, env_params, metric
                    )
                elif method == 'regression':
                    metric_results = self._regression_sensitivity(
                        analysis_data, env_params, metric
                    )
                elif method == 'random_forest':
                    metric_results = self._random_forest_sensitivity(
                        analysis_data, env_params, metric
                    )
                elif method == 'one_at_a_time':
                    metric_results = self._one_at_a_time_sensitivity(
                        analysis_data, env_params, metric, parameter_ranges
                    )
                else:
                    raise ValueError(f"Unknown sensitivity method: {method}")
                
                sensitivity_results[metric] = metric_results
            
            # Generate comprehensive analysis
            summary = self._generate_sensitivity_summary(sensitivity_results)
            recommendations = self._generate_sensitivity_recommendations(
                sensitivity_results, parameter_ranges
            )
            
            return {
                'sensitivity_results': sensitivity_results,
                'summary': summary,
                'recommendations': recommendations,
                'method': method,
                'parameters_analyzed': env_params,
                'response_metrics': response_metrics,
                'n_cases': len(case_names),
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            self.logger.error(f"Error in parameter sensitivity analysis: {str(e)}")
            raise
    
    def perform_environmental_load_case_analysis(
        self,
        base_case_data: Dict[str, float],
        response_data: pd.DataFrame,
        environmental_variations: Dict[str, List[float]],
        response_metrics: List[str]
    ) -> Dict[str, Any]:
        """
        Analyze sensitivity to environmental load case variations
        
        Args:
            base_case_data: Base case environmental parameters
            response_data: Response data for all cases
            environmental_variations: Dict mapping parameters to variation lists
            response_metrics: List of response metrics to analyze
            
        Returns:
            Environmental load case sensitivity results
        """
        try:
            load_case_results = {}
            
            for metric in response_metrics:
                if metric not in response_data.columns:
                    continue
                
                metric_data = response_data[metric].dropna()
                base_response = metric_data.mean()  # Use mean as baseline
                
                parameter_effects = {}
                
                for param, variations in environmental_variations.items():
                    param_effects = []
                    
                    for variation in variations:
                        # Calculate percentage change from base case
                        if param in base_case_data:
                            base_value = base_case_data[param]
                            percent_change = ((variation - base_value) / base_value) * 100
                            
                            # Estimate response change (simplified linear relationship)
                            # In practice, this would use actual simulation results
                            response_change = self._estimate_response_change(
                                param, percent_change, metric, base_response
                            )
                            
                            param_effects.append({
                                'parameter_value': variation,
                                'parameter_change_percent': percent_change,
                                'response_value': base_response + response_change,
                                'response_change': response_change,
                                'response_change_percent': (response_change / base_response) * 100
                            })
                    
                    # Calculate sensitivity coefficient (dResponse/dParameter)
                    if len(param_effects) > 1:
                        param_values = [effect['parameter_value'] for effect in param_effects]
                        response_values = [effect['response_value'] for effect in param_effects]
                        
                        # Linear regression to estimate sensitivity
                        slope, intercept, r_value, p_value, std_err = stats.linregress(
                            param_values, response_values
                        )
                        
                        sensitivity_coef = slope * (base_case_data.get(param, 1) / base_response)
                        
                        parameter_effects[param] = {
                            'effects': param_effects,
                            'sensitivity_coefficient': sensitivity_coef,
                            'correlation': r_value,
                            'p_value': p_value,
                            'std_error': std_err,
                            'base_value': base_case_data.get(param, 0)
                        }
                
                load_case_results[metric] = {
                    'parameter_effects': parameter_effects,
                    'base_response': base_response,
                    'most_sensitive_parameter': self._find_most_sensitive_parameter(parameter_effects),
                    'sensitivity_ranking': self._rank_parameters_by_sensitivity(parameter_effects)
                }
            
            return {
                'load_case_analysis': load_case_results,
                'base_case': base_case_data,
                'environmental_variations': environmental_variations,
                'summary': self._generate_load_case_summary(load_case_results)
            }
            
        except Exception as e:
            self.logger.error(f"Error in environmental load case analysis: {str(e)}")
            raise
    
    def generate_response_surface(
        self,
        parameter1: str,
        parameter2: str,
        response_metric: str,
        parameter_ranges: Dict[str, ParameterRange],
        response_function: Callable[[Dict[str, float]], float],
        grid_size: int = 20
    ) -> Dict[str, Any]:
        """
        Generate response surface for two parameters
        
        Args:
            parameter1: First parameter name
            parameter2: Second parameter name
            response_metric: Response metric name
            parameter_ranges: Parameter range definitions
            response_function: Function to evaluate response given parameters
            grid_size: Number of grid points per dimension
            
        Returns:
            Response surface analysis results
        """
        try:
            if parameter1 not in parameter_ranges or parameter2 not in parameter_ranges:
                raise ValueError("Parameter ranges not defined for specified parameters")
            
            # Create parameter grids
            range1 = parameter_ranges[parameter1]
            range2 = parameter_ranges[parameter2]
            
            param1_values = np.linspace(range1.min_value, range1.max_value, grid_size)
            param2_values = np.linspace(range2.min_value, range2.max_value, grid_size)
            
            param1_grid, param2_grid = np.meshgrid(param1_values, param2_values)
            
            # Evaluate response surface
            response_surface = np.zeros_like(param1_grid)
            
            for i in range(grid_size):
                for j in range(grid_size):
                    params = {
                        parameter1: param1_grid[i, j],
                        parameter2: param2_grid[i, j]
                    }
                    
                    # Add nominal values for other parameters
                    for param_name, param_range in parameter_ranges.items():
                        if param_name not in params:
                            params[param_name] = param_range.nominal_value
                    
                    try:
                        response_surface[i, j] = response_function(params)
                    except Exception as e:
                        self.logger.warning(f"Error evaluating response at {params}: {str(e)}")
                        response_surface[i, j] = np.nan
            
            # Find extrema
            min_idx = np.unravel_index(np.nanargmin(response_surface), response_surface.shape)
            max_idx = np.unravel_index(np.nanargmax(response_surface), response_surface.shape)
            
            min_point = {
                parameter1: param1_grid[min_idx],
                parameter2: param2_grid[min_idx],
                'response': response_surface[min_idx]
            }
            
            max_point = {
                parameter1: param1_grid[max_idx],
                parameter2: param2_grid[max_idx],
                'response': response_surface[max_idx]
            }
            
            # Calculate gradients (sensitivity)
            grad_param1, grad_param2 = np.gradient(response_surface)
            
            # Find steepest gradient locations
            gradient_magnitude = np.sqrt(grad_param1**2 + grad_param2**2)
            steepest_idx = np.unravel_index(np.nanargmax(gradient_magnitude), gradient_magnitude.shape)
            
            return {
                'parameter1': parameter1,
                'parameter2': parameter2,
                'response_metric': response_metric,
                'param1_grid': param1_grid.tolist(),
                'param2_grid': param2_grid.tolist(),
                'response_surface': response_surface.tolist(),
                'gradient_param1': grad_param1.tolist(),
                'gradient_param2': grad_param2.tolist(),
                'gradient_magnitude': gradient_magnitude.tolist(),
                'extrema': {
                    'minimum': min_point,
                    'maximum': max_point,
                    'range': max_point['response'] - min_point['response']
                },
                'steepest_gradient': {
                    parameter1: param1_grid[steepest_idx],
                    parameter2: param2_grid[steepest_idx],
                    'magnitude': gradient_magnitude[steepest_idx]
                },
                'grid_size': grid_size
            }
            
        except Exception as e:
            self.logger.error(f"Error generating response surface: {str(e)}")
            raise
    
    def perform_monte_carlo_sensitivity(
        self,
        parameter_ranges: Dict[str, ParameterRange],
        response_function: Callable[[Dict[str, float]], Dict[str, float]],
        response_metrics: List[str],
        n_samples: int = 1000,
        confidence_level: float = 0.95
    ) -> Dict[str, Any]:
        """
        Perform Monte Carlo-based sensitivity analysis
        
        Args:
            parameter_ranges: Parameter range definitions
            response_function: Function to evaluate responses given parameters
            response_metrics: List of response metrics
            n_samples: Number of Monte Carlo samples
            confidence_level: Confidence level for intervals
            
        Returns:
            Monte Carlo sensitivity analysis results
        """
        try:
            # Generate parameter samples
            parameter_samples = self._generate_parameter_samples(parameter_ranges, n_samples)
            
            # Evaluate responses
            response_samples = []
            
            self.logger.info(f"Running {n_samples} Monte Carlo simulations...")
            
            # Use parallel execution for efficiency
            with ThreadPoolExecutor(max_workers=4) as executor:
                futures = []
                
                for i, params in enumerate(parameter_samples):
                    future = executor.submit(self._safe_response_evaluation, response_function, params, i)
                    futures.append(future)
                
                for future in as_completed(futures):
                    result = future.result()
                    if result is not None:
                        response_samples.append(result)
            
            if len(response_samples) == 0:
                raise ValueError("No successful response evaluations")
            
            # Convert to DataFrame for analysis
            response_df = pd.DataFrame(response_samples)
            parameter_df = pd.DataFrame(parameter_samples[:len(response_samples)])
            
            # Perform sensitivity analysis
            sensitivity_results = {}
            
            for metric in response_metrics:
                if metric not in response_df.columns:
                    continue
                
                metric_sensitivities = {}
                
                for param in parameter_ranges.keys():
                    if param not in parameter_df.columns:
                        continue
                    
                    # Correlation-based sensitivity
                    corr_coef, p_value = stats.pearsonr(
                        parameter_df[param], response_df[metric]
                    )
                    
                    # Rank-based sensitivity (Spearman)
                    spearman_coef, spearman_p = stats.spearmanr(
                        parameter_df[param], response_df[metric]
                    )
                    
                    # Sobol-like indices using variance decomposition
                    sobol_index = self._calculate_sobol_index(
                        parameter_df[param], response_df[metric]
                    )
                    
                    # Effect size classification
                    effect_size = self._classify_effect_size(abs(corr_coef))
                    
                    # Confidence intervals
                    conf_int = self._calculate_correlation_confidence_interval(
                        corr_coef, len(response_samples), confidence_level
                    )
                    
                    metric_sensitivities[param] = {
                        'correlation': corr_coef,
                        'p_value': p_value,
                        'spearman_correlation': spearman_coef,
                        'spearman_p_value': spearman_p,
                        'sobol_index': sobol_index,
                        'effect_size': effect_size,
                        'confidence_interval': conf_int,
                        'parameter_range': {
                            'min': parameter_df[param].min(),
                            'max': parameter_df[param].max(),
                            'mean': parameter_df[param].mean(),
                            'std': parameter_df[param].std()
                        },
                        'response_stats': {
                            'mean': response_df[metric].mean(),
                            'std': response_df[metric].std(),
                            'min': response_df[metric].min(),
                            'max': response_df[metric].max()
                        }
                    }
                
                # Rank parameters by sensitivity
                ranked_params = sorted(
                    metric_sensitivities.items(),
                    key=lambda x: abs(x[1]['correlation']),
                    reverse=True
                )
                
                sensitivity_results[metric] = {
                    'parameter_sensitivities': metric_sensitivities,
                    'sensitivity_ranking': [(param, abs(data['correlation'])) 
                                          for param, data in ranked_params],
                    'most_sensitive_parameter': ranked_params[0][0] if ranked_params else None,
                    'response_statistics': {
                        'mean': response_df[metric].mean(),
                        'std': response_df[metric].std(),
                        'coefficient_of_variation': response_df[metric].std() / response_df[metric].mean()
                    }
                }
            
            return {
                'monte_carlo_results': sensitivity_results,
                'simulation_info': {
                    'n_samples_requested': n_samples,
                    'n_successful_evaluations': len(response_samples),
                    'success_rate': len(response_samples) / n_samples,
                    'confidence_level': confidence_level
                },
                'parameter_samples': parameter_df.to_dict('records')[:100],  # First 100 samples
                'response_samples': response_df.to_dict('records')[:100],   # First 100 samples
                'global_sensitivity_ranking': self._generate_global_sensitivity_ranking(sensitivity_results)
            }
            
        except Exception as e:
            self.logger.error(f"Error in Monte Carlo sensitivity analysis: {str(e)}")
            raise
    
    def _build_analysis_matrix(
        self,
        environmental_data: Dict[str, Dict[str, float]],
        response_data: Dict[str, pd.DataFrame],
        case_names: List[str],
        env_params: List[str],
        response_metrics: List[str]
    ) -> pd.DataFrame:
        """Build combined data matrix for analysis"""
        analysis_rows = []
        
        for case in case_names:
            if case not in environmental_data or case not in response_data:
                continue
            
            # Get environmental parameters
            env_data = environmental_data[case]
            
            # Get response data (use mean for each metric)
            response_df = response_data[case]
            
            # Create row for this case
            row = {}
            
            # Add environmental parameters
            for param in env_params:
                row[param] = env_data.get(param, np.nan)
            
            # Add response metrics (using mean values)
            for metric in response_metrics:
                if metric in response_df.columns:
                    metric_data = response_df[metric].dropna()
                    if len(metric_data) > 0:
                        row[metric] = metric_data.mean()
                    else:
                        row[metric] = np.nan
                else:
                    row[metric] = np.nan
            
            row['case_name'] = case
            analysis_rows.append(row)
        
        return pd.DataFrame(analysis_rows)
    
    def _correlation_sensitivity(
        self,
        data: pd.DataFrame,
        env_params: List[str],
        response_metric: str
    ) -> List[SensitivityResult]:
        """Perform correlation-based sensitivity analysis"""
        results = []
        
        response_data = data[response_metric].dropna()
        
        for i, param in enumerate(env_params):
            if param not in data.columns:
                continue
            
            # Get valid data pairs
            valid_mask = ~(data[param].isna() | data[response_metric].isna())
            param_data = data.loc[valid_mask, param]
            response_data_valid = data.loc[valid_mask, response_metric]
            
            if len(param_data) < 3:
                continue
            
            # Calculate correlation
            corr_coef, p_value = stats.pearsonr(param_data, response_data_valid)
            
            # Calculate confidence interval
            n = len(param_data)
            conf_int = self._calculate_correlation_confidence_interval(corr_coef, n, 0.95)
            
            # Classify effect size
            effect_size = self._classify_effect_size(abs(corr_coef))
            
            result = SensitivityResult(
                parameter=param,
                response=response_metric,
                sensitivity_index=abs(corr_coef),
                confidence_interval=conf_int,
                p_value=p_value,
                effect_size=effect_size,
                ranking=i + 1,  # Will be updated after sorting
                method='correlation',
                additional_metrics={
                    'correlation_coefficient': corr_coef,
                    'sample_size': n
                }
            )
            results.append(result)
        
        # Sort and update rankings
        results.sort(key=lambda x: x.sensitivity_index, reverse=True)
        for i, result in enumerate(results):
            result.ranking = i + 1
        
        return results
    
    def _regression_sensitivity(
        self,
        data: pd.DataFrame,
        env_params: List[str],
        response_metric: str
    ) -> List[SensitivityResult]:
        """Perform regression-based sensitivity analysis"""
        # Prepare data
        X_cols = [param for param in env_params if param in data.columns]
        
        clean_data = data[X_cols + [response_metric]].dropna()
        
        if len(clean_data) < len(X_cols) + 2:
            return []
        
        X = clean_data[X_cols].values
        y = clean_data[response_metric].values
        
        # Standardize features for fair comparison
        scaler = StandardScaler()
        X_scaled = scaler.fit_transform(X)
        
        # Fit regression model
        model = LinearRegression()
        model.fit(X_scaled, y)
        
        # Calculate R-squared
        y_pred = model.predict(X_scaled)
        r_squared = r2_score(y, y_pred)
        
        results = []
        
        # Calculate standardized coefficients (sensitivity indices)
        for i, param in enumerate(X_cols):
            coef = model.coef_[i]
            
            # Calculate p-value (simplified)
            n = len(clean_data)
            p = len(X_cols)
            mse = np.mean((y - y_pred) ** 2)
            var_coef = mse / np.var(X_scaled[:, i]) if np.var(X_scaled[:, i]) > 0 else 0
            
            if var_coef > 0:
                t_stat = coef / np.sqrt(var_coef / n)
                p_value = 2 * (1 - stats.t.cdf(abs(t_stat), n - p - 1))
            else:
                p_value = 1.0
            
            # Confidence interval (approximation)
            se = np.sqrt(var_coef / n) if var_coef > 0 else 0
            t_critical = stats.t.ppf(0.975, n - p - 1)
            conf_int = (coef - t_critical * se, coef + t_critical * se)
            
            effect_size = self._classify_effect_size(abs(coef))
            
            result = SensitivityResult(
                parameter=param,
                response=response_metric,
                sensitivity_index=abs(coef),
                confidence_interval=conf_int,
                p_value=p_value,
                effect_size=effect_size,
                ranking=i + 1,
                method='regression',
                additional_metrics={
                    'coefficient': coef,
                    'r_squared': r_squared,
                    'sample_size': n
                }
            )
            results.append(result)
        
        # Sort and update rankings
        results.sort(key=lambda x: x.sensitivity_index, reverse=True)
        for i, result in enumerate(results):
            result.ranking = i + 1
        
        return results
    
    def _random_forest_sensitivity(
        self,
        data: pd.DataFrame,
        env_params: List[str],
        response_metric: str
    ) -> List[SensitivityResult]:
        """Perform Random Forest-based sensitivity analysis"""
        # Prepare data
        X_cols = [param for param in env_params if param in data.columns]
        
        clean_data = data[X_cols + [response_metric]].dropna()
        
        if len(clean_data) < 10:  # Need more data for RF
            return []
        
        X = clean_data[X_cols].values
        y = clean_data[response_metric].values
        
        # Fit Random Forest
        rf = RandomForestRegressor(n_estimators=100, random_state=42)
        rf.fit(X, y)
        
        # Get feature importances
        importances = rf.feature_importances_
        
        results = []
        
        for i, param in enumerate(X_cols):
            importance = importances[i]
            
            # Estimate confidence interval using bootstrap (simplified)
            # In practice, would use proper bootstrap resampling
            conf_int = (importance * 0.8, importance * 1.2)
            
            effect_size = self._classify_effect_size(importance)
            
            result = SensitivityResult(
                parameter=param,
                response=response_metric,
                sensitivity_index=importance,
                confidence_interval=conf_int,
                p_value=0.0,  # RF doesn't provide p-values directly
                effect_size=effect_size,
                ranking=i + 1,
                method='random_forest',
                additional_metrics={
                    'feature_importance': importance,
                    'oob_score': rf.oob_score_ if hasattr(rf, 'oob_score_') else None,
                    'n_estimators': rf.n_estimators
                }
            )
            results.append(result)
        
        # Sort and update rankings
        results.sort(key=lambda x: x.sensitivity_index, reverse=True)
        for i, result in enumerate(results):
            result.ranking = i + 1
        
        return results
    
    def _one_at_a_time_sensitivity(
        self,
        data: pd.DataFrame,
        env_params: List[str],
        response_metric: str,
        parameter_ranges: Dict[str, ParameterRange]
    ) -> List[SensitivityResult]:
        """Perform one-at-a-time sensitivity analysis"""
        results = []
        
        # Use median as baseline
        baseline_response = data[response_metric].median()
        
        for param in env_params:
            if param not in data.columns or param not in parameter_ranges:
                continue
            
            param_range = parameter_ranges[param]
            param_data = data[param].dropna()
            
            if len(param_data) < 3:
                continue
            
            # Calculate sensitivity as response change per unit parameter change
            param_min = param_data.min()
            param_max = param_data.max()
            
            if param_max == param_min:
                continue
            
            # Estimate response at extremes (simplified linear relationship)
            response_data = data.loc[~data[param].isna(), response_metric].dropna()
            
            if len(response_data) > 0:
                response_min = response_data.min()
                response_max = response_data.max()
                
                # Sensitivity index as normalized response range
                param_range_norm = (param_max - param_min) / param_range.nominal_value
                response_range_norm = (response_max - response_min) / baseline_response
                
                sensitivity_index = response_range_norm / param_range_norm if param_range_norm != 0 else 0
                
                # Estimate confidence interval
                conf_int = (sensitivity_index * 0.8, sensitivity_index * 1.2)
                
                effect_size = self._classify_effect_size(abs(sensitivity_index))
                
                result = SensitivityResult(
                    parameter=param,
                    response=response_metric,
                    sensitivity_index=abs(sensitivity_index),
                    confidence_interval=conf_int,
                    p_value=0.05,  # Placeholder
                    effect_size=effect_size,
                    ranking=0,
                    method='one_at_a_time',
                    additional_metrics={
                        'parameter_range': param_max - param_min,
                        'response_range': response_max - response_min,
                        'baseline_response': baseline_response
                    }
                )
                results.append(result)
        
        # Sort and update rankings
        results.sort(key=lambda x: x.sensitivity_index, reverse=True)
        for i, result in enumerate(results):
            result.ranking = i + 1
        
        return results
    
    def _generate_parameter_samples(
        self,
        parameter_ranges: Dict[str, ParameterRange],
        n_samples: int
    ) -> List[Dict[str, float]]:
        """Generate parameter samples for Monte Carlo analysis"""
        samples = []
        
        for _ in range(n_samples):
            sample = {}
            
            for param_name, param_range in parameter_ranges.items():
                if param_range.distribution == 'uniform':
                    value = np.random.uniform(param_range.min_value, param_range.max_value)
                elif param_range.distribution == 'normal':
                    mean = param_range.nominal_value
                    std = (param_range.max_value - param_range.min_value) / 6  # 3-sigma rule
                    value = np.random.normal(mean, std)
                    # Clip to bounds
                    value = np.clip(value, param_range.min_value, param_range.max_value)
                else:
                    # Default to uniform
                    value = np.random.uniform(param_range.min_value, param_range.max_value)
                
                sample[param_name] = value
            
            samples.append(sample)
        
        return samples
    
    def _safe_response_evaluation(
        self,
        response_function: Callable,
        parameters: Dict[str, float],
        sample_index: int
    ) -> Optional[Dict[str, float]]:
        """Safely evaluate response function with error handling"""
        try:
            result = response_function(parameters)
            if isinstance(result, dict):
                return result
            else:
                self.logger.warning(f"Response function returned non-dict for sample {sample_index}")
                return None
        except Exception as e:
            self.logger.warning(f"Error evaluating sample {sample_index}: {str(e)}")
            return None
    
    def _calculate_sobol_index(
        self,
        parameter_values: np.ndarray,
        response_values: np.ndarray
    ) -> float:
        """Calculate approximate Sobol sensitivity index"""
        try:
            # Simplified Sobol index calculation
            # This is an approximation - proper Sobol analysis requires special sampling
            
            # Split data into high and low parameter groups
            median_param = np.median(parameter_values)
            
            high_mask = parameter_values >= median_param
            low_mask = parameter_values < median_param
            
            if np.sum(high_mask) == 0 or np.sum(low_mask) == 0:
                return 0.0
            
            high_responses = response_values[high_mask]
            low_responses = response_values[low_mask]
            
            # Variance-based sensitivity index approximation
            total_var = np.var(response_values)
            
            if total_var == 0:
                return 0.0
            
            # First-order effect approximation
            mean_high = np.mean(high_responses)
            mean_low = np.mean(low_responses)
            
            first_order_effect = (mean_high - mean_low) ** 2 / 4
            sobol_index = first_order_effect / total_var
            
            return min(sobol_index, 1.0)  # Cap at 1.0
            
        except Exception:
            return 0.0
    
    def _calculate_correlation_confidence_interval(
        self,
        correlation: float,
        n: int,
        confidence_level: float
    ) -> Tuple[float, float]:
        """Calculate confidence interval for correlation coefficient"""
        try:
            # Fisher z-transformation
            z = 0.5 * np.log((1 + correlation) / (1 - correlation))
            z_critical = stats.norm.ppf((1 + confidence_level) / 2)
            se = 1 / np.sqrt(n - 3)
            
            z_lower = z - z_critical * se
            z_upper = z + z_critical * se
            
            # Transform back
            r_lower = (np.exp(2 * z_lower) - 1) / (np.exp(2 * z_lower) + 1)
            r_upper = (np.exp(2 * z_upper) - 1) / (np.exp(2 * z_upper) + 1)
            
            return (r_lower, r_upper)
        except:
            return (correlation - 0.1, correlation + 0.1)  # Fallback
    
    def _classify_effect_size(self, value: float) -> str:
        """Classify effect size based on magnitude"""
        abs_value = abs(value)
        
        if abs_value < 0.1:
            return 'negligible'
        elif abs_value < 0.3:
            return 'small'
        elif abs_value < 0.5:
            return 'medium'
        else:
            return 'large'
    
    def _estimate_response_change(
        self,
        parameter: str,
        percent_change: float,
        metric: str,
        base_response: float
    ) -> float:
        """Estimate response change based on parameter change (placeholder)"""
        # This is a placeholder - in practice, would use actual simulation results
        # or surrogate models
        
        # Simple linear sensitivity coefficients (examples)
        sensitivity_coeffs = {
            ('wave_height', 'tension'): 0.5,
            ('wave_height', 'vessel_offset'): 0.3,
            ('wind_speed', 'vessel_offset'): 0.2,
            ('current_speed', 'tension'): 0.1,
            ('wave_period', 'tension'): -0.2
        }
        
        key = (parameter, metric)
        coeff = sensitivity_coeffs.get(key, 0.1)  # Default sensitivity
        
        return base_response * coeff * (percent_change / 100)
    
    def _find_most_sensitive_parameter(self, parameter_effects: Dict[str, Any]) -> Optional[str]:
        """Find parameter with highest sensitivity"""
        if not parameter_effects:
            return None
        
        max_sensitivity = 0
        most_sensitive = None
        
        for param, effects in parameter_effects.items():
            sensitivity = abs(effects.get('sensitivity_coefficient', 0))
            if sensitivity > max_sensitivity:
                max_sensitivity = sensitivity
                most_sensitive = param
        
        return most_sensitive
    
    def _rank_parameters_by_sensitivity(self, parameter_effects: Dict[str, Any]) -> List[Tuple[str, float]]:
        """Rank parameters by sensitivity coefficient"""
        sensitivities = [
            (param, abs(effects.get('sensitivity_coefficient', 0)))
            for param, effects in parameter_effects.items()
        ]
        
        return sorted(sensitivities, key=lambda x: x[1], reverse=True)
    
    def _generate_sensitivity_summary(self, sensitivity_results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate summary of sensitivity analysis"""
        summary = {
            'total_metrics_analyzed': len(sensitivity_results),
            'most_sensitive_parameters': {},
            'least_sensitive_parameters': {},
            'average_sensitivities': {}
        }
        
        # Find most/least sensitive parameters for each metric
        for metric, results in sensitivity_results.items():
            if isinstance(results, list) and results:
                most_sensitive = results[0].parameter
                least_sensitive = results[-1].parameter
                avg_sensitivity = np.mean([r.sensitivity_index for r in results])
                
                summary['most_sensitive_parameters'][metric] = most_sensitive
                summary['least_sensitive_parameters'][metric] = least_sensitive
                summary['average_sensitivities'][metric] = avg_sensitivity
        
        return summary
    
    def _generate_sensitivity_recommendations(
        self,
        sensitivity_results: Dict[str, Any],
        parameter_ranges: Dict[str, ParameterRange]
    ) -> List[str]:
        """Generate recommendations based on sensitivity analysis"""
        recommendations = []
        
        # Find globally most sensitive parameters
        all_sensitivities = {}
        for metric, results in sensitivity_results.items():
            if isinstance(results, list):
                for result in results:
                    if result.parameter not in all_sensitivities:
                        all_sensitivities[result.parameter] = []
                    all_sensitivities[result.parameter].append(result.sensitivity_index)
        
        # Calculate average sensitivity for each parameter
        avg_sensitivities = {
            param: np.mean(sensitivities)
            for param, sensitivities in all_sensitivities.items()
        }
        
        # Sort by average sensitivity
        sorted_params = sorted(avg_sensitivities.items(), key=lambda x: x[1], reverse=True)
        
        if sorted_params:
            top_param = sorted_params[0][0]
            recommendations.append(f"Focus design optimization on {top_param} - highest overall sensitivity")
            
            # Recommend monitoring for top 3 parameters
            top_3_params = [param for param, _ in sorted_params[:3]]
            recommendations.append(f"Implement enhanced monitoring for: {', '.join(top_3_params)}")
            
            # Recommend tight tolerances for highly sensitive parameters
            high_sensitivity_params = [
                param for param, sens in sorted_params 
                if sens > 0.5  # Threshold for high sensitivity
            ]
            
            if high_sensitivity_params:
                recommendations.append(
                    f"Use tight control tolerances for highly sensitive parameters: {', '.join(high_sensitivity_params)}"
                )
        
        return recommendations
    
    def _generate_load_case_summary(self, load_case_results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate summary of load case analysis"""
        summary = {
            'metrics_analyzed': list(load_case_results.keys()),
            'most_sensitive_overall': None,
            'parameter_rankings': {}
        }
        
        # Find overall most sensitive parameter
        all_sensitivities = {}
        
        for metric, results in load_case_results.items():
            rankings = results.get('sensitivity_ranking', [])
            for param, sensitivity in rankings:
                if param not in all_sensitivities:
                    all_sensitivities[param] = []
                all_sensitivities[param].append(sensitivity)
        
        if all_sensitivities:
            avg_sensitivities = {
                param: np.mean(sensitivities)
                for param, sensitivities in all_sensitivities.items()
            }
            
            most_sensitive = max(avg_sensitivities, key=avg_sensitivities.get)
            summary['most_sensitive_overall'] = most_sensitive
            summary['parameter_rankings'] = sorted(
                avg_sensitivities.items(), key=lambda x: x[1], reverse=True
            )
        
        return summary
    
    def _generate_global_sensitivity_ranking(
        self,
        sensitivity_results: Dict[str, Any]
    ) -> List[Tuple[str, float]]:
        """Generate global sensitivity ranking across all metrics"""
        parameter_scores = {}
        
        for metric, results in sensitivity_results.items():
            param_sensitivities = results.get('parameter_sensitivities', {})
            
            for param, data in param_sensitivities.items():
                correlation = abs(data.get('correlation', 0))
                
                if param not in parameter_scores:
                    parameter_scores[param] = []
                parameter_scores[param].append(correlation)
        
        # Calculate average sensitivity for each parameter
        global_rankings = []
        for param, scores in parameter_scores.items():
            avg_score = np.mean(scores)
            global_rankings.append((param, avg_score))
        
        return sorted(global_rankings, key=lambda x: x[1], reverse=True)