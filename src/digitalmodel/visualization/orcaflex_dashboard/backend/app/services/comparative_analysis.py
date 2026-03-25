"""
Comparative Analysis Service for OrcaFlex Results Dashboard

Provides comprehensive comparative analysis capabilities including:
- Multi-case comparison
- Loading condition analysis
- Environmental condition comparison
- Statistical significance testing
- Performance benchmarking
"""

import numpy as np
import pandas as pd
from scipy import stats
from scipy.stats import ttest_ind, mannwhitneyu, kruskal
from typing import Dict, List, Tuple, Optional, Any, Union
import logging
from dataclasses import dataclass
from enum import Enum
import itertools
from datetime import datetime

logger = logging.getLogger(__name__)


class ComparisonType(Enum):
    PAIRWISE = "pairwise"
    GROUP = "group"
    TREND = "trend"
    BENCHMARK = "benchmark"


class TestType(Enum):
    PARAMETRIC = "parametric"
    NON_PARAMETRIC = "non_parametric"
    AUTO = "auto"


@dataclass
class ComparisonResult:
    """Container for comparison analysis results"""
    case_1: str
    case_2: str
    metric: str
    test_statistic: float
    p_value: float
    effect_size: float
    is_significant: bool
    confidence_level: float
    mean_difference: float
    percent_difference: float
    recommendation: str


@dataclass
class GroupComparisonResult:
    """Container for multi-group comparison results"""
    groups: List[str]
    metric: str
    test_statistic: float
    p_value: float
    is_significant: bool
    post_hoc_results: List[ComparisonResult]
    effect_size: float
    confidence_level: float


class ComparativeAnalysisService:
    """Service for comparative analysis of OrcaFlex results"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
    
    def compare_loading_conditions(
        self,
        results_data: Dict[str, pd.DataFrame],
        metrics: List[str],
        confidence_level: float = 0.95,
        test_type: str = "auto"
    ) -> Dict[str, Any]:
        """
        Compare multiple loading conditions across specified metrics
        
        Args:
            results_data: Dict mapping condition names to result DataFrames
            metrics: List of metric column names to compare
            confidence_level: Statistical significance level
            test_type: Type of statistical test ('parametric', 'non_parametric', 'auto')
            
        Returns:
            Comprehensive comparison analysis results
        """
        try:
            if len(results_data) < 2:
                raise ValueError("Need at least 2 loading conditions for comparison")
            
            condition_names = list(results_data.keys())
            comparison_results = {}
            
            for metric in metrics:
                self.logger.info(f"Comparing metric: {metric}")
                
                # Extract metric data for all conditions
                metric_data = {}
                for condition, df in results_data.items():
                    if metric in df.columns:
                        # Remove NaN values
                        clean_data = df[metric].dropna().values
                        if len(clean_data) > 0:
                            metric_data[condition] = clean_data
                
                if len(metric_data) < 2:
                    self.logger.warning(f"Insufficient data for metric {metric}")
                    continue
                
                # Perform pairwise comparisons
                pairwise_results = self._perform_pairwise_comparisons(
                    metric_data, metric, confidence_level, test_type
                )
                
                # Perform group comparison if more than 2 conditions
                group_result = None
                if len(metric_data) > 2:
                    group_result = self._perform_group_comparison(
                        metric_data, metric, confidence_level, test_type
                    )
                
                # Calculate descriptive statistics
                descriptive_stats = self._calculate_descriptive_statistics(metric_data)
                
                # Generate recommendations
                recommendations = self._generate_comparison_recommendations(
                    pairwise_results, group_result, descriptive_stats
                )
                
                comparison_results[metric] = {
                    'pairwise_comparisons': pairwise_results,
                    'group_comparison': group_result,
                    'descriptive_statistics': descriptive_stats,
                    'recommendations': recommendations
                }
            
            return {
                'comparison_results': comparison_results,
                'summary': self._generate_comparison_summary(comparison_results),
                'metadata': {
                    'conditions': condition_names,
                    'metrics': metrics,
                    'confidence_level': confidence_level,
                    'test_type': test_type,
                    'timestamp': datetime.now().isoformat()
                }
            }
            
        except Exception as e:
            self.logger.error(f"Error in loading condition comparison: {str(e)}")
            raise
    
    def compare_environmental_conditions(
        self,
        environmental_data: Dict[str, Dict[str, float]],
        response_data: Dict[str, pd.DataFrame],
        response_metrics: List[str]
    ) -> Dict[str, Any]:
        """
        Compare responses under different environmental conditions
        
        Args:
            environmental_data: Dict mapping case names to environmental parameters
            response_data: Dict mapping case names to response DataFrames
            response_metrics: List of response metrics to analyze
            
        Returns:
            Environmental sensitivity analysis results
        """
        try:
            if len(environmental_data) != len(response_data):
                raise ValueError("Environmental and response data must have matching cases")
            
            case_names = list(environmental_data.keys())
            sensitivity_results = {}
            
            # Extract environmental parameters
            env_params = set()
            for case_env in environmental_data.values():
                env_params.update(case_env.keys())
            env_params = list(env_params)
            
            # Build environmental matrix
            env_matrix = []
            for case in case_names:
                case_env = []
                for param in env_params:
                    case_env.append(environmental_data[case].get(param, 0.0))
                env_matrix.append(case_env)
            env_matrix = np.array(env_matrix)
            
            # Analyze each response metric
            for metric in response_metrics:
                # Extract response data
                response_values = []
                valid_cases = []
                
                for case in case_names:
                    if case in response_data and metric in response_data[case].columns:
                        metric_data = response_data[case][metric].dropna()
                        if len(metric_data) > 0:
                            # Use mean response for correlation analysis
                            response_values.append(metric_data.mean())
                            valid_cases.append(case)
                
                if len(response_values) < 3:
                    self.logger.warning(f"Insufficient data for environmental analysis of {metric}")
                    continue
                
                # Filter environmental matrix for valid cases
                valid_indices = [case_names.index(case) for case in valid_cases]
                valid_env_matrix = env_matrix[valid_indices, :]
                response_array = np.array(response_values)
                
                # Calculate correlations between environmental parameters and responses
                correlations = {}
                for i, param in enumerate(env_params):
                    env_values = valid_env_matrix[:, i]
                    
                    # Skip if all values are the same
                    if np.std(env_values) == 0:
                        continue
                    
                    corr_coef, p_value = stats.pearsonr(env_values, response_array)
                    correlations[param] = {
                        'correlation': corr_coef,
                        'p_value': p_value,
                        'is_significant': p_value < 0.05,
                        'strength': self._interpret_correlation_strength(abs(corr_coef))
                    }
                
                # Identify most influential parameters
                significant_params = [
                    param for param, result in correlations.items()
                    if result['is_significant']
                ]
                
                # Rank by correlation strength
                ranked_params = sorted(
                    correlations.items(),
                    key=lambda x: abs(x[1]['correlation']),
                    reverse=True
                )
                
                sensitivity_results[metric] = {
                    'correlations': correlations,
                    'significant_parameters': significant_params,
                    'ranked_parameters': [(param, data['correlation']) for param, data in ranked_params],
                    'most_influential': ranked_params[0][0] if ranked_params else None
                }
            
            return {
                'sensitivity_analysis': sensitivity_results,
                'environmental_parameters': env_params,
                'summary': self._generate_environmental_summary(sensitivity_results),
                'recommendations': self._generate_environmental_recommendations(sensitivity_results)
            }
            
        except Exception as e:
            self.logger.error(f"Error in environmental condition comparison: {str(e)}")
            raise
    
    def benchmark_performance(
        self,
        current_data: pd.DataFrame,
        benchmark_data: pd.DataFrame,
        metrics: List[str],
        benchmark_name: str = "Baseline"
    ) -> Dict[str, Any]:
        """
        Benchmark current performance against baseline/reference data
        
        Args:
            current_data: Current analysis results
            benchmark_data: Benchmark/baseline results
            metrics: List of metrics to compare
            benchmark_name: Name of the benchmark case
            
        Returns:
            Performance benchmarking results
        """
        try:
            benchmark_results = {}
            
            for metric in metrics:
                if metric not in current_data.columns or metric not in benchmark_data.columns:
                    self.logger.warning(f"Metric {metric} not found in both datasets")
                    continue
                
                current_values = current_data[metric].dropna().values
                benchmark_values = benchmark_data[metric].dropna().values
                
                if len(current_values) == 0 or len(benchmark_values) == 0:
                    continue
                
                # Calculate basic statistics
                current_stats = {
                    'mean': np.mean(current_values),
                    'std': np.std(current_values),
                    'median': np.median(current_values),
                    'max': np.max(current_values),
                    'min': np.min(current_values)
                }
                
                benchmark_stats = {
                    'mean': np.mean(benchmark_values),
                    'std': np.std(benchmark_values),
                    'median': np.median(benchmark_values),
                    'max': np.max(benchmark_values),
                    'min': np.min(benchmark_values)
                }
                
                # Calculate differences
                mean_diff = current_stats['mean'] - benchmark_stats['mean']
                percent_diff = (mean_diff / benchmark_stats['mean']) * 100 if benchmark_stats['mean'] != 0 else 0
                
                # Statistical significance test
                stat, p_value = stats.ttest_ind(current_values, benchmark_values)
                is_significant = p_value < 0.05
                
                # Effect size (Cohen's d)
                pooled_std = np.sqrt(((len(current_values) - 1) * current_stats['std']**2 + 
                                    (len(benchmark_values) - 1) * benchmark_stats['std']**2) / 
                                   (len(current_values) + len(benchmark_values) - 2))
                cohens_d = mean_diff / pooled_std if pooled_std != 0 else 0
                
                # Performance assessment
                performance_category = self._assess_performance(percent_diff, cohens_d)
                
                benchmark_results[metric] = {
                    'current_statistics': current_stats,
                    'benchmark_statistics': benchmark_stats,
                    'mean_difference': mean_diff,
                    'percent_difference': percent_diff,
                    'statistical_test': {
                        'test_statistic': stat,
                        'p_value': p_value,
                        'is_significant': is_significant
                    },
                    'effect_size': cohens_d,
                    'performance_category': performance_category,
                    'interpretation': self._interpret_benchmark_result(
                        percent_diff, cohens_d, is_significant
                    )
                }
            
            return {
                'benchmark_results': benchmark_results,
                'overall_assessment': self._generate_overall_benchmark_assessment(benchmark_results),
                'benchmark_name': benchmark_name,
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            self.logger.error(f"Error in performance benchmarking: {str(e)}")
            raise
    
    def _perform_pairwise_comparisons(
        self,
        data: Dict[str, np.ndarray],
        metric: str,
        confidence_level: float,
        test_type: str
    ) -> List[ComparisonResult]:
        """Perform pairwise statistical comparisons"""
        results = []
        condition_pairs = list(itertools.combinations(data.keys(), 2))
        
        for case1, case2 in condition_pairs:
            data1 = data[case1]
            data2 = data[case2]
            
            # Choose appropriate test
            if test_type == "auto":
                # Use normality tests to decide
                _, p1 = stats.shapiro(data1[:min(len(data1), 5000)])
                _, p2 = stats.shapiro(data2[:min(len(data2), 5000)])
                use_parametric = p1 > 0.05 and p2 > 0.05
            else:
                use_parametric = test_type == "parametric"
            
            if use_parametric:
                # t-test
                stat, p_value = stats.ttest_ind(data1, data2)
                test_name = "t-test"
            else:
                # Mann-Whitney U test
                stat, p_value = mannwhitneyu(data1, data2, alternative='two-sided')
                test_name = "Mann-Whitney U"
            
            # Calculate effect size
            mean1, mean2 = np.mean(data1), np.mean(data2)
            pooled_std = np.sqrt((np.var(data1) + np.var(data2)) / 2)
            effect_size = abs(mean1 - mean2) / pooled_std if pooled_std != 0 else 0
            
            # Calculate differences
            mean_diff = mean1 - mean2
            percent_diff = (mean_diff / mean2) * 100 if mean2 != 0 else 0
            
            is_significant = p_value < (1 - confidence_level)
            
            recommendation = self._generate_pairwise_recommendation(
                case1, case2, mean_diff, percent_diff, effect_size, is_significant
            )
            
            results.append(ComparisonResult(
                case_1=case1,
                case_2=case2,
                metric=metric,
                test_statistic=stat,
                p_value=p_value,
                effect_size=effect_size,
                is_significant=is_significant,
                confidence_level=confidence_level,
                mean_difference=mean_diff,
                percent_difference=percent_diff,
                recommendation=recommendation
            ))
        
        return results
    
    def _perform_group_comparison(
        self,
        data: Dict[str, np.ndarray],
        metric: str,
        confidence_level: float,
        test_type: str
    ) -> GroupComparisonResult:
        """Perform multi-group comparison"""
        groups = list(data.keys())
        values = list(data.values())
        
        # Choose appropriate test
        if test_type == "auto":
            # Simple heuristic: use Kruskal-Wallis for robustness
            use_parametric = False
        else:
            use_parametric = test_type == "parametric"
        
        if use_parametric:
            # One-way ANOVA
            stat, p_value = stats.f_oneway(*values)
        else:
            # Kruskal-Wallis test
            stat, p_value = kruskal(*values)
        
        is_significant = p_value < (1 - confidence_level)
        
        # Calculate effect size (eta-squared approximation)
        total_var = np.var(np.concatenate(values))
        within_group_var = np.mean([np.var(group_data) for group_data in values])
        effect_size = 1 - (within_group_var / total_var) if total_var != 0 else 0
        
        # Post-hoc analysis if significant
        post_hoc_results = []
        if is_significant:
            post_hoc_results = self._perform_pairwise_comparisons(
                data, metric, confidence_level, test_type
            )
        
        return GroupComparisonResult(
            groups=groups,
            metric=metric,
            test_statistic=stat,
            p_value=p_value,
            is_significant=is_significant,
            post_hoc_results=post_hoc_results,
            effect_size=effect_size,
            confidence_level=confidence_level
        )
    
    def _calculate_descriptive_statistics(
        self,
        data: Dict[str, np.ndarray]
    ) -> Dict[str, Dict[str, float]]:
        """Calculate descriptive statistics for each group"""
        stats_dict = {}
        
        for group_name, group_data in data.items():
            stats_dict[group_name] = {
                'count': len(group_data),
                'mean': np.mean(group_data),
                'median': np.median(group_data),
                'std': np.std(group_data, ddof=1),
                'variance': np.var(group_data, ddof=1),
                'min': np.min(group_data),
                'max': np.max(group_data),
                'q25': np.percentile(group_data, 25),
                'q75': np.percentile(group_data, 75),
                'skewness': stats.skew(group_data),
                'kurtosis': stats.kurtosis(group_data)
            }
        
        return stats_dict
    
    def _interpret_correlation_strength(self, correlation: float) -> str:
        """Interpret correlation coefficient strength"""
        abs_corr = abs(correlation)
        if abs_corr >= 0.7:
            return "strong"
        elif abs_corr >= 0.5:
            return "moderate"
        elif abs_corr >= 0.3:
            return "weak"
        else:
            return "very_weak"
    
    def _assess_performance(self, percent_diff: float, effect_size: float) -> str:
        """Assess performance category based on differences"""
        if abs(percent_diff) < 5 and abs(effect_size) < 0.2:
            return "equivalent"
        elif percent_diff > 10 and effect_size > 0.5:
            return "significantly_better"
        elif percent_diff < -10 and effect_size > 0.5:
            return "significantly_worse"
        elif percent_diff > 5:
            return "better"
        elif percent_diff < -5:
            return "worse"
        else:
            return "similar"
    
    def _generate_pairwise_recommendation(
        self,
        case1: str,
        case2: str,
        mean_diff: float,
        percent_diff: float,
        effect_size: float,
        is_significant: bool
    ) -> str:
        """Generate recommendation based on pairwise comparison"""
        if not is_significant:
            return f"No statistically significant difference between {case1} and {case2}"
        
        direction = "higher" if mean_diff > 0 else "lower"
        magnitude = "large" if abs(effect_size) > 0.8 else "moderate" if abs(effect_size) > 0.5 else "small"
        
        return f"{case1} shows {magnitude} {direction} values than {case2} ({percent_diff:.1f}% difference)"
    
    def _generate_comparison_recommendations(
        self,
        pairwise_results: List[ComparisonResult],
        group_result: Optional[GroupComparisonResult],
        descriptive_stats: Dict[str, Dict[str, float]]
    ) -> List[str]:
        """Generate overall comparison recommendations"""
        recommendations = []
        
        # Count significant comparisons
        significant_pairs = [r for r in pairwise_results if r.is_significant]
        
        if len(significant_pairs) == 0:
            recommendations.append("No significant differences found between loading conditions")
        else:
            recommendations.append(f"Found {len(significant_pairs)} significant pairwise differences")
            
            # Find best and worst performing conditions
            means = {name: stats['mean'] for name, stats in descriptive_stats.items()}
            best_condition = max(means, key=means.get)
            worst_condition = min(means, key=means.get)
            
            recommendations.append(f"Best performing condition: {best_condition}")
            recommendations.append(f"Worst performing condition: {worst_condition}")
        
        if group_result and group_result.is_significant:
            recommendations.append("Overall group differences are statistically significant")
        
        return recommendations
    
    def _generate_comparison_summary(self, comparison_results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate summary of all comparisons"""
        summary = {
            'total_metrics': len(comparison_results),
            'metrics_with_significant_differences': 0,
            'most_variable_metric': None,
            'least_variable_metric': None
        }
        
        metric_variabilities = {}
        
        for metric, results in comparison_results.items():
            # Count significant differences
            pairwise = results.get('pairwise_comparisons', [])
            significant_count = sum(1 for r in pairwise if r.is_significant)
            
            if significant_count > 0:
                summary['metrics_with_significant_differences'] += 1
            
            # Calculate variability (average effect size)
            if pairwise:
                avg_effect_size = np.mean([r.effect_size for r in pairwise])
                metric_variabilities[metric] = avg_effect_size
        
        if metric_variabilities:
            summary['most_variable_metric'] = max(metric_variabilities, key=metric_variabilities.get)
            summary['least_variable_metric'] = min(metric_variabilities, key=metric_variabilities.get)
        
        return summary
    
    def _generate_environmental_summary(self, sensitivity_results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate summary of environmental sensitivity analysis"""
        summary = {
            'total_metrics': len(sensitivity_results),
            'metrics_with_significant_correlations': 0,
            'most_sensitive_metric': None,
            'least_sensitive_metric': None
        }
        
        metric_sensitivities = {}
        
        for metric, results in sensitivity_results.items():
            correlations = results.get('correlations', {})
            significant_params = results.get('significant_parameters', [])
            
            if significant_params:
                summary['metrics_with_significant_correlations'] += 1
            
            # Calculate overall sensitivity (max absolute correlation)
            if correlations:
                max_correlation = max(abs(data['correlation']) for data in correlations.values())
                metric_sensitivities[metric] = max_correlation
        
        if metric_sensitivities:
            summary['most_sensitive_metric'] = max(metric_sensitivities, key=metric_sensitivities.get)
            summary['least_sensitive_metric'] = min(metric_sensitivities, key=metric_sensitivities.get)
        
        return summary
    
    def _generate_environmental_recommendations(self, sensitivity_results: Dict[str, Any]) -> List[str]:
        """Generate environmental analysis recommendations"""
        recommendations = []
        
        # Find most influential parameters across all metrics
        all_correlations = {}
        for metric, results in sensitivity_results.items():
            correlations = results.get('correlations', {})
            for param, data in correlations.items():
                if param not in all_correlations:
                    all_correlations[param] = []
                all_correlations[param].append(abs(data['correlation']))
        
        # Calculate average influence
        param_influences = {
            param: np.mean(correlations) 
            for param, correlations in all_correlations.items()
        }
        
        if param_influences:
            most_influential = max(param_influences, key=param_influences.get)
            recommendations.append(f"Most influential environmental parameter: {most_influential}")
            
            # Recommend focusing on top parameters
            top_params = sorted(param_influences.items(), key=lambda x: x[1], reverse=True)[:3]
            top_param_names = [param for param, _ in top_params]
            recommendations.append(f"Focus monitoring on: {', '.join(top_param_names)}")
        
        return recommendations
    
    def _interpret_benchmark_result(
        self,
        percent_diff: float,
        effect_size: float,
        is_significant: bool
    ) -> str:
        """Interpret benchmark comparison result"""
        if not is_significant:
            return "Performance is statistically equivalent to benchmark"
        
        if abs(percent_diff) < 5:
            return "Minor difference from benchmark (within 5%)"
        
        direction = "better than" if percent_diff > 0 else "worse than"
        magnitude = "substantially" if abs(effect_size) > 0.8 else "moderately" if abs(effect_size) > 0.5 else "slightly"
        
        return f"Performance is {magnitude} {direction} benchmark ({percent_diff:.1f}% difference)"
    
    def _generate_overall_benchmark_assessment(self, benchmark_results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate overall benchmark assessment"""
        total_metrics = len(benchmark_results)
        better_count = 0
        worse_count = 0
        equivalent_count = 0
        
        significant_improvements = []
        significant_degradations = []
        
        for metric, results in benchmark_results.items():
            percent_diff = results['percent_difference']
            is_significant = results['statistical_test']['is_significant']
            
            if is_significant:
                if percent_diff > 5:
                    better_count += 1
                    significant_improvements.append(metric)
                elif percent_diff < -5:
                    worse_count += 1
                    significant_degradations.append(metric)
                else:
                    equivalent_count += 1
            else:
                equivalent_count += 1
        
        # Overall assessment
        if better_count > worse_count:
            overall = "improved"
        elif worse_count > better_count:
            overall = "degraded"
        else:
            overall = "mixed"
        
        return {
            'overall_assessment': overall,
            'total_metrics': total_metrics,
            'better_metrics': better_count,
            'worse_metrics': worse_count,
            'equivalent_metrics': equivalent_count,
            'significant_improvements': significant_improvements,
            'significant_degradations': significant_degradations
        }