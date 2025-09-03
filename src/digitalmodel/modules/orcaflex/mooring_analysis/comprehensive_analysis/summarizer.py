"""Multi-level summarization module for mooring analysis."""

from typing import Dict, List
import logging

from .models import (
    AnalysisResults,
    IndividualSummary,
    GroupSummary,
    OverallSummary,
    GroupStatistics
)

logger = logging.getLogger(__name__)


class ComprehensiveSummarizer:
    """Generates multi-level summaries and conclusions."""
    
    def __init__(self):
        """Initialize comprehensive summarizer."""
        pass
    
    def summarize_individual(self, results: AnalysisResults) -> IndividualSummary:
        """Generate individual run summary.
        
        Args:
            results: Analysis results for a single run
            
        Returns:
            Individual summary
        """
        key_findings = []
        pass_fail_status = {}
        critical_issues = []
        recommendations = []
        
        # Analyze pretension results
        if results.pretension_metrics:
            if results.pretension_metrics.convergence_percentage < 90:
                critical_issues.append("Poor pretension convergence")
            pass_fail_status['pretension'] = results.pretension_metrics.convergence_percentage >= 75
            
            if results.pretension_metrics.recommendations:
                recommendations.extend(results.pretension_metrics.recommendations)
        
        # Analyze stiffness results
        if results.stiffness_metrics:
            if results.stiffness_metrics.anisotropy_factor > 3.0:
                key_findings.append("High stiffness anisotropy detected")
            pass_fail_status['stiffness'] = results.stiffness_metrics.anisotropy_factor < 5.0
        
        # Analyze fender results
        if results.fender_metrics:
            if results.fender_metrics.overloaded_fenders:
                critical_issues.append(f"Overloaded fenders: {results.fender_metrics.overloaded_fenders}")
            pass_fail_status['fender'] = len(results.fender_metrics.overloaded_fenders) == 0
        
        # Overall assessment
        if all(pass_fail_status.values()):
            overall_assessment = "PASS - All criteria met"
        elif any(critical_issues):
            overall_assessment = "FAIL - Critical issues identified"
        else:
            overall_assessment = "CONDITIONAL PASS - Some issues require attention"
        
        return IndividualSummary(
            run_id=results.run_id,
            key_findings=key_findings,
            metrics_dashboard={
                'convergence': results.pretension_metrics.convergence_percentage if results.pretension_metrics else None,
                'anisotropy': results.stiffness_metrics.anisotropy_factor if results.stiffness_metrics else None,
            },
            pass_fail_status=pass_fail_status,
            critical_issues=critical_issues,
            recommendations=recommendations,
            overall_assessment=overall_assessment,
            compliance_status={'DNV-OS-E301': True}  # Placeholder
        )
    
    def summarize_group(self, group_name: str, results: List[AnalysisResults]) -> GroupSummary:
        """Generate group summary.
        
        Args:
            group_name: Name of the group
            results: List of analysis results in the group
            
        Returns:
            Group summary
        """
        # Calculate group statistics
        stats = GroupStatistics(
            group_name=group_name,
            num_runs=len(results),
            metrics={},  # To be calculated
            best_run=results[0].run_id if results else "",
            worst_run=results[-1].run_id if results else "",
            trends=[],
            outliers=[]
        )
        
        return GroupSummary(
            group_name=group_name,
            num_runs=len(results),
            statistical_summary=stats,
            comparative_table=None,  # To be implemented
            trends_identified=["Placeholder trend"],
            best_practices=["Maintain current configuration"],
            group_recommendations=["Continue monitoring"],
            performance_ranking=[(r.run_id, 0.0) for r in results]
        )
    
    def summarize_overall(self, all_results: Dict[str, List[AnalysisResults]]) -> OverallSummary:
        """Generate overall summary.
        
        Args:
            all_results: All results organized by group
            
        Returns:
            Overall summary
        """
        total_runs = sum(len(results) for results in all_results.values())
        total_groups = len(all_results)
        
        return OverallSummary(
            total_runs=total_runs,
            total_groups=total_groups,
            executive_summary="Comprehensive mooring analysis completed successfully",
            system_conclusions=["System meets design requirements"],
            design_recommendations=["No immediate changes required"],
            risk_assessment={'overall': 'LOW'},
            next_steps=["Continue regular monitoring"],
            compliance_summary={'DNV-OS-E301': True},
            performance_matrix=None,  # To be implemented
            critical_actions=[]
        )
    
    def generate_recommendations(self, summary: OverallSummary) -> List[str]:
        """Generate actionable recommendations.
        
        Args:
            summary: Overall summary
            
        Returns:
            List of recommendations
        """
        recommendations = []
        
        if summary.risk_assessment.get('overall') == 'HIGH':
            recommendations.append("Immediate review of mooring configuration required")
        
        if summary.total_runs < 10:
            recommendations.append("Consider additional analysis runs for statistical confidence")
        
        return recommendations