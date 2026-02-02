"""Pretension analysis module for mooring systems."""

from pathlib import Path
from typing import Optional, List, Dict, Any
import pandas as pd
import numpy as np
import logging
from datetime import datetime

from .models import (
    PretensionLineData,
    PretensionData,
    PretensionMetrics,
    ConvergenceStatus
)
from .exceptions import DataParsingError, ConvergenceError

logger = logging.getLogger(__name__)


class PretensionAnalyzer:
    """Analyzes pretension data from mooring analysis."""
    
    def __init__(self, tolerance: float = 0.05, warning_threshold: float = 0.10):
        """Initialize pretension analyzer.
        
        Args:
            tolerance: Convergence tolerance (default 5%)
            warning_threshold: Warning threshold (default 10%)
        """
        self.tolerance = tolerance
        self.warning_threshold = warning_threshold
    
    def parse_pretension_csv(self, file: Path) -> PretensionData:
        """Parse pretension CSV file.
        
        Args:
            file: Path to CSV file
            
        Returns:
            Parsed pretension data
            
        Raises:
            DataParsingError: If CSV cannot be parsed
        """
        try:
            df = pd.read_csv(file)
            
            # Validate required columns
            required_columns = [
                'ObjectName', 'target_tension', 'current_tension',
                'line_length', 'calculated_length', 'new_line_length',
                'tension_diff_percent', 'end_Gx_force', 'end_Gy_force', 
                'end_Gz_force'
            ]
            
            missing_columns = set(required_columns) - set(df.columns)
            if missing_columns:
                raise DataParsingError(
                    str(file),
                    f"Missing required columns: {missing_columns}"
                )
            
            # Parse line data
            lines = []
            for _, row in df.iterrows():
                line_data = PretensionLineData(
                    object_name=row['ObjectName'],
                    target_tension=float(row['target_tension']),
                    current_tension=float(row['current_tension']),
                    line_length=self._parse_list_value(row['line_length']),
                    calculated_length=float(row['calculated_length']),
                    new_line_length=self._parse_list_value(row['new_line_length']),
                    tension_diff_percent=float(row['tension_diff_percent']),
                    converged=abs(row['tension_diff_percent']) <= self.tolerance * 100,
                    end_forces=(
                        float(row['end_Gx_force']),
                        float(row['end_Gy_force']),
                        float(row['end_Gz_force'])
                    ),
                    line_EA=self._parse_list_value(row.get('line_EA', 0)) if 'line_EA' in row else None
                )
                lines.append(line_data)
            
            return PretensionData(
                filename=file,
                timestamp=datetime.now(),
                lines=lines,
                raw_dataframe=df
            )
            
        except Exception as e:
            raise DataParsingError(str(file), str(e))
    
    def _parse_list_value(self, value):
        """Parse string representation of list or return float."""
        if isinstance(value, str) and value.startswith('['):
            # Parse string list representation
            try:
                import ast
                return ast.literal_eval(value)
            except:
                return float(value) if value else 0.0
        return float(value) if value else 0.0
    
    def calculate_metrics(self, data: PretensionData) -> PretensionMetrics:
        """Calculate pretension metrics.
        
        Args:
            data: Pretension data
            
        Returns:
            Calculated metrics
        """
        df = data.raw_dataframe
        
        # Calculate convergence metrics
        convergence_rates = abs(df['tension_diff_percent'])
        converged_mask = convergence_rates <= self.tolerance * 100
        
        # Get tension values
        tensions = df['current_tension']
        
        metrics = PretensionMetrics(
            mean_convergence=1.0 - (convergence_rates.mean() / 100),
            std_convergence=convergence_rates.std() / 100,
            max_deviation=convergence_rates.max() / 100,
            min_deviation=convergence_rates.min() / 100,
            converged_lines=df.loc[converged_mask, 'ObjectName'].tolist(),
            problem_lines=df.loc[~converged_mask, 'ObjectName'].tolist(),
            total_lines=len(df),
            tension_distribution={
                'mean': tensions.mean(),
                'std': tensions.std(),
                'min': tensions.min(),
                'max': tensions.max(),
                'q25': tensions.quantile(0.25),
                'q50': tensions.quantile(0.50),
                'q75': tensions.quantile(0.75),
                'q95': tensions.quantile(0.95)
            },
            convergence_percentage=(converged_mask.sum() / len(df)) * 100,
            average_tension=tensions.mean(),
            tension_range=(tensions.min(), tensions.max()),
            recommendations=self._generate_recommendations(df, converged_mask)
        )
        
        return metrics
    
    def _generate_recommendations(self, df: pd.DataFrame, converged_mask: pd.Series) -> List[str]:
        """Generate recommendations based on analysis."""
        recommendations = []
        
        # Check for non-converged lines
        if not converged_mask.all():
            problem_lines = df.loc[~converged_mask, 'ObjectName'].tolist()
            recommendations.append(
                f"Lines {', '.join(problem_lines)} require further iteration"
            )
        
        # Check for high tension variations
        tension_std = df['current_tension'].std()
        if tension_std > df['current_tension'].mean() * 0.2:
            recommendations.append(
                "High tension variation detected - consider load balancing"
            )
        
        return recommendations
    
    def assess_convergence(self, metrics: PretensionMetrics) -> ConvergenceStatus:
        """Assess convergence status.
        
        Args:
            metrics: Pretension metrics
            
        Returns:
            Convergence status assessment
        """
        # Determine overall convergence
        overall_converged = len(metrics.problem_lines) == 0
        
        # Assess quality
        if metrics.convergence_percentage >= 95:
            quality = "excellent"
        elif metrics.convergence_percentage >= 85:
            quality = "good"
        elif metrics.convergence_percentage >= 75:
            quality = "acceptable"
        else:
            quality = "poor"
        
        # Calculate adjustments needed
        adjustment_needed = {}
        for line in metrics.problem_lines:
            # Simplified adjustment calculation
            adjustment_needed[line] = 5.0  # Placeholder
        
        # Estimate iterations
        estimated_iterations = {}
        for line in metrics.problem_lines:
            estimated_iterations[line] = 3  # Placeholder
        
        return ConvergenceStatus(
            overall_converged=overall_converged,
            convergence_quality=quality,
            critical_lines=metrics.problem_lines[:5],  # Top 5 problem lines
            adjustment_needed=adjustment_needed,
            estimated_iterations=estimated_iterations,
            confidence_level=min(95.0, metrics.convergence_percentage + 10)
        )