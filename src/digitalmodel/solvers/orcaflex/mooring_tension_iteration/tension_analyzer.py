"""
Tension extraction and analysis module.

This module handles extraction and analysis of mooring line tensions
from OrcaFlex static analysis results.
"""

import numpy as np
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field
from loguru import logger
from datetime import datetime

from .orcaflex_interface import OrcaFlexModelInterface, StaticResult


@dataclass
class TensionData:
    """Data class for tension analysis results."""
    line_name: str
    current_tension: float  # kN
    target_tension: float  # kN
    error: float  # kN
    error_percentage: float  # %
    converged: bool
    
    def __str__(self) -> str:
        """String representation of tension data."""
        status = "✓" if self.converged else "✗"
        return (f"{status} {self.line_name}: "
                f"Current={self.current_tension:.1f}kN, "
                f"Target={self.target_tension:.1f}kN, "
                f"Error={self.error_percentage:.2f}%")


@dataclass 
class TensionSnapshot:
    """Snapshot of tensions at a specific iteration."""
    iteration: int
    timestamp: datetime
    tensions: Dict[str, float]  # Line name to tension
    errors: Dict[str, float]  # Line name to error percentage
    max_error: float
    converged: bool
    
    def summary(self) -> str:
        """Generate summary string."""
        return (f"Iteration {self.iteration}: "
                f"Max Error={self.max_error:.2f}%, "
                f"Converged={'Yes' if self.converged else 'No'}")


class TensionAnalyzer:
    """
    Analyzer for mooring line tensions.
    
    Handles tension extraction, error calculation, and convergence checking.
    """
    
    def __init__(self, model_interface: OrcaFlexModelInterface):
        """
        Initialize the tension analyzer.
        
        Args:
            model_interface: Interface to OrcaFlex model
        """
        self.model = model_interface
        self.history: List[TensionSnapshot] = []
        self.current_tensions: Dict[str, float] = {}
        self.target_tensions: Dict[str, float] = {}
        
    def set_target_tensions(self, targets: Dict[str, float]) -> None:
        """
        Set target tensions for mooring lines.
        
        Args:
            targets: Dictionary mapping line names to target tensions (kN)
        """
        self.target_tensions = targets.copy()
        logger.info(f"Set target tensions for {len(targets)} lines")
        
    def extract_mooring_tensions(self) -> Dict[str, float]:
        """
        Extract current tensions from OrcaFlex model.
        
        Returns:
            Dictionary mapping line names to current tensions (kN)
        """
        logger.debug("Extracting mooring tensions")
        
        # Run static analysis
        result = self.model.run_static_analysis()
        
        if not result.converged:
            logger.warning("Static analysis did not converge")
        
        self.current_tensions = result.tensions.copy()
        return self.current_tensions
    
    def analyze_tensions(self, 
                        current_tensions: Optional[Dict[str, float]] = None,
                        target_tensions: Optional[Dict[str, float]] = None) -> List[TensionData]:
        """
        Analyze tensions and calculate errors.
        
        Args:
            current_tensions: Current tensions (uses stored if not provided)
            target_tensions: Target tensions (uses stored if not provided)
            
        Returns:
            List of TensionData objects with analysis results
        """
        if current_tensions is None:
            current_tensions = self.current_tensions
        if target_tensions is None:
            target_tensions = self.target_tensions
            
        if not current_tensions or not target_tensions:
            raise ValueError("Tensions not available for analysis")
        
        analysis_results = []
        
        for line_name in target_tensions:
            if line_name not in current_tensions:
                logger.warning(f"No current tension for line {line_name}")
                continue
                
            current = current_tensions[line_name]
            target = target_tensions[line_name]
            error = current - target
            error_pct = (error / target) * 100 if target != 0 else float('inf')
            
            # Check convergence (default 1% tolerance)
            converged = abs(error_pct) <= 1.0
            
            data = TensionData(
                line_name=line_name,
                current_tension=current,
                target_tension=target,
                error=error,
                error_percentage=error_pct,
                converged=converged
            )
            
            analysis_results.append(data)
            
        return analysis_results
    
    def check_convergence(self,
                         tolerance: float = 1.0,
                         min_tolerance_kn: float = 0.1) -> Tuple[bool, float]:
        """
        Check if all lines have converged within tolerance.
        
        Args:
            tolerance: Percentage tolerance for convergence
            min_tolerance_kn: Minimum absolute tolerance in kN
            
        Returns:
            Tuple of (converged, max_error_percentage)
        """
        if not self.current_tensions or not self.target_tensions:
            return False, float('inf')
        
        max_error = 0.0
        all_converged = True
        
        for line_name in self.target_tensions:
            if line_name not in self.current_tensions:
                all_converged = False
                continue
                
            current = self.current_tensions[line_name]
            target = self.target_tensions[line_name]
            error = abs(current - target)
            error_pct = (error / target) * 100 if target != 0 else float('inf')
            
            # Check both percentage and absolute tolerance
            if error_pct > tolerance and error > min_tolerance_kn:
                all_converged = False
                
            max_error = max(max_error, error_pct)
        
        logger.debug(f"Convergence check: {all_converged}, Max error: {max_error:.2f}%")
        return all_converged, max_error
    
    def calculate_residuals(self) -> np.ndarray:
        """
        Calculate residual vector (current - target tensions).
        
        Returns:
            Numpy array of residuals in order of line names
        """
        if not self.current_tensions or not self.target_tensions:
            raise ValueError("Tensions not available")
        
        residuals = []
        for line_name in sorted(self.target_tensions.keys()):
            current = self.current_tensions.get(line_name, 0)
            target = self.target_tensions[line_name]
            residuals.append(current - target)
        
        return np.array(residuals)
    
    def record_snapshot(self, iteration: int) -> TensionSnapshot:
        """
        Record current state as a snapshot.
        
        Args:
            iteration: Current iteration number
            
        Returns:
            TensionSnapshot object
        """
        if not self.current_tensions or not self.target_tensions:
            raise ValueError("Tensions not available for snapshot")
        
        errors = {}
        for line_name in self.target_tensions:
            if line_name in self.current_tensions:
                current = self.current_tensions[line_name]
                target = self.target_tensions[line_name]
                error_pct = ((current - target) / target) * 100 if target != 0 else 0
                errors[line_name] = error_pct
        
        converged, max_error = self.check_convergence()
        
        snapshot = TensionSnapshot(
            iteration=iteration,
            timestamp=datetime.now(),
            tensions=self.current_tensions.copy(),
            errors=errors,
            max_error=max_error,
            converged=converged
        )
        
        self.history.append(snapshot)
        return snapshot
    
    def get_convergence_history(self) -> Tuple[List[int], List[float]]:
        """
        Get convergence history for plotting.
        
        Returns:
            Tuple of (iterations, max_errors)
        """
        if not self.history:
            return [], []
        
        iterations = [s.iteration for s in self.history]
        max_errors = [s.max_error for s in self.history]
        
        return iterations, max_errors
    
    def generate_tension_report(self) -> str:
        """
        Generate a text report of current tension state.
        
        Returns:
            Formatted report string
        """
        lines = ["=" * 60]
        lines.append("MOORING TENSION ANALYSIS REPORT")
        lines.append("=" * 60)
        
        if not self.current_tensions or not self.target_tensions:
            lines.append("No tension data available")
            return "\n".join(lines)
        
        # Analyze current state
        analysis = self.analyze_tensions()
        
        # Summary
        converged, max_error = self.check_convergence()
        lines.append(f"\nConvergence Status: {'CONVERGED' if converged else 'NOT CONVERGED'}")
        lines.append(f"Maximum Error: {max_error:.2f}%")
        lines.append(f"Number of Lines: {len(self.target_tensions)}")
        
        # Detailed results
        lines.append("\nDetailed Results:")
        lines.append("-" * 60)
        lines.append(f"{'Line':<10} {'Current':>10} {'Target':>10} {'Error':>10} {'Error %':>10} {'Status':>8}")
        lines.append("-" * 60)
        
        for data in analysis:
            status = "OK" if data.converged else "FAIL"
            lines.append(
                f"{data.line_name:<10} "
                f"{data.current_tension:>10.1f} "
                f"{data.target_tension:>10.1f} "
                f"{data.error:>10.1f} "
                f"{data.error_percentage:>10.2f} "
                f"{status:>8}"
            )
        
        # History summary if available
        if self.history:
            lines.append("\nIteration History:")
            lines.append("-" * 60)
            for snapshot in self.history[-5:]:  # Last 5 iterations
                lines.append(snapshot.summary())
        
        lines.append("=" * 60)
        return "\n".join(lines)
    
    def export_results(self, filepath: str, format: str = "csv") -> None:
        """
        Export analysis results to file.
        
        Args:
            filepath: Output file path
            format: Export format ('csv' or 'excel')
        """
        import pandas as pd
        
        if not self.current_tensions or not self.target_tensions:
            logger.warning("No data to export")
            return
        
        # Create dataframe
        data = []
        for line_name in sorted(self.target_tensions.keys()):
            current = self.current_tensions.get(line_name, 0)
            target = self.target_tensions[line_name]
            error = current - target
            error_pct = (error / target) * 100 if target != 0 else 0
            
            data.append({
                'Line': line_name,
                'Current_Tension_kN': current,
                'Target_Tension_kN': target,
                'Error_kN': error,
                'Error_Percentage': error_pct,
                'Converged': abs(error_pct) <= 1.0
            })
        
        df = pd.DataFrame(data)
        
        # Export based on format
        if format.lower() == 'excel':
            df.to_excel(filepath, index=False)
            logger.info(f"Exported results to Excel: {filepath}")
        else:
            df.to_csv(filepath, index=False)
            logger.info(f"Exported results to CSV: {filepath}")