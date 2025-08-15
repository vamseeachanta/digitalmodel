"""
Main mooring tension iteration module.

This module implements the core iteration algorithm using scipy optimization
and Newton-Raphson methods to achieve target mooring tensions.
"""

import numpy as np
import scipy.optimize
from typing import Dict, List, Optional, Tuple, Callable
from dataclasses import dataclass
from loguru import logger
from pathlib import Path
import time

from .config import IterationConfig
from .orcaflex_interface import OrcaFlexModelInterface
from .tension_analyzer import TensionAnalyzer
from .line_manager import LinePropertyManager


@dataclass
class IterationResult:
    """Result of the iteration process."""
    converged: bool
    iterations: int
    final_tensions: Dict[str, float]
    final_lengths: Dict[str, List[float]]
    max_error: float  # Maximum error percentage
    execution_time: float  # Seconds
    convergence_history: List[float]  # Max error at each iteration
    
    def summary(self) -> str:
        """Generate summary string."""
        status = "SUCCESS" if self.converged else "FAILED"
        return (f"Iteration {status}: "
                f"{self.iterations} iterations, "
                f"Max Error={self.max_error:.2f}%, "
                f"Time={self.execution_time:.1f}s")


class MooringTensionIterator:
    """
    Main orchestrator for mooring tension iteration.
    
    Implements scipy optimization and Newton-Raphson methods to achieve
    target tensions in mooring lines.
    """
    
    def __init__(self, 
                 config: IterationConfig,
                 model_interface: Optional[OrcaFlexModelInterface] = None,
                 use_mock: bool = False):
        """
        Initialize the tension iterator.
        
        Args:
            config: Iteration configuration
            model_interface: OrcaFlex model interface (created if not provided)
            use_mock: Use mock API for testing
        """
        self.config = config
        
        # Initialize components
        if model_interface:
            self.model = model_interface
        else:
            self.model = OrcaFlexModelInterface(use_mock=use_mock)
            
        self.tension_analyzer = TensionAnalyzer(self.model)
        self.line_manager = LinePropertyManager(self.model)
        
        # Set up configuration
        self.line_manager.set_line_properties(config.lines)
        self.tension_analyzer.set_target_tensions(config.get_target_tensions_dict())
        
        # Iteration state
        self.iteration_count = 0
        self.start_time = None
        self._jacobian_cache = None
        self._jacobian_iteration = -1
        
        logger.info(f"Initialized MooringTensionIterator with {len(config.lines)} lines")
    
    def load_model(self, model_path: str) -> None:
        """
        Load OrcaFlex model from file.
        
        Args:
            model_path: Path to model file (.sim or .dat)
        """
        logger.info(f"Loading model from {model_path}")
        self.model.load_model(model_path)
        
        # Validate model
        if not self.model.validate_model():
            raise RuntimeError("Model validation failed")
        
        # Backup original state
        self.line_manager.backup_original_properties()
    
    def iterate_to_targets(self,
                          vessel_names: Optional[List[str]] = None) -> IterationResult:
        """
        Main iteration method to achieve target tensions.
        
        Args:
            vessel_names: Vessels to fix during iteration
            
        Returns:
            IterationResult with convergence status and final state
        """
        logger.info("Starting tension iteration to targets")
        self.start_time = time.time()
        self.iteration_count = 0
        convergence_history = []
        
        # Fix vessels if specified
        if vessel_names or self.config.vessel_config.fix_vessels:
            vessels = vessel_names or self.config.vessel_config.vessels_to_fix
            dofs = self.config.vessel_config.fix_degrees_of_freedom
            self.model.fix_vessels(vessels, dofs)
            logger.info(f"Fixed vessels: {vessels}")
        
        # Choose iteration method
        if self.config.method == "scipy":
            result = self._iterate_scipy()
        elif self.config.method == "newton_raphson":
            result = self._iterate_newton_raphson()
        else:
            result = self._iterate_ea_based()  # Fallback to EA-based method
        
        # Add execution time
        result.execution_time = time.time() - self.start_time
        
        # Generate report
        logger.info(result.summary())
        logger.info("\n" + self.tension_analyzer.generate_tension_report())
        logger.info("\n" + self.line_manager.generate_length_report())
        
        return result
    
    def _iterate_scipy(self) -> IterationResult:
        """
        Iterate using scipy optimization (fsolve).
        
        This approach is based on the ScipyRootFinding.py reference.
        """
        logger.info("Using scipy.optimize.fsolve method")
        
        # Get initial line lengths as optimization variables
        line_names = sorted(self.config.get_target_tensions_dict().keys())
        initial_lengths = []
        
        for line_name in line_names:
            state = self.line_manager.current_state.get(line_name)
            if state:
                # Use total length as optimization variable
                initial_lengths.append(state.total_length)
            else:
                initial_lengths.append(200.0)  # Default initial guess
        
        initial_lengths = np.array(initial_lengths)
        
        # Define objective function
        def objective(lengths: np.ndarray) -> np.ndarray:
            """Calculate tension residuals for given lengths."""
            self.iteration_count += 1
            
            # Apply length updates
            length_dict = {}
            for i, line_name in enumerate(line_names):
                # Distribute length change proportionally across sections
                current_sections = self.line_manager.current_state[line_name].sections
                total_current = self.line_manager.current_state[line_name].total_length
                scale = lengths[i] / total_current if total_current > 0 else 1.0
                
                new_sections = []
                for section in current_sections:
                    if section is None:
                        new_sections.append(None)
                    else:
                        new_sections.append(section * scale)
                
                # Handle variable section
                if None in new_sections:
                    fixed_length = sum(s for s in new_sections if s is not None)
                    variable_length = max(0.1, lengths[i] - fixed_length)
                    for j, s in enumerate(new_sections):
                        if s is None:
                            new_sections[j] = variable_length
                            break
                
                length_dict[line_name] = new_sections
            
            # Apply to model
            self.line_manager.adjust_line_lengths(length_dict)
            
            # Calculate tensions
            current_tensions = self.tension_analyzer.extract_mooring_tensions()
            
            # Calculate residuals
            residuals = []
            target_dict = self.config.get_target_tensions_dict()
            for line_name in line_names:
                current = current_tensions.get(line_name, 0)
                target = target_dict[line_name]
                residuals.append(current - target)
            
            # Record snapshot
            self.tension_analyzer.record_snapshot(self.iteration_count)
            
            # Check convergence
            converged, max_error = self.tension_analyzer.check_convergence(
                self.config.convergence.tolerance,
                self.config.convergence.min_tolerance
            )
            
            logger.info(f"Iteration {self.iteration_count}: Max error = {max_error:.2f}%")
            
            return np.array(residuals)
        
        # Run scipy optimization
        try:
            solution, info, ier, msg = scipy.optimize.fsolve(
                objective,
                initial_lengths,
                full_output=True,
                xtol=1e-6,
                maxfev=self.config.convergence.max_iterations * len(line_names)
            )
            
            # Check final convergence
            converged, max_error = self.tension_analyzer.check_convergence(
                self.config.convergence.tolerance,
                self.config.convergence.min_tolerance
            )
            
            # Get convergence history
            iterations, errors = self.tension_analyzer.get_convergence_history()
            
            return IterationResult(
                converged=converged and ier == 1,
                iterations=self.iteration_count,
                final_tensions=self.tension_analyzer.current_tensions,
                final_lengths=self.line_manager.get_current_lengths(),
                max_error=max_error,
                execution_time=0,  # Will be set by caller
                convergence_history=errors
            )
            
        except Exception as e:
            logger.error(f"Scipy optimization failed: {e}")
            
            converged, max_error = self.tension_analyzer.check_convergence()
            iterations, errors = self.tension_analyzer.get_convergence_history()
            
            return IterationResult(
                converged=False,
                iterations=self.iteration_count,
                final_tensions=self.tension_analyzer.current_tensions,
                final_lengths=self.line_manager.get_current_lengths(),
                max_error=max_error,
                execution_time=0,
                convergence_history=errors
            )
    
    def _iterate_newton_raphson(self) -> IterationResult:
        """
        Iterate using multi-dimensional Newton-Raphson method.
        """
        logger.info("Using Newton-Raphson method")
        
        line_names = sorted(self.config.get_target_tensions_dict().keys())
        convergence_history = []
        
        for iteration in range(self.config.convergence.max_iterations):
            self.iteration_count = iteration + 1
            
            # Extract current tensions
            current_tensions = self.tension_analyzer.extract_mooring_tensions()
            
            # Check convergence
            converged, max_error = self.tension_analyzer.check_convergence(
                self.config.convergence.tolerance,
                self.config.convergence.min_tolerance
            )
            
            convergence_history.append(max_error)
            logger.info(f"Iteration {self.iteration_count}: Max error = {max_error:.2f}%")
            
            # Record snapshot
            self.tension_analyzer.record_snapshot(self.iteration_count)
            
            if converged:
                logger.info(f"Converged in {self.iteration_count} iterations")
                break
            
            # Calculate Jacobian matrix
            jacobian = self._compute_jacobian(line_names)
            
            # Calculate residuals
            residuals = self.tension_analyzer.calculate_residuals()
            
            # Solve linear system: J * delta_L = -residuals
            try:
                # Add regularization for numerical stability
                reg_factor = 1e-6
                J_reg = jacobian + reg_factor * np.eye(len(line_names))
                
                delta_lengths = np.linalg.solve(J_reg, -residuals)
                
                # Apply damping factor
                delta_lengths *= self.config.convergence.damping_factor
                
            except np.linalg.LinAlgError:
                logger.warning("Singular Jacobian matrix, using pseudo-inverse")
                delta_lengths = -np.linalg.pinv(jacobian) @ residuals
                delta_lengths *= self.config.convergence.damping_factor
            
            # Apply length updates
            current_lengths = self.line_manager.get_total_lengths()
            new_length_dict = {}
            
            for i, line_name in enumerate(line_names):
                current_total = current_lengths[line_name]
                new_total = current_total + delta_lengths[i]
                
                # Apply safety limits
                new_total = max(current_total * 0.9, min(current_total * 1.1, new_total))
                
                # Distribute change across sections
                current_sections = self.line_manager.current_state[line_name].sections
                scale = new_total / current_total if current_total > 0 else 1.0
                
                new_sections = []
                for section in current_sections:
                    if section is None:
                        new_sections.append(None)
                    else:
                        new_sections.append(section * scale)
                
                # Handle variable section
                if None in new_sections:
                    fixed_length = sum(s for s in new_sections if s is not None)
                    variable_length = max(0.1, new_total - fixed_length)
                    for j, s in enumerate(new_sections):
                        if s is None:
                            new_sections[j] = variable_length
                            break
                
                new_length_dict[line_name] = new_sections
            
            # Apply to model
            self.line_manager.adjust_line_lengths(new_length_dict)
        
        # Final convergence check
        converged, max_error = self.tension_analyzer.check_convergence(
            self.config.convergence.tolerance,
            self.config.convergence.min_tolerance
        )
        
        return IterationResult(
            converged=converged,
            iterations=self.iteration_count,
            final_tensions=self.tension_analyzer.current_tensions,
            final_lengths=self.line_manager.get_current_lengths(),
            max_error=max_error,
            execution_time=0,
            convergence_history=convergence_history
        )
    
    def _iterate_ea_based(self) -> IterationResult:
        """
        Iterate using EA-based method from existing mooring.py.
        """
        logger.info("Using EA-based iteration method")
        
        convergence_history = []
        
        for iteration in range(self.config.convergence.max_iterations):
            self.iteration_count = iteration + 1
            
            # Extract current tensions
            current_tensions = self.tension_analyzer.extract_mooring_tensions()
            
            # Check convergence
            converged, max_error = self.tension_analyzer.check_convergence(
                self.config.convergence.tolerance,
                self.config.convergence.min_tolerance
            )
            
            convergence_history.append(max_error)
            logger.info(f"Iteration {self.iteration_count}: Max error = {max_error:.2f}%")
            
            # Record snapshot
            self.tension_analyzer.record_snapshot(self.iteration_count)
            
            if converged:
                logger.info(f"Converged in {self.iteration_count} iterations")
                break
            
            # Calculate tension errors
            target_tensions = self.config.get_target_tensions_dict()
            tension_errors = {}
            for line_name, current in current_tensions.items():
                if line_name in target_tensions:
                    tension_errors[line_name] = current - target_tensions[line_name]
            
            # Calculate length updates using EA method
            new_lengths = self.line_manager.calculate_length_updates_ea(tension_errors)
            
            # Apply to model
            self.line_manager.adjust_line_lengths(new_lengths)
        
        # Final convergence check
        converged, max_error = self.tension_analyzer.check_convergence(
            self.config.convergence.tolerance,
            self.config.convergence.min_tolerance
        )
        
        return IterationResult(
            converged=converged,
            iterations=self.iteration_count,
            final_tensions=self.tension_analyzer.current_tensions,
            final_lengths=self.line_manager.get_current_lengths(),
            max_error=max_error,
            execution_time=0,
            convergence_history=convergence_history
        )
    
    def _compute_jacobian(self, line_names: List[str]) -> np.ndarray:
        """
        Compute Jacobian matrix using finite differences.
        
        Args:
            line_names: Ordered list of line names
            
        Returns:
            Jacobian matrix J[i,j] = dT_i/dL_j
        """
        # Check cache
        if self._jacobian_cache is not None and self._jacobian_iteration == self.iteration_count - 1:
            logger.debug("Using cached Jacobian")
            return self._jacobian_cache
        
        logger.debug("Computing Jacobian matrix")
        n = len(line_names)
        jacobian = np.zeros((n, n))
        
        # Get baseline tensions
        baseline_tensions = self.tension_analyzer.current_tensions.copy()
        baseline_lengths = self.line_manager.get_current_lengths()
        
        for j, line_name in enumerate(line_names):
            # Calculate perturbation
            current_total = self.line_manager.current_state[line_name].total_length
            h = max(
                self.config.jacobian.min_perturbation,
                current_total * self.config.jacobian.perturbation_factor
            )
            
            # Perturb length
            perturbed_sections = baseline_lengths[line_name].copy()
            scale = (current_total + h) / current_total
            
            for k in range(len(perturbed_sections)):
                if perturbed_sections[k] is not None:
                    perturbed_sections[k] *= scale
            
            # Handle variable section
            if None in perturbed_sections:
                fixed_length = sum(s for s in perturbed_sections if s is not None)
                variable_length = (current_total + h) - fixed_length
                for k, s in enumerate(perturbed_sections):
                    if s is None:
                        perturbed_sections[k] = variable_length
                        break
            
            # Apply perturbation
            self.line_manager.adjust_line_lengths({line_name: perturbed_sections})
            
            # Calculate perturbed tensions
            perturbed_tensions = self.tension_analyzer.extract_mooring_tensions()
            
            # Calculate derivatives
            for i, tension_line in enumerate(line_names):
                dT = perturbed_tensions.get(tension_line, 0) - baseline_tensions.get(tension_line, 0)
                jacobian[i, j] = dT / h
            
            # Restore original lengths
            self.line_manager.adjust_line_lengths(baseline_lengths)
        
        # Cache result
        self._jacobian_cache = jacobian
        self._jacobian_iteration = self.iteration_count
        
        return jacobian
    
    def generate_report(self, output_path: Optional[str] = None) -> str:
        """
        Generate comprehensive iteration report.
        
        Args:
            output_path: Optional path to save report
            
        Returns:
            Report string
        """
        report = []
        report.append("=" * 80)
        report.append("MOORING TENSION ITERATION REPORT")
        report.append("=" * 80)
        report.append("")
        
        # Configuration summary
        report.append("Configuration:")
        report.append(self.config.summary())
        report.append("")
        
        # Tension analysis
        report.append(self.tension_analyzer.generate_tension_report())
        report.append("")
        
        # Length modifications
        report.append(self.line_manager.generate_length_report())
        report.append("")
        
        # Convergence history
        if self.tension_analyzer.history:
            report.append("Convergence History:")
            report.append("-" * 60)
            for snapshot in self.tension_analyzer.history:
                report.append(snapshot.summary())
        
        report.append("=" * 80)
        
        report_text = "\n".join(report)
        
        if output_path:
            Path(output_path).write_text(report_text)
            logger.info(f"Report saved to {output_path}")
        
        return report_text