"""
Main Orchestrator for Mooring Tension Iteration
Coordinates the complete iteration workflow from CSV targets to convergence
"""

import logging
import time
from pathlib import Path
from typing import Dict, Optional, Tuple, List
from datetime import datetime
import pandas as pd
import numpy as np
import yaml
import json

from test_implementation.csv_parser import CSVParser
from test_implementation.length_calculator import LengthCalculator
from orcaflex_interface import OrcaFlexInterface, OrcaFlexResult

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class MooringIterationOrchestrator:
    """Orchestrates the complete mooring tension iteration process"""
    
    def __init__(
        self,
        base_path: Path,
        target_csv: Path,
        model_file: str,
        config: Optional[Dict] = None
    ):
        """
        Initialize the orchestrator
        
        Args:
            base_path: Base directory for OrcaFlex files
            target_csv: CSV file with target tensions
            model_file: OrcaFlex model file name
            config: Optional configuration dictionary
        """
        self.base_path = Path(base_path)
        self.target_csv = Path(target_csv)
        self.model_file = model_file
        
        # Default configuration
        self.config = {
            'max_iterations': 10,
            'convergence_tolerance': 0.01,  # 1%
            'damping_factor': 0.8,
            'extraction_config': 'dm_ofx_post_fsts_lngc.yml',
            'output_dir': self.base_path / 'iteration_output',
            'save_history': True,
            'plot_convergence': True
        }
        
        # Update with provided config
        if config:
            self.config.update(config)
        
        # Create output directory
        self.output_dir = Path(self.config['output_dir'])
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize components
        self._initialize_components()
        
        # History tracking
        self.iteration_history = []
        self.start_time = None
        
    def _initialize_components(self):
        """Initialize all required components"""
        # Parse target tensions
        logger.info(f"Loading target tensions from: {self.target_csv}")
        parser = CSVParser(self.target_csv)
        self.targets = parser.parse_mooring_targets()
        
        # Validate targets
        warnings = parser.validate_targets(self.targets)
        if warnings:
            logger.warning(f"Target validation warnings: {len(warnings)}")
            for warning in warnings[:5]:  # Show first 5 warnings
                logger.warning(f"  - {warning}")
        
        # Initialize calculator
        self.calculator = LengthCalculator(self.targets)
        
        # Initialize OrcaFlex interface
        self.orcaflex = OrcaFlexInterface(self.base_path)
        
        logger.info(f"Initialized with {len(self.targets)} target lines")
    
    def run_iteration_process(self) -> Tuple[bool, Dict]:
        """
        Run the complete iteration process until convergence or max iterations
        
        Returns:
            Tuple of (converged, final_results)
        """
        self.start_time = time.time()
        
        logger.info("="*80)
        logger.info("STARTING MOORING TENSION ITERATION PROCESS")
        logger.info("="*80)
        logger.info(f"Model: {self.model_file}")
        logger.info(f"Target lines: {len(self.targets)}")
        logger.info(f"Max iterations: {self.config['max_iterations']}")
        logger.info(f"Convergence tolerance: {self.config['convergence_tolerance']*100:.1f}%")
        logger.info(f"Damping factor: {self.config['damping_factor']}")
        
        converged = False
        iteration = 0
        
        # Run baseline analysis first
        logger.info("\n--- Baseline Analysis ---")
        baseline_result = self._run_baseline_analysis()
        
        if not baseline_result:
            logger.error("Baseline analysis failed")
            return False, {}
        
        # Main iteration loop
        while iteration < self.config['max_iterations'] and not converged:
            iteration += 1
            logger.info(f"\n--- Iteration {iteration} ---")
            
            # Get current tensions from last analysis
            current_tensions = baseline_result.line_tensions if iteration == 1 else last_result.line_tensions
            
            # Calculate adjustments
            adjustments = self.calculator.calculate_adjustments(
                current_tensions,
                damping_factor=self.config['damping_factor']
            )
            
            # Check convergence
            converged, unconverged = self.calculator.check_convergence(
                tolerance=self.config['convergence_tolerance']
            )
            
            # Log status
            self._log_iteration_status(iteration, adjustments, unconverged)
            
            if converged:
                logger.info(f"\n[CONVERGED] in {iteration} iterations!")
                break
            
            # Generate includefile for next iteration
            includefile = self._generate_includefile(iteration)
            self.calculator.generate_includefile(includefile)
            
            # Run OrcaFlex analysis with updated lengths
            logger.info(f"Running OrcaFlex analysis with updated lengths...")
            last_result = self.orcaflex.run_iteration_analysis(
                self.model_file,
                includefile,
                self.config['extraction_config']
            )
            
            if not last_result:
                logger.error(f"Analysis failed at iteration {iteration}")
                break
            
            # Store history
            self._store_iteration_history(iteration, adjustments, last_result)
        
        # Final reporting
        elapsed_time = time.time() - self.start_time
        self._generate_final_report(converged, iteration, elapsed_time)
        
        # Save results
        if self.config['save_history']:
            self._save_results()
        
        # Plot convergence
        if self.config['plot_convergence']:
            self._plot_convergence()
        
        return converged, self.iteration_history[-1] if self.iteration_history else {}
    
    def _run_baseline_analysis(self) -> Optional[OrcaFlexResult]:
        """Run baseline analysis with current line lengths"""
        logger.info("Running baseline OrcaFlex analysis...")
        
        # Run static analysis
        success = self.orcaflex.run_static_analysis(self.model_file)
        
        if not success:
            return None
        
        # Extract results
        result = self.orcaflex.extract_results(self.config['extraction_config'])
        
        if result:
            logger.info(f"Baseline complete: {len(result.line_tensions)} tensions extracted")
            
            # Log baseline statistics
            tensions = list(result.line_tensions.values())
            logger.info(f"Baseline tensions - Min: {min(tensions):.1f} kN, "
                       f"Max: {max(tensions):.1f} kN, "
                       f"Mean: {np.mean(tensions):.1f} kN")
        
        return result
    
    def _generate_includefile(self, iteration: int) -> Path:
        """Generate includefile path for iteration"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"iteration_{iteration}_{timestamp}.yml"
        return self.output_dir / "includefiles" / filename
    
    def _log_iteration_status(self, iteration: int, adjustments: Dict, unconverged: List):
        """Log current iteration status"""
        if adjustments:
            errors = [abs(adj.tension_error_percent) for adj in adjustments.values()]
            max_error = max(errors)
            avg_error = np.mean(errors)
            
            logger.info(f"Max error: {max_error:.2f}%")
            logger.info(f"Avg error: {avg_error:.2f}%")
            logger.info(f"Unconverged lines: {len(unconverged)}/{len(adjustments)}")
            
            # Show worst lines
            if unconverged and len(unconverged) <= 5:
                logger.info("Worst lines:")
                for line in unconverged[:3]:
                    logger.info(f"  - {line}")
    
    def _store_iteration_history(self, iteration: int, adjustments: Dict, result: OrcaFlexResult):
        """Store iteration data for history tracking"""
        history_entry = {
            'iteration': iteration,
            'timestamp': datetime.now().isoformat(),
            'tensions': result.line_tensions.copy(),
            'adjustments': {
                name: {
                    'current_tension': adj.current_tension,
                    'target_tension': adj.target_tension,
                    'error_percent': adj.tension_error_percent,
                    'length_adjustment': adj.length_adjustment,
                    'new_length': adj.new_length
                }
                for name, adj in adjustments.items()
            },
            'max_error': max(abs(adj.tension_error_percent) for adj in adjustments.values()),
            'analysis_time': result.analysis_time
        }
        
        self.iteration_history.append(history_entry)
    
    def _generate_final_report(self, converged: bool, iterations: int, elapsed_time: float):
        """Generate final convergence report"""
        logger.info("\n" + "="*80)
        logger.info("FINAL REPORT")
        logger.info("="*80)
        
        logger.info(f"Status: {'CONVERGED' if converged else 'NOT CONVERGED'}")
        logger.info(f"Iterations: {iterations}")
        logger.info(f"Total time: {elapsed_time:.1f} seconds")
        logger.info(f"Time per iteration: {elapsed_time/iterations:.1f} seconds")
        
        if self.iteration_history:
            # Final tensions vs targets
            last_entry = self.iteration_history[-1]
            logger.info("\nFinal Tensions vs Targets:")
            logger.info(f"{'Line':<10} {'Target(kN)':<12} {'Final(kN)':<12} {'Error(%)':<10}")
            logger.info("-"*44)
            
            for name, target in self.targets.items():
                if target.has_tension_target():
                    current = last_entry['tensions'].get(name, 0)
                    error = (current - target.target_tension) / target.target_tension * 100
                    logger.info(f"{name:<10} {target.target_tension:<12.1f} "
                              f"{current:<12.1f} {error:<10.2f}")
    
    def _save_results(self):
        """Save iteration history and results to files"""
        # Save iteration history as JSON
        history_file = self.output_dir / f"iteration_history_{datetime.now():%Y%m%d_%H%M%S}.json"
        with open(history_file, 'w') as f:
            json.dump(self.iteration_history, f, indent=2, default=str)
        logger.info(f"Iteration history saved to: {history_file}")
        
        # Save summary as CSV
        if self.iteration_history:
            summary_data = []
            for entry in self.iteration_history:
                summary_data.append({
                    'iteration': entry['iteration'],
                    'max_error': entry['max_error'],
                    'analysis_time': entry['analysis_time'],
                    'timestamp': entry['timestamp']
                })
            
            summary_df = pd.DataFrame(summary_data)
            summary_file = self.output_dir / f"convergence_summary_{datetime.now():%Y%m%d_%H%M%S}.csv"
            summary_df.to_csv(summary_file, index=False)
            logger.info(f"Convergence summary saved to: {summary_file}")
    
    def _plot_convergence(self):
        """Generate convergence plot"""
        if not self.iteration_history:
            return
        
        try:
            import matplotlib.pyplot as plt
            
            iterations = [e['iteration'] for e in self.iteration_history]
            max_errors = [e['max_error'] for e in self.iteration_history]
            
            plt.figure(figsize=(10, 6))
            plt.plot(iterations, max_errors, 'b-o', label='Max Error')
            plt.axhline(y=self.config['convergence_tolerance']*100, 
                       color='r', linestyle='--', 
                       label=f"{self.config['convergence_tolerance']*100:.1f}% Tolerance")
            
            plt.xlabel('Iteration')
            plt.ylabel('Maximum Tension Error (%)')
            plt.title('Mooring Tension Iteration Convergence')
            plt.legend()
            plt.grid(True, alpha=0.3)
            plt.yscale('log')
            
            plot_file = self.output_dir / f"convergence_plot_{datetime.now():%Y%m%d_%H%M%S}.png"
            plt.savefig(plot_file, dpi=150, bbox_inches='tight')
            plt.close()
            
            logger.info(f"Convergence plot saved to: {plot_file}")
            
        except ImportError:
            logger.warning("Matplotlib not available, skipping convergence plot")


def main():
    """Main entry point for running mooring iteration"""
    # Configuration
    base_path = Path(r"D:\1522\ctr7\orcaflex\rev_a08\base_files")
    target_csv = base_path / "fsts_lngc_pretension" / "180km3_l000_pb_target_mooring_pretension.csv"
    model_file = "fsts_lngc_vessel_statics_6dof.yml"
    
    # Custom configuration
    config = {
        'max_iterations': 10,
        'convergence_tolerance': 0.01,
        'damping_factor': 0.8,
        'save_history': True,
        'plot_convergence': True
    }
    
    # Create orchestrator
    orchestrator = MooringIterationOrchestrator(
        base_path=base_path,
        target_csv=target_csv,
        model_file=model_file,
        config=config
    )
    
    # Run iteration process
    converged, results = orchestrator.run_iteration_process()
    
    if converged:
        logger.info("\nProcess completed successfully!")
    else:
        logger.warning("\nProcess did not converge within maximum iterations")
    
    return converged, results


if __name__ == "__main__":
    # Run the main process
    converged, results = main()