"""
Test Iteration Simulation
Simulates the complete mooring tension iteration process without OrcaFlex
"""

import numpy as np
from pathlib import Path
from typing import Dict, Tuple
import matplotlib.pyplot as plt

from csv_parser import CSVParser
from length_calculator import LengthCalculator


class IterationSimulator:
    """Simulates the iteration process with synthetic tension responses"""
    
    def __init__(self, targets_file: Path):
        """Initialize with target specifications"""
        parser = CSVParser(targets_file)
        self.targets = parser.parse_mooring_targets()
        self.calculator = LengthCalculator(self.targets)
        
        # Initialize line lengths
        self.line_lengths = {}
        for name, target in self.targets.items():
            self.line_lengths[name] = target.line_lengths[0] if target.line_lengths else 11.0
        
        # History tracking
        self.tension_history = []
        self.error_history = []
        self.convergence_history = []
    
    def simulate_orcaflex_response(self, line_lengths: Dict[str, float]) -> Dict[str, float]:
        """
        Simulate OrcaFlex static analysis response
        This is a simplified model that assumes linear response with some non-linearity
        """
        tensions = {}
        
        for name, length in line_lengths.items():
            target = self.targets[name]
            if not target.has_tension_target():
                continue
            
            # Get EA and target
            ea = target.get_governing_ea()
            target_tension = target.target_tension
            
            # Simple model: T = T_target + EA * (L_target - L) / L_target
            # With some non-linearity to make it more realistic
            nominal_length = target.line_lengths[0] if target.line_lengths else 11.0
            strain = (length - nominal_length) / nominal_length
            
            # Add non-linear effects (catenary-like)
            nonlinear_factor = 1.0 + 0.1 * strain**2
            
            # Calculate tension
            tension = target_tension - ea * strain * nonlinear_factor * 0.1
            
            # Add some coupling effects (lines affect each other slightly)
            coupling_noise = np.random.normal(0, 2)  # Small random coupling
            tension += coupling_noise
            
            # Ensure positive tension
            tensions[name] = max(tension, 10.0)
        
        return tensions
    
    def run_iteration(self, max_iterations: int = 10, tolerance: float = 0.01, damping: float = 0.8):
        """
        Run the complete iteration process
        
        Args:
            max_iterations: Maximum number of iterations
            tolerance: Convergence tolerance (1% default)
            damping: Damping factor for adjustments (reduce oscillations)
        """
        print("="*80)
        print("MOORING TENSION ITERATION SIMULATION")
        print("="*80)
        print(f"Max iterations: {max_iterations}")
        print(f"Convergence tolerance: {tolerance*100:.1f}%")
        print(f"Damping factor: {damping:.1f}")
        print()
        
        for iteration in range(max_iterations):
            print(f"\n--- Iteration {iteration + 1} ---")
            
            # Simulate OrcaFlex response
            current_tensions = self.simulate_orcaflex_response(self.line_lengths)
            
            # Store history
            self.tension_history.append(current_tensions.copy())
            
            # Calculate adjustments
            adjustments = self.calculator.calculate_adjustments(
                current_tensions, 
                self.line_lengths, 
                damping_factor=damping
            )
            
            # Apply adjustments
            for name, adj in adjustments.items():
                self.line_lengths[name] = adj.new_length
            
            # Check convergence
            converged, unconverged = self.calculator.check_convergence(tolerance)
            
            # Calculate statistics
            errors = [abs(adj.tension_error_percent) for adj in adjustments.values()]
            max_error = max(errors) if errors else 0
            avg_error = np.mean(errors) if errors else 0
            
            self.error_history.append({
                'max': max_error,
                'avg': avg_error,
                'unconverged_count': len(unconverged)
            })
            
            print(f"Max error: {max_error:.2f}%")
            print(f"Avg error: {avg_error:.2f}%")
            print(f"Unconverged lines: {len(unconverged)}/{len(adjustments)}")
            
            if converged:
                print(f"\n[CONVERGED] in {iteration + 1} iterations!")
                self.generate_final_report()
                return True
            
            # Show worst lines
            if unconverged and len(unconverged) <= 5:
                print("Worst lines:")
                for line in unconverged[:5]:
                    print(f"  - {line}")
        
        print(f"\n[FAILED] to converge after {max_iterations} iterations")
        self.generate_final_report()
        return False
    
    def generate_final_report(self):
        """Generate final convergence report"""
        print("\n" + "="*80)
        print("FINAL REPORT")
        print("="*80)
        
        # Final tensions
        if self.tension_history:
            final_tensions = self.tension_history[-1]
            print("\nFinal Tensions:")
            print(f"{'Line':<10} {'Target(kN)':<12} {'Final(kN)':<12} {'Error(%)':<10}")
            print("-"*44)
            
            for name, target in self.targets.items():
                if target.has_tension_target():
                    current = final_tensions.get(name, 0)
                    error = (current - target.target_tension) / target.target_tension * 100
                    print(f"{name:<10} {target.target_tension:<12.1f} {current:<12.1f} {error:<10.2f}")
    
    def plot_convergence(self):
        """Plot convergence history"""
        if not self.error_history:
            print("No data to plot")
            return
        
        iterations = range(1, len(self.error_history) + 1)
        max_errors = [e['max'] for e in self.error_history]
        avg_errors = [e['avg'] for e in self.error_history]
        
        plt.figure(figsize=(10, 6))
        plt.plot(iterations, max_errors, 'r-', label='Max Error', marker='o')
        plt.plot(iterations, avg_errors, 'b-', label='Avg Error', marker='s')
        plt.axhline(y=1.0, color='g', linestyle='--', label='1% Tolerance')
        
        plt.xlabel('Iteration')
        plt.ylabel('Tension Error (%)')
        plt.title('Mooring Tension Iteration Convergence')
        plt.legend()
        plt.grid(True, alpha=0.3)
        plt.yscale('log')
        
        # Save plot
        plot_path = Path(__file__).parent / "data" / "convergence_plot.png"
        plt.savefig(plot_path, dpi=150, bbox_inches='tight')
        print(f"\nConvergence plot saved to: {plot_path}")
        plt.show()


def main():
    """Run the iteration simulation"""
    # Path to target tensions CSV
    targets_file = Path(__file__).parent / "data" / "sample_target_tensions.csv"
    
    # Create simulator
    simulator = IterationSimulator(targets_file)
    
    # Run simulation with different settings
    print("\n" + "="*80)
    print("TEST 1: Standard iteration with 0.8 damping")
    print("="*80)
    simulator.run_iteration(max_iterations=15, tolerance=0.01, damping=0.8)
    simulator.plot_convergence()
    
    # Reset for second test
    simulator = IterationSimulator(targets_file)
    
    print("\n" + "="*80)
    print("TEST 2: Aggressive iteration with 1.0 damping (no damping)")
    print("="*80)
    simulator.run_iteration(max_iterations=15, tolerance=0.01, damping=1.0)
    
    # Reset for third test
    simulator = IterationSimulator(targets_file)
    
    print("\n" + "="*80)
    print("TEST 3: Conservative iteration with 0.5 damping")
    print("="*80)
    simulator.run_iteration(max_iterations=20, tolerance=0.01, damping=0.5)


if __name__ == "__main__":
    main()