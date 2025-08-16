"""
Length Calculator Module for Mooring Tension Iteration
Calculates line length adjustments based on tension differences
"""

import yaml
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, asdict
import numpy as np

from csv_parser import MooringLineTarget


@dataclass
class LineLengthAdjustment:
    """Data class for line length adjustment results"""
    line_name: str
    current_tension: float  # kN
    target_tension: float   # kN
    tension_error: float    # kN
    tension_error_percent: float  # %
    current_length: float   # m
    length_adjustment: float  # m
    new_length: float      # m
    ea_used: float         # kN
    
    def is_converged(self, tolerance: float = 0.01) -> bool:
        """Check if this line has converged within tolerance"""
        return abs(self.tension_error_percent) <= tolerance * 100


class LengthCalculator:
    """Calculator for mooring line length adjustments"""
    
    def __init__(self, targets: Dict[str, MooringLineTarget]):
        """Initialize with target specifications"""
        self.targets = targets
        self.adjustments: List[LineLengthAdjustment] = []
        self.iteration_count = 0
    
    def calculate_adjustments(
        self,
        current_tensions: Dict[str, float],
        current_lengths: Optional[Dict[str, float]] = None,
        damping_factor: float = 1.0
    ) -> Dict[str, LineLengthAdjustment]:
        """
        Calculate line length adjustments using ΔL = L/EA × (T_current - T_target)
        
        Args:
            current_tensions: Current line tensions in kN
            current_lengths: Current line lengths in m (optional)
            damping_factor: Factor to reduce adjustment magnitude (0.0-1.0)
        
        Returns:
            Dictionary of line adjustments
        """
        adjustments = {}
        
        for line_name, target in self.targets.items():
            # Skip if no tension target
            if not target.has_tension_target():
                continue
            
            # Get current tension
            current_tension = current_tensions.get(line_name, 0.0)
            
            # Get current length (use default if not provided)
            if current_lengths:
                current_length = current_lengths.get(line_name, 100.0)
            else:
                # Use first segment length if available
                current_length = target.line_lengths[0] if target.line_lengths else 100.0
            
            # Get governing EA
            try:
                ea = target.get_governing_ea()
            except ValueError:
                print(f"Warning: No EA for {line_name}, skipping adjustment")
                continue
            
            # Calculate tension error
            tension_error = current_tension - target.target_tension
            tension_error_percent = (tension_error / target.target_tension * 100) if target.target_tension != 0 else 0
            
            # Calculate length adjustment: ΔL = L/EA × (T_current - T_target)
            # Note: Sign convention - positive tension error means line is too tight, needs lengthening
            # So we add positive adjustment to increase length
            length_adjustment = (current_length / ea) * tension_error * damping_factor
            
            # Calculate new length
            new_length = current_length + length_adjustment
            
            # Safety check - don't allow negative or extremely small lengths
            if new_length < 0.1:
                print(f"Warning: Calculated length for {line_name} too small ({new_length:.2f}m), limiting to 0.1m")
                new_length = 0.1
                length_adjustment = new_length - current_length
            
            # Create adjustment record
            adjustment = LineLengthAdjustment(
                line_name=line_name,
                current_tension=current_tension,
                target_tension=target.target_tension,
                tension_error=tension_error,
                tension_error_percent=tension_error_percent,
                current_length=current_length,
                length_adjustment=length_adjustment,
                new_length=new_length,
                ea_used=ea
            )
            
            adjustments[line_name] = adjustment
        
        self.adjustments = list(adjustments.values())
        self.iteration_count += 1
        
        return adjustments
    
    def check_convergence(self, tolerance: float = 0.01) -> Tuple[bool, List[str]]:
        """
        Check if all lines have converged within tolerance
        
        Args:
            tolerance: Convergence tolerance (default 1%)
        
        Returns:
            Tuple of (is_converged, list_of_unconverged_lines)
        """
        if not self.adjustments:
            return False, ["No adjustments calculated yet"]
        
        unconverged = []
        for adj in self.adjustments:
            if not adj.is_converged(tolerance):
                unconverged.append(f"{adj.line_name}: {adj.tension_error_percent:.1f}% error")
        
        return len(unconverged) == 0, unconverged
    
    def generate_includefile(
        self,
        output_path: Path,
        adjustments: Optional[Dict[str, LineLengthAdjustment]] = None
    ) -> Path:
        """
        Generate includefile YAML for OrcaFlex model update
        
        Args:
            output_path: Path for output includefile
            adjustments: Adjustments to write (uses latest if not provided)
        
        Returns:
            Path to generated file
        """
        if adjustments is None:
            adjustments = {adj.line_name: adj for adj in self.adjustments}
        
        # Create includefile content
        includefile_data = {
            'iteration': self.iteration_count,
            'lines': {}
        }
        
        for line_name, adj in adjustments.items():
            # Format for OrcaFlex includefile
            # This assumes the line length is modified via UnstretchedLength property
            includefile_data['lines'][line_name] = {
                'UnstretchedLength': [adj.new_length],  # List for segment lengths
                '_comment': f"Target: {adj.target_tension}kN, Current: {adj.current_tension:.1f}kN, Error: {adj.tension_error_percent:.1f}%"
            }
        
        # Write YAML file
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(output_path, 'w') as f:
            yaml.dump(includefile_data, f, default_flow_style=False, sort_keys=False)
        
        return output_path
    
    def generate_report(self) -> str:
        """Generate iteration report"""
        if not self.adjustments:
            return "No adjustments calculated yet"
        
        report = []
        report.append(f"=== Iteration {self.iteration_count} ===")
        report.append("")
        
        # Summary statistics
        max_error = max(abs(adj.tension_error_percent) for adj in self.adjustments)
        avg_error = np.mean([abs(adj.tension_error_percent) for adj in self.adjustments])
        converged, unconverged = self.check_convergence()
        
        report.append(f"Convergence: {'YES' if converged else 'NO'}")
        report.append(f"Max error: {max_error:.1f}%")
        report.append(f"Avg error: {avg_error:.1f}%")
        report.append("")
        
        # Line details
        report.append("Line Details:")
        report.append("-" * 80)
        report.append(f"{'Line':<10} {'Current':<10} {'Target':<10} {'Error':<10} {'Error%':<10} {'Adj(m)':<10}")
        report.append("-" * 80)
        
        for adj in sorted(self.adjustments, key=lambda x: abs(x.tension_error_percent), reverse=True):
            report.append(
                f"{adj.line_name:<10} "
                f"{adj.current_tension:<10.1f} "
                f"{adj.target_tension:<10.1f} "
                f"{adj.tension_error:<10.1f} "
                f"{adj.tension_error_percent:<10.1f} "
                f"{adj.length_adjustment:<10.3f}"
            )
        
        if unconverged:
            report.append("")
            report.append("Unconverged lines:")
            for line in unconverged:
                report.append(f"  - {line}")
        
        return "\n".join(report)


def main():
    """Test the length calculator"""
    from csv_parser import CSVParser
    
    # Load targets
    sample_file = Path(__file__).parent / "data" / "sample_target_tensions.csv"
    parser = CSVParser(sample_file)
    targets = parser.parse_mooring_targets()
    
    # Create calculator
    calculator = LengthCalculator(targets)
    
    # Simulate current tensions (start with 10% error)
    current_tensions = {}
    for name, target in targets.items():
        if target.has_tension_target():
            # Add some random error for testing
            error_factor = 1.1 if name in ['Line01', 'Line02'] else 0.9
            current_tensions[name] = target.target_tension * error_factor
    
    print("Initial tensions (simulated):")
    for name, tension in current_tensions.items():
        target = targets[name].target_tension
        error = (tension - target) / target * 100
        print(f"  {name}: {tension:.1f} kN (target: {target:.1f} kN, error: {error:+.1f}%)")
    
    # Calculate adjustments
    print("\nCalculating adjustments...")
    adjustments = calculator.calculate_adjustments(current_tensions)
    
    # Print report
    print("\n" + calculator.generate_report())
    
    # Generate includefile
    includefile_path = Path(__file__).parent / "data" / "test_includefile.yml"
    calculator.generate_includefile(includefile_path)
    print(f"\nIncludefile generated: {includefile_path}")
    
    # Simulate iteration (reduce errors by 50%)
    print("\n" + "="*80)
    print("Simulating next iteration (50% error reduction)...")
    for name in current_tensions:
        target = targets[name].target_tension
        current = current_tensions[name]
        current_tensions[name] = current + 0.5 * (target - current)
    
    adjustments = calculator.calculate_adjustments(current_tensions)
    print("\n" + calculator.generate_report())


if __name__ == "__main__":
    main()