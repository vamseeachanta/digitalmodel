#!/usr/bin/env python
"""
Mooring Tension Iteration Orchestrator
Simple loop execution of existing batch commands with convergence tracking.
NO engineering calculations - just orchestration.
"""

import subprocess
import csv
import os
import sys
import argparse
from datetime import datetime
from pathlib import Path


class MooringTensionOrchestrator:
    """Orchestrates the execution of mooring tension iteration batch commands."""
    
    def __init__(self, working_dir="go-by", max_iterations=10, verbose=False):
        """
        Initialize the orchestrator.
        
        Args:
            working_dir: Directory containing batch files and configs
            max_iterations: Maximum number of iterations to run
            verbose: Enable verbose output
        """
        self.working_dir = Path(working_dir).resolve()
        self.max_iterations = max_iterations
        self.verbose = verbose
        # Use uv run for all Python commands to ensure repo environment is used
        self.python_exec = "uv run python"
        self.iteration_history = []
        self.target_tensions = {}
        self.tolerances = {}
        
        # Validate working directory
        if not self.working_dir.exists():
            raise ValueError(f"Working directory does not exist: {self.working_dir}")
            
    def load_targets(self):
        """Load target tensions and tolerances from CSV."""
        csv_path = self.working_dir / "fsts_l015_125km3_pb_target_mooring_pretension.csv"
        
        if not csv_path.exists():
            raise FileNotFoundError(f"Target CSV not found: {csv_path}")
            
        print(f"Loading targets from: {csv_path}")
        
        with open(csv_path, 'r') as f:
            reader = csv.DictReader(f)
            for row in reader:
                line_name = row['ObjectName']
                self.target_tensions[line_name] = float(row['target_tension'])
                self.tolerances[line_name] = float(row['tolerance'])
                
        print(f"Loaded {len(self.target_tensions)} target tensions")
    
    def run_command(self, command, description=""):
        """
        Execute shell command and wait for completion.
        
        Args:
            command: Command to execute
            description: Description of what the command does
            
        Returns:
            subprocess.CompletedProcess result
        """
        if description:
            print(f"\n{description}")
            
        if self.verbose:
            print(f"Executing: {command}")
            
        try:
            # Determine the repo root (parent of specs/modules/orcaflex/mooring-tension-iteration)
            repo_root = Path(__file__).parent.parent.parent.parent.parent
            
            # Adjust command to include working directory context
            # Replace python commands with uv run python
            if command.startswith("python ") or " python " in command:
                command = command.replace("python ", "uv run python ")
            
            # For Windows, ensure proper command execution
            # Run from repo root but with files in working_dir
            result = subprocess.run(
                command,
                shell=True,
                cwd=repo_root,  # Run from repo root for uv
                capture_output=not self.verbose,
                text=True
            )
            
            if result.returncode != 0:
                print(f"Warning: Command returned non-zero exit code: {result.returncode}")
                if not self.verbose and result.stderr:
                    print(f"Error output: {result.stderr}")
                    
            return result
            
        except Exception as e:
            print(f"Error executing command: {e}")
            return None
    
    def read_current_tensions(self):
        """Read current tension values from result CSV."""
        # Try multiple possible result file patterns
        result_patterns = [
            "results/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv",
            "results/*_pretension_analysis.csv",
            "fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv",
            "*_pretension_analysis.csv"
        ]
        
        tensions = {}
        csv_path = None
        
        # Find the result file
        for pattern in result_patterns:
            if '*' in pattern:
                # Handle glob patterns
                search_dir = self.working_dir / "results" if "results/" in pattern else self.working_dir
                pattern_only = pattern.replace("results/", "")
                matching_files = list(search_dir.glob(pattern_only))
                if matching_files:
                    csv_path = matching_files[0]  # Use first matching file
                    break
            else:
                # Direct path
                test_path = self.working_dir / pattern
                if test_path.exists():
                    csv_path = test_path
                    break
        
        if not csv_path or not csv_path.exists():
            print("Warning: No result CSV file found")
            return tensions
            
        if self.verbose:
            print(f"Reading tensions from: {csv_path}")
            
        try:
            with open(csv_path, 'r') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    # Try different column names
                    line_name = row.get('ObjectName', row.get('LineName', ''))
                    # Try multiple possible column names for tension
                    tension_value = row.get('current_tension', row.get('EffectiveTension', row.get('Tension', row.get('tension', ''))))
                    
                    if line_name and tension_value:
                        try:
                            tensions[line_name] = float(tension_value)
                        except ValueError:
                            if self.verbose:
                                print(f"Warning: Could not parse tension value for {line_name}: {tension_value}")
                                
        except Exception as e:
            print(f"Error reading result CSV: {e}")
            
        return tensions
    
    def check_convergence(self, current_tensions):
        """
        Check if all lines are within tolerance.
        
        Args:
            current_tensions: Dictionary of current tension values
            
        Returns:
            True if all lines converged, False otherwise
        """
        if not current_tensions:
            print("No tension data available for convergence check")
            return False
            
        all_converged = True
        converged_count = 0
        
        print("\nConvergence Status:")
        print("-" * 70)
        
        for line_name, target in sorted(self.target_tensions.items()):
            if line_name not in current_tensions:
                print(f"  {line_name}: NO DATA AVAILABLE")
                all_converged = False
                continue
                
            current = current_tensions[line_name]
            tolerance = self.tolerances[line_name]
            
            # Calculate percentage difference
            diff_percent = abs(current - target) / target * 100 if target != 0 else 0
            
            is_converged = diff_percent <= tolerance
            if is_converged:
                converged_count += 1
            else:
                all_converged = False
                
            status = "PASS" if is_converged else "FAIL"
            print(f"  {line_name}: {current:7.1f} kN (target: {target:5.1f} kN, "
                  f"diff: {diff_percent:5.1f}%, tol: {tolerance:3.0f}%) {status}")
        
        print("-" * 70)
        print(f"Converged: {converged_count}/{len(self.target_tensions)} lines")
        
        return all_converged
    
    def run(self):
        """Main orchestration loop."""
        print("=" * 70)
        print("MOORING TENSION ITERATION ORCHESTRATOR")
        print("=" * 70)
        print(f"Working directory: {self.working_dir}")
        print(f"Maximum iterations: {self.max_iterations}")
        print(f"Start time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        
        # Load targets
        try:
            self.load_targets()
        except Exception as e:
            print(f"Error loading targets: {e}")
            return False
        
        # Main iteration loop
        for iteration in range(1, self.max_iterations + 1):
            print(f"\n{'='*70}")
            print(f"ITERATION {iteration}")
            print('='*70)
            
            # Get relative path to working directory from repo root
            rel_working_dir = f"specs/modules/orcaflex/mooring-tension-iteration/{self.working_dir.name}"
            
            # Step 1: Run tension calculation
            self.run_command(
                f"uv run python -m digitalmodel {rel_working_dir}/dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml",
                "Step 1: Running tension calculation..."
            )
            
            # Step 2: Run OrcaFlex analysis
            self.run_command(
                f'uv run python -m digitalmodel.modules.orcaflex.universal '
                f'pattern="fsts*125km3*pb_*.yml" input_directory="{rel_working_dir}" '
                f'output_directory="{rel_working_dir}" validate=false',
                "Step 2: Running OrcaFlex analysis..."
            )
            
            # Step 3: Post-process results
            self.run_command(
                f"uv run python -m digitalmodel {rel_working_dir}/dm_ofx_post_fsts_lngc.yml --workers 30",
                "Step 3: Post-processing results..."
            )
            
            # Step 4: Check convergence
            print("\nStep 4: Checking convergence...")
            current_tensions = self.read_current_tensions()
            
            if not current_tensions:
                print("Warning: No tension data found, continuing to next iteration...")
                continue
            
            # Track history
            self.iteration_history.append({
                'iteration': iteration,
                'timestamp': datetime.now(),
                'tensions': current_tensions.copy(),
                'converged': False
            })
            
            # Check convergence
            if self.check_convergence(current_tensions):
                print(f"\n*** CONVERGED at iteration {iteration} ***")
                self.iteration_history[-1]['converged'] = True
                self.generate_summary()
                return True
            
            if iteration < self.max_iterations:
                print(f"\nNot converged, continuing to iteration {iteration + 1}...")
            
        print(f"\n!!! Maximum iterations ({self.max_iterations}) reached without convergence")
        self.generate_summary()
        return False
    
    def generate_summary(self):
        """Generate convergence summary report."""
        print("\n" + "="*70)
        print("CONVERGENCE SUMMARY")
        print("="*70)
        
        print(f"Total iterations: {len(self.iteration_history)}")
        print(f"End time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        
        if self.iteration_history:
            # Show convergence trend
            print("\nConvergence Trend:")
            print("-" * 50)
            
            for record in self.iteration_history:
                iteration = record['iteration']
                tensions = record['tensions']
                
                converged_count = 0
                for line_name, target in self.target_tensions.items():
                    if line_name in tensions:
                        current = tensions[line_name]
                        tolerance = self.tolerances[line_name]
                        diff_percent = abs(current - target) / target * 100 if target != 0 else 0
                        if diff_percent <= tolerance:
                            converged_count += 1
                
                total_lines = len(self.target_tensions)
                percentage = converged_count/total_lines*100 if total_lines > 0 else 0
                status = "CONVERGED" if record.get('converged', False) else ""
                print(f"  Iteration {iteration}: {converged_count}/{total_lines} lines "
                      f"converged ({percentage:.1f}%) {status}")
                      
            # Final status
            print("-" * 50)
            if self.iteration_history[-1].get('converged', False):
                print("*** SUCCESSFULLY CONVERGED ***")
            else:
                print("XXX DID NOT CONVERGE XXX")
                
        print("="*70)


def main():
    """Main entry point with command-line argument parsing."""
    parser = argparse.ArgumentParser(
        description='Mooring Tension Iteration Orchestrator - Runs batch commands until convergence'
    )
    
    parser.add_argument(
        '--working-dir', '-d',
        type=str,
        default='go-by',
        help='Working directory containing batch files (default: go-by)'
    )
    
    parser.add_argument(
        '--max-iterations', '-m',
        type=int,
        default=10,
        help='Maximum number of iterations (default: 10)'
    )
    
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Enable verbose output'
    )
    
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be executed without running commands'
    )
    
    args = parser.parse_args()
    
    try:
        if args.dry_run:
            print("DRY RUN MODE - Commands will not be executed")
            print(f"Working directory: {args.working_dir}")
            print(f"Max iterations: {args.max_iterations}")
            print("\nCommands that would be executed per iteration:")
            print("1. python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml")
            print("2. python -m digitalmodel.modules.orcaflex.universal pattern=\"fsts*125km3*pb_*.yml\"")
            print("3. python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30")
        else:
            orchestrator = MooringTensionOrchestrator(
                working_dir=args.working_dir,
                max_iterations=args.max_iterations,
                verbose=args.verbose
            )
            success = orchestrator.run()
            sys.exit(0 if success else 1)
            
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()