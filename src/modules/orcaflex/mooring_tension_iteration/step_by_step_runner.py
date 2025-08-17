#!/usr/bin/env python
"""
Interactive Step-by-Step Runner for Mooring Tension Iteration
==============================================================
This script provides an interactive way to run through the manual process
step by step, allowing you to see exactly what happens at each stage.
"""

import sys
import time
import logging
from pathlib import Path
from typing import Dict, Optional
import yaml
import json
import pandas as pd
import numpy as np
from datetime import datetime

# Import automation module
from manual_steps_automation import ManualStepsAutomation

# Configure colored output for better visibility
try:
    from colorama import init, Fore, Back, Style
    init(autoreset=True)
    HAS_COLOR = True
except ImportError:
    HAS_COLOR = False
    # Fallback if colorama not installed
    class Fore:
        GREEN = YELLOW = RED = CYAN = BLUE = MAGENTA = WHITE = RESET = ''
    class Style:
        BRIGHT = DIM = NORMAL = RESET_ALL = ''


class InteractiveRunner:
    """Interactive runner for step-by-step execution"""
    
    def __init__(self, config_file: str = 'config.yaml'):
        """Initialize the interactive runner"""
        self.automation = ManualStepsAutomation(config_file)
        self.current_step = 0
        self.steps_completed = []
        self.results = {}
        
    def print_header(self, text: str, level: int = 1):
        """Print formatted header"""
        if level == 1:
            border = "=" * 80
            print(f"\n{Fore.CYAN}{Style.BRIGHT}{border}")
            print(f"{Fore.CYAN}{Style.BRIGHT}{text.center(80)}")
            print(f"{Fore.CYAN}{Style.BRIGHT}{border}{Style.RESET_ALL}")
        elif level == 2:
            border = "-" * 60
            print(f"\n{Fore.YELLOW}{border}")
            print(f"{Fore.YELLOW}{text}")
            print(f"{Fore.YELLOW}{border}{Style.RESET_ALL}")
    
    def print_success(self, text: str):
        """Print success message"""
        print(f"{Fore.GREEN}✓ {text}{Style.RESET_ALL}")
    
    def print_error(self, text: str):
        """Print error message"""
        print(f"{Fore.RED}✗ {text}{Style.RESET_ALL}")
    
    def print_info(self, text: str):
        """Print info message"""
        print(f"{Fore.BLUE}ℹ {text}{Style.RESET_ALL}")
    
    def print_warning(self, text: str):
        """Print warning message"""
        print(f"{Fore.YELLOW}⚠ {text}{Style.RESET_ALL}")
    
    def wait_for_user(self, prompt: str = "Press Enter to continue...") -> str:
        """Wait for user input"""
        return input(f"\n{Fore.MAGENTA}{prompt}{Style.RESET_ALL} ")
    
    def show_main_menu(self):
        """Show the main menu"""
        self.print_header("MOORING TENSION ITERATION - INTERACTIVE RUNNER", 1)
        
        print("\nThis tool will guide you through each step of the mooring tension")
        print("iteration process. You can run steps individually or all at once.")
        
        print(f"\n{Fore.CYAN}Available Options:{Style.RESET_ALL}")
        print("  1. Run Step-by-Step (Interactive)")
        print("  2. Run All Steps (Automated)")
        print("  3. View Current Configuration")
        print("  4. Check Previous Results")
        print("  5. Export Manual Process Guide")
        print("  Q. Quit")
        
        while True:
            choice = input(f"\n{Fore.YELLOW}Select option (1-5, Q): {Style.RESET_ALL}").strip().upper()
            
            if choice == '1':
                self.run_step_by_step()
            elif choice == '2':
                self.run_all_steps()
            elif choice == '3':
                self.view_configuration()
            elif choice == '4':
                self.check_previous_results()
            elif choice == '5':
                self.export_manual_guide()
            elif choice == 'Q':
                print("\nExiting...")
                break
            else:
                self.print_error("Invalid option. Please try again.")
    
    def run_step_by_step(self):
        """Run the process step by step with user interaction"""
        self.print_header("STEP-BY-STEP EXECUTION", 1)
        
        steps = [
            ("Input Preparation", self.run_step1),
            ("Baseline Analysis", self.run_step2),
            ("Result Extraction", self.run_step3),
            ("Length Calculation", self.run_step4),
            ("Convergence Check", self.run_step5)
        ]
        
        for i, (step_name, step_func) in enumerate(steps):
            self.print_header(f"STEP {i+1}: {step_name}", 2)
            
            # Show what this step does
            self.show_step_description(i+1)
            
            # Ask if user wants to proceed
            choice = self.wait_for_user(f"Run Step {i+1}? (Y/n/s to skip): ").strip().lower()
            
            if choice == 'n':
                print("Stopping execution.")
                break
            elif choice == 's':
                self.print_warning(f"Skipping Step {i+1}")
                continue
            
            # Run the step
            try:
                result = step_func()
                self.results[f"step_{i+1}"] = result
                self.steps_completed.append(i+1)
                self.print_success(f"Step {i+1} completed successfully!")
                
                # Show results summary
                self.show_step_results(i+1, result)
                
            except Exception as e:
                self.print_error(f"Error in Step {i+1}: {e}")
                choice = self.wait_for_user("Continue anyway? (y/N): ").strip().lower()
                if choice != 'y':
                    break
        
        # Check if we need to iterate
        if self.automation.converged:
            self.print_success("CONVERGENCE ACHIEVED! Process complete.")
        else:
            self.print_warning("Not converged yet. Need more iterations.")
            choice = self.wait_for_user("Run another iteration? (Y/n): ").strip().lower()
            if choice != 'n':
                self.automation.current_iteration += 1
                self.run_step_by_step()
    
    def show_step_description(self, step_num: int):
        """Show description of what each step does"""
        descriptions = {
            1: """
This step loads the target mooring tensions from CSV files.
Manual Process: Open Excel, read target tensions
Automated: Parse CSV automatically and validate data
            """,
            2: """
This step runs the OrcaFlex static analysis.
Manual Process: Open OrcaFlex, load model, click 'Run Static'
Automated: Execute analysis via Python API
            """,
            3: """
This step extracts tensions from the analysis results.
Manual Process: Run post-processing script, open results CSV
Automated: Extract tensions programmatically
            """,
            4: """
This step calculates required length adjustments.
Manual Process: Use Excel formula ΔL = L/EA × (T_current - T_target)
Automated: Calculate adjustments with damping factor
            """,
            5: """
This step checks if tensions have converged to targets.
Manual Process: Compare tensions in Excel, decide if close enough
Automated: Check all lines against tolerance criterion
            """
        }
        
        if step_num in descriptions:
            print(f"{Fore.CYAN}{descriptions[step_num]}{Style.RESET_ALL}")
    
    def show_step_results(self, step_num: int, result):
        """Show results from each step"""
        print(f"\n{Fore.GREEN}Step {step_num} Results:{Style.RESET_ALL}")
        
        if step_num == 1 and isinstance(result, dict):
            # Show loaded targets
            print(f"  Loaded {len(result)} mooring line targets")
            for i, (line, data) in enumerate(list(result.items())[:3]):
                print(f"    {line}: Target = {data['target_tension']:.1f} kN")
            if len(result) > 3:
                print(f"    ... and {len(result)-3} more lines")
                
        elif step_num == 2 and isinstance(result, str):
            # Show sim file created
            print(f"  Simulation file: {result}")
            
        elif step_num == 3 and isinstance(result, dict):
            # Show extracted tensions
            print(f"  Extracted tensions for {len(result)} lines")
            max_error = 0
            for line in list(result.keys())[:3]:
                current = result[line]
                target = self.automation.targets[line]['target_tension']
                error = abs(current - target) / target * 100
                max_error = max(max_error, error)
                print(f"    {line}: Current = {current:.1f} kN, Error = {error:.1f}%")
            print(f"  Maximum error: {max_error:.1f}%")
            
        elif step_num == 4 and isinstance(result, dict):
            # Show adjustments
            print(f"  Calculated adjustments for {len(result)} lines")
            for i, (line, adj) in enumerate(list(result.items())[:3]):
                if abs(adj) > 0.01:
                    print(f"    {line}: ΔL = {adj:+.3f} m")
                    
        elif step_num == 5 and isinstance(result, bool):
            # Show convergence status
            if result:
                print(f"  {Fore.GREEN}All lines converged!{Style.RESET_ALL}")
            else:
                print(f"  {Fore.YELLOW}Not converged, iteration needed{Style.RESET_ALL}")
    
    def run_step1(self):
        """Run Step 1: Input Preparation"""
        return self.automation.step1_input_preparation()
    
    def run_step2(self):
        """Run Step 2: Baseline Analysis"""
        return self.automation.step2_baseline_analysis()
    
    def run_step3(self):
        """Run Step 3: Result Extraction"""
        sim_file = self.results.get('step_2', 'iteration_00.sim')
        return self.automation.step3_result_extraction(sim_file)
    
    def run_step4(self):
        """Run Step 4: Length Calculation"""
        return self.automation.step4_length_calculation()
    
    def run_step5(self):
        """Run Step 5: Convergence Check"""
        adjustments = self.results.get('step_4', {})
        return self.automation.step5_model_update_iteration(adjustments)
    
    def run_all_steps(self):
        """Run all steps automatically"""
        self.print_header("AUTOMATED EXECUTION", 1)
        print("\nRunning all steps automatically...")
        print("This will continue until convergence or max iterations.\n")
        
        choice = self.wait_for_user("Proceed? (Y/n): ").strip().lower()
        if choice == 'n':
            return
        
        try:
            self.automation.run_complete_workflow()
            self.print_success("Automated execution complete!")
        except Exception as e:
            self.print_error(f"Error during execution: {e}")
    
    def view_configuration(self):
        """View current configuration"""
        self.print_header("CURRENT CONFIGURATION", 1)
        
        config = self.automation.config
        
        print(f"\n{Fore.CYAN}Paths:{Style.RESET_ALL}")
        print(f"  Base path: {config['paths']['base_path']}")
        print(f"  Target file: {config['target_files']['default']}")
        print(f"  Output folder: {config['paths']['output_folder']}")
        
        print(f"\n{Fore.CYAN}Iteration Parameters:{Style.RESET_ALL}")
        print(f"  Max iterations: {config['iteration']['max_iterations']}")
        print(f"  Convergence tolerance: {config['iteration']['convergence_tolerance']*100:.1f}%")
        print(f"  Damping factor: {config['iteration']['damping_factor']}")
        
        print(f"\n{Fore.CYAN}Model Files:{Style.RESET_ALL}")
        print(f"  Static model: {config['models']['static_6dof']}")
        print(f"  Extraction config: {config['models']['extraction_config']}")
        
        self.wait_for_user()
    
    def check_previous_results(self):
        """Check previous iteration results"""
        self.print_header("PREVIOUS RESULTS", 1)
        
        history_file = self.automation.output_folder / 'iteration_history.json'
        
        if history_file.exists():
            with open(history_file, 'r') as f:
                history = json.load(f)
            
            print(f"\nFound {len(history)} iterations in history:")
            for entry in history:
                print(f"\n  Iteration {entry['iteration']}:")
                print(f"    Time: {entry['timestamp']}")
                print(f"    Max error: {entry['max_error']*100:.1f}%")
                print(f"    Converged: {entry['converged']}")
        else:
            print("\nNo previous results found.")
            print("Run the iteration process to generate results.")
        
        self.wait_for_user()
    
    def export_manual_guide(self):
        """Export a guide for manual execution"""
        self.print_header("EXPORT MANUAL PROCESS GUIDE", 1)
        
        guide_file = Path("manual_process_guide.md")
        
        guide_content = """# Manual Process Guide for Mooring Tension Iteration

## Overview
This guide documents the manual steps for mooring tension iteration in OrcaFlex.

## Step 1: Input Preparation
1. Open the target tension CSV file: `*_target_mooring_pretension.csv`
2. Note the target tensions for each mooring line
3. Check if EA values are provided or need extraction from model
4. Optional: Load fender properties from `_target_fender_force.csv`

## Step 2: Baseline Analysis  
1. Open OrcaFlex
2. Load the model file: `fsts_lngc_vessel_statics_6dof.yml`
3. Ensure vessel is in 6DOF mode
4. Run static analysis (Calculation > Calculate Statics)
5. Save the .sim file with iteration number

## Step 3: Result Extraction
1. Run the post-processing script: `dm_ofx_post_fsts_lngc.yml`
2. Open the generated CSV file with tensions
3. Create a comparison table: Current vs Target tensions
4. Calculate percentage errors for each line

## Step 4: Length Calculation
1. For each mooring line, calculate:
   - ΔL = L/EA × (T_current - T_target)
2. Apply damping factor (typically 0.8) to prevent oscillation
3. Generate includefile YAML with new line lengths
4. Save includefile (overwrite previous if iterating)

## Step 5: Model Update & Iteration
1. Load the includefile in OrcaFlex
2. Re-run static analysis
3. Check convergence:
   - If all tensions within tolerance (±1%), STOP
   - If not converged and iterations < max, go to Step 2
   - If max iterations reached, review and adjust approach

## Convergence Criteria
- Tolerance: ±1% of target tension
- Typical convergence: 3-5 iterations
- Maximum iterations: 10 (configurable)

## Tips for Manual Execution
- Keep a spreadsheet to track iterations
- Save .sim files with iteration numbers
- Document any issues or anomalies
- Consider reducing damping if oscillating
- Increase damping if diverging

## Time Estimates
- Step 1: 5 minutes
- Step 2: 10 minutes  
- Step 3: 5 minutes
- Step 4: 10 minutes
- Step 5: 5 minutes
- **Total per iteration: ~35 minutes**
- **Typical total (3-5 iterations): 1.5-3 hours**

## Automation Benefits
Using the automated system reduces this to:
- Setup: 2 minutes
- Execution: 5-10 minutes total
- **Time saved: 80-95%**
"""
        
        with open(guide_file, 'w') as f:
            f.write(guide_content)
        
        self.print_success(f"Manual guide exported to: {guide_file}")
        print("\nThis guide can be used for:")
        print("  - Training new users on the manual process")
        print("  - Understanding what the automation does")
        print("  - Fallback if automation is unavailable")
        
        self.wait_for_user()


def main():
    """Main entry point"""
    # Check for config file
    config_file = 'config.yaml'
    if not Path(config_file).exists():
        print(f"{Fore.RED}ERROR: {config_file} not found!{Style.RESET_ALL}")
        print("Please ensure config.yaml exists in the current directory.")
        return 1
    
    try:
        # Create and run interactive runner
        runner = InteractiveRunner(config_file)
        runner.show_main_menu()
        return 0
        
    except KeyboardInterrupt:
        print(f"\n{Fore.YELLOW}Interrupted by user{Style.RESET_ALL}")
        return 1
    except Exception as e:
        print(f"{Fore.RED}Error: {e}{Style.RESET_ALL}")
        return 1


if __name__ == "__main__":
    sys.exit(main())