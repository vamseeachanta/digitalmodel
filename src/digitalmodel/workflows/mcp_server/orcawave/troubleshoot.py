#!/usr/bin/env python
"""
OrcaWave MCP Troubleshooting Tool
=================================
Interactive troubleshooting for OrcaWave analysis issues
"""

import sys
from pathlib import Path
from datetime import datetime
import json

# Simple console output without rich dependency
class SimpleTroubleshooter:
    """Simplified troubleshooter for OrcaWave runs"""
    
    def __init__(self):
        self.issues_found = []
        self.recommendations = []
        
    def display_banner(self):
        """Display welcome banner"""
        print("=" * 60)
        print("OrcaWave MCP Troubleshooter")
        print("Quick diagnostics for OrcaWave analysis issues")
        print("=" * 60)
        print()
        
    def analyze_common_issues(self):
        """Analyze common OrcaWave issues"""
        print("Common OrcaWave Issues:")
        print("-" * 40)
        
        issues = {
            "1": {
                "name": "Mesh Quality Issues",
                "symptoms": ["Slow convergence", "Non-physical results", "Warnings in log"],
                "fix": "Use mesh optimizer with target quality > 0.85"
            },
            "2": {
                "name": "Frequency Range Problems",
                "symptoms": ["Missing resonance", "Incomplete RAOs", "Poor QTF"],
                "fix": "Extend frequency range (0.1-3.0 rad/s) or increase resolution"
            },
            "3": {
                "name": "Convergence Failures",
                "symptoms": ["Analysis doesn't complete", "Oscillating residuals"],
                "fix": "Improve mesh quality, adjust solver parameters"
            },
            "4": {
                "name": "Memory/Performance",
                "symptoms": ["Out of memory", "Crash during QTF", "Very slow"],
                "fix": "Reduce panel count with symmetry, disable full QTF"
            },
            "5": {
                "name": "License Issues",
                "symptoms": ["Cannot start OrcaWave", "Features unavailable"],
                "fix": "Check license server connection and available tokens"
            }
        }
        
        # Display issues
        for key, details in issues.items():
            print(f"\n{key}. {details['name']}")
            for symptom in details['symptoms']:
                print(f"   • {symptom}")
                
        print("\nWhich issues are you experiencing? (comma-separated numbers or 'all'): ", end="")
        selection = input().strip()
        
        if selection.lower() == "all":
            selected = list(issues.keys())
        else:
            selected = [s.strip() for s in selection.split(",") if s.strip() in issues]
            
        print("\n" + "=" * 60)
        print("RECOMMENDATIONS:")
        print("=" * 60)
        
        for key in selected:
            details = issues[key]
            print(f"\n{details['name']}:")
            print(f"  Fix: {details['fix']}")
            self.recommendations.append(details['fix'])
            
    def generate_diagnostic_script(self):
        """Generate a diagnostic script"""
        print("\n" + "=" * 60)
        print("DIAGNOSTIC SCRIPT GENERATED")
        print("=" * 60)
        
        script_content = f'''# OrcaWave Diagnostic Script
# Generated: {datetime.now().isoformat()}
# Save this as: orcawave_diagnostic.py

import win32com.client
from pathlib import Path

def diagnose(model_path):
    """Diagnose OrcaWave model"""
    try:
        app = win32com.client.Dispatch("OrcaWave.Application")
        app.Visible = True
        model = app.OpenDocument(str(model_path))
        
        mesh = model.Mesh
        print(f"Panel count: {{mesh.PanelCount}}")
        print(f"Node count: {{mesh.NodeCount}}")
        
        freq = model.Frequencies
        print(f"Frequency range: {{freq.Min}}-{{freq.Max}} rad/s")
        print(f"Frequency count: {{freq.Count}}")
        
        env = model.Environment
        print(f"Water depth: {{env.WaterDepth}} m")
        
        # Recommendations
        if mesh.PanelCount > 10000:
            print("WARNING: Consider using symmetry to reduce panels")
        if freq.Max < 2.0:
            print("WARNING: Extend frequency range for high-freq response")
            
        app.Quit()
        return True
        
    except Exception as e:
        print(f"Error: {{e}}")
        return False

if __name__ == "__main__":
    model_path = input("Enter OrcaWave model path (.owd): ")
    diagnose(Path(model_path))
'''
        
        # Save to file
        script_path = Path("orcawave_diagnostic.py")
        script_path.write_text(script_content)
        
        print(f"\nScript saved to: {script_path}")
        print(f"Run with: python {script_path}")
        
    def suggest_mcp_commands(self):
        """Suggest MCP commands to use"""
        print("\n" + "=" * 60)
        print("AVAILABLE MCP COMMANDS")
        print("=" * 60)
        
        commands = [
            ("Mesh Optimization", "python -m mcp.orcawave mesh-optimize <model.owd>"),
            ("Frequency Analysis", "python -m mcp.orcawave freq-analyze <model.owd>"),
            ("Convergence Monitor", "python -m mcp.orcawave monitor <model.owd>"),
            ("Validation Suite", "python -m mcp.orcawave validate <model.owd>"),
            ("Batch Processing", "python -m mcp.orcawave batch <config.yml>"),
            ("Quick Fix All Issues", "python -m mcp.orcawave quick-fix <model.owd>")
        ]
        
        for name, cmd in commands:
            print(f"\n{name}:")
            print(f"  {cmd}")
            
    def create_config_template(self):
        """Create configuration template"""
        print("\n" + "=" * 60)
        print("CONFIGURATION TEMPLATE")
        print("=" * 60)
        
        config = {
            "vessel": {
                "name": "your_vessel",
                "model_path": "path/to/model.owd"
            },
            "mesh": {
                "target_quality": 0.85,
                "waterline_refinement": True,
                "use_symmetry": True
            },
            "frequencies": {
                "min": 0.1,
                "max": 3.0,
                "resolution": 0.05
            },
            "environment": {
                "water_depth": 1000,
                "wave_directions": [0, 45, 90, 135, 180],
                "current_speed": 0.0
            },
            "solver": {
                "max_iterations": 100,
                "tolerance": 1e-6,
                "use_gpu": False
            },
            "output": {
                "format": "excel",
                "include_qtf": True,
                "include_plots": True
            }
        }
        
        config_path = Path("orcawave_config.yml")
        
        # Save as YAML
        try:
            import yaml
            with open(config_path, 'w') as f:
                yaml.dump(config, f, default_flow_style=False)
            print(f"\nConfiguration template saved to: {config_path}")
        except ImportError:
            # Save as JSON if YAML not available
            config_path = Path("orcawave_config.json")
            with open(config_path, 'w') as f:
                json.dump(config, f, indent=2)
            print(f"\nConfiguration template saved to: {config_path}")
            
        print("Edit this file and use with: python -m mcp.orcawave batch <config>")
        
    def run(self, auto_mode=False):
        """Run troubleshooting session"""
        self.display_banner()
        
        if auto_mode:
            # Run all functions in demo mode
            print("[AUTO MODE - Demonstrating all features]\n")
            self.suggest_mcp_commands()
            self.create_config_template()
            return 0
        
        while True:
            print("\nOptions:")
            print("1. Analyze common issues")
            print("2. Generate diagnostic script")
            print("3. Show MCP commands")
            print("4. Create config template")
            print("5. Exit")
            print("\nSelect option (1-5): ", end="")
            
            try:
                choice = input().strip()
            except EOFError:
                print("\nNo input available. Running in demo mode...")
                self.suggest_mcp_commands()
                return 0
            
            if choice == "1":
                self.analyze_common_issues()
            elif choice == "2":
                self.generate_diagnostic_script()
            elif choice == "3":
                self.suggest_mcp_commands()
            elif choice == "4":
                self.create_config_template()
            elif choice == "5":
                print("\nGoodbye!")
                break
            else:
                print("Invalid option. Please select 1-5.")
                
        return 0


def main():
    """Main entry point"""
    troubleshooter = SimpleTroubleshooter()
    
    # Check for auto mode
    auto_mode = "--auto" in sys.argv or "--demo" in sys.argv
    
    try:
        return troubleshooter.run(auto_mode=auto_mode)
    except KeyboardInterrupt:
        print("\n\nTroubleshooting cancelled.")
        return 1
    except Exception as e:
        print(f"\nError: {e}")
        if "--debug" in sys.argv:
            import traceback
            traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())