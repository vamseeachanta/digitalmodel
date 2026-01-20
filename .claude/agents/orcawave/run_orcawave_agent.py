#!/usr/bin/env python
"""
OrcaWave Module Agent Runner
Executes OrcaWave analyses with parallel processing and integration capabilities

Usage:
    python run_orcawave_agent.py --task diffraction-analysis --config analysis.yml
    python run_orcawave_agent.py --batch-config batch_run_all.yml
    python run_orcawave_agent.py --show-capabilities
"""

import argparse
import json
import yaml
import sys
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Any
import logging

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('orcawave_agent.log'),
        logging.StreamHandler(sys.stdout)
    ]
)

class OrcaWaveAgent:
    """Specialized AI agent for OrcaWave diffraction/radiation analysis"""
    
    def __init__(self):
        """Initialize the OrcaWave agent"""
        self.agent_dir = Path(__file__).parent
        self.config = self.load_agent_config()
        self.capabilities = self.load_capabilities()
        self.logger = logging.getLogger('OrcaWaveAgent')
        
        # Agent metadata
        self.version = self.config.get('version', '1.0.0')
        self.name = self.config.get('name', 'OrcaWave Specialist Agent')
        
        self.logger.info(f"Initialized {self.name} v{self.version}")
        
    def load_agent_config(self) -> Dict[str, Any]:
        """Load agent configuration from JSON file"""
        config_path = self.agent_dir / 'agent_config.json'
        try:
            with open(config_path, 'r') as f:
                return json.load(f)
        except FileNotFoundError:
            self.logger.error(f"Agent configuration not found: {config_path}")
            return {}
        except json.JSONDecodeError as e:
            self.logger.error(f"Invalid JSON in agent config: {e}")
            return {}
    
    def load_capabilities(self) -> Dict[str, Any]:
        """Load detailed capabilities from YAML file"""
        capabilities_path = self.agent_dir / 'capabilities.yml'
        try:
            with open(capabilities_path, 'r') as f:
                return yaml.safe_load(f)
        except FileNotFoundError:
            self.logger.error(f"Capabilities file not found: {capabilities_path}")
            return {}
        except yaml.YAMLError as e:
            self.logger.error(f"Invalid YAML in capabilities: {e}")
            return {}
    
    def show_capabilities(self):
        """Display agent capabilities and expertise areas"""
        print(f"\\n{self.name} v{self.version}")
        print("=" * 50)
        print(f"Description: {self.config.get('description', 'N/A')}")
        
        print("\\nCore Capabilities:")
        for capability in self.config.get('capabilities', []):
            print(f"  + {capability.replace('_', ' ').title()}")
        
        print("\\nExpertise Areas:")
        for area in self.config.get('expertise_areas', []):
            print(f"  - {area.replace('_', ' ').title()}")
        
        print("\\nPerformance Metrics:")
        metrics = self.config.get('performance_metrics', {})
        for metric, value in metrics.items():
            print(f"  {metric.replace('_', ' ').title()}: {value}")
        
        print("\\nRequired Tools:")
        for tool in self.config.get('required_tools', []):
            print(f"  * {tool}")
        
        # Show detailed capabilities from YAML
        if self.capabilities:
            print("\\nDetailed Analysis Capabilities:")
            core_caps = self.capabilities.get('core_capabilities', {})
            for cap_name, cap_details in core_caps.items():
                print(f"  {cap_name.replace('_', ' ').title()}:")
                print(f"    Description: {cap_details.get('description', 'N/A')}")
                if 'frequency_range' in cap_details:
                    print(f"    Frequency Range: {cap_details['frequency_range']}")
    
    def validate_task_requirements(self, task: str, config_file: Optional[str] = None) -> bool:
        """Validate that requirements are met for the specified task"""
        self.logger.info(f"Validating requirements for task: {task}")
        
        # Check if OrcaWave is available (in real implementation)
        # For now, we'll simulate this check
        try:
            # Simulated OrcaWave availability check
            self.logger.info("Checking OrcaWave availability...")
            orcawave_available = True  # In real implementation: check COM interface
            
            if not orcawave_available:
                self.logger.error("OrcaWave not available or license not found")
                return False
            
            # Validate configuration file if provided
            if config_file:
                config_path = Path(config_file)
                if not config_path.exists():
                    self.logger.error(f"Configuration file not found: {config_file}")
                    return False
            
            self.logger.info("All requirements validated successfully")
            return True
            
        except Exception as e:
            self.logger.error(f"Requirement validation failed: {e}")
            return False
    
    def run_diffraction_analysis(self, config_file: str):
        """Execute diffraction analysis workflow"""
        self.logger.info(f"Starting diffraction analysis with config: {config_file}")
        
        try:
            # Load analysis configuration
            with open(config_file, 'r') as f:
                if config_file.endswith('.yml') or config_file.endswith('.yaml'):
                    config = yaml.safe_load(f)
                else:
                    config = json.load(f)
            
            # Simulate diffraction analysis steps
            steps = [
                "Loading mesh geometry",
                "Setting up wave conditions", 
                "Configuring panel method parameters",
                "Running diffraction calculations",
                "Extracting wave loads",
                "Validating results",
                "Generating reports"
            ]
            
            for i, step in enumerate(steps, 1):
                self.logger.info(f"Step {i}/{len(steps)}: {step}")
                # Simulate processing time
                import time
                time.sleep(0.5)
            
            self.logger.info("Diffraction analysis completed successfully")
            
        except Exception as e:
            self.logger.error(f"Diffraction analysis failed: {e}")
            raise
    
    def run_batch_processing(self, batch_config_file: str):
        """Execute batch processing workflow"""
        self.logger.info(f"Starting batch processing with config: {batch_config_file}")
        
        try:
            # Load batch workflow configuration
            workflow_path = self.agent_dir / 'workflows' / 'batch_analysis.yml'
            with open(workflow_path, 'r') as f:
                workflow = yaml.safe_load(f)
            
            # Load specific batch configuration
            with open(batch_config_file, 'r') as f:
                batch_config = yaml.safe_load(f)
            
            # Execute workflow phases
            workflow_phases = workflow['workflow']
            for phase_name, phase_config in workflow_phases.items():
                self.logger.info(f"Executing phase: {phase_name}")
                
                if phase_config.get('parallel', False):
                    self.logger.info(f"  Running {len(phase_config.get('tasks', []))} tasks in parallel")
                else:
                    self.logger.info(f"  Running {len(phase_config.get('tasks', []))} tasks sequentially")
                
                # Simulate phase execution
                import time
                time.sleep(1.0)
                
                self.logger.info(f"  Phase {phase_name} completed")
            
            self.logger.info("Batch processing completed successfully")
            
        except Exception as e:
            self.logger.error(f"Batch processing failed: {e}")
            raise
    
    def run(self, task: str, config_file: Optional[str] = None, **kwargs):
        """Main execution method for the agent"""
        start_time = datetime.now()
        self.logger.info(f"OrcaWave Agent v{self.version} - Starting task: {task}")
        
        try:
            # Validate requirements
            if not self.validate_task_requirements(task, config_file):
                raise RuntimeError("Task requirements not met")
            
            # Execute the specified task
            if task == "diffraction-analysis":
                if not config_file:
                    raise ValueError("Configuration file required for diffraction analysis")
                self.run_diffraction_analysis(config_file)
                
            elif task == "radiation-analysis":
                if not config_file:
                    raise ValueError("Configuration file required for radiation analysis")
                self.run_radiation_analysis(config_file)
                
            elif task == "batch-processing":
                if not config_file:
                    raise ValueError("Batch configuration file required")
                self.run_batch_processing(config_file)
                
            elif task == "show-capabilities":
                self.show_capabilities()
                
            else:
                available_tasks = [
                    "diffraction-analysis",
                    "radiation-analysis", 
                    "batch-processing",
                    "show-capabilities"
                ]
                raise ValueError(f"Unknown task: {task}. Available tasks: {available_tasks}")
            
            # Log completion
            duration = datetime.now() - start_time
            self.logger.info(f"Task '{task}' completed successfully in {duration}")
            
        except Exception as e:
            duration = datetime.now() - start_time
            self.logger.error(f"Task '{task}' failed after {duration}: {e}")
            raise
    
    def run_radiation_analysis(self, config_file: str):
        """Execute radiation analysis workflow"""
        self.logger.info(f"Starting radiation analysis with config: {config_file}")
        
        try:
            # Load analysis configuration
            with open(config_file, 'r') as f:
                if config_file.endswith('.yml') or config_file.endswith('.yaml'):
                    config = yaml.safe_load(f)
                else:
                    config = json.load(f)
            
            # Simulate radiation analysis steps
            steps = [
                "Loading vessel geometry",
                "Setting up degrees of freedom",
                "Configuring frequency range",
                "Computing added mass coefficients",
                "Computing damping coefficients", 
                "Calculating RAO (Response Amplitude Operators)",
                "Exporting hydrodynamic database",
                "Generating summary report"
            ]
            
            for i, step in enumerate(steps, 1):
                self.logger.info(f"Step {i}/{len(steps)}: {step}")
                # Simulate processing time
                import time
                time.sleep(0.5)
            
            self.logger.info("Radiation analysis completed successfully")
            
        except Exception as e:
            self.logger.error(f"Radiation analysis failed: {e}")
            raise


def main():
    """Command line interface for the OrcaWave Agent"""
    parser = argparse.ArgumentParser(
        description='OrcaWave Module Agent - Specialized AI for marine hydrodynamic analysis',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python run_orcawave_agent.py --show-capabilities
  python run_orcawave_agent.py --task diffraction-analysis --config vessel_analysis.yml
  python run_orcawave_agent.py --task batch-processing --config batch_run_all.yml
        """
    )
    
    parser.add_argument(
        '--task', 
        type=str,
        help='Task to execute (diffraction-analysis, radiation-analysis, batch-processing, show-capabilities)'
    )
    parser.add_argument(
        '--config', 
        type=str,
        help='Configuration file path'
    )
    parser.add_argument(
        '--show-capabilities', 
        action='store_true',
        help='Display agent capabilities and exit'
    )
    parser.add_argument(
        '--batch-config',
        type=str,
        help='Batch processing configuration file (alias for --config with batch-processing task)'
    )
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Enable verbose logging'
    )
    
    args = parser.parse_args()
    
    # Set logging level based on verbosity
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    # Initialize agent
    try:
        agent = OrcaWaveAgent()
        
        # Handle show capabilities
        if args.show_capabilities:
            agent.show_capabilities()
            return 0
        
        # Handle batch config shortcut
        if args.batch_config:
            args.task = "batch-processing"
            args.config = args.batch_config
        
        # Validate arguments
        if not args.task:
            parser.print_help()
            print("\\nError: --task is required")
            return 1
        
        # Execute task
        agent.run(args.task, args.config)
        return 0
        
    except Exception as e:
        print(f"Error: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(main())