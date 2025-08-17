#!/usr/bin/env python3
"""
OrcaFlex Agent Runner for Mooring Tension Iteration
Uses the OrcaFlex module-based agent for automated analysis
This file belongs in: src/modules/orcaflex/mooring_tension_iteration/
"""

import os
import sys
import yaml
import argparse
from pathlib import Path
from datetime import datetime
import logging

# Add project root to path
project_root = Path(__file__).parent.parent.parent.parent.parent
sys.path.insert(0, str(project_root))

# Import the batch runner
from src.modules.orcaflex.mooring_tension_iteration.batch_processing.orcaflex_batch_runner import (
    OrcaFlexBatchRunner
)


class OrcaFlexAgent:
    """OrcaFlex module-based agent for mooring tension iteration"""
    
    def __init__(self, config_path=None):
        """Initialize the OrcaFlex agent"""
        self.agent_dir = project_root / "agents" / "orcaflex"
        self.workflow_dir = self.agent_dir / "workflows"
        self.config = self._load_config(config_path)
        self.logger = self._setup_logging()
        
        # Load agent configuration
        self.agent_config = self._load_agent_config()
        self.workflow_config = self._load_workflow_config()
        
    def _load_config(self, config_path):
        """Load configuration from file or use defaults"""
        if config_path and Path(config_path).exists():
            with open(config_path, 'r') as f:
                return yaml.safe_load(f)
        else:
            # Default configuration
            return {
                'base_directory': 'D:/1522/ctr7/orcaflex/rev_a08/base_files/fsts_lngc_pretension',
                'parallel_processing': True,
                'section_to_modify': 2,
                'convergence_tolerance': 0.01,
                'damping_factor': 0.7,
                'max_iterations': 10
            }
    
    def _load_agent_config(self):
        """Load the OrcaFlex agent configuration"""
        agent_yaml = self.agent_dir / "agent.yaml"
        if agent_yaml.exists():
            with open(agent_yaml, 'r') as f:
                return yaml.safe_load(f)
        return {}
    
    def _load_workflow_config(self):
        """Load the mooring tension iteration workflow"""
        workflow_yaml = self.workflow_dir / "mooring_tension_iteration.yaml"
        if workflow_yaml.exists():
            with open(workflow_yaml, 'r') as f:
                return yaml.safe_load(f)
        return {}
    
    def _setup_logging(self):
        """Set up logging for the agent"""
        logging.basicConfig(
            level=logging.INFO,
            format='[%(asctime)s] %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler('orcaflex_agent.log'),
                logging.StreamHandler()
            ]
        )
        return logging.getLogger(__name__)
    
    def run_batch_analysis(self, batch_config_file, mock_mode=False):
        """Run batch analysis using the agent workflow"""
        self.logger.info("=" * 80)
        self.logger.info("OrcaFlex Agent - Mooring Tension Iteration")
        self.logger.info("=" * 80)
        self.logger.info(f"Agent Version: {self.agent_config.get('version', '2.0.0')}")
        self.logger.info(f"Workflow: {self.workflow_config.get('workflow', {}).get('name', 'mooring_tension_iteration')}")
        self.logger.info(f"Batch Config: {batch_config_file}")
        self.logger.info("-" * 80)
        
        # Use the OrcaFlexBatchRunner for actual processing
        runner = OrcaFlexBatchRunner(batch_config_file, mock_mode=mock_mode)
        summary = runner.run_batch()
        
        # Log summary
        self.logger.info("=" * 80)
        self.logger.info("AGENT PROCESSING COMPLETE")
        self.logger.info(f"Total Models: {summary['total_models']}")
        self.logger.info(f"Successful: {summary['successful']}")
        self.logger.info(f"Failed: {summary['failed']}")
        self.logger.info(f"Success Rate: {summary['success_rate']:.1f}%")
        self.logger.info("=" * 80)
        
        return summary
    
    def show_capabilities(self):
        """Display agent capabilities"""
        print("\n" + "=" * 80)
        print("OrcaFlex Module Agent - Capabilities")
        print("=" * 80)
        
        if self.agent_config:
            print(f"\nAgent Version: {self.agent_config.get('version', '2.0.0')}")
            print(f"Description: {self.agent_config.get('description', 'N/A')}")
            
            print("\nDomain Expertise:")
            for expertise in self.agent_config.get('specialization', {}).get('expertise', []):
                print(f"  • {expertise}")
            
            print("\nSupported Standards:")
            for standard in self.agent_config.get('specialization', {}).get('standards', []):
                print(f"  • {standard}")
        
        if self.workflow_config:
            workflow = self.workflow_config.get('workflow', {})
            print(f"\nWorkflow: {workflow.get('name', 'N/A')}")
            print(f"Description: {workflow.get('description', 'N/A')}")
            
            print("\nCapabilities:")
            for capability in workflow.get('capabilities', []):
                print(f"  • {capability}")
            
            print("\nConfiguration:")
            config = workflow.get('configuration', {})
            print(f"  • Section to modify: Length[{config.get('section_to_modify', 2)}]")
            print(f"  • Convergence tolerance: {config.get('convergence_tolerance', 0.01)*100}%")
            print(f"  • Damping factor: {config.get('damping_factor', 0.7)}")
            print(f"  • Max iterations: {config.get('max_iterations', 10)}")
        
        print("=" * 80 + "\n")


def main():
    """Main entry point for the OrcaFlex agent"""
    parser = argparse.ArgumentParser(
        description='OrcaFlex Module Agent - Mooring Tension Iteration'
    )
    
    parser.add_argument(
        '--batch-config',
        help='Path to batch configuration YAML file',
        default='batch_run_all_fsts.yml'
    )
    
    parser.add_argument(
        '--show-capabilities',
        action='store_true',
        help='Show agent capabilities and exit'
    )
    
    parser.add_argument(
        '--mock',
        action='store_true',
        help='Run in mock mode without OrcaFlex license'
    )
    
    parser.add_argument(
        '--config',
        help='Path to agent configuration file',
        default=None
    )
    
    args = parser.parse_args()
    
    # Initialize the agent
    agent = OrcaFlexAgent(config_path=args.config)
    
    if args.show_capabilities:
        agent.show_capabilities()
    else:
        # Run batch analysis
        summary = agent.run_batch_analysis(args.batch_config, mock_mode=args.mock)
        
        # Return appropriate exit code
        sys.exit(0 if summary['failed'] == 0 else 1)


if __name__ == "__main__":
    main()