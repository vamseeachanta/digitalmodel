"""Create Module Agent Command Implementation.

This module implements the /create-module-agent slash command for creating
specialized AI agents within the Agent OS ecosystem.
"""

import re
import yaml
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Any, Optional
from dataclasses import dataclass


@dataclass
class CommandResult:
    """Result of command execution."""
    success: bool
    message: str
    data: Optional[Dict[str, Any]] = None


class ArgumentParser:
    """Parser for create-module-agent command arguments."""

    def parse(self, args: List[str]) -> 'ParsedArgs':
        """Parse command arguments.
        
        Args:
            args: List of command arguments
            
        Returns:
            ParsedArgs object with parsed arguments
            
        Raises:
            ValueError: If arguments are invalid
        """
        if len(args) < 2:
            raise ValueError("Module name is required")
        
        # Skip the command name
        args = args[1:]
        module_name = args[0]
        
        if not self._validate_module_name(module_name):
            raise ValueError(f"Invalid module name: {module_name}")
        
        # Parse options
        parsed_args = ParsedArgs(
            module_name=module_name,
            type="general-purpose",
            repos=[],
            context_cache=True,
            templates=[]
        )
        
        i = 1
        while i < len(args):
            if args[i] == "--type" and i + 1 < len(args):
                parsed_args.type = args[i + 1]
                i += 2
            elif args[i] == "--repos" and i + 1 < len(args):
                parsed_args.repos = [r.strip() for r in args[i + 1].split(",")]
                i += 2
            elif args[i] == "--context-cache" and i + 1 < len(args):
                parsed_args.context_cache = args[i + 1].lower() == "true"
                i += 2
            elif args[i] == "--templates" and i + 1 < len(args):
                parsed_args.templates = [t.strip() for t in args[i + 1].split(",")]
                i += 2
            else:
                i += 1
        
        return parsed_args
    
    def _validate_module_name(self, name: str) -> bool:
        """Validate module name format."""
        if not name:
            return False
        # Allow alphanumeric, hyphens, and underscores
        return bool(re.match(r'^[a-zA-Z0-9_-]+$', name))


@dataclass
class ParsedArgs:
    """Parsed command arguments."""
    module_name: str
    type: str
    repos: List[str]
    context_cache: bool
    templates: List[str]


class AgentStructureGenerator:
    """Generates agent folder structure."""

    def __init__(self, base_dir: Path):
        """Initialize generator.
        
        Args:
            base_dir: Base directory for agents
        """
        self.base_dir = base_dir

    def create_structure(self, module_name: str) -> Path:
        """Create agent folder structure.
        
        Args:
            module_name: Name of the module agent
            
        Returns:
            Path to created agent directory
        """
        agent_dir = self.base_dir / module_name
        
        # Create main directory
        agent_dir.mkdir(parents=True, exist_ok=True)
        
        # Create subdirectories
        subdirs = [
            "context/repository",
            "context/external", 
            "context/optimized",
            "templates/responses",
            "templates/prompts",
            "workflows"
        ]
        
        for subdir in subdirs:
            (agent_dir / subdir).mkdir(parents=True, exist_ok=True)
        
        return agent_dir

    def validate_module_name(self, name: str) -> bool:
        """Validate module name format."""
        if not name:
            return False
        return bool(re.match(r'^[a-zA-Z0-9_-]+$', name))


class ConfigGenerator:
    """Generates agent configuration files."""

    def generate_agent_config(self, module_name: str, agent_type: str,
                            repos: List[str], templates: List[str]) -> Dict[str, Any]:
        """Generate agent.yaml configuration.
        
        Args:
            module_name: Name of the module agent
            agent_type: Type of agent
            repos: List of repository references
            templates: List of template references
            
        Returns:
            Configuration dictionary
        """
        config = {
            "name": module_name,
            "type": agent_type,
            "version": "1.0.0",
            "created_at": datetime.now().isoformat(),
            "description": f"AI agent for {module_name} module",
            "repositories": repos,
            "templates": templates,
            "context_optimization": {
                "enabled": True,
                "cache_ttl": 3600,  # 1 hour
                "embedding_model": "sentence-transformers/all-MiniLM-L6-v2"
            },
            "workflows": {
                "enhanced_specs": {
                    "enabled": True,
                    "auto_update": True,
                    "learning": True
                }
            },
            "documentation": {
                "internal_sources": [],
                "external_sources": [],
                "auto_refresh": True
            }
        }
        
        return config

    def save_config(self, config: Dict[str, Any], path: Path) -> None:
        """Save configuration to YAML file.
        
        Args:
            config: Configuration dictionary
            path: Path to save configuration
        """
        with open(path, 'w') as f:
            yaml.dump(config, f, default_flow_style=False, indent=2)


class DocumentationScanner:
    """Scans repositories for documentation."""

    def scan_repositories(self, repos: List[str]) -> Dict[str, Dict[str, Any]]:
        """Scan repositories for documentation.
        
        Args:
            repos: List of repository names
            
        Returns:
            Dictionary of repository documentation
        """
        # Placeholder implementation
        # In real implementation, this would scan actual repositories
        documentation = {}
        
        for repo in repos:
            documentation[repo] = {
                "docs": [],
                "references": [],
                "api_docs": [],
                "readme": f"README.md for {repo}",
                "last_updated": datetime.now().isoformat()
            }
        
        return documentation


class CreateModuleAgentCommand:
    """Main command implementation."""

    def __init__(self, base_dir: Optional[Path] = None):
        """Initialize command.
        
        Args:
            base_dir: Base directory for agents
        """
        self.base_dir = base_dir or Path.cwd()
        self.agents_dir = self.base_dir / "agents"
        
        # Initialize components
        self.parser = ArgumentParser()
        self.structure_generator = AgentStructureGenerator(self.agents_dir)
        self.config_generator = ConfigGenerator()
        self.doc_scanner = DocumentationScanner()

    def execute(self, args: List[str]) -> CommandResult:
        """Execute the create-module-agent command.
        
        Args:
            args: Command arguments
            
        Returns:
            CommandResult with execution status
        """
        try:
            # Parse arguments
            parsed_args = self.parser.parse(args)
            
            # Create agent structure
            agent_dir = self.structure_generator.create_structure(parsed_args.module_name)
            
            # Generate configuration
            config = self.config_generator.generate_agent_config(
                parsed_args.module_name,
                parsed_args.type,
                parsed_args.repos,
                parsed_args.templates
            )
            
            # Save configuration
            config_path = agent_dir / "agent.yaml"
            self.config_generator.save_config(config, config_path)
            
            # Scan documentation if repositories specified
            if parsed_args.repos:
                documentation = self.doc_scanner.scan_repositories(parsed_args.repos)
                # Save documentation references
                self._save_documentation_references(agent_dir, documentation)
            
            # Create default templates
            self._create_default_templates(agent_dir, parsed_args.type)
            
            # Create workflow integration
            self._create_workflow_integration(agent_dir, parsed_args.module_name, parsed_args.repos)
            
            return CommandResult(
                success=True,
                message=f"Successfully created module agent '{parsed_args.module_name}' at {agent_dir}",
                data={
                    "agent_dir": str(agent_dir),
                    "config": config,
                    "module_name": parsed_args.module_name
                }
            )
            
        except Exception as e:
            return CommandResult(
                success=False,
                message=f"Error creating module agent: {str(e)}"
            )

    def _save_documentation_references(self, agent_dir: Path, 
                                     documentation: Dict[str, Dict[str, Any]]) -> None:
        """Save documentation references to agent directory."""
        references_path = agent_dir / "context" / "repository" / "references.yaml"
        with open(references_path, 'w') as f:
            yaml.dump(documentation, f, default_flow_style=False, indent=2)
    
    def _create_default_templates(self, agent_dir: Path, agent_type: str) -> None:
        """Create default templates for the agent."""
        # Create default response template
        response_template = f"""# {agent_type.title()} Agent Response Template

## Standard Response Format

**Analysis:** [Your analysis here]

**Recommendations:** 
- [Recommendation 1]
- [Recommendation 2]

**Next Steps:**
1. [Step 1]
2. [Step 2]

**Resources:**
- [Resource 1]
- [Resource 2]
"""
        
        response_path = agent_dir / "templates" / "responses" / "default.md"
        with open(response_path, 'w') as f:
            f.write(response_template)
        
        # Create default prompt template
        prompt_template = f"""# {agent_type.title()} Agent Prompt Template

You are a specialized AI agent for {{module_name}} with expertise in {{domain}}.

## Your Role
- Provide expert analysis and recommendations
- Follow established patterns and best practices
- Reference relevant documentation and resources
- Maintain consistency with project standards

## Response Guidelines
- Be concise and actionable
- Include specific examples when helpful
- Reference relevant documentation
- Suggest next steps for implementation

## Context
{{context}}

## Query
{{query}}
"""
        
        prompt_path = agent_dir / "templates" / "prompts" / "default.md"
        with open(prompt_path, 'w') as f:
            f.write(prompt_template)
    
    def _create_workflow_integration(self, agent_dir: Path, module_name: str = None, repos: List[str] = None) -> None:
        """Create enhanced workflow integration configuration."""
        from agent_os.integration.enhanced_specs import EnhancedSpecsIntegration
        
        # Use enhanced specs integration
        integration = EnhancedSpecsIntegration(agent_dir)
        
        # Get module name from agent directory if not provided
        if module_name is None:
            module_name = agent_dir.name
        
        # Default repos if not provided
        if repos is None:
            repos = []
        
        # Create complete integration configuration
        integration_config = integration.integrate(module_name, repos)
        
        # Also create the original simplified config for backward compatibility
        simplified_config = {
            "enhanced_specs": {
                "integration": True,
                "auto_update": True,
                "workflow_refresh": {
                    "enabled": True,
                    "triggers": ["file_change", "time_interval", "manual"],
                    "interval": "1w"
                },
                "learning": {
                    "enabled": True,
                    "pattern_recognition": True,
                    "optimization": True
                }
            }
        }
        
        # Save simplified config for backward compatibility
        workflow_path = agent_dir / "workflows" / "enhanced_specs.yaml"
        workflow_path.parent.mkdir(parents=True, exist_ok=True)
        with open(workflow_path, 'w') as f:
            yaml.dump(simplified_config, f, default_flow_style=False, indent=2)
        
        # Save complete integration config
        full_config_path = agent_dir / "workflows" / "enhanced_specs_full.yaml"
        with open(full_config_path, 'w') as f:
            yaml.dump(integration_config, f, default_flow_style=False, indent=2)


def main():
    """Main entry point for command line usage."""
    import sys
    from agent_os.cli.main import main_cli
    
    # If called directly, use the enhanced CLI
    if len(sys.argv) == 1:
        # No arguments, show help or start interactive
        from agent_os.cli.interactive import InteractiveMode
        interactive = InteractiveMode()
        
        if interactive.get_yes_no("Start interactive mode?", default=True):
            return main_cli()
        else:
            print("Usage: create_module_agent <module_name> [options]")
            return 1
    
    # Legacy support - execute command directly
    command = CreateModuleAgentCommand()
    result = command.execute(sys.argv)
    
    print(result.message)
    return 0 if result.success else 1


if __name__ == "__main__":
    exit(main())