"""Enhanced Specs Integration for Agent OS.

This module integrates module agents with the enhanced create-specs workflow,
providing prompt evolution tracking, executive summaries, mermaid diagrams,
and cross-repository references.
"""

import yaml
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, List, Optional
from dataclasses import dataclass, field


@dataclass
class EnhancedSpecsConfig:
    """Configuration for enhanced specs integration."""
    
    enabled: bool = True
    auto_update: bool = True
    workflow_refresh: Dict[str, Any] = field(default_factory=lambda: {
        "enabled": True,
        "triggers": ["file_change", "time_interval", "manual"],
        "interval": "1w"
    })
    learning: Dict[str, Any] = field(default_factory=lambda: {
        "enabled": True,
        "pattern_recognition": True,
        "optimization": True
    })
    features: Dict[str, bool] = field(default_factory=lambda: {
        "prompt_evolution": True,
        "executive_summaries": True,
        "mermaid_diagrams": True,
        "task_tracking": True,
        "cross_repo_references": True
    })


class EnhancedSpecsIntegration:
    """Integrates agents with enhanced create-specs workflow."""
    
    def __init__(self, agent_path: Path):
        """Initialize enhanced specs integration.
        
        Args:
            agent_path: Path to the agent directory
        """
        self.agent_path = agent_path
        self.workflows_dir = agent_path / "workflows"
        self.config = self._load_or_create_config()
        
    def _load_or_create_config(self) -> EnhancedSpecsConfig:
        """Load existing config or create default."""
        config_path = self.workflows_dir / "enhanced_specs.yaml"
        
        if config_path.exists():
            with open(config_path, 'r') as f:
                config_data = yaml.safe_load(f)
                return EnhancedSpecsConfig(**config_data.get('enhanced_specs', {}))
        else:
            return EnhancedSpecsConfig()
    
    def create_enhanced_specs_config(self, module_name: str, 
                                    repos: List[str]) -> Dict[str, Any]:
        """Create enhanced specs configuration for an agent.
        
        Args:
            module_name: Name of the module
            repos: List of repository references
            
        Returns:
            Configuration dictionary for enhanced specs
        """
        config = {
            "integration": {
                "type": "enhanced_create_specs",
                "version": "2.0.0",
                "source": "@assetutilities:src/modules/agent-os/enhanced-create-specs/"
            },
            "configuration": {
                "agent_module": module_name,
                "agent_path": f"@agents/{module_name}/",
                "features": self.config.features,
                "repositories": repos
            },
            "workflow_refresh": self.config.workflow_refresh,
            "learning": self.config.learning,
            "hooks": {
                "pre_spec_creation": "validate_context",
                "post_spec_creation": "update_references",
                "pre_task_execution": "prepare_environment",
                "post_task_execution": "capture_learnings"
            }
        }
        
        # Add prompt evolution tracking config
        if self.config.features.get("prompt_evolution"):
            config["prompt_evolution"] = {
                "enabled": True,
                "tracking_file": f"@agents/{module_name}/context/prompt_evolution.md",
                "summary_format": "structured",
                "optimization_threshold": 0.7
            }
        
        # Add executive summary config  
        if self.config.features.get("executive_summaries"):
            config["executive_summaries"] = {
                "enabled": True,
                "auto_generate": True,
                "include_metrics": True,
                "format": "markdown"
            }
        
        # Add mermaid diagram config
        if self.config.features.get("mermaid_diagrams"):
            config["mermaid_diagrams"] = {
                "enabled": True,
                "auto_generate": True,
                "types": ["flowchart", "sequence", "class", "state"],
                "output_dir": f"@agents/{module_name}/diagrams/"
            }
        
        # Add task tracking config
        if self.config.features.get("task_tracking"):
            config["task_tracking"] = {
                "enabled": True,
                "integration": "agent_os_tasks",
                "auto_decompose": True,
                "max_subtask_depth": 3
            }
        
        # Add cross-repo references config
        if self.config.features.get("cross_repo_references"):
            config["cross_repo_references"] = {
                "enabled": True,
                "repositories": repos,
                "auto_link": True,
                "cache_references": True,
                "refresh_interval": "24h"
            }
        
        return config
    
    def save_config(self, config: Dict[str, Any]) -> None:
        """Save enhanced specs configuration.
        
        Args:
            config: Configuration dictionary to save
        """
        config_path = self.workflows_dir / "enhanced_specs.yaml"
        
        # Ensure directory exists
        self.workflows_dir.mkdir(parents=True, exist_ok=True)
        
        with open(config_path, 'w') as f:
            yaml.dump(config, f, default_flow_style=False, indent=2)
    
    def connect_to_create_specs(self, module_name: str) -> Dict[str, Any]:
        """Connect agent to create-specs workflow.
        
        Args:
            module_name: Name of the module
            
        Returns:
            Connection configuration
        """
        connection = {
            "workflow": "create-specs",
            "module": module_name,
            "integration_points": [
                {
                    "trigger": "/create-spec",
                    "handler": f"@agents/{module_name}/handlers/create_spec_handler.py",
                    "config": f"@agents/{module_name}/workflows/enhanced_specs.yaml"
                },
                {
                    "trigger": "/execute-tasks",
                    "handler": f"@agents/{module_name}/handlers/execute_tasks_handler.py",
                    "config": f"@agents/{module_name}/workflows/enhanced_specs.yaml"
                }
            ],
            "data_flow": {
                "input": {
                    "spec_request": "user input",
                    "context": f"@agents/{module_name}/context/",
                    "templates": f"@agents/{module_name}/templates/"
                },
                "processing": {
                    "prompt_evolution": "track and optimize",
                    "task_decomposition": "automatic",
                    "cross_references": "resolve and validate"
                },
                "output": {
                    "spec": "@specs/modules/{module}/",
                    "tasks": "@specs/modules/{module}/tasks.md",
                    "documentation": "@specs/modules/{module}/sub-specs/",
                    "summaries": f"@agents/{module_name}/summaries/"
                }
            }
        }
        
        return connection
    
    def setup_workflow_refresh(self, module_name: str) -> Dict[str, Any]:
        """Setup workflow refresh integration.
        
        Args:
            module_name: Name of the module
            
        Returns:
            Workflow refresh configuration
        """
        refresh_config = {
            "enabled": True,
            "module": module_name,
            "triggers": [
                {
                    "type": "file_change",
                    "paths": [
                        f"src/assetutilities/agent_os/agents/{module_name}/",
                        f"specs/modules/{module_name}/"
                    ],
                    "debounce": 5  # seconds
                },
                {
                    "type": "time_interval",
                    "interval": self.config.workflow_refresh.get("interval", "1w")
                },
                {
                    "type": "manual",
                    "command": f"/refresh-workflow {module_name}"
                },
                {
                    "type": "execution_complete",
                    "condition": "task_completed"
                }
            ],
            "actions": [
                "detect_changes",
                "update_spec",
                "validate_updates",
                "execute_if_changed",
                "capture_learnings",
                "update_context"
            ],
            "learning": {
                "enabled": self.config.learning.get("enabled", True),
                "pattern_recognition": self.config.learning.get("pattern_recognition", True),
                "optimization": self.config.learning.get("optimization", True),
                "feedback_loop": True
            },
            "monitoring": {
                "track_executions": True,
                "measure_performance": True,
                "detect_patterns": True,
                "generate_insights": True
            }
        }
        
        return refresh_config
    
    def create_integration_hooks(self, module_name: str) -> Dict[str, Any]:
        """Create integration hooks for the workflow.
        
        Args:
            module_name: Name of the module
            
        Returns:
            Hooks configuration
        """
        hooks = {
            "pre_spec_creation": {
                "handler": "validate_context",
                "actions": [
                    "load_agent_context",
                    "resolve_references",
                    "prepare_templates"
                ]
            },
            "post_spec_creation": {
                "handler": "update_references",
                "actions": [
                    "update_cross_references",
                    "generate_diagrams",
                    "create_summaries"
                ]
            },
            "pre_task_execution": {
                "handler": "prepare_environment",
                "actions": [
                    "validate_dependencies",
                    "setup_monitoring",
                    "initialize_tracking"
                ]
            },
            "post_task_execution": {
                "handler": "capture_learnings",
                "actions": [
                    "track_prompt_evolution",
                    "measure_performance",
                    "update_context",
                    "generate_report"
                ]
            },
            "on_workflow_completion": {
                "handler": "finalize_workflow",
                "actions": [
                    "generate_documentation",
                    "create_evolution_report",
                    "update_agent_learning",
                    "schedule_next_refresh"
                ]
            }
        }
        
        return hooks
    
    def integrate(self, module_name: str, repos: List[str]) -> Dict[str, Any]:
        """Perform complete integration with enhanced specs workflow.
        
        Args:
            module_name: Name of the module
            repos: List of repository references
            
        Returns:
            Complete integration configuration
        """
        # Create enhanced specs config
        enhanced_config = self.create_enhanced_specs_config(module_name, repos)
        
        # Setup workflow refresh
        refresh_config = self.setup_workflow_refresh(module_name)
        enhanced_config["workflow_refresh"] = refresh_config
        
        # Create connection to create-specs
        connection = self.connect_to_create_specs(module_name)
        enhanced_config["connection"] = connection
        
        # Setup integration hooks
        hooks = self.create_integration_hooks(module_name)
        enhanced_config["hooks"] = hooks
        
        # Add metadata
        enhanced_config["metadata"] = {
            "created_at": datetime.now().isoformat(),
            "module_name": module_name,
            "repositories": repos,
            "version": "1.0.0"
        }
        
        # Save configuration
        self.save_config(enhanced_config)
        
        return enhanced_config