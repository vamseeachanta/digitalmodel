"""Workflow Refresh Manager for Agent OS.

This module manages continuous workflow refresh and learning capabilities.
"""

import json
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, List, Optional


class WorkflowRefreshManager:
    """Manage workflow refresh and continuous learning."""
    
    def __init__(self, agent_path: Path):
        """Initialize workflow refresh manager.
        
        Args:
            agent_path: Path to the agent directory
        """
        self.agent_path = agent_path
        self.refresh_config_path = agent_path / "workflows" / "refresh_config.json"
        self.learning_path = agent_path / "context" / "learning.json"
        
        # Ensure directories exist
        self.refresh_config_path.parent.mkdir(parents=True, exist_ok=True)
        self.learning_path.parent.mkdir(parents=True, exist_ok=True)
    
    def setup_refresh(self, module_name: str, config: Dict[str, Any]) -> None:
        """Setup workflow refresh configuration.
        
        Args:
            module_name: Name of the module
            config: Refresh configuration
        """
        refresh_config = {
            "module_name": module_name,
            "enabled": True,
            "last_refresh": datetime.now().isoformat(),
            "config": config
        }
        
        with open(self.refresh_config_path, 'w') as f:
            json.dump(refresh_config, f, indent=2)
    
    def capture_learning(self, execution_data: Dict[str, Any]) -> None:
        """Capture learning from execution.
        
        Args:
            execution_data: Data from workflow execution
        """
        learning_entry = {
            "timestamp": datetime.now().isoformat(),
            "data": execution_data
        }
        
        # Load existing learning data
        learning_data = []
        if self.learning_path.exists():
            with open(self.learning_path, 'r') as f:
                learning_data = json.load(f)
        
        # Add new entry
        learning_data.append(learning_entry)
        
        # Keep only last 50 entries
        if len(learning_data) > 50:
            learning_data = learning_data[-50:]
        
        # Save updated data
        with open(self.learning_path, 'w') as f:
            json.dump(learning_data, f, indent=2)
    
    def get_refresh_status(self) -> Dict[str, Any]:
        """Get current refresh status.
        
        Returns:
            Refresh status information
        """
        if self.refresh_config_path.exists():
            with open(self.refresh_config_path, 'r') as f:
                return json.load(f)
        return {"enabled": False, "status": "not_configured"}
    
    def get_learning_insights(self) -> Dict[str, Any]:
        """Get learning insights.
        
        Returns:
            Learning insights and statistics
        """
        if not self.learning_path.exists():
            return {"total_entries": 0, "insights": []}
        
        with open(self.learning_path, 'r') as f:
            learning_data = json.load(f)
        
        return {
            "total_entries": len(learning_data),
            "latest_entry": learning_data[-1] if learning_data else None,
            "insights": ["Continuous learning is active"]
        }