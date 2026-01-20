"""Web Resource Manager for Module Agents."""

import json
import yaml
from datetime import datetime, timedelta
from pathlib import Path
from typing import List, Dict, Any, Optional
from dataclasses import dataclass
import hashlib

@dataclass
class WebResource:
    """Web resource metadata."""
    url: str
    type: str  # official_docs, community, tutorial, user_added
    title: str
    description: str
    last_fetched: Optional[datetime] = None
    cache_file: Optional[str] = None
    refresh_interval: str = "1w"
    added_by: str = "system"
    notes: str = ""
    status: str = "active"  # active, outdated, broken

class WebResourceManager:
    """Manages web resources for module agents."""
    
    def __init__(self, agent_dir: Path):
        """Initialize manager with agent directory."""
        self.agent_dir = agent_dir
        self.web_dir = agent_dir / "context" / "external" / "web"
        self.web_dir.mkdir(parents=True, exist_ok=True)
        self.config_file = self.web_dir / "web_resources.yaml"
        self.cache_dir = self.web_dir / "cache"
        self.cache_dir.mkdir(exist_ok=True)
        self.load_config()
    
    def load_config(self) -> Dict[str, Any]:
        """Load web resources configuration."""
        if self.config_file.exists():
            with open(self.config_file, 'r') as f:
                self.config = yaml.safe_load(f) or {}
        else:
            self.config = {
                "enabled": True,
                "sources": [],
                "search_history": [],
                "user_added_links": [],
                "cache_settings": {
                    "max_age_days": 7,
                    "max_size_mb": 100
                }
            }
            self.save_config()
        return self.config
    
    def save_config(self) -> None:
        """Save configuration to file."""
        with open(self.config_file, 'w') as f:
            yaml.dump(self.config, f, default_flow_style=False, indent=2)
    
    def add_user_link(self, url: str, notes: str = "", title: str = "") -> Dict[str, Any]:
        """Add user-provided link to resources."""
        resource = {
            "url": url,
            "title": title or url,
            "notes": notes,
            "added_by": "user",
            "date_added": datetime.now().isoformat(),
            "type": "user_added"
        }
        
        self.config["user_added_links"].append(resource)
        self.save_config()
        return resource
    
    def generate_review_report(self) -> str:
        """Generate markdown report of all web resources."""
        report = []
        report.append("# Web Resources Review")
        report.append(f"\nGenerated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report.append(f"\nModule: {self.agent_dir.name}\n")
        
        # User-added links
        user_links = self.config.get("user_added_links", [])
        if user_links:
            report.append("\n## User-Added Resources")
            for link in user_links:
                report.append(f"- ✅ {link['url']}")
                if link.get("notes"):
                    report.append(f"  Notes: {link['notes']}")
                report.append(f"  Added: {link.get('date_added', 'Unknown')}")
        
        return "\n".join(report)
