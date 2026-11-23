# Web Resource Management Implementation Handover

## 📋 Implementation Request for `/create-module-agent` Enhancement

**Date**: 2025-01-10  
**From**: digitalmodel repository session  
**To**: GitHub folder session (for cross-repository implementation)  
**Priority**: Enhancement for all repositories using Agent OS

## 🎯 Objective

Enhance the `/create-module-agent` command across all repositories to include:
1. Web search capabilities for module-specific resources
2. URL management and caching system
3. User review and link addition interface
4. Automatic content refresh mechanism

## 🔍 Current State Analysis

### What Exists Now
- Basic `/create-module-agent` command in `agent_os/commands/create_module_agent.py`
- Local documentation scanning only (`DocumentationScanner` class)
- Static configuration in `agent.yaml`
- Context folders: `context/repository/`, `context/external/`, `context/optimized/`

### What's Missing
- ❌ Web search functionality
- ❌ URL fetching and caching
- ❌ Web resource management UI
- ❌ Link validation and updating
- ❌ Web content refresh mechanism

## 🏗️ Implementation Blueprint

### 1. Enhanced Configuration Structure

Add to `agent.yaml` generation in `ConfigGenerator.generate_agent_config()`:

```yaml
documentation:
  internal_sources: []
  external_sources: []
  web_resources:
    enabled: true
    sources: []
    search_history: []
    user_added_links: []
    cache_settings:
      max_age_days: 7
      max_size_mb: 100
      auto_refresh: true
    refresh_schedule:
      official_docs: "1w"
      community_resources: "1d"
      user_links: "manual"
```

### 2. New Component: WebResourceManager

Create `agent_os/commands/web_resource_manager.py`:

```python
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
    
    def search_web(self, query: str, module_context: str) -> List[Dict[str, Any]]:
        """Search web for module-specific resources.
        
        This should integrate with WebSearch tool when available.
        For now, returns placeholder structure.
        """
        # TODO: Integrate with actual WebSearch tool
        search_results = {
            "query": f"{module_context} {query}",
            "timestamp": datetime.now().isoformat(),
            "results": []
        }
        
        # Add to search history
        self.config["search_history"].append({
            "query": query,
            "module_context": module_context,
            "date": datetime.now().isoformat(),
            "results_count": len(search_results["results"])
        })
        self.save_config()
        
        return search_results["results"]
    
    def fetch_and_cache(self, url: str, resource_type: str = "general") -> Dict[str, Any]:
        """Fetch web content and cache locally.
        
        This should integrate with WebFetch tool when available.
        """
        # Generate cache filename
        url_hash = hashlib.md5(url.encode()).hexdigest()[:8]
        cache_file = self.cache_dir / f"{url_hash}.md"
        
        # TODO: Integrate with actual WebFetch tool
        # For now, create placeholder
        
        resource = {
            "url": url,
            "type": resource_type,
            "cache_file": str(cache_file.relative_to(self.agent_dir)),
            "last_fetched": datetime.now().isoformat(),
            "status": "active"
        }
        
        # Add to sources
        self.config["sources"].append(resource)
        self.save_config()
        
        return resource
    
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
        
        # Also fetch and cache if possible
        self.fetch_and_cache(url, "user_added")
        
        return resource
    
    def validate_links(self) -> Dict[str, str]:
        """Check if stored links are still valid."""
        validation_results = {}
        
        for source in self.config.get("sources", []):
            # TODO: Actual validation with HEAD request
            validation_results[source["url"]] = "active"
        
        for link in self.config.get("user_added_links", []):
            validation_results[link["url"]] = "active"
        
        return validation_results
    
    def generate_review_report(self) -> str:
        """Generate markdown report of all web resources."""
        report = []
        report.append("# Web Resources Review")
        report.append(f"\nGenerated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report.append(f"\nModule: {self.agent_dir.name}\n")
        
        # Official sources
        official = [s for s in self.config.get("sources", []) 
                   if s.get("type") == "official_docs"]
        if official:
            report.append("## Official Documentation")
            for source in official:
                status = "✅" if source.get("status") == "active" else "⚠️"
                report.append(f"- {status} {source['url']}")
                if source.get("last_fetched"):
                    report.append(f"  Last updated: {source['last_fetched']}")
        
        # Community resources
        community = [s for s in self.config.get("sources", []) 
                    if s.get("type") in ["community", "tutorial"]]
        if community:
            report.append("\n## Community Resources")
            for source in community:
                status = "✅" if source.get("status") == "active" else "⚠️"
                report.append(f"- {status} {source['url']}")
        
        # User-added links
        user_links = self.config.get("user_added_links", [])
        if user_links:
            report.append("\n## User-Added Resources")
            for link in user_links:
                report.append(f"- ✅ {link['url']}")
                if link.get("notes"):
                    report.append(f"  Notes: {link['notes']}")
                report.append(f"  Added: {link.get('date_added', 'Unknown')}")
        
        # Search history
        searches = self.config.get("search_history", [])[-5:]  # Last 5
        if searches:
            report.append("\n## Recent Searches")
            for search in searches:
                report.append(f"- \"{search['query']}\" ({search.get('results_count', 0)} results)")
        
        # Instructions
        report.append("\n## Managing Resources")
        report.append("```bash")
        report.append("# Add a new link")
        report.append(f"python tools/manage-agent-resources.py add-link {self.agent_dir.name} <url>")
        report.append("\n# Search for resources")
        report.append(f"python tools/manage-agent-resources.py search {self.agent_dir.name} <query>")
        report.append("\n# Refresh cached content")
        report.append(f"python tools/manage-agent-resources.py refresh {self.agent_dir.name}")
        report.append("```")
        
        return "\n".join(report)
    
    def refresh_cache(self, force: bool = False) -> Dict[str, bool]:
        """Refresh cached web content based on age."""
        refresh_results = {}
        
        for source in self.config.get("sources", []):
            needs_refresh = False
            
            if force:
                needs_refresh = True
            elif source.get("last_fetched"):
                last_fetched = datetime.fromisoformat(source["last_fetched"])
                interval = source.get("refresh_interval", "1w")
                
                # Parse interval (simplified)
                if interval.endswith("d"):
                    days = int(interval[:-1])
                elif interval.endswith("w"):
                    days = int(interval[:-1]) * 7
                else:
                    days = 7  # Default to weekly
                
                if datetime.now() - last_fetched > timedelta(days=days):
                    needs_refresh = True
            
            if needs_refresh:
                # TODO: Actually refresh with WebFetch
                refresh_results[source["url"]] = True
                source["last_fetched"] = datetime.now().isoformat()
        
        self.save_config()
        return refresh_results
```

### 3. Management CLI Tool

Create `tools/manage-agent-resources.py`:

```python
#!/usr/bin/env python3
"""Manage web resources for module agents."""

import sys
import argparse
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from agent_os.commands.web_resource_manager import WebResourceManager

def main():
    parser = argparse.ArgumentParser(description="Manage agent web resources")
    parser.add_argument("action", choices=["add-link", "search", "review", "refresh", "validate"],
                       help="Action to perform")
    parser.add_argument("module", help="Module name")
    parser.add_argument("--url", help="URL for add-link action")
    parser.add_argument("--query", help="Search query")
    parser.add_argument("--notes", help="Notes for added link")
    parser.add_argument("--force", action="store_true", help="Force refresh")
    
    args = parser.parse_args()
    
    # Find agent directory
    agent_dir = Path.cwd() / "agents" / args.module
    if not agent_dir.exists():
        print(f"Error: Agent directory not found: {agent_dir}")
        return 1
    
    manager = WebResourceManager(agent_dir)
    
    if args.action == "add-link":
        if not args.url:
            print("Error: --url required for add-link")
            return 1
        result = manager.add_user_link(args.url, args.notes or "")
        print(f"Added link: {result['url']}")
    
    elif args.action == "search":
        if not args.query:
            print("Error: --query required for search")
            return 1
        results = manager.search_web(args.query, args.module)
        print(f"Found {len(results)} results for '{args.query}'")
    
    elif args.action == "review":
        report = manager.generate_review_report()
        print(report)
        # Also save to file
        review_file = agent_dir / "context" / "web_resources_review.md"
        with open(review_file, 'w') as f:
            f.write(report)
        print(f"\nReport saved to: {review_file}")
    
    elif args.action == "refresh":
        results = manager.refresh_cache(force=args.force)
        print(f"Refreshed {len(results)} resources")
    
    elif args.action == "validate":
        results = manager.validate_links()
        broken = [url for url, status in results.items() if status != "active"]
        if broken:
            print(f"Found {len(broken)} broken links:")
            for url in broken:
                print(f"  ❌ {url}")
        else:
            print("All links validated successfully ✅")
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
```

### 4. Integration with create_module_agent.py

Add to `CreateModuleAgentCommand.execute()` method:

```python
# After creating default templates
if parsed_args.enable_web_search:
    self._initialize_web_resources(agent_dir, parsed_args.module_name)

def _initialize_web_resources(self, agent_dir: Path, module_name: str) -> None:
    """Initialize web resource management."""
    from agent_os.commands.web_resource_manager import WebResourceManager
    
    manager = WebResourceManager(agent_dir)
    
    # Perform initial search for common resources
    initial_queries = [
        f"{module_name} documentation",
        f"{module_name} API reference",
        f"{module_name} tutorials",
        f"{module_name} examples"
    ]
    
    for query in initial_queries:
        manager.search_web(query, module_name)
    
    # Generate initial review report
    report = manager.generate_review_report()
    review_file = agent_dir / "context" / "web_resources_review.md"
    with open(review_file, 'w') as f:
        f.write(report)
```

## 🚀 Implementation Steps

1. **Phase 1: Core Infrastructure**
   - [ ] Create `web_resource_manager.py` in agent_os/commands/
   - [ ] Create `manage-agent-resources.py` in tools/
   - [ ] Update `create_module_agent.py` to include web resource initialization

2. **Phase 2: Tool Integration**
   - [ ] Integrate with WebSearch tool for actual web searching
   - [ ] Integrate with WebFetch tool for content retrieval
   - [ ] Add proper error handling and retry logic

3. **Phase 3: Advanced Features**
   - [ ] Implement automatic refresh scheduler
   - [ ] Add embedding-based similarity search for cached content
   - [ ] Create web UI for resource management

4. **Phase 4: Cross-Repository Deployment**
   - [ ] Test in digitalmodel repository
   - [ ] Package as reusable component
   - [ ] Deploy to all repositories using Agent OS

## 📦 Files to Create/Modify

### New Files
- `agent_os/commands/web_resource_manager.py`
- `tools/manage-agent-resources.py`
- `agents/<module>/context/external/web/web_resources.yaml` (auto-generated)
- `agents/<module>/context/web_resources_review.md` (auto-generated)

### Modified Files
- `agent_os/commands/create_module_agent.py` (add web resource initialization)
- `CLAUDE.md` (document new capabilities)

## 🧪 Testing Plan

```bash
# 1. Create agent with web search
python tools/create-module-agent.py test-module --enable-web-search

# 2. Add a custom link
python tools/manage-agent-resources.py add-link test-module \
  "https://example.com/docs" --notes "Official documentation"

# 3. Review resources
python tools/manage-agent-resources.py review test-module

# 4. Search for resources
python tools/manage-agent-resources.py search test-module "API tutorial"

# 5. Validate links
python tools/manage-agent-resources.py validate test-module

# 6. Refresh cache
python tools/manage-agent-resources.py refresh test-module --force
```

## 🎯 Success Criteria

- ✅ Web resources can be searched and cached
- ✅ Users can add custom links with notes
- ✅ Resources can be reviewed in markdown format
- ✅ Links are validated periodically
- ✅ Cache refreshes based on configured intervals
- ✅ Works across all repositories with Agent OS

## 📝 Notes for Implementation

1. **Security**: Validate all URLs before fetching
2. **Rate Limiting**: Implement delays between web requests
3. **Storage**: Consider size limits for cached content
4. **Privacy**: Don't cache sensitive or authenticated content
5. **Versioning**: Track changes to cached content over time

## 🔄 Migration Path

For existing agents without web resources:
```bash
# Run migration script
python tools/migrate-agents-web-resources.py

# This will:
# 1. Add web_resources config to existing agent.yaml files
# 2. Create web resource directories
# 3. Perform initial searches based on module names
```

---

**Handover Complete**: This implementation can be applied to all repositories using Agent OS. The modular design allows for easy integration without breaking existing functionality.

**Next Steps for GitHub Session**: 
1. Review this plan
2. Implement in a test repository
3. Deploy across all repositories
4. Document in global Agent OS documentation