#!/usr/bin/env python3
"""Deploy Web Contextualization to All Repositories.

This script deploys the comprehensive web resource contextualization system
from assetutilities to all repositories with agent_os.
"""

import sys
import shutil
from pathlib import Path
from datetime import datetime

class WebContextualizationDeployer:
    """Deploy web contextualization across repositories."""
    
    def __init__(self, base_path="/mnt/github/github"):
        self.base_path = Path(base_path)
        self.assetutilities_path = self.base_path / "assetutilities"
        self.module_path = self.assetutilities_path / "src" / "modules" / "web-contextualization"
        
        # Check if assetutilities module exists
        if not self.module_path.exists():
            print(f"❌ Web contextualization module not found at: {self.module_path}")
            sys.exit(1)
    
    def find_agent_os_repos(self):
        """Find all repositories with agent_os directory."""
        repos = []
        for item in self.base_path.iterdir():
            if item.is_dir() and (item / "agent_os").exists():
                repos.append(item)
        return sorted(repos, key=lambda x: x.name)
    
    def create_wrapper_manager(self, repo_path):
        """Create wrapper for web_resource_manager.py."""
        manager_path = repo_path / "agent_os" / "commands" / "web_resource_manager.py"
        
        wrapper_code = '''"""Web Resource Manager - Enhanced with AssetUtilities Contextualization."""

import sys
from pathlib import Path

# Add assetutilities to path
assetutilities_path = Path("/mnt/github/github/assetutilities")
if assetutilities_path.exists():
    sys.path.insert(0, str(assetutilities_path / "src" / "modules"))
    
    try:
        from web_contextualization import WebContextualizer
        
        # Create compatibility wrapper
        class WebResourceManager(WebContextualizer):
            """Backward compatible wrapper for WebContextualizer."""
            
            def __init__(self, agent_dir):
                super().__init__(agent_dir)
            
            # Add any compatibility methods if needed
            def add_user_link(self, url, notes="", title=""):
                """Compatibility method for old interface."""
                return self.add_resource(
                    url=url,
                    resource_type="user_added",
                    title=title,
                    notes=notes,
                    auto_fetch=True
                )
            
            def generate_review_report(self):
                """Compatibility method for old interface."""
                return self.generate_status_report()
        
        print("[WebResourceManager] Using enhanced contextualization from AssetUtilities")
        
    except ImportError as e:
        print(f"[WebResourceManager] Failed to import from AssetUtilities: {e}")
        # Fall back to basic implementation
        from .web_resource_manager_basic import WebResourceManager
else:
    print("[WebResourceManager] AssetUtilities not found, using basic implementation")
    # Fall back to basic implementation
    from .web_resource_manager_basic import WebResourceManager

__all__ = ["WebResourceManager"]
'''
        
        # Backup existing file if it exists
        if manager_path.exists():
            backup_path = manager_path.with_suffix('.py.basic')
            shutil.copy2(manager_path, backup_path)
        
        # Write new wrapper
        manager_path.parent.mkdir(parents=True, exist_ok=True)
        with open(manager_path, 'w') as f:
            f.write(wrapper_code)
        
        return True
    
    def update_management_tool(self, repo_path):
        """Update manage-agent-resources.py tool."""
        tool_path = repo_path / "tools" / "manage-agent-resources.py"
        
        if not tool_path.exists():
            return False
        
        enhanced_tool = '''#!/usr/bin/env python3
"""Enhanced agent resource management with full contextualization."""

import sys
import argparse
from pathlib import Path

# Add paths
sys.path.insert(0, str(Path(__file__).parent.parent))
assetutilities_path = Path("/mnt/github/github/assetutilities")
if assetutilities_path.exists():
    sys.path.insert(0, str(assetutilities_path / "src" / "modules"))

try:
    from web_contextualization import WebContextualizer
    HAS_CONTEXTUALIZER = True
except ImportError:
    from agent_os.commands.web_resource_manager import WebResourceManager as WebContextualizer
    HAS_CONTEXTUALIZER = False

def main():
    parser = argparse.ArgumentParser(description="Manage agent web resources")
    
    if HAS_CONTEXTUALIZER:
        parser.add_argument("action", choices=[
            "add", "fetch", "fetch-all", "search", "status", 
            "refresh", "review", "add-link"  # Include old commands for compatibility
        ])
    else:
        parser.add_argument("action", choices=["add-link", "review"])
    
    parser.add_argument("module", help="Module name")
    parser.add_argument("--url", help="URL to add")
    parser.add_argument("--type", default="user_added", help="Resource type")
    parser.add_argument("--notes", help="Notes for the resource")
    parser.add_argument("--query", help="Search query")
    parser.add_argument("--force", action="store_true", help="Force refresh")
    
    args = parser.parse_args()
    
    # Find agent directory
    agent_dir = Path.cwd() / "agents" / args.module
    if not agent_dir.exists():
        print(f"Error: Agent directory not found: {agent_dir}")
        return 1
    
    # Initialize manager/contextualizer
    manager = WebContextualizer(agent_dir)
    
    # Handle actions
    if args.action in ["add", "add-link"]:
        if not args.url:
            print("Error: --url required")
            return 1
        
        if HAS_CONTEXTUALIZER:
            resource = manager.add_resource(
                url=args.url,
                resource_type=args.type,
                notes=args.notes or "",
                auto_fetch=True
            )
            print(f"✅ Added and contextualized: {resource.url}")
        else:
            result = manager.add_user_link(args.url, args.notes or "")
            print(f"Added link: {result['url']}")
    
    elif args.action == "fetch" and HAS_CONTEXTUALIZER:
        if not args.url:
            print("Error: --url required")
            return 1
        success, message = manager.fetch_and_process(args.url)
        print(f"{'✅' if success else '❌'} {message}")
    
    elif args.action == "fetch-all" and HAS_CONTEXTUALIZER:
        results = manager.fetch_all_pending()
        for url, (success, message) in results.items():
            print(f"{'✅' if success else '❌'} {url}")
    
    elif args.action == "search" and HAS_CONTEXTUALIZER:
        if not args.query:
            print("Error: --query required")
            return 1
        results = manager.search(args.query, top_k=5)
        for i, result in enumerate(results, 1):
            print(f"{i}. Score: {result['score']:.3f}")
            print(f"   {result['text'][:200]}...")
    
    elif args.action in ["status", "review"]:
        if HAS_CONTEXTUALIZER:
            report = manager.generate_status_report()
        else:
            report = manager.generate_review_report()
        print(report)
    
    elif args.action == "refresh" and HAS_CONTEXTUALIZER:
        results = manager.refresh_outdated(force=args.force)
        for url, (success, message) in results.items():
            print(f"{'✅' if success else '❌'} {url}")
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
'''
        
        # Write enhanced tool
        with open(tool_path, 'w') as f:
            f.write(enhanced_tool)
        
        # Make executable
        import os
        os.chmod(tool_path, 0o755)
        
        return True
    
    def process_existing_agents(self, repo_path):
        """Process existing agents to fetch their resources."""
        agents_dir = repo_path / "agents"
        if not agents_dir.exists():
            return 0
        
        processed = 0
        for agent_dir in agents_dir.iterdir():
            if not agent_dir.is_dir():
                continue
            
            # Check if agent has web resources
            web_config = agent_dir / "context" / "external" / "web" / "web_resources.yaml"
            if web_config.exists():
                print(f"    Processing agent: {agent_dir.name}")
                
                # Try to fetch content
                try:
                    sys.path.insert(0, str(self.assetutilities_path / "src" / "modules"))
                    from web_contextualization import WebContextualizer
                    
                    contextualizer = WebContextualizer(agent_dir)
                    
                    # Import existing URLs
                    import yaml
                    with open(web_config) as f:
                        data = yaml.safe_load(f) or {}
                    
                    for link in data.get("user_added_links", []):
                        contextualizer.add_resource(
                            url=link["url"],
                            notes=link.get("notes", ""),
                            auto_fetch=False  # Don't fetch yet
                        )
                    
                    # Fetch all at once
                    results = contextualizer.fetch_all_pending()
                    success_count = sum(1 for _, (s, _) in results.items() if s)
                    print(f"      Contextualized {success_count}/{len(results)} resources")
                    processed += 1
                    
                except Exception as e:
                    print(f"      Error processing: {e}")
        
        return processed
    
    def deploy(self):
        """Main deployment process."""
        print("🚀 Web Resource Contextualization Deployment")
        print("=" * 50)
        print(f"Timestamp: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"AssetUtilities module: {self.module_path}")
        print()
        
        # Find repositories
        repos = self.find_agent_os_repos()
        print(f"📍 Found {len(repos)} repositories with agent_os:")
        for repo in repos[:5]:  # Show first 5
            print(f"  - {repo.name}")
        if len(repos) > 5:
            print(f"  ... and {len(repos) - 5} more")
        print()
        
        # Process each repository
        success_count = 0
        for i, repo in enumerate(repos, 1):
            print(f"\n[{i}/{len(repos)}] 📦 {repo.name}")
            
            try:
                # Update web_resource_manager.py
                if self.create_wrapper_manager(repo):
                    print("  ✅ Updated web_resource_manager.py")
                
                # Update management tool
                if self.update_management_tool(repo):
                    print("  ✅ Updated manage-agent-resources.py")
                
                # Process existing agents
                agent_count = self.process_existing_agents(repo)
                if agent_count > 0:
                    print(f"  ✅ Processed {agent_count} agents")
                
                success_count += 1
                
            except Exception as e:
                print(f"  ❌ Error: {e}")
        
        # Summary
        print("\n" + "=" * 50)
        print(f"✅ Deployment Complete!")
        print(f"   Successful: {success_count}/{len(repos)} repositories")
        print()
        print("📝 Next Steps:")
        print("1. Install dependencies in each repo:")
        print("   pip install requests PyPDF2 pdfplumber sentence-transformers")
        print()
        print("2. Test with a repository:")
        print("   cd /mnt/github/github/digitalmodel")
        print("   python tools/manage-agent-resources.py status orcaflex")
        print()
        print("3. Fetch all pending resources:")
        print("   python tools/manage-agent-resources.py fetch-all orcaflex")
        print()
        print("4. Search indexed content:")
        print("   python tools/manage-agent-resources.py search orcaflex --query 'mooring'")

def main():
    """Main entry point."""
    print("Starting Web Contextualization Deployment...")
    print()
    
    deployer = WebContextualizationDeployer()
    deployer.deploy()
    
    return 0

if __name__ == "__main__":
    sys.exit(main())