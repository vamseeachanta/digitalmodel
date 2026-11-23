# Web Resource Contextualization Deployment Prompt

## 🎯 Task: Deploy Web Contextualization Across All Repositories

### Overview
Implement comprehensive web resource contextualization that actually fetches, caches, processes PDFs, and indexes content for all module agents. This replaces the current "URL-only" storage with real content processing.

### Core Implementation Location
**Central Library**: `/mnt/github/github/assetutilities/src/modules/web-contextualization/`

All 25 repositories should import and use this shared module rather than duplicating code.

## 📋 Implementation Steps for Each Repository

### Step 1: Update Web Resource Manager
Replace the basic `web_resource_manager.py` in each repo with an import wrapper:

```python
# File: agent_os/commands/web_resource_manager.py
"""Web Resource Manager - Wrapper for AssetUtilities implementation."""

import sys
from pathlib import Path

# Add assetutilities to path
assetutilities_path = Path("/mnt/github/github/assetutilities")
if assetutilities_path.exists():
    sys.path.insert(0, str(assetutilities_path / "src" / "modules"))
    
    from web_contextualization import WebContextualizer
    
    # Re-export for backward compatibility
    WebResourceManager = WebContextualizer
else:
    # Fallback to basic implementation if assetutilities not available
    from .web_resource_manager_basic import WebResourceManager

__all__ = ["WebResourceManager"]
```

### Step 2: Update manage-agent-resources.py Tool
Enhance the CLI tool to use full contextualization:

```python
#!/usr/bin/env python3
"""Enhanced agent resource management with content contextualization."""

import sys
import argparse
from pathlib import Path

# Add paths
sys.path.insert(0, str(Path(__file__).parent.parent))
assetutilities_path = Path("/mnt/github/github/assetutilities")
if assetutilities_path.exists():
    sys.path.insert(0, str(assetutilities_path / "src" / "modules"))

from web_contextualization import WebContextualizer

def main():
    parser = argparse.ArgumentParser(description="Manage agent web resources with contextualization")
    parser.add_argument("action", choices=[
        "add", "fetch", "fetch-all", "search", "status", 
        "refresh", "clean", "index-stats"
    ])
    parser.add_argument("module", help="Module name")
    parser.add_argument("--url", help="URL to add")
    parser.add_argument("--type", choices=[
        "official_docs", "api_reference", "tutorial", 
        "standard", "user_added"
    ], default="user_added")
    parser.add_argument("--notes", help="Notes for the resource")
    parser.add_argument("--query", help="Search query")
    parser.add_argument("--force", action="store_true", help="Force refresh")
    parser.add_argument("--auto-fetch", action="store_true", 
                       help="Automatically fetch content when adding", default=True)
    
    args = parser.parse_args()
    
    # Find agent directory
    agent_dir = Path.cwd() / "agents" / args.module
    if not agent_dir.exists():
        print(f"Error: Agent directory not found: {agent_dir}")
        return 1
    
    # Initialize contextualizer
    contextualizer = WebContextualizer(agent_dir)
    
    if args.action == "add":
        if not args.url:
            print("Error: --url required for add action")
            return 1
        
        resource = contextualizer.add_resource(
            url=args.url,
            resource_type=args.type,
            notes=args.notes or "",
            auto_fetch=args.auto_fetch
        )
        print(f"✅ Added: {resource.url}")
        print(f"   Type: {resource.type}")
        print(f"   Status: {resource.status}")
        
        if resource.status == "indexed":
            print(f"   ✅ Content fetched, processed, and indexed!")
    
    elif args.action == "fetch":
        if not args.url:
            print("Error: --url required for fetch action")
            return 1
        
        success, message = contextualizer.fetch_and_process(args.url)
        if success:
            print(f"✅ {message}")
        else:
            print(f"❌ {message}")
    
    elif args.action == "fetch-all":
        print("Fetching all pending resources...")
        results = contextualizer.fetch_all_pending()
        
        for url, (success, message) in results.items():
            status = "✅" if success else "❌"
            print(f"{status} {url}: {message}")
    
    elif args.action == "search":
        if not args.query:
            print("Error: --query required for search")
            return 1
        
        results = contextualizer.search(args.query, top_k=5)
        
        if not results:
            print("No results found")
        else:
            print(f"\nSearch Results for: '{args.query}'\n")
            for i, result in enumerate(results, 1):
                print(f"{i}. Score: {result['score']:.3f}")
                print(f"   Source: {result['source']}")
                print(f"   Text: {result['text'][:200]}...")
                print()
    
    elif args.action == "status":
        report = contextualizer.generate_status_report()
        print(report)
        
        # Save report
        report_file = agent_dir / "context" / "contextualization_status.md"
        with open(report_file, 'w') as f:
            f.write(report)
        print(f"\n📄 Report saved to: {report_file}")
    
    elif args.action == "refresh":
        print("Refreshing outdated resources...")
        results = contextualizer.refresh_outdated(force=args.force)
        
        for url, (success, message) in results.items():
            status = "✅" if success else "❌"
            print(f"{status} {url}: {message}")
    
    elif args.action == "clean":
        # Clean old cache files
        from web_contextualization.resource_fetcher import ResourceFetcher
        fetcher = ResourceFetcher(
            agent_dir / "context" / "external" / "web" / "cache"
        )
        fetcher.clean_cache(max_age_days=30, max_size_mb=500)
        print("✅ Cache cleaned")
    
    elif args.action == "index-stats":
        stats = contextualizer.indexer.get_statistics()
        print("\n📊 Index Statistics:")
        print(f"   Documents: {stats.get('total_documents', 0)}")
        print(f"   Chunks: {stats.get('total_chunks', 0)}")
        print(f"   Tokens: {stats.get('total_tokens', 0)}")
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
```

### Step 3: Update create-module-agent.py
Add contextualization initialization to agent creation:

```python
# In create_module_agent.py, add to the agent creation process:

def create_agent_with_contextualization(agent_name, agent_type):
    """Create agent with web contextualization enabled."""
    
    # ... existing agent creation code ...
    
    # Initialize web contextualization
    print("🌐 Initializing web contextualization...")
    
    from web_contextualization import WebContextualizer
    contextualizer = WebContextualizer(agent_dir)
    
    # Add default resources based on agent type
    default_resources = get_default_resources(agent_type)
    
    for resource in default_resources:
        contextualizer.add_resource(
            url=resource['url'],
            resource_type=resource['type'],
            title=resource.get('title', ''),
            description=resource.get('description', ''),
            auto_fetch=True
        )
    
    # Fetch all resources
    print("📥 Fetching and indexing resources...")
    results = contextualizer.fetch_all_pending()
    
    success_count = sum(1 for _, (success, _) in results.items() if success)
    print(f"✅ Contextualized {success_count}/{len(results)} resources")
    
    return agent_dir

def get_default_resources(agent_type):
    """Get default resources based on agent type."""
    
    resources_map = {
        "offshore-engineering": [
            {
                "url": "https://www.dnv.com/rules-standards/",
                "type": "standard",
                "title": "DNV Standards",
                "description": "Offshore engineering standards"
            },
            {
                "url": "https://www.api.org/products-and-services/standards",
                "type": "standard",
                "title": "API Standards",
                "description": "Oil & gas industry standards"
            }
        ],
        "python": [
            {
                "url": "https://docs.python.org/3/",
                "type": "official_docs",
                "title": "Python Documentation",
                "description": "Official Python documentation"
            }
        ],
        # Add more mappings...
    }
    
    return resources_map.get(agent_type, [])
```

### Step 4: Install Required Dependencies
Create requirements file for web contextualization:

```bash
# File: requirements-contextualization.txt
requests>=2.28.0
PyPDF2>=3.0.0
pdfplumber>=0.9.0
PyMuPDF>=1.23.0
sentence-transformers>=2.2.0
numpy>=1.24.0
pyyaml>=6.0
```

### Step 5: Update Agent Configuration
Modify agent.yaml template to include contextualization settings:

```yaml
# Additional fields for agent.yaml
web_contextualization:
  enabled: true
  auto_fetch: true
  auto_index: true
  pdf_extraction: true
  cache_settings:
    max_age_days: 7
    max_size_mb: 500
    version_control: true
  indexing:
    chunk_size: 1000
    overlap: 200
    embedding_model: "sentence-transformers/all-MiniLM-L6-v2"
  refresh_intervals:
    official_docs: "7d"
    api_reference: "14d"
    tutorial: "30d"
    standard: "90d"
    user_added: "manual"
```

## 📦 Deployment Script

Create and run this deployment script:

```python
#!/usr/bin/env python3
"""Deploy web contextualization to all repositories."""

import subprocess
from pathlib import Path

def deploy_to_all_repos():
    """Deploy contextualization to all repos with agent_os."""
    
    base_path = Path("/mnt/github/github")
    repos = [d for d in base_path.iterdir() 
             if d.is_dir() and (d / "agent_os").exists()]
    
    print(f"🚀 Deploying to {len(repos)} repositories...")
    
    for repo in repos:
        print(f"\n📦 Processing: {repo.name}")
        
        # Update web_resource_manager.py
        manager_path = repo / "agent_os" / "commands" / "web_resource_manager.py"
        if manager_path.exists():
            # Create wrapper that imports from assetutilities
            update_to_wrapper(manager_path)
            print(f"  ✅ Updated web_resource_manager.py")
        
        # Update manage-agent-resources.py
        tool_path = repo / "tools" / "manage-agent-resources.py"
        if tool_path.exists():
            update_management_tool(tool_path)
            print(f"  ✅ Updated manage-agent-resources.py")
        
        # Install dependencies
        install_dependencies(repo)
        print(f"  ✅ Dependencies installed")
        
        # Process existing agents
        agents_dir = repo / "agents"
        if agents_dir.exists():
            for agent_dir in agents_dir.iterdir():
                if agent_dir.is_dir() and (agent_dir / "agent.yaml").exists():
                    contextualize_existing_agent(agent_dir)
                    print(f"  ✅ Contextualized: {agent_dir.name}")
    
    print("\n✅ Deployment complete!")

def contextualize_existing_agent(agent_dir):
    """Fetch and index content for existing agent."""
    from web_contextualization import WebContextualizer
    
    contextualizer = WebContextualizer(agent_dir)
    
    # Check for existing web_resources.yaml
    old_config = agent_dir / "context" / "external" / "web" / "web_resources.yaml"
    if old_config.exists():
        # Migrate existing URLs
        import yaml
        with open(old_config) as f:
            data = yaml.safe_load(f)
        
        for link in data.get("user_added_links", []):
            contextualizer.add_resource(
                url=link["url"],
                notes=link.get("notes", ""),
                auto_fetch=True
            )
        
        # Fetch all
        contextualizer.fetch_all_pending()

if __name__ == "__main__":
    deploy_to_all_repos()
```

## 🔧 Testing Commands

After deployment, test with:

```bash
# Add and fetch a PDF
python tools/manage-agent-resources.py add orcaflex \
  --url "https://example.com/technical-paper.pdf" \
  --type official_docs \
  --auto-fetch

# Search indexed content
python tools/manage-agent-resources.py search orcaflex \
  --query "mooring line tension"

# Check status
python tools/manage-agent-resources.py status orcaflex

# Refresh all outdated
python tools/manage-agent-resources.py refresh orcaflex

# View index statistics
python tools/manage-agent-resources.py index-stats orcaflex
```

## 🔄 Continuous Improvement

When issues are encountered:

1. **Fix in AssetUtilities First**: All improvements should be made to `/mnt/github/github/assetutilities/src/modules/web-contextualization/`

2. **Common Issues to Handle**:
   - PDF parsing failures → Add fallback methods
   - Large file handling → Implement streaming
   - Authentication required → Add auth support
   - Rate limiting → Add retry logic
   - Encoding issues → Better text extraction

3. **Test Changes**: Test in one repo before deploying to all

4. **Version Control**: Tag assetutilities when making breaking changes

## 📊 Success Metrics

After implementation, each agent should have:
- ✅ Actual cached content (not just URLs)
- ✅ Extracted text from PDFs
- ✅ Searchable indexed chunks
- ✅ Context available for RAG queries
- ✅ Automatic refresh based on intervals

## 🎯 Expected Outcome

Transform from:
```
URLs stored: 6 ✅
Content cached: 0 ❌
Searchable: No ❌
```

To:
```
URLs stored: 6 ✅
Content cached: 6 ✅
PDFs processed: 4 ✅
Chunks indexed: 248 ✅
Searchable: Yes ✅
Context available: 16,000 tokens ✅
```

---

**Implementation Priority**: Start with high-value agents (orcaflex, aqwa) then deploy to all 25 repos.