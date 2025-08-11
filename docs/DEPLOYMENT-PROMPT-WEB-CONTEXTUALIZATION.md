# 🚀 DEPLOYMENT PROMPT: Web Resource Contextualization System

## Executive Summary
Deploy a comprehensive web resource contextualization system that **actually fetches, caches, processes PDFs, and indexes content** for all module agents. This replaces the current "URL-only" storage with real content processing.

**Current State**: URLs are stored but content is NOT fetched or available to agents
**Target State**: Full content pipeline with PDF processing, caching, indexing, and semantic search

## 📍 Prerequisites

### 1. Verify AssetUtilities Module Exists
```bash
ls -la /mnt/github/github/assetutilities/src/modules/web-contextualization/
```

If not present, create it first using the code provided below.

### 2. Install Dependencies
```bash
pip install requests PyPDF2 pdfplumber PyMuPDF sentence-transformers numpy pyyaml
```

## 🎯 Implementation Task

### STEP 1: Create Central Module in AssetUtilities

Create these files in `/mnt/github/github/assetutilities/src/modules/web-contextualization/`:

#### A. `__init__.py`
```python
"""Web Resource Contextualization Module for Agent OS."""

from .web_contextualizer import WebContextualizer
from .pdf_processor import PDFProcessor
from .content_indexer import ContentIndexer
from .resource_fetcher import ResourceFetcher

__version__ = "1.0.0"
__all__ = ["WebContextualizer", "PDFProcessor", "ContentIndexer", "ResourceFetcher"]
```

#### B. Main Files (Get from this repository)
Copy these files from `/mnt/github/github/digitalmodel/` to assetutilities:
- `web_contextualizer.py` - Main orchestrator (600+ lines)
- `pdf_processor.py` - PDF processing (300+ lines)
- `resource_fetcher.py` - URL fetching (400+ lines)
- `content_indexer.py` - Search indexing (320+ lines)
- `requirements.txt` - Dependencies

### STEP 2: Deploy to All 25 Repositories

Run this deployment script in the folder containing all repos:

```python
#!/usr/bin/env python3
"""Deploy Web Contextualization to All Repositories."""

import os
import sys
from pathlib import Path

def deploy_to_all_repos():
    """Deploy to all repos with agent_os."""
    
    base_path = Path.cwd()  # Current directory with all repos
    repos = [d for d in base_path.iterdir() 
             if d.is_dir() and (d / "agent_os").exists()]
    
    print(f"🚀 Found {len(repos)} repositories to update")
    
    for repo in repos:
        print(f"\n📦 Processing: {repo.name}")
        
        # 1. Update web_resource_manager.py to use AssetUtilities
        update_web_resource_manager(repo)
        
        # 2. Update manage-agent-resources.py tool
        update_management_tool(repo)
        
        # 3. Process existing agents
        process_existing_agents(repo)
    
    print("\n✅ Deployment complete!")

def update_web_resource_manager(repo_path):
    """Replace with wrapper that imports from AssetUtilities."""
    
    manager_path = repo_path / "agent_os" / "commands" / "web_resource_manager.py"
    
    wrapper_code = '''"""Web Resource Manager - Enhanced with AssetUtilities."""

import sys
from pathlib import Path

# Import from AssetUtilities
assetutilities_path = Path("/mnt/github/github/assetutilities")
if assetutilities_path.exists():
    sys.path.insert(0, str(assetutilities_path / "src" / "modules"))
    
    from web_contextualization import WebContextualizer
    
    # Compatibility wrapper
    class WebResourceManager(WebContextualizer):
        """Backward compatible wrapper."""
        
        def add_user_link(self, url, notes="", title=""):
            return self.add_resource(url=url, notes=notes, auto_fetch=True)
        
        def generate_review_report(self):
            return self.generate_status_report()
else:
    # Fallback to original
    from .web_resource_manager_original import WebResourceManager

__all__ = ["WebResourceManager"]
'''
    
    # Backup original
    if manager_path.exists():
        backup = manager_path.with_suffix('.py.original')
        if not backup.exists():
            import shutil
            shutil.copy2(manager_path, backup)
    
    # Write wrapper
    manager_path.parent.mkdir(parents=True, exist_ok=True)
    with open(manager_path, 'w') as f:
        f.write(wrapper_code)
    
    print(f"  ✅ Updated web_resource_manager.py")

def update_management_tool(repo_path):
    """Update CLI tool with enhanced commands."""
    
    tool_path = repo_path / "tools" / "manage-agent-resources.py"
    if not tool_path.exists():
        return
    
    # Read the enhanced tool code from template
    enhanced_code = '''#!/usr/bin/env python3
"""Enhanced resource management with contextualization."""

import sys
from pathlib import Path

# Setup paths
sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, "/mnt/github/github/assetutilities/src/modules")

from web_contextualization import WebContextualizer

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("action", choices=[
        "add", "fetch", "fetch-all", "search", "status", "refresh"
    ])
    parser.add_argument("module")
    parser.add_argument("--url")
    parser.add_argument("--query")
    parser.add_argument("--notes")
    parser.add_argument("--type", default="user_added")
    args = parser.parse_args()
    
    agent_dir = Path.cwd() / "agents" / args.module
    ctx = WebContextualizer(agent_dir)
    
    if args.action == "add":
        resource = ctx.add_resource(args.url, notes=args.notes)
        print(f"✅ Added and fetched: {resource.url}")
    
    elif args.action == "fetch-all":
        results = ctx.fetch_all_pending()
        for url, (success, msg) in results.items():
            print(f"{'✅' if success else '❌'} {url}")
    
    elif args.action == "search":
        results = ctx.search(args.query)
        for r in results:
            print(f"Score: {r['score']:.2f} - {r['text'][:100]}...")
    
    elif args.action == "status":
        print(ctx.generate_status_report())
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
'''
    
    with open(tool_path, 'w') as f:
        f.write(enhanced_code)
    
    os.chmod(tool_path, 0o755)
    print(f"  ✅ Updated manage-agent-resources.py")

def process_existing_agents(repo_path):
    """Fetch content for existing agent resources."""
    
    agents_dir = repo_path / "agents"
    if not agents_dir.exists():
        return
    
    sys.path.insert(0, "/mnt/github/github/assetutilities/src/modules")
    from web_contextualization import WebContextualizer
    
    for agent_dir in agents_dir.iterdir():
        if not agent_dir.is_dir():
            continue
        
        # Check for existing resources
        old_config = agent_dir / "context" / "external" / "web" / "web_resources.yaml"
        if old_config.exists():
            print(f"    Fetching content for: {agent_dir.name}")
            
            ctx = WebContextualizer(agent_dir)
            
            # Import existing URLs
            import yaml
            with open(old_config) as f:
                data = yaml.safe_load(f) or {}
            
            for link in data.get("user_added_links", []):
                ctx.add_resource(link["url"], notes=link.get("notes", ""))
            
            # Fetch all
            results = ctx.fetch_all_pending()
            success = sum(1 for _, (s, _) in results.items() if s)
            print(f"      Fetched {success}/{len(results)} resources")

if __name__ == "__main__":
    deploy_to_all_repos()
```

### STEP 3: Update create-module-agent Integration

Add to each repo's `create-module-agent.py`:

```python
# After creating agent structure, add:

def initialize_web_contextualization(agent_dir, agent_type):
    """Initialize with contextualized web resources."""
    
    sys.path.insert(0, "/mnt/github/github/assetutilities/src/modules")
    from web_contextualization import WebContextualizer
    
    ctx = WebContextualizer(agent_dir)
    
    # Add default resources by type
    if agent_type == "offshore-engineering":
        resources = [
            ("https://www.dnv.com/rules-standards/", "DNV Standards"),
            ("https://www.api.org/standards", "API Standards"),
        ]
    elif agent_type == "python":
        resources = [
            ("https://docs.python.org/3/", "Python Documentation"),
        ]
    # Add more types...
    
    for url, title in resources:
        ctx.add_resource(url, resource_type="official_docs", title=title)
    
    # Fetch all content
    ctx.fetch_all_pending()
    print(f"✅ Contextualized {len(resources)} resources")
```

## 🧪 Testing Commands

After deployment, test in any repository:

```bash
# 1. Check status of an agent
python tools/manage-agent-resources.py status orcaflex

# Expected output:
# ✅ Total Resources: 6
# ✅ Fetched: 6
# ✅ Processed: 6
# ✅ Indexed: 6
# ✅ Cache Size: 12.4 MB

# 2. Fetch all pending content
python tools/manage-agent-resources.py fetch-all orcaflex

# 3. Search indexed content
python tools/manage-agent-resources.py search orcaflex --query "mooring tension"

# 4. Add new PDF with auto-fetch
python tools/manage-agent-resources.py add aqwa \
  --url "https://example.com/technical-paper.pdf" \
  --type official_docs
```

## 📊 Success Metrics

### Before (Current State)
```
URLs stored: ✅ 6
Content cached: ❌ 0
PDFs processed: ❌ 0
Searchable: ❌ No
Context available: ❌ No
```

### After (Target State)
```
URLs stored: ✅ 6
Content cached: ✅ 6
PDFs processed: ✅ 4
Chunks indexed: ✅ 248
Searchable: ✅ Yes
Context available: ✅ 16,000 tokens
```

## 🔧 Troubleshooting

### Issue: ImportError for web_contextualization
**Solution**: Ensure AssetUtilities path is correct and module exists

### Issue: PDF extraction fails
**Solution**: Install at least one PDF library: `pip install PyPDF2 pdfplumber`

### Issue: No search results
**Solution**: Check if content was actually fetched: `fetch-all` command

### Issue: Cache growing too large
**Solution**: Use clean command: `python tools/manage-agent-resources.py clean module-name`

## 🔄 Continuous Improvement

**IMPORTANT**: When issues are encountered in ANY repository:

1. **Fix in AssetUtilities FIRST**
   - Navigate to `/mnt/github/github/assetutilities/src/modules/web-contextualization/`
   - Make improvements to the central module
   - Test the fix

2. **All repos automatically benefit**
   - Since all repos import from AssetUtilities, fixes propagate immediately
   - No need to update 25 repos individually

3. **Common improvements to make**:
   - Better PDF parsing for specific formats
   - Authentication handling for protected resources
   - Rate limiting and retry logic
   - Support for new file types (Excel, Word, etc.)

## 📝 Summary

This deployment transforms agent web resources from passive URL storage to active content contextualization with:

1. **Real content fetching** - Downloads actual files
2. **PDF processing** - Extracts text, tables, metadata
3. **Content indexing** - Makes everything searchable
4. **Semantic search** - Find relevant information across all resources
5. **Centralized maintenance** - Single codebase in AssetUtilities

**Run the deployment script above in your folder with all 25 repos to implement this system.**

---
*Implementation time: ~5 minutes per repository*
*Total time for 25 repos: ~2 hours including testing*