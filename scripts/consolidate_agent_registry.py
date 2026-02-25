"""
ABOUTME: Consolidates agent definitions from multiple sources into a unified registry.
This script parses agent definitions from .claude/agents/*/*.md files and config/ai-agents-registry.json.
"""

import json
import re
from pathlib import Path
from typing import Dict, List, Any, Optional
from datetime import datetime, timedelta
import shutil
import argparse
import sys


def generate_alias(agent_name: str) -> str:
    """
    Auto-generate alias from agent name.

    Rules:
    - "backend-dev" → "backend"
    - "code-analyzer" → "analyzer"
    - Take last meaningful word before special suffix

    Args:
        agent_name: Full agent name

    Returns:
        Generated alias (unique shortened form)
    """
    # Remove common suffixes
    suffixes = ['-agent', '-dev', '-coordinator', '-manager', '-specialist']
    alias = agent_name

    for suffix in suffixes:
        if alias.endswith(suffix):
            alias = alias.replace(suffix, '')
            break

    # If no suffix found, take the last word after last hyphen
    if alias == agent_name and '-' in agent_name:
        parts = agent_name.split('-')
        alias = parts[-1]

    return alias


def parse_agent_markdown(md_file: Path) -> Optional[Dict[str, Any]]:
    """
    Parse agent definition from markdown file.

    Expected format:
    ---
    name: agent-name
    type: agent-type
    description: Agent description
    capabilities:
      - capability1
      - capability2
    priority: high/medium/low
    ---

    Args:
        md_file: Path to markdown file

    Returns:
        Parsed agent metadata or None if invalid
    """
    try:
        content = md_file.read_text(encoding='utf-8')

        # Extract YAML frontmatter
        frontmatter_match = re.match(r'^---\s*\n(.*?)\n---', content, re.DOTALL)
        if not frontmatter_match:
            print(f"⚠️  Warning: No frontmatter in {md_file}")
            return None

        frontmatter = frontmatter_match.group(1)

        # Parse YAML manually (simple parser for our format)
        agent_data = {}
        current_key = None
        current_list = []

        for line in frontmatter.split('\n'):
            line = line.rstrip()

            # List item
            if line.strip().startswith('- '):
                if current_key:
                    current_list.append(line.strip()[2:])
                continue

            # Key-value pair
            if ':' in line and not line.startswith(' '):
                # Save previous list if exists
                if current_key and current_list:
                    agent_data[current_key] = current_list
                    current_list = []

                key, value = line.split(':', 1)
                key = key.strip()
                value = value.strip()

                if value:
                    agent_data[key] = value
                    current_key = None
                else:
                    current_key = key

        # Save last list if exists
        if current_key and current_list:
            agent_data[current_key] = current_list

        # Validate required fields
        if 'name' not in agent_data:
            print(f"⚠️  Warning: No name in {md_file}")
            return None

        # Set defaults for missing fields
        agent_data.setdefault('description', f"Agent: {agent_data['name']}")
        agent_data.setdefault('capabilities', [])
        agent_data.setdefault('type', 'specialist')
        agent_data.setdefault('priority', 'medium')

        # Add file location
        agent_data['definition_file'] = str(md_file.relative_to(Path.cwd()))

        return agent_data

    except Exception as e:
        print(f"❌ Error parsing {md_file}: {e}")
        return None


def scan_agent_definitions(agents_dir: Path) -> Dict[str, Dict[str, Any]]:
    """
    Scan all agent markdown files in .claude/agents/.

    Args:
        agents_dir: Path to .claude/agents directory

    Returns:
        Dictionary mapping agent names to parsed metadata
    """
    agents = {}

    if not agents_dir.exists():
        print(f"❌ Error: Agent directory not found: {agents_dir}")
        return agents

    # Find all .md files recursively
    md_files = list(agents_dir.rglob('*.md'))
    print(f"📁 Found {len(md_files)} markdown files in {agents_dir}")

    for md_file in md_files:
        # Skip README files
        if md_file.name == 'README.md':
            continue

        agent_data = parse_agent_markdown(md_file)
        if agent_data:
            agent_name = agent_data['name']
            agents[agent_name] = agent_data
            print(f"  ✓ Parsed: {agent_name}")

    return agents


def load_existing_registry(registry_file: Path) -> Dict[str, Any]:
    """
    Load existing ai-agents-registry.json for performance data.

    Args:
        registry_file: Path to ai-agents-registry.json

    Returns:
        Loaded registry data or empty dict if not found
    """
    if not registry_file.exists():
        print(f"⚠️  Warning: Registry file not found: {registry_file}")
        return {}

    try:
        with open(registry_file, 'r', encoding='utf-8') as f:
            data = json.load(f)
            print(f"✓ Loaded existing registry: {len(data.get('agents', {}))} agents")
            return data
    except Exception as e:
        print(f"❌ Error loading registry: {e}")
        return {}


def merge_agent_data(md_agent: Dict[str, Any], registry_agent: Optional[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Merge agent data from markdown and registry.

    Registry takes precedence for performance data.
    Markdown takes precedence for description and capabilities.

    Args:
        md_agent: Agent data from markdown file
        registry_agent: Agent data from ai-agents-registry.json (may be None)

    Returns:
        Merged agent data
    """
    # Start with markdown data
    merged = md_agent.copy()

    # Initialize performance tracking structure
    merged['performance'] = {
        'success_rate': 0.0,
        'avg_execution_time': 0.0,
        'total_tasks': 0,
        'successful_tasks': 0,
        'failed_tasks': 0,
        'cost_per_task': 0.0,
        'last_updated': datetime.utcnow().isoformat() + 'Z'
    }

    # Initialize availability metrics
    merged['availability'] = {
        'is_available': True,
        'current_load': 0.0,
        'max_concurrent': 10,
        'queue_length': 0,
        'uptime_percentage': 100.0,
        'last_health_check': datetime.utcnow().isoformat() + 'Z'
    }

    # Initialize performance history (last 30 days)
    merged['performance_history'] = []

    # If registry data exists, use it for performance metrics
    if registry_agent:
        # Extract performance data from registry capabilities
        capabilities = registry_agent.get('capabilities', {})

        # Calculate average score across capabilities
        scores = []
        for cap_data in capabilities.values():
            if isinstance(cap_data, dict) and 'score' in cap_data:
                scores.append(cap_data['score'])

        if scores:
            avg_score = sum(scores) / len(scores)
            merged['performance']['success_rate'] = avg_score
            merged['performance_score'] = int(avg_score)
        else:
            merged['performance_score'] = 75  # Default

        # Use bestFor and limitations from registry
        merged['best_for'] = registry_agent.get('bestFor', [])
        merged['limitations'] = registry_agent.get('limitations', [])
        merged['cost_tier'] = registry_agent.get('costTier', 'low')
        merged['platform'] = registry_agent.get('platform', 'claude-flow')
    else:
        # Set defaults for new agents
        merged['performance_score'] = 75
        merged['best_for'] = []
        merged['limitations'] = []
        merged['cost_tier'] = 'low'
        merged['platform'] = 'claude-flow'

    # Generate alias
    merged['alias'] = generate_alias(merged['name'])

    return merged


def validate_unique_aliases(agents: Dict[str, Dict[str, Any]]) -> bool:
    """
    Validate that all aliases are unique.

    Args:
        agents: Dictionary of agent data

    Returns:
        True if all aliases are unique, False otherwise
    """
    aliases = {}
    duplicates = []

    for agent_name, agent_data in agents.items():
        alias = agent_data.get('alias', '')
        if alias in aliases:
            duplicates.append(f"{alias}: {aliases[alias]} and {agent_name}")
        else:
            aliases[alias] = agent_name

    if duplicates:
        print("❌ ERROR: Duplicate aliases found:")
        for dup in duplicates:
            print(f"  - {dup}")
        return False

    return True


def create_consolidated_registry(
    agents_from_md: Dict[str, Dict[str, Any]],
    existing_registry: Dict[str, Any]
) -> Dict[str, Any]:
    """
    Create consolidated agent registry.

    Args:
        agents_from_md: Agents parsed from markdown files
        existing_registry: Existing registry data

    Returns:
        Consolidated registry dictionary
    """
    registry_agents = existing_registry.get('agents', {})

    # Merge all agents
    consolidated_agents = {}

    for agent_name, md_agent in agents_from_md.items():
        registry_agent = registry_agents.get(agent_name)
        merged = merge_agent_data(md_agent, registry_agent)
        consolidated_agents[agent_name] = merged

    # Add agents from registry that aren't in markdown (visualization agents, etc.)
    for agent_name, registry_agent in registry_agents.items():
        if agent_name not in consolidated_agents:
            # Create minimal structure from registry
            agent_data = {
                'name': agent_name,
                'description': registry_agent.get('description', f"Agent: {agent_name}"),
                'capabilities': list(registry_agent.get('capabilities', {}).keys()),
                'type': registry_agent.get('type', 'specialist'),
                'definition_file': 'config/ai-agents-registry.json',
                'platform': registry_agent.get('platform', 'unknown'),
                'alias': generate_alias(agent_name)
            }
            merged = merge_agent_data(agent_data, registry_agent)
            consolidated_agents[agent_name] = merged

    # Validate unique aliases
    if not validate_unique_aliases(consolidated_agents):
        print("⚠️  Warning: Proceeding despite alias conflicts")

    # Build final registry structure
    consolidated = {
        'meta': {
            'version': '2.0.0',
            'last_updated': datetime.utcnow().isoformat() + 'Z',
            'total_agents': len(consolidated_agents),
            'sync_sources': [
                '.claude/agents/*/*.md',
                'config/ai-agents-registry.json'
            ],
            'auto_sync_enabled': True
        },
        'agents': consolidated_agents,
        'aliases': {
            agent['alias']: agent['name']
            for agent in consolidated_agents.values()
        },
        'categories': categorize_agents(consolidated_agents),
        'performance_tracking': {
            'enabled': True,
            'history_window_days': 30,
            'metrics': [
                'success_rate',
                'avg_execution_time',
                'total_tasks',
                'cost_per_task'
            ]
        },
        'availability_config': {
            'health_check_interval_seconds': 60,
            'max_response_time_seconds': 5,
            'max_error_rate_percentage': 10,
            'max_concurrent_tasks': 10
        }
    }

    return consolidated


def categorize_agents(agents: Dict[str, Dict[str, Any]]) -> Dict[str, List[str]]:
    """
    Categorize agents by type.

    Args:
        agents: Dictionary of agent data

    Returns:
        Dictionary mapping categories to agent name lists
    """
    categories = {}

    for agent_name, agent_data in agents.items():
        agent_type = agent_data.get('type', 'specialist')

        if agent_type not in categories:
            categories[agent_type] = []

        categories[agent_type].append(agent_name)

    return categories


def backup_existing_registry(registry_file: Path) -> None:
    """
    Create backup of existing registry file.

    Args:
        registry_file: Path to registry file to backup
    """
    if registry_file.exists():
        backup_file = registry_file.with_suffix('.json.backup')
        shutil.copy2(registry_file, backup_file)
        print(f"✓ Created backup: {backup_file}")


def save_consolidated_registry(registry: Dict[str, Any], output_file: Path) -> None:
    """
    Save consolidated registry to file.

    Args:
        registry: Consolidated registry data
        output_file: Path to output file
    """
    output_file.parent.mkdir(parents=True, exist_ok=True)

    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(registry, f, indent=2, ensure_ascii=False)

    print(f"✓ Saved consolidated registry: {output_file}")
    print(f"  Total agents: {registry['meta']['total_agents']}")
    print(f"  Categories: {len(registry['categories'])}")
    print(f"  Unique aliases: {len(registry['aliases'])}")


def main():
    """Main consolidation workflow."""
    # Set UTF-8 output for Windows
    if sys.platform == 'win32':
        import io
        sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
        sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

    parser = argparse.ArgumentParser(
        description='Consolidate agent definitions from multiple sources'
    )
    parser.add_argument(
        '--agents-dir',
        type=Path,
        default=Path('.claude/agents'),
        help='Directory containing agent markdown files'
    )
    parser.add_argument(
        '--registry-file',
        type=Path,
        default=Path('config/ai-agents-registry.json'),
        help='Existing registry file with performance data'
    )
    parser.add_argument(
        '--output',
        type=Path,
        default=Path('.claude/agents-registry.json'),
        help='Output file for consolidated registry'
    )
    parser.add_argument(
        '--no-backup',
        action='store_true',
        help='Skip creating backup of existing registry'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be done without saving'
    )

    args = parser.parse_args()

    print("🚀 Starting agent registry consolidation...")
    print(f"  Agents directory: {args.agents_dir}")
    print(f"  Registry file: {args.registry_file}")
    print(f"  Output file: {args.output}")
    print()

    # Step 1: Parse markdown files
    print("📝 Step 1: Scanning agent definitions...")
    agents_from_md = scan_agent_definitions(args.agents_dir)
    print(f"  Found {len(agents_from_md)} agents in markdown files")
    print()

    # Step 2: Load existing registry
    print("📊 Step 2: Loading existing registry...")
    existing_registry = load_existing_registry(args.registry_file)
    print()

    # Step 3: Merge and consolidate
    print("🔄 Step 3: Merging agent data...")
    consolidated = create_consolidated_registry(agents_from_md, existing_registry)
    print(f"  Consolidated {consolidated['meta']['total_agents']} agents")
    print()

    # Step 4: Validate
    print("✅ Step 4: Validating registry...")
    from validate_agent_registry import validate_registry
    is_valid, errors = validate_registry(consolidated, args.agents_dir)

    if errors:
        print("⚠️  Validation warnings:")
        for error in errors:
            print(f"  - {error}")

    if not is_valid:
        print("\n❌ Registry validation failed. Aborting.")
        sys.exit(1)

    print("  ✓ Validation passed")
    print()

    # Step 5: Save
    if args.dry_run:
        print("🔍 DRY RUN: Would save registry to:", args.output)
        print("\nSample agents:")
        for i, (name, data) in enumerate(list(consolidated['agents'].items())[:3]):
            print(f"\n  {i+1}. {name}:")
            print(f"     Description: {data['description']}")
            print(f"     Capabilities: {', '.join(data['capabilities'][:3])}")
            print(f"     Alias: {data['alias']}")
            print(f"     Performance Score: {data.get('performance_score', 'N/A')}")
    else:
        print("💾 Step 5: Saving consolidated registry...")
        if not args.no_backup:
            backup_existing_registry(args.output)
        save_consolidated_registry(consolidated, args.output)
        print()
        print("✨ Consolidation complete!")

    return 0


if __name__ == '__main__':
    sys.exit(main())
