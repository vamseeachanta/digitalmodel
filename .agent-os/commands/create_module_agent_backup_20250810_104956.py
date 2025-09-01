"""Enhanced Create Module Agent Command Implementation.

This module implements the enhanced /create-module-agent slash command with:
- Create or update modes
- Documentation management by category
- Duplicate detection and prevention
- Web resource integration
"""

import re
import yaml
import json
import hashlib
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass
from enum import Enum


class AgentMode(Enum):
    """Agent operation modes."""
    CREATE = "create"
    UPDATE = "update"


class DocumentCategory(Enum):
    """Documentation categories."""
    INTERNAL = "internal"
    EXTERNAL = "external"
    WEB = "web"
    REPOSITORY = "repository"
    OPTIMIZED = "optimized"


@dataclass
class CommandResult:
    """Result of command execution."""
    success: bool
    message: str
    data: Optional[Dict[str, Any]] = None


@dataclass
class DocumentationItem:
    """Documentation item with metadata."""
    path: str
    category: DocumentCategory
    hash: str
    title: str
    description: str = ""
    url: Optional[str] = None
    added_date: Optional[datetime] = None
    added_by: str = "system"


class EnhancedArgumentParser:
    """Enhanced parser for create-module-agent command arguments."""

    def parse(self, args: List[str]) -> 'EnhancedParsedArgs':
        """Parse enhanced command arguments.
        
        Supports:
        - --mode create|update (default: create)
        - --add-doc <path> --category <category> [--title <title>] [--description <desc>]
        - --remove-doc <path>
        - --list-docs [category]
        - Standard options from original parser
        """
        if len(args) < 2:
            raise ValueError("Module name is required")
        
        # Skip command name
        args = args[1:]
        module_name = args[0]
        
        if not self._validate_module_name(module_name):
            raise ValueError(f"Invalid module name: {module_name}")
        
        parsed_args = EnhancedParsedArgs(
            module_name=module_name,
            mode=AgentMode.CREATE,
            type="general-purpose",
            repos=[],
            context_cache=True,
            templates=[],
            documentation_ops=[]
        )
        
        i = 1
        while i < len(args):
            arg = args[i]
            
            # Mode selection
            if arg == "--mode" and i + 1 < len(args):
                mode_str = args[i + 1].lower()
                if mode_str not in ["create", "update"]:
                    raise ValueError(f"Invalid mode: {mode_str}. Use 'create' or 'update'")
                parsed_args.mode = AgentMode(mode_str)
                i += 2
                
            # Documentation management
            elif arg == "--add-doc" and i + 1 < len(args):
                doc_op = self._parse_doc_operation(args, i)
                parsed_args.documentation_ops.append(doc_op)
                i = doc_op['next_index']
                
            elif arg == "--remove-doc" and i + 1 < len(args):
                parsed_args.documentation_ops.append({
                    'operation': 'remove',
                    'path': args[i + 1]
                })
                i += 2
                
            elif arg == "--list-docs":
                category = None
                if i + 1 < len(args) and not args[i + 1].startswith("--"):
                    category = args[i + 1]
                    i += 1
                parsed_args.documentation_ops.append({
                    'operation': 'list',
                    'category': category
                })
                i += 1
                
            # Original options
            elif arg == "--type" and i + 1 < len(args):
                parsed_args.type = args[i + 1]
                i += 2
            elif arg == "--repos" and i + 1 < len(args):
                parsed_args.repos = [r.strip() for r in args[i + 1].split(",")]
                i += 2
            elif arg == "--context-cache" and i + 1 < len(args):
                parsed_args.context_cache = args[i + 1].lower() == "true"
                i += 2
            elif arg == "--templates" and i + 1 < len(args):
                parsed_args.templates = [t.strip() for t in args[i + 1].split(",")]
                i += 2
            else:
                i += 1
        
        return parsed_args
    
    def _parse_doc_operation(self, args: List[str], start_idx: int) -> Dict[str, Any]:
        """Parse documentation add operation."""
        doc_op = {
            'operation': 'add',
            'path': args[start_idx + 1],
            'category': DocumentCategory.INTERNAL,
            'title': None,
            'description': None,
            'next_index': start_idx + 2
        }
        
        i = start_idx + 2
        while i < len(args) and not args[i].startswith("--"):
            if args[i - 1] == "--category":
                try:
                    doc_op['category'] = DocumentCategory(args[i].lower())
                except ValueError:
                    raise ValueError(f"Invalid category: {args[i]}. Use: {', '.join([c.value for c in DocumentCategory])}")
            elif args[i - 1] == "--title":
                doc_op['title'] = args[i]
            elif args[i - 1] == "--description":
                doc_op['description'] = args[i]
            i += 1
        
        doc_op['next_index'] = i
        return doc_op
    
    def _validate_module_name(self, name: str) -> bool:
        """Validate module name format."""
        if not name:
            return False
        return bool(re.match(r'^[a-zA-Z0-9_-]+$', name))


@dataclass
class EnhancedParsedArgs:
    """Enhanced parsed command arguments."""
    module_name: str
    mode: AgentMode
    type: str
    repos: List[str]
    context_cache: bool
    templates: List[str]
    documentation_ops: List[Dict[str, Any]]


class DocumentationManager:
    """Manages agent documentation with duplicate detection."""
    
    def __init__(self, agent_dir: Path):
        """Initialize documentation manager."""
        self.agent_dir = agent_dir
        self.docs_registry_file = agent_dir / "context" / "docs_registry.yaml"
        self.load_registry()
    
    def load_registry(self):
        """Load documentation registry."""
        if self.docs_registry_file.exists():
            with open(self.docs_registry_file, 'r') as f:
                self.registry = yaml.safe_load(f) or {'documents': {}}
        else:
            self.registry = {'documents': {}}
    
    def save_registry(self):
        """Save documentation registry."""
        self.docs_registry_file.parent.mkdir(parents=True, exist_ok=True)
        with open(self.docs_registry_file, 'w') as f:
            yaml.dump(self.registry, f, default_flow_style=False)
    
    def calculate_hash(self, content: str) -> str:
        """Calculate SHA256 hash of content."""
        return hashlib.sha256(content.encode()).hexdigest()
    
    def add_documentation(self, path: str, category: DocumentCategory, 
                         title: Optional[str] = None, 
                         description: Optional[str] = None) -> Tuple[bool, str]:
        """Add documentation with duplicate detection.
        
        Returns:
            Tuple of (success, message)
        """
        # Read file content
        file_path = Path(path)
        if not file_path.exists():
            return False, f"File not found: {path}"
        
        content = file_path.read_text()
        content_hash = self.calculate_hash(content)
        
        # Check for duplicates
        for doc_id, doc_info in self.registry['documents'].items():
            if doc_info['hash'] == content_hash:
                return False, f"Duplicate detected! This content already exists as '{doc_info['title']}' in category '{doc_info['category']}'"
            
            if doc_info['path'] == str(path):
                return False, f"Path already registered: {path}"
        
        # Generate unique ID
        doc_id = f"{category.value}_{file_path.stem}_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        # Add to registry
        self.registry['documents'][doc_id] = {
            'path': str(path),
            'category': category.value,
            'hash': content_hash,
            'title': title or file_path.stem,
            'description': description or "",
            'added_date': datetime.now().isoformat(),
            'added_by': 'user'
        }
        
        # Copy file to appropriate category folder
        target_dir = self.agent_dir / "context" / category.value
        target_dir.mkdir(parents=True, exist_ok=True)
        target_file = target_dir / file_path.name
        
        if target_file.exists():
            return False, f"File already exists in {category.value}: {target_file}"
        
        target_file.write_text(content)
        self.save_registry()
        
        return True, f"Successfully added '{title or file_path.stem}' to {category.value} category"
    
    def remove_documentation(self, path: str) -> Tuple[bool, str]:
        """Remove documentation from registry."""
        found = False
        to_remove = []
        
        for doc_id, doc_info in self.registry['documents'].items():
            if doc_info['path'] == str(path):
                found = True
                to_remove.append(doc_id)
                
                # Remove physical file
                category_file = self.agent_dir / "context" / doc_info['category'] / Path(path).name
                if category_file.exists():
                    category_file.unlink()
        
        if not found:
            return False, f"Documentation not found: {path}"
        
        for doc_id in to_remove:
            del self.registry['documents'][doc_id]
        
        self.save_registry()
        return True, f"Removed {len(to_remove)} documentation item(s)"
    
    def list_documentation(self, category: Optional[str] = None) -> str:
        """List all documentation, optionally filtered by category."""
        if not self.registry['documents']:
            return "No documentation registered"
        
        output = []
        categories = {}
        
        for doc_id, doc_info in self.registry['documents'].items():
            doc_cat = doc_info['category']
            if category and doc_cat != category:
                continue
            
            if doc_cat not in categories:
                categories[doc_cat] = []
            
            categories[doc_cat].append(doc_info)
        
        for cat, docs in sorted(categories.items()):
            output.append(f"\n📁 {cat.upper()}:")
            for doc in docs:
                output.append(f"  • {doc['title']}")
                if doc['description']:
                    output.append(f"    {doc['description']}")
                output.append(f"    Path: {doc['path']}")
                output.append(f"    Added: {doc['added_date'][:10]}")
        
        return "\n".join(output) if output else f"No documentation in category '{category}'"


class EnhancedAgentGenerator:
    """Enhanced agent generator with create/update capabilities."""
    
    def __init__(self, base_dir: Path):
        """Initialize enhanced generator."""
        self.base_dir = base_dir
        self.agents_dir = base_dir / "agents"
    
    def create_or_update_agent(self, args: EnhancedParsedArgs) -> CommandResult:
        """Create or update an agent based on mode."""
        agent_dir = self.agents_dir / args.module_name
        
        if args.mode == AgentMode.CREATE:
            if agent_dir.exists():
                return CommandResult(
                    success=False,
                    message=f"Agent '{args.module_name}' already exists. Use --mode update to modify it."
                )
            return self._create_agent(agent_dir, args)
        
        else:  # UPDATE mode
            if not agent_dir.exists():
                return CommandResult(
                    success=False,
                    message=f"Agent '{args.module_name}' does not exist. Use --mode create to create it."
                )
            return self._update_agent(agent_dir, args)
    
    def _create_agent(self, agent_dir: Path, args: EnhancedParsedArgs) -> CommandResult:
        """Create a new agent."""
        try:
            # Create directory structure
            agent_dir.mkdir(parents=True, exist_ok=False)
            
            # Create standard folders
            for folder in ['context/internal', 'context/external', 'context/web', 
                          'context/repository', 'context/optimized', 'prompts', 
                          'templates', 'tools']:
                (agent_dir / folder).mkdir(parents=True, exist_ok=True)
            
            # Create agent.yaml
            config = {
                'name': args.module_name,
                'type': args.type,
                'created': datetime.now().isoformat(),
                'version': '1.0.0',
                'repositories': args.repos,
                'context_cache': args.context_cache,
                'templates': args.templates,
                'documentation': {
                    'internal_sources': [],
                    'external_sources': [],
                    'web_resources': {
                        'enabled': True,
                        'sources': [],
                        'cache_settings': {
                            'max_age_days': 7,
                            'max_size_mb': 100
                        }
                    }
                }
            }
            
            with open(agent_dir / "agent.yaml", 'w') as f:
                yaml.dump(config, f, default_flow_style=False)
            
            # Initialize documentation manager
            doc_manager = DocumentationManager(agent_dir)
            
            # Process documentation operations
            for doc_op in args.documentation_ops:
                if doc_op['operation'] == 'add':
                    success, msg = doc_manager.add_documentation(
                        doc_op['path'],
                        doc_op['category'],
                        doc_op.get('title'),
                        doc_op.get('description')
                    )
                    if not success:
                        print(f"Warning: {msg}")
            
            return CommandResult(
                success=True,
                message=f"Successfully created agent '{args.module_name}'",
                data={'agent_dir': str(agent_dir)}
            )
            
        except Exception as e:
            return CommandResult(
                success=False,
                message=f"Failed to create agent: {str(e)}"
            )
    
    def _update_agent(self, agent_dir: Path, args: EnhancedParsedArgs) -> CommandResult:
        """Update an existing agent."""
        try:
            doc_manager = DocumentationManager(agent_dir)
            results = []
            
            # Process documentation operations
            for doc_op in args.documentation_ops:
                if doc_op['operation'] == 'add':
                    success, msg = doc_manager.add_documentation(
                        doc_op['path'],
                        doc_op['category'],
                        doc_op.get('title'),
                        doc_op.get('description')
                    )
                    results.append(msg)
                    
                elif doc_op['operation'] == 'remove':
                    success, msg = doc_manager.remove_documentation(doc_op['path'])
                    results.append(msg)
                    
                elif doc_op['operation'] == 'list':
                    listing = doc_manager.list_documentation(doc_op.get('category'))
                    results.append(listing)
            
            # Update agent.yaml if needed
            config_file = agent_dir / "agent.yaml"
            if config_file.exists():
                with open(config_file, 'r') as f:
                    config = yaml.safe_load(f)
                
                # Update fields if provided
                if args.repos:
                    config['repositories'] = args.repos
                if args.templates:
                    config['templates'] = args.templates
                
                config['last_updated'] = datetime.now().isoformat()
                
                with open(config_file, 'w') as f:
                    yaml.dump(config, f, default_flow_style=False)
            
            return CommandResult(
                success=True,
                message=f"Successfully updated agent '{args.module_name}'\n" + "\n".join(results),
                data={'agent_dir': str(agent_dir)}
            )
            
        except Exception as e:
            return CommandResult(
                success=False,
                message=f"Failed to update agent: {str(e)}"
            )


def main():
    """Main entry point for enhanced create-module-agent command."""
    import sys
    
    parser = EnhancedArgumentParser()
    generator = EnhancedAgentGenerator(Path.cwd())
    
    try:
        args = parser.parse(sys.argv)
        result = generator.create_or_update_agent(args)
        
        if result.success:
            print(f"✅ {result.message}")
            if result.data:
                print(f"📁 Agent location: {result.data.get('agent_dir', 'N/A')}")
        else:
            print(f"❌ {result.message}")
            sys.exit(1)
            
    except ValueError as e:
        print(f"❌ Error: {e}")
        print("\nUsage:")
        print("  create-module-agent <module-name> [options]")
        print("\nOptions:")
        print("  --mode create|update         Operation mode (default: create)")
        print("  --type <type>               Agent type (default: general-purpose)")
        print("  --repos <repo1,repo2>       Comma-separated repository list")
        print("  --context-cache true|false  Enable context caching (default: true)")
        print("  --templates <t1,t2>         Comma-separated template list")
        print("\nDocumentation Management:")
        print("  --add-doc <path> --category <internal|external|web|repository|optimized>")
        print("           [--title <title>] [--description <description>]")
        print("  --remove-doc <path>         Remove documentation")
        print("  --list-docs [category]      List all or category-specific docs")
        print("\nExamples:")
        print("  # Create new agent with documentation")
        print("  create-module-agent my-agent --add-doc ./docs/api.md --category internal --title 'API Docs'")
        print("\n  # Update existing agent with new documentation")
        print("  create-module-agent my-agent --mode update --add-doc ./specs/feature.md --category external")
        print("\n  # List all documentation in an agent")
        print("  create-module-agent my-agent --mode update --list-docs")
        sys.exit(1)
    except Exception as e:
        print(f"❌ Unexpected error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()