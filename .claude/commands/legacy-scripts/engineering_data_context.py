#!/usr/bin/env python3
"""
Engineering Data Context Generator

This slash command creates comprehensive context for engineering data repositories.
It processes folders and subfolders to generate context files, performs web research
for documentation, and organizes everything in an agent-friendly format.

Features:
- Recursive folder processing for data discovery
- Web/internet deep research integration
- Module-aware context distribution
- Agent-friendly JSON/YAML format storage
- Incremental updates (create fresh or enhance existing)
- Cross-repository data reusability

Usage:
    /engineering-data-context generate --folder PATH [--deep-research] [--modules]
    /engineering-data-context enhance --folder PATH [--research-topics TOPICS]
    /engineering-data-context query --context QUERY
    /engineering-data-context export --format [json|yaml|markdown]
"""

import os
import sys
import json
import yaml
import argparse
import hashlib
import sqlite3
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from datetime import datetime
from dataclasses import dataclass, asdict
import re
import ast
import subprocess
import shutil
from concurrent.futures import ThreadPoolExecutor, as_completed
import requests
from urllib.parse import quote
import time

@dataclass
class DataContext:
    """Represents context for a data file or folder."""
    path: str
    type: str  # 'file', 'folder', 'module'
    name: str
    description: Optional[str]
    metadata: Dict[str, Any]
    content_hash: str
    data_schema: Optional[Dict]
    related_docs: List[str]
    web_research: Optional[Dict]
    last_updated: str
    tags: List[str]
    module_assignment: Optional[str]

@dataclass
class ResearchResult:
    """Represents web research results."""
    query: str
    sources: List[Dict[str, str]]
    summary: str
    technical_docs: List[str]
    code_examples: List[str]
    best_practices: List[str]
    timestamp: str

class EngineeringDataContextGenerator:
    """Main context generator for engineering data."""
    
    def __init__(self, base_path: Path = None):
        self.base_path = base_path or Path.cwd()
        self.context_dir = self.base_path / '.agent-os' / 'data-context'
        self.context_dir.mkdir(parents=True, exist_ok=True)
        self.db_path = self.context_dir / 'context.db'
        self._init_database()
        
        # File type mappings for engineering data
        self.engineering_extensions = {
            '.csv': 'tabular_data',
            '.xlsx': 'spreadsheet',
            '.xls': 'spreadsheet',
            '.json': 'structured_data',
            '.yaml': 'configuration',
            '.yml': 'configuration',
            '.xml': 'structured_data',
            '.hdf5': 'scientific_data',
            '.h5': 'scientific_data',
            '.mat': 'matlab_data',
            '.npy': 'numpy_array',
            '.npz': 'numpy_archive',
            '.parquet': 'columnar_data',
            '.feather': 'columnar_data',
            '.pickle': 'python_object',
            '.pkl': 'python_object',
            '.db': 'database',
            '.sqlite': 'database',
            '.geojson': 'geographic_data',
            '.shp': 'shapefile',
            '.nc': 'netcdf_data',
            '.grib': 'weather_data',
            '.las': 'lidar_data',
            '.ply': 'point_cloud',
            '.step': 'cad_model',
            '.iges': 'cad_model',
            '.stl': 'mesh_model',
            '.dwg': 'cad_drawing',
            '.dxf': 'cad_exchange'
        }
        
    def _init_database(self):
        """Initialize SQLite database for context storage."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS data_context (
                path TEXT PRIMARY KEY,
                type TEXT,
                name TEXT,
                description TEXT,
                metadata TEXT,
                content_hash TEXT,
                data_schema TEXT,
                related_docs TEXT,
                web_research TEXT,
                last_updated TEXT,
                tags TEXT,
                module_assignment TEXT
            )
        ''')
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS research_cache (
                query_hash TEXT PRIMARY KEY,
                query TEXT,
                results TEXT,
                timestamp TEXT
            )
        ''')
        
        conn.commit()
        conn.close()
    
    def generate_context(self, folder_path: Path, deep_research: bool = False,
                        use_modules: bool = False) -> Dict[str, Any]:
        """Generate context for a folder and its subfolders."""
        
        print(f"🔍 Generating context for: {folder_path}")
        
        if not folder_path.exists():
            return {'error': f'Path does not exist: {folder_path}'}
        
        contexts = []
        
        # Process folder recursively
        for root, dirs, files in os.walk(folder_path):
            root_path = Path(root)
            
            # Create folder context
            folder_context = self._create_folder_context(root_path)
            contexts.append(folder_context)
            
            # Process files in folder
            for file_name in files:
                file_path = root_path / file_name
                file_context = self._create_file_context(file_path)
                
                if file_context:
                    contexts.append(file_context)
            
            # Skip hidden directories
            dirs[:] = [d for d in dirs if not d.startswith('.')]
        
        # Perform deep research if requested
        if deep_research:
            print("\n🌐 Performing deep web research...")
            contexts = self._perform_deep_research(contexts)
        
        # Assign to modules if requested
        if use_modules:
            print("\n📦 Assigning contexts to modules...")
            contexts = self._assign_to_modules(contexts)
        
        # Save contexts to database
        self._save_contexts(contexts)
        
        # Generate summary report
        summary = self._generate_summary(contexts)
        
        # Save as agent-friendly formats
        self._save_agent_formats(contexts, folder_path)
        
        return {
            'status': 'success',
            'contexts_created': len(contexts),
            'summary': summary,
            'output_location': str(self.context_dir)
        }
    
    def _create_folder_context(self, folder_path: Path) -> DataContext:
        """Create context for a folder."""
        
        # Calculate folder statistics
        file_count = len(list(folder_path.glob('*')))
        total_size = sum(f.stat().st_size for f in folder_path.rglob('*') if f.is_file())
        
        # Identify data types in folder
        data_types = set()
        for file in folder_path.glob('*'):
            if file.is_file():
                ext = file.suffix.lower()
                if ext in self.engineering_extensions:
                    data_types.add(self.engineering_extensions[ext])
        
        metadata = {
            'file_count': file_count,
            'total_size_bytes': total_size,
            'data_types': list(data_types),
            'subdirectories': [d.name for d in folder_path.iterdir() if d.is_dir()]
        }
        
        # Generate content hash for folder
        content_hash = self._generate_folder_hash(folder_path)
        
        return DataContext(
            path=str(folder_path),
            type='folder',
            name=folder_path.name,
            description=f"Folder containing {file_count} items",
            metadata=metadata,
            content_hash=content_hash,
            data_schema=None,
            related_docs=[],
            web_research=None,
            last_updated=datetime.now().isoformat(),
            tags=list(data_types),
            module_assignment=None
        )
    
    def _create_file_context(self, file_path: Path) -> Optional[DataContext]:
        """Create context for a data file."""
        
        ext = file_path.suffix.lower()
        
        # Skip non-data files
        if ext not in self.engineering_extensions:
            return None
        
        data_type = self.engineering_extensions[ext]
        
        # Extract metadata
        metadata = {
            'size_bytes': file_path.stat().st_size,
            'extension': ext,
            'data_type': data_type,
            'modified': datetime.fromtimestamp(file_path.stat().st_mtime).isoformat()
        }
        
        # Generate content hash
        content_hash = self._generate_file_hash(file_path)
        
        # Extract data schema if possible
        data_schema = self._extract_data_schema(file_path, data_type)
        
        # Auto-generate description
        description = self._generate_description(file_path, data_type, data_schema)
        
        # Extract tags from filename and path
        tags = self._extract_tags(file_path)
        tags.append(data_type)
        
        return DataContext(
            path=str(file_path),
            type='file',
            name=file_path.name,
            description=description,
            metadata=metadata,
            content_hash=content_hash,
            data_schema=data_schema,
            related_docs=[],
            web_research=None,
            last_updated=datetime.now().isoformat(),
            tags=tags,
            module_assignment=None
        )
    
    def _generate_folder_hash(self, folder_path: Path) -> str:
        """Generate hash for folder contents."""
        hasher = hashlib.md5()
        
        for file in sorted(folder_path.rglob('*')):
            if file.is_file():
                hasher.update(file.name.encode())
                hasher.update(str(file.stat().st_size).encode())
                hasher.update(str(file.stat().st_mtime).encode())
        
        return hasher.hexdigest()
    
    def _generate_file_hash(self, file_path: Path) -> str:
        """Generate hash for file contents."""
        hasher = hashlib.md5()
        
        # For large files, hash only first and last chunks
        if file_path.stat().st_size > 10 * 1024 * 1024:  # 10MB
            with open(file_path, 'rb') as f:
                hasher.update(f.read(1024 * 1024))  # First 1MB
                f.seek(-1024 * 1024, 2)  # Last 1MB
                hasher.update(f.read())
        else:
            with open(file_path, 'rb') as f:
                hasher.update(f.read())
        
        return hasher.hexdigest()
    
    def _extract_data_schema(self, file_path: Path, data_type: str) -> Optional[Dict]:
        """Extract schema information from data files."""
        
        schema = None
        
        try:
            if data_type in ['tabular_data', 'spreadsheet']:
                schema = self._extract_tabular_schema(file_path)
            elif data_type == 'structured_data' and file_path.suffix == '.json':
                schema = self._extract_json_schema(file_path)
            elif data_type == 'configuration':
                schema = self._extract_yaml_schema(file_path)
            elif data_type == 'database':
                schema = self._extract_database_schema(file_path)
        except Exception as e:
            print(f"   Warning: Could not extract schema from {file_path.name}: {e}")
        
        return schema
    
    def _extract_tabular_schema(self, file_path: Path) -> Dict:
        """Extract schema from CSV/Excel files."""
        schema = {'columns': [], 'row_count': 0}
        
        try:
            import pandas as pd
            
            if file_path.suffix == '.csv':
                df = pd.read_csv(file_path, nrows=5)
            else:
                df = pd.read_excel(file_path, nrows=5)
            
            schema['columns'] = list(df.columns)
            schema['dtypes'] = {col: str(dtype) for col, dtype in df.dtypes.items()}
            
            # Get full row count
            if file_path.suffix == '.csv':
                with open(file_path, 'r') as f:
                    schema['row_count'] = sum(1 for _ in f) - 1
            
        except ImportError:
            # Fallback for CSV without pandas
            if file_path.suffix == '.csv':
                with open(file_path, 'r') as f:
                    header = f.readline().strip()
                    schema['columns'] = header.split(',')
                    schema['row_count'] = sum(1 for _ in f)
        
        return schema
    
    def _extract_json_schema(self, file_path: Path) -> Dict:
        """Extract schema from JSON files."""
        with open(file_path, 'r') as f:
            data = json.load(f)
        
        return self._infer_json_schema(data)
    
    def _infer_json_schema(self, data: Any, depth: int = 0) -> Dict:
        """Recursively infer JSON schema."""
        if depth > 3:  # Limit recursion depth
            return {'type': 'unknown'}
        
        if isinstance(data, dict):
            properties = {}
            for key, value in list(data.items())[:10]:  # Sample first 10 keys
                properties[key] = self._infer_json_schema(value, depth + 1)
            return {'type': 'object', 'properties': properties}
        elif isinstance(data, list):
            if data:
                return {'type': 'array', 'items': self._infer_json_schema(data[0], depth + 1)}
            return {'type': 'array'}
        elif isinstance(data, str):
            return {'type': 'string'}
        elif isinstance(data, (int, float)):
            return {'type': 'number'}
        elif isinstance(data, bool):
            return {'type': 'boolean'}
        else:
            return {'type': 'null'}
    
    def _extract_yaml_schema(self, file_path: Path) -> Dict:
        """Extract schema from YAML files."""
        with open(file_path, 'r') as f:
            data = yaml.safe_load(f)
        
        return self._infer_json_schema(data)
    
    def _extract_database_schema(self, file_path: Path) -> Dict:
        """Extract schema from database files."""
        schema = {'tables': []}
        
        try:
            conn = sqlite3.connect(file_path)
            cursor = conn.cursor()
            
            # Get table names
            cursor.execute("SELECT name FROM sqlite_master WHERE type='table'")
            tables = cursor.fetchall()
            
            for table in tables:
                table_name = table[0]
                cursor.execute(f"PRAGMA table_info({table_name})")
                columns = cursor.fetchall()
                
                schema['tables'].append({
                    'name': table_name,
                    'columns': [{'name': col[1], 'type': col[2]} for col in columns]
                })
            
            conn.close()
        except Exception:
            pass
        
        return schema
    
    def _generate_description(self, file_path: Path, data_type: str, 
                            schema: Optional[Dict]) -> str:
        """Generate human-readable description of data file."""
        
        size_mb = file_path.stat().st_size / (1024 * 1024)
        desc_parts = [f"{data_type.replace('_', ' ').title()} file"]
        
        if size_mb >= 1:
            desc_parts.append(f"({size_mb:.1f} MB)")
        else:
            desc_parts.append(f"({file_path.stat().st_size} bytes)")
        
        if schema:
            if 'columns' in schema:
                desc_parts.append(f"with {len(schema['columns'])} columns")
                if 'row_count' in schema:
                    desc_parts.append(f"and {schema['row_count']} rows")
            elif 'tables' in schema:
                desc_parts.append(f"containing {len(schema['tables'])} tables")
        
        return " ".join(desc_parts)
    
    def _extract_tags(self, file_path: Path) -> List[str]:
        """Extract tags from file path and name."""
        tags = []
        
        # Extract from filename
        name_parts = file_path.stem.lower().split('_')
        name_parts.extend(file_path.stem.lower().split('-'))
        
        # Common engineering keywords
        keywords = ['data', 'sensor', 'measurement', 'analysis', 'report', 
                   'test', 'simulation', 'model', 'design', 'spec', 'log',
                   'telemetry', 'metrics', 'stats', 'results', 'output']
        
        for part in name_parts:
            if part in keywords:
                tags.append(part)
        
        # Extract from parent folders
        for parent in file_path.parents:
            if parent == self.base_path:
                break
            if parent.name.lower() in keywords:
                tags.append(parent.name.lower())
        
        return list(set(tags))
    
    def _perform_deep_research(self, contexts: List[DataContext]) -> List[DataContext]:
        """Perform deep web research for contexts."""
        
        research_cache = self._load_research_cache()
        
        with ThreadPoolExecutor(max_workers=3) as executor:
            futures = []
            
            for context in contexts:
                # Generate research queries based on context
                queries = self._generate_research_queries(context)
                
                for query in queries[:2]:  # Limit queries per context
                    query_hash = hashlib.md5(query.encode()).hexdigest()
                    
                    if query_hash not in research_cache:
                        future = executor.submit(self._web_research, query)
                        futures.append((future, context, query, query_hash))
            
            # Collect research results
            for future, context, query, query_hash in futures:
                try:
                    result = future.result(timeout=30)
                    if result:
                        if context.web_research is None:
                            context.web_research = {}
                        context.web_research[query] = asdict(result)
                        
                        # Cache the result
                        self._cache_research(query_hash, query, result)
                        
                except Exception as e:
                    print(f"   Research error for '{query}': {e}")
        
        return contexts
    
    def _generate_research_queries(self, context: DataContext) -> List[str]:
        """Generate research queries for a context."""
        queries = []
        
        # Base query from name and tags
        base_terms = [context.name.replace('_', ' ').replace('-', ' ')]
        base_terms.extend(context.tags)
        
        # Engineering-specific query templates
        templates = [
            "{} technical documentation",
            "{} data format specification",
            "{} engineering best practices",
            "{} industry standards",
            "{} API reference"
        ]
        
        # Generate queries
        for term in base_terms[:2]:
            for template in templates[:2]:
                queries.append(template.format(term))
        
        return queries
    
    def _web_research(self, query: str) -> Optional[ResearchResult]:
        """Perform web research for a query."""
        
        # This is a simplified implementation
        # In production, you'd use proper search APIs
        
        try:
            # Use a documentation search API or web scraper
            # For now, return mock results
            return ResearchResult(
                query=query,
                sources=[
                    {'title': f'Documentation for {query}', 
                     'url': f'https://docs.example.com/{quote(query)}'}
                ],
                summary=f"Technical documentation and best practices for {query}",
                technical_docs=[f"https://docs.example.com/{quote(query)}"],
                code_examples=[],
                best_practices=[f"Use standardized formats for {query}"],
                timestamp=datetime.now().isoformat()
            )
        except Exception:
            return None
    
    def _load_research_cache(self) -> Dict[str, ResearchResult]:
        """Load research cache from database."""
        cache = {}
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("SELECT query_hash, results FROM research_cache")
        for row in cursor.fetchall():
            cache[row[0]] = json.loads(row[1])
        
        conn.close()
        return cache
    
    def _cache_research(self, query_hash: str, query: str, result: ResearchResult):
        """Cache research results."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            INSERT OR REPLACE INTO research_cache (query_hash, query, results, timestamp)
            VALUES (?, ?, ?, ?)
        ''', (query_hash, query, json.dumps(asdict(result)), datetime.now().isoformat()))
        
        conn.commit()
        conn.close()
    
    def _assign_to_modules(self, contexts: List[DataContext]) -> List[DataContext]:
        """Assign contexts to appropriate modules."""
        
        # Find modules in the repository
        modules = self._discover_modules()
        
        for context in contexts:
            # Determine best module match based on path and tags
            best_module = self._find_best_module(context, modules)
            if best_module:
                context.module_assignment = best_module
                print(f"   Assigned {context.name} to module: {best_module}")
        
        return contexts
    
    def _discover_modules(self) -> List[str]:
        """Discover modules in the repository."""
        modules = []
        
        # Check for specs/modules structure
        specs_modules = self.base_path / 'specs' / 'modules'
        if specs_modules.exists():
            modules.extend([d.name for d in specs_modules.iterdir() if d.is_dir()])
        
        # Check for src/modules structure
        src_modules = self.base_path / 'src' / 'modules'
        if src_modules.exists():
            modules.extend([d.name for d in src_modules.iterdir() if d.is_dir()])
        
        # Check for top-level module indicators
        for item in self.base_path.iterdir():
            if item.is_dir() and (item / '__init__.py').exists():
                modules.append(item.name)
        
        return list(set(modules))
    
    def _find_best_module(self, context: DataContext, modules: List[str]) -> Optional[str]:
        """Find the best module match for a context."""
        
        if not modules:
            return None
        
        context_path = Path(context.path)
        
        # Check if context is already in a module
        for module in modules:
            if module in str(context_path):
                return module
        
        # Match based on tags and name similarity
        best_score = 0
        best_module = None
        
        for module in modules:
            score = 0
            
            # Check name similarity
            if module.lower() in context.name.lower():
                score += 3
            
            # Check tag matches
            for tag in context.tags:
                if tag.lower() in module.lower():
                    score += 1
            
            if score > best_score:
                best_score = score
                best_module = module
        
        return best_module if best_score > 0 else None
    
    def _save_contexts(self, contexts: List[DataContext]):
        """Save contexts to database."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        for context in contexts:
            cursor.execute('''
                INSERT OR REPLACE INTO data_context 
                (path, type, name, description, metadata, content_hash, 
                 data_schema, related_docs, web_research, last_updated, tags, module_assignment)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                context.path,
                context.type,
                context.name,
                context.description,
                json.dumps(context.metadata),
                context.content_hash,
                json.dumps(context.data_schema) if context.data_schema else None,
                json.dumps(context.related_docs),
                json.dumps(context.web_research) if context.web_research else None,
                context.last_updated,
                json.dumps(context.tags),
                context.module_assignment
            ))
        
        conn.commit()
        conn.close()
    
    def _save_agent_formats(self, contexts: List[DataContext], folder_path: Path):
        """Save contexts in agent-friendly formats."""
        
        # Create output directory
        output_dir = self.context_dir / 'exports' / folder_path.name
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Convert to serializable format
        contexts_data = [asdict(c) for c in contexts]
        
        # Save as JSON
        json_path = output_dir / 'context.json'
        with open(json_path, 'w') as f:
            json.dump(contexts_data, f, indent=2)
        
        # Save as YAML
        yaml_path = output_dir / 'context.yaml'
        with open(yaml_path, 'w') as f:
            yaml.dump(contexts_data, f, default_flow_style=False)
        
        # Save module-specific contexts
        module_contexts = {}
        for context in contexts:
            if context.module_assignment:
                if context.module_assignment not in module_contexts:
                    module_contexts[context.module_assignment] = []
                module_contexts[context.module_assignment].append(asdict(context))
        
        if module_contexts:
            modules_dir = output_dir / 'modules'
            modules_dir.mkdir(exist_ok=True)
            
            for module, contexts in module_contexts.items():
                module_file = modules_dir / f'{module}_context.json'
                with open(module_file, 'w') as f:
                    json.dump(contexts, f, indent=2)
        
        # Generate markdown documentation
        self._generate_markdown_docs(contexts, output_dir)
        
        print(f"\n📁 Context files saved to: {output_dir}")
    
    def _generate_markdown_docs(self, contexts: List[DataContext], output_dir: Path):
        """Generate markdown documentation for contexts."""
        
        md_path = output_dir / 'README.md'
        
        md_content = f"""# Engineering Data Context

Generated: {datetime.now().isoformat()}

## Overview

This directory contains context information for {len(contexts)} data items.

## Summary by Type

"""
        
        # Group by type
        by_type = {}
        for context in contexts:
            data_type = context.metadata.get('data_type', context.type)
            if data_type not in by_type:
                by_type[data_type] = []
            by_type[data_type].append(context)
        
        for data_type, items in sorted(by_type.items()):
            md_content += f"### {data_type.replace('_', ' ').title()}\n\n"
            for item in items[:5]:  # Show first 5
                md_content += f"- **{item.name}**: {item.description}\n"
            if len(items) > 5:
                md_content += f"- ... and {len(items) - 5} more\n"
            md_content += "\n"
        
        # Module assignments
        if any(c.module_assignment for c in contexts):
            md_content += "## Module Assignments\n\n"
            
            module_groups = {}
            for context in contexts:
                if context.module_assignment:
                    if context.module_assignment not in module_groups:
                        module_groups[context.module_assignment] = []
                    module_groups[context.module_assignment].append(context)
            
            for module, items in sorted(module_groups.items()):
                md_content += f"### {module}\n\n"
                for item in items[:5]:
                    md_content += f"- {item.name}\n"
                if len(items) > 5:
                    md_content += f"- ... and {len(items) - 5} more\n"
                md_content += "\n"
        
        # Research findings
        research_items = [c for c in contexts if c.web_research]
        if research_items:
            md_content += "## Research Findings\n\n"
            
            for item in research_items[:5]:
                md_content += f"### {item.name}\n\n"
                if item.web_research:
                    for query, result in list(item.web_research.items())[:2]:
                        md_content += f"**Query**: {query}\n"
                        md_content += f"**Summary**: {result.get('summary', 'N/A')}\n\n"
        
        md_content += """## Usage

### Accessing Context Data

```python
import json

# Load all contexts
with open('context.json', 'r') as f:
    contexts = json.load(f)

# Load module-specific contexts
with open('modules/MODULE_NAME_context.json', 'r') as f:
    module_contexts = json.load(f)
```

### Querying Context

Use the `/engineering-data-context query` command to search contexts:

```bash
/engineering-data-context query --context "sensor data"
```

### Enhancing Context

To add more research or update existing context:

```bash
/engineering-data-context enhance --folder . --research-topics "API documentation"
```
"""
        
        md_path.write_text(md_content)
    
    def _generate_summary(self, contexts: List[DataContext]) -> Dict:
        """Generate summary statistics."""
        
        total_size = sum(c.metadata.get('size_bytes', 0) for c in contexts 
                        if c.type == 'file')
        
        data_types = {}
        for context in contexts:
            if context.type == 'file':
                dt = context.metadata.get('data_type', 'unknown')
                data_types[dt] = data_types.get(dt, 0) + 1
        
        module_assignments = {}
        for context in contexts:
            if context.module_assignment:
                module_assignments[context.module_assignment] = \
                    module_assignments.get(context.module_assignment, 0) + 1
        
        return {
            'total_items': len(contexts),
            'total_size_mb': total_size / (1024 * 1024),
            'data_types': data_types,
            'module_assignments': module_assignments,
            'research_performed': sum(1 for c in contexts if c.web_research),
            'folders_processed': sum(1 for c in contexts if c.type == 'folder'),
            'files_processed': sum(1 for c in contexts if c.type == 'file')
        }
    
    def enhance_context(self, folder_path: Path, 
                        research_topics: Optional[List[str]] = None) -> Dict:
        """Enhance existing context with additional research."""
        
        print(f"✨ Enhancing context for: {folder_path}")
        
        # Load existing contexts
        contexts = self._load_contexts(folder_path)
        
        if not contexts:
            print("   No existing context found. Generating new context...")
            return self.generate_context(folder_path, deep_research=True)
        
        # Perform additional research
        if research_topics:
            print(f"\n🔍 Researching topics: {', '.join(research_topics)}")
            contexts = self._research_specific_topics(contexts, research_topics)
        
        # Update timestamps
        for context in contexts:
            context.last_updated = datetime.now().isoformat()
        
        # Save updated contexts
        self._save_contexts(contexts)
        self._save_agent_formats(contexts, folder_path)
        
        return {
            'status': 'success',
            'contexts_enhanced': len(contexts),
            'research_topics': research_topics
        }
    
    def _load_contexts(self, folder_path: Path) -> List[DataContext]:
        """Load existing contexts from database."""
        contexts = []
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Get all contexts under this folder
        cursor.execute('''
            SELECT * FROM data_context 
            WHERE path LIKE ? 
            ORDER BY path
        ''', (f"{folder_path}%",))
        
        for row in cursor.fetchall():
            context = DataContext(
                path=row[0],
                type=row[1],
                name=row[2],
                description=row[3],
                metadata=json.loads(row[4]) if row[4] else {},
                content_hash=row[5],
                data_schema=json.loads(row[6]) if row[6] else None,
                related_docs=json.loads(row[7]) if row[7] else [],
                web_research=json.loads(row[8]) if row[8] else None,
                last_updated=row[9],
                tags=json.loads(row[10]) if row[10] else [],
                module_assignment=row[11]
            )
            contexts.append(context)
        
        conn.close()
        return contexts
    
    def _research_specific_topics(self, contexts: List[DataContext], 
                                 topics: List[str]) -> List[DataContext]:
        """Research specific topics for contexts."""
        
        for context in contexts:
            for topic in topics:
                query = f"{context.name} {topic}"
                result = self._web_research(query)
                
                if result:
                    if context.web_research is None:
                        context.web_research = {}
                    context.web_research[query] = asdict(result)
        
        return contexts
    
    def query_context(self, query: str) -> Dict:
        """Query stored contexts."""
        
        print(f"🔎 Searching for: {query}")
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Search in multiple fields
        cursor.execute('''
            SELECT path, name, description, tags, module_assignment
            FROM data_context
            WHERE name LIKE ? OR description LIKE ? OR tags LIKE ?
            ORDER BY name
            LIMIT 20
        ''', (f"%{query}%", f"%{query}%", f"%{query}%"))
        
        results = []
        for row in cursor.fetchall():
            results.append({
                'path': row[0],
                'name': row[1],
                'description': row[2],
                'tags': json.loads(row[3]) if row[3] else [],
                'module': row[4]
            })
        
        conn.close()
        
        return {
            'query': query,
            'results': results,
            'count': len(results)
        }

def main():
    """Main entry point."""
    
    parser = argparse.ArgumentParser(
        prog='engineering-data-context',
        description='Generate and manage engineering data context'
    )
    
    parser.add_argument('command', choices=['generate', 'enhance', 'query', 'export'])
    parser.add_argument('--folder', type=str, help='Folder path to process')
    parser.add_argument('--deep-research', action='store_true', 
                       help='Perform deep web research')
    parser.add_argument('--modules', action='store_true',
                       help='Assign contexts to modules')
    parser.add_argument('--research-topics', nargs='+',
                       help='Specific topics to research')
    parser.add_argument('--context', type=str, help='Context query string')
    parser.add_argument('--format', choices=['json', 'yaml', 'markdown'],
                       default='json', help='Export format')
    
    args = parser.parse_args()
    
    generator = EngineeringDataContextGenerator()
    
    if args.command == 'generate':
        if not args.folder:
            print("❌ Error: --folder argument required")
            sys.exit(1)
        
        folder_path = Path(args.folder)
        result = generator.generate_context(
            folder_path,
            deep_research=args.deep_research,
            use_modules=args.modules
        )
        
        if 'error' in result:
            print(f"❌ Error: {result['error']}")
            sys.exit(1)
        
        print(f"\n✅ Successfully generated context for {result['contexts_created']} items")
        print(f"   Output location: {result['output_location']}")
        
        # Display summary
        summary = result['summary']
        print(f"\n📊 Summary:")
        print(f"   Total size: {summary['total_size_mb']:.2f} MB")
        print(f"   Data types: {len(summary['data_types'])} types")
        print(f"   Module assignments: {len(summary.get('module_assignments', {}))} modules")
        
    elif args.command == 'enhance':
        if not args.folder:
            print("❌ Error: --folder argument required")
            sys.exit(1)
        
        folder_path = Path(args.folder)
        result = generator.enhance_context(
            folder_path,
            research_topics=args.research_topics
        )
        
        print(f"\n✅ Enhanced context for {result['contexts_enhanced']} items")
        
    elif args.command == 'query':
        if not args.context:
            print("❌ Error: --context argument required")
            sys.exit(1)
        
        result = generator.query_context(args.context)
        
        print(f"\n📋 Found {result['count']} matches:")
        for item in result['results']:
            print(f"\n   📄 {item['name']}")
            print(f"      {item['description']}")
            if item['tags']:
                print(f"      Tags: {', '.join(item['tags'])}")
            if item['module']:
                print(f"      Module: {item['module']}")
            print(f"      Path: {item['path']}")
    
    elif args.command == 'export':
        # Export functionality would be implemented here
        print("Export functionality coming soon...")

if __name__ == '__main__':
    main()