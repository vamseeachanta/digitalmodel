"""
Markdown documentation generator for Create Go-By Folder Tool
"""

from pathlib import Path
from typing import Dict, List, Optional, Any
from datetime import datetime
import logging

from .generator import BaseMetadataGenerator

logger = logging.getLogger(__name__)


class MarkdownDocsGenerator(BaseMetadataGenerator):
    """Generate markdown documentation for go-by folders."""
    
    def generate_metadata(self, scan_results: Dict, patterns: Dict, preservation_stats: Dict) -> Dict:
        """
        Generate markdown documentation files.
        
        Args:
            scan_results: Scanner results
            patterns: Detected patterns
            preservation_stats: Preservation statistics
            
        Returns:
            Dictionary of metadata files created
        """
        # Enhanced agent overview
        agent_overview = self._create_enhanced_agent_overview(scan_results, patterns, preservation_stats)
        self.save_metadata(agent_overview, 'AGENT_OVERVIEW.md')
        
        # Developer guide
        dev_guide = self._create_developer_guide(scan_results, patterns)
        self.save_metadata(dev_guide, 'docs/DEVELOPER_GUIDE.md')
        
        # File structure documentation
        structure_doc = self._create_structure_documentation(scan_results)
        self.save_metadata(structure_doc, 'docs/FILE_STRUCTURE.md')
        
        # Pattern documentation
        pattern_doc = self._create_pattern_documentation(patterns)
        self.save_metadata(pattern_doc, 'docs/PATTERN_GUIDE.md')
        
        # Usage examples
        usage_examples = self._create_usage_examples()
        self.save_metadata(usage_examples, 'docs/USAGE_EXAMPLES.md')
        
        return {
            'markdown_docs_generated': True,
            'files_created': 5
        }
    
    def _create_enhanced_agent_overview(self, scan_results: Dict, patterns: Dict, preservation_stats: Dict) -> str:
        """Create enhanced AGENT_OVERVIEW.md."""
        total_size = scan_results.get('total_size', 0)
        total_files = scan_results.get('total_files', 0)
        final_size = preservation_stats.get('final_size', 0)
        
        content = f"""# 🤖 Agent Quick Start Guide

## 📋 Executive Summary
This go-by folder provides a lightweight, AI-agent-friendly representation of the original folder structure, reducing size by {self.get_size_reduction(total_size, final_size)['percentage']:.1f}% while preserving all essential information.

## 🎯 Purpose & Benefits
- **Quick Understanding**: Grasp folder structure without processing large files
- **Pattern Recognition**: Identify naming conventions and parameter variations
- **Development Templates**: Use preserved originals as untouched references
- **Rapid Prototyping**: Work with representative samples instead of full datasets
- **Memory Efficient**: Process structure in seconds instead of hours

## 📊 Source Information
| Metric | Value |
|--------|-------|
| **Original Path** | `{self.source_path}` |
| **Creation Date** | {self.creation_time.strftime('%Y-%m-%d %H:%M:%S')} |
| **Total Files** | {total_files:,} |
| **Original Size** | {self.format_size(total_size)} |
| **Compressed Size** | {self.format_size(final_size)} |
| **Size Reduction** | {self.get_size_reduction(total_size, final_size)['percentage']:.1f}% |
| **Folder Hash** | `{self.calculate_folder_hash(self.source_path)}` |

## 🗂️ Quick Navigation
```
{self.target_path.name}/
├── _originals/          # Preserved original files (one per type)
├── _templates/          # Pattern templates and examples
├── _samples/            # Representative variation samples
├── metadata/            # Detailed JSON metadata files
├── docs/                # Additional documentation
├── GO_BY_METADATA.json  # Main metadata file
└── AGENT_OVERVIEW.md    # This file
```

## 📈 File Type Distribution
"""
        
        # Create file type table with visual bars
        file_types = scan_results.get('file_types', {})
        if file_types:
            max_count = max(
                info.get('count', 0) if isinstance(info, dict) else info
                for info in file_types.values()
            )
            
            content += "| Extension | Count | Category | Distribution |\n"
            content += "|-----------|-------|----------|-------------|\n"
            
            for ext, info in sorted(file_types.items(), 
                                   key=lambda x: x[1].get('count', 0) if isinstance(x[1], dict) else x[1],
                                   reverse=True)[:15]:
                count = info.get('count', 0) if isinstance(info, dict) else info
                category = self.get_file_category(ext)
                bar_length = int((count / max_count) * 20) if max_count > 0 else 0
                bar = '█' * bar_length + '░' * (20 - bar_length)
                content += f"| {ext or 'none'} | {count} | {category} | {bar} |\n"
        
        # Add pattern section
        if patterns:
            content += "\n## 🔍 Detected Patterns\n"
            content += self._format_patterns_section(patterns)
        
        content += """
## 🚀 Quick Start Commands

### For AI Agents
```python
# Load metadata
import json
with open('GO_BY_METADATA.json') as f:
    metadata = json.load(f)

# Understand structure
file_types = metadata['statistics']['file_types']
patterns = metadata['patterns']

# Work with originals
from pathlib import Path
originals = Path('_originals').glob('*')
```

### For Developers
```bash
# View folder structure
tree -L 2

# Check metadata
cat GO_BY_METADATA.json | jq '.statistics'

# Find original files
ls -la _originals/

# View pattern examples
ls _templates/
```

## 📝 Important Notes

### Preserved Originals
- One file of each type is preserved completely unchanged in `_originals/`
- These serve as reference implementations
- Use for understanding exact format and structure

### Minimized Files
- Large files truncated to first/last 10 lines
- Binary files replaced with descriptive stubs
- Code files preserve structure (imports, functions, classes)

### Restoration
To restore full content:
1. Obtain original files from: `{self.source_path}`
2. Verify with folder hash: `{self.calculate_folder_hash(self.source_path)}`
3. Replace stub files with actual binaries
4. Use metadata to verify completeness

## 🎯 Use Cases

1. **Code Analysis**: Understand project structure without full codebase
2. **Documentation**: Generate docs from structure and patterns
3. **Migration Planning**: Plan migrations based on file organization
4. **Dependency Analysis**: Identify dependencies from imports/configs
5. **Test Generation**: Create tests based on file patterns

## 📚 Additional Resources
- `docs/DEVELOPER_GUIDE.md` - Detailed developer documentation
- `docs/FILE_STRUCTURE.md` - Complete file structure map
- `docs/PATTERN_GUIDE.md` - Pattern analysis and usage
- `docs/USAGE_EXAMPLES.md` - Practical usage examples
- `metadata/` - Detailed JSON metadata files

---
*Generated by digitalmodel.automation.go_by_folder v1.0.0*
"""
        
        return content
    
    def _format_patterns_section(self, patterns: Dict) -> str:
        """Format patterns section for markdown."""
        content = ""
        
        for pattern_type, pattern_data in patterns.items():
            if not pattern_data:
                continue
            
            title = pattern_type.replace('_', ' ').title()
            content += f"\n### {title}\n"
            
            if isinstance(pattern_data, list):
                # List patterns
                for item in pattern_data[:5]:
                    content += f"- `{item}`\n"
                if len(pattern_data) > 5:
                    content += f"- *... and {len(pattern_data) - 5} more*\n"
            
            elif isinstance(pattern_data, dict):
                # Dictionary patterns
                content += "| Pattern | Example | Count |\n"
                content += "|---------|---------|-------|\n"
                for key, value in list(pattern_data.items())[:5]:
                    if isinstance(value, list):
                        content += f"| `{key}` | {value[0] if value else 'N/A'} | {len(value)} |\n"
                    else:
                        content += f"| `{key}` | `{value}` | - |\n"
                if len(pattern_data) > 5:
                    content += f"| *... and {len(pattern_data) - 5} more* | | |\n"
        
        return content
    
    def _create_developer_guide(self, scan_results: Dict, patterns: Dict) -> str:
        """Create developer guide documentation."""
        content = f"""# Developer Guide

## Overview
This go-by folder represents the structure and patterns found in:
`{self.source_path}`

## Working with Go-By Folders

### Understanding the Structure
The go-by folder maintains the original directory structure but with minimized file content:

```
original/                  go-by/
├── src/                  ├── src/
│   ├── main.py (10KB)    │   ├── main.py (1KB - structure only)
│   └── utils.py (5KB)    │   └── utils.py (original - preserved)
└── data/                 └── data/
    └── large.csv (1GB)       └── large.csv (10KB - first/last rows)
```

### File Categories and Handling

| Category | Strategy | Location | Usage |
|----------|----------|----------|-------|
| **Originals** | Preserved unchanged | `_originals/` | Reference implementation |
| **Code** | Structure preserved | In-place | Understand architecture |
| **Config** | Keys preserved | In-place | Understand settings |
| **Data** | Sample rows | In-place | Understand format |
| **Binary** | Stub files | In-place | Know existence |

### Accessing Metadata

```python
import json
from pathlib import Path

# Load main metadata
with open('GO_BY_METADATA.json') as f:
    metadata = json.load(f)

# Access specific metadata
file_inventory = json.load(open('metadata/FILE_INVENTORY.json'))
pattern_analysis = json.load(open('metadata/PATTERN_ANALYSIS.json'))
size_analysis = json.load(open('metadata/SIZE_ANALYSIS.json'))

# Work with preserved originals
originals_dir = Path('_originals')
for original in originals_dir.iterdir():
    print(f"Original: {{original.name}}")
```

### Pattern Usage

"""
        
        if patterns:
            content += "#### Detected Patterns\n"
            for pattern_type, pattern_data in patterns.items():
                if pattern_data:
                    content += f"\n**{pattern_type.replace('_', ' ').title()}**\n"
                    content += f"- Count: {len(pattern_data) if isinstance(pattern_data, (list, dict)) else 1}\n"
                    content += f"- Type: {type(pattern_data).__name__}\n"
        
        content += """
### Development Workflow

1. **Exploration Phase**
   - Review `AGENT_OVERVIEW.md` for quick understanding
   - Check `FILE_STRUCTURE.md` for detailed layout
   - Examine patterns in `PATTERN_GUIDE.md`

2. **Implementation Phase**
   - Use files in `_originals/` as reference
   - Copy structure from minimized files
   - Apply patterns from `_templates/`

3. **Testing Phase**
   - Use `_samples/` for test data
   - Verify against metadata statistics
   - Compare with original folder hash

### Best Practices

1. **Always check originals first** - They contain unchanged reference implementations
2. **Use metadata for verification** - Ensure completeness and accuracy
3. **Respect patterns** - Follow detected naming conventions
4. **Document changes** - Update metadata when modifying structure

## Advanced Usage

### Creating Variations
```python
# Use detected patterns to create variations
patterns = metadata['patterns']
for pattern in patterns.get('numeric_sequences', []):
    # Generate new files following pattern
    pass
```

### Batch Processing
```python
# Process all files of a type
for ext, info in metadata['statistics']['file_types'].items():
    if ext == '.yml':
        # Process all YAML files
        pass
```

### Integration
```python
# Integrate with existing tools
from digitalmodel.automation.go_by_folder import FileScanner
scanner = FileScanner()
# Use go-by folder as template
```

---
*Part of the digitalmodel automation toolkit*
"""
        
        return content
    
    def _create_structure_documentation(self, scan_results: Dict) -> str:
        """Create file structure documentation."""
        content = f"""# File Structure Documentation

## Source Folder
- **Path**: `{self.source_path}`
- **Total Files**: {scan_results.get('total_files', 0):,}
- **Total Directories**: {scan_results.get('total_dirs', 0):,}
- **Total Size**: {self.format_size(scan_results.get('total_size', 0))}

## Directory Tree
```
{self._generate_tree_structure(scan_results)}
```

## File Type Summary

| Extension | Count | Total Size | Avg Size | Category |
|-----------|-------|------------|----------|----------|
"""
        
        file_types = scan_results.get('file_types', {})
        for ext, info in sorted(file_types.items()):
            if isinstance(info, dict):
                count = info.get('count', 0)
                total_size = info.get('total_size', 0)
                avg_size = total_size / count if count > 0 else 0
            else:
                count = info
                total_size = 0
                avg_size = 0
            
            category = self.get_file_category(ext)
            content += f"| {ext or 'none'} | {count} | {self.format_size(total_size)} | "
            content += f"{self.format_size(int(avg_size))} | {category} |\n"
        
        content += """

## Size Distribution

### By Category
"""
        
        # Group by category
        categories = {}
        for ext, info in file_types.items():
            category = self.get_file_category(ext)
            if category not in categories:
                categories[category] = {'count': 0, 'size': 0}
            
            if isinstance(info, dict):
                categories[category]['count'] += info.get('count', 0)
                categories[category]['size'] += info.get('total_size', 0)
            else:
                categories[category]['count'] += info
        
        content += "| Category | File Count | Total Size | Percentage |\n"
        content += "|----------|------------|------------|------------|\n"
        
        total_size = scan_results.get('total_size', 1)  # Avoid division by zero
        for category, stats in sorted(categories.items()):
            percentage = (stats['size'] / total_size * 100) if total_size > 0 else 0
            content += f"| {category} | {stats['count']} | "
            content += f"{self.format_size(stats['size'])} | {percentage:.1f}% |\n"
        
        content += """

## Special Files

### Largest Files
The largest files in the source folder have been minimized or replaced with stubs.

### Configuration Files
Configuration files have been preserved with structure but sensitive values redacted.

### Binary Files
Binary files have been replaced with descriptive stub files containing metadata.

---
*Use this documentation to understand the folder structure without processing large files*
"""
        
        return content
    
    def _generate_tree_structure(self, scan_results: Dict) -> str:
        """Generate a tree structure representation."""
        # Simple tree structure (could be enhanced with actual directory walking)
        tree = f"{self.source_path.name}/\n"
        tree += "├── _originals/      # Preserved original files\n"
        tree += "├── _templates/      # Pattern templates\n"
        tree += "├── _samples/        # Variation samples\n"
        tree += "├── metadata/        # JSON metadata files\n"
        tree += "├── docs/            # Documentation\n"
        tree += "└── [original structure preserved with minimized files]\n"
        
        return tree
    
    def _create_pattern_documentation(self, patterns: Dict) -> str:
        """Create pattern documentation."""
        content = """# Pattern Analysis Guide

## Overview
This document describes the patterns detected in the source folder structure.

## Detected Patterns

"""
        
        if not patterns:
            content += "*No significant patterns detected*\n"
            return content
        
        for pattern_type, pattern_data in patterns.items():
            if not pattern_data:
                continue
            
            title = pattern_type.replace('_', ' ').title()
            content += f"### {title}\n\n"
            
            if pattern_type == 'numeric_sequences':
                content += "Files with numeric sequence patterns:\n\n"
                if isinstance(pattern_data, list):
                    for pattern in pattern_data[:10]:
                        content += f"- `{pattern}`\n"
                content += "\n**Usage**: These patterns indicate batch processing or time series data.\n\n"
            
            elif pattern_type == 'naming_conventions':
                content += "Detected naming conventions:\n\n"
                if isinstance(pattern_data, dict):
                    for convention, examples in pattern_data.items():
                        content += f"**{convention}**:\n"
                        if isinstance(examples, list):
                            for ex in examples[:3]:
                                content += f"- `{ex}`\n"
                        content += "\n"
            
            elif pattern_type == 'parameter_variations':
                content += "Parameter variations found in filenames:\n\n"
                content += "| Parameter | Values | Count |\n"
                content += "|-----------|--------|-------|\n"
                if isinstance(pattern_data, dict):
                    for param, values in pattern_data.items():
                        if isinstance(values, list):
                            value_str = ', '.join(str(v) for v in values[:3])
                            if len(values) > 3:
                                value_str += f" ... ({len(values)} total)"
                            content += f"| `{param}` | {value_str} | {len(values)} |\n"
                content += "\n"
            
            elif pattern_type == 'file_groups':
                content += "Related file groups:\n\n"
                if isinstance(pattern_data, dict):
                    for group, files in pattern_data.items():
                        content += f"**{group}**:\n"
                        if isinstance(files, list):
                            for f in files[:5]:
                                content += f"- `{f}`\n"
                        content += "\n"
        
        content += """
## Using Patterns

### For Automation
```python
# Generate files following detected patterns
patterns = metadata['patterns']
for seq in patterns['numeric_sequences']:
    # Create new files following sequence
    pass
```

### For Analysis
```python
# Analyze parameter variations
params = patterns['parameter_variations']
for param, values in params.items():
    print(f"{param}: {len(values)} variations")
```

### For Testing
```python
# Use patterns for test generation
conventions = patterns['naming_conventions']
# Generate test files following conventions
```

---
*Patterns help understand the organization and enable automation*
"""
        
        return content
    
    def _create_usage_examples(self) -> str:
        """Create usage examples documentation."""
        content = """# Usage Examples

## Python Examples

### Basic Usage
```python
from pathlib import Path
import json

# Load metadata
with open('GO_BY_METADATA.json') as f:
    metadata = json.load(f)

print(f"Original size: {metadata['statistics']['size']['original_formatted']}")
print(f"Reduced to: {metadata['statistics']['size']['final_formatted']}")
print(f"Reduction: {metadata['statistics']['size']['reduction']['percentage']}%")
```

### Working with Originals
```python
# Access preserved original files
originals = Path('_originals')
for original_file in originals.iterdir():
    print(f"Original: {original_file.name}")
    # Use as reference for implementation
    with open(original_file) as f:
        content = f.read()
        # Process original content
```

### Pattern Analysis
```python
# Analyze detected patterns
patterns = metadata['patterns']

# Work with numeric sequences
if 'numeric_sequences' in patterns:
    for sequence in patterns['numeric_sequences']:
        print(f"Sequence pattern: {sequence}")

# Work with parameter variations
if 'parameter_variations' in patterns:
    for param, values in patterns['parameter_variations'].items():
        print(f"Parameter {param} has {len(values)} variations")
```

### File Type Processing
```python
# Process specific file types
file_types = metadata['statistics']['file_types']

for ext, info in file_types.items():
    if ext == '.yml':
        print(f"Found {info['count']} YAML files")
        # Process YAML files
    elif ext == '.py':
        print(f"Found {info['count']} Python files")
        # Analyze Python structure
```

## Command Line Examples

### Quick Analysis
```bash
# View metadata summary
cat GO_BY_METADATA.json | jq '.statistics'

# Check file types
cat GO_BY_METADATA.json | jq '.statistics.file_types'

# View patterns
cat GO_BY_METADATA.json | jq '.patterns'
```

### File Operations
```bash
# List original files
ls -la _originals/

# Check template patterns
ls _templates/

# View sample variations
ls _samples/
```

### Integration
```bash
# Use with other tools
find . -name "*.py" -exec head -5 {} \\;

# Generate report
cat metadata/SIZE_ANALYSIS.json | jq '.reduction'
```

## Use Cases

### 1. Documentation Generation
```python
# Generate documentation from structure
from pathlib import Path

docs = []
for p in Path('.').rglob('*.py'):
    # Extract docstrings from minimized files
    docs.append(extract_docstring(p))

# Create documentation
create_api_docs(docs)
```

### 2. Migration Planning
```python
# Plan migration based on structure
structure = json.load(open('metadata/FILE_INVENTORY.json'))

migration_plan = {
    'files_to_migrate': structure['total_files'],
    'estimated_size': structure['total_size'],
    'file_types': structure['file_types']
}
```

### 3. Test Data Generation
```python
# Generate test data from samples
samples = Path('_samples')
for sample in samples.iterdir():
    # Use as template for test data
    test_data = generate_variations(sample)
```

### 4. Dependency Analysis
```python
# Analyze dependencies from minimized code
imports = []
for py_file in Path('.').rglob('*.py'):
    with open(py_file) as f:
        # Extract imports from minimized structure
        imports.extend(extract_imports(f))

# Analyze dependencies
analyze_dependencies(imports)
```

---
*These examples demonstrate practical usage of go-by folders*
"""
        
        return content