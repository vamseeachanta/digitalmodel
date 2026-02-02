"""
Base metadata generator for Create Go-By Folder Tool
"""

from pathlib import Path
from typing import Dict, Any, List, Optional, Tuple
from datetime import datetime
import json
import yaml
import hashlib
import logging
from abc import ABC, abstractmethod

logger = logging.getLogger(__name__)


class BaseMetadataGenerator(ABC):
    """Base class for metadata generation."""
    
    def __init__(self, source_path: Path, target_path: Path, config: Optional[Dict] = None):
        """
        Initialize metadata generator.
        
        Args:
            source_path: Path to source folder
            target_path: Path to target go-by folder
            config: Optional configuration dictionary
        """
        self.source_path = source_path
        self.target_path = target_path
        self.config = config or {}
        self.metadata = {}
        self.creation_time = datetime.now()
    
    @abstractmethod
    def generate_metadata(self, scan_results: Dict, patterns: Dict, preservation_stats: Dict) -> Dict:
        """Generate metadata specific to this generator."""
        pass
    
    def save_metadata(self, metadata: Dict, filename: str) -> Path:
        """Save metadata to file."""
        output_path = self.target_path / filename
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        if filename.endswith('.json'):
            with open(output_path, 'w', encoding='utf-8') as f:
                json.dump(metadata, f, indent=2, default=str)
        elif filename.endswith(('.yaml', '.yml')):
            with open(output_path, 'w', encoding='utf-8') as f:
                yaml.dump(metadata, f, default_flow_style=False, sort_keys=False)
        elif filename.endswith('.md'):
            output_path.write_text(str(metadata), encoding='utf-8')
        else:
            output_path.write_text(str(metadata), encoding='utf-8')
        
        logger.info(f"Created {output_path}")
        return output_path
    
    def calculate_folder_hash(self, path: Path) -> str:
        """Calculate a hash representing folder structure."""
        hasher = hashlib.sha256()
        for file_path in sorted(path.rglob('*')):
            if file_path.is_file():
                hasher.update(str(file_path.relative_to(path)).encode())
                hasher.update(str(file_path.stat().st_size).encode())
        return hasher.hexdigest()[:16]
    
    def get_size_reduction(self, original_size: int, current_size: int) -> Dict:
        """Calculate size reduction metrics."""
        if original_size == 0:
            return {'percentage': 0, 'factor': 0, 'saved': 0}
        
        reduction = original_size - current_size
        percentage = (reduction / original_size) * 100
        factor = original_size / current_size if current_size > 0 else float('inf')
        
        return {
            'percentage': round(percentage, 2),
            'factor': round(factor, 2),
            'saved_bytes': reduction,
            'saved_formatted': self.format_size(reduction)
        }
    
    def format_size(self, size_bytes: int) -> str:
        """
        Format size in human-readable format.
        
        Args:
            size_bytes: Size in bytes
            
        Returns:
            Formatted size string
        """
        for unit in ['B', 'KB', 'MB', 'GB']:
            if size_bytes < 1024.0:
                return f"{size_bytes:.2f}{unit}"
            size_bytes /= 1024.0
        return f"{size_bytes:.2f}TB"
    
    def get_file_category(self, extension: str) -> str:
        """Categorize file by extension."""
        categories = {
            'code': {'.py', '.js', '.ts', '.java', '.cpp', '.c', '.h', '.cs', '.go', '.rs'},
            'config': {'.json', '.yaml', '.yml', '.toml', '.ini', '.cfg', '.xml'},
            'data': {'.csv', '.tsv', '.dat', '.db', '.sqlite'},
            'docs': {'.md', '.txt', '.rst', '.pdf', '.docx', '.html'},
            'binary': {'.exe', '.dll', '.so', '.sim', '.bin'},
            'image': {'.png', '.jpg', '.jpeg', '.gif', '.svg', '.bmp'},
            'archive': {'.zip', '.tar', '.gz', '.7z', '.rar'}
        }
        
        for category, extensions in categories.items():
            if extension.lower() in extensions:
                return category
        return 'other'


class MetadataGenerator(BaseMetadataGenerator):
    """Main metadata generator that orchestrates all metadata creation."""
    
    def __init__(self, source_path: Path, target_path: Path, config: Optional[Dict] = None):
        super().__init__(source_path, target_path, config)
        self.generators = []
    
    def add_generator(self, generator: BaseMetadataGenerator) -> None:
        """Add a metadata generator."""
        self.generators.append(generator)
    
    def generate_metadata(self, scan_results: Dict, patterns: Dict, preservation_stats: Dict) -> Dict:
        """Generate all metadata using registered generators."""
        all_metadata = {}
        for generator in self.generators:
            metadata = generator.generate_metadata(scan_results, patterns, preservation_stats)
            all_metadata.update(metadata)
        return all_metadata
    
    def generate_all_metadata(
        self,
        scan_results: Dict,
        patterns: Dict,
        preservation_stats: Dict
    ) -> None:
        """
        Generate all metadata files.

        Args:
            scan_results: Results from file scanner
            patterns: Detected patterns
            preservation_stats: Preservation statistics
        """
        # Generate GO_BY_METADATA.json
        self.create_go_by_metadata(scan_results, patterns, preservation_stats)

        # Generate AGENT_OVERVIEW.md
        self.create_agent_overview(scan_results, patterns)

        # Call all registered generators
        for generator in self.generators:
            try:
                generator.generate_metadata(scan_results, patterns, preservation_stats)
            except Exception as e:
                logger.warning(f"Error in generator {generator.__class__.__name__}: {e}")

        logger.info("Generated all metadata files")
    
    def create_go_by_metadata(
        self,
        scan_results: Dict,
        patterns: Dict,
        preservation_stats: Dict
    ) -> None:
        """
        Create GO_BY_METADATA.json file.
        
        Args:
            scan_results: Scanner results
            patterns: Detected patterns
            preservation_stats: Preservation statistics
        """
        metadata = {
            'version': '1.0.0',
            'tool': 'digitalmodel.workflows.automation.go_by_folder',
            'original_path': str(self.source_path),
            'creation_date': self.creation_time.isoformat(),
            'folder_hash': self.calculate_folder_hash(self.source_path),
            'statistics': {
                **scan_results,
                'size_reduction': self.get_size_reduction(
                    scan_results.get('total_size', 0),
                    preservation_stats.get('final_size', 0)
                )
            },
            'patterns': patterns,
            'preservation': preservation_stats,
            'sampling_rules': {
                'large_files': 'truncated_to_10KB',
                'binaries': 'replaced_with_stubs',
                'sequences': 'kept_first_middle_last',
                'originals': 'one_of_each_type_preserved'
            }
        }
        
        output_path = self.target_path / 'GO_BY_METADATA.json'
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(metadata, f, indent=2, default=str)
        
        logger.info(f"Created {output_path}")
    
    def create_agent_overview(
        self,
        scan_results: Dict,
        patterns: Dict
    ) -> None:
        """
        Create AGENT_OVERVIEW.md for AI agents.
        
        Args:
            scan_results: Scanner results
            patterns: Detected patterns
        """
        content = self.generate_overview_content(scan_results, patterns)
        
        output_path = self.target_path / 'AGENT_OVERVIEW.md'
        output_path.write_text(content, encoding='utf-8')
        
        logger.info(f"Created {output_path}")
    
    def generate_overview_content(
        self,
        scan_results: Dict,
        patterns: Dict
    ) -> str:
        """
        Generate content for AGENT_OVERVIEW.md.
        
        Args:
            scan_results: Scanner results
            patterns: Detected patterns
            
        Returns:
            Markdown content string
        """
        total_size = scan_results.get('total_size', 0)
        total_files = scan_results.get('total_files', 0)
        
        content = f"""# 🤖 Agent Quick Start Guide

## Purpose
This go-by folder is a lightweight representation of: {self.source_path}

## Source Information
- **Original Folder**: {self.source_path}
- **Creation Date**: {self.creation_time.isoformat()}
- **Total Files**: {total_files}
- **Total Size**: {self.format_size(total_size)}
- **Folder Hash**: {self.calculate_folder_hash(self.source_path)}

## Quick Navigation
- Original preserved files: `./_originals/`
- Template patterns: `./_templates/`
- Sample files: `./_samples/`
- Metadata: `./GO_BY_METADATA.json`

## File Types Present
| Extension | Count | Category |
|-----------|-------|----------|
"""
        
        file_types = scan_results.get('file_types', {})
        for ext, info in sorted(file_types.items()):
            count = info.get('count', 0) if isinstance(info, dict) else info
            category = self.get_file_category(ext)
            content += f"| {ext or 'no extension'} | {count} | {category} |\n"
        
        # Add pattern information
        if patterns:
            content += "\n## Detected Patterns\n"
            for pattern_type, pattern_data in patterns.items():
                if pattern_data:
                    content += f"\n### {pattern_type.replace('_', ' ').title()}\n"
                    if isinstance(pattern_data, list):
                        for item in pattern_data[:10]:
                            content += f"- {item}\n"
                    elif isinstance(pattern_data, dict):
                        for key, value in list(pattern_data.items())[:10]:
                            content += f"- **{key}**: {value}\n"
        
        content += """
## Usage Guide

### For Development
1. Use files in `_originals/` as untouched references
2. Use files in `_templates/` for pattern understanding
3. Use files in `_samples/` for variation examples

### For Analysis
- Review `GO_BY_METADATA.json` for complete statistics
- Check `VARIATION_MAPPING.yml` for parameter patterns
- Use this folder to understand structure without processing large files

### Restoration
To restore full content:
1. Obtain original files from source location
2. Use metadata to verify completeness
3. Replace stub files with actual binaries
"""
        
        return content