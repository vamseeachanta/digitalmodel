# Create Go-By Folder Tool - Implementation Specification

## Executive Summary
A Python-based tool that creates compact, representative "go-by" versions of existing folders. The tool preserves directory structure, maintains one original file per type/pattern unchanged, replaces redundant files with minimized representatives, and generates comprehensive metadata for both human and AI agent consumption.

## Problem Statement
Large project folders (multi-GB) are difficult to share, template, or use as references. Teams need a way to create lightweight representations that preserve structure, patterns, and key examples while drastically reducing size. This is especially critical for:
- Analysis/simulation projects with thousands of parameter variations
- Creating shareable project templates
- Documenting folder structures for AI agents
- Archiving project organization patterns

## Proposed Solution

### Core Functionality
Create a tool that:
1. Scans source folders up to 10GB
2. Preserves complete directory structure
3. Keeps one original file per type/pattern unchanged
4. Creates lightweight representatives for variations
5. Generates comprehensive metadata
6. Provides AI-agent friendly documentation

### Technical Architecture

```python
# Main module structure
create_go_by/
├── __init__.py
├── cli.py                 # Command-line interface
├── scanner.py             # File system scanning
├── analyzer.py            # Pattern detection and analysis
├── preservator.py         # Original file selection
├── minimizer.py           # File size reduction strategies
├── metadata_generator.py  # Metadata creation
├── progress_reporter.py   # Real-time progress tracking
├── error_handler.py       # Error recovery and logging
├── validators.py          # Pre/post validation
└── utils/
    ├── file_utils.py      # File operations
    ├── hash_utils.py      # Content hashing
    └── pattern_utils.py   # Pattern matching
```

## Detailed Design

### 1. Command-Line Interface

```python
import argparse
from pathlib import Path

class CreateGoByCLI:
    def __init__(self):
        self.parser = self.create_parser()
    
    def create_parser(self):
        parser = argparse.ArgumentParser(
            description='Create representative go-by folder from existing folder'
        )
        
        # Required arguments
        parser.add_argument('-s', '--source-folder', 
                          type=Path, required=True,
                          help='Path to existing folder')
        parser.add_argument('-t', '--target-folder',
                          type=Path, required=True,
                          help='Path for go-by folder')
        
        # Optional flags
        parser.add_argument('--overwrite', action='store_true',
                          help='Overwrite existing target folder')
        parser.add_argument('--max-file-size', type=str, default='10KB',
                          help='Max size for representative files')
        parser.add_argument('--variation-coverage', 
                          choices=['low', 'medium', 'high'],
                          default='medium',
                          help='How many variations to keep')
        parser.add_argument('--analysis-mode', action='store_true',
                          help='Enable analysis-specific features')
        parser.add_argument('--resume', action='store_true',
                          help='Resume from checkpoint')
        
        return parser
```

### 2. File Scanner Module

```python
import os
from pathlib import Path
from typing import List, Dict, Generator
import hashlib

class FileScanner:
    def __init__(self, source_path: Path, exclude_patterns: List[str] = None):
        self.source_path = source_path
        self.exclude_patterns = exclude_patterns or []
        self.file_registry = {}
        self.symlink_registry = {}
        
    def scan(self) -> Generator[Dict, None, None]:
        """Scan source folder and yield file information"""
        for root, dirs, files in os.walk(self.source_path, followlinks=False):
            # Check for symlinks
            for name in dirs + files:
                full_path = Path(root) / name
                if full_path.is_symlink():
                    self.register_symlink(full_path)
                    
            # Process files
            for file in files:
                file_path = Path(root) / file
                if not self.should_exclude(file_path):
                    yield self.analyze_file(file_path)
                    
    def analyze_file(self, file_path: Path) -> Dict:
        """Analyze single file and return metadata"""
        stat = file_path.stat()
        return {
            'path': file_path,
            'relative_path': file_path.relative_to(self.source_path),
            'size': stat.st_size,
            'modified': stat.st_mtime,
            'created': stat.st_ctime,
            'extension': file_path.suffix,
            'hash': self.get_file_hash(file_path) if stat.st_size < 1_000_000 else None
        }
```

### 3. Pattern Analyzer

```python
import re
from typing import List, Dict, Set
from collections import defaultdict

class PatternAnalyzer:
    def __init__(self):
        self.patterns = defaultdict(list)
        self.parameter_patterns = {}
        
    def detect_patterns(self, files: List[Dict]) -> Dict:
        """Detect naming patterns and variations"""
        # Group by directory and base pattern
        for file_info in files:
            pattern = self.extract_pattern(file_info['path'].name)
            self.patterns[pattern].append(file_info)
            
        # Detect parameter variations
        self.detect_parameter_patterns()
        
        return {
            'file_patterns': dict(self.patterns),
            'parameter_patterns': self.parameter_patterns,
            'variation_count': self.count_variations()
        }
        
    def extract_pattern(self, filename: str) -> str:
        """Extract base pattern from filename"""
        # Replace numbers with placeholders
        pattern = re.sub(r'\d+', '{N}', filename)
        # Replace common parameter patterns
        pattern = re.sub(r'_(low|medium|high)', '_{LEVEL}', pattern)
        pattern = re.sub(r'_v\d+', '_v{VERSION}', pattern)
        return pattern
        
    def detect_parameter_patterns(self):
        """Identify parameter sweep patterns"""
        for pattern, files in self.patterns.items():
            if len(files) > 5:  # Likely a parameter sweep
                self.analyze_parameter_variation(pattern, files)
```

### 4. File Preservation Strategy

```python
from typing import List, Dict, Set
from pathlib import Path

class FilePreservator:
    def __init__(self, selection_strategy: str = 'chronological'):
        self.selection_strategy = selection_strategy
        self.preserved_files = set()
        
    def select_originals(self, file_groups: Dict[str, List]) -> Set[Path]:
        """Select one original file per pattern to preserve unchanged"""
        for pattern, files in file_groups.items():
            if self.selection_strategy == 'chronological':
                # Sort by creation time, select oldest
                files.sort(key=lambda f: f['created'])
                original = files[0]
            elif self.selection_strategy == 'smallest':
                # Select smallest file
                files.sort(key=lambda f: f['size'])
                original = files[0]
            else:  # alphabetical
                files.sort(key=lambda f: f['path'])
                original = files[0]
                
            self.preserved_files.add(original['path'])
            
        return self.preserved_files
```

### 5. File Minimizer

```python
import io
from pathlib import Path
from typing import Optional

class FileMinimizer:
    def __init__(self, max_size: int = 10240):  # 10KB default
        self.max_size = max_size
        
    def minimize_file(self, file_path: Path, preserve: bool = False) -> Optional[bytes]:
        """Create minimized version of file"""
        if preserve:
            # Return None to indicate file should be copied as-is
            return None
            
        file_size = file_path.stat().st_size
        
        if file_path.suffix in ['.txt', '.csv', '.log', '.dat']:
            return self.minimize_text_file(file_path)
        elif file_path.suffix in ['.json', '.yml', '.yaml', '.xml']:
            return self.minimize_config_file(file_path)
        elif file_path.suffix in ['.py', '.js', '.java', '.cpp']:
            return self.minimize_code_file(file_path)
        else:
            return self.create_stub_file(file_path)
            
    def minimize_text_file(self, file_path: Path) -> bytes:
        """Keep first/last lines of text files"""
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()
            
        if len(lines) <= 20:
            return ''.join(lines).encode('utf-8')
            
        # Keep first 10 and last 10 lines
        minimized = lines[:10] + [f'\n... ({len(lines)-20} lines omitted) ...\n\n'] + lines[-10:]
        return ''.join(minimized).encode('utf-8')
```

### 6. Metadata Generator

```python
import json
import yaml
from pathlib import Path
from typing import Dict, Any
from datetime import datetime

class MetadataGenerator:
    def __init__(self, source_path: Path, target_path: Path):
        self.source_path = source_path
        self.target_path = target_path
        self.metadata = {}
        
    def generate_all_metadata(self, scan_results: Dict, patterns: Dict) -> None:
        """Generate all metadata files"""
        self.create_go_by_metadata(scan_results)
        self.create_agent_overview(scan_results, patterns)
        self.create_variation_mapping(patterns)
        self.create_originals_index()
        
    def create_agent_overview(self, scan_results: Dict, patterns: Dict) -> None:
        """Create AGENT_OVERVIEW.md for AI agents"""
        content = f"""# 🤖 Agent Quick Start Guide

## Purpose
This go-by folder is a lightweight representation of: {self.source_path}

## Source Information
- **Original Folder**: {self.source_path}
- **Creation Date**: {datetime.now().isoformat()}
- **Total Files**: {scan_results['total_files']}
- **Total Size**: Original: {self.format_size(scan_results['total_size'])} | Go-By: {self.format_size(scan_results['goby_size'])}
- **Compression Ratio**: {scan_results['compression_ratio']:.2f}%

## Quick Navigation
- Original preserved files: `./_originals/`
- Template patterns: `./_templates/`
- Sample files: `./_samples/`
- Main structure: See directory tree below

## File Types Present
{self.generate_file_type_table(scan_results)}

## Key Patterns Detected
{self.generate_pattern_summary(patterns)}

## How to Use This Go-By
1. Review this file first for orientation
2. Check `_originals/` for authentic file formats
3. Examine GO_BY_METADATA.json for detailed statistics
4. Use templates for creating new variations

## Important Notes
- Original files in `_originals/` are unmodified
- All other files are minimized representatives
- Symlinks documented in LINK_HANDLING.md (if present)
"""
        
        output_path = self.target_path / 'AGENT_OVERVIEW.md'
        output_path.write_text(content)
```

### 7. Progress Reporter

```python
import time
import sys
from typing import Optional

class ProgressReporter:
    def __init__(self, total_files: int, update_interval: float = 0.5):
        self.total_files = total_files
        self.processed_files = 0
        self.start_time = time.time()
        self.update_interval = update_interval
        self.last_update = 0
        
    def update(self, current_file: str, increment: int = 1):
        """Update progress with current file being processed"""
        self.processed_files += increment
        current_time = time.time()
        
        if current_time - self.last_update >= self.update_interval:
            self.display_progress(current_file)
            self.last_update = current_time
            
    def display_progress(self, current_file: str):
        """Display progress bar and statistics"""
        percentage = (self.processed_files / self.total_files) * 100
        bar_length = 40
        filled = int(bar_length * self.processed_files / self.total_files)
        bar = '█' * filled + '░' * (bar_length - filled)
        
        elapsed = time.time() - self.start_time
        rate = self.processed_files / elapsed if elapsed > 0 else 0
        eta = (self.total_files - self.processed_files) / rate if rate > 0 else 0
        
        sys.stdout.write(f'\rCreating go-by folder: [{bar}] {percentage:.1f}% '
                        f'({self.processed_files}/{self.total_files} files)\n')
        sys.stdout.write(f'Current: {current_file[:50]}...\n')
        sys.stdout.write(f'Rate: {rate:.0f} files/sec | ETA: {eta:.0f} seconds\n')
        sys.stdout.flush()
```

### 8. Main Orchestrator

```python
from pathlib import Path
from typing import Optional
import shutil

class CreateGoBy:
    def __init__(self, source: Path, target: Path, config: dict):
        self.source = source
        self.target = target
        self.config = config
        
        # Initialize components
        self.scanner = FileScanner(source)
        self.analyzer = PatternAnalyzer()
        self.preservator = FilePreservator()
        self.minimizer = FileMinimizer()
        self.metadata_gen = MetadataGenerator(source, target)
        self.progress = None
        
    def execute(self) -> bool:
        """Main execution flow"""
        try:
            # Pre-validation
            if not self.validate_preconditions():
                return False
                
            # Scan source
            print(f"Scanning {self.source}...")
            files = list(self.scanner.scan())
            self.progress = ProgressReporter(len(files))
            
            # Analyze patterns
            patterns = self.analyzer.detect_patterns(files)
            
            # Select originals to preserve
            originals = self.preservator.select_originals(patterns['file_patterns'])
            
            # Create target structure
            self.create_target_structure()
            
            # Process files
            for file_info in files:
                self.process_file(file_info, file_info['path'] in originals)
                self.progress.update(str(file_info['path']))
                
            # Generate metadata
            self.metadata_gen.generate_all_metadata(
                {'total_files': len(files), 'patterns': patterns},
                patterns
            )
            
            print(f"\n✅ Go-by folder created successfully at {self.target}")
            return True
            
        except Exception as e:
            print(f"\n❌ Error: {e}")
            return False
```

## Implementation Requirements

### Dependencies
```toml
[tool.poetry.dependencies]
python = "^3.10"
click = "^8.1.0"       # CLI framework
pyyaml = "^6.0"        # YAML support
tqdm = "^4.65.0"       # Progress bars
colorama = "^0.4.6"    # Colored output
pandas = "^2.0.0"      # Data file handling
```

### System Requirements
- Python 3.10+
- 2GB RAM minimum
- Disk space: source_size * 0.01 + 100MB
- OS: Windows, Linux, macOS

### Performance Targets
- Process 1GB source in <2 minutes
- Memory usage <500MB for 10GB source
- Support concurrent file processing
- Checkpoint saves every 1000 files

## Continuous Learning System

### Overview
The Go-By Learning System captures insights from each execution to improve future runs.

### Features
- **Pattern Recognition**: Learns common file patterns and naming conventions
- **Error Prevention**: Tracks errors and their solutions
- **Performance Optimization**: Suggests optimizations based on folder characteristics
- **Naming Suggestions**: Recommends brief descriptive names based on content
- **Success Tracking**: Monitors successful strategies and reuses them

### Learning Data Structure
```python
{
    'version': '1.0',
    'sessions': [],          # History of all runs
    'patterns': {},          # Common patterns found
    'file_type_strategies': {}, # Best practices per file type
    'common_errors': {},     # Error patterns and solutions
    'optimization_hints': {}, # Performance suggestions
    'naming_patterns': {}    # Popular naming conventions
}
```

### Usage
```bash
# View learning report
python -m digitalmodel.modules.automation.go_by_folder --show-learning-report

# Learning happens automatically during each run
python -m digitalmodel.modules.automation.go_by_folder -s source -t target

# Preserve exact folder structure (default behavior)
python -m digitalmodel.modules.automation.go_by_folder -s source -t target --preserve-structure
```

### Benefits
- Improved performance with each use
- Better error handling based on experience
- Consistent naming suggestions
- Optimized processing strategies

## Folder Structure Preservation

### Overview
The go-by folder maintains the exact directory structure of the source to preserve file interdependencies and path references.

### Features
- **Exact Structure**: All directories are recreated in the target, including empty ones
- **Path Preservation**: Files maintain their original relative paths
- **Reference Integrity**: Internal file references continue to work
- **Dependency Maintenance**: Configuration hierarchies remain intact

### Structure Modes

#### Preserve Structure Mode (Default: `--preserve-structure`)
```
target/
├── current_coeffs/
│   ├── 03_c_fst1_f.yml [minimized]
│   └── 10_c_lngc_125km3_pb_f_fsts_l.yml [minimized]
├── moorings/
│   ├── 07_c_lines_lngc_ec125km3_pb_line01.yml [minimized]
│   └── 07_d_lines_lngc_common.yml [minimized]
├── README.md [original]
├── 01_a_static_analysis_fsts.yml [original]
├── _default.wrk [original]
└── [all other files minimized in place]
```

#### Legacy Mode (`--no-preserve-structure`)
```
target/
├── _originals/           # One original per type
├── _samples/             # Minimized samples
├── _templates/           # Pattern templates
└── metadata files
```

### Benefits of Structure Preservation
- **Runnable Configuration**: Files can execute with proper references
- **No Rewiring Required**: Path dependencies remain intact
- **Familiar Navigation**: Users can navigate as they would the original
- **Tool Compatibility**: External tools can process the structure

### Implementation Notes
- One original file per extension is preserved unchanged
- All other files are minimized but keep their exact locations
- Empty directories are created to maintain structure completeness
- Symbolic links are documented but structure is preserved

## Testing Strategy

### Unit Tests
- Scanner: file discovery, symlink detection
- Analyzer: pattern recognition, parameter detection
- Preservator: selection strategies
- Minimizer: file reduction techniques
- Metadata: file generation and validation

### Integration Tests
- End-to-end folder processing
- Recovery from interruption
- Overwrite scenarios
- Large folder handling

### Performance Tests
- 10GB folder processing
- Memory usage monitoring
- Progress reporting accuracy

## Security Considerations
- No automatic processing of sensitive data
- User prompted for compressed archives
- Symlink handling with user confirmation
- File permissions preserved where possible

## Success Metrics
- Go-by folder <1% of original size
- All file patterns represented
- Original files preserved unchanged
- Metadata validates successfully
- Agent can understand structure in <5 minutes

## Future Enhancements
1. Cloud storage integration
2. Incremental updates
3. GUI interface
4. Batch processing multiple folders
5. Custom minimization strategies
6. Integration with version control