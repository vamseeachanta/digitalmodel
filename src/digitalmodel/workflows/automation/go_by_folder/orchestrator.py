"""
Main orchestrator for Create Go-By Folder Tool
"""

import shutil
import json
from pathlib import Path
from typing import Dict, Optional, Set
from datetime import datetime
import logging

from .scanner import FileScanner
from .analyzer import PatternAnalyzer
from .preservator import FilePreservator
from .metadata.generator import MetadataGenerator
from .metadata.json_metadata import JSONMetadataGenerator
from .metadata.markdown_docs import MarkdownDocsGenerator
from .metadata.analysis import AnalysisMetadataGenerator
from .progress import ProgressReporter, MemoryMonitor
from .interaction import UserInteraction, InteractionMode
from .exceptions import GoByError, ValidationError
from .learning import GoByLearningSystem

logger = logging.getLogger(__name__)


class CreateGoBy:
    """Main orchestrator for creating go-by folders."""
    
    def __init__(
        self,
        source: Optional[Path],
        target: Path,
        config: Dict
    ):
        """
        Initialize CreateGoBy orchestrator.
        
        Args:
            source: Source folder path (optional if resuming)
            target: Target folder path
            config: Configuration dictionary
        """
        self.source = Path(source) if source else None
        self.target = Path(target)
        self.config = config
        
        # Components (will be initialized as needed)
        self.scanner = None
        self.analyzer = None
        self.preservator = None
        self.metadata_gen = None
        self.progress = None
        self.interaction = None
        self.memory_monitor = None
        self.learning = None
        
        # State
        self.files_processed = 0
        self.start_time = None
        self.scan_results = {}
        self.patterns = {}
        self.preservation_stats = {}
        
    def execute(self) -> bool:
        """
        Main execution flow.
        
        Returns:
            True if successful, False otherwise
        """
        try:
            self.start_time = datetime.now()
            
            # Initialize components
            self._initialize_components()
            
            # Handle resume
            if self.config.get('resume'):
                logger.info("Resuming from checkpoint...")
                if not self.load_checkpoint():
                    logger.error("Failed to load checkpoint")
                    return False
            else:
                # Pre-validation
                if not self.validate_preconditions():
                    return False
                
                # Get initial statistics for preview
                initial_stats = self._get_initial_statistics()
                
                # Show preview and get confirmation
                if self.interaction and not self.interaction.show_preview(initial_stats):
                    logger.info("Operation cancelled by user")
                    return False
                
                # Check for large folder warning
                if initial_stats['total_size'] > 10 * 1024**3:  # 10GB
                    if self.interaction and not self.interaction.confirm_large_folder(
                        initial_stats['total_size'], 
                        initial_stats['total_files']
                    ):
                        logger.info("Operation cancelled due to large folder size")
                        return False
                
                # Prepare target directory
                if not self.prepare_target_directory():
                    return False
            
            # Main processing flow
            if self.source:
                # Phase 1: Scan source
                logger.info(f"Scanning {self.source}...")
                if self.progress:
                    self.progress.update(current_stage="Scanning files")
                
                files = self.scan_source()
                
                if not files:
                    logger.warning("No files found to process")
                    return True
                
                logger.info(f"Found {len(files)} files to process")
                
                # Start progress tracking
                if self.progress:
                    total_size = sum(f.get('size', 0) for f in files)
                    self.progress.start(len(files), total_size)
                
                # Phase 2: Analyze patterns
                logger.info("Analyzing patterns...")
                if self.progress:
                    self.progress.update(current_stage="Analyzing patterns")
                
                self.patterns = self.analyze_patterns(files)
                
                # Learn from patterns
                if self.learning:
                    self.learning.learn_from_patterns(self.patterns)
                    file_types = self.get_file_types(files)
                    self.learning.learn_from_file_types(file_types)
                
                # Phase 3: Create structure
                logger.info("Creating folder structure...")
                if self.progress:
                    self.progress.update(current_stage="Creating structure")
                
                self.create_basic_structure(files)
                
                # Phase 4: Process files
                logger.info("Processing files...")
                if self.progress:
                    self.progress.update(current_stage="Processing files")
                
                self.preservation_stats = self.process_files(files)
                
                # Phase 5: Generate metadata
                logger.info("Generating metadata...")
                if self.progress:
                    self.progress.update(current_stage="Generating metadata")
                
                self.generate_all_metadata(files)
            
            # Complete progress tracking
            if self.progress:
                self.progress.complete(success=True)
            
            # Stop memory monitoring
            if self.memory_monitor:
                self.memory_monitor.stop()
            
            # Report completion
            elapsed = datetime.now() - self.start_time
            completion_stats = {
                'target_path': str(self.target),
                'original_size': self.scan_results.get('total_size', 0),
                'final_size': self.preservation_stats.get('final_size', 0),
                'processing_time': elapsed.total_seconds(),
                'errors': self.preservation_stats.get('errors_count', 0)
            }
            
            if self.interaction:
                self.interaction.report_completion(True, completion_stats)
            
            # Complete learning session
            if self.learning:
                self.learning.complete_session(
                    success=True,
                    stats={
                        'total_processed': self.preservation_stats.get('total_processed', 0),
                        'size_reduction': completion_stats.get('size_reduction', 0),
                        'processing_time': elapsed.total_seconds(),
                        'patterns_found': len(self.patterns),
                        'file_types': len(self.get_file_types(files))
                    }
                )
                
                # Suggest naming for future reference
                if self.patterns:
                    suggested_name = self.learning.get_naming_suggestion(
                        self.patterns,
                        self.get_file_types(files)
                    )
                    logger.info(f"Suggested brief name for similar folders: {suggested_name}")
            
            logger.info(
                f"✅ Go-by folder created successfully at {self.target} "
                f"in {elapsed.total_seconds():.1f} seconds"
            )
            
            return True
            
        except KeyboardInterrupt:
            logger.info("Operation cancelled by user")
            if self.progress:
                self.progress.complete(success=False)
            self.save_checkpoint()
            return False
            
        except Exception as e:
            logger.error(f"Error during execution: {e}", exc_info=True)
            
            # Learn from error
            if self.learning:
                self.learning.learn_from_errors(
                    str(e),
                    {
                        'phase': getattr(self, 'current_phase', 'unknown'),
                        'files_processed': self.files_processed,
                        'source': str(self.source),
                        'target': str(self.target)
                    }
                )
                self.learning.complete_session(
                    success=False,
                    stats={'error': str(e)}
                )
            
            if self.progress:
                self.progress.error(str(e))
                self.progress.complete(success=False)
            return False
    
    def validate_preconditions(self) -> bool:
        """
        Validate preconditions before execution.
        
        Returns:
            True if validation passes
        """
        if not self.source:
            logger.error("Source folder is required")
            return False
        
        if not self.source.exists():
            logger.error(f"Source folder does not exist: {self.source}")
            return False
        
        if not self.source.is_dir():
            logger.error(f"Source is not a directory: {self.source}")
            return False
        
        # Check target
        if self.target.exists() and not self.config.get('overwrite'):
            logger.error(
                f"Target folder already exists: {self.target}. "
                "Use --overwrite to replace"
            )
            return False
        
        return True
    
    def _initialize_components(self) -> None:
        """Initialize all components."""
        # Initialize interaction handler
        if self.config.get('yes_to_all'):
            mode = InteractionMode.AUTO_YES
        elif self.config.get('no_interaction'):
            mode = InteractionMode.SILENT
        else:
            mode = InteractionMode.INTERACTIVE
        
        self.interaction = UserInteraction(mode=mode)
        
        # Initialize progress reporter
        self.progress = ProgressReporter(
            verbose=self.config.get('verbose', True),
            show_bar=not self.config.get('no_progress_bar', False)
        )
        
        # Initialize memory monitor
        if self.config.get('monitor_memory', True):
            self.memory_monitor = MemoryMonitor()
            self.memory_monitor.start()
        
        # Initialize learning system
        self.learning = GoByLearningSystem()
        
        # Get optimization suggestions based on learning
        if self.source and self.source.exists():
            suggestions = self.learning.suggest_optimizations(self.source)
            if suggestions and self.config.get('verbose'):
                logger.info("Learning system suggestions:")
                for suggestion in suggestions:
                    logger.info(f"  - {suggestion}")
    
    def _get_initial_statistics(self) -> Dict:
        """Get initial statistics for preview."""
        if not self.source:
            return {}
        
        # Quick scan for statistics
        total_files = 0
        total_size = 0
        file_types = {}
        
        for item in self.source.rglob('*'):
            if item.is_file():
                total_files += 1
                total_size += item.stat().st_size
                ext = item.suffix.lower()
                file_types[ext] = file_types.get(ext, 0) + 1
        
        return {
            'source_path': str(self.source),
            'target_path': str(self.target),
            'total_files': total_files,
            'total_size': total_size,
            'file_types': file_types
        }
    
    def prepare_target_directory(self) -> bool:
        """
        Prepare target directory.
        
        Returns:
            True if successful
        """
        try:
            if self.target.exists():
                if self.config.get('overwrite'):
                    # Confirm overwrite with user interaction
                    if self.interaction and not self.interaction.confirm_overwrite(self.target):
                        logger.info("Overwrite cancelled by user")
                        return False
                    logger.warning(f"Removing existing target: {self.target}")
                    shutil.rmtree(self.target)
                else:
                    return False
            
            # Create target directory
            self.target.mkdir(parents=True, exist_ok=True)
            logger.info(f"Created target directory: {self.target}")
            
            # Create subdirectories
            (self.target / '_originals').mkdir(exist_ok=True)
            (self.target / '_templates').mkdir(exist_ok=True)
            (self.target / '_samples').mkdir(exist_ok=True)
            
            return True
            
        except Exception as e:
            logger.error(f"Failed to prepare target directory: {e}")
            return False
    
    def scan_source(self) -> list:
        """
        Scan source directory.
        
        Returns:
            List of file information dictionaries
        """
        self.scanner = FileScanner(
            self.source,
            exclude_patterns=self.config.get('exclude_patterns'),
            include_patterns=self.config.get('include_patterns')
        )
        
        files = []
        for file_info in self.scanner.scan():
            files.append(file_info)
            
            # Update progress
            if self.progress and len(files) % 100 == 0:
                self.progress.update(
                    processed_files=len(files),
                    current_file=str(file_info.get('path', ''))
                )
            
            # Save checkpoint periodically
            if len(files) % 1000 == 0:
                self.save_checkpoint()
        
        # Store scan results
        self.scan_results = {
            'total_files': len(files),
            'total_size': sum(f.get('size', 0) for f in files),
            'file_types': self._count_file_types(files),
            'total_dirs': len(set(f.get('parent', '') for f in files))
        }
        
        return files
    
    def _count_file_types(self, files: list) -> Dict:
        """Count file types from file list."""
        file_types = {}
        for file_info in files:
            ext = file_info.get('extension', '')
            if ext not in file_types:
                file_types[ext] = {'count': 0, 'total_size': 0}
            file_types[ext]['count'] += 1
            file_types[ext]['total_size'] += file_info.get('size', 0)
        return file_types
    
    def analyze_patterns(self, files: list) -> Dict:
        """Analyze patterns in files."""
        if not self.analyzer:
            self.analyzer = PatternAnalyzer()
        
        # Use the detect_patterns method which analyzes all files
        patterns = self.analyzer.detect_patterns(files)
        
        logger.info(f"Detected {len(patterns)} pattern types")
        return patterns
    
    def process_files(self, files: list) -> Dict:
        """Process files for preservation."""
        if not self.preservator:
            self.preservator = FilePreservator('chronological')
        
        preservation_stats = {
            'total_processed': 0,
            'originals_count': 0,
            'minimized_count': 0,
            'stubs_count': 0,
            'errors_count': 0,
            'final_size': 0,
            'originals_list': [],
            'minimized_list': [],
            'stubs_list': [],
            'errors': []
        }
        
        # Select originals by type (one per extension)
        originals = self.preservator.select_by_type(files)
        
        # Process each file
        for idx, file_info in enumerate(files):
            try:
                # Update progress
                if self.progress:
                    self.progress.update(
                        increment=True,
                        current_file=str(file_info.get('path', '')),
                        current_stage="Processing files"
                    )
                
                # Check if this is an original to preserve
                file_path = file_info.get('path')
                is_original = file_path in originals if file_path else False
                
                source_file = Path(file_info['path'])
                if source_file.exists():
                    if self.config.get('preserve_structure', True):
                        # Place file in exact same location preserving structure
                        target_file = self.target / file_info['relative_path']
                    else:
                        # Use original _originals/_samples approach
                        if is_original:
                            target_file = self.target / '_originals' / file_info['relative_path']
                        else:
                            target_file = self.target / '_samples' / file_info['relative_path']
                    
                    target_file.parent.mkdir(parents=True, exist_ok=True)
                    
                    if is_original:
                        # Copy original files unchanged
                        shutil.copy2(source_file, target_file)
                        preservation_stats['originals_count'] += 1
                        preservation_stats['originals_list'].append(str(file_info['path']))
                    else:
                        # Minimize non-original files
                        if file_info.get('extension') in ['.yml', '.yaml', '.txt', '.md']:
                            try:
                                with open(source_file, 'r', encoding='utf-8') as f:
                                    lines = f.readlines()[:10]
                                with open(target_file, 'w', encoding='utf-8') as f:
                                    f.writelines(lines)
                                    if len(lines) == 10:
                                        f.write("\n... [truncated] ...\n")
                            except:
                                # If text processing fails, just copy the file
                                shutil.copy2(source_file, target_file)
                        else:
                            # For binary files, create a stub
                            target_file.write_text(f"[Binary file stub]\nOriginal size: {file_info.get('size', 0)} bytes\n")
                        preservation_stats['minimized_count'] += 1
                        preservation_stats['minimized_list'].append(str(file_info['path']))
                
                preservation_stats['total_processed'] += 1
                
            except Exception as e:
                logger.warning(f"Error processing {file_info['path']}: {e}")
                preservation_stats['errors_count'] += 1
                preservation_stats['errors'].append(str(e))
                if self.progress:
                    self.progress.error(str(e), str(file_info['path']))
        
        # Calculate final size (simplified for now)
        preservation_stats['final_size'] = sum(
            f.get('size', 0) for f in files[:10]  # Estimate based on preserved files
        ) + 1024 * 1024  # Add 1MB for metadata
        
        return preservation_stats
    
    def get_file_types(self, files: list) -> Dict:
        """
        Get file types summary from files list.
        
        Args:
            files: List of file information
            
        Returns:
            Dictionary mapping extensions to counts
        """
        file_types = {}
        for file_info in files:
            ext = file_info.get('extension', '')
            if ext not in file_types:
                file_types[ext] = {'count': 0, 'total_size': 0}
            file_types[ext]['count'] += 1
            file_types[ext]['total_size'] += file_info.get('size', 0)
        return file_types
    
    def generate_all_metadata(self, files: list) -> None:
        """Generate all metadata files."""
        # Initialize metadata generator
        self.metadata_gen = MetadataGenerator(self.source, self.target)
        
        # Add specialized generators
        self.metadata_gen.add_generator(
            JSONMetadataGenerator(self.source, self.target)
        )
        self.metadata_gen.add_generator(
            MarkdownDocsGenerator(self.source, self.target)
        )
        self.metadata_gen.add_generator(
            AnalysisMetadataGenerator(self.source, self.target)
        )
        
        # Generate all metadata
        self.metadata_gen.generate_all_metadata(
            self.scan_results,
            self.patterns,
            self.preservation_stats
        )
        
        logger.info("Generated all metadata files")
    
    def create_basic_structure(self, files: list) -> None:
        """
        Create basic directory structure in target.
        
        Args:
            files: List of file information
        """
        if self.config.get('preserve_structure', True):
            # Preserve exact directory structure from source
            directories = set()
            
            # Scan source for ALL directories (including empty ones)
            for item in self.source.rglob('*'):
                if item.is_dir():
                    relative_path = item.relative_to(self.source)
                    if relative_path != Path('.'):
                        directories.add(relative_path)
            
            # Also add directories from files
            for file_info in files:
                parent = file_info.get('parent')
                if parent and parent != Path('.'):
                    directories.add(Path(parent))
                    # Add all parent directories
                    for parent_dir in Path(parent).parents:
                        if parent_dir != Path('.'):
                            directories.add(parent_dir)
            
            # Create all directories
            for directory in sorted(directories):
                target_dir = self.target / directory
                target_dir.mkdir(parents=True, exist_ok=True)
            
            logger.info(f"Created {len(directories)} directories preserving exact structure")
        else:
            # Original approach - collect from files only
            directories = set()
            for file_info in files:
                parent = file_info.get('parent')
                if parent and parent != Path('.'):
                    for parent_dir in Path(parent).parents:
                        if parent_dir != Path('.'):
                            directories.add(parent_dir)
                    directories.add(Path(parent))
            
            # Create directories
            for directory in sorted(directories):
                target_dir = self.target / directory
                target_dir.mkdir(parents=True, exist_ok=True)
            
            # Create standard go-by directories
            required_dirs = ['_originals', '_templates', '_samples']
            for dir_name in required_dirs:
                (self.target / dir_name).mkdir(exist_ok=True)
            
            logger.info(f"Created {len(directories)} + 3 standard directories")
    
    def create_basic_metadata_old(self, files: list) -> None:
        """
        Create basic metadata files.
        
        Args:
            files: List of file information
        """
        # Create GO_BY_METADATA.json
        metadata = {
            'source_path': str(self.source),
            'creation_date': datetime.now().isoformat(),
            'total_files': len(files),
            'total_size': sum(f['size'] for f in files),
            'file_types': {},
            'configuration': self.config
        }
        
        # Count file types
        for file_info in files:
            ext = file_info.get('extension', '')
            if ext not in metadata['file_types']:
                metadata['file_types'][ext] = 0
            metadata['file_types'][ext] += 1
        
        metadata_path = self.target / 'GO_BY_METADATA.json'
        with open(metadata_path, 'w') as f:
            json.dump(metadata, f, indent=2, default=str)
        
        # Create basic AGENT_OVERVIEW.md
        overview = f"""# 🤖 Agent Quick Start Guide

## Purpose
This go-by folder is a lightweight representation of: {self.source}

## Source Information
- **Original Folder**: {self.source}
- **Creation Date**: {datetime.now().isoformat()}
- **Total Files**: {len(files)}
- **Total Size**: {metadata['total_size'] / (1024*1024):.2f}MB

## Quick Navigation
- Original preserved files: `./_originals/`
- Template patterns: `./_templates/`
- Sample files: `./_samples/`

## File Types Present
| Extension | Count |
|-----------|-------|
"""
        
        for ext, count in sorted(metadata['file_types'].items()):
            overview += f"| {ext or 'no extension'} | {count} |\n"
        
        overview_path = self.target / 'AGENT_OVERVIEW.md'
        overview_path.write_text(overview, encoding='utf-8')
        
        logger.info("Created metadata files")
    
    def save_checkpoint(self) -> None:
        """Save checkpoint for resume capability."""
        if not self.target.exists():
            return
        
        checkpoint_dir = self.target / '.go_by_checkpoint'
        checkpoint_dir.mkdir(exist_ok=True)
        
        checkpoint = {
            'version': '0.1.0',
            'timestamp': datetime.now().isoformat(),
            'progress': {
                'files_processed': self.files_processed
            },
            'config': self.config
        }
        
        checkpoint_path = checkpoint_dir / 'checkpoint.json'
        with open(checkpoint_path, 'w') as f:
            json.dump(checkpoint, f, indent=2, default=str)
    
    def load_checkpoint(self) -> bool:
        """
        Load checkpoint for resume.
        
        Returns:
            True if checkpoint loaded successfully
        """
        checkpoint_path = self.target / '.go_by_checkpoint' / 'checkpoint.json'
        
        if not checkpoint_path.exists():
            logger.error("No checkpoint found")
            return False
        
        try:
            with open(checkpoint_path, 'r') as f:
                checkpoint = json.load(f)
            
            self.files_processed = checkpoint['progress']['files_processed']
            logger.info(f"Resumed from checkpoint: {self.files_processed} files processed")
            
            return True
            
        except Exception as e:
            logger.error(f"Failed to load checkpoint: {e}")
            return False