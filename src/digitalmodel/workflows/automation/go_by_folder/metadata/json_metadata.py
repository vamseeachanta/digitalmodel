"""
JSON metadata generator for Create Go-By Folder Tool
"""

from pathlib import Path
from typing import Dict, List, Optional, Any
from datetime import datetime
import json
import hashlib
import logging

from .generator import BaseMetadataGenerator

logger = logging.getLogger(__name__)


class JSONMetadataGenerator(BaseMetadataGenerator):
    """Generate detailed JSON metadata for go-by folders."""
    
    def generate_metadata(self, scan_results: Dict, patterns: Dict, preservation_stats: Dict) -> Dict:
        """
        Generate comprehensive JSON metadata.
        
        Args:
            scan_results: Scanner results
            patterns: Detected patterns
            preservation_stats: Preservation statistics
            
        Returns:
            Dictionary of metadata files to create
        """
        # Main metadata file
        main_metadata = self._create_main_metadata(scan_results, patterns, preservation_stats)
        self.save_metadata(main_metadata, 'GO_BY_METADATA.json')
        
        # File inventory
        file_inventory = self._create_file_inventory(scan_results)
        self.save_metadata(file_inventory, 'metadata/FILE_INVENTORY.json')
        
        # Pattern analysis
        pattern_analysis = self._create_pattern_analysis(patterns)
        self.save_metadata(pattern_analysis, 'metadata/PATTERN_ANALYSIS.json')
        
        # Preservation log
        preservation_log = self._create_preservation_log(preservation_stats)
        self.save_metadata(preservation_log, 'metadata/PRESERVATION_LOG.json')
        
        # Size analysis
        size_analysis = self._create_size_analysis(scan_results, preservation_stats)
        self.save_metadata(size_analysis, 'metadata/SIZE_ANALYSIS.json')
        
        return {
            'json_metadata_generated': True,
            'files_created': 5
        }
    
    def _create_main_metadata(self, scan_results: Dict, patterns: Dict, preservation_stats: Dict) -> Dict:
        """Create main GO_BY_METADATA.json content."""
        return {
            'version': '1.0.0',
            'tool': {
                'name': 'digitalmodel.workflows.automation.go_by_folder',
                'version': '1.0.0',
                'repository': 'digitalmodel'
            },
            'source': {
                'path': str(self.source_path),
                'absolute_path': str(self.source_path.resolve()),
                'exists': self.source_path.exists(),
                'is_directory': self.source_path.is_dir()
            },
            'target': {
                'path': str(self.target_path),
                'absolute_path': str(self.target_path.resolve())
            },
            'creation': {
                'timestamp': self.creation_time.isoformat(),
                'timestamp_unix': self.creation_time.timestamp(),
                'timezone': str(self.creation_time.tzinfo) if self.creation_time.tzinfo else 'local'
            },
            'statistics': {
                'files': {
                    'total': scan_results.get('total_files', 0),
                    'preserved_original': preservation_stats.get('originals_count', 0),
                    'minimized': preservation_stats.get('minimized_count', 0),
                    'stub_replaced': preservation_stats.get('stubs_count', 0)
                },
                'size': {
                    'original_bytes': scan_results.get('total_size', 0),
                    'original_formatted': self.format_size(scan_results.get('total_size', 0)),
                    'final_bytes': preservation_stats.get('final_size', 0),
                    'final_formatted': self.format_size(preservation_stats.get('final_size', 0)),
                    'reduction': self.get_size_reduction(
                        scan_results.get('total_size', 0),
                        preservation_stats.get('final_size', 0)
                    )
                },
                'directories': scan_results.get('total_dirs', 0),
                'file_types': len(scan_results.get('file_types', {})),
                'patterns_detected': len(patterns)
            },
            'sampling_rules': {
                'large_files': {
                    'strategy': 'truncated',
                    'max_size': '10KB',
                    'preserve_lines': {'first': 10, 'last': 10}
                },
                'binary_files': {
                    'strategy': 'stub_replacement',
                    'stub_format': 'text_description',
                    'metadata_preserved': True
                },
                'sequences': {
                    'strategy': 'representative_sampling',
                    'samples': ['first', 'middle', 'last'],
                    'max_variations': 3
                },
                'originals': {
                    'strategy': 'one_per_type',
                    'selection': 'chronological_oldest',
                    'preserved_unchanged': True
                }
            },
            'patterns': patterns,
            'folder_hash': self.calculate_folder_hash(self.source_path),
            'config': self.config
        }
    
    def _create_file_inventory(self, scan_results: Dict) -> Dict:
        """Create detailed file inventory."""
        inventory = {
            'timestamp': datetime.now().isoformat(),
            'total_files': scan_results.get('total_files', 0),
            'total_directories': scan_results.get('total_dirs', 0),
            'file_types': {},
            'largest_files': [],
            'oldest_files': [],
            'newest_files': []
        }
        
        # Process file types
        for ext, info in scan_results.get('file_types', {}).items():
            if isinstance(info, dict):
                inventory['file_types'][ext] = {
                    'count': info.get('count', 0),
                    'total_size': info.get('total_size', 0),
                    'average_size': info.get('average_size', 0),
                    'category': self.get_file_category(ext)
                }
            else:
                inventory['file_types'][ext] = {
                    'count': info,
                    'category': self.get_file_category(ext)
                }
        
        # Add file lists if available
        if 'largest_files' in scan_results:
            inventory['largest_files'] = [
                {
                    'path': str(f['path']),
                    'size': f['size'],
                    'size_formatted': self.format_size(f['size'])
                }
                for f in scan_results['largest_files'][:10]
            ]
        
        return inventory
    
    def _create_pattern_analysis(self, patterns: Dict) -> Dict:
        """Create pattern analysis metadata."""
        analysis = {
            'timestamp': datetime.now().isoformat(),
            'patterns_found': len(patterns),
            'pattern_types': list(patterns.keys()),
            'details': {}
        }
        
        for pattern_type, pattern_data in patterns.items():
            if isinstance(pattern_data, list):
                analysis['details'][pattern_type] = {
                    'count': len(pattern_data),
                    'samples': pattern_data[:10],
                    'type': 'list'
                }
            elif isinstance(pattern_data, dict):
                analysis['details'][pattern_type] = {
                    'count': len(pattern_data),
                    'samples': dict(list(pattern_data.items())[:10]),
                    'type': 'dictionary'
                }
            else:
                analysis['details'][pattern_type] = {
                    'value': str(pattern_data),
                    'type': type(pattern_data).__name__
                }
        
        return analysis
    
    def _create_preservation_log(self, preservation_stats: Dict) -> Dict:
        """Create preservation log."""
        return {
            'timestamp': datetime.now().isoformat(),
            'summary': {
                'files_processed': preservation_stats.get('total_processed', 0),
                'originals_preserved': preservation_stats.get('originals_count', 0),
                'files_minimized': preservation_stats.get('minimized_count', 0),
                'stubs_created': preservation_stats.get('stubs_count', 0),
                'errors_encountered': preservation_stats.get('errors_count', 0)
            },
            'originals': preservation_stats.get('originals_list', []),
            'minimized': preservation_stats.get('minimized_list', []),
            'stubs': preservation_stats.get('stubs_list', []),
            'errors': preservation_stats.get('errors', []),
            'strategies_used': {
                'text_files': 'first_last_lines',
                'code_files': 'structure_preservation',
                'config_files': 'key_structure',
                'binary_files': 'stub_replacement',
                'data_files': 'sample_rows'
            }
        }
    
    def _create_size_analysis(self, scan_results: Dict, preservation_stats: Dict) -> Dict:
        """Create detailed size analysis."""
        original_size = scan_results.get('total_size', 0)
        final_size = preservation_stats.get('final_size', 0)
        
        return {
            'timestamp': datetime.now().isoformat(),
            'original': {
                'total_bytes': original_size,
                'formatted': self.format_size(original_size),
                'file_count': scan_results.get('total_files', 0)
            },
            'final': {
                'total_bytes': final_size,
                'formatted': self.format_size(final_size),
                'file_count': preservation_stats.get('final_file_count', 0)
            },
            'reduction': self.get_size_reduction(original_size, final_size),
            'by_category': self._analyze_size_by_category(scan_results, preservation_stats),
            'effectiveness': self._calculate_effectiveness(original_size, final_size)
        }
    
    def _analyze_size_by_category(self, scan_results: Dict, preservation_stats: Dict) -> Dict:
        """Analyze size reduction by file category."""
        categories = {}
        
        for ext, info in scan_results.get('file_types', {}).items():
            category = self.get_file_category(ext)
            if category not in categories:
                categories[category] = {
                    'original_size': 0,
                    'final_size': 0,
                    'file_count': 0
                }
            
            if isinstance(info, dict):
                categories[category]['original_size'] += info.get('total_size', 0)
                categories[category]['file_count'] += info.get('count', 0)
        
        # Calculate reductions
        for category in categories:
            original = categories[category]['original_size']
            # Estimate final size based on strategy
            if category == 'binary':
                final = categories[category]['file_count'] * 1024  # ~1KB stubs
            elif category == 'code':
                final = original * 0.1  # ~10% for structure
            elif category == 'config':
                final = original * 0.2  # ~20% for config
            else:
                final = original * 0.05  # ~5% for others
            
            categories[category]['final_size'] = int(final)
            categories[category]['reduction'] = self.get_size_reduction(original, final)
        
        return categories
    
    def _calculate_effectiveness(self, original_size: int, final_size: int) -> Dict:
        """Calculate effectiveness metrics."""
        if original_size == 0:
            return {'rating': 'N/A', 'score': 0}
        
        reduction_percentage = ((original_size - final_size) / original_size) * 100
        
        if reduction_percentage >= 99:
            rating = 'Excellent'
            score = 10
        elif reduction_percentage >= 95:
            rating = 'Very Good'
            score = 8
        elif reduction_percentage >= 90:
            rating = 'Good'
            score = 6
        elif reduction_percentage >= 80:
            rating = 'Fair'
            score = 4
        else:
            rating = 'Poor'
            score = 2
        
        return {
            'rating': rating,
            'score': score,
            'percentage': round(reduction_percentage, 2),
            'target_met': reduction_percentage >= 99
        }