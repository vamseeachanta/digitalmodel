"""
OrcaFlex File Size Profiler
Task 1.3: Profile File Size Distribution
Analyzes .sim and .dat files to understand size distribution for optimization.
"""

import os
import json
import argparse
import logging
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime
import matplotlib.pyplot as plt
import numpy as np

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class FileSizeProfiler:
    """Profile OrcaFlex file sizes for optimization analysis."""
    
    def __init__(self):
        """Initialize the file size profiler."""
        self.profiles = {
            'lngc': {'files': [], 'sizes': [], 'stats': {}},
            'standard': {'files': [], 'sizes': [], 'stats': {}},
            'all': {'files': [], 'sizes': [], 'stats': {}}
        }
        
    def scan_directory(self, directory: str, pattern: str = "*.sim", 
                       category: str = "all") -> Dict[str, Any]:
        """
        Scan directory for files and collect size information.
        
        Args:
            directory: Directory to scan
            pattern: File pattern to match
            category: Category for these files (lngc, standard, all)
            
        Returns:
            Dictionary with scan results
        """
        dir_path = Path(directory)
        if not dir_path.exists():
            logger.error(f"Directory not found: {directory}")
            return {'error': 'Directory not found'}
        
        files = list(dir_path.glob(pattern))
        logger.info(f"Found {len(files)} files matching '{pattern}' in {directory}")
        
        file_data = []
        for file_path in files:
            try:
                size = file_path.stat().st_size
                file_data.append({
                    'path': str(file_path),
                    'name': file_path.name,
                    'size_bytes': size,
                    'size_mb': size / 1e6,
                    'size_gb': size / 1e9
                })
                
                # Add to appropriate category
                self.profiles[category]['files'].append(str(file_path))
                self.profiles[category]['sizes'].append(size)
                
                # Also add to 'all' category
                if category != 'all':
                    self.profiles['all']['files'].append(str(file_path))
                    self.profiles['all']['sizes'].append(size)
                    
            except Exception as e:
                logger.error(f"Error scanning {file_path}: {e}")
        
        # Calculate statistics
        stats = self._calculate_statistics(file_data)
        self.profiles[category]['stats'] = stats
        
        return {
            'category': category,
            'directory': directory,
            'pattern': pattern,
            'file_count': len(file_data),
            'files': file_data,
            'statistics': stats
        }
    
    def scan_lngc_files(self, directory: str) -> Dict[str, Any]:
        """
        Scan for LNGC-specific .sim files.
        
        Args:
            directory: Directory to scan
            
        Returns:
            Scan results for LNGC files
        """
        logger.info("Scanning for LNGC .sim files...")
        
        # Look for LNGC patterns
        lngc_patterns = [
            "*lngc*.sim",
            "*LNGC*.sim",
            "*lng*.sim"
        ]
        
        all_results = []
        for pattern in lngc_patterns:
            result = self.scan_directory(directory, pattern, 'lngc')
            if result.get('file_count', 0) > 0:
                all_results.extend(result['files'])
        
        # Recalculate combined statistics
        if all_results:
            stats = self._calculate_statistics(all_results)
            self.profiles['lngc']['stats'] = stats
            
            return {
                'category': 'lngc',
                'directory': directory,
                'file_count': len(all_results),
                'files': all_results,
                'statistics': stats
            }
        
        return {'category': 'lngc', 'file_count': 0, 'files': []}
    
    def scan_standard_files(self, directory: str) -> Dict[str, Any]:
        """
        Scan for standard (non-LNGC) .sim files.
        
        Args:
            directory: Directory to scan
            
        Returns:
            Scan results for standard files
        """
        logger.info("Scanning for standard .sim files...")
        
        # Get all .sim files
        all_sim = list(Path(directory).glob("*.sim"))
        
        # Filter out LNGC files
        standard_files = []
        for file_path in all_sim:
            name_lower = file_path.name.lower()
            if 'lngc' not in name_lower and 'lng' not in name_lower:
                try:
                    size = file_path.stat().st_size
                    standard_files.append({
                        'path': str(file_path),
                        'name': file_path.name,
                        'size_bytes': size,
                        'size_mb': size / 1e6,
                        'size_gb': size / 1e9
                    })
                    
                    self.profiles['standard']['files'].append(str(file_path))
                    self.profiles['standard']['sizes'].append(size)
                    self.profiles['all']['files'].append(str(file_path))
                    self.profiles['all']['sizes'].append(size)
                    
                except Exception as e:
                    logger.error(f"Error scanning {file_path}: {e}")
        
        # Calculate statistics
        if standard_files:
            stats = self._calculate_statistics(standard_files)
            self.profiles['standard']['stats'] = stats
            
            return {
                'category': 'standard',
                'directory': directory,
                'file_count': len(standard_files),
                'files': standard_files,
                'statistics': stats
            }
        
        return {'category': 'standard', 'file_count': 0, 'files': []}
    
    def _calculate_statistics(self, file_data: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Calculate statistical metrics for file sizes.
        
        Args:
            file_data: List of file information dictionaries
            
        Returns:
            Statistical metrics
        """
        if not file_data:
            return {}
        
        sizes_mb = [f['size_mb'] for f in file_data]
        
        return {
            'count': len(file_data),
            'total_size_mb': sum(sizes_mb),
            'total_size_gb': sum(sizes_mb) / 1000,
            'mean_size_mb': np.mean(sizes_mb),
            'median_size_mb': np.median(sizes_mb),
            'std_dev_mb': np.std(sizes_mb),
            'min_size_mb': min(sizes_mb),
            'max_size_mb': max(sizes_mb),
            'percentiles': {
                '25': np.percentile(sizes_mb, 25),
                '50': np.percentile(sizes_mb, 50),
                '75': np.percentile(sizes_mb, 75),
                '90': np.percentile(sizes_mb, 90),
                '95': np.percentile(sizes_mb, 95)
            },
            'size_categories': self._categorize_sizes(sizes_mb)
        }
    
    def _categorize_sizes(self, sizes_mb: List[float]) -> Dict[str, int]:
        """
        Categorize files by size ranges.
        
        Args:
            sizes_mb: List of file sizes in MB
            
        Returns:
            Count of files in each category
        """
        categories = {
            'tiny': 0,      # < 10MB
            'small': 0,     # 10-100MB
            'medium': 0,    # 100-500MB
            'large': 0,     # 500MB-1GB
            'xlarge': 0,    # 1GB-5GB
            'huge': 0       # > 5GB
        }
        
        for size in sizes_mb:
            if size < 10:
                categories['tiny'] += 1
            elif size < 100:
                categories['small'] += 1
            elif size < 500:
                categories['medium'] += 1
            elif size < 1000:
                categories['large'] += 1
            elif size < 5000:
                categories['xlarge'] += 1
            else:
                categories['huge'] += 1
        
        return categories
    
    def create_size_histogram(self, category: str = 'all', 
                            save_path: Optional[str] = None) -> None:
        """
        Create histogram visualization of file sizes.
        
        Args:
            category: Which category to visualize
            save_path: Optional path to save the figure
        """
        if not self.profiles[category]['sizes']:
            logger.warning(f"No data available for category: {category}")
            return
        
        sizes_mb = [s / 1e6 for s in self.profiles[category]['sizes']]
        
        # Create figure with subplots
        fig, axes = plt.subplots(2, 2, figsize=(12, 10))
        fig.suptitle(f'File Size Distribution Analysis - {category.upper()}', fontsize=16)
        
        # 1. Histogram
        ax1 = axes[0, 0]
        ax1.hist(sizes_mb, bins=30, edgecolor='black', alpha=0.7)
        ax1.set_xlabel('File Size (MB)')
        ax1.set_ylabel('Count')
        ax1.set_title('Size Distribution Histogram')
        ax1.grid(True, alpha=0.3)
        
        # 2. Log-scale histogram
        ax2 = axes[0, 1]
        if min(sizes_mb) > 0:  # Avoid log(0)
            ax2.hist(sizes_mb, bins=30, edgecolor='black', alpha=0.7)
            ax2.set_xlabel('File Size (MB)')
            ax2.set_ylabel('Count')
            ax2.set_title('Size Distribution (Log Scale)')
            ax2.set_xscale('log')
            ax2.grid(True, alpha=0.3)
        
        # 3. Box plot
        ax3 = axes[1, 0]
        ax3.boxplot(sizes_mb, vert=False)
        ax3.set_xlabel('File Size (MB)')
        ax3.set_title('Size Distribution Box Plot')
        ax3.grid(True, alpha=0.3)
        
        # 4. Category pie chart
        ax4 = axes[1, 1]
        stats = self.profiles[category]['stats']
        if stats and 'size_categories' in stats:
            categories = stats['size_categories']
            labels = []
            sizes = []
            for cat, count in categories.items():
                if count > 0:
                    labels.append(f'{cat}\n({count} files)')
                    sizes.append(count)
            
            if sizes:
                ax4.pie(sizes, labels=labels, autopct='%1.1f%%')
                ax4.set_title('Size Categories')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=100, bbox_inches='tight')
            logger.info(f"Histogram saved to {save_path}")
        
        plt.show()
    
    def analyze_size_distribution(self) -> Dict[str, Any]:
        """
        Analyze the complete size distribution across all categories.
        
        Returns:
            Comprehensive analysis results
        """
        analysis = {
            'timestamp': datetime.now().isoformat(),
            'categories': {}
        }
        
        for category in ['lngc', 'standard', 'all']:
            if self.profiles[category]['stats']:
                stats = self.profiles[category]['stats']
                analysis['categories'][category] = {
                    'file_count': stats.get('count', 0),
                    'total_gb': stats.get('total_size_gb', 0),
                    'mean_mb': stats.get('mean_size_mb', 0),
                    'median_mb': stats.get('median_size_mb', 0),
                    'size_range_mb': [
                        stats.get('min_size_mb', 0),
                        stats.get('max_size_mb', 0)
                    ],
                    'size_categories': stats.get('size_categories', {})
                }
        
        # Optimization recommendations
        analysis['recommendations'] = self._generate_recommendations()
        
        return analysis
    
    def _generate_recommendations(self) -> List[str]:
        """
        Generate optimization recommendations based on file size analysis.
        
        Returns:
            List of recommendations
        """
        recommendations = []
        
        for category in ['lngc', 'standard']:
            stats = self.profiles[category]['stats']
            if not stats:
                continue
            
            mean_size = stats.get('mean_size_mb', 0)
            size_cats = stats.get('size_categories', {})
            
            # Thread recommendations
            if mean_size < 100:
                recommendations.append(
                    f"{category.upper()}: Use 45+ threads (small files, mean {mean_size:.1f}MB)"
                )
            elif mean_size < 500:
                recommendations.append(
                    f"{category.upper()}: Use 30 threads (medium files, mean {mean_size:.1f}MB)"
                )
            else:
                recommendations.append(
                    f"{category.upper()}: Use 15-20 threads (large files, mean {mean_size:.1f}MB)"
                )
            
            # Memory recommendations
            if size_cats.get('xlarge', 0) > 0 or size_cats.get('huge', 0) > 0:
                recommendations.append(
                    f"{category.upper()}: Implement memory pooling for {size_cats.get('xlarge', 0) + size_cats.get('huge', 0)} very large files"
                )
            
            # Batch recommendations
            if stats.get('std_dev_mb', 0) > mean_size * 0.5:
                recommendations.append(
                    f"{category.upper()}: High size variance - use size-based batching"
                )
        
        # General recommendations
        all_stats = self.profiles['all']['stats']
        if all_stats:
            total_gb = all_stats.get('total_size_gb', 0)
            if total_gb > 10:
                recommendations.append(
                    f"Consider distributed processing for {total_gb:.1f}GB total data"
                )
        
        return recommendations
    
    def save_profile(self, output_file: str = None) -> str:
        """
        Save profiling results to JSON file.
        
        Args:
            output_file: Output file path (auto-generated if None)
            
        Returns:
            Path to saved file
        """
        if not output_file:
            output_file = f"file_size_profile_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        analysis = self.analyze_size_distribution()
        
        with open(output_file, 'w') as f:
            json.dump(analysis, f, indent=2, default=str)
        
        logger.info(f"Profile saved to {output_file}")
        return output_file
    
    def print_summary(self) -> None:
        """Print summary of file size analysis."""
        print("\n" + "="*60)
        print("FILE SIZE DISTRIBUTION ANALYSIS")
        print("="*60)
        
        for category in ['lngc', 'standard', 'all']:
            stats = self.profiles[category]['stats']
            if not stats:
                continue
            
            print(f"\n{category.upper()} FILES:")
            print(f"  Count: {stats.get('count', 0)}")
            print(f"  Total: {stats.get('total_size_gb', 0):.2f} GB")
            print(f"  Mean: {stats.get('mean_size_mb', 0):.1f} MB")
            print(f"  Median: {stats.get('median_size_mb', 0):.1f} MB")
            print(f"  Range: {stats.get('min_size_mb', 0):.1f} - {stats.get('max_size_mb', 0):.1f} MB")
            
            size_cats = stats.get('size_categories', {})
            if size_cats:
                print("  Categories:")
                for cat, count in size_cats.items():
                    if count > 0:
                        print(f"    {cat}: {count} files")
        
        print("\n" + "="*60)
        print("OPTIMIZATION RECOMMENDATIONS:")
        print("="*60)
        for rec in self._generate_recommendations():
            print(f"  • {rec}")
        print("="*60 + "\n")


def main():
    """Main entry point for file size profiling."""
    parser = argparse.ArgumentParser(description='OrcaFlex File Size Profiler')
    parser.add_argument(
        '--directory', '-d',
        type=str,
        required=True,
        help='Directory containing OrcaFlex files'
    )
    parser.add_argument(
        '--pattern', '-p',
        type=str,
        default='*.sim',
        help='File pattern to match (default: *.sim)'
    )
    parser.add_argument(
        '--histogram',
        action='store_true',
        help='Create histogram visualization'
    )
    parser.add_argument(
        '--output', '-o',
        type=str,
        help='Output file for profile results'
    )
    
    args = parser.parse_args()
    
    # Create profiler
    profiler = FileSizeProfiler()
    
    # Scan for LNGC files
    lngc_results = profiler.scan_lngc_files(args.directory)
    logger.info(f"LNGC files found: {lngc_results.get('file_count', 0)}")
    
    # Scan for standard files
    standard_results = profiler.scan_standard_files(args.directory)
    logger.info(f"Standard files found: {standard_results.get('file_count', 0)}")
    
    # Analyze distribution
    analysis = profiler.analyze_size_distribution()
    
    # Print summary
    profiler.print_summary()
    
    # Save profile
    output_file = profiler.save_profile(args.output)
    
    # Create histogram if requested
    if args.histogram:
        profiler.create_size_histogram('all', save_path='file_size_histogram.png')
    
    return 0


if __name__ == "__main__":
    import sys
    sys.exit(main())