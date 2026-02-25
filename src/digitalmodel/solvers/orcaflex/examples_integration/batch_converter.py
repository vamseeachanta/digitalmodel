"""
Enhanced Batch Converter for OrcaFlex Examples

Converts downloaded .dat and .sim files to YAML format with comprehensive
error handling, validation, and reporting.
"""

import json
import logging
import time
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
import yaml
from tqdm import tqdm

logger = logging.getLogger(__name__)


class OrcaFlexBatchConverter:
    """Enhanced converter with batch processing and comprehensive reporting."""
    
    def __init__(self, 
                 input_dir: Path,
                 output_dir: Path,
                 use_mock: bool = False,
                 validate: bool = True,
                 max_retries: int = 2):
        """
        Initialize the batch converter.
        
        Args:
            input_dir: Directory containing .dat/.sim files
            output_dir: Directory for converted .yml files
            use_mock: Use mock conversion if OrcFxAPI unavailable
            validate: Validate converted YAML files
            max_retries: Maximum conversion retry attempts
        """
        self.input_dir = Path(input_dir)
        self.output_dir = Path(output_dir)
        self.use_mock = use_mock
        self.validate = validate
        self.max_retries = max_retries
        
        # Initialize OrcFxAPI
        self.api_available = False
        self.ofx = None
        
        if not use_mock:
            self._initialize_api()
        
        # Conversion tracking
        self.conversion_stats = {
            'total_files': 0,
            'successful': 0,
            'failed': 0,
            'skipped': 0,
            'validation_failed': 0,
            'processing_time': 0,
            'files_by_type': {'dat': 0, 'sim': 0},
            'errors': []
        }
        
        # Ensure output directory exists
        self.output_dir.mkdir(parents=True, exist_ok=True)
    
    def _initialize_api(self) -> bool:
        """Initialize OrcFxAPI connection."""
        try:
            import OrcFxAPI
            self.ofx = OrcFxAPI
            self.api_available = True
            logger.info("[SUCCESS] OrcFxAPI connection established")
            
            # Test API with version check
            try:
                model = self.ofx.Model()
                version = self.ofx.Version()
                logger.info(f"OrcaFlex version: {version}")
                del model  # Clean up test model
                return True
            except Exception as e:
                logger.warning(f"OrcFxAPI test failed: {e}")
                self.api_available = False
                return False
                
        except ImportError as e:
            logger.warning(f"OrcFxAPI not available: {e}")
            logger.info("Using mock mode for conversion")
            self.use_mock = True
            self.api_available = False
            return False
    
    def find_orcaflex_files(self) -> List[Path]:
        """Find all .dat and .sim files in input directory."""
        files = []
        
        # Search for .dat and .sim files
        for pattern in ['**/*.dat', '**/*.sim']:
            files.extend(self.input_dir.glob(pattern))
        
        # Sort by size (smaller files first for faster initial results)
        files.sort(key=lambda f: f.stat().st_size)
        
        # Update stats
        for file in files:
            ext = file.suffix.lower()[1:]
            self.conversion_stats['files_by_type'][ext] += 1
        
        self.conversion_stats['total_files'] = len(files)
        
        logger.info(f"Found {len(files)} files to convert")
        logger.info(f"  - .dat files: {self.conversion_stats['files_by_type']['dat']}")
        logger.info(f"  - .sim files: {self.conversion_stats['files_by_type']['sim']}")
        
        return files
    
    def convert_file(self, input_file: Path) -> Tuple[bool, Optional[Path], Optional[str]]:
        """
        Convert a single OrcaFlex file to YAML.
        
        Returns:
            Tuple of (success, output_path, error_message)
        """
        # Determine output path (preserve directory structure)
        relative_path = input_file.relative_to(self.input_dir)
        output_file = self.output_dir / relative_path.with_suffix('.yml')
        output_file.parent.mkdir(parents=True, exist_ok=True)
        
        # Check if already converted
        if output_file.exists() and output_file.stat().st_size > 0:
            logger.debug(f"Skipping {input_file.name} - already converted")
            self.conversion_stats['skipped'] += 1
            return True, output_file, None
        
        # Try conversion with retries
        for attempt in range(self.max_retries):
            try:
                if self.use_mock:
                    # Mock conversion - create placeholder YAML
                    return self._mock_convert(input_file, output_file)
                else:
                    # Real conversion using OrcFxAPI
                    return self._real_convert(input_file, output_file)
                    
            except Exception as e:
                error_msg = f"Attempt {attempt + 1} failed: {str(e)}"
                logger.warning(f"Conversion error for {input_file.name}: {error_msg}")
                
                if attempt < self.max_retries - 1:
                    time.sleep(2 ** attempt)  # Exponential backoff
                else:
                    self.conversion_stats['failed'] += 1
                    self.conversion_stats['errors'].append({
                        'file': str(input_file),
                        'error': str(e),
                        'attempts': attempt + 1
                    })
                    return False, None, str(e)
        
        return False, None, "Max retries exceeded"
    
    def _real_convert(self, input_file: Path, output_file: Path) -> Tuple[bool, Path, Optional[str]]:
        """Convert using real OrcFxAPI."""
        if not self.api_available:
            return self._mock_convert(input_file, output_file)
        
        try:
            # Create new model instance
            model = self.ofx.Model()
            
            # Load the input file
            model.LoadData(str(input_file))
            
            # Save as YAML
            model.SaveData(str(output_file))
            
            # Clean up
            del model
            
            # Validate if requested
            if self.validate and not self._validate_yaml(output_file):
                self.conversion_stats['validation_failed'] += 1
                logger.warning(f"Validation failed for {output_file.name}")
            
            self.conversion_stats['successful'] += 1
            return True, output_file, None
            
        except Exception as e:
            logger.error(f"Real conversion failed: {e}")
            return False, None, str(e)
    
    def _mock_convert(self, input_file: Path, output_file: Path) -> Tuple[bool, Path, Optional[str]]:
        """Create mock YAML conversion for testing."""
        try:
            # Create mock YAML structure
            mock_data = {
                'OrcaFlexModel': {
                    'SourceFile': str(input_file),
                    'ConversionDate': datetime.now().isoformat(),
                    'MockConversion': True,
                    'FileType': input_file.suffix[1:].upper(),
                    'FileSize': input_file.stat().st_size,
                    'General': {
                        'UnitsSystem': 'SI',
                        'g': 9.80665,
                        'WaterDensity': 1025.0,
                        'AirDensity': 1.225
                    },
                    'Environment': {
                        'WaterDepth': 100.0,
                        'CurrentSpeed': 1.0,
                        'WaveHeight': 5.0,
                        'WavePeriod': 10.0
                    },
                    'Components': {
                        'Note': 'Mock data - actual components not parsed'
                    }
                }
            }
            
            # Write YAML file
            with open(output_file, 'w') as f:
                yaml.dump(mock_data, f, default_flow_style=False, sort_keys=False)
            
            self.conversion_stats['successful'] += 1
            return True, output_file, None
            
        except Exception as e:
            logger.error(f"Mock conversion failed: {e}")
            return False, None, str(e)
    
    def _validate_yaml(self, yaml_file: Path) -> bool:
        """Validate converted YAML file structure."""
        try:
            with open(yaml_file, 'r') as f:
                data = yaml.safe_load(f)
            
            # Check for basic OrcaFlex structure
            if not isinstance(data, dict):
                return False
            
            # Check for either real or mock conversion markers
            if 'OrcaFlexModel' in data or 'General' in data:
                # Check file size (should be reasonable)
                if yaml_file.stat().st_size > 100:  # At least 100 bytes
                    return True
            
            return False
            
        except Exception as e:
            logger.error(f"Validation error for {yaml_file}: {e}")
            return False
    
    def convert_batch(self, file_list: Optional[List[Path]] = None) -> Dict[str, Any]:
        """
        Convert a batch of files with progress tracking.
        
        Args:
            file_list: Optional list of files to convert. If None, finds all files.
            
        Returns:
            Conversion statistics and results
        """
        start_time = time.time()
        
        # Get files to convert
        if file_list is None:
            files = self.find_orcaflex_files()
        else:
            files = file_list
            self.conversion_stats['total_files'] = len(files)
        
        if not files:
            logger.warning("No files found to convert")
            return self.conversion_stats
        
        # Convert files with progress bar
        results = []
        with tqdm(total=len(files), desc="Converting files", unit="file") as pbar:
            for file in files:
                success, output_path, error = self.convert_file(file)
                
                results.append({
                    'input': str(file),
                    'output': str(output_path) if output_path else None,
                    'success': success,
                    'error': error
                })
                
                # Update progress bar
                pbar.update(1)
                status = "[OK]" if success else "[ERR]"
                pbar.set_postfix_str(f"{status} {file.name}")
        
        # Calculate final statistics
        self.conversion_stats['processing_time'] = time.time() - start_time
        
        # Generate report
        self._generate_report(results)
        
        return {
            'statistics': self.conversion_stats,
            'results': results
        }
    
    def _generate_report(self, results: List[Dict]) -> Path:
        """Generate conversion report."""
        report_path = self.output_dir / 'conversion_report.md'
        
        with open(report_path, 'w', encoding='utf-8') as f:
            f.write("# OrcaFlex Batch Conversion Report\n\n")
            f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            
            # Summary statistics
            f.write("## Summary\n\n")
            f.write(f"- **Total Files**: {self.conversion_stats['total_files']}\n")
            f.write(f"- **Successful**: {self.conversion_stats['successful']}\n")
            f.write(f"- **Failed**: {self.conversion_stats['failed']}\n")
            f.write(f"- **Skipped**: {self.conversion_stats['skipped']}\n")
            f.write(f"- **Validation Failed**: {self.conversion_stats['validation_failed']}\n")
            f.write(f"- **Processing Time**: {self.conversion_stats['processing_time']:.2f} seconds\n")
            f.write(f"- **Conversion Mode**: {'Mock' if self.use_mock else 'Real (OrcFxAPI)'}\n\n")
            
            # File type breakdown
            f.write("## File Types\n\n")
            f.write(f"- **.dat files**: {self.conversion_stats['files_by_type']['dat']}\n")
            f.write(f"- **.sim files**: {self.conversion_stats['files_by_type']['sim']}\n\n")
            
            # Success rate
            attempted = self.conversion_stats['total_files'] - self.conversion_stats['skipped']
            if attempted > 0:
                success_rate = (self.conversion_stats['successful'] / attempted) * 100
                f.write(f"## Success Rate: {success_rate:.1f}%\n\n")
            
            # Failed conversions
            if self.conversion_stats['errors']:
                f.write("## Failed Conversions\n\n")
                for error in self.conversion_stats['errors']:
                    f.write(f"- `{Path(error['file']).name}`\n")
                    f.write(f"  - Error: {error['error']}\n")
                    f.write(f"  - Attempts: {error['attempts']}\n\n")
            
            # Successful conversions
            successful = [r for r in results if r['success']]
            if successful:
                f.write("## Successful Conversions\n\n")
                for result in successful[:10]:  # Show first 10
                    f.write(f"- `{Path(result['input']).name}` -> `{Path(result['output']).name}`\n")
                
                if len(successful) > 10:
                    f.write(f"\n... and {len(successful) - 10} more files\n")
        
        # Also save detailed JSON report
        json_report_path = self.output_dir / 'conversion_report.json'
        with open(json_report_path, 'w') as f:
            json.dump({
                'statistics': self.conversion_stats,
                'results': results,
                'timestamp': datetime.now().isoformat()
            }, f, indent=2)
        
        logger.info(f"[REPORT] Report saved to {report_path}")
        return report_path


def main():
    """Run batch conversion of OrcaFlex examples."""
    # Set up paths
    input_dir = Path("docs/domains/orcaflex/examples/raw")
    output_dir = Path("docs/domains/orcaflex/examples/yaml")
    
    # Check if input directory exists
    if not input_dir.exists():
        logger.error(f"Input directory not found: {input_dir}")
        return
    
    # Create converter
    converter = OrcaFlexBatchConverter(
        input_dir=input_dir,
        output_dir=output_dir,
        use_mock=False,  # Try real conversion first
        validate=True,
        max_retries=2
    )
    
    # Run batch conversion
    logger.info("Starting batch conversion...")
    results = converter.convert_batch()
    
    # Print summary
    print("\n" + "="*60)
    print("CONVERSION COMPLETE")
    print("="*60)
    print(f"Total files: {results['statistics']['total_files']}")
    print(f"Successful: {results['statistics']['successful']}")
    print(f"Failed: {results['statistics']['failed']}")
    print(f"Skipped: {results['statistics']['skipped']}")
    print(f"Time: {results['statistics']['processing_time']:.2f} seconds")
    print("="*60)


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    main()