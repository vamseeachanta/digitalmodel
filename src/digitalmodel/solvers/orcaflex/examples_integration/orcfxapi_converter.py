#!/usr/bin/env python
"""
OrcFxAPI Converter - Convert .sim files to both .dat and .yml formats

This converter uses the actual OrcFxAPI to load .sim files and save them
as both .dat (data) and .yml (YAML) files in the same directory.
"""

import logging
import sys
import time
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from datetime import datetime
import json

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class OrcFxAPIConverter:
    """Convert OrcaFlex .sim files to .dat and .yml using OrcFxAPI."""
    
    def __init__(self, input_dir: Path, preserve_originals: bool = True):
        """
        Initialize the OrcFxAPI converter.
        
        Args:
            input_dir: Directory containing .sim files
            preserve_originals: Keep original .sim files (default: True)
        """
        self.input_dir = Path(input_dir)
        self.preserve_originals = preserve_originals
        self.api_available = False
        self.ofx = None
        
        # Statistics tracking
        self.stats = {
            'total_files': 0,
            'successful_conversions': 0,
            'failed_conversions': 0,
            'dat_files_created': 0,
            'yml_files_created': 0,
            'errors': [],
            'processing_time': 0
        }
        
        # Initialize OrcFxAPI
        self._initialize_api()
    
    def _initialize_api(self) -> bool:
        """Initialize and test OrcFxAPI connection."""
        try:
            import OrcFxAPI
            self.ofx = OrcFxAPI
            self.api_available = True
            
            # Test the API
            logger.info("Testing OrcFxAPI connection...")
            model = self.ofx.Model()
            
            # Get version info
            try:
                # Try different version methods
                version_info = None
                if hasattr(self.ofx, 'Version'):
                    version_info = self.ofx.Version()
                elif hasattr(model, 'Version'):
                    version_info = model.Version
                elif hasattr(model, 'general') and hasattr(model.general, 'ProgramVersion'):
                    version_info = model.general.ProgramVersion
                
                if version_info:
                    logger.info(f"OrcaFlex version: {version_info}")
                else:
                    logger.info("OrcaFlex API connected (version info not available)")
            except:
                logger.info("OrcaFlex API connected successfully")
            
            del model  # Clean up test model
            return True
            
        except ImportError as e:
            logger.error(f"OrcFxAPI not available: {e}")
            logger.error("Please ensure OrcaFlex is installed and Python API is configured")
            self.api_available = False
            return False
        except Exception as e:
            logger.error(f"Failed to initialize OrcFxAPI: {e}")
            self.api_available = False
            return False
    
    def find_sim_files(self) -> List[Path]:
        """Find all .sim files in the input directory."""
        sim_files = list(self.input_dir.glob('**/*.sim'))
        self.stats['total_files'] = len(sim_files)
        
        logger.info(f"Found {len(sim_files)} .sim files to convert")
        return sim_files
    
    def convert_file(self, sim_file: Path) -> Tuple[bool, Optional[str]]:
        """
        Convert a single .sim file to both .dat and .yml formats.
        
        Args:
            sim_file: Path to the .sim file
            
        Returns:
            Tuple of (success, error_message)
        """
        if not self.api_available:
            return False, "OrcFxAPI not available"
        
        try:
            logger.info(f"Converting: {sim_file.name}")
            
            # Create a new model instance
            model = self.ofx.Model()
            
            # Load the .sim file
            logger.debug(f"Loading {sim_file}")
            model.LoadSimulation(str(sim_file))
            
            # Define output paths (same directory as .sim file)
            base_path = sim_file.parent / sim_file.stem
            dat_path = base_path.with_suffix('.dat')
            yml_path = base_path.with_suffix('.yml')
            
            # Save as .dat file
            try:
                logger.debug(f"Saving as .dat: {dat_path}")
                model.SaveData(str(dat_path))
                self.stats['dat_files_created'] += 1
                logger.info(f"  [OK] Created: {dat_path.name}")
            except Exception as e:
                logger.warning(f"  [FAIL] Failed to save .dat: {e}")
            
            # Save as .yml file
            try:
                logger.debug(f"Saving as .yml: {yml_path}")
                model.SaveData(str(yml_path))
                self.stats['yml_files_created'] += 1
                logger.info(f"  [OK] Created: {yml_path.name}")
            except Exception as e:
                logger.warning(f"  [FAIL] Failed to save .yml: {e}")
            
            # Clean up
            del model
            
            self.stats['successful_conversions'] += 1
            return True, None
            
        except Exception as e:
            error_msg = str(e)
            logger.error(f"Failed to convert {sim_file.name}: {error_msg}")
            self.stats['failed_conversions'] += 1
            self.stats['errors'].append({
                'file': str(sim_file),
                'error': error_msg
            })
            return False, error_msg
    
    def convert_all(self) -> Dict:
        """
        Convert all .sim files in the input directory.
        
        Returns:
            Statistics dictionary
        """
        if not self.api_available:
            logger.error("Cannot proceed - OrcFxAPI not available")
            return self.stats
        
        start_time = time.time()
        
        # Find all .sim files
        sim_files = self.find_sim_files()
        
        if not sim_files:
            logger.warning("No .sim files found to convert")
            return self.stats
        
        # Convert each file
        logger.info("\nStarting conversion process...")
        logger.info("="*60)
        
        for i, sim_file in enumerate(sim_files, 1):
            logger.info(f"\n[{i}/{len(sim_files)}] Processing: {sim_file.relative_to(self.input_dir)}")
            success, error = self.convert_file(sim_file)
            
            if not success and error:
                logger.error(f"  Error: {error}")
        
        # Calculate final statistics
        self.stats['processing_time'] = time.time() - start_time
        
        # Generate report
        self._generate_report()
        
        return self.stats
    
    def _generate_report(self):
        """Generate and display conversion report."""
        print("\n" + "="*60)
        print("CONVERSION REPORT")
        print("="*60)
        print(f"Total .sim files found:     {self.stats['total_files']}")
        print(f"Successful conversions:     {self.stats['successful_conversions']}")
        print(f"Failed conversions:         {self.stats['failed_conversions']}")
        print("-"*60)
        print(f".dat files created:         {self.stats['dat_files_created']}")
        print(f".yml files created:         {self.stats['yml_files_created']}")
        print("-"*60)
        print(f"Processing time:            {self.stats['processing_time']:.2f} seconds")
        
        if self.stats['successful_conversions'] > 0:
            avg_time = self.stats['processing_time'] / self.stats['successful_conversions']
            print(f"Average time per file:      {avg_time:.2f} seconds")
        
        # Success rate
        if self.stats['total_files'] > 0:
            success_rate = (self.stats['successful_conversions'] / self.stats['total_files']) * 100
            print(f"Success rate:               {success_rate:.1f}%")
        
        print("="*60)
        
        # Save report to JSON
        report_path = self.input_dir / 'conversion_report_orcfxapi.json'
        with open(report_path, 'w') as f:
            json.dump({
                'timestamp': datetime.now().isoformat(),
                'statistics': self.stats,
                'input_directory': str(self.input_dir)
            }, f, indent=2)
        
        print(f"\nDetailed report saved to: {report_path}")
        
        # Display errors if any
        if self.stats['errors']:
            print(f"\n[WARNING] {len(self.stats['errors'])} files failed to convert:")
            for error in self.stats['errors'][:5]:
                print(f"  - {Path(error['file']).name}: {error['error']}")
            if len(self.stats['errors']) > 5:
                print(f"  ... and {len(self.stats['errors']) - 5} more errors")


def main():
    """Main function to run the converter."""
    print("\n" + "="*70)
    print("ORCFXAPI CONVERTER - .sim to .dat/.yml")
    print("="*70)
    
    # Set up paths
    base_dir = Path(__file__).parent.parent.parent.parent.parent.parent
    input_dir = base_dir / "docs/domains/orcaflex/examples/raw"
    
    # Check if input directory exists
    if not input_dir.exists():
        logger.error(f"Input directory not found: {input_dir}")
        print("\n[ERROR] Input directory not found.")
        print("Please run the downloader first to get OrcaFlex examples.")
        return 1
    
    # Create converter
    print(f"\nInput directory: {input_dir}")
    print("Initializing OrcFxAPI converter...")
    
    converter = OrcFxAPIConverter(
        input_dir=input_dir,
        preserve_originals=True  # Keep the .sim files
    )
    
    if not converter.api_available:
        print("\n[ERROR] OrcFxAPI is not available!")
        print("\nTo use this converter, you need:")
        print("1. OrcaFlex installed on this system")
        print("2. OrcFxAPI Python module configured")
        print("\nInstallation steps:")
        print("1. Install OrcaFlex from Orcina")
        print("2. Run: pip install <OrcaFlex_install_dir>/OrcFxAPI/Python")
        print("3. Ensure you have a valid OrcaFlex license")
        return 1
    
    # Run conversion
    print("\nStarting conversion of .sim files to .dat and .yml formats...")
    print("Files will be saved in the same directory as the originals.")
    print("-"*70)
    
    stats = converter.convert_all()
    
    # Display next steps
    if stats['successful_conversions'] > 0:
        print("\n" + "="*70)
        print("NEXT STEPS")
        print("="*70)
        print("1. Converted files are in their original directories")
        print("2. .dat files: OrcaFlex data format (smaller, faster to load)")
        print("3. .yml files: YAML format (human-readable, version control friendly)")
        print("4. Original .sim files are preserved")
        print("5. Ready for feature analysis (Task 4)")
        print("="*70)
    
    return 0 if stats['failed_conversions'] == 0 else 1


if __name__ == "__main__":
    sys.exit(main())