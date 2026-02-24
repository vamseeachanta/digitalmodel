#!/usr/bin/env python
"""
Download all OrcaFlex examples from the Orcina portal.

This script downloads all available example ZIP files and extracts them.
It can be run multiple times - it will skip already downloaded files.
"""

import sys
import logging
from pathlib import Path
from enhanced_downloader import EnhancedOrcaflexDownloader

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


def download_all_examples(skip_large: bool = False):
    """
    Download all OrcaFlex examples.
    
    Args:
        skip_large: If True, skip files larger than 50MB
    """
    logger.info("="*60)
    logger.info("OrcaFlex Examples Bulk Download")
    logger.info("="*60)
    
    downloader = EnhancedOrcaflexDownloader()
    
    # Parse the examples page
    logger.info("\nStep 1: Parsing examples page...")
    zip_files = downloader.parse_examples_page()
    
    if not zip_files:
        logger.error("No ZIP files found on the examples page!")
        return False
    
    logger.info(f"Found {len(zip_files)} ZIP files")
    
    # Calculate total size
    total_size_mb = sum(zf['size_mb'] for zf in zip_files)
    logger.info(f"Total download size: {total_size_mb}MB")
    
    if skip_large:
        # Filter out large files
        original_count = len(zip_files)
        zip_files = [zf for zf in zip_files if zf['size_mb'] <= 50]
        if len(zip_files) < original_count:
            logger.info(f"Skipping {original_count - len(zip_files)} large files (>50MB)")
            new_total = sum(zf['size_mb'] for zf in zip_files)
            logger.info(f"Reduced download size: {new_total}MB")
    
    # Sort by size (smallest first for quicker initial results)
    zip_files.sort(key=lambda x: x['size_mb'] if x['size_mb'] > 0 else float('inf'))
    
    logger.info("\nFiles to download:")
    for i, zf in enumerate(zip_files, 1):
        logger.info(f"  {i}. {zf['category_code']} - {zf['description']} ({zf['size_mb']}MB)")
    
    # Download each file
    logger.info("\nStep 2: Downloading and extracting files...")
    logger.info("-"*40)
    
    successful_downloads = 0
    failed_downloads = 0
    total_extracted = 0
    
    for i, zip_info in enumerate(zip_files, 1):
        logger.info(f"\n[{i}/{len(zip_files)}] Processing: {zip_info['filename']}")
        
        try:
            # Download
            zip_path = downloader.download_zip_file(zip_info)
            
            if zip_path:
                # Extract
                extracted_files = downloader.extract_zip_file(zip_path, zip_info)
                
                if extracted_files:
                    successful_downloads += 1
                    total_extracted += len(extracted_files)
                    logger.info(f"✅ Success: Extracted {len(extracted_files)} OrcaFlex files")
                else:
                    # Still count as success if download worked but no OrcaFlex files
                    successful_downloads += 1
                    logger.warning("⚠️  No OrcaFlex files found in ZIP (may contain only docs)")
            else:
                failed_downloads += 1
                logger.error(f"❌ Failed to download {zip_info['filename']}")
                
        except Exception as e:
            failed_downloads += 1
            logger.error(f"❌ Error processing {zip_info['filename']}: {e}")
    
    # Generate catalog
    logger.info("\nStep 3: Generating catalog...")
    try:
        catalog = downloader.generate_catalog()
        logger.info("✅ Catalog generated successfully")
    except Exception as e:
        logger.error(f"Failed to generate catalog: {e}")
    
    # Summary
    logger.info("\n" + "="*60)
    logger.info("Download Summary")
    logger.info("="*60)
    logger.info(f"✅ Successful downloads: {successful_downloads}")
    logger.info(f"❌ Failed downloads: {failed_downloads}")
    logger.info(f"📁 Total OrcaFlex files extracted: {total_extracted}")
    
    # Check what we have
    raw_dir = Path("docs/modules/orcaflex/examples/raw")
    if raw_dir.exists():
        dat_files = list(raw_dir.glob("**/*.dat"))
        sim_files = list(raw_dir.glob("**/*.sim"))
        yml_files = list(raw_dir.glob("**/*.yml")) + list(raw_dir.glob("**/*.yaml"))
        
        logger.info(f"\nFile types extracted:")
        logger.info(f"  .dat files: {len(dat_files)}")
        logger.info(f"  .sim files: {len(sim_files)}")
        logger.info(f"  .yml/.yaml files: {len(yml_files)}")
    
    return successful_downloads > 0


def main():
    """Main entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(description="Download OrcaFlex examples from Orcina portal")
    parser.add_argument(
        '--skip-large',
        action='store_true',
        help='Skip files larger than 50MB'
    )
    parser.add_argument(
        '--test',
        action='store_true',
        help='Test mode - download only the smallest file'
    )
    
    args = parser.parse_args()
    
    if args.test:
        logger.info("Running in TEST mode - will download only the smallest file")
        # Use the test script instead
        from test_single_download import test_single_download
        success = test_single_download()
    else:
        success = download_all_examples(skip_large=args.skip_large)
    
    if success:
        logger.info("\n✅ Download process completed successfully!")
        sys.exit(0)
    else:
        logger.error("\n❌ Download process failed!")
        sys.exit(1)


if __name__ == "__main__":
    main()