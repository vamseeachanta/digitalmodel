#!/usr/bin/env python
"""
Test downloading a single example ZIP file to verify the enhanced downloader works.
"""

import logging
from enhanced_downloader import EnhancedOrcaflexDownloader

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def test_single_download():
    """Test downloading a single small example."""
    
    downloader = EnhancedOrcaflexDownloader()
    
    # Parse the page to get available ZIPs
    logger.info("Parsing examples page...")
    zip_files = downloader.parse_examples_page()
    
    if not zip_files:
        logger.error("No ZIP files found!")
        return False
    
    # Find the smallest ZIP file for testing
    smallest_zip = min(zip_files, key=lambda x: x['size_mb'] if x['size_mb'] > 0 else float('inf'))
    
    logger.info(f"\nSelected for test download:")
    logger.info(f"  Category: {smallest_zip['category_code']}")
    logger.info(f"  Description: {smallest_zip['description']}")
    logger.info(f"  Size: {smallest_zip['size_mb']}MB")
    logger.info(f"  URL: {smallest_zip['url']}")
    
    # Download the ZIP file
    logger.info("\nDownloading ZIP file...")
    zip_path = downloader.download_zip_file(smallest_zip)
    
    if not zip_path:
        logger.error("Download failed!")
        return False
    
    logger.info(f"Successfully downloaded to: {zip_path}")
    
    # Extract the files
    logger.info("\nExtracting OrcaFlex files...")
    extracted_files = downloader.extract_zip_file(zip_path, smallest_zip)
    
    if not extracted_files:
        logger.warning("No OrcaFlex files extracted (ZIP might contain only docs)")
    else:
        logger.info(f"Extracted {len(extracted_files)} files:")
        for file_path in extracted_files[:5]:  # Show first 5
            logger.info(f"  - {file_path}")
    
    # Generate a mini catalog
    catalog = downloader.generate_catalog()
    logger.info("\nCatalog generated successfully")
    
    return True


if __name__ == "__main__":
    success = test_single_download()
    if success:
        print("\n✅ Test completed successfully!")
    else:
        print("\n❌ Test failed!")