#!/usr/bin/env python
"""
Fix extraction of OrcaFlex files from already downloaded ZIPs.
"""

import os
import zipfile
import logging
from pathlib import Path
from enhanced_downloader import EnhancedOrcaflexDownloader

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def re_extract_all():
    """Re-extract all OrcaFlex files from downloaded ZIPs."""
    
    downloader = EnhancedOrcaflexDownloader()
    
    # Get all downloaded ZIP files
    zip_dir = Path("docs/modules/orcaflex/examples/downloads")
    zip_files = list(zip_dir.glob("*.zip"))
    
    logger.info(f"Found {len(zip_files)} ZIP files to re-extract")
    
    total_extracted = 0
    
    for zip_path in zip_files:
        logger.info(f"\nProcessing: {zip_path.name}")
        
        # Extract category from filename
        # A01 Catenary and wave systems.zip -> A01
        category_code = zip_path.name[:3] if zip_path.name[0] == 'A' else 'Unknown'
        
        # Create category directory
        category_dir = Path("docs/modules/orcaflex/examples/raw") / category_code
        category_dir.mkdir(parents=True, exist_ok=True)
        
        try:
            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                # List all files
                file_list = zip_ref.namelist()
                logger.info(f"  ZIP contains {len(file_list)} files")
                
                # Extract OrcaFlex files
                orcaflex_extensions = ['.dat', '.sim', '.yml', '.yaml']
                extracted_count = 0
                
                for file_name in file_list:
                    # Check if it's an OrcaFlex file
                    if any(file_name.lower().endswith(ext) for ext in orcaflex_extensions):
                        # Get just the filename without path
                        base_name = os.path.basename(file_name)
                        if not base_name:  # Skip directories
                            continue
                            
                        target_path = category_dir / base_name
                        
                        # Extract file
                        with zip_ref.open(file_name) as source:
                            with open(target_path, 'wb') as target:
                                target.write(source.read())
                        
                        extracted_count += 1
                        logger.info(f"  Extracted: {base_name}")
                
                logger.info(f"  Total extracted: {extracted_count} OrcaFlex files")
                total_extracted += extracted_count
                
                # Also extract PDFs to docs subdirectory
                doc_dir = category_dir / 'docs'
                for file_name in file_list:
                    if file_name.lower().endswith('.pdf'):
                        doc_dir.mkdir(exist_ok=True)
                        base_name = os.path.basename(file_name)
                        if base_name:
                            target_path = doc_dir / base_name
                            with zip_ref.open(file_name) as source:
                                with open(target_path, 'wb') as target:
                                    target.write(source.read())
                            logger.info(f"  Extracted PDF: {base_name}")
                
        except Exception as e:
            logger.error(f"Failed to extract {zip_path.name}: {e}")
    
    logger.info(f"\n{'='*60}")
    logger.info(f"Re-extraction complete!")
    logger.info(f"Total OrcaFlex files extracted: {total_extracted}")
    
    # List what we have now
    raw_dir = Path("docs/modules/orcaflex/examples/raw")
    for category_dir in sorted(raw_dir.iterdir()):
        if category_dir.is_dir():
            files = list(category_dir.glob("*.sim")) + list(category_dir.glob("*.dat"))
            if files:
                logger.info(f"\n{category_dir.name}: {len(files)} files")
                for f in files[:3]:  # Show first 3
                    logger.info(f"  - {f.name}")
                if len(files) > 3:
                    logger.info(f"  ... and {len(files)-3} more")


if __name__ == "__main__":
    re_extract_all()