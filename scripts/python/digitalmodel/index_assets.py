import os
import json
import time
import logging
import argparse
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Any

# Configure Logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# Default Configuration
DEFAULT_SEARCH_PATHS = [
    "/mnt/ace/O&G-Standards",
    "/mnt/ace/docs",
    "/mnt/ace/Production"
]

DEFAULT_OUTPUT_FILE = "data/legacy_assets_index.json"

# File Type Classification
TYPE_PATTERNS = {
    "Standard": [".pdf", "API", "DNV", "ISO", "ABS", "BSI"],
    "Report": [".pdf", ".docx", ".doc", "Report", "REP"],
    "Spreadsheet": [".xlsx", ".xls", ".csv"],
    "Presentation": [".pptx", ".ppt"],
    "FEA_Model": [".inp", ".odb", ".dat", ".ans", ".rst", ".cae"],
    "OrcaFlex": [".dat", ".sim", ".yml"],
    "Code": [".py", ".m", ".vbs"],
    "Image": [".jpg", ".png", ".jpeg", ".tif"]
}

def guess_type(file_path: Path) -> str:
    path_str = str(file_path).lower()
    ext = file_path.suffix.lower()
    
    # Priority checks for high-value engineering files
    if ext in [".sim", ".yml"]:
        if "orcaflex" in path_str or "orca" in path_str or ext == ".sim":
            return "OrcaFlex"
            
    if ext == ".dat":
        # In this workspace, .dat is almost always OrcaFlex unless specified otherwise
        if "nastran" in path_str or "ansys" in path_str:
            return "FEA_Model"
        return "OrcaFlex"

    if ext in [".inp", ".odb", ".ans", ".rst", ".cae"]:
        return "FEA_Model"
    
    for type_name, patterns in TYPE_PATTERNS.items():
        # Check extension first
        if ext in patterns:
            return type_name
            
        # Check keywords in path for PDF/Docs
        if ext in [".pdf", ".docx", ".doc"]:
            for pattern in patterns:
                if pattern.lower() in path_str and pattern.lower() not in [".pdf", ".docx", ".doc"]:
                    return type_name
                    
    return "Unknown"

def index_directory(paths: List[str]) -> List[Dict[str, Any]]:
    assets = []
    
    logger.info(f"Starting index of: {paths}")
    start_time = time.time()
    
    for root_path in paths:
        root = Path(root_path)
        if not root.exists():
            logger.warning(f"Path not found: {root}")
            continue
            
        for path in root.rglob("*"):
            if path.is_file():
                try:
                    stat = path.stat()
                    asset = {
                        "path": str(path),
                        "name": path.name,
                        "size_bytes": stat.st_size,
                        "modified_timestamp": stat.st_mtime,
                        "modified_date": datetime.fromtimestamp(stat.st_mtime).isoformat(),
                        "type": guess_type(path),
                        "extension": path.suffix.lower()
                    }
                    assets.append(asset)
                    
                    if len(assets) % 5000 == 0:
                        logger.info(f"Indexed {len(assets)} files...")
                        
                except PermissionError:
                    logger.warning(f"Permission denied: {path}")
                except Exception as e:
                    logger.error(f"Error processing {path}: {e}")

    duration = time.time() - start_time
    logger.info(f"Indexing complete. Found {len(assets)} files in {duration:.2f} seconds.")
    return assets

def main():
    parser = argparse.ArgumentParser(description="Index legacy assets for Digital Model.")
    parser.add_argument("--paths", nargs='+', default=DEFAULT_SEARCH_PATHS, help="List of paths to index")
    parser.add_argument("--output", default=DEFAULT_OUTPUT_FILE, help="Output JSON file path")
    
    args = parser.parse_args()
    
    assets = index_directory(args.paths)
    
    # Ensure output directory exists
    Path(args.output).parent.mkdir(parents=True, exist_ok=True)
    
    with open(args.output, "w") as f:
        json.dump(assets, f, indent=2)
    
    logger.info(f"Index saved to {args.output}")

    # Print summary
    type_counts = {}
    for a in assets:
        t = a["type"]
        type_counts[t] = type_counts.get(t, 0) + 1
    
    print("\nAsset Summary:")
    for t, count in sorted(type_counts.items(), key=lambda x: x[1], reverse=True):
        print(f"  {t}: {count}")

if __name__ == "__main__":
    main()
