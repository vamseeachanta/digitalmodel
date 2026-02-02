import json
import logging
import os
from pathlib import Path
from typing import List, Dict, Optional, Any, TypedDict

# Configure logging
logger = logging.getLogger(__name__)

class StandardDoc(TypedDict):
    path: str
    name: str
    size_bytes: int
    modified_timestamp: float
    modified_date: str
    type: str
    extension: str

class StandardsLookup:
    """
    A tool to lookup engineering standards from the indexed legacy data.
    """
    
    def __init__(self, index_path: Optional[str] = None):
        self.index_path = self._resolve_index_path(index_path)
        self.standards: List[StandardDoc] = []
        self._load_index()

    def _resolve_index_path(self, provided_path: Optional[str]) -> Path:
        """Resolve the absolute path to the standards index."""
        if provided_path:
            return Path(provided_path)

        # 1. Try environment variable
        env_path = os.environ.get("DIGITALMODEL_STANDARDS_INDEX")
        if env_path:
            return Path(env_path)

        # 2. Try relative to current working directory (dev mode)
        cwd_path = Path("digitalmodel/data/standards/index.json")
        if cwd_path.exists():
            return cwd_path.resolve()

        # 3. Fallback to package-relative path
        # Assuming: src/digitalmodel/modules/standards_lookup.py
        # Target:   data/standards/index.json (relative to repo root)
        # This logic attempts to find the data dir relative to the installed package location
        base_dir = Path(__file__).resolve().parent.parent.parent.parent
        package_path = base_dir / "data" / "standards" / "index.json"
        
        return package_path

    def _load_index(self):
        if not self.index_path.exists():
            logger.warning(f"Standards index not found at {self.index_path}")
            return
            
        try:
            with open(self.index_path, "r") as f:
                self.standards = json.load(f)
            logger.info(f"Loaded {len(self.standards)} standards from {self.index_path}")
        except json.JSONDecodeError:
            logger.error(f"Corrupted standards index at {self.index_path}. Please rebuild the index.")
        except Exception as e:
            logger.error(f"Error loading standards index: {e}")

    def search(self, query: str, limit: int = 10) -> List[StandardDoc]:
        """
        Search for standards by name or path.
        
        Args:
            query: The search string (case-insensitive).
            limit: Maximum number of results to return.
            
        Returns:
            List of matching StandardDoc dictionaries.
        """
        query = query.lower()
        results: List[StandardDoc] = []
        
        for std in self.standards:
            # Safe access in case of malformed index entries
            name = std.get("name", "").lower()
            path = std.get("path", "").lower()
            
            if query in name or query in path:
                results.append(std)
                if len(results) >= limit:
                    break
                    
        return results

    def get_stats(self) -> Dict[str, Any]:
        return {
            "total_count": len(self.standards),
            "index_path": str(self.index_path)
        }

# Example usage
if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    lookup = StandardsLookup()
    
    results = lookup.search("API")
    print("\nSearch results for 'API':")
    for r in results:
        print(f" - {r.get('name')} ({r.get('path')})")
