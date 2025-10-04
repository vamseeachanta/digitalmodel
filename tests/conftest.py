"""Pytest configuration for marine engineering tests."""
import sys
from pathlib import Path

# Add src/ to Python path
repo_root = Path(__file__).parent.parent
src_path = repo_root / "src"
if str(src_path) not in sys.path:
    sys.path.insert(0, str(src_path))
