"""
Create Go-By Folder Tool

A tool for creating lightweight, representative "go-by" versions of existing folders.
Reduces folder size by >99% while preserving structure, patterns, and key examples.
"""

__version__ = "0.1.0"

from .cli import main as cli_main
from .scanner import FileScanner
from .analyzer import PatternAnalyzer
from .preservator import FilePreservator
from .orchestrator import CreateGoBy

__all__ = [
    "cli_main",
    "FileScanner",
    "PatternAnalyzer", 
    "FilePreservator",
    "CreateGoBy",
    "__version__"
]