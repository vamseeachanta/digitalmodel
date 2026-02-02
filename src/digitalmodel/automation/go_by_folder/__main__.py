#!/usr/bin/env python
"""
Direct module execution for Create Go-By Folder Tool

Usage:
    python -m digitalmodel.automation.go_by_folder [args]
    
This allows the module to be run directly without installation.
"""

import sys
from .cli import main

if __name__ == "__main__":
    sys.exit(main())