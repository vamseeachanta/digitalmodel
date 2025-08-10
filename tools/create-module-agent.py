#!/usr/bin/env python3
"""
Create-Module-Agent Command for digitalmodel

This script provides access to the /create-module-agent command.
"""

import sys
from pathlib import Path

# Add parent directory to path to find agent_os
sys.path.insert(0, str(Path(__file__).parent.parent))

from agent_os.commands.create_module_agent import main

if __name__ == "__main__":
    sys.exit(main())
