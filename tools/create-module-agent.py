#!/usr/bin/env python3
"""
Create-Module-Agent Command

This script provides access to the /create-module-agent command.
"""

import sys
from pathlib import Path

# Add agent_os to path
agent_os_path = Path(__file__).parent / "agent_os"
sys.path.insert(0, str(agent_os_path))

from commands.create_module_agent import main

if __name__ == "__main__":
    sys.exit(main())