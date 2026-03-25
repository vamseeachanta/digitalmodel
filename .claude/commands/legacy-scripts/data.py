#!/usr/bin/env python3
"""
Simplified Data Command - Engineering data management

Replaces: engineering-data-context
"""

import sys
import subprocess
import argparse
from pathlib import Path

def main():
    parser = argparse.ArgumentParser(prog='data', add_help=False)
    parser.add_argument('subcommand', nargs='?', default='help',
                       choices=['scan', 'context', 'research', 'query', 'help'])
    parser.add_argument('target', nargs='?')
    parser.add_argument('--topics', nargs='+')
    
    args = parser.parse_args()
    
    base_cmd = [sys.executable, str(Path(__file__).parent / "engineering_data_context.py")]
    
    if args.subcommand == 'scan' or args.subcommand == 'context':
        cmd = base_cmd + ["generate", "--folder", args.target or "."]
        if args.subcommand == 'context':
            cmd.append("--deep-research")
    elif args.subcommand == 'research':
        cmd = base_cmd + ["enhance", "--folder", ".", "--research-topics"] + (args.topics or [])
    elif args.subcommand == 'query':
        cmd = base_cmd + ["query", "--context", args.target or ""]
    else:
        print("""
📊 Data Command

Usage: /data [subcommand] [options]

Subcommands:
  scan FOLDER      Scan for engineering data
  context FOLDER   Generate context with research
  research TOPICS  Add research on topics
  query "TERM"     Query data context
  help            Show this help

Examples:
  /data scan ./measurements
  /data context ./data
  /data research "sensor calibration" "API docs"
  /data query "temperature sensor"
""")
        return
    
    subprocess.run(cmd)

if __name__ == '__main__':
    main()
