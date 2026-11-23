#!/usr/bin/env python3
"""Manage web resources for module agents."""

import sys
import argparse
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from agent_os.commands.web_resource_manager import WebResourceManager

def main():
    parser = argparse.ArgumentParser(description="Manage agent web resources")
    parser.add_argument("action", choices=["add-link", "review"],
                       help="Action to perform")
    parser.add_argument("module", help="Module name")
    parser.add_argument("--url", help="URL for add-link action")
    parser.add_argument("--notes", help="Notes for added link")
    
    args = parser.parse_args()
    
    # Find agent directory
    agent_dir = Path.cwd() / "agents" / args.module
    if not agent_dir.exists():
        print(f"Error: Agent directory not found: {agent_dir}")
        return 1
    
    manager = WebResourceManager(agent_dir)
    
    if args.action == "add-link":
        if not args.url:
            print("Error: --url required for add-link")
            return 1
        result = manager.add_user_link(args.url, args.notes or "")
        print(f"Added link: {result['url']}")
    
    elif args.action == "review":
        report = manager.generate_review_report()
        print(report)
        # Also save to file
        review_file = agent_dir / "context" / "web_resources_review.md"
        with open(review_file, 'w') as f:
            f.write(report)
        print(f"\nReport saved to: {review_file}")
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
