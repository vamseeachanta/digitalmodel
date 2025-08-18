#!/usr/bin/env python3
"""
DEPRECATED: This command has been replaced by /git commit

This wrapper provides backward compatibility.
Please update your workflow to use: /git commit
"""

import sys
import subprocess

print("⚠️  DEPRECATED: Use '/git commit' instead")
print("   Redirecting to new command...\n")

# Convert old command to new format
args = sys.argv[1:]
new_args = ["/git"]

# Add subcommand based on old command name
if "sync-all" in "git-commit-push-merge-all":
    new_args.extend(["sync", "--all"])
elif "sync" in "git-commit-push-merge-all":
    new_args.append("sync")
elif "trunk-flow" in "git-commit-push-merge-all":
    new_args.append("trunk")
elif "trunk-status" in "git-commit-push-merge-all":
    new_args.append("status")
elif "commit" in "git-commit-push-merge-all":
    new_args.append("commit")
    new_args.extend(args)  # Pass commit message

# Execute new command
cmd = [sys.executable, "/mnt/github/github/.agent-os/commands/git.py"] + new_args[1:]
subprocess.run(cmd)
