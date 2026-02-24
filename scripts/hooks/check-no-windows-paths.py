#!/usr/bin/env python3
"""Pre-commit guard: reject staged files with Windows-path directory names.

Catches artifacts like 'D:\\workspace-hub\\...' that are created when a
Windows-environment script writes a path string as a directory name.
"""
import re
import subprocess
import sys

result = subprocess.run(
    ["git", "diff", "--cached", "--name-only"],
    capture_output=True,
    text=True,
)

# Match Windows drive-letter prefix (C:, D:, etc.) or backslash in path
pattern = re.compile(r"^[A-Za-z]:|\\")
bad = [p for p in result.stdout.splitlines() if pattern.search(p)]

if bad:
    print("ERROR: Windows-path directory artifacts detected in staged files:")
    for p in bad:
        print(f"  {p}")
    print()
    print("These are Windows-style path strings used as directory names.")
    print("Fix: rm -rf \"<path>\" and remove from git index before committing.")
    sys.exit(1)

sys.exit(0)
