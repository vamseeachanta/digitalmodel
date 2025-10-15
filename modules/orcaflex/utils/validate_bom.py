#!/usr/bin/env python3
"""
OrcaFlex File BOM Validator

Validates that YAML files have proper UTF-8 BOM encoding for OrcaFlex compatibility.
"""

import os
import sys
from pathlib import Path
from typing import List, Tuple


class BOMValidator:
    """Validates UTF-8 BOM in files for OrcaFlex compatibility."""

    SINGLE_BOM = b'\xef\xbb\xbf'
    DOUBLE_BOM = b'\xef\xbb\xbf\xef\xbb\xbf'

    @staticmethod
    def check_file(filepath: str) -> Tuple[bool, str]:
        """
        Check if file has proper BOM.

        Args:
            filepath: Path to file to check

        Returns:
            Tuple of (is_valid, message)
        """
        try:
            with open(filepath, 'rb') as f:
                first_bytes = f.read(6)

            if first_bytes.startswith(BOMValidator.DOUBLE_BOM):
                return False, "DOUBLE BOM detected (ef bb bf ef bb bf)"
            elif first_bytes.startswith(BOMValidator.SINGLE_BOM):
                return True, "Valid single BOM (ef bb bf)"
            else:
                return False, f"Missing BOM (starts with: {first_bytes[:3].hex()})"

        except Exception as e:
            return False, f"Error reading file: {e}"

    @staticmethod
    def fix_double_bom(filepath: str) -> bool:
        """
        Remove double BOM from file, keeping only one.

        Args:
            filepath: Path to file to fix

        Returns:
            True if fixed, False if no fix needed or error
        """
        try:
            with open(filepath, 'rb') as f:
                content = f.read()

            if content.startswith(BOMValidator.DOUBLE_BOM):
                # Remove first BOM, keep second
                content = content[3:]

                with open(filepath, 'wb') as f:
                    f.write(content)
                return True

            return False

        except Exception as e:
            print(f"Error fixing {filepath}: {e}", file=sys.stderr)
            return False

    @staticmethod
    def add_bom(filepath: str) -> bool:
        """
        Add UTF-8 BOM to file if missing.

        Args:
            filepath: Path to file

        Returns:
            True if BOM added, False if already present or error
        """
        try:
            with open(filepath, 'rb') as f:
                content = f.read()

            if not content.startswith(BOMValidator.SINGLE_BOM):
                # Add BOM
                content = BOMValidator.SINGLE_BOM + content

                with open(filepath, 'wb') as f:
                    f.write(content)
                return True

            return False

        except Exception as e:
            print(f"Error adding BOM to {filepath}: {e}", file=sys.stderr)
            return False


def validate_directory(directory: str, pattern: str = "*.yml", fix: bool = False) -> None:
    """
    Validate all YAML files in directory.

    Args:
        directory: Directory to scan
        pattern: File pattern to match
        fix: If True, attempt to fix issues
    """
    path = Path(directory)
    files = list(path.glob(pattern))

    if not files:
        print(f"No files matching '{pattern}' found in {directory}")
        return

    print(f"\nValidating {len(files)} files in {directory}\n")
    print("-" * 80)

    valid_count = 0
    invalid_count = 0
    fixed_count = 0

    for filepath in sorted(files):
        is_valid, message = BOMValidator.check_file(str(filepath))

        status = "✓" if is_valid else "✗"
        print(f"{status} {filepath.name:50} {message}")

        if is_valid:
            valid_count += 1
        else:
            invalid_count += 1

            if fix:
                if "DOUBLE BOM" in message:
                    if BOMValidator.fix_double_bom(str(filepath)):
                        print(f"  → Fixed double BOM")
                        fixed_count += 1
                elif "Missing BOM" in message:
                    if BOMValidator.add_bom(str(filepath)):
                        print(f"  → Added BOM")
                        fixed_count += 1

    print("-" * 80)
    print(f"\nSummary:")
    print(f"  Valid:   {valid_count}")
    print(f"  Invalid: {invalid_count}")
    if fix:
        print(f"  Fixed:   {fixed_count}")


def main():
    """Main entry point."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Validate UTF-8 BOM in OrcaFlex YAML files"
    )
    parser.add_argument(
        "directory",
        help="Directory containing YAML files to validate"
    )
    parser.add_argument(
        "--pattern",
        default="*.yml",
        help="File pattern to match (default: *.yml)"
    )
    parser.add_argument(
        "--fix",
        action="store_true",
        help="Attempt to fix BOM issues"
    )

    args = parser.parse_args()

    if not os.path.isdir(args.directory):
        print(f"Error: {args.directory} is not a directory", file=sys.stderr)
        sys.exit(1)

    validate_directory(args.directory, args.pattern, args.fix)


if __name__ == "__main__":
    main()
