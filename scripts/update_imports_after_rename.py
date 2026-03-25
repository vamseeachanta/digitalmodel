#!/usr/bin/env python3
"""
ABOUTME: Update Python import statements after file/directory renaming
ABOUTME: Automatically fixes imports broken by filename sanitization (spaces→underscores)
"""

import ast
import sys
from pathlib import Path
from typing import Dict, Set, Tuple
import re


class ImportUpdater(ast.NodeTransformer):
    """AST transformer to update import statements"""

    def __init__(self, rename_map: Dict[str, str]):
        self.rename_map = rename_map
        self.updated = False

    def visit_Import(self, node: ast.Import):
        """Update 'import X' statements"""
        for alias in node.names:
            old_name = alias.name
            if old_name in self.rename_map:
                alias.name = self.rename_map[old_name]
                self.updated = True
        return node

    def visit_ImportFrom(self, node: ast.ImportFrom):
        """Update 'from X import Y' statements"""
        if node.module and node.module in self.rename_map:
            node.module = self.rename_map[node.module]
            self.updated = True
        return node


def build_rename_map(src_dir: Path) -> Dict[str, str]:
    """
    Build mapping of old import paths to new import paths.

    Examples:
        'digitalmodel.common.plate_buckling' → 'digitalmodel.analysis.plate_buckling'
        'plateBucklingCal_Bi-axial Plot' → 'plateBucklingCal_Bi_axial_Plot'
    """
    rename_map = {}

    # Common pattern: spaces/commas → underscores
    patterns = [
        (r'\s+', '_'),  # spaces to underscores
        (r',', '_'),    # commas to underscores
        (r'__+', '_'),  # multiple underscores to single
    ]

    # Find all Python files and build potential old names
    for py_file in src_dir.rglob('*.py'):
        rel_path = py_file.relative_to(src_dir)
        module_parts = list(rel_path.parts[:-1]) + [rel_path.stem]

        # Build current import path
        current_path = '.'.join(module_parts)

        # Build potential old paths (with spaces/commas)
        # This is heuristic - won't catch all cases
        for part in module_parts:
            if '_' in part:
                # Try replacing underscores with spaces
                old_part_spaces = part.replace('_', ' ')
                old_path_spaces = current_path.replace(part, old_part_spaces)
                if old_path_spaces != current_path:
                    rename_map[old_path_spaces] = current_path

                # Try with commas
                old_part_comma = part.replace('_', ',')
                old_path_comma = current_path.replace(part, old_part_comma)
                if old_path_comma != current_path:
                    rename_map[old_path_comma] = current_path

    return rename_map


def update_imports_in_file(file_path: Path, rename_map: Dict[str, str]) -> bool:
    """
    Update imports in a single Python file.

    Returns True if file was modified.
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            source = f.read()

        # Parse the file
        try:
            tree = ast.parse(source, filename=str(file_path))
        except SyntaxError:
            print(f"⚠️  Syntax error in {file_path}, skipping")
            return False

        # Update imports
        updater = ImportUpdater(rename_map)
        new_tree = updater.visit(tree)

        if updater.updated:
            # Convert AST back to source
            # Note: This loses formatting, so we'll use regex instead
            # for better preservation of code style
            return update_imports_with_regex(file_path, rename_map)

        return False

    except Exception as e:
        print(f"❌ Error processing {file_path}: {e}")
        return False


def update_imports_with_regex(file_path: Path, rename_map: Dict[str, str]) -> bool:
    """
    Update imports using regex (preserves formatting better than AST).
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()

        original_content = content
        updated = False

        # Update each import pattern
        for old_import, new_import in rename_map.items():
            # Pattern 1: import old_module
            pattern1 = rf'\bimport\s+{re.escape(old_import)}\b'
            if re.search(pattern1, content):
                content = re.sub(pattern1, f'import {new_import}', content)
                updated = True

            # Pattern 2: from old_module import X
            pattern2 = rf'\bfrom\s+{re.escape(old_import)}\s+import\b'
            if re.search(pattern2, content):
                content = re.sub(pattern2, f'from {new_import} import', content)
                updated = True

            # Pattern 3: import old_module as X
            pattern3 = rf'\bimport\s+{re.escape(old_import)}\s+as\b'
            if re.search(pattern3, content):
                content = re.sub(pattern3, f'import {new_import} as', content)
                updated = True

        if updated and content != original_content:
            # Write back
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(content)
            return True

        return False

    except Exception as e:
        print(f"❌ Error updating {file_path}: {e}")
        return False


def main():
    """Main execution"""
    print("╔════════════════════════════════════════════════════════════╗")
    print("║     digitalmodel - Import Update Utility                  ║")
    print("╚════════════════════════════════════════════════════════════╝")
    print()

    # Determine source directory
    repo_root = Path(__file__).parent.parent
    src_dir = repo_root / 'src'

    if not src_dir.exists():
        print(f"❌ Source directory not found: {src_dir}")
        sys.exit(1)

    print(f"📁 Scanning: {src_dir}")
    print()

    # Build rename mapping
    print("🔍 Building rename mapping...")
    rename_map = build_rename_map(src_dir)

    if not rename_map:
        print("✅ No import renames detected. All imports should be up to date.")
        return

    print(f"📋 Found {len(rename_map)} potential import renames:")
    for old, new in list(rename_map.items())[:5]:
        print(f"  {old} → {new}")
    if len(rename_map) > 5:
        print(f"  ... and {len(rename_map) - 5} more")
    print()

    # Find all Python files
    py_files = list(src_dir.rglob('*.py'))
    print(f"📝 Scanning {len(py_files)} Python files for import updates...")
    print()

    # Update imports
    updated_files = []
    for py_file in py_files:
        if update_imports_with_regex(py_file, rename_map):
            updated_files.append(py_file)
            print(f"✓ Updated: {py_file.relative_to(repo_root)}")

    print()
    print("═══════════════════════════════════════════════════════════")
    print(f"✅ Import update complete!")
    print()
    print(f"Updated {len(updated_files)} files")
    print()

    if updated_files:
        print("📝 NEXT STEPS:")
        print()
        print("1. Review the changes:")
        print("   git diff src/")
        print()
        print("2. Run tests to verify:")
        print("   pytest tests/ -v")
        print()
        print("3. If tests pass, commit:")
        print("   git add -A")
        print("   git commit -m 'Fix: Update imports after file renaming'")
        print()
    else:
        print("ℹ️  No imports needed updating (either already correct or no matches found)")
        print()


if __name__ == '__main__':
    main()
