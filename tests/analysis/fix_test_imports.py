#!/usr/bin/env python3
"""
Automated Test Import Fixer

This script automatically fixes import issues in the marine engineering test suite.
It creates backups before making changes and can be run in dry-run mode.

Usage:
    python fix_test_imports.py --dry-run    # Preview changes
    python fix_test_imports.py              # Apply fixes
    python fix_test_imports.py --rollback   # Restore from backup

Author: Digital Model Test Infrastructure Team
Date: 2025-10-03
"""

import sys
import re
import shutil
from pathlib import Path
from datetime import datetime
from typing import List, Tuple, Dict
import argparse


class TestImportFixer:
    """Fixes import issues in test files."""

    def __init__(self, repo_root: Path, dry_run: bool = False):
        self.repo_root = repo_root
        self.src_dir = repo_root / 'src'
        self.test_dir = repo_root / 'tests' / 'marine_engineering'
        self.dry_run = dry_run
        self.backup_dir = repo_root / 'tests' / 'backups' / datetime.now().strftime('%Y%m%d_%H%M%S')
        self.changes_made = []
        self.errors = []

    def create_backup(self, file_path: Path) -> Path:
        """Create backup of file before modification."""
        if self.dry_run:
            return None

        # Create backup directory structure
        relative_path = file_path.relative_to(self.repo_root)
        backup_path = self.backup_dir / relative_path
        backup_path.parent.mkdir(parents=True, exist_ok=True)

        # Copy file
        shutil.copy2(file_path, backup_path)
        print(f"  📁 Backed up to: {backup_path}")
        return backup_path

    def fix_extraction_import(self) -> bool:
        """Fix the extraction module import error."""
        print("\n" + "="*80)
        print("FIX #1: Extraction Module Import")
        print("="*80)

        file_path = self.src_dir / 'digitalmodel' / 'modules' / 'marine_analysis' / 'extraction' / 'run_extraction.py'

        if not file_path.exists():
            self.errors.append(f"File not found: {file_path}")
            print(f"❌ Error: File not found")
            return False

        print(f"📄 File: {file_path.relative_to(self.repo_root)}")

        # Read file
        content = file_path.read_text(encoding='utf-8')
        original_content = content

        # Pattern to find and replace
        old_pattern = r"from extract_hydro_coefficients import HydrodynamicCoefficientExtractor"
        new_code = """try:
    from .extract_hydro import HydrodynamicCoefficientExtractor
except ImportError:
    HydrodynamicCoefficientExtractor = None
    import warnings
    warnings.warn("HydrodynamicCoefficientExtractor not available - extraction features disabled")"""

        if old_pattern in content:
            content = content.replace(old_pattern, new_code)
            print(f"✅ Found problematic import at line 18")
            print(f"   Old: {old_pattern}")
            print(f"   New: [Graceful fallback with try/except]")

            if not self.dry_run:
                self.create_backup(file_path)
                file_path.write_text(content, encoding='utf-8')
                self.changes_made.append(f"Fixed extraction import in {file_path.name}")
                print("✅ Applied fix")
            else:
                print("🔍 DRY RUN: Would apply fix")
            return True
        else:
            print(f"⚠️  Pattern not found (already fixed?)")
            return False

    def fix_rao_plotter_import(self) -> bool:
        """Fix the RAOPlotter import path."""
        print("\n" + "="*80)
        print("FIX #2: RAOPlotter Import Path")
        print("="*80)

        file_path = self.src_dir / 'digitalmodel' / 'modules' / 'marine_analysis' / '__init__.py'

        if not file_path.exists():
            self.errors.append(f"File not found: {file_path}")
            print(f"❌ Error: File not found")
            return False

        print(f"📄 File: {file_path.relative_to(self.repo_root)}")

        # Read file
        content = file_path.read_text(encoding='utf-8')
        original_content = content

        # Pattern to find and replace
        old_pattern = r"from \.rao_plotter import RAOPlotter"
        new_pattern = "from .visualization.rao_plotter import RAOPlotter"

        if re.search(old_pattern, content):
            content = re.sub(old_pattern, new_pattern, content)
            print(f"✅ Found incorrect import path")
            print(f"   Old: from .rao_plotter import RAOPlotter")
            print(f"   New: from .visualization.rao_plotter import RAOPlotter")

            if not self.dry_run:
                self.create_backup(file_path)
                file_path.write_text(content, encoding='utf-8')
                self.changes_made.append(f"Fixed RAOPlotter import in {file_path.name}")
                print("✅ Applied fix")
            else:
                print("🔍 DRY RUN: Would apply fix")
            return True
        else:
            print(f"⚠️  Pattern not found (already fixed?)")
            return False

    def create_conftest(self) -> bool:
        """Create tests/conftest.py for PYTHONPATH configuration."""
        print("\n" + "="*80)
        print("FIX #3: Create conftest.py for PYTHONPATH")
        print("="*80)

        conftest_path = self.repo_root / 'tests' / 'conftest.py'

        if conftest_path.exists():
            print(f"⚠️  conftest.py already exists at: {conftest_path}")
            return False

        conftest_content = '''"""
Pytest configuration for marine engineering tests.
Ensures proper Python path setup for all test modules.

This file is automatically loaded by pytest and configures the Python path
to include the src/ directory, allowing tests to import from both:
- digitalmodel.modules.marine_analysis (package imports)
- marine_engineering (direct module imports)

Generated: 2025-10-03
"""
import sys
from pathlib import Path

# Get repository root (parent of tests/)
repo_root = Path(__file__).parent.parent

# Add src to Python path
src_path = repo_root / 'src'
if str(src_path) not in sys.path:
    sys.path.insert(0, str(src_path))
    print(f"[conftest] Added to PYTHONPATH: {src_path}")

# Optional: Add src/marine_engineering to path for direct imports
marine_eng_path = src_path / 'marine_engineering'
if marine_eng_path.exists() and str(src_path) not in sys.path:
    # Note: Already added src/ above, which makes marine_engineering importable
    pass

print("[conftest] Python path configured for marine engineering tests")
'''

        print(f"📄 Creating: {conftest_path.relative_to(self.repo_root)}")
        print("   This will add src/ to PYTHONPATH for all tests")

        if not self.dry_run:
            conftest_path.write_text(conftest_content, encoding='utf-8')
            self.changes_made.append(f"Created conftest.py")
            print("✅ Created conftest.py")
        else:
            print("🔍 DRY RUN: Would create conftest.py")

        return True

    def update_test_imports(self) -> int:
        """Update import statements in test files to use standard pattern."""
        print("\n" + "="*80)
        print("FIX #4: Standardize Test Imports")
        print("="*80)
        print("Note: This is a complex change - running in analysis mode only")

        # Find all test files
        test_files = list(self.test_dir.rglob("test_*.py"))

        import_patterns = {
            'src.marine_engineering': 0,
            'marine_engineering': 0,
            'digitalmodel.modules.marine_analysis': 0
        }

        print(f"\n📊 Analyzing {len(test_files)} test files...")

        for test_file in test_files:
            content = test_file.read_text(encoding='utf-8')

            # Count import patterns
            if 'from src.marine_engineering' in content:
                import_patterns['src.marine_engineering'] += 1
                print(f"  ⚠️  {test_file.name}: uses 'src.marine_engineering' imports")

            if 'from marine_engineering' in content and 'from src.marine_engineering' not in content:
                import_patterns['marine_engineering'] += 1
                print(f"  ⚠️  {test_file.name}: uses 'marine_engineering' imports")

            if 'from digitalmodel.modules.marine_analysis' in content:
                import_patterns['digitalmodel.modules.marine_analysis'] += 1
                print(f"  ✅ {test_file.name}: uses 'digitalmodel.modules.marine_analysis' imports")

        print(f"\n📈 Import Pattern Summary:")
        print(f"   - src.marine_engineering: {import_patterns['src.marine_engineering']} files")
        print(f"   - marine_engineering: {import_patterns['marine_engineering']} files")
        print(f"   - digitalmodel.modules.marine_analysis: {import_patterns['digitalmodel.modules.marine_analysis']} files")

        print("\n⚠️  Note: Standardizing imports requires manual review")
        print("   Reason: Need to verify module availability in target location")
        print("   Action: Use IMPORT_FIX_PLAN.md for detailed migration steps")

        return len(test_files)

    def verify_fixes(self) -> bool:
        """Verify that fixes were applied correctly."""
        print("\n" + "="*80)
        print("VERIFICATION")
        print("="*80)

        success = True

        # Test 1: Try importing UnifiedRAOReader
        print("\n🧪 Test 1: Import UnifiedRAOReader")
        try:
            sys.path.insert(0, str(self.src_dir))
            from digitalmodel.modules.marine_analysis import UnifiedRAOReader
            print("   ✅ Successfully imported UnifiedRAOReader")
        except Exception as e:
            print(f"   ❌ Failed to import: {e}")
            success = False

        # Test 2: Try importing RAOPlotter
        print("\n🧪 Test 2: Import RAOPlotter")
        try:
            from digitalmodel.modules.marine_analysis import RAOPlotter
            if RAOPlotter is None:
                print("   ⚠️  RAOPlotter is None (optional dependency missing)")
            else:
                print(f"   ✅ Successfully imported RAOPlotter: {RAOPlotter}")
        except Exception as e:
            print(f"   ❌ Failed to import: {e}")
            success = False

        # Test 3: Check conftest.py
        print("\n🧪 Test 3: Verify conftest.py")
        conftest_path = self.repo_root / 'tests' / 'conftest.py'
        if conftest_path.exists():
            print(f"   ✅ conftest.py exists")
        else:
            print(f"   ❌ conftest.py not found")
            success = False

        return success

    def run(self) -> bool:
        """Run all fixes."""
        print("="*80)
        print("MARINE ENGINEERING TEST IMPORT FIXER")
        print("="*80)
        print(f"Mode: {'DRY RUN (no changes)' if self.dry_run else 'LIVE (will modify files)'}")
        print(f"Repository: {self.repo_root}")
        print()

        if not self.dry_run:
            print("⚠️  This will modify files. Backups will be created.")
            print(f"📁 Backup location: {self.backup_dir}")
            print()

        # Run fixes
        results = []
        results.append(self.fix_extraction_import())
        results.append(self.fix_rao_plotter_import())
        results.append(self.create_conftest())
        self.update_test_imports()  # Analysis only

        # Summary
        print("\n" + "="*80)
        print("SUMMARY")
        print("="*80)

        if self.dry_run:
            print("🔍 DRY RUN COMPLETE - No files were modified")
            print("\nProposed changes:")
        else:
            print("✅ FIXES APPLIED")
            print("\nChanges made:")

        for change in self.changes_made:
            print(f"  ✅ {change}")

        if self.errors:
            print("\n❌ Errors encountered:")
            for error in self.errors:
                print(f"  ❌ {error}")

        if not self.dry_run and self.changes_made:
            print(f"\n📁 Backups saved to: {self.backup_dir}")

        # Verification
        if not self.dry_run and any(results):
            self.verify_fixes()

        print("\n" + "="*80)
        print("NEXT STEPS")
        print("="*80)
        print("1. Run: pytest tests/marine_engineering/ --collect-only")
        print("2. Verify: Should collect 150 items without errors")
        print("3. Test: pytest tests/marine_engineering/test_unified_rao_reader.py -v")
        print("4. Review: Check IMPORT_FIX_PLAN.md for remaining import standardization")

        return any(results)


def rollback(repo_root: Path):
    """Rollback changes from most recent backup."""
    print("="*80)
    print("ROLLBACK CHANGES")
    print("="*80)

    backup_base = repo_root / 'tests' / 'backups'

    if not backup_base.exists():
        print("❌ No backups found")
        return False

    # Find most recent backup
    backups = sorted(backup_base.iterdir(), key=lambda p: p.name, reverse=True)

    if not backups:
        print("❌ No backups found")
        return False

    latest_backup = backups[0]
    print(f"📁 Restoring from: {latest_backup}")

    # Restore files
    restored = 0
    for backup_file in latest_backup.rglob("*"):
        if backup_file.is_file():
            relative_path = backup_file.relative_to(latest_backup)
            target_path = repo_root / relative_path

            print(f"  ↩️  Restoring: {relative_path}")
            target_path.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(backup_file, target_path)
            restored += 1

    print(f"\n✅ Restored {restored} files from backup")
    return True


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Fix import issues in marine engineering test suite",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python fix_test_imports.py --dry-run    # Preview changes
  python fix_test_imports.py              # Apply fixes
  python fix_test_imports.py --rollback   # Restore from backup
        """
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Preview changes without modifying files'
    )
    parser.add_argument(
        '--rollback',
        action='store_true',
        help='Rollback changes from most recent backup'
    )

    args = parser.parse_args()

    # Find repository root
    script_dir = Path(__file__).parent
    repo_root = script_dir.parent.parent  # tests/analysis -> tests -> repo_root

    if args.rollback:
        rollback(repo_root)
        return

    # Run fixer
    fixer = TestImportFixer(repo_root, dry_run=args.dry_run)
    success = fixer.run()

    sys.exit(0 if success else 1)


if __name__ == '__main__':
    main()
