#!/usr/bin/env python
"""
Migration Script: Update codebase to use new signal_analysis module

This script identifies and helps migrate old rainflow/FFT implementations
to use the new consolidated signal_analysis module.
"""

import os
import re
from pathlib import Path
from typing import List, Dict, Tuple
import argparse


class SignalAnalysisMigrator:
    """Migrate old signal processing code to new module"""
    
    def __init__(self, dry_run: bool = True):
        self.dry_run = dry_run
        self.migration_map = self._build_migration_map()
        self.files_to_update = []
        self.migration_report = []
        
    def _build_migration_map(self) -> Dict[str, str]:
        """Build mapping from old to new implementations"""
        return {
            # Old imports to new imports
            'from digitalmodel.signal_analysis.adapters import TimeSeriesComponentsAdapter as TimeSeriesComponents': 
                'from digitalmodel.signal_analysis.adapters import TimeSeriesComponentsAdapter as TimeSeriesComponents',
            
            'from digitalmodel.signal_analysis.fatigue import': 
                'from digitalmodel.signal_analysis.fatigue import',
            
            'from digitalmodel.signal_analysis import': 
                'from digitalmodel.signal_analysis import',
            
            # Old class names to new
            'TimeSeriesComponentsAdapter(': 'TimeSeriesComponentsAdapter(',
            
            # Old method calls to new
            'count_cycles': 'count_cycles',
            'window_averaged_fft': 'window_averaged_fft',
            'filter_spectrum': 'filter_spectrum',
            
            # Update configuration keys
            '"signal_analysis"': '"signal_analysis"',
            "'signal_analysis'": "'signal_analysis'",
        }
    
    def scan_codebase(self, root_dir: Path) -> List[Path]:
        """Scan codebase for files needing migration"""
        files_to_check = []
        patterns = [
            'TimeSeriesComponents',
            'count_cycles',
            'window_averaged_fft',
            'fatigue_analysis',
            'signal_analysis'
        ]
        
        for root, dirs, files in os.walk(root_dir):
            # Skip test files for signal_analysis module itself
            if 'signal_analysis' in root:
                continue
                
            # Skip .git and other hidden directories
            dirs[:] = [d for d in dirs if not d.startswith('.')]
            
            for file in files:
                if file.endswith('.py'):
                    file_path = Path(root) / file
                    content = file_path.read_text(encoding='utf-8', errors='ignore')
                    
                    # Check if file contains old patterns
                    if any(pattern in content for pattern in patterns):
                        files_to_check.append(file_path)
        
        return files_to_check
    
    def analyze_file(self, file_path: Path) -> Dict:
        """Analyze a file for migration needs"""
        content = file_path.read_text(encoding='utf-8')
        analysis = {
            'file': file_path,
            'changes_needed': [],
            'line_numbers': []
        }
        
        lines = content.split('\n')
        for i, line in enumerate(lines, 1):
            for old_pattern in self.migration_map.keys():
                if old_pattern in line:
                    analysis['changes_needed'].append({
                        'line': i,
                        'old': old_pattern,
                        'new': self.migration_map[old_pattern],
                        'context': line.strip()
                    })
                    analysis['line_numbers'].append(i)
        
        return analysis
    
    def generate_migration_report(self, analyses: List[Dict]) -> str:
        """Generate detailed migration report"""
        report = []
        report.append("# Signal Analysis Module Migration Report\n")
        report.append(f"Found {len(analyses)} files requiring migration\n")
        
        # Summary
        total_changes = sum(len(a['changes_needed']) for a in analyses)
        report.append(f"Total changes required: {total_changes}\n")
        
        # Group by type
        import_changes = 0
        method_changes = 0
        config_changes = 0
        
        for analysis in analyses:
            for change in analysis['changes_needed']:
                if 'import' in change['old']:
                    import_changes += 1
                elif '(' in change['old']:
                    method_changes += 1
                else:
                    config_changes += 1
        
        report.append("\n## Change Summary")
        report.append(f"- Import statements: {import_changes}")
        report.append(f"- Method calls: {method_changes}")
        report.append(f"- Configuration keys: {config_changes}")
        
        # Detailed changes by file
        report.append("\n## Files Requiring Migration\n")
        
        for analysis in analyses:
            if analysis['changes_needed']:
                report.append(f"\n### {analysis['file']}")
                report.append(f"Lines to update: {', '.join(map(str, analysis['line_numbers']))}")
                
                for change in analysis['changes_needed']:
                    report.append(f"\nLine {change['line']}:")
                    report.append(f"  Old: `{change['old']}`")
                    report.append(f"  New: `{change['new']}`")
                    report.append(f"  Context: {change['context']}")
        
        # Migration instructions
        report.append("\n## Migration Instructions\n")
        report.append("1. **Backup your code** before running migration")
        report.append("2. **Review changes** in the report above")
        report.append("3. **Run migration** with `--apply` flag to make changes")
        report.append("4. **Test thoroughly** after migration")
        report.append("5. **Update configurations** in YAML/JSON files")
        
        # Code examples
        report.append("\n## Example Migrations\n")
        report.append("### Old Code:")
        report.append("```python")
        report.append("from digitalmodel.signal_analysis.adapters import TimeSeriesComponentsAdapter as TimeSeriesComponents")
        report.append("tsc = TimeSeriesComponentsAdapter(cfg)")
        report.append("cycles = tsc.count_cycles(signal)")
        report.append("```")
        
        report.append("\n### New Code:")
        report.append("```python")
        report.append("from digitalmodel.signal_analysis import RainflowCounter")
        report.append("counter = RainflowCounter()")
        report.append("cycles = counter.count_cycles(signal)")
        report.append("```")
        
        report.append("\n### Or using adapter for compatibility:")
        report.append("```python")
        report.append("from digitalmodel.signal_analysis.adapters import TimeSeriesComponentsAdapter")
        report.append("adapter = TimeSeriesComponentsAdapter(cfg)")
        report.append("cycles = adapter.count_cycles(signal)")
        report.append("```")
        
        return '\n'.join(report)
    
    def apply_migrations(self, analyses: List[Dict]) -> List[str]:
        """Apply migrations to files"""
        results = []
        
        for analysis in analyses:
            if not analysis['changes_needed']:
                continue
            
            file_path = analysis['file']
            content = file_path.read_text(encoding='utf-8')
            original_content = content
            
            # Apply replacements
            for old_pattern, new_pattern in self.migration_map.items():
                if old_pattern in content:
                    content = content.replace(old_pattern, new_pattern)
            
            if content != original_content:
                if not self.dry_run:
                    # Backup original
                    backup_path = file_path.with_suffix('.py.bak')
                    backup_path.write_text(original_content, encoding='utf-8')
                    
                    # Write updated content
                    file_path.write_text(content, encoding='utf-8')
                    results.append(f"✓ Updated: {file_path}")
                else:
                    results.append(f"Would update: {file_path}")
            else:
                results.append(f"No changes needed: {file_path}")
        
        return results
    
    def add_deprecation_warnings(self, file_path: Path):
        """Add deprecation warnings to old modules"""
        warning_text = '''"""
DEPRECATED: This module is being replaced by digitalmodel.signal_analysis

Please migrate to the new signal analysis module:
- RainflowCounter for rainflow counting
- SpectralAnalyzer for FFT/spectral analysis
- FatigueDamageCalculator for fatigue calculations

See migration guide: docs/migration/signal_analysis_migration.md
"""

import warnings
warnings.warn(
    "This module is deprecated. Use digitalmodel.signal_analysis instead",
    DeprecationWarning,
    stacklevel=2
)

'''
        content = file_path.read_text(encoding='utf-8')
        if 'DEPRECATED' not in content:
            content = warning_text + content
            if not self.dry_run:
                file_path.write_text(content, encoding='utf-8')


def main():
    parser = argparse.ArgumentParser(
        description='Migrate codebase to use new signal_analysis module'
    )
    parser.add_argument(
        '--scan', 
        action='store_true',
        help='Scan codebase for files needing migration'
    )
    parser.add_argument(
        '--analyze',
        action='store_true', 
        help='Analyze files and generate migration report'
    )
    parser.add_argument(
        '--apply',
        action='store_true',
        help='Apply migrations (use with caution!)'
    )
    parser.add_argument(
        '--root',
        type=str,
        default='.',
        help='Root directory to scan (default: current directory)'
    )
    parser.add_argument(
        '--output',
        type=str,
        default='migration_report.md',
        help='Output file for migration report'
    )
    
    args = parser.parse_args()
    
    # Initialize migrator
    migrator = SignalAnalysisMigrator(dry_run=not args.apply)
    root_dir = Path(args.root)
    
    print("Signal Analysis Module Migration Tool")
    print("="*50)
    
    if args.scan or args.analyze or args.apply:
        # Scan codebase
        print(f"\nScanning {root_dir}...")
        files = migrator.scan_codebase(root_dir)
        print(f"Found {len(files)} files with old signal processing code")
        
        if files:
            print("\nFiles to migrate:")
            for f in files:
                print(f"  - {f}")
    
    if args.analyze or args.apply:
        # Analyze files
        print("\nAnalyzing files...")
        analyses = []
        for file_path in files:
            analysis = migrator.analyze_file(file_path)
            if analysis['changes_needed']:
                analyses.append(analysis)
        
        # Generate report
        report = migrator.generate_migration_report(analyses)
        
        # Save report
        report_path = Path(args.output)
        report_path.write_text(report, encoding='utf-8')
        print(f"\nMigration report saved to: {report_path}")
        
        if args.apply:
            print("\nApplying migrations...")
            results = migrator.apply_migrations(analyses)
            for result in results:
                print(f"  {result}")
            
            if not migrator.dry_run:
                print("\n✓ Migrations applied successfully!")
                print("  Original files backed up with .bak extension")
                print("  Please test your code thoroughly")
        else:
            print("\nTo apply these changes, run with --apply flag")
            print("WARNING: This will modify your files! Make sure to backup first.")
    
    # Add deprecation warnings to old modules
    if args.apply:
        old_modules = [
            'src/digitalmodel/modules/time_series/time_series_components.py',
            'src/digitalmodel/time_series.py',
            'src/digitalmodel/common/fatigue_analysis.py'
        ]
        
        print("\nAdding deprecation warnings to old modules...")
        for module in old_modules:
            module_path = root_dir / module
            if module_path.exists():
                migrator.add_deprecation_warnings(module_path)
                print(f"  ✓ Added warning to {module}")


if __name__ == "__main__":
    main()