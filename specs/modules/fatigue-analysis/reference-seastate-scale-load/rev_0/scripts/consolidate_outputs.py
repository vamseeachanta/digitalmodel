#!/usr/bin/env python3
"""
Consolidate multiple output directories into a single organized structure.
"""
import os
import shutil
from pathlib import Path
import json
from datetime import datetime


def consolidate_outputs():
    """Consolidate all output directories into a single organized structure."""
    
    base_path = Path(__file__).parent
    
    # Define old directories and their new locations
    migration_map = {
        'output_auto_verify': 'output/verification/automated',
        'output_benchmark': 'output/benchmarks',
        'output_progress_test': 'output/testing',
        'output_step5_intermediate': 'output/verification/intermediate',
        'output_step_verify': 'output/verification/step_by_step',
        'output_verification': 'output/verification/general'
    }
    
    # Create migration report
    report = {
        'timestamp': datetime.now().isoformat(),
        'directories_processed': [],
        'files_moved': [],
        'errors': []
    }
    
    # Create new directory structure
    for new_path in migration_map.values():
        full_path = base_path / new_path
        full_path.mkdir(parents=True, exist_ok=True)
        print(f"Created: {new_path}")
    
    # Move files from old directories to new locations
    for old_dir, new_dir in migration_map.items():
        old_path = base_path / old_dir
        new_path = base_path / new_dir
        
        if old_path.exists():
            print(f"\nProcessing: {old_dir}")
            
            # Count files
            files = list(old_path.glob('**/*'))
            file_count = sum(1 for f in files if f.is_file())
            
            if file_count > 0:
                print(f"  Found {file_count} files")
                
                # Move all contents
                for item in old_path.iterdir():
                    if item.is_file():
                        dest = new_path / item.name
                        if not dest.exists():
                            shutil.copy2(item, dest)
                            report['files_moved'].append({
                                'from': str(item.relative_to(base_path)),
                                'to': str(dest.relative_to(base_path))
                            })
                            print(f"  Moved: {item.name}")
                        else:
                            print(f"  Skipped (exists): {item.name}")
                    elif item.is_dir():
                        dest = new_path / item.name
                        if not dest.exists():
                            shutil.copytree(item, dest)
                            print(f"  Moved directory: {item.name}")
            else:
                print(f"  Directory is empty")
            
            report['directories_processed'].append(old_dir)
        else:
            print(f"\nSkipping: {old_dir} (does not exist)")
    
    # Save migration report
    report_path = base_path / 'output' / 'MIGRATION_REPORT.json'
    with open(report_path, 'w') as f:
        json.dump(report, f, indent=2)
    print(f"\nMigration report saved: {report_path}")
    
    print("\n" + "="*60)
    print("CONSOLIDATION COMPLETE")
    print("="*60)
    print("\nNew structure:")
    print("  output/")
    print("    ├── production/           # Final production outputs")
    print("    ├── verification/         # All verification outputs")
    print("    │   ├── automated/        # Auto-verification")
    print("    │   ├── step_by_step/     # Step 1-6 results")
    print("    │   ├── intermediate/     # Intermediate calculations")
    print("    │   └── general/          # General verification")
    print("    ├── benchmarks/           # Performance benchmarks")
    print("    └── testing/              # Test outputs")
    
    print("\n" + "="*60)
    print("NEXT STEPS:")
    print("="*60)
    print("1. Review the new structure in output/")
    print("2. Verify all files were moved correctly")
    print("3. Update scripts to use new paths")
    print("4. Delete old directories after confirmation:")
    for old_dir in migration_map.keys():
        print(f"   rm -rf {old_dir}")
    
    return report


def cleanup_old_directories():
    """Remove old output directories after confirmation."""
    base_path = Path(__file__).parent
    
    old_dirs = [
        'output_auto_verify',
        'output_benchmark',
        'output_progress_test',
        'output_step5_intermediate',
        'output_step_verify',
        'output_verification'
    ]
    
    print("\nCleaning up old directories...")
    for old_dir in old_dirs:
        old_path = base_path / old_dir
        if old_path.exists():
            try:
                shutil.rmtree(old_path)
                print(f"  Removed: {old_dir}")
            except Exception as e:
                print(f"  Error removing {old_dir}: {e}")
    
    print("\nCleanup complete!")


if __name__ == "__main__":
    import sys
    
    if len(sys.argv) > 1 and sys.argv[1] == '--cleanup':
        cleanup_old_directories()
    else:
        consolidate_outputs()
        print("\nTo remove old directories after verification, run:")
        print("  python consolidate_outputs.py --cleanup")