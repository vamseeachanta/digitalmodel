#!/usr/bin/env python3
"""
Content Migration Script with Backward Compatibility
Part of the AI-Friendly Documentation Organization project.

This script migrates content from the old prefix-based structure
to the new hierarchical organization while maintaining backward
compatibility through symbolic links.
"""

import os
import json
import shutil
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
import hashlib
import time


class ContentMigrator:
    """Handles systematic content migration with backward compatibility."""
    
    def __init__(self, docs_root: str = "K:\\github\\digitalmodel\\docs"):
        """Initialize migrator with docs root directory."""
        self.docs_root = Path(docs_root)
        self.analysis_file = self.docs_root / "docs_analysis_results.json"
        self.migration_log = []
        self.backup_created = False
        
    def load_migration_mapping(self) -> Dict[str, str]:
        """Load migration mapping from analysis results."""
        try:
            if self.analysis_file.exists():
                with open(self.analysis_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    return data.get("mapping", {})
            else:
                print(f"Warning: Analysis file not found at {self.analysis_file}")
                return {}
        except Exception as e:
            print(f"Error loading migration mapping: {e}")
            return {}
            
    def create_backup(self) -> bool:
        """Create backup of the docs directory before migration."""
        try:
            backup_name = f"docs_backup_{int(time.time())}"
            backup_path = self.docs_root.parent / backup_name
            
            print(f"Creating backup at {backup_path}...")
            shutil.copytree(self.docs_root, backup_path)
            
            self.backup_created = True
            print(f"Backup created successfully at {backup_path}")
            return True
            
        except Exception as e:
            print(f"Error creating backup: {e}")
            return False
            
    def calculate_file_hash(self, file_path: Path) -> str:
        """Calculate SHA-256 hash of a file for integrity checking."""
        try:
            hash_sha256 = hashlib.sha256()
            with open(file_path, "rb") as f:
                for chunk in iter(lambda: f.read(4096), b""):
                    hash_sha256.update(chunk)
            return hash_sha256.hexdigest()
        except Exception:
            return ""
            
    def verify_file_integrity(self, source_file: Path, target_file: Path) -> bool:
        """Verify file integrity after migration."""
        if not source_file.exists() or not target_file.exists():
            return False
            
        source_hash = self.calculate_file_hash(source_file)
        target_hash = self.calculate_file_hash(target_file)
        
        return source_hash == target_hash and source_hash != ""
        
    def migrate_file(self, source_file: Path, target_file: Path, 
                    preserve_metadata: bool = True) -> bool:
        """Migrate a single file with integrity checking."""
        try:
            # Ensure target directory exists
            target_file.parent.mkdir(parents=True, exist_ok=True)
            
            # Copy file
            shutil.copy2(source_file, target_file)
            
            # Verify integrity
            if not self.verify_file_integrity(source_file, target_file):
                print(f"Warning: Integrity check failed for {source_file}")
                return False
                
            # Log migration
            self.migration_log.append({
                "source": str(source_file),
                "target": str(target_file),
                "size": source_file.stat().st_size,
                "timestamp": time.time(),
                "integrity_verified": True
            })
            
            return True
            
        except Exception as e:
            print(f"Error migrating {source_file} to {target_file}: {e}")
            self.migration_log.append({
                "source": str(source_file),
                "target": str(target_file),
                "error": str(e),
                "timestamp": time.time(),
                "integrity_verified": False
            })
            return False
            
    def migrate_directory(self, source_dir: Path, target_dir: Path, 
                         preserve_structure: bool = True) -> Tuple[int, int]:
        """Migrate entire directory with all contents."""
        success_count = 0
        error_count = 0
        
        if not source_dir.exists():
            print(f"Source directory does not exist: {source_dir}")
            return 0, 1
            
        print(f"Migrating {source_dir} to {target_dir}...")
        
        # Ensure target directory exists
        target_dir.mkdir(parents=True, exist_ok=True)
        
        # Migrate all files and subdirectories
        for item in source_dir.rglob("*"):
            if item.is_file():
                # Calculate relative path
                rel_path = item.relative_to(source_dir)
                target_file = target_dir / rel_path
                
                # Migrate file
                if self.migrate_file(item, target_file):
                    success_count += 1
                else:
                    error_count += 1
                    
        return success_count, error_count
        
    def create_symlink(self, target_path: Path, link_path: Path, 
                      force: bool = False) -> bool:
        """Create symbolic link for backward compatibility."""
        try:
            # Ensure link directory exists
            link_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Remove existing link/file if force is True
            if force and link_path.exists():
                if link_path.is_symlink():
                    link_path.unlink()
                elif link_path.is_file():
                    link_path.unlink()
                elif link_path.is_dir():
                    shutil.rmtree(link_path)
                    
            # Create symlink (or copy on Windows due to permission issues)
            if os.name == 'nt':
                # On Windows, create junction for directories or copy for files
                if target_path.is_dir():
                    # Use junction for directories on Windows
                    import subprocess
                    try:
                        subprocess.run([
                            'mklink', '/J', str(link_path), str(target_path)
                        ], check=True, shell=True, capture_output=True)
                        return True
                    except subprocess.CalledProcessError:
                        # Fallback: copy directory
                        if target_path.exists():
                            shutil.copytree(target_path, link_path)
                            return True
                else:
                    # For files, create a copy
                    if target_path.exists():
                        shutil.copy2(target_path, link_path)
                        return True
            else:
                # On Unix systems, create actual symlink
                link_path.symlink_to(target_path)
                return True
                
            return False
            
        except Exception as e:
            print(f"Error creating symlink from {link_path} to {target_path}: {e}")
            return False
            
    def create_compatibility_links(self, mapping: Dict[str, str]) -> Dict[str, Any]:
        """Create symbolic links for all migrated content."""
        print("Creating backward compatibility links...")
        
        results = {
            "links_created": 0,
            "links_failed": 0,
            "directories_linked": []
        }
        
        for old_path, new_path in mapping.items():
            old_dir = self.docs_root / old_path
            new_dir = self.docs_root / new_path
            
            # Skip if old directory doesn't exist or new directory doesn't exist
            if not old_dir.exists() or not new_dir.exists():
                continue
                
            # Create symlink from old location to new location
            if self.create_symlink(new_dir, old_dir, force=True):
                results["links_created"] += 1
                results["directories_linked"].append({
                    "old_path": old_path,
                    "new_path": new_path,
                    "link_target": str(new_dir),
                    "link_location": str(old_dir)
                })
                print(f"Created link: {old_path} -> {new_path}")
            else:
                results["links_failed"] += 1
                print(f"Failed to create link: {old_path} -> {new_path}")
                
        return results
        
    def validate_migration(self, mapping: Dict[str, str]) -> Dict[str, Any]:
        """Validate that migration was successful."""
        print("Validating migration...")
        
        validation_results = {
            "directories_checked": 0,
            "directories_valid": 0,
            "directories_missing": 0,
            "files_checked": 0,
            "files_valid": 0,
            "files_corrupted": 0,
            "missing_directories": [],
            "corrupted_files": []
        }
        
        for old_path, new_path in mapping.items():
            old_dir = self.docs_root / old_path
            new_dir = self.docs_root / new_path
            
            validation_results["directories_checked"] += 1
            
            # Check if new directory exists
            if not new_dir.exists():
                validation_results["directories_missing"] += 1
                validation_results["missing_directories"].append(new_path)
                continue
                
            validation_results["directories_valid"] += 1
            
            # Check file integrity if old directory still exists
            if old_dir.exists() and not old_dir.is_symlink():
                for old_file in old_dir.rglob("*"):
                    if old_file.is_file():
                        validation_results["files_checked"] += 1
                        
                        # Find corresponding new file
                        rel_path = old_file.relative_to(old_dir)
                        new_file = new_dir / rel_path
                        
                        if new_file.exists():
                            if self.verify_file_integrity(old_file, new_file):
                                validation_results["files_valid"] += 1
                            else:
                                validation_results["files_corrupted"] += 1
                                validation_results["corrupted_files"].append({
                                    "old_file": str(old_file),
                                    "new_file": str(new_file)
                                })
                        else:
                            validation_results["files_corrupted"] += 1
                            validation_results["corrupted_files"].append({
                                "old_file": str(old_file),
                                "new_file": str(new_file),
                                "error": "File not found in new location"
                            })
                            
        return validation_results
        
    def execute_migration(self, dry_run: bool = True, 
                         create_backup: bool = True,
                         create_links: bool = True) -> Dict[str, Any]:
        """Execute the complete migration process."""
        print("=" * 60)
        print("CONTENT MIGRATION EXECUTION")
        print("=" * 60)
        print(f"Dry run: {dry_run}")
        print(f"Create backup: {create_backup}")
        print(f"Create compatibility links: {create_links}")
        print()
        
        # Load migration mapping
        mapping = self.load_migration_mapping()
        if not mapping:
            return {"error": "No migration mapping available"}
            
        print(f"Loaded migration mapping for {len(mapping)} directories")
        
        results = {
            "dry_run": dry_run,
            "backup_created": False,
            "directories_migrated": 0,
            "files_migrated": 0,
            "migration_errors": 0,
            "links_created": 0,
            "validation_results": {},
            "migration_mapping": mapping
        }
        
        # Create backup if requested and not dry run
        if create_backup and not dry_run:
            if self.create_backup():
                results["backup_created"] = True
            else:
                return {"error": "Failed to create backup"}
                
        # Execute migration
        total_files_migrated = 0
        total_errors = 0
        
        for old_path, new_path in mapping.items():
            old_dir = self.docs_root / old_path
            new_dir = self.docs_root / new_path
            
            if not old_dir.exists():
                print(f"Skipping {old_path} (does not exist)")
                continue
                
            if old_dir.is_symlink():
                print(f"Skipping {old_path} (already a symlink)")
                continue
                
            print(f"Processing: {old_path} -> {new_path}")
            
            if dry_run:
                # Count files that would be migrated
                file_count = len([f for f in old_dir.rglob("*") if f.is_file()])
                print(f"  Would migrate {file_count} files")
                total_files_migrated += file_count
            else:
                # Actually migrate
                success_count, error_count = self.migrate_directory(old_dir, new_dir)
                total_files_migrated += success_count
                total_errors += error_count
                print(f"  Migrated {success_count} files, {error_count} errors")
                
            results["directories_migrated"] += 1
            
        results["files_migrated"] = total_files_migrated
        results["migration_errors"] = total_errors
        
        # Create compatibility links if requested and not dry run
        if create_links and not dry_run:
            link_results = self.create_compatibility_links(mapping)
            results["links_created"] = link_results["links_created"]
            results["link_results"] = link_results
            
        # Validate migration if not dry run
        if not dry_run:
            validation_results = self.validate_migration(mapping)
            results["validation_results"] = validation_results
            
        # Save migration log
        if not dry_run:
            log_file = self.docs_root / "migration_log.json"
            with open(log_file, 'w', encoding='utf-8') as f:
                json.dump({
                    "migration_log": self.migration_log,
                    "results": results,
                    "timestamp": time.time()
                }, f, indent=2, ensure_ascii=False)
            print(f"Migration log saved to: {log_file}")
            
        return results
        
    def rollback_migration(self, backup_path: Optional[Path] = None) -> bool:
        """Rollback migration using backup."""
        try:
            if backup_path is None:
                # Find most recent backup
                backup_pattern = f"docs_backup_*"
                backups = list(self.docs_root.parent.glob(backup_pattern))
                if not backups:
                    print("No backup found for rollback")
                    return False
                backup_path = max(backups, key=lambda p: p.stat().st_mtime)
                
            print(f"Rolling back from backup: {backup_path}")
            
            # Remove current docs directory
            if self.docs_root.exists():
                shutil.rmtree(self.docs_root)
                
            # Restore from backup
            shutil.copytree(backup_path, self.docs_root)
            
            print("Rollback completed successfully")
            return True
            
        except Exception as e:
            print(f"Error during rollback: {e}")
            return False


def main():
    """Main function to run content migration."""
    migrator = ContentMigrator()
    
    print("AI-Friendly Documentation Content Migrator")
    print("=" * 50)
    
    # First, run in dry-run mode
    print("1. Running migration analysis (dry-run)...")
    dry_results = migrator.execute_migration(dry_run=True)
    
    if "error" in dry_results:
        print(f"Error: {dry_results['error']}")
        return
        
    print(f"\nMigration Analysis Results:")
    print(f"  Directories to migrate: {dry_results['directories_migrated']}")
    print(f"  Files to migrate: {dry_results['files_migrated']}")
    print(f"  Migration mapping entries: {len(dry_results['migration_mapping'])}")
    
    # Show mapping
    print(f"\nMigration mapping:")
    for old, new in list(dry_results['migration_mapping'].items())[:5]:
        print(f"  {old} -> {new}")
    if len(dry_results['migration_mapping']) > 5:
        print(f"  ... and {len(dry_results['migration_mapping']) - 5} more")
        
    # Ask for confirmation
    if dry_results['files_migrated'] > 0:
        print(f"\nReady to migrate {dry_results['files_migrated']} files.")
        print("This will:")
        print("  1. Create a backup of the current docs directory")
        print("  2. Migrate content to new hierarchical structure")
        print("  3. Create symbolic links for backward compatibility")
        print("  4. Validate migration integrity")
        
        confirm = input("\nProceed with migration? (y/N): ")
        
        if confirm.lower() == 'y':
            print("\n2. Executing migration...")
            results = migrator.execute_migration(
                dry_run=False, 
                create_backup=True, 
                create_links=True
            )
            
            if "error" in results:
                print(f"Migration failed: {results['error']}")
                return
                
            print(f"\nMigration completed:")
            print(f"  Backup created: {results['backup_created']}")
            print(f"  Directories migrated: {results['directories_migrated']}")
            print(f"  Files migrated: {results['files_migrated']}")
            print(f"  Migration errors: {results['migration_errors']}")
            print(f"  Compatibility links created: {results['links_created']}")
            
            # Show validation results
            if results.get('validation_results'):
                val = results['validation_results']
                print(f"\nValidation results:")
                print(f"  Directories validated: {val['directories_valid']}/{val['directories_checked']}")
                print(f"  Files validated: {val['files_valid']}/{val['files_checked']}")
                if val['files_corrupted'] > 0:
                    print(f"  WARNING: {val['files_corrupted']} files failed validation")
                    
            print(f"\nMigration log saved. Check migration_log.json for details.")
            
        else:
            print("Migration cancelled.")
    else:
        print("No files need migration.")


if __name__ == "__main__":
    main()