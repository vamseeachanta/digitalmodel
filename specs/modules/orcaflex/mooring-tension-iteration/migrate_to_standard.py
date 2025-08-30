#!/usr/bin/env python
"""
Migrate existing OrcaFlex files to standardized folder structure.
This ensures repeatability and consistency across all OrcaFlex analyses.
"""

import sys
from pathlib import Path
import shutil
import logging
from datetime import datetime

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class OrcaFlexMigration:
    """Handle migration of OrcaFlex files to standard folder structure"""
    
    def __init__(self, base_dir="go-by"):
        self.base = Path(base_dir)
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.migration_report = []
        
    def create_standard_structure(self):
        """Create all required directories for standard structure"""
        
        directories = [
            # .dat directories
            ".dat/original",
            ".dat/modified", 
            ".dat/archive",
            
            # .sim directories
            ".sim/baseline",
            ".sim/iterations",
            ".sim/final",
            
            # Configuration
            "configs",
            
            # Results
            "results/csv",
            "results/plots",
            "results/reports",
            
            # Logs
            "logs/iteration_logs",
            
            # Scripts
            "scripts/utilities"
        ]
        
        logger.info("Creating standard directory structure...")
        for dir_path in directories:
            full_path = self.base / dir_path
            full_path.mkdir(parents=True, exist_ok=True)
            logger.debug(f"Created: {full_path}")
            
        # Create .gitkeep files to track empty directories
        for dir_path in directories:
            gitkeep = self.base / dir_path / ".gitkeep"
            if not gitkeep.exists():
                gitkeep.touch()
                
        logger.info("✓ Standard directory structure created")
        
    def migrate_files(self):
        """Migrate existing files to standard locations"""
        
        # Define migration rules: (pattern, destination, description)
        migration_rules = [
            # .dat files
            ("*.dat", ".dat/original", "OrcaFlex data files"),
            
            # .sim files  
            ("*.sim", ".sim/baseline", "OrcaFlex simulation files"),
            
            # Configuration files
            ("dm_ofx_anal_*.yml", "configs", "Analysis configuration"),
            ("dm_ofx_post_*.yml", "configs", "Post-processing configuration"),
            ("*target*.csv", "configs", "Target value files"),
            ("*fender*.csv", "configs", "Fender configuration"),
            
            # Scripts (keep in place but note location)
            ("run_models_to_sim.py", "scripts", "Model runner script"),
            ("dm_iterator.sh", "scripts", "Iterator script"),
            ("dm_pretension_iteration.sh", "scripts", "Pretension script"),
        ]
        
        logger.info("\nStarting file migration...")
        
        for pattern, dest_dir, description in migration_rules:
            for file_path in self.base.glob(pattern):
                # Skip if already in correct location
                if any(part.startswith('.') for part in file_path.parts):
                    continue
                    
                # Skip directories
                if file_path.is_dir():
                    continue
                    
                dest_path = self.base / dest_dir / file_path.name
                
                # Handle existing files
                if dest_path.exists():
                    # Archive existing file with timestamp
                    if dest_dir.startswith(".dat") or dest_dir.startswith(".sim"):
                        archive_dir = self.base / dest_dir.replace("original", "archive").replace("baseline", "archive")
                        archive_dir.mkdir(parents=True, exist_ok=True)
                        archive_path = archive_dir / f"{file_path.stem}_{self.timestamp}{file_path.suffix}"
                        shutil.copy2(str(dest_path), str(archive_path))
                        logger.warning(f"Archived existing: {dest_path.name} -> {archive_path.name}")
                
                # Move or copy file
                if dest_dir == "scripts":
                    # Keep scripts in place, just note their location
                    logger.info(f"Script location noted: {file_path.name}")
                    self.migration_report.append({
                        'file': file_path.name,
                        'action': 'kept',
                        'location': str(file_path.parent),
                        'type': description
                    })
                else:
                    # Move file to new location
                    shutil.move(str(file_path), str(dest_path))
                    logger.info(f"Moved: {file_path.name} -> {dest_dir}/")
                    self.migration_report.append({
                        'file': file_path.name,
                        'action': 'moved',
                        'from': str(file_path.parent),
                        'to': dest_dir,
                        'type': description
                    })
                    
        logger.info("✓ File migration complete")
        
    def create_migration_report(self):
        """Generate a migration report"""
        
        report_path = self.base / "MIGRATION_REPORT.md"
        
        with open(report_path, 'w') as f:
            f.write(f"# OrcaFlex File Migration Report\n\n")
            f.write(f"**Date**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            f.write(f"## Summary\n\n")
            f.write(f"- Total files processed: {len(self.migration_report)}\n")
            f.write(f"- Files moved: {sum(1 for r in self.migration_report if r['action'] == 'moved')}\n")
            f.write(f"- Files kept: {sum(1 for r in self.migration_report if r['action'] == 'kept')}\n\n")
            
            f.write(f"## File Migrations\n\n")
            f.write("| File | Type | Action | Destination |\n")
            f.write("|------|------|--------|-------------|\n")
            
            for report in self.migration_report:
                if report['action'] == 'moved':
                    dest = report['to']
                else:
                    dest = report['location']
                f.write(f"| {report['file']} | {report['type']} | {report['action']} | {dest}/ |\n")
                
            f.write(f"\n## New Directory Structure\n\n")
            f.write("```\n")
            f.write(self.generate_tree_view())
            f.write("```\n")
            
        logger.info(f"✓ Migration report saved: {report_path}")
        
    def generate_tree_view(self):
        """Generate a tree view of the new structure"""
        
        def tree(dir_path, prefix="", is_last=True):
            contents = []
            entries = sorted(dir_path.iterdir(), key=lambda x: (x.is_file(), x.name))
            
            for i, path in enumerate(entries):
                is_last_entry = i == len(entries) - 1
                
                if path.name == ".gitkeep":
                    continue
                    
                connector = "└── " if is_last_entry else "├── "
                contents.append(f"{prefix}{connector}{path.name}")
                
                if path.is_dir() and not path.name.startswith('.git'):
                    extension = "    " if is_last_entry else "│   "
                    subtree = tree(path, prefix + extension, is_last_entry)
                    if subtree:
                        contents.append(subtree)
                        
            return "\n".join(contents)
            
        return f"{self.base.name}/\n{tree(self.base)}"
        
    def update_gitignore(self):
        """Update .gitignore for standard structure"""
        
        gitignore_path = self.base / ".gitignore"
        
        gitignore_content = """# OrcaFlex Standard Structure Gitignore

# Large binary simulation files
.sim/
*.sim

# Keep original .dat files
!.dat/original/*.dat

# Ignore modified and archived .dat files
.dat/modified/
.dat/archive/

# Ignore results but keep structure
results/csv/*.csv
results/plots/*.png
results/plots/*.jpg
results/reports/*.pdf
results/reports/*.html

# Keep .gitkeep files
!.gitkeep

# Logs
logs/*.log
logs/iteration_logs/*.log

# Temporary files
*.tmp
*.temp
*.backup
"""
        
        with open(gitignore_path, 'w') as f:
            f.write(gitignore_content)
            
        logger.info("✓ Created .gitignore for standard structure")
        
    def run(self):
        """Execute the complete migration"""
        
        logger.info("=" * 60)
        logger.info("OrcaFlex File Structure Migration")
        logger.info("=" * 60)
        
        if not self.base.exists():
            logger.error(f"Directory not found: {self.base}")
            return False
            
        try:
            # Step 1: Create directory structure
            self.create_standard_structure()
            
            # Step 2: Migrate files
            self.migrate_files()
            
            # Step 3: Create migration report
            self.create_migration_report()
            
            # Step 4: Update .gitignore
            self.update_gitignore()
            
            logger.info("\n" + "=" * 60)
            logger.info("✓ Migration completed successfully!")
            logger.info("=" * 60)
            
            # Print quick summary
            logger.info("\nQuick Summary:")
            logger.info(f"  • Standard structure created in: {self.base}")
            logger.info(f"  • Files migrated: {len(self.migration_report)}")
            logger.info(f"  • Report saved: MIGRATION_REPORT.md")
            logger.info(f"  • Gitignore updated")
            
            return True
            
        except Exception as e:
            logger.error(f"Migration failed: {str(e)}")
            return False


def main():
    """Main entry point"""
    
    # Get directory from command line or use default
    if len(sys.argv) > 1:
        target_dir = sys.argv[1]
    else:
        target_dir = "go-by"
        
    # Run migration
    migration = OrcaFlexMigration(target_dir)
    success = migration.run()
    
    if not success:
        sys.exit(1)
        
    # Print next steps
    print("\n" + "=" * 60)
    print("NEXT STEPS:")
    print("=" * 60)
    print("1. Review MIGRATION_REPORT.md for details")
    print("2. Update scripts to use new paths:")
    print("   - .dat/original/ for input .dat files")
    print("   - .sim/baseline/ for output .sim files")
    print("   - configs/ for configuration files")
    print("3. Test the workflow with new structure")
    print("4. Commit changes to version control")
    

if __name__ == "__main__":
    main()