#!/usr/bin/env python
"""
Update import paths in reorganized test files
==============================================
"""

import re
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class ImportUpdater:
    """Update import statements in test files."""
    
    def __init__(self):
        self.base_path = Path("D:/github/digitalmodel/tests/domains/orcaflex")
        
        # Map of test locations to update
        self.test_locations = {
            "mooring_tension_iteration/batch_processing": [
                "test_batch_runner.py",
                "test_with_actual_models.py",
                "run_tests.py"
            ],
            "mooring_tension_iteration/moorings": [
                "mooring_tension_iteration_test.py",
                "test_ofx_mooring_analysis.py",
                "ofx_mooring_analysis_test.py"
            ],
            "analysis": [
                "test_dat_analysis.py",
                "test_direct_orcaflex.py",
                "test_parallel_processing.py",
                "test_run_analysis.py",
                "test_sim_verification.py"
            ],
            "post_processing": [
                "opp_summary_full_test.py",
                "raos_test.py",
                "plot_yml_raos_test.py",
                "fsts_simultaneous_test.py",
                "fsts_time_traces_test.py"
            ]
        }
    
    def calculate_relative_path_depth(self, test_location: str) -> int:
        """Calculate how many levels up to get to src/."""
        # From tests/domains/orcaflex/[location] to src/
        depth = len(Path(test_location).parts) + 3  # +3 for tests/domains/orcaflex
        return depth
    
    def update_file_imports(self, file_path: Path, depth: int) -> dict:
        """Update imports in a single file."""
        try:
            content = file_path.read_text()
            original = content
            
            # Update sys.path.insert statements
            # Pattern: sys.path.insert(0, str(Path(__file__).parent.parent...
            pattern = r'sys\.path\.insert\(0,\s*str\(Path\(__file__\)\.parent(\.parent)*.*?\)\)'
            
            # Calculate correct parent chain
            parent_chain = '.parent' * depth
            replacement = f'sys.path.insert(0, str(Path(__file__){parent_chain} / "src"))'
            
            content = re.sub(pattern, replacement, content)
            
            # Also update relative imports if any
            # From: from modules.orcaflex...
            # Should remain the same as we're adding src to path
            
            if content != original:
                file_path.write_text(content)
                return {"file": str(file_path), "updated": True}
            else:
                return {"file": str(file_path), "updated": False}
                
        except Exception as e:
            logger.error(f"Failed to update {file_path}: {e}")
            return {"file": str(file_path), "error": str(e)}
    
    def update_all_imports(self, max_workers=4):
        """Update imports in all test files in parallel."""
        tasks = []
        
        # Prepare update tasks
        for location, files in self.test_locations.items():
            depth = self.calculate_relative_path_depth(location)
            
            for file_name in files:
                file_path = self.base_path / location / file_name
                if file_path.exists():
                    tasks.append((file_path, depth))
        
        # Execute updates in parallel
        results = []
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = {}
            
            for file_path, depth in tasks:
                future = executor.submit(self.update_file_imports, file_path, depth)
                futures[future] = file_path
            
            for future in as_completed(futures):
                file_path = futures[future]
                try:
                    result = future.result()
                    if result.get("updated"):
                        logger.info(f"[UPDATED] {file_path.name}")
                    else:
                        logger.info(f"[NO CHANGE] {file_path.name}")
                    results.append(result)
                except Exception as e:
                    logger.error(f"[ERROR] {file_path.name}: {e}")
        
        return results
    
    def verify_imports(self):
        """Quick verification that imports are correct."""
        test_file = self.base_path / "mooring_tension_iteration/batch_processing/test_batch_runner.py"
        
        if test_file.exists():
            content = test_file.read_text()
            if "sys.path.insert" in content:
                match = re.search(r'sys\.path\.insert\(0,\s*str\(Path\(__file__\)(\.parent)*.*?\)\)', content)
                if match:
                    logger.info(f"Import line found: {match.group()}")
                    return True
        return False

def main():
    """Main execution."""
    updater = ImportUpdater()
    
    logger.info("Updating import statements in reorganized test files...")
    results = updater.update_all_imports(max_workers=4)
    
    updated_count = sum(1 for r in results if r.get("updated"))
    logger.info(f"Updated {updated_count}/{len(results)} files")
    
    # Verify
    if updater.verify_imports():
        logger.info("Import verification passed!")
    else:
        logger.warning("Import verification needs manual check")

if __name__ == "__main__":
    main()