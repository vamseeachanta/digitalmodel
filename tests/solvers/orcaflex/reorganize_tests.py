#!/usr/bin/env python
"""
Reorganize OrcaFlex test files into proper module structure
=========================================================
This script reorganizes test files according to their functional areas.
"""

import os
import shutil
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class TestReorganizer:
    """Reorganize test files into proper module structure."""
    
    def __init__(self):
        self.base_path = Path("D:/github/digitalmodel/tests/domains/orcaflex")
        self.moves = []
        
    def plan_reorganization(self):
        """Plan the test file reorganization."""
        
        # Define proper test structure
        reorganization_plan = {
            # Batch processing tests belong together
            "batch_processing": [
                "mooring_tension_iteration/test_batch_runner.py",
                "mooring_tension_iteration/test_with_actual_models.py",
                "mooring_tension_iteration/run_tests.py",
            ],
            
            # Core OrcaFlex API tests
            "core": [
                "core/test_configuration.py",
                "core/test_core_framework.py",
                "core/test_adaptive_mode.py",
            ],
            
            # Analysis tests (main simulation runs)
            "analysis": [
                "orcaflex_analysis/test_dat_analysis.py",
                "orcaflex_analysis/test_direct_orcaflex.py",
                "orcaflex_analysis/test_parallel_processing.py",
                "orcaflex_analysis/test_run_analysis.py",
                "orcaflex_analysis/test_sim_verification.py",
            ],
            
            # Mooring-specific tests
            "moorings": [
                "orcaflex_analysis/moorings/test_ofx_mooring_analysis.py",
                "orcaflex_analysis/moorings/ofx_mooring_analysis_test.py",
                "mooring-tension-iteration/mooring_tension_iteration_test.py",  # Move from hyphenated dir
            ],
            
            # File preparation tests
            "file_preparation": [
                "orcaflex_file_preparation/test_load_vessel_aqwa.py",
                "orcaflex_file_preparation/test_opreproc_check_yml.py",
            ],
            
            # Post-processing tests
            "post_processing": [
                "post_process/basic/opp_summary_full_test.py",
                "post_process/raos/raos_test.py",
                "post_process/visualization/plot_yml_raos_test.py",
                "post_process/wlng/fsts_simultaneous_test.py",
                "post_process/wlng/fsts_time_traces_test.py",
            ],
            
            # Browser interface tests
            "browser_interface": [
                "browser-interface/test_browser_integration.py",
            ]
        }
        
        # Convert to move operations
        for category, files in reorganization_plan.items():
            for file_path in files:
                src = self.base_path / file_path
                # Determine destination based on category
                if category in ["batch_processing", "moorings"]:
                    # These stay under mooring_tension_iteration
                    dest_dir = self.base_path / "mooring_tension_iteration" / category
                else:
                    # These go to their respective directories
                    dest_dir = self.base_path / category
                
                dest = dest_dir / Path(file_path).name
                
                if src.exists():
                    self.moves.append((src, dest, category))
                    
        return self.moves
    
    def execute_moves(self, max_workers=4):
        """Execute file moves in parallel."""
        results = []
        
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = {}
            
            for src, dest, category in self.moves:
                future = executor.submit(self.move_file, src, dest, category)
                futures[future] = (src, dest, category)
            
            for future in as_completed(futures):
                src, dest, category = futures[future]
                try:
                    result = future.result()
                    results.append(result)
                    logger.info(f"[OK] Moved {src.name} to {category}/")
                except Exception as e:
                    logger.error(f"[FAILED] Could not move {src.name}: {e}")
                    results.append({"success": False, "error": str(e)})
        
        return results
    
    def move_file(self, src: Path, dest: Path, category: str):
        """Move a single file."""
        # Create destination directory
        dest.parent.mkdir(parents=True, exist_ok=True)
        
        # Move file
        if src != dest:  # Only move if different location
            shutil.move(str(src), str(dest))
            
        return {
            "success": True,
            "src": str(src),
            "dest": str(dest),
            "category": category
        }
    
    def cleanup_empty_dirs(self):
        """Remove empty directories after reorganization."""
        dirs_to_check = [
            self.base_path / "mooring-tension-iteration",  # Remove hyphenated version
        ]
        
        for dir_path in dirs_to_check:
            if dir_path.exists() and not any(dir_path.iterdir()):
                shutil.rmtree(dir_path)
                logger.info(f"Removed empty directory: {dir_path}")
    
    def create_test_index(self):
        """Create an index file documenting test organization."""
        index_content = """# OrcaFlex Test Organization

## Test Structure

### Core Tests (`/core/`)
- Configuration and framework tests
- Basic OrcaFlex API functionality

### Analysis Tests (`/analysis/`)
- Direct OrcaFlex model runs
- Simulation verification
- Parallel processing tests

### Mooring Tension Iteration (`/mooring_tension_iteration/`)
- **batch_processing/** - Batch runner tests, sim file creation
- **moorings/** - Mooring-specific analysis tests

### File Preparation (`/file_preparation/`)
- AQWA to OrcaFlex conversion
- YAML preprocessing tests

### Post Processing (`/post_processing/`)
- Results extraction and analysis
- Visualization tests
- Summary generation

### Browser Interface (`/browser_interface/`)
- Web interface integration tests

## Running Tests

```bash
# Run all tests
python tests/domains/orcaflex/run_tests.py

# Run specific category
pytest tests/domains/orcaflex/mooring_tension_iteration/batch_processing/

# Run with OrcaFlex license
ORCAFLEX_LICENSE_AVAILABLE=true pytest tests/domains/orcaflex/
```
"""
        
        index_file = self.base_path / "TEST_ORGANIZATION.md"
        index_file.write_text(index_content)
        logger.info(f"Created test index: {index_file}")

def main():
    """Main execution."""
    reorganizer = TestReorganizer()
    
    # Plan moves
    moves = reorganizer.plan_reorganization()
    logger.info(f"Planned {len(moves)} file moves")
    
    # Execute in parallel
    if moves:
        logger.info("Executing file moves in parallel...")
        results = reorganizer.execute_moves(max_workers=4)
        
        successful = sum(1 for r in results if r.get("success", False))
        logger.info(f"Completed: {successful}/{len(moves)} moves successful")
    
    # Cleanup
    reorganizer.cleanup_empty_dirs()
    
    # Create index
    reorganizer.create_test_index()
    
    logger.info("Test reorganization complete!")

if __name__ == "__main__":
    main()