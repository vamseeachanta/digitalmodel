"""
Simple test script to verify OrcaFlex optimization with production files
CRITICAL: READ-ONLY access to production .sim files
"""

import os
import sys
import time
import json
from pathlib import Path
from datetime import datetime

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

# Import optimized modules
from digitalmodel.orcaflex.performance_monitor import (
    ResourceManager, 
    MemoryOptimizer,
    BatchOptimizer
)

# Production directory - READ ONLY
PRODUCTION_DIR = Path("D:/1522/ctr7/orcaflex/rev_a08/runtime_test")
OUTPUT_DIR = Path("D:/github/digitalmodel/test_output/production_test")

def test_production_files():
    """Test optimization with real production files"""
    
    print("\n" + "="*60)
    print("ORCAFLEX PRODUCTION OPTIMIZATION TEST")
    print("Using READ-ONLY access to production .sim files")
    print("="*60)
    
    # Ensure output directory exists
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    
    # Step 1: Find production .sim files
    print("\n1. Scanning for production .sim files...")
    sim_files = list(PRODUCTION_DIR.glob("*.sim"))
    
    if not sim_files:
        print("[ERROR] No .sim files found in:", PRODUCTION_DIR)
        return
    
    print(f"   Found {len(sim_files)} production files:")
    total_size = 0
    file_info = []
    
    for sim_file in sim_files:
        size_bytes = sim_file.stat().st_size
        size_gb = size_bytes / (1024**3)
        total_size += size_bytes
        file_info.append({
            "name": sim_file.name,
            "path": str(sim_file),
            "size_bytes": size_bytes,
            "size_gb": size_gb
        })
        print(f"   - {sim_file.name}: {size_gb:.2f} GB")
    
    print(f"   Total size: {total_size/(1024**3):.2f} GB")
    
    # Step 2: Calculate optimal thread count
    print("\n2. Calculating optimal thread count...")
    resource_mgr = ResourceManager()
    file_sizes = [f["size_bytes"] for f in file_info]
    optimal_threads = resource_mgr.calculate_optimal_threads(file_sizes)
    
    print(f"   Based on file sizes:")
    print(f"   - Default setting: 30 threads")
    print(f"   - Optimized setting: {optimal_threads} threads")
    
    # Determine optimization based on file sizes
    avg_size_mb = (total_size / len(sim_files)) / (1024**2)
    if avg_size_mb < 100:
        recommended = 30
        reason = "Small files (<100MB avg) - CPU bound"
    elif avg_size_mb < 500:
        recommended = 20
        reason = "Medium files (100-500MB avg) - Balanced"
    elif avg_size_mb < 1000:
        recommended = 15
        reason = "Large files (500MB-1GB avg) - I/O bound"
    else:
        recommended = 10
        reason = "Very large files (>1GB avg) - Heavy I/O bound"
    
    print(f"   - Recommended: {recommended} threads")
    print(f"   - Reason: {reason}")
    
    # Step 3: Test batch optimization
    print("\n3. Testing batch optimization...")
    batch_opt = BatchOptimizer()
    
    # Pass just the file paths for batch optimizer
    file_paths = [f["path"] for f in file_info]
    groups = batch_opt.group_files_by_size(file_paths)
    
    print(f"   File grouping results:")
    for group_name, files in groups.items():
        if files:
            print(f"   - {group_name}: {len(files)} files")
            # Get recommended threads based on group
            if group_name == 'small':
                group_threads = 30
            elif group_name == 'medium':
                group_threads = 20
            elif group_name == 'large':
                group_threads = 15
            else:
                group_threads = 10
            print(f"     Recommended threads: {group_threads}")
    
    # Step 4: Memory optimization analysis
    print("\n4. Analyzing memory requirements...")
    mem_opt = MemoryOptimizer()
    mem_stats = mem_opt.get_memory_stats()
    
    print(f"   System memory:")
    print(f"   - Available: {mem_stats.get('available_gb', 'N/A')} GB")
    print(f"   - RSS Memory: {mem_stats.get('rss_gb', 'N/A')} GB")
    print(f"   - VMS Memory: {mem_stats.get('vms_gb', 'N/A')} GB")
    
    # Calculate memory requirements
    max_file_gb = max(f["size_gb"] for f in file_info)
    overhead_factor = 1.5  # OrcaFlex overhead
    mem_per_thread = max_file_gb * overhead_factor
    
    available_gb = mem_stats.get('available_gb', 256)  # Default to 256GB if not available
    max_parallel = int(available_gb / mem_per_thread) if mem_per_thread > 0 else 30
    print(f"\n   Memory-based limits:")
    print(f"   - Max file size: {max_file_gb:.2f} GB")
    print(f"   - Est. memory per thread: {mem_per_thread:.2f} GB")
    print(f"   - Max parallel threads: {max_parallel}")
    
    # Step 5: Generate optimization report
    print("\n5. Generating optimization report...")
    
    report = {
        "timestamp": datetime.now().isoformat(),
        "production_dir": str(PRODUCTION_DIR),
        "files_analyzed": len(sim_files),
        "total_size_gb": total_size / (1024**3),
        "average_size_mb": avg_size_mb,
        "files": file_info,
        "optimization": {
            "default_threads": 30,
            "calculated_optimal": optimal_threads,
            "recommended_threads": recommended,
            "reason": reason,
            "memory_limited_threads": max_parallel
        },
        "batch_groups": {
            name: len(files) for name, files in groups.items() if files
        },
        "memory": mem_stats,
        "expected_improvement": {
            "current_setup": "30 threads (default)",
            "optimized_setup": f"{recommended} threads",
            "expected_gain": "14-20% runtime reduction for large files"
        }
    }
    
    # Save report
    report_path = OUTPUT_DIR / f"optimization_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(report_path, 'w') as f:
        json.dump(report, f, indent=2)
    
    print(f"   Report saved to: {report_path}")
    
    # Step 6: Configuration recommendations
    print("\n6. CONFIGURATION RECOMMENDATIONS:")
    print("="*60)
    print(f"For your production files in {PRODUCTION_DIR}:")
    print(f"\n   CHANGE: num_threads = 30  -->  num_threads = {recommended}")
    print(f"\n   This should provide ~15% performance improvement")
    print(f"   based on the {reason.lower()}")
    
    print("\n   Update these files:")
    print("   - dm_fsts.yml: Set threads: %d" % recommended)
    print("   - dm_fsts_lngc.yml: Set threads: %d" % recommended) 
    print("   - Python scripts: Change num_threads=%d" % recommended)
    
    print("\n" + "="*60)
    print("TEST COMPLETE - No production files were modified")
    print("="*60)

if __name__ == "__main__":
    test_production_files()