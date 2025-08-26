"""
Verification script for OrcaFlex optimization deployment
Checks that all configurations are properly updated
"""

import yaml
import json
from pathlib import Path
from datetime import datetime

def verify_deployment():
    """Verify the optimization deployment is complete"""
    
    print("\n" + "="*60)
    print("ORCAFLEX OPTIMIZATION DEPLOYMENT VERIFICATION")
    print("="*60)
    
    results = {
        "timestamp": datetime.now().isoformat(),
        "checks": {},
        "all_passed": True
    }
    
    # Check 1: Verify configuration files have been updated
    print("\n1. Checking configuration files...")
    config_files = [
        Path("D:/1522/ctr7/orcaflex/rev_a08/runtime_test/dm_fsts.yml"),
        Path("D:/1522/ctr7/orcaflex/rev_a08/runtime_test/dm_fsts_lngc.yml")
    ]
    
    for config_file in config_files:
        if config_file.exists():
            with open(config_file, 'r') as f:
                config = yaml.safe_load(f)
            
            # Check for parallel configuration
            if 'orcaflex' in config and 'parallel' in config['orcaflex']:
                threads = config['orcaflex']['parallel'].get('threads', None)
                if threads == 15:
                    print(f"   [OK] {config_file.name}: threads = {threads} (optimized)")
                    results["checks"][config_file.name] = "PASS"
                else:
                    print(f"   [WARN] {config_file.name}: threads = {threads} (expected 15)")
                    results["checks"][config_file.name] = f"FAIL - threads={threads}"
                    results["all_passed"] = False
            else:
                print(f"   [WARN] {config_file.name}: No parallel configuration found")
                results["checks"][config_file.name] = "FAIL - no parallel config"
                results["all_passed"] = False
        else:
            print(f"   [ERROR] {config_file.name}: File not found")
            results["checks"][config_file.name] = "FAIL - file not found"
            results["all_passed"] = False
    
    # Check 2: Verify backup files exist
    print("\n2. Checking backup files...")
    backup_files = [
        Path("D:/1522/ctr7/orcaflex/rev_a08/runtime_test/dm_fsts_backup_20250825.yml"),
        Path("D:/1522/ctr7/orcaflex/rev_a08/runtime_test/dm_fsts_lngc_backup_20250825.yml")
    ]
    
    for backup_file in backup_files:
        if backup_file.exists():
            print(f"   [OK] {backup_file.name}: Backup exists")
            results["checks"][f"backup_{backup_file.name}"] = "PASS"
        else:
            print(f"   [WARN] {backup_file.name}: Backup not found")
            results["checks"][f"backup_{backup_file.name}"] = "FAIL"
            results["all_passed"] = False
    
    # Check 3: Verify Python modules are available
    print("\n3. Checking Python optimization modules...")
    modules_to_check = [
        "digitalmodel.modules.orcaflex.performance_monitor",
        "digitalmodel.modules.orcaflex.orcaflex_optimized_parallel_v2"
    ]
    
    for module_name in modules_to_check:
        try:
            module = __import__(module_name, fromlist=[''])
            print(f"   [OK] {module_name}: Module available")
            results["checks"][module_name] = "PASS"
        except ImportError as e:
            print(f"   [ERROR] {module_name}: Import failed - {e}")
            results["checks"][module_name] = f"FAIL - {e}"
            results["all_passed"] = False
    
    # Check 4: Verify agent configuration
    print("\n4. Checking agent configuration...")
    agent_config = Path("D:/github/digitalmodel/agents/orcaflex/agent.yaml")
    if agent_config.exists():
        with open(agent_config, 'r') as f:
            content = f.read()
        
        if "NO_MOCK_SIM_FILES" in content:
            print("   [OK] Agent config: Production rules enforced")
            results["checks"]["agent_config"] = "PASS"
        else:
            print("   [WARN] Agent config: Missing production rules")
            results["checks"]["agent_config"] = "FAIL - missing rules"
            results["all_passed"] = False
    else:
        print("   [ERROR] Agent config not found")
        results["checks"]["agent_config"] = "FAIL - not found"
        results["all_passed"] = False
    
    # Check 5: Verify CLAUDE.md has protection rules
    print("\n5. Checking CLAUDE.md protection rules...")
    claude_md = Path("D:/github/digitalmodel/CLAUDE.md")
    if claude_md.exists():
        with open(claude_md, 'r', encoding='utf-8') as f:
            content = f.read()
        
        if "NO MOCK .SIM FILES" in content:
            print("   [OK] CLAUDE.md: .sim file protection rules present")
            results["checks"]["claude_md"] = "PASS"
        else:
            print("   [WARN] CLAUDE.md: Missing .sim protection rules")
            results["checks"]["claude_md"] = "FAIL - missing rules"
            results["all_passed"] = False
    else:
        print("   [ERROR] CLAUDE.md not found")
        results["checks"]["claude_md"] = "FAIL - not found"
        results["all_passed"] = False
    
    # Generate summary
    print("\n" + "="*60)
    print("VERIFICATION SUMMARY")
    print("="*60)
    
    passed = sum(1 for v in results["checks"].values() if v == "PASS")
    total = len(results["checks"])
    
    print(f"Checks Passed: {passed}/{total}")
    print(f"Overall Status: {'PASSED' if results['all_passed'] else 'FAILED'}")
    
    if results["all_passed"]:
        print("\n[SUCCESS] Optimization deployment is complete and verified!")
        print("\nExpected Performance Improvement:")
        print("  - 14-20% runtime reduction for large .sim files")
        print("  - Reduced I/O contention with 15 threads (was 30)")
        print("  - Better resource utilization")
    else:
        print("\n[WARNING] Some checks failed. Please review and fix.")
    
    # Save verification report
    report_path = Path("D:/github/digitalmodel/reports/production_deployment/verification_report.json")
    report_path.parent.mkdir(parents=True, exist_ok=True)
    
    with open(report_path, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"\nVerification report saved to: {report_path}")
    
    return results

if __name__ == "__main__":
    verify_deployment()