#!/usr/bin/env python3
"""
Verify that generated OrcaWave configurations match template formatting.
"""

import re
from pathlib import Path
from typing import Dict, List, Tuple

def compare_format_patterns(template_file: Path, generated_file: Path) -> Dict[str, Tuple[int, int]]:
    """Compare formatting patterns between template and generated files.
    
    Args:
        template_file: Path to template file
        generated_file: Path to generated file
        
    Returns:
        Dictionary of pattern comparisons
    """
    # Read files
    with open(template_file, 'r', encoding='utf-8-sig') as f:
        template_lines = f.readlines()
    
    with open(generated_file, 'r', encoding='utf-8-sig') as f:
        generated_lines = f.readlines()
    
    # Patterns to check
    patterns = {
        'Yes format': r':\s*Yes\s*$',
        'No format': r':\s*No\s*$',
        'Quoted Yes': r":\s*'Yes'\s*$",
        'Quoted No': r":\s*'No'\s*$",
        'true format': r':\s*true\s*$',
        'false format': r':\s*false\s*$',
        'Tilde (~)': r':\s*~\s*$',
        'Infinity': r':\s*Infinity\s*$',
        'Empty string': r':\s*\'\'\s*$',
        'UTF-8 BOM': r'^﻿',
    }
    
    results = {}
    for name, pattern in patterns.items():
        template_count = sum(1 for line in template_lines if re.search(pattern, line))
        generated_count = sum(1 for line in generated_lines if re.search(pattern, line))
        results[name] = (template_count, generated_count)
    
    return results

def check_specific_fields(generated_file: Path) -> List[str]:
    """Check that specific fields have correct formatting.
    
    Args:
        generated_file: Path to generated file
        
    Returns:
        List of issues found
    """
    issues = []
    
    with open(generated_file, 'r', encoding='utf-8-sig') as f:
        content = f.read()
    
    # Fields that should be Yes/No (not true/false)
    yes_no_fields = [
        'QuadraticLoadPressureIntegration',
        'QuadraticLoadControlSurface',
        'DivideNonPlanarPanels',
        'OutputPanelPressures',
        'ValidatePanelArrangement',
        'BodyIncludedInAnalysis',
        'BodyAddInteriorSurfacePanels',
        'DetectAndSkipFieldPointsInsideBodies',
    ]
    
    for field in yes_no_fields:
        # Check if field uses true/false instead of Yes/No
        if re.search(f'{field}:\\s*(true|false)', content):
            issues.append(f"{field} uses true/false instead of Yes/No")
        # For OrcaWave, unquoted Yes/No is CORRECT - check if they are incorrectly quoted
        matches = re.findall(f'{field}:\\s*([\'"])(Yes|No)([\'"])', content)
        for match in matches:
            if match[0] and match[2]:  # Incorrectly quoted
                issues.append(f"{field} has incorrectly quoted {match[1]}")
    
    # Check for BOM
    if not content.startswith('\ufeff'):
        issues.append("Missing UTF-8 BOM")
    
    # Check YAML header
    if not content.startswith('\ufeff%YAML 1.1'):
        issues.append("Missing or incorrect YAML 1.1 header")
    
    return issues

def main():
    """Verify all generated configurations."""
    base_dir = Path(__file__).parent.parent
    template_file = base_dir / "outputs" / "orcawave_configs" / "go-by" / "go-by-template_rev2.yml"
    merged_dir = base_dir / "outputs" / "orcawave_configs" / "merged"
    
    configs = [
        "orcawave_incident_draft_fuel_centered.yml",
        "orcawave_incident_draft_fuel_centered_adjusted.yml",
        "orcawave_fo_to_port.yml",
        "orcawave_fo_to_port_with_ingress.yml",
    ]
    
    print("="*60)
    print("ORCAWAVE FORMAT VERIFICATION REPORT")
    print("="*60)
    print()
    
    all_good = True
    
    for config_file in configs:
        config_path = merged_dir / config_file
        if not config_path.exists():
            print(f"ERROR: {config_file} not found")
            all_good = False
            continue
        
        print(f"Checking: {config_file}")
        print("-"*40)
        
        # Compare patterns
        patterns = compare_format_patterns(template_file, config_path)
        
        print("Pattern comparison (Template vs Generated):")
        for pattern_name, (template_count, generated_count) in patterns.items():
            status = "OK" if generated_count > 0 or template_count == 0 else "MISMATCH"
            print(f"  {pattern_name:15} Template: {template_count:3}  Generated: {generated_count:3}  [{status}]")
        
        # Check specific fields
        issues = check_specific_fields(config_path)
        
        if issues:
            print("\nIssues found:")
            for issue in issues:
                print(f"  - {issue}")
            all_good = False
        else:
            print("\nNo issues found - formatting matches OrcaWave requirements")
        
        print()
    
    print("="*60)
    if all_good:
        print("RESULT: ALL FILES PASS FORMATTING VERIFICATION")
    else:
        print("RESULT: SOME FILES HAVE FORMATTING ISSUES")
    print("="*60)
    
    return all_good

if __name__ == "__main__":
    success = main()
    exit(0 if success else 1)