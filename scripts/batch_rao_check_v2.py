#!/usr/bin/env python
"""Batch RAO quality check with phase convention support."""

import yaml
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.modules.marine_analysis import RAODataValidators, PhaseConvention
from digitalmodel.modules.marine_analysis.rao_quality_report import RAOQualityReportGenerator

# Test files with actual RAO data
rao_files = [
    'docs/modules/orcawave/examples/L02 OC4 Semi-sub/L02 OC4 Semi-sub_orcaflex.yml',
    'docs/modules/orcawave/examples/L03 Semi-sub multibody analysis/L03 Semi-sub multibody analysis_orcaflex.yml',
]

validators = RAODataValidators()
generator = RAOQualityReportGenerator(output_dir='docs/reports/rao_qa')

results = []

for rao_file in rao_files:
    path = Path(rao_file)
    if not path.exists():
        results.append({'file': path.name, 'status': 'FILE_NOT_FOUND'})
        continue

    try:
        with open(path, 'r', encoding='utf-8') as f:
            rao_data = yaml.safe_load(f)

        report = validators.validate_displacement_rao_quality(
            rao_data, source_file=str(path)
        )

        results.append({
            'file': path.name,
            'vessel_type': report.vessel_type.value,
            'confidence': report.vessel_type_confidence,
            'phase_convention': report.phase_convention.value,
            'total': report.total_checks,
            'passed': report.passed_checks,
            'warnings': report.warning_checks,
            'failed': report.failed_checks,
            'pass_rate': report.pass_rate,
            'status': report.overall_status,
        })

        # Print detailed phase check results
        results.append({'file': path.name + ' - Phase Checks:', 'details': True})
        for check in report.phase_checks:
            results.append({
                'file': f"  {check.dof} @ {check.heading}°",
                'status': check.status,
                'actual_phase': check.actual_phase,
                'expected_phase': check.expected_phase,
                'phase_error': check.phase_error,
            })

    except Exception as e:
        results.append({'file': path.name, 'status': 'ERROR: ' + str(e)[:80]})

# Write results
with open('rao_phase_convention_results.txt', 'w') as f:
    f.write('RAO QUALITY CHECK - PHASE CONVENTION TEST\n')
    f.write('=' * 90 + '\n\n')

    for r in results:
        if r.get('details'):
            f.write(f"\n{r['file']}\n")
        elif 'actual_phase' in r:
            f.write(f"{r['file']}: {r['status']} "
                   f"(actual={r['actual_phase']:.1f}°, expected={r['expected_phase']:.1f}°, error={r['phase_error']:.1f}°)\n")
        elif 'vessel_type' in r:
            f.write(f"\nFile: {r['file']}\n")
            f.write(f"  Vessel Type: {r['vessel_type']} (confidence: {r['confidence']:.1%})\n")
            f.write(f"  Phase Convention: {r['phase_convention']}\n")
            f.write(f"  Results: {r['passed']}/{r['total']} passed, {r['warnings']} warnings, {r['failed']} failed\n")
            f.write(f"  Pass Rate: {r['pass_rate']:.1f}% | Status: {r['status']}\n")
        else:
            f.write(f"File: {r['file']}\n")
            f.write(f"  Status: {r['status']}\n")

# Print results summary
with open('rao_phase_convention_results.txt', 'r') as f:
    print(f.read())
