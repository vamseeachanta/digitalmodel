#!/usr/bin/env python
"""Batch RAO quality check for diverse vessel types."""

import yaml
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.marine_analysis import RAODataValidators
from digitalmodel.marine_analysis.rao_quality_report import RAOQualityReportGenerator

# Diverse vessel type files with actual RAO data
rao_files = [
    'docs/modules/orcawave/examples/L02 OC4 Semi-sub/L02 OC4 Semi-sub_orcaflex.yml',
    'docs/modules/orcawave/examples/L03 Semi-sub multibody analysis/L03 Semi-sub multibody analysis_orcaflex.yml',
    'docs/modules/orcaflex/examples/raw/A01/A01 Catenary riser.yml',
    'docs/modules/orcaflex/examples/raw/A01/A01 Lazy wave riser.yml',
    'docs/modules/orcaflex/examples/raw/A05/A05 Lazy wave with FPSO.yml',
    'docs/modules/orcaflex/examples/raw/C05/C05 Single point mooring.yml',
    'docs/modules/orcaflex/examples/raw/C06/C06 CALM buoy.yml',
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

        # Generate HTML report if checks were done
        html_path = None
        if report.total_checks > 0:
            report_name = path.stem.replace(' ', '_')
            html_path = generator.generate_html_report(report, report_name=report_name + '_quality')

        results.append({
            'file': path.name,
            'vessel_type': report.vessel_type.value,
            'confidence': report.vessel_type_confidence,
            'total': report.total_checks,
            'passed': report.passed_checks,
            'warnings': report.warning_checks,
            'failed': report.failed_checks,
            'pass_rate': report.pass_rate,
            'status': report.overall_status,
            'html_report': str(html_path) if html_path else 'N/A'
        })
    except Exception as e:
        results.append({'file': path.name, 'status': 'ERROR: ' + str(e)[:80]})

# Write results
with open('rao_diverse_results.txt', 'w') as f:
    f.write('RAO QUALITY CHECK - DIVERSE VESSEL TYPES\n')
    f.write('=' * 90 + '\n\n')

    for r in results:
        f.write('File: ' + r['file'] + '\n')
        if 'vessel_type' in r:
            f.write(f"  Vessel Type: {r['vessel_type']} (confidence: {r['confidence']:.1%})\n")
            f.write(f"  Results: {r['passed']}/{r['total']} passed, {r['warnings']} warnings, {r['failed']} failed\n")
            f.write(f"  Pass Rate: {r['pass_rate']:.1f}% | Status: {r['status']}\n")
            if r['html_report'] != 'N/A':
                f.write(f"  Report: {r['html_report']}\n")
        else:
            f.write(f"  Status: {r['status']}\n")
        f.write('\n')

    # Summary
    valid = [r for r in results if 'pass_rate' in r and r['total'] > 0]
    if valid:
        avg = sum(r['pass_rate'] for r in valid) / len(valid)
        by_type = {}
        for r in valid:
            vt = r['vessel_type']
            if vt not in by_type:
                by_type[vt] = []
            by_type[vt].append(r['pass_rate'])

        f.write('=' * 90 + '\n')
        f.write(f'SUMMARY: {len(valid)} files with valid checks\n')
        f.write(f'Average Pass Rate: {avg:.1f}%\n\n')
        f.write('By Vessel Type:\n')
        for vt, rates in sorted(by_type.items()):
            f.write(f'  {vt}: {sum(rates)/len(rates):.1f}% avg ({len(rates)} files)\n')

# Results written to rao_diverse_results.txt
