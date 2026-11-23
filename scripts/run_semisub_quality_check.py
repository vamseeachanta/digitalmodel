#!/usr/bin/env python
"""Run RAO quality check on L02 OC4 Semi-sub model."""

import yaml
from pathlib import Path

# Import after module setup to avoid stdout issues
from digitalmodel.marine_analysis import (
    RAODataValidators,
    VesselType,
    PhaseConvention
)

def main():
    # Load the FPSO model with RAO data
    model_path = Path('docs/domains/orcaflex/examples/raw/A05/A05 Lazy wave with FPSO.yml')

    output_lines = []
    output_lines.append(f'Loading: {model_path}')

    # Load multi-document YAML
    with open(model_path, 'r', encoding='utf-8-sig') as mf:
        documents = list(yaml.safe_load_all(mf))

    # Merge all documents into one dict
    rao_data = {}
    for doc in documents:
        if doc:
            rao_data.update(doc)

    # Initialize validators
    validators = RAODataValidators()

    # Run quality check
    report = validators.validate_displacement_rao_quality(
        rao_data,
        source_file=str(model_path),
        vessel_type=None,  # Auto-detect
        amplitude_tolerance=0.05,
        phase_tolerance=10.0
    )

    # Build output
    output_lines.append('')
    output_lines.append('=' * 60)
    output_lines.append('RAO QUALITY CHECK RESULTS')
    output_lines.append('=' * 60)
    output_lines.append(f'Model: {model_path.name}')
    output_lines.append(f'Detected Vessel Type: {report.vessel_type.value}')
    output_lines.append(f'Vessel Type Confidence: {report.vessel_type_confidence:.1%}')
    convention = report.phase_convention.value if hasattr(report, 'phase_convention') else 'orcina'
    output_lines.append(f'Phase Convention: {convention}')
    output_lines.append('')
    output_lines.append(f'Overall Status: {report.overall_status}')
    output_lines.append(f'Pass Rate: {report.pass_rate:.1f}%')
    output_lines.append(f'Total Checks: {report.total_checks}')
    output_lines.append(f'Passed: {report.passed_checks}')
    output_lines.append(f'Warnings: {report.warning_checks}')
    output_lines.append(f'Failed: {report.failed_checks}')

    # Phase check details
    output_lines.append('')
    output_lines.append('=' * 60)
    output_lines.append('PHASE CHECK RESULTS')
    output_lines.append('=' * 60)
    output_lines.append(f'{"DOF":8} {"Heading":>8} {"Expected":>10} {"Actual":>10} {"Error":>8} {"Status":>8}')
    output_lines.append(f'{"-"*8} {"-"*8} {"-"*10} {"-"*10} {"-"*8} {"-"*8}')

    for check in report.phase_checks:
        status_icon = '[PASS]' if check.status == 'PASS' else ('[WARN]' if check.status == 'WARNING' else '[FAIL]')
        output_lines.append(f'{check.dof:8} {check.heading:8.1f} {check.expected_phase:10.1f} {check.actual_phase:10.1f} {check.phase_error:8.1f} {status_icon}')

    # Peak detection details
    output_lines.append('')
    output_lines.append('=' * 60)
    output_lines.append('PEAK DETECTION RESULTS')
    output_lines.append('=' * 60)
    output_lines.append(f'{"DOF":8} {"Peak":>10} {"Expected Range":>16} {"Status":>8} Message')
    output_lines.append(f'{"-"*8} {"-"*10} {"-"*16} {"-"*8} {"-"*30}')

    for check in report.peak_checks:
        status_icon = '[PASS]' if check.status == 'PASS' else ('[WARN]' if check.status == 'WARNING' else '[FAIL]')
        peak_str = f'{check.peak_period:.1f}s' if check.peak_period else 'None'
        range_str = f'{check.expected_range[0]:.0f}-{check.expected_range[1]:.0f}s'
        output_lines.append(f'{check.dof:8} {peak_str:>10} {range_str:>16} {status_icon} {check.message}')

    output_lines.append('')
    output_lines.append('=' * 60)
    output_lines.append('Quality check complete.')

    # Write to file
    output_path = Path('reports/rao_quality_fpso_results.txt')
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, 'w') as f:
        f.write('\n'.join(output_lines))

    return output_path


if __name__ == '__main__':
    output_file = main()
