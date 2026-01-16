#!/usr/bin/env python
"""Test phase convention imports and functionality."""

import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.modules.marine_analysis import (
    PhaseConvention,
    get_long_period_expectations,
    LONG_PERIOD_EXPECTATIONS_ORCINA,
    LONG_PERIOD_EXPECTATIONS_ISO
)

output = []

output.append("=== Phase Convention Test ===")
output.append("")
output.append("PhaseConvention enum values:")
for pc in PhaseConvention:
    output.append(f"  {pc.name} = {pc.value}")

output.append("")
output.append("=== Surge Long Period Expectations ===")
output.append("")
output.append("Orcina Convention (OrcaFlex/OrcaWave):")
surge_orcina = LONG_PERIOD_EXPECTATIONS_ORCINA['surge']
for heading, exp in surge_orcina.items():
    output.append(f"  {heading}°: amp={exp.expected_amplitude}, phase={exp.expected_phase}°, active={exp.is_active}")

output.append("")
output.append("ISO 6954 Convention (AQWA/WAMIT):")
surge_iso = LONG_PERIOD_EXPECTATIONS_ISO['surge']
for heading, exp in surge_iso.items():
    output.append(f"  {heading}°: amp={exp.expected_amplitude}, phase={exp.expected_phase}°, active={exp.is_active}")

output.append("")
output.append("=== Pitch Long Period Expectations ===")
output.append("")
output.append("Orcina Convention:")
pitch_orcina = LONG_PERIOD_EXPECTATIONS_ORCINA['pitch']
for heading, exp in pitch_orcina.items():
    output.append(f"  {heading}°: amp={exp.expected_amplitude}, phase={exp.expected_phase}°, active={exp.is_active}")

output.append("")
output.append("ISO 6954 Convention:")
pitch_iso = LONG_PERIOD_EXPECTATIONS_ISO['pitch']
for heading, exp in pitch_iso.items():
    output.append(f"  {heading}°: amp={exp.expected_amplitude}, phase={exp.expected_phase}°, active={exp.is_active}")

output.append("")
output.append("=== get_long_period_expectations() Function ===")
output.append("")
exp_orcina = get_long_period_expectations(PhaseConvention.ORCINA)
exp_iso = get_long_period_expectations(PhaseConvention.ISO_6954)
output.append(f"Orcina surge head seas phase: {exp_orcina['surge'][180.0].expected_phase}°")
output.append(f"ISO surge head seas phase: {exp_iso['surge'][180.0].expected_phase}°")
output.append(f"Relationship check: ISO = -Orcina? {exp_iso['surge'][180.0].expected_phase == -exp_orcina['surge'][180.0].expected_phase}")

output.append("")
output.append("=== SUCCESS ===")

# Write to file
with open('phase_convention_test_results.txt', 'w') as f:
    f.write('\n'.join(output))

# Also print
for line in output:
    print(line)
