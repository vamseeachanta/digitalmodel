"""Diagnose RAO Parsing Issues"""

if __name__ == '__main__':
    import re
    from pathlib import Path

    lis_file = Path(__file__).parent.parent / 'specs' / 'modules' / 'aqwa' / 'ship-analysis' / 'go-by-ship-raos' / '001_SHIP_RAOS_REV3.LIS'

    with open(lis_file, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()

    # Find displacement RAO sections
    displacement_pattern = re.compile(
        r'(?<!VEL\s)(?<!ACC\s)R\.A\.O\.S-VARIATION\s+WITH\s+WAVE\s+DIRECTION',
        re.IGNORECASE
    )

    matches = list(displacement_pattern.finditer(content))

    print(f'Found {len(matches)} displacement RAO sections')
    print()

    for i, match in enumerate(matches):
        start = match.start()
        # Look at next 2000 characters to see what periods are in this section
        sample = content[start:start+2000]

        # Extract periods from this section
        period_pattern = re.compile(r'^\s*(\d+\.\d+)\s+(\d+\.\d+)\s+(-?\d+\.\d+)', re.MULTILINE)
        periods_in_section = []
        for pm in period_pattern.finditer(sample):
            period = pm.group(1)
            if period not in periods_in_section:
                periods_in_section.append(period)

        print(f'Section {i+1}:')
        print(f'  Position: {start}')
        print(f'  Periods found: {periods_in_section[:5]}...' if len(periods_in_section) > 5 else f'  Periods found: {periods_in_section}')
        print()

    print('='*80)
    print('Checking what periods exist in entire file:')
    print('='*80)

    # Find all unique periods in the file
    period_freq_pattern = re.compile(r'^\s*(\d+\.\d+)\s+(\d+\.\d+)\s+(-?\d+\.\d+)', re.MULTILINE)
    all_periods = set()
    for pm in period_freq_pattern.finditer(content):
        period = pm.group(1)
        all_periods.add(float(period))

    sorted_periods = sorted(all_periods, reverse=True)
    print(f'Total unique periods: {len(sorted_periods)}')
    print(f'Period range: {min(sorted_periods):.2f}s to {max(sorted_periods):.2f}s')
    print(f'All periods: {sorted_periods}')
