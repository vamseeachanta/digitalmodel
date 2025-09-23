#!/usr/bin/env python
"""Analyze per-condition weighting (occurrence) calculation"""

import pandas as pd

# Load the fatigue conditions
df = pd.read_csv('input/fatigue_conditions.csv')

print('='*60)
print('PER-CONDITION WEIGHTING (OCCURRENCE) ANALYSIS')
print('='*60)

print('\nFatigue Conditions and Occurrences:')
print('-'*40)

# Show all conditions
for _, row in df.iterrows():
    fc_id = int(row["Row"])
    wind = row["Wind Speed (m/s)"]
    hs = row["Hs (m)"]
    occ = row["Occurrence (%)"]
    print(f'FC{fc_id:03d}: Wind={wind:2.0f} m/s, Hs={hs:.2f} m, Occurrence={occ:5.1f}%')

print('\n' + '='*60)
print('OCCURRENCE PERCENTAGE CALCULATION')
print('='*60)

# Total occurrence
total_occurrence = df['Occurrence (%)'].sum()
print(f'\nTotal Occurrence: {total_occurrence:.1f}%')

if abs(total_occurrence - 100.0) > 0.01:
    print('[WARNING] Total occurrence != 100%')
    print('         Occurrences should sum to 100% for complete coverage')

print('\n' + '='*60)
print('ENVIRONMENTAL CONDITION GROUPING')
print('='*60)

# Group by wind speed
print('\nGrouped by Wind Speed:')
wind_groups = df.groupby('Wind Speed (m/s)')['Occurrence (%)'].sum()
for wind, occ in wind_groups.items():
    print(f'  {wind:2.0f} m/s: {occ:5.1f}% total occurrence')

# Group by wave height
print('\nGrouped by Wave Height (Hs):')
wave_groups = df.groupby('Hs (m)')['Occurrence (%)'].sum()
for hs, occ in wave_groups.items():
    print(f'  {hs:.2f} m: {occ:5.1f}% total occurrence')

print('\n' + '='*60)
print('WEIGHTING INTERPRETATION')
print('='*60)

print('\nThe occurrence percentages represent:')
print('- Fraction of time each sea state occurs annually')
print('- Based on site-specific metocean data')
print('- Typically from 3-10 year hindcast analysis')

print('\nExample Calculation for FC001:')
fc001 = df.iloc[0]
print(f'  Wind: {fc001["Wind Speed (m/s)"]} m/s, Hs: {fc001["Hs (m)"]} m')
print(f'  Occurrence: {fc001["Occurrence (%)"]}%')
print(f'  Means: This condition occurs {fc001["Occurrence (%)"]:.1f}% of the year')
hours_per_year = 365.25 * 24 * fc001["Occurrence (%)"] / 100
print(f'  Duration: {hours_per_year:.1f} hours per year')

print('\n' + '='*60)
print('HOW OCCURRENCE IS TYPICALLY DETERMINED')
print('='*60)

print('\n1. METOCEAN DATA COLLECTION:')
print('   - Gather multi-year wind/wave measurements')
print('   - Use hindcast models or buoy data')
print('   - Typically 3-10 years of hourly data')

print('\n2. SCATTER DIAGRAM CREATION:')
print('   - Bin data by wind speed and wave height')
print('   - Count hours in each bin')
print('   - Calculate percentage of total time')

print('\n3. FATIGUE CONDITIONS SELECTION:')
print('   - Select representative conditions from scatter diagram')
print('   - Ensure coverage of all significant states')
print('   - Weight by actual occurrence frequency')

print('\n4. VALIDATION:')
print('   - Sum should equal 100% (or close)')
print('   - Cover full operational range')
print('   - Include extreme conditions even if rare')

print('\n' + '='*60)
print('CURRENT DATASET ANALYSIS')
print('='*60)

print('\nObservations:')
mild_total = df[df['Wind Speed (m/s)'] <= 10]['Occurrence (%)'].sum()
severe_total = df[df['Wind Speed (m/s)'] >= 15]['Occurrence (%)'].sum()
extreme_total = df[df['Wind Speed (m/s)'] == 20]['Occurrence (%)'].sum()

print(f'1. Mild conditions (5-10 m/s) dominate: {mild_total:.1f}% total')
print(f'2. Severe conditions (15+ m/s): {severe_total:.1f}% total')
print(f'3. Extreme conditions (20 m/s) are rare: {extreme_total:.1f}% total')
print(f'4. Total coverage: {total_occurrence:.1f}%')

print('\nRealism Check:')
if mild_total > 50 and severe_total < 30:
    print('  [PASS] Distribution appears realistic for moderate offshore site')
else:
    print('  [WARN] Distribution may need review')

print('\n' + '='*60)
print('IMPACT ON FATIGUE CALCULATIONS')
print('='*60)

print('\nHow occurrence affects fatigue life:')
print('1. Higher occurrence -> more cycles accumulated annually')
print('2. Rare extreme events (low %) still critical due to large stress ranges')
print('3. Fatigue damage = Sum(occurrence x cycles x damage_per_cycle)')

print('\nExample Impact:')
print('  FC001 (20% occurrence):')
print('    - 100 cycles in sample -> 20.6M cycles/year')
print('  FC004 (5% occurrence):')
print('    - 100 cycles in sample -> 5.2M cycles/year')
print('  FC001 contributes 4x more cycles despite same sample size')