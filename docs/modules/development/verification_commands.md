# DataFrame Verification Commands

When the script stops at breakpoints, use these commands in the debugger to manually verify the DataFrames:

## At AQWA Breakpoint:

```python
# Inspect the AQWA DataFrames
print("AQWA Amplitude DataFrame Info:")
print(aqwa_dataframes['amplitude'].info())

# Check specific values
print("\nSurge amplitude at all headings for first frequency:")
print(aqwa_dataframes['amplitude'].loc[aqwa_dataframes['amplitude'].index[0], 'surge'])

# Check phase data
print("\nSurge phase at all headings for first frequency:")
print(aqwa_dataframes['phase'].loc[aqwa_dataframes['phase'].index[0], 'surge'])

# View metadata
print("\nMetadata:")
import pprint
pprint.pprint(aqwa_dataframes['metadata'])

# Check for missing data
print("\nMissing values in amplitude:")
print(aqwa_dataframes['amplitude'].isnull().sum().sum())

# Sample verification: Check if values match source
# First frequency should be 0.157 rad/s (T=40s), heading -180°, surge amp ≈ 0.0249
print(f"\nFirst data point verification:")
print(f"Frequency: {aqwa_dataframes['amplitude'].index[0]:.3f} rad/s (expected ~0.157)")
print(f"Surge amplitude at -180°: {aqwa_dataframes['amplitude'].loc[aqwa_dataframes['amplitude'].index[0], ('surge', '-180.0°')]:.4f} (expected ~0.0249)")
```

## At OrcaFlex Breakpoint:

```python
# Inspect the OrcaFlex DataFrames
print("OrcaFlex Amplitude DataFrame Info:")
print(orcaflex_dataframes['amplitude'].info())

# Check specific values
print("\nSurge amplitude at all headings for first frequency:")
print(orcaflex_dataframes['amplitude'].loc[orcaflex_dataframes['amplitude'].index[0], 'surge'])

# Check phase data
print("\nSurge phase at all headings for first frequency:")
print(orcaflex_dataframes['phase'].loc[orcaflex_dataframes['phase'].index[0], 'surge'])

# View metadata
print("\nMetadata:")
import pprint
pprint.pprint(orcaflex_dataframes['metadata'])

# Check for missing data
print("\nMissing values in amplitude:")
print(orcaflex_dataframes['amplitude'].isnull().sum().sum())

# Sample verification: Check if values match source
# First row of data should have period=3s, so freq=2π/3≈2.094, heading 0°, surge amp=0.009
print(f"\nFirst data point verification:")
print(f"Frequency: {orcaflex_dataframes['amplitude'].index[0]:.3f} rad/s")
print(f"Surge amplitude at 0°: {orcaflex_dataframes['amplitude'].loc[orcaflex_dataframes['amplitude'].index[0], ('surge', '0.0°')]:.4f}")
```

## At Final Comparison Breakpoint:

```python
# Compare the two datasets
print("Comparison Summary:")
print(f"AQWA: {aqwa_dataframes['amplitude'].shape[0]} frequencies, {len(aqwa_dataframes['amplitude'].columns.get_level_values('Heading').unique())} headings")
print(f"OrcaFlex: {orcaflex_dataframes['amplitude'].shape[0]} frequencies, {len(orcaflex_dataframes['amplitude'].columns.get_level_values('Heading').unique())} headings")

# Check frequency ranges
print(f"\nFrequency ranges:")
print(f"AQWA: {aqwa_dataframes['amplitude'].index.min():.3f} to {aqwa_dataframes['amplitude'].index.max():.3f} rad/s")
print(f"OrcaFlex: {orcaflex_dataframes['amplitude'].index.min():.3f} to {orcaflex_dataframes['amplitude'].index.max():.3f} rad/s")

# Check heading coverage
print(f"\nHeading coverage:")
print(f"AQWA: {sorted([float(h[:-1]) for h in aqwa_dataframes['amplitude'].columns.get_level_values('Heading').unique()])}")
print(f"OrcaFlex: {sorted([float(h[:-1]) for h in orcaflex_dataframes['amplitude'].columns.get_level_values('Heading').unique()])}")

# Statistical checks
print(f"\nAmplitude ranges:")
print(f"AQWA: {aqwa_dataframes['amplitude'].min().min():.6f} to {aqwa_dataframes['amplitude'].max().max():.6f}")
print(f"OrcaFlex: {orcaflex_dataframes['amplitude'].min().min():.6f} to {orcaflex_dataframes['amplitude'].max().max():.6f}")
```

## Continue Execution:
Type `c` and press Enter to continue to the next breakpoint or end the script.

## Exit Debugger:
Type `q` and press Enter to quit the debugger.