#!/usr/bin/env python3
"""
ABOUTME: Generate standalone environment files for all directions
ABOUTME: Creates JONSWAP wave files, current files, and wind files
"""

from pathlib import Path

# Configuration
ENV_DIR = Path("D:/workspace-hub/digitalmodel/projects/TEST_OPERABILITY/orcaflex/base_files/env")
DIRECTIONS = [0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330]

# Return period metocean data (North Sea)
METOCEAN_DATA = {
    '1yr': {
        'wave_hs': 2.5,
        'wave_tz': 5.3,
        'wave_gamma': 3.3,
        'current_speed': 1.4,
        'wind_speed': 20
    },
    '10yr': {
        'wave_hs': 5.0,
        'wave_tz': 7.8,
        'wave_gamma': 3.3,
        'current_speed': 1.9,
        'wind_speed': 30
    },
    '100yr': {
        'wave_hs': 8.5,
        'wave_tz': 9.9,
        'wave_gamma': 3.3,
        'current_speed': 2.3,
        'wind_speed': 37
    }
}

def create_wave_file(direction: int, return_period: str = "1yr"):
    """Create JONSWAP wave file for specific direction."""

    data = METOCEAN_DATA[return_period]
    rp_label = {'1yr': '1-Year', '10yr': '10-Year', '100yr': '100-Year'}[return_period]

    content = f"""# Waves - {direction} deg Direction, {rp_label} Return Period
# North Sea Conditions: Hs={data['wave_hs']}m, Tz={data['wave_tz']}s
# JONSWAP spectrum - Simple, transparent, and industry standard
# Standalone and reusable

KinematicStretchingMethod: Vertical stretching
UserSpecifiedRandomWaveSeeds: No
WaveFrequencySpectrumDiscretisationMethod: Equal energy
WaveTrains:
  - Name: Wave 1
    WaveType: JONSWAP
    WaveDirection: {direction}
    WaveOrigin: [0, 0]
    WaveTimeOrigin: 0
    WaveNumberOfSpectralDirections: 1
    WaveJONSWAPParameters: Partially specified
    WaveHs: {data['wave_hs']}
    WaveTz: {data['wave_tz']}
    WaveGamma: {data['wave_gamma']}
    WaveNumberOfComponents: 100
    WaveSpectrumMinRelFrequency: 0.5
    WaveSpectrumMaxRelFrequency: 10
    WaveSpectrumMaxComponentFrequencyRange: 0.05
# Wave calculation
WaveKinematicsCutoffDepth: Infinity
WaveCalculationMethod: Instantaneous position (exact)
WaveCalculationTimeInterval: 0
WaveCalculationSpatialInterval: 0
"""

    filename = ENV_DIR / f"waves_{direction:03d}deg_{return_period}.yml"
    with open(filename, 'w') as f:
        f.write(content)
    print(f"Created: {filename.name}")

def create_current_file(direction: int, return_period: str = "1yr"):
    """Create current file for specific direction."""

    data = METOCEAN_DATA[return_period]
    rp_label = {'1yr': '1-Year', '10yr': '10-Year', '100yr': '100-Year'}[return_period]

    content = f"""# Current - {direction} deg Direction, {rp_label} Return Period
# North Sea Conditions: Surface speed={data['current_speed']} m/s
# Standalone and reusable

MultipleCurrentDataCanBeDefined: No
CurrentModel: Variation scheme
CurrentRamped: No
CurrentApplyVerticalStretching: No
HorizontalCurrentFactor: ~
VerticalCurrentVariationMethod: Interpolated
RefCurrentSpeed: {data['current_speed']}
RefCurrentDirection: {direction}
CurrentDepth, CurrentFactor, CurrentRotation:
  - [0, 0.4, 0]
  - [45, 0.4, 20]
  - [100, 0.2, 40]
"""

    filename = ENV_DIR / f"current_{direction:03d}deg_{return_period}.yml"
    with open(filename, 'w') as f:
        f.write(content)
    print(f"Created: {filename.name}")

def create_wind_file(direction: int, return_period: str = "1yr"):
    """Create wind file for specific direction."""

    data = METOCEAN_DATA[return_period]
    rp_label = {'1yr': '1-Year', '10yr': '10-Year', '100yr': '100-Year'}[return_period]

    content = f"""# Wind - {direction} deg Direction, {rp_label} Return Period
# North Sea Conditions: Speed={data['wind_speed']} m/s
# Standalone and reusable

IncludeVesselWindLoads: Yes
IncludeLineWindLoads: Yes
IncludeBuoyWindLoads: No
IncludeBuoyWingWindLoads: Yes
WindRamping: From mean
WindType: NPD spectrum
AirDensity: 0.00128
AirSpeedOfSound: 343
WindSpectrumElevation: ~
WindSpectrumFMin: 0
WindSpectrumFMax: Infinity
WindTimeOrigin: 0
WindSeed: 12345
NumberOfWindComponents: 1000
WindSpeed: {data['wind_speed']}
WindDirection: {direction}
VerticalWindVariationFactor: ~
"""

    filename = ENV_DIR / f"wind_{direction:03d}deg_{return_period}.yml"
    with open(filename, 'w') as f:
        f.write(content)
    print(f"Created: {filename.name}")

def main():
    """Generate all environment files."""
    print("=" * 80)
    print("GENERATING STANDALONE ENVIRONMENT FILES")
    print("=" * 80)
    print(f"\nOutput directory: {ENV_DIR}")
    print(f"Directions: {len(DIRECTIONS)} ({DIRECTIONS[0]}° to {DIRECTIONS[-1]}° in 30° steps)")
    print(f"Return Periods: 1-year, 10-year, 100-year")

    print("\nMetocean Conditions by Return Period:")
    for rp, data in METOCEAN_DATA.items():
        rp_label = {'1yr': '1-Year', '10yr': '10-Year', '100yr': '100-Year'}[rp]
        print(f"\n{rp_label}:")
        print(f"  Wave Hs: {data['wave_hs']} m, Tz: {data['wave_tz']} s")
        print(f"  Current: {data['current_speed']} m/s")
        print(f"  Wind: {data['wind_speed']} m/s")

    total_files = 0
    for return_period in ['1yr', '10yr', '100yr']:
        rp_label = {'1yr': '1-Year', '10yr': '10-Year', '100yr': '100-Year'}[return_period]
        print(f"\n{'=' * 80}")
        print(f"Generating {rp_label} Return Period Files")
        print(f"{'=' * 80}")

        print(f"\nGenerating wave files (JONSWAP)...")
        for direction in DIRECTIONS:
            create_wave_file(direction, return_period)
            total_files += 1

        print(f"\nGenerating current files...")
        for direction in DIRECTIONS:
            create_current_file(direction, return_period)
            total_files += 1

        print(f"\nGenerating wind files...")
        for direction in DIRECTIONS:
            create_wind_file(direction, return_period)
            total_files += 1

    print("\n" + "=" * 80)
    print("GENERATION COMPLETE")
    print("=" * 80)
    print(f"\nTotal files created: {total_files}")
    print(f"  - {len(DIRECTIONS) * 3} files per return period")
    print(f"  - {len(DIRECTIONS)} directions × 3 env types × 3 return periods")
    print(f"\nBreakdown:")
    print(f"  - {len(DIRECTIONS) * 3} files for 1-year return period")
    print(f"  - {len(DIRECTIONS) * 3} files for 10-year return period")
    print(f"  - {len(DIRECTIONS) * 3} files for 100-year return period")
    print(f"\nAll files are:")
    print(f"  - Simple and transparent (JONSWAP with explicit Hs/Tz)")
    print(f"  - Standalone and reusable")
    print(f"  - Easy to compare between directions and return periods")

if __name__ == "__main__":
    main()
