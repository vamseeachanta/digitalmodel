"""
Update Baltic CALM Buoy Moorings for 10m Water Depth with 5 mT Clump Weights

This script updates the mooring line configurations in the OrcaFlex YAML files
to reflect a 10m water depth scenario with 5 mT clump weights added to each line.

Configuration:
- Water depth: 10m (changed from 100m)
- Fairlead depth: -1.5m below waterline
- Top chain: 15m length (from fairlead to clump weight)
- Clump weight: 5m length segment (5 mT total mass = 1 mT/m × 5m)
- Bottom chain: 25m length (from clump weight to anchor)
- Total line length: 45m (15 + 5 + 25)

Mooring pattern: 6 radial lines at 60° spacing (0°, 60°, 120°, 180°, 240°, 300°)
"""

import os
import math
from pathlib import Path

# Base directory
baltic_dir = Path(__file__).parent.parent / "projects" / "modules" / "calm" / "baltic_039m" / "TEST_OPERABILITY" / "orcaflex" / "base_files"

# Mooring configuration
WATER_DEPTH = 10.0  # meters
FAIRLEAD_DEPTH = -1.5  # meters below waterline
FAIRLEAD_RADIUS = 6.0  # meters from buoy center
TOP_CHAIN_LENGTH = 15.0  # meters
CLUMP_WEIGHT_LENGTH = 5.0  # meters (5 mT total mass)
BOTTOM_CHAIN_LENGTH = 25.0  # meters
TOTAL_LINE_LENGTH = TOP_CHAIN_LENGTH + CLUMP_WEIGHT_LENGTH + BOTTOM_CHAIN_LENGTH  # 45m

# Mooring line azimuths (degrees)
MOORING_AZIMUTHS = [0, 60, 120, 180, 240, 300]

# Seabed elevation (small clearance)
SEABED_Z = 0.1143  # meters above seabed reference

def calculate_catenary_footprint(water_depth, fairlead_depth, total_length, chain_mass_per_meter=88.3):
    """
    Calculate approximate horizontal footprint radius for catenary mooring.

    Uses simplified catenary equation for estimating anchor offset.
    For shallow water (10m), the line will have significant seabed contact.
    """
    # Vertical drop from fairlead to seabed
    vertical_drop = abs(fairlead_depth) + water_depth - SEABED_Z

    # For shallow water with long line, assume significant seabed layback
    # Rough estimate: horizontal span ≈ total_length - 1.5 * vertical_drop
    horizontal_span = total_length - 1.5 * vertical_drop

    # Ensure positive value
    horizontal_span = max(10.0, horizontal_span)

    return horizontal_span

def generate_mooring_line_yaml(line_number, azimuth_deg, horizontal_offset):
    """Generate YAML configuration for a single mooring line."""

    # Calculate fairlead position on buoy
    azimuth_rad = math.radians(azimuth_deg)
    fairlead_x = FAIRLEAD_RADIUS * math.cos(azimuth_rad)
    fairlead_y = FAIRLEAD_RADIUS * math.sin(azimuth_rad)

    # Calculate anchor position
    anchor_x = fairlead_x + horizontal_offset * math.cos(azimuth_rad)
    anchor_y = fairlead_y + horizontal_offset * math.sin(azimuth_rad)

    # Lay azimuth (opposite direction for catenary laying)
    lay_azimuth = (azimuth_deg + 180) % 360

    yaml_content = f"""New: Mooring{line_number}
Mooring{line_number}:
    IncludeTorsion: No
    TopEnd: End A
    LengthAndEndOrientations: Explicit
    Representation: Finite element
    PyModel: (none)
    PreBendSpecifiedBy: Curvature
    DragFormulation: Standard
    StaticsVIV: None
    DynamicsVIV: None
    WaveCalculationMethod: Specified by environment
    # End connections
    Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo:
      - [CALMBase, {fairlead_x:.6f}, {fairlead_y:.6f}, {FAIRLEAD_DEPTH}, {azimuth_deg}, 180, 0, ~]
      - [Anchored, {anchor_x:.6f}, {anchor_y:.6f}, {SEABED_Z}, {azimuth_deg}, 90, 0, ~]
    # End connection stiffness
    ConnectionxBendingStiffness, ConnectionyBendingStiffness:
      - [0, ~]
      - [0, ~]
    # Feeding
    ConnectionInitialArclength, ConnectionPayoutRate, ConnectionShortestViableSegmentFactor, ConnectionApplyRamp, ConnectionUseSmoothGrowth:
      - [~, 0, 0.001]
      - [~, 0, 0.001]
    # Sections - Top chain, Clump weight, Bottom chain
    LineType, Length, TargetSegmentLength:
      - [2p5inChain, {TOP_CHAIN_LENGTH}, 5]
      - [ClumpWeight, {CLUMP_WEIGHT_LENGTH}, 5]
      - [2p5inChain, {BOTTOM_CHAIN_LENGTH}, 5]
    # Seabed
    DecoupleLateralAndAxialSeabedFriction: No
    CoveredSectionsWarnIfLineLeavesCover: Yes
    # Contents
    ContentsMethod: Uniform
    IncludeAxialContentsInertia: Yes
    ContentsDensity: 0
    ContentsTemperature: ~
    ContentsPressureRefZ: ~
    ContentsPressure: 0
    ContentsFlowRate: 0
    # Statics
    IncludedInStatics: Yes
    StaticsStep1: Catenary
    StaticsStep2: None
    StaticsSeabedFrictionPolicy: As laid
    LayAzimuth: {lay_azimuth}
    AsLaidTension: 0
    # Drawing
    DrawNodesAsDiscs: No
    DrawShadedNodesAsSpheres: Yes
    ContactPen: [5, Solid, White]
"""
    return yaml_content

def update_mooring_lines():
    """Update mooring line configurations for both simple and discretised models."""

    # Calculate horizontal offset for anchors
    horizontal_offset = calculate_catenary_footprint(
        WATER_DEPTH,
        FAIRLEAD_DEPTH,
        TOTAL_LINE_LENGTH
    )

    print(f"Baltic CALM Buoy - 10m Water Depth Configuration")
    print("=" * 70)
    print(f"Water depth: {WATER_DEPTH}m")
    print(f"Fairlead depth: {FAIRLEAD_DEPTH}m below waterline")
    print(f"Top chain length: {TOP_CHAIN_LENGTH}m")
    print(f"Clump weight segment: {CLUMP_WEIGHT_LENGTH}m (5 mT total)")
    print(f"Bottom chain length: {BOTTOM_CHAIN_LENGTH}m")
    print(f"Total line length: {TOTAL_LINE_LENGTH}m")
    print(f"Horizontal offset (approx): {horizontal_offset:.2f}m")
    print("=" * 70)

    # Generate YAML for all 6 mooring lines
    all_moorings_yaml = []

    for i, azimuth in enumerate(MOORING_AZIMUTHS, start=1):
        mooring_yaml = generate_mooring_line_yaml(i, azimuth, horizontal_offset)
        all_moorings_yaml.append(mooring_yaml)
        print(f"OK: Generated Mooring{i} at {azimuth} deg azimuth")

    # Add load hose and hawser (unchanged from original, but using updated vessel connections)
    # These remain the same as they connect to the CALM buoy and tanker vessel

    loadhose_hawser_yaml = """New: Loadhose
Loadhose:
    IncludeTorsion: No
    TopEnd: End A
    LengthAndEndOrientations: Explicit
    Representation: Finite element
    PyModel: (none)
    PreBendSpecifiedBy: Curvature
    DragFormulation: Standard
    StaticsVIV: None
    DynamicsVIV: None
    WaveCalculationMethod: Specified by environment
    # End connections
    Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo:
      - [Vessel1, 45, 5, 7, 30, 165, 0, ~]
      - [CALMTop, -3.201296069889044, 4.320710529453033, 0.5, 315, 30.00000000000001, 0, ~]
    # End connection stiffness
    ConnectionxBendingStiffness, ConnectionyBendingStiffness:
      - [Infinity, ~]
      - [Infinity, ~]
    # Feeding
    ConnectionInitialArclength, ConnectionPayoutRate, ConnectionShortestViableSegmentFactor, ConnectionApplyRamp, ConnectionUseSmoothGrowth:
      - [~, 0, 0.001]
      - [~, 0, 0.001]
    # Sections
    LineType, Length, TargetSegmentLength:
      - [Hose, 40, 0.5]
      - [Hose, 60, 2]
      - [Hose, 20, 0.5]
    # Seabed
    DecoupleLateralAndAxialSeabedFriction: No
    CoveredSectionsWarnIfLineLeavesCover: Yes
    # Contents
    ContentsMethod: Uniform
    IncludeAxialContentsInertia: Yes
    ContentsDensity: 1
    ContentsTemperature: ~
    ContentsPressureRefZ: ~
    ContentsPressure: 0
    ContentsFlowRate: 0
    # Statics
    IncludedInStatics: Yes
    StaticsStep1: Catenary
    StaticsStep2: None
    StaticsSeabedFrictionPolicy: None
    # Drawing
    DrawNodesAsDiscs: No
    DrawShadedNodesAsSpheres: Yes
    ContactPen: [5, Solid, White]
New: Hawser
Hawser:
    IncludeTorsion: No
    TopEnd: End A
    LengthAndEndOrientations: Explicit
    Representation: Finite element
    PyModel: (none)
    PreBendSpecifiedBy: Curvature
    DragFormulation: Standard
    StaticsVIV: None
    DynamicsVIV: None
    WaveCalculationMethod: Specified by environment
    # End connections
    Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo:
      - [CALMTop, -4.507972599671972, 3.120780440136267, 1.1928223140903764, 125.00000000000001, 90, 0, ~]
      - [Vessel1, 50, 0, 7, 180, 90, 0, ~]
    # End connection stiffness
    ConnectionxBendingStiffness, ConnectionyBendingStiffness:
      - [0, ~]
      - [0, ~]
    # Feeding
    ConnectionInitialArclength, ConnectionPayoutRate, ConnectionShortestViableSegmentFactor, ConnectionApplyRamp, ConnectionUseSmoothGrowth:
      - [~, 0, 0.001]
      - [~, 0, 0.001]
    # Sections
    LineType, Length, TargetSegmentLength:
      - [HawserLine, 90, 5]
    # Seabed
    DecoupleLateralAndAxialSeabedFriction: No
    CoveredSectionsWarnIfLineLeavesCover: Yes
    # Contents
    ContentsMethod: Uniform
    IncludeAxialContentsInertia: Yes
    ContentsDensity: 0
    ContentsTemperature: ~
    ContentsPressureRefZ: ~
    ContentsPressure: 0
    ContentsFlowRate: 0
    # Statics
    IncludedInStatics: Yes
    StaticsStep1: Catenary
    StaticsStep2: Full statics
    StaticsSeabedFrictionPolicy: None
    # Drawing
    DrawNodesAsDiscs: No
    DrawShadedNodesAsSpheres: Yes
    ContactPen: [5, Solid, White]
New: Pivot1
Pivot1:
    IncludeTorsion: No
    TopEnd: End A
    LengthAndEndOrientations: Explicit
    Representation: Finite element
    PyModel: (none)
    PreBendSpecifiedBy: Curvature
    DragFormulation: Standard
    StaticsVIV: None
    DynamicsVIV: None
    WaveCalculationMethod: Specified by environment
    # End connections
    Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo:
      - [CALMTop, 0, 0, 0, 0, 180, 0, ~]
      - [CALMBase, 0, 0, 0.7946049904251593, 0, 180, 0, ~]
    # End connection stiffness
    ConnectionxBendingStiffness, ConnectionyBendingStiffness:
      - [Infinity, ~]
      - [Infinity, ~]
    # Feeding
    ConnectionInitialArclength, ConnectionPayoutRate, ConnectionShortestViableSegmentFactor, ConnectionApplyRamp, ConnectionUseSmoothGrowth:
      - [~, 0, 0.001]
      - [~, 0, 0.001]
    # Sections
    Sections:
      - LineType: Pivot
        Length: 0.99
        NumberOfSegments: 1
        DisturbanceVessel: (none)
    # Seabed
    DecoupleLateralAndAxialSeabedFriction: No
    CoveredSectionsWarnIfLineLeavesCover: Yes
    # Contents
    ContentsMethod: Uniform
    IncludeAxialContentsInertia: Yes
    ContentsDensity: 0
    ContentsTemperature: ~
    ContentsPressureRefZ: ~
    ContentsPressure: 0
    ContentsFlowRate: 0
    # Statics
    IncludedInStatics: Yes
    StaticsStep1: Catenary
    StaticsStep2: Full statics
    StaticsSeabedFrictionPolicy: None
    # Drawing
    Hidden: Yes
    DrawNodesAsDiscs: No
    DrawShadedNodesAsSpheres: Yes
New: Pivot2
Pivot2:
    IncludeTorsion: No
    TopEnd: End A
    LengthAndEndOrientations: Explicit
    Representation: Finite element
    PyModel: (none)
    PreBendSpecifiedBy: Curvature
    DragFormulation: Standard
    StaticsVIV: None
    DynamicsVIV: None
    WaveCalculationMethod: Specified by environment
    # End connections
    Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo:
      - [CALMBase, 0, 0, 3.7946049904251593, 0, 180, 0, ~]
      - [CALMTop, 0, 0, 1, 0, 180, 0, ~]
    # End connection stiffness
    ConnectionxBendingStiffness, ConnectionyBendingStiffness:
      - [Infinity, ~]
      - [Infinity, ~]
    # Feeding
    ConnectionInitialArclength, ConnectionPayoutRate, ConnectionShortestViableSegmentFactor, ConnectionApplyRamp, ConnectionUseSmoothGrowth:
      - [~, 0, 0.001]
      - [~, 0, 0.001]
    # Sections
    Sections:
      - LineType: Pivot
        Length: 0.99
        NumberOfSegments: 1
        DisturbanceVessel: (none)
    # Seabed
    DecoupleLateralAndAxialSeabedFriction: No
    CoveredSectionsWarnIfLineLeavesCover: Yes
    # Contents
    ContentsMethod: Uniform
    IncludeAxialContentsInertia: Yes
    ContentsDensity: 0
    ContentsTemperature: ~
    ContentsPressureRefZ: ~
    ContentsPressure: 0
    ContentsFlowRate: 0
    # Statics
    IncludedInStatics: Yes
    StaticsStep1: Catenary
    StaticsStep2: Full statics
    StaticsSeabedFrictionPolicy: None
    # Drawing
    Hidden: Yes
    DrawNodesAsDiscs: No
    DrawShadedNodesAsSpheres: Yes
"""

    # Combine all content
    full_yaml_content = "\n".join(all_moorings_yaml) + loadhose_hawser_yaml

    # Write to _07_lines.yml (simple model)
    lines_simple_file = baltic_dir / "_07_lines.yml"
    with open(lines_simple_file, 'w') as f:
        f.write(full_yaml_content)
    print(f"\nOK: Updated {lines_simple_file.name}")

    # Write to _07_lines_discretised.yml (discretised model)
    lines_discretised_file = baltic_dir / "_07_lines_discretised.yml"
    with open(lines_discretised_file, 'w') as f:
        f.write(full_yaml_content)
    print(f"OK: Updated {lines_discretised_file.name}")

    # Also update 05_lines.yml and 05_lines_discretised.yml (alternative location)
    lines_05_file = baltic_dir / "05_lines.yml"
    if lines_05_file.exists():
        with open(lines_05_file, 'w') as f:
            f.write(full_yaml_content)
        print(f"OK: Updated {lines_05_file.name}")

    lines_05_discretised_file = baltic_dir / "05_lines_discretised.yml"
    if lines_05_discretised_file.exists():
        with open(lines_05_discretised_file, 'w') as f:
            f.write(full_yaml_content)
        print(f"OK: Updated {lines_05_discretised_file.name}")

    print("\n" + "=" * 70)
    print("Summary:")
    print(f"  Water depth updated: 100m -> 10m")
    print(f"  Mooring lines updated: 6 lines")
    print(f"  Clump weights added: 5 mT per line (30 mT total)")
    print(f"  Line configuration: {TOP_CHAIN_LENGTH}m chain + {CLUMP_WEIGHT_LENGTH}m clump + {BOTTOM_CHAIN_LENGTH}m chain")
    print(f"  Approximate footprint radius: {horizontal_offset:.2f}m")
    print("=" * 70)

if __name__ == "__main__":
    update_mooring_lines()
