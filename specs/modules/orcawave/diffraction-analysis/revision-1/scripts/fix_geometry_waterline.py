#!/usr/bin/env python3
"""
Fix Sea Cypress geometry to be at correct waterline position
The current geometry is entirely above water (Z: 0.501 to 4.776)
Need to translate it down so vessel sits at proper draft
"""

import numpy as np
from pathlib import Path

def fix_waterline_position(input_gdf, output_gdf, target_draft=3.0):
    """
    Fix geometry to sit at correct waterline
    
    Parameters:
    - input_gdf: Input GDF file with geometry above water
    - output_gdf: Output GDF file with corrected position
    - target_draft: Desired draft (m) - how deep the vessel should sit
    """
    
    print(f"Reading: {input_gdf}")
    
    with open(input_gdf, 'r') as f:
        lines = f.readlines()
    
    # Parse header
    header = lines[0]
    ulen_grav = lines[1]
    symmetry = lines[2]
    vertex_count = int(lines[3].strip())
    
    print(f"Processing {vertex_count} vertices...")
    
    # Parse all vertices
    vertices = []
    for i in range(4, 4 + vertex_count):
        coords = list(map(float, lines[i].strip().split()))
        vertices.append(coords)
    
    vertices = np.array(vertices)
    
    # Analyze current position
    z_min = vertices[:, 2].min()
    z_max = vertices[:, 2].max()
    z_range = z_max - z_min
    
    print(f"\nCurrent Z range: {z_min:.3f} to {z_max:.3f} (height: {z_range:.3f}m)")
    print(f"Problem: Vessel is entirely above water!")
    
    # Calculate translation needed
    # We want the bottom of the vessel to be at -target_draft
    # and the top to be above water
    translation_z = -target_draft - z_min
    
    print(f"\nApplying Z translation: {translation_z:.3f}m")
    
    # Apply translation
    vertices[:, 2] += translation_z
    
    # Verify new position
    new_z_min = vertices[:, 2].min()
    new_z_max = vertices[:, 2].max()
    
    print(f"New Z range: {new_z_min:.3f} to {new_z_max:.3f}")
    print(f"Draft: {abs(new_z_min):.3f}m")
    print(f"Freeboard: {new_z_max:.3f}m")
    
    # Write corrected GDF
    with open(output_gdf, 'w') as f:
        # Write header (unchanged)
        f.write(header)
        f.write(ulen_grav)
        f.write(symmetry)
        f.write(f"{vertex_count}\n")
        
        # Write corrected vertices
        for vertex in vertices:
            f.write(f"  {vertex[0]:.5f} {vertex[1]:.5f} {vertex[2]:.5f}\n")
    
    print(f"\nCorrected geometry saved to: {output_gdf}")
    
    # Also create a version with even more draft for testing
    deep_draft_file = output_gdf.parent / output_gdf.name.replace('.gdf', '_deep_draft.gdf')
    
    # Apply additional translation for 4m draft
    vertices[:, 2] -= 1.0  # Additional 1m deeper
    
    with open(deep_draft_file, 'w') as f:
        f.write(header)
        f.write(ulen_grav)
        f.write(symmetry)
        f.write(f"{vertex_count}\n")
        
        for vertex in vertices:
            f.write(f"  {vertex[0]:.5f} {vertex[1]:.5f} {vertex[2]:.5f}\n")
    
    print(f"Alternative deep draft version: {deep_draft_file}")
    
    return vertices

def create_waterline_check_file():
    """Create a simple floating box at correct waterline for testing"""
    
    output_file = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/inputs/geometry/waterline_test_box.gdf")
    
    # Create a 10x5x3m box with 2m draft
    vertices = [
        # Bottom (at -2m)
        [-5, -2.5, -2], [5, -2.5, -2], [5, 2.5, -2],
        [-5, -2.5, -2], [5, 2.5, -2], [-5, 2.5, -2],
        
        # Front face (underwater part)
        [-5, -2.5, -2], [-5, 2.5, -2], [-5, 2.5, 0],
        [-5, -2.5, -2], [-5, 2.5, 0], [-5, -2.5, 0],
        
        # Front face (above water part)  
        [-5, -2.5, 0], [-5, 2.5, 0], [-5, 2.5, 1],
        [-5, -2.5, 0], [-5, 2.5, 1], [-5, -2.5, 1],
        
        # Back face (underwater)
        [5, -2.5, -2], [5, -2.5, 0], [5, 2.5, 0],
        [5, -2.5, -2], [5, 2.5, 0], [5, 2.5, -2],
        
        # Back face (above water)
        [5, -2.5, 0], [5, -2.5, 1], [5, 2.5, 1],
        [5, -2.5, 0], [5, 2.5, 1], [5, 2.5, 0],
        
        # Port side (underwater)
        [-5, -2.5, -2], [-5, -2.5, 0], [5, -2.5, 0],
        [-5, -2.5, -2], [5, -2.5, 0], [5, -2.5, -2],
        
        # Port side (above water)
        [-5, -2.5, 0], [-5, -2.5, 1], [5, -2.5, 1],
        [-5, -2.5, 0], [5, -2.5, 1], [5, -2.5, 0],
        
        # Starboard side (underwater)
        [-5, 2.5, -2], [5, 2.5, -2], [5, 2.5, 0],
        [-5, 2.5, -2], [5, 2.5, 0], [-5, 2.5, 0],
        
        # Starboard side (above water)
        [-5, 2.5, 0], [5, 2.5, 0], [5, 2.5, 1],
        [-5, 2.5, 0], [5, 2.5, 1], [-5, 2.5, 1],
        
        # Top (above water at +1m)
        [-5, -2.5, 1], [-5, 2.5, 1], [5, 2.5, 1],
        [-5, -2.5, 1], [5, 2.5, 1], [5, -2.5, 1]
    ]
    
    with open(output_file, 'w') as f:
        f.write("Rhino->WAMIT file export (mesh)\n")
        f.write("1 9.80665 \tULEN GRAV\n")
        f.write("0  0 \tISX  ISY\n")
        f.write(f"{len(vertices)}\n")
        
        for vertex in vertices:
            f.write(f"  {vertex[0]:.5f} {vertex[1]:.5f} {vertex[2]:.5f}\n")
    
    print(f"\nWaterline test box created: {output_file}")
    print("This box has:")
    print("  - 2m draft (below water)")
    print("  - 1m freeboard (above water)")
    print("  - Waterline at Z=0")
    
def main():
    """Fix all geometry files"""
    
    base_dir = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/inputs/geometry")
    
    print("="*70)
    print("FIXING GEOMETRY WATERLINE POSITION")
    print("="*70)
    
    # Fix the main Sea Cypress geometry
    input_file = base_dir / "sea_cypress_orcawave.gdf"
    output_file = base_dir / "sea_cypress_corrected.gdf"
    
    if input_file.exists():
        fix_waterline_position(input_file, output_file, target_draft=3.0)
    else:
        print(f"Input file not found: {input_file}")
    
    # Create waterline test box
    create_waterline_check_file()
    
    print("\n" + "="*70)
    print("GEOMETRY FILES READY FOR TESTING:")
    print("="*70)
    print("\n1. waterline_test_box.gdf - Simple box with correct waterline")
    print("2. sea_cypress_corrected.gdf - Sea Cypress at 3m draft")
    print("3. sea_cypress_corrected_deep_draft.gdf - Sea Cypress at 4m draft")
    print("\nTest these in OrcaWave to verify waterline position is correct")

if __name__ == "__main__":
    main()