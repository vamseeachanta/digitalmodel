#!/usr/bin/env python3
"""
Create a simple box geometry in GDF format for OrcaWave testing
This helps verify that OrcaWave can read our GDF files
"""

import numpy as np
from pathlib import Path

def create_simple_box_gdf(output_file, length=10, beam=5, draft=2):
    """
    Create a simple rectangular box in GDF format
    
    Parameters:
    - length: Box length (m)
    - beam: Box width (m)  
    - draft: Box draft/depth (m)
    """
    
    # Define box vertices
    # Box centered at origin, waterline at z=0
    half_length = length / 2
    half_beam = beam / 2
    
    # Create panels as triangles
    # Each face of the box will be 2 triangles
    panels = []
    
    # Bottom face (2 triangles)
    panels.extend([
        [-half_length, -half_beam, -draft],
        [half_length, -half_beam, -draft],
        [half_length, half_beam, -draft],
        
        [-half_length, -half_beam, -draft],
        [half_length, half_beam, -draft],
        [-half_length, half_beam, -draft]
    ])
    
    # Front face (2 triangles)
    panels.extend([
        [-half_length, -half_beam, -draft],
        [-half_length, half_beam, -draft],
        [-half_length, half_beam, 0],
        
        [-half_length, -half_beam, -draft],
        [-half_length, half_beam, 0],
        [-half_length, -half_beam, 0]
    ])
    
    # Back face (2 triangles)
    panels.extend([
        [half_length, -half_beam, -draft],
        [half_length, -half_beam, 0],
        [half_length, half_beam, 0],
        
        [half_length, -half_beam, -draft],
        [half_length, half_beam, 0],
        [half_length, half_beam, -draft]
    ])
    
    # Port side (2 triangles)
    panels.extend([
        [-half_length, -half_beam, -draft],
        [-half_length, -half_beam, 0],
        [half_length, -half_beam, 0],
        
        [-half_length, -half_beam, -draft],
        [half_length, -half_beam, 0],
        [half_length, -half_beam, -draft]
    ])
    
    # Starboard side (2 triangles)
    panels.extend([
        [-half_length, half_beam, -draft],
        [half_length, half_beam, -draft],
        [half_length, half_beam, 0],
        
        [-half_length, half_beam, -draft],
        [half_length, half_beam, 0],
        [-half_length, half_beam, 0]
    ])
    
    # Write GDF file
    with open(output_file, 'w') as f:
        # Header
        f.write("Rhino->WAMIT file export (mesh)\n")
        
        # ULEN and GRAV
        f.write("1 9.80665 \tULEN GRAV\n")
        
        # Symmetry flags (no symmetry for simple test)
        f.write("0  0 \tISX  ISY\n")
        
        # Total number of vertices
        f.write(f"{len(panels)}\n")
        
        # Write vertices
        for vertex in panels:
            f.write(f"  {vertex[0]:.5f} {vertex[1]:.5f} {vertex[2]:.5f}\n")
    
    print(f"Simple box GDF created: {output_file}")
    print(f"Box dimensions: {length}m x {beam}m x {draft}m draft")
    print(f"Total panels: {len(panels)//3} triangles")

def main():
    """Create test geometries"""
    
    base_dir = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/inputs/geometry")
    
    # Create simple box
    simple_box = base_dir / "simple_box_test.gdf"
    create_simple_box_gdf(simple_box, length=10, beam=5, draft=2)
    
    # Create smaller box
    small_box = base_dir / "small_box_test.gdf"
    create_simple_box_gdf(small_box, length=5, beam=3, draft=1)
    
    print("\n=== Test Instructions ===")
    print("1. Open OrcaWave")
    print("2. File -> Import -> Wamit gdf")
    print(f"3. Try loading: {simple_box}")
    print("4. If successful, geometry import format is correct")
    print("5. If not, check OrcaWave error messages")

if __name__ == "__main__":
    main()