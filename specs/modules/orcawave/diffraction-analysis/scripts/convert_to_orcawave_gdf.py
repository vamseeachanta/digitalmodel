#!/usr/bin/env python3
"""
Convert Sea Cypress STL to OrcaWave-compatible GDF format
Matches the format from L01 Vessel mesh.gdf example
"""

import numpy as np
import trimesh
from pathlib import Path
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def convert_stl_to_orcawave_gdf(stl_file, gdf_file):
    """
    Convert STL to OrcaWave GDF format matching L01 example
    
    Format:
    - Header: "Rhino->WAMIT file export (mesh)"
    - Line 2: ULEN GRAV (1.0 9.80665)
    - Line 3: ISX ISY (symmetry flags)
    - Line 4: Number of vertices
    - Lines 5+: Vertex coordinates (groups of 3 or 4 forming panels)
    """
    
    logger.info(f"Loading STL file: {stl_file}")
    mesh = trimesh.load(str(stl_file))
    
    # Fix mesh orientation
    mesh.fix_normals()
    
    # Get vertices and faces
    vertices = mesh.vertices
    faces = mesh.faces
    
    logger.info(f"Mesh stats: {len(vertices)} vertices, {len(faces)} faces")
    
    # Write GDF file in OrcaWave format
    with open(gdf_file, 'w') as f:
        # Header
        f.write("Rhino->WAMIT file export (mesh)\n")
        
        # ULEN and GRAV
        f.write("1 9.80665 \tULEN GRAV\n")
        
        # Symmetry flags (0 = no symmetry, 1 = symmetry)
        # Using 0 1 for Y-symmetry as in L01 example
        f.write("0  1 \tISX  ISY\n")
        
        # Total number of panel vertices (3 per triangle)
        total_vertices = len(faces) * 3
        f.write(f"{total_vertices}\n")
        
        # Write panel vertices
        # For each face, write its 3 vertices directly
        for face in faces:
            for vertex_idx in face:
                v = vertices[vertex_idx]
                # Format with consistent spacing as in example
                f.write(f"  {v[0]:.5f} {v[1]:.5f} {v[2]:.5f}\n")
    
    logger.info(f"GDF file written: {gdf_file}")
    return True

def verify_gdf_format(gdf_file):
    """Verify the GDF file format"""
    
    with open(gdf_file, 'r') as f:
        lines = f.readlines()
    
    # Check header
    if "WAMIT" not in lines[0]:
        logger.warning("Header doesn't mention WAMIT")
    
    # Check gravity
    if "9.80665" not in lines[1]:
        logger.warning("Gravity value not standard")
    
    # Get vertex count
    vertex_count = int(lines[3].strip())
    logger.info(f"GDF declares {vertex_count} vertices")
    
    # Count actual vertex lines
    actual_vertices = len([l for l in lines[4:] if l.strip() and not l.startswith('#')])
    logger.info(f"GDF contains {actual_vertices} vertex lines")
    
    if vertex_count != actual_vertices:
        logger.error(f"Vertex count mismatch: declared {vertex_count}, actual {actual_vertices}")
        return False
    
    logger.info("GDF format verification passed")
    return True

def main():
    """Main conversion function"""
    
    base_dir = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis")
    
    # Input STL file (using the ASCII version)
    stl_file = base_dir / "inputs/geometry/Sea Cypress_0.25 Mesh_Ascii.stl"
    
    # Output GDF file
    gdf_file = base_dir / "inputs/geometry/sea_cypress_orcawave.gdf"
    
    if not stl_file.exists():
        logger.error(f"STL file not found: {stl_file}")
        return False
    
    # Convert
    success = convert_stl_to_orcawave_gdf(stl_file, gdf_file)
    
    if success:
        # Verify format
        verify_gdf_format(gdf_file)
        
        logger.info("\n=== Next Steps ===")
        logger.info("1. Open OrcaWave")
        logger.info("2. File -> Import -> Wamit gdf")
        logger.info(f"3. Select: {gdf_file}")
        logger.info("4. Verify mesh loads correctly")
        logger.info("5. Run diffraction analysis")
    
    return success

if __name__ == "__main__":
    main()