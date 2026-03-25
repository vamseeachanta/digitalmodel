#!/usr/bin/env python3
"""
STL to AQWA DAT Converter using GMsh
Converts Sea Cypress STL mesh to OrcaWave/AQWA compatible format
"""

import gmsh
import numpy as np
import os
import sys
from pathlib import Path

def convert_stl_to_aqwa_dat(stl_file: str, output_file: str, target_element_size: float = None):
    """
    Convert STL file to AQWA DAT format with proper mesh optimization
    
    Args:
        stl_file: Path to input STL file
        output_file: Path to output AQWA DAT file
        target_element_size: Target element size for remeshing (optional)
    """
    
    print(f"Converting {stl_file} to AQWA DAT format...")
    
    # Initialize Gmsh
    gmsh.initialize()
    gmsh.option.setNumber("General.Verbosity", 3)
    
    try:
        # Create new model
        gmsh.model.add("sea_cypress")
        
        # Import STL mesh
        print("Importing STL file...")
        gmsh.merge(stl_file)
        
        # Get all surfaces
        surfaces = gmsh.model.getEntities(2)
        print(f"Found {len(surfaces)} surfaces")
        
        # Create physical groups for proper export
        all_surfaces = [s[1] for s in surfaces]
        if all_surfaces:
            gmsh.model.addPhysicalGroup(2, all_surfaces, tag=1)
            gmsh.model.setPhysicalName(2, 1, "Hull")
        
        # Optionally remesh with target element size
        if target_element_size:
            print(f"Remeshing with target element size: {target_element_size}")
            gmsh.option.setNumber("Mesh.CharacteristicLengthMin", target_element_size * 0.5)
            gmsh.option.setNumber("Mesh.CharacteristicLengthMax", target_element_size * 2.0)
            gmsh.option.setNumber("Mesh.CharacteristicLengthFromCurvature", 8)
            
            # Generate 2D mesh
            gmsh.model.mesh.generate(2)
            
        # Check mesh quality
        print("Assessing mesh quality...")
        node_tags, coords, _ = gmsh.model.mesh.getNodes()
        element_tags, element_nodes = gmsh.model.mesh.getElementsByType(2)  # Triangles
        
        print(f"Mesh statistics:")
        print(f"  Nodes: {len(node_tags)}")
        print(f"  Elements: {len(element_tags)}")
        
        # Calculate some basic quality metrics
        if len(element_tags) > 0:
            element_quality = calculate_mesh_quality(coords, node_tags, element_nodes)
            print(f"  Average aspect ratio: {element_quality['avg_aspect_ratio']:.3f}")
            print(f"  Min aspect ratio: {element_quality['min_aspect_ratio']:.3f}")
            print(f"  Max aspect ratio: {element_quality['max_aspect_ratio']:.3f}")
        
        # Export to AQWA format (using MSH as intermediate)
        temp_msh = output_file.replace('.dat', '.msh')
        gmsh.write(temp_msh)
        
        # Convert MSH to AQWA DAT format
        print("Converting to AQWA DAT format...")
        convert_msh_to_aqwa_dat(temp_msh, output_file)
        
        # Clean up temporary file
        if os.path.exists(temp_msh):
            os.remove(temp_msh)
            
        print(f"Successfully converted to: {output_file}")
        
    except Exception as e:
        print(f"Error during conversion: {e}")
        return False
    finally:
        gmsh.finalize()
    
    return True

def calculate_mesh_quality(coords, node_tags, element_nodes):
    """Calculate basic mesh quality metrics"""
    coords_array = np.array(coords).reshape(-1, 3)
    node_map = {node_tags[i]: i for i in range(len(node_tags))}
    
    aspect_ratios = []
    
    # Process triangular elements (assuming triangles)
    elements = element_nodes  # element_nodes already contains the connectivity
    for i in range(0, len(elements), 3):
        n1, n2, n3 = elements[i], elements[i+1], elements[i+2]
        
        if n1 in node_map and n2 in node_map and n3 in node_map:
            p1 = coords_array[node_map[n1]]
            p2 = coords_array[node_map[n2]]
            p3 = coords_array[node_map[n3]]
            
            # Calculate edge lengths
            edge1 = np.linalg.norm(p2 - p1)
            edge2 = np.linalg.norm(p3 - p2)
            edge3 = np.linalg.norm(p1 - p3)
            
            # Calculate aspect ratio
            max_edge = max(edge1, edge2, edge3)
            min_edge = min(edge1, edge2, edge3)
            
            if min_edge > 1e-12:
                aspect_ratio = max_edge / min_edge
                aspect_ratios.append(aspect_ratio)
    
    if aspect_ratios:
        return {
            'avg_aspect_ratio': np.mean(aspect_ratios),
            'min_aspect_ratio': np.min(aspect_ratios),
            'max_aspect_ratio': np.max(aspect_ratios),
            'std_aspect_ratio': np.std(aspect_ratios)
        }
    else:
        return {'avg_aspect_ratio': 0, 'min_aspect_ratio': 0, 'max_aspect_ratio': 0, 'std_aspect_ratio': 0}

def convert_msh_to_aqwa_dat(msh_file: str, dat_file: str):
    """
    Convert Gmsh MSH format to AQWA DAT format
    
    AQWA DAT format:
    Line 1: g acceleration
    Line 2: isx isy (symmetry flags)
    Line 3: number of nodes
    Lines 4+: x y z coordinates for each node
    Then: connectivity information for panels
    """
    
    gmsh.initialize()
    
    try:
        gmsh.open(msh_file)
        
        # Get nodes
        node_tags, node_coords, _ = gmsh.model.mesh.getNodes()
        coords = np.array(node_coords).reshape(-1, 3)
        
        # Get triangular elements
        element_types = gmsh.model.mesh.getElementTypes()
        triangular_elements = []
        
        for elem_type in element_types:
            if elem_type == 2:  # Triangle type
                elem_tags, elem_nodes = gmsh.model.mesh.getElementsByType(elem_type)
                triangular_elements.extend(elem_nodes.reshape(-1, 3))
        
        # Write AQWA DAT file
        with open(dat_file, 'w') as f:
            # Header
            f.write("1.0 9.80665\n")  # g (gravity acceleration)
            f.write("0 0\n")          # isx isy (no symmetry)
            f.write(f"{len(node_tags)}\n")  # number of nodes
            
            # Node coordinates
            node_map = {node_tags[i]: i+1 for i in range(len(node_tags))}  # 1-based indexing
            
            for i, node_id in enumerate(node_tags):
                x, y, z = coords[i]
                f.write(f"{x:12.6f} {y:12.6f} {z:12.6f}\n")
            
            # Panel connectivity (triangular elements)
            f.write(f"{len(triangular_elements)}\n")  # number of panels
            
            for triangle in triangular_elements:
                # Convert to 1-based indexing and write triangle
                n1 = node_map[triangle[0]]
                n2 = node_map[triangle[1]]  
                n3 = node_map[triangle[2]]
                f.write(f"{n1:8d} {n2:8d} {n3:8d}\n")
                
        print(f"AQWA DAT file written: {dat_file}")
        print(f"  Nodes: {len(node_tags)}")
        print(f"  Panels: {len(triangular_elements)}")
        
    except Exception as e:
        print(f"Error converting MSH to DAT: {e}")
        raise
    finally:
        gmsh.finalize()

def main():
    """Main conversion function"""
    
    # Set up file paths
    base_dir = Path("D:/github/digitalmodel")
    input_dir = base_dir / "specs/modules/orcawave/diffraction-analysis/inputs/geometry"
    
    # Use ASCII STL file for better compatibility
    stl_file = input_dir / "Sea Cypress_0.25 Mesh_Ascii.stl"
    output_file = input_dir / "sea_cypress_gmsh_optimized.dat"
    
    if not stl_file.exists():
        # Fallback to binary STL
        stl_file = input_dir / "Sea Cypress_0.25 Mesh_Binary.stl"
        
    if not stl_file.exists():
        print(f"Error: STL file not found at {stl_file}")
        sys.exit(1)
    
    print("=== Sea Cypress STL to AQWA DAT Conversion ===")
    print(f"Input: {stl_file}")
    print(f"Output: {output_file}")
    print()
    
    # Convert with appropriate element size for 30m vessel
    # Target ~1-2m element size for reasonable computation time
    target_size = 1.5  # meters
    
    success = convert_stl_to_aqwa_dat(
        str(stl_file), 
        str(output_file),
        target_element_size=target_size
    )
    
    if success:
        print("\n=== Conversion Complete ===")
        print(f"AQWA DAT file ready for OrcaWave: {output_file}")
        print("\nNext steps:")
        print("1. Load the DAT file in OrcaWave")
        print("2. Set up the sea state and wave frequencies")
        print("3. Run the diffraction analysis")
        print("4. Export results for use in OrcaFlex")
    else:
        print("Conversion failed!")
        sys.exit(1)

if __name__ == "__main__":
    main()