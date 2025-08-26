#!/usr/bin/env python3
"""
Comprehensive Geometry Analysis for OrcaWave Files
Shows detailed structure and format for manual verification
"""

import numpy as np
from pathlib import Path
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

def analyze_gdf_structure(gdf_file):
    """Analyze GDF file structure in detail"""
    
    print(f"\n{'='*70}")
    print(f"ANALYZING: {gdf_file.name}")
    print(f"{'='*70}")
    
    with open(gdf_file, 'r') as f:
        lines = f.readlines()
    
    # Parse header
    print("\n1. HEADER SECTION:")
    print(f"   Line 1: {lines[0].strip()}")
    print(f"   Line 2: {lines[1].strip()}")
    print(f"   Line 3: {lines[2].strip()}")
    print(f"   Line 4: {lines[3].strip()}")
    
    # Parse key values
    header = lines[0].strip()
    ulen_grav = lines[1].strip().split()
    symmetry = lines[2].strip().split()
    vertex_count = int(lines[3].strip())
    
    print("\n2. PARSED VALUES:")
    print(f"   Header type: {header}")
    print(f"   Length scale (ULEN): {ulen_grav[0]}")
    print(f"   Gravity (GRAV): {ulen_grav[1] if len(ulen_grav) > 1 else 'N/A'}")
    print(f"   X-symmetry (ISX): {symmetry[0]}")
    print(f"   Y-symmetry (ISY): {symmetry[1] if len(symmetry) > 1 else 'N/A'}")
    print(f"   Declared vertices: {vertex_count}")
    
    # Parse vertices
    vertices = []
    for i in range(4, min(len(lines), 4 + vertex_count)):
        line = lines[i].strip()
        if line:
            try:
                coords = list(map(float, line.split()))
                if len(coords) >= 3:
                    vertices.append(coords[:3])
            except ValueError:
                continue
    
    vertices = np.array(vertices)
    
    print("\n3. GEOMETRY STATISTICS:")
    print(f"   Total vertices parsed: {len(vertices)}")
    print(f"   Vertices match declared: {len(vertices) == vertex_count}")
    
    if len(vertices) > 0:
        # Calculate bounds
        min_coords = vertices.min(axis=0)
        max_coords = vertices.max(axis=0)
        dimensions = max_coords - min_coords
        
        print(f"\n   Bounding Box:")
        print(f"   X: {min_coords[0]:.3f} to {max_coords[0]:.3f} (Length: {dimensions[0]:.3f}m)")
        print(f"   Y: {min_coords[1]:.3f} to {max_coords[1]:.3f} (Beam: {dimensions[1]:.3f}m)")  
        print(f"   Z: {min_coords[2]:.3f} to {max_coords[2]:.3f} (Draft: {abs(min_coords[2]):.3f}m)")
        
        # Panel analysis
        if vertex_count % 3 == 0:
            num_triangles = vertex_count // 3
            print(f"\n   Panel Type: TRIANGLES")
            print(f"   Number of panels: {num_triangles}")
            print(f"   Vertices per panel: 3")
            
            # Sample first panel
            if len(vertices) >= 3:
                print(f"\n   First Panel Vertices:")
                for j in range(3):
                    print(f"     V{j+1}: ({vertices[j][0]:.5f}, {vertices[j][1]:.5f}, {vertices[j][2]:.5f})")
                
                # Calculate first panel area
                v1, v2, v3 = vertices[0], vertices[1], vertices[2]
                area = 0.5 * np.linalg.norm(np.cross(v2-v1, v3-v1))
                print(f"   First Panel Area: {area:.6f} m²")
                
        elif vertex_count % 4 == 0:
            num_quads = vertex_count // 4
            print(f"\n   Panel Type: QUADRILATERALS")
            print(f"   Number of panels: {num_quads}")
            print(f"   Vertices per panel: 4")
        else:
            print(f"\n   Panel Type: MIXED or INDEXED")
            print(f"   Cannot determine panel structure from vertex count")
        
        # Check for duplicate vertices
        unique_vertices = np.unique(vertices, axis=0)
        print(f"\n   Unique vertices: {len(unique_vertices)}")
        print(f"   Duplicate vertices: {len(vertices) - len(unique_vertices)}")
        
        # Estimate volume (simple method)
        if vertex_count % 3 == 0:
            volume = 0
            for i in range(0, len(vertices)-2, 3):
                v1, v2, v3 = vertices[i], vertices[i+1], vertices[i+2]
                # Signed volume contribution
                volume += np.dot(v1, np.cross(v2, v3)) / 6.0
            print(f"\n   Estimated Volume: {abs(volume):.1f} m³")
            print(f"   Estimated Displacement: {abs(volume) * 1.025:.1f} tonnes (seawater)")
    
    # Check file consistency
    print("\n4. FILE FORMAT CHECKS:")
    print(f"   [CHECK] Has header line: {'Yes' if 'WAMIT' in header or 'GDF' in header else 'Check'}")
    print(f"   [CHECK] Has gravity value: {'Yes' if '9.80665' in lines[1] else 'No'}")
    print(f"   [CHECK] Has symmetry flags: {'Yes' if len(symmetry) >= 2 else 'No'}")
    print(f"   [CHECK] Vertex count declared: Yes")
    print(f"   [CHECK] File has sufficient lines: {'Yes' if len(lines) >= 4 + vertex_count else 'No'}")
    
    # Sample of actual vertex data
    print("\n5. VERTEX DATA SAMPLE (Lines 5-10):")
    for i in range(4, min(10, len(lines))):
        print(f"   Line {i+1}: {lines[i].strip()}")
    
    print("\n6. FORMAT COMPATIBILITY:")
    print("   For OrcaWave GDF import:")
    print("   - Header should contain 'WAMIT' or be 'Rhino->WAMIT file export (mesh)'")
    print("   - Line 2 should have: ULEN GRAV (typically '1 9.80665')")
    print("   - Line 3 should have: ISX ISY (0 or 1 for symmetry)")
    print("   - Line 4 should have: Number of vertices")
    print("   - Following lines: X Y Z coordinates (one vertex per line)")
    
    return vertices

def analyze_aqwa_dat(dat_file):
    """Analyze AQWA DAT format"""
    
    print(f"\n{'='*70}")
    print(f"ANALYZING AQWA DAT: {dat_file.name}")
    print(f"{'='*70}")
    
    with open(dat_file, 'r') as f:
        lines = f.readlines()
    
    print("\n1. AQWA DAT HEADER:")
    print(f"   Line 1: {lines[0].strip()}")
    print(f"   Line 2: {lines[1].strip()}")
    print(f"   Line 3: {lines[2].strip()}")
    
    # Parse AQWA format
    line1_parts = lines[0].strip().split()
    scale = float(line1_parts[0])
    gravity = float(line1_parts[1]) if len(line1_parts) > 1 else 9.80665
    
    line2_parts = lines[1].strip().split()
    isx = int(line2_parts[0])
    isy = int(line2_parts[1]) if len(line2_parts) > 1 else 0
    
    num_nodes = int(lines[2].strip())
    
    print("\n2. AQWA PARAMETERS:")
    print(f"   Scale factor: {scale}")
    print(f"   Gravity: {gravity} m/s²")
    print(f"   X-symmetry: {isx}")
    print(f"   Y-symmetry: {isy}")
    print(f"   Number of nodes: {num_nodes}")
    
    # Read nodes
    nodes = []
    for i in range(3, min(len(lines), 3 + num_nodes)):
        try:
            coords = list(map(float, lines[i].strip().split()))
            nodes.append(coords[:3])
        except ValueError:
            continue
    
    nodes = np.array(nodes)
    
    print(f"\n3. NODE STATISTICS:")
    print(f"   Nodes read: {len(nodes)}")
    print(f"   Node range:")
    print(f"   X: {nodes[:,0].min():.3f} to {nodes[:,0].max():.3f}")
    print(f"   Y: {nodes[:,1].min():.3f} to {nodes[:,1].max():.3f}")
    print(f"   Z: {nodes[:,2].min():.3f} to {nodes[:,2].max():.3f}")
    
    # Check for panels section
    panel_start = 3 + num_nodes
    if panel_start < len(lines):
        try:
            num_panels = int(lines[panel_start].strip())
            print(f"\n4. PANEL INFORMATION:")
            print(f"   Number of panels: {num_panels}")
            print(f"   Panel connectivity starts at line: {panel_start + 2}")
            
            # Sample first panel
            if panel_start + 1 < len(lines):
                print(f"   First panel: {lines[panel_start + 1].strip()}")
        except ValueError:
            print("\n4. No panel section found (node-only format)")
    
    return nodes

def create_geometry_visualization(vertices, title="Geometry Visualization"):
    """Create 3D visualization of geometry"""
    
    if len(vertices) == 0:
        print("No vertices to visualize")
        return
    
    # Take every 3 vertices as a triangle
    fig = plt.figure(figsize=(12, 8))
    ax = fig.add_subplot(121, projection='3d')
    
    # Plot vertices as points
    sample_size = min(1000, len(vertices))  # Limit for visualization
    sample_indices = np.linspace(0, len(vertices)-1, sample_size, dtype=int)
    sampled = vertices[sample_indices]
    
    ax.scatter(sampled[:, 0], sampled[:, 1], sampled[:, 2], 
              c=sampled[:, 2], cmap='viridis', s=1, alpha=0.6)
    
    ax.set_xlabel('X (m)')
    ax.set_ylabel('Y (m)')
    ax.set_zlabel('Z (m)')
    ax.set_title(f'{title}\n({len(vertices)} vertices, showing {sample_size})')
    
    # Side view
    ax2 = fig.add_subplot(122)
    ax2.scatter(sampled[:, 0], sampled[:, 2], c=sampled[:, 2], 
               cmap='viridis', s=1, alpha=0.6)
    ax2.set_xlabel('X (m)')
    ax2.set_ylabel('Z (m)')
    ax2.set_title('Side View')
    ax2.grid(True, alpha=0.3)
    ax2.axhline(y=0, color='b', linestyle='--', alpha=0.5, label='Waterline')
    ax2.legend()
    
    plt.tight_layout()
    return fig

def main():
    """Analyze all geometry files"""
    
    base_dir = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/inputs/geometry")
    
    print("="*70)
    print("COMPREHENSIVE GEOMETRY ANALYSIS FOR ORCAWAVE")
    print("="*70)
    
    # Analyze GDF files
    gdf_files = [
        "simple_box_test.gdf",
        "sea_cypress_orcawave.gdf",
        "sea_cypress_trimesh.gdf"
    ]
    
    all_vertices = {}
    
    for gdf_name in gdf_files:
        gdf_file = base_dir / gdf_name
        if gdf_file.exists():
            vertices = analyze_gdf_structure(gdf_file)
            all_vertices[gdf_name] = vertices
        else:
            print(f"\nFile not found: {gdf_name}")
    
    # Analyze AQWA DAT
    dat_file = base_dir / "sea_cypress_gmsh_optimized.dat"
    if dat_file.exists():
        nodes = analyze_aqwa_dat(dat_file)
        all_vertices["AQWA DAT"] = nodes
    
    # Summary comparison
    print("\n" + "="*70)
    print("SUMMARY COMPARISON")
    print("="*70)
    
    print("\n| File | Format | Vertices/Nodes | Panels | Length | Beam | Draft |")
    print("|------|--------|---------------|--------|--------|------|-------|")
    
    for name, verts in all_vertices.items():
        if len(verts) > 0:
            dims = verts.max(axis=0) - verts.min(axis=0)
            panels = len(verts) // 3 if len(verts) % 3 == 0 else "N/A"
            print(f"| {name[:20]:20} | GDF/DAT | {len(verts):13} | {panels:6} | {dims[0]:6.2f} | {dims[1]:4.2f} | {abs(verts[:,2].min()):5.2f} |")
    
    print("\n" + "="*70)
    print("RECOMMENDATIONS FOR MANUAL VERIFICATION")
    print("="*70)
    print("\n1. Check simple_box_test.gdf first:")
    print("   - Should have exactly 30 vertices (10 triangles)")
    print("   - Dimensions: 10m x 5m x 2m draft")
    print("   - All Z coordinates should be <= 0")
    
    print("\n2. For sea_cypress_orcawave.gdf:")
    print("   - Should have 72,996 vertices (24,332 triangles)")
    print("   - Header must be: 'Rhino->WAMIT file export (mesh)'")
    print("   - Line 2 must be: '1 9.80665 	ULEN GRAV'")
    print("   - Dimensions approximately: 23m x 8.6m x 4.3m")
    
    print("\n3. Import test procedure:")
    print("   a. Open OrcaWave")
    print("   b. File -> Import -> Wamit gdf")
    print("   c. Select simple_box_test.gdf")
    print("   d. Should load without errors")
    print("   e. Check 3D view shows a box")
    
    # Create visualizations
    try:
        for name, verts in all_vertices.items():
            if len(verts) > 0 and len(verts) < 100000:  # Skip very large files
                fig = create_geometry_visualization(verts, name)
                output_file = base_dir / f"visualization_{name.replace('.', '_')}.png"
                fig.savefig(output_file, dpi=100)
                print(f"\nVisualization saved: {output_file}")
                plt.close(fig)
    except Exception as e:
        print(f"\nVisualization error: {e}")

if __name__ == "__main__":
    main()