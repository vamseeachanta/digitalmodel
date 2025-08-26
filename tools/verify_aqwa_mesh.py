#!/usr/bin/env python3
"""
AQWA Mesh Verification Script
Verifies the quality and correctness of AQWA DAT files for OrcaWave
"""

import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

def load_aqwa_dat(dat_file):
    """Load AQWA DAT file and parse geometry"""
    
    print(f"Loading AQWA DAT file: {dat_file}")
    
    with open(dat_file, 'r') as f:
        lines = f.readlines()
    
    # Parse header
    line_idx = 0
    g_accel = float(lines[line_idx].split()[1])
    print(f"Gravity acceleration: {g_accel} m/s²")
    line_idx += 1
    
    # Symmetry flags
    isx, isy = map(int, lines[line_idx].split())
    print(f"Symmetry flags - X: {isx}, Y: {isy}")
    line_idx += 1
    
    # Number of nodes
    num_nodes = int(lines[line_idx].strip())
    print(f"Number of nodes: {num_nodes}")
    line_idx += 1
    
    # Read node coordinates
    nodes = []
    for i in range(num_nodes):
        coords = list(map(float, lines[line_idx + i].split()))
        nodes.append(coords)
    line_idx += num_nodes
    
    nodes = np.array(nodes)
    print(f"Node coordinates range:")
    print(f"  X: {nodes[:, 0].min():.3f} to {nodes[:, 0].max():.3f}")
    print(f"  Y: {nodes[:, 1].min():.3f} to {nodes[:, 1].max():.3f}")
    print(f"  Z: {nodes[:, 2].min():.3f} to {nodes[:, 2].max():.3f}")
    
    # Number of panels
    num_panels = int(lines[line_idx].strip())
    print(f"Number of panels: {num_panels}")
    line_idx += 1
    
    # Read panel connectivity
    panels = []
    for i in range(num_panels):
        connectivity = list(map(int, lines[line_idx + i].split()))
        # Convert from 1-based to 0-based indexing
        panels.append([c - 1 for c in connectivity])
    
    panels = np.array(panels)
    
    return {
        'nodes': nodes,
        'panels': panels,
        'num_nodes': num_nodes,
        'num_panels': num_panels,
        'g_accel': g_accel,
        'symmetry': (isx, isy)
    }

def calculate_mesh_metrics(mesh_data):
    """Calculate comprehensive mesh quality metrics"""
    
    nodes = mesh_data['nodes']
    panels = mesh_data['panels']
    
    print("\n=== Mesh Quality Analysis ===")
    
    # Calculate panel areas and aspect ratios
    areas = []
    aspect_ratios = []
    normals = []
    
    for panel in panels:
        if len(panel) == 3:  # Triangle
            p1, p2, p3 = nodes[panel[0]], nodes[panel[1]], nodes[panel[2]]
            
            # Calculate vectors
            v1 = p2 - p1
            v2 = p3 - p1
            
            # Calculate normal (cross product)
            normal = np.cross(v1, v2)
            normal_mag = np.linalg.norm(normal)
            
            if normal_mag > 1e-12:
                # Area is half the magnitude of cross product
                area = normal_mag / 2.0
                areas.append(area)
                
                # Normalized normal
                normals.append(normal / normal_mag)
                
                # Edge lengths for aspect ratio
                edge1 = np.linalg.norm(v1)
                edge2 = np.linalg.norm(v2)
                edge3 = np.linalg.norm(p3 - p2)
                
                max_edge = max(edge1, edge2, edge3)
                min_edge = min(edge1, edge2, edge3)
                
                if min_edge > 1e-12:
                    aspect_ratio = max_edge / min_edge
                    aspect_ratios.append(aspect_ratio)
    
    areas = np.array(areas)
    aspect_ratios = np.array(aspect_ratios)
    normals = np.array(normals)
    
    # Panel area statistics
    print(f"Panel Areas:")
    print(f"  Total surface area: {np.sum(areas):.3f} m²")
    print(f"  Average panel area: {np.mean(areas):.6f} m²")
    print(f"  Min panel area: {np.min(areas):.6f} m²")
    print(f"  Max panel area: {np.max(areas):.6f} m²")
    print(f"  Area standard deviation: {np.std(areas):.6f} m²")
    
    # Aspect ratio statistics
    print(f"\nAspect Ratios:")
    print(f"  Average: {np.mean(aspect_ratios):.3f}")
    print(f"  Min: {np.min(aspect_ratios):.3f}")
    print(f"  Max: {np.max(aspect_ratios):.3f}")
    print(f"  Standard deviation: {np.std(aspect_ratios):.3f}")
    
    # Quality assessment
    good_aspect = np.sum(aspect_ratios < 3.0)
    fair_aspect = np.sum((aspect_ratios >= 3.0) & (aspect_ratios < 5.0))
    poor_aspect = np.sum(aspect_ratios >= 5.0)
    
    print(f"\nMesh Quality Assessment:")
    print(f"  Good aspect ratio (< 3.0): {good_aspect} panels ({100*good_aspect/len(aspect_ratios):.1f}%)")
    print(f"  Fair aspect ratio (3.0-5.0): {fair_aspect} panels ({100*fair_aspect/len(aspect_ratios):.1f}%)")
    print(f"  Poor aspect ratio (> 5.0): {poor_aspect} panels ({100*poor_aspect/len(aspect_ratios):.1f}%)")
    
    # Check for consistent normal directions (should point outward)
    z_normals = normals[:, 2]  # Z-component of normals
    outward_normals = np.sum(z_normals > 0)  # Assuming most normals should point up for a floating vessel
    
    print(f"\nNormal Direction Check:")
    print(f"  Normals pointing upward: {outward_normals} ({100*outward_normals/len(normals):.1f}%)")
    
    # Vessel dimension estimates
    print(f"\nVessel Dimensions (from mesh extents):")
    x_extent = nodes[:, 0].max() - nodes[:, 0].min()
    y_extent = nodes[:, 1].max() - nodes[:, 1].min()  
    z_extent = nodes[:, 2].max() - nodes[:, 2].min()
    
    print(f"  Length (X): {x_extent:.2f} m")
    print(f"  Beam (Y): {y_extent:.2f} m") 
    print(f"  Height (Z): {z_extent:.2f} m")
    
    # Volume estimation (assuming closed mesh)
    # Simple volume estimate using divergence theorem
    volume_est = estimate_volume(nodes, panels)
    print(f"  Estimated volume: {volume_est:.1f} m³")
    
    return {
        'areas': areas,
        'aspect_ratios': aspect_ratios,
        'normals': normals,
        'dimensions': (x_extent, y_extent, z_extent),
        'volume': volume_est
    }

def estimate_volume(nodes, panels):
    """Estimate volume using divergence theorem"""
    volume = 0.0
    
    for panel in panels:
        if len(panel) == 3:
            p1, p2, p3 = nodes[panel[0]], nodes[panel[1]], nodes[panel[2]]
            
            # Triangle centroid
            centroid = (p1 + p2 + p3) / 3.0
            
            # Triangle area vector
            area_vec = 0.5 * np.cross(p2 - p1, p3 - p1)
            
            # Volume contribution
            volume += np.dot(centroid, area_vec)
    
    return abs(volume / 3.0)  # Final division by 3

def main():
    """Main verification function"""
    
    # File paths
    base_dir = Path("D:/github/digitalmodel")
    geometry_dir = base_dir / "specs/modules/orcawave/diffraction-analysis/inputs/geometry"
    
    # Check both the GMsh optimized version and the original trimesh version
    files_to_check = [
        geometry_dir / "sea_cypress_gmsh_optimized.dat",
        geometry_dir / "sea_cypress_trimesh.gdf"  # If it's actually in DAT format
    ]
    
    for dat_file in files_to_check:
        if dat_file.exists() and dat_file.suffix in ['.dat', '.gdf']:
            print(f"\n{'='*60}")
            print(f"VERIFYING: {dat_file.name}")
            print(f"{'='*60}")
            
            try:
                # Load and verify
                mesh_data = load_aqwa_dat(str(dat_file))
                metrics = calculate_mesh_metrics(mesh_data)
                
                # Recommendations
                print(f"\n=== Recommendations for OrcaWave ===")
                
                if np.mean(metrics['aspect_ratios']) < 3.0:
                    print("✓ Good mesh quality - suitable for OrcaWave analysis")
                elif np.mean(metrics['aspect_ratios']) < 5.0:
                    print("⚠ Acceptable mesh quality - monitor convergence in OrcaWave")
                else:
                    print("⚠ Poor mesh quality - consider remeshing with smaller elements")
                
                if metrics['volume'] > 300 and metrics['volume'] < 600:
                    print("✓ Reasonable volume estimate for Sea Cypress vessel")
                else:
                    print(f"⚠ Volume estimate {metrics['volume']:.1f} m³ - verify mesh is watertight")
                
                length, beam, height = metrics['dimensions']
                if 25 < length < 35 and 8 < beam < 12:
                    print("✓ Dimensions consistent with Sea Cypress specifications")
                else:
                    print("⚠ Check vessel dimensions - may need coordinate transformation")
                    
            except Exception as e:
                print(f"Error processing {dat_file}: {e}")
                continue

if __name__ == "__main__":
    main()