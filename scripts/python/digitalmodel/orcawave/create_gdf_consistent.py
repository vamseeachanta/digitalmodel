#!/usr/bin/env python
"""
Create GDF files in a consistent manner based on OrcaWave examples
Uses the working GDF format from docs/domains/orcawave/examples
"""

import numpy as np
from pathlib import Path
from typing import List, Tuple, Optional

class GDFGenerator:
    """Generate WAMIT GDF format files consistently"""
    
    def __init__(self):
        self.gravity = 9.80665  # Standard gravity
        self.ulen = 1.0  # Unit length scale
        
    def create_box_mesh(self, 
                        length: float = 10.0,
                        width: float = 5.0, 
                        draft: float = 2.0,
                        nx: int = 4,
                        ny: int = 2,
                        nz: int = 2) -> Tuple[np.ndarray, List[List[float]]]:
        """
        Create a simple box mesh with triangular panels
        
        Args:
            length: Box length (m)
            width: Box width (m)
            draft: Box draft below waterline (m)
            nx: Number of divisions along length
            ny: Number of divisions along width
            nz: Number of divisions along height
            
        Returns:
            vertices: Array of vertex coordinates
            panels: List of triangular panels (3 vertices each)
        """
        # Create vertices
        x = np.linspace(-length/2, length/2, nx + 1)
        y = np.linspace(-width/2, width/2, ny + 1)
        z = np.linspace(-draft, 0.0, nz + 1)
        
        panels = []
        
        # Bottom face (z = -draft)
        for i in range(nx):
            for j in range(ny):
                # Two triangles per rectangle
                v1 = [x[i], y[j], z[0]]
                v2 = [x[i+1], y[j], z[0]]
                v3 = [x[i+1], y[j+1], z[0]]
                v4 = [x[i], y[j+1], z[0]]
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v2[0], v2[1], v2[2]])
                panels.append([v3[0], v3[1], v3[2]])
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v3[0], v3[1], v3[2]])
                panels.append([v4[0], v4[1], v4[2]])
        
        # Side faces
        # Front face (y = -width/2)
        for i in range(nx):
            for k in range(nz):
                v1 = [x[i], y[0], z[k]]
                v2 = [x[i+1], y[0], z[k]]
                v3 = [x[i+1], y[0], z[k+1]]
                v4 = [x[i], y[0], z[k+1]]
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v2[0], v2[1], v2[2]])
                panels.append([v3[0], v3[1], v3[2]])
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v3[0], v3[1], v3[2]])
                panels.append([v4[0], v4[1], v4[2]])
        
        # Back face (y = width/2)
        for i in range(nx):
            for k in range(nz):
                v1 = [x[i], y[-1], z[k]]
                v2 = [x[i+1], y[-1], z[k]]
                v3 = [x[i+1], y[-1], z[k+1]]
                v4 = [x[i], y[-1], z[k+1]]
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v2[0], v2[1], v2[2]])
                panels.append([v3[0], v3[1], v3[2]])
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v3[0], v3[1], v3[2]])
                panels.append([v4[0], v4[1], v4[2]])
        
        # Left face (x = -length/2)
        for j in range(ny):
            for k in range(nz):
                v1 = [x[0], y[j], z[k]]
                v2 = [x[0], y[j+1], z[k]]
                v3 = [x[0], y[j+1], z[k+1]]
                v4 = [x[0], y[j], z[k+1]]
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v2[0], v2[1], v2[2]])
                panels.append([v3[0], v3[1], v3[2]])
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v3[0], v3[1], v3[2]])
                panels.append([v4[0], v4[1], v4[2]])
        
        # Right face (x = length/2)
        for j in range(ny):
            for k in range(nz):
                v1 = [x[-1], y[j], z[k]]
                v2 = [x[-1], y[j+1], z[k]]
                v3 = [x[-1], y[j+1], z[k+1]]
                v4 = [x[-1], y[j], z[k+1]]
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v2[0], v2[1], v2[2]])
                panels.append([v3[0], v3[1], v3[2]])
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v3[0], v3[1], v3[2]])
                panels.append([v4[0], v4[1], v4[2]])
        
        return None, panels
    
    def create_cylinder_mesh(self,
                           radius: float = 5.0,
                           draft: float = 10.0,
                           n_theta: int = 16,
                           n_z: int = 8) -> Tuple[np.ndarray, List[List[float]]]:
        """
        Create a vertical cylinder mesh
        
        Args:
            radius: Cylinder radius (m)
            draft: Draft below waterline (m)
            n_theta: Number of circumferential divisions
            n_z: Number of vertical divisions
            
        Returns:
            vertices: Array of vertex coordinates
            panels: List of triangular panels
        """
        theta = np.linspace(0, 2*np.pi, n_theta + 1)[:-1]  # Exclude last point (same as first)
        z = np.linspace(-draft, 0.0, n_z + 1)
        
        panels = []
        
        # Bottom cap
        for i in range(n_theta):
            v1 = [0, 0, z[0]]
            j = (i + 1) % n_theta
            v2 = [radius * np.cos(theta[i]), radius * np.sin(theta[i]), z[0]]
            v3 = [radius * np.cos(theta[j]), radius * np.sin(theta[j]), z[0]]
            
            panels.append([v1[0], v1[1], v1[2]])
            panels.append([v2[0], v2[1], v2[2]])
            panels.append([v3[0], v3[1], v3[2]])
        
        # Side surface
        for k in range(n_z):
            for i in range(n_theta):
                j = (i + 1) % n_theta
                
                v1 = [radius * np.cos(theta[i]), radius * np.sin(theta[i]), z[k]]
                v2 = [radius * np.cos(theta[j]), radius * np.sin(theta[j]), z[k]]
                v3 = [radius * np.cos(theta[j]), radius * np.sin(theta[j]), z[k+1]]
                v4 = [radius * np.cos(theta[i]), radius * np.sin(theta[i]), z[k+1]]
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v2[0], v2[1], v2[2]])
                panels.append([v3[0], v3[1], v3[2]])
                
                panels.append([v1[0], v1[1], v1[2]])
                panels.append([v3[0], v3[1], v3[2]])
                panels.append([v4[0], v4[1], v4[2]])
        
        return None, panels
    
    def write_gdf_file(self, 
                      panels: List[List[float]],
                      filename: str,
                      title: str = "Generated mesh",
                      isx: int = 0,
                      isy: int = 0) -> str:
        """
        Write panels to WAMIT GDF format file
        
        Args:
            panels: List of triangular panels (each with 3 vertices of x,y,z)
            filename: Output filename
            title: Title line for GDF file
            isx: Symmetry index about xz plane (0=none, 1=symmetric)
            isy: Symmetry index about yz plane (0=none, 1=symmetric)
            
        Returns:
            Path to created file
        """
        output_path = Path(filename)
        
        with open(output_path, 'w') as f:
            # Header
            f.write(f"{title}\n")
            f.write(f"{self.ulen} {self.gravity} \tULEN GRAV\n")
            f.write(f"{isx}  {isy} \tISX  ISY\n")
            
            # Number of panels (triangular panels have 3 vertices each)
            n_panels = len(panels)
            f.write(f"{n_panels}\n")
            
            # Write panel vertices
            for panel in panels:
                f.write(f"  {panel[0]:.5f} {panel[1]:.5f} {panel[2]:.5f}\n")
            
            # Add empty line at end
            f.write("\n")
        
        return str(output_path)
    
    def create_standard_geometries(self, output_dir: str = "."):
        """Create a set of standard geometry files for testing"""
        
        output_dir = Path(output_dir)
        created_files = []
        
        # 1. Simple box (barge-like)
        print("\nCreating simple box (barge)...")
        _, panels = self.create_box_mesh(length=100, width=20, draft=8, nx=10, ny=4, nz=4)
        file1 = self.write_gdf_file(
            panels,
            output_dir / "barge.gdf",
            title="Simple Barge Geometry",
            isx=0,
            isy=1  # Symmetric about yz plane
        )
        created_files.append(file1)
        print(f"  Created: {file1} ({len(panels)} panels)")
        
        # 2. Small test box
        print("\nCreating small test box...")
        _, panels = self.create_box_mesh(length=10, width=6, draft=3, nx=4, ny=3, nz=2)
        file2 = self.write_gdf_file(
            panels,
            output_dir / "test_box.gdf",
            title="Test Box Geometry",
            isx=0,
            isy=0
        )
        created_files.append(file2)
        print(f"  Created: {file2} ({len(panels)} panels)")
        
        # 3. Cylinder (spar-like)
        print("\nCreating cylinder (spar)...")
        _, panels = self.create_cylinder_mesh(radius=10, draft=50, n_theta=24, n_z=10)
        file3 = self.write_gdf_file(
            panels,
            output_dir / "spar.gdf", 
            title="Spar Cylinder Geometry",
            isx=0,
            isy=0
        )
        created_files.append(file3)
        print(f"  Created: {file3} ({len(panels)} panels)")
        
        # 4. Semi-submersible base (simplified)
        print("\nCreating simplified semi-sub columns...")
        panels = []
        
        # Create 4 columns at corners
        column_positions = [
            (20, 20),    # Front right
            (-20, 20),   # Front left
            (-20, -20),  # Back left
            (20, -20)    # Back right
        ]
        
        for x_offset, y_offset in column_positions:
            # Each column is a small cylinder
            theta = np.linspace(0, 2*np.pi, 13)[:-1]  # 12 segments
            z_vals = [-20, -15, -10, -5, 0]  # 5 levels
            
            # Add column panels
            for k in range(len(z_vals) - 1):
                for i in range(12):
                    j = (i + 1) % 12
                    r = 6  # 6m radius columns
                    
                    v1 = [x_offset + r*np.cos(theta[i]), y_offset + r*np.sin(theta[i]), z_vals[k]]
                    v2 = [x_offset + r*np.cos(theta[j]), y_offset + r*np.sin(theta[j]), z_vals[k]]
                    v3 = [x_offset + r*np.cos(theta[j]), y_offset + r*np.sin(theta[j]), z_vals[k+1]]
                    v4 = [x_offset + r*np.cos(theta[i]), y_offset + r*np.sin(theta[i]), z_vals[k+1]]
                    
                    panels.append([v1[0], v1[1], v1[2]])
                    panels.append([v2[0], v2[1], v2[2]])
                    panels.append([v3[0], v3[1], v3[2]])
                    
                    panels.append([v1[0], v1[1], v1[2]])
                    panels.append([v3[0], v3[1], v3[2]])
                    panels.append([v4[0], v4[1], v4[2]])
        
        file4 = self.write_gdf_file(
            panels,
            output_dir / "simple_semisub.gdf",
            title="Simplified Semi-Sub Geometry",
            isx=1,  # Symmetric about xz plane
            isy=1   # Symmetric about yz plane
        )
        created_files.append(file4)
        print(f"  Created: {file4} ({len(panels)} panels)")
        
        return created_files

def use_example_gdf_files():
    """Function to consistently use GDF files from OrcaWave examples"""
    
    examples_dir = Path("D:/github/digitalmodel/docs/domains/orcawave/examples")
    
    available_gdf = {
        "vessel": examples_dir / "L01_default_vessel" / "L01 Vessel mesh.gdf",
        "vessel_cs": examples_dir / "L01_default_vessel" / "L01 Control surface mesh.gdf",
        "oc4_semisub": examples_dir / "L02 OC4 Semi-sub" / "L02 OC4 Semi-sub mesh.gdf",
        "column": examples_dir / "L04 Sectional bodies" / "L04 Column.gdf",
        "pontoon": examples_dir / "L04 Sectional bodies" / "L04 Pontoon.gdf",
        "keystone": examples_dir / "L04 Sectional bodies" / "L04 Keystone.gdf",
        "centre_column": examples_dir / "L03 Semi-sub multibody analysis" / "L03 Centre column.gdf",
        "outer_column": examples_dir / "L03 Semi-sub multibody analysis" / "L03 Outer column.gdf"
    }
    
    # Verify which files exist
    existing_files = {}
    for name, path in available_gdf.items():
        if path.exists():
            existing_files[name] = str(path).replace('/', '\\')
            print(f"[OK] {name}: {path.name}")
        else:
            print(f"[Missing] {name}: {path}")
    
    return existing_files

def create_config_with_gdf(gdf_path: str, config_name: str, body_name: str = "TestBody"):
    """Create OrcaWave config with specified GDF file"""
    
    import yaml
    
    config = {
        'UnitsSystem': 'SI',
        'SolveType': 'Potential formulation only',
        'LoadRAOCalculationMethod': 'Both',
        'PreferredLoadRAOCalculationMethod': 'Haskind',
        'WaterDepth': 200,
        'WaterDensity': 1.025,
        'WavesReferredToBy': 'period (s)',
        'ValidatePanelArrangement': False,
        'PeriodOrFrequency': [6, 8, 10, 12, 15],
        'WaveHeading': [0, 45, 90, 135, 180],
        
        'Bodies': [{
            'BodyName': body_name,
            'BodyMeshFileName': gdf_path,
            'BodyMeshFormat': 'Wamit gdf',
            'BodyMeshLengthUnits': 'm',
            'BodyMeshPosition': [0, 0, 0],
            'BodyMeshAttitude': [0, 0, 0],
            'BodyIncludedInAnalysis': True,
            'BodyMeshSymmetry': 'None',
            
            # Proper inertia specification
            'BodyInertiaSpecifiedBy': 'Radii of gyration (for a free-floating body)',
            'BodyCentreOfMass': [0, 0, -5],
            
            # Radii of gyration - reasonable defaults
            'BodyRadiiOfGyrationRx, BodyRadiiOfGyrationRy, BodyRadiiOfGyrationRz': [
                [10.0, 0, 0],
                [0, 15.0, 0],
                [0, 0, 15.0]
            ],
            'BodyRadiiOfGyrationOriginType': 'Body origin',
            
            # Free floating
            'BodyConnectionParent': 'Free',
            'BodyFixedDOFx': False,
            'BodyFixedDOFy': False,
            'BodyFixedDOFz': False,
            'BodyFixedDOFRx': False,
            'BodyFixedDOFRy': False,
            'BodyFixedDOFRz': False
        }]
    }
    
    with open(config_name, 'w') as f:
        f.write('%YAML 1.1\n')
        f.write(f'# OrcaWave Config using {Path(gdf_path).name}\n')
        f.write('---\n')
        yaml.dump(config, f, default_flow_style=False, sort_keys=False)
    
    print(f"Created config: {config_name}")
    return config_name

def main():
    print("="*60)
    print(" GDF File Generation and Management ")
    print("="*60)
    
    # 1. Show available example GDF files
    print("\n1. AVAILABLE EXAMPLE GDF FILES:")
    print("-" * 40)
    example_files = use_example_gdf_files()
    
    # 2. Generate new consistent GDF files
    print("\n2. GENERATING NEW GDF FILES:")
    print("-" * 40)
    generator = GDFGenerator()
    created_files = generator.create_standard_geometries()
    
    # 3. Create configs for generated files
    print("\n3. CREATING CONFIGS FOR NEW GDF FILES:")
    print("-" * 40)
    
    # Create config for barge
    create_config_with_gdf("barge.gdf", "config_barge.yml", "Barge")
    
    # Create config for test box
    create_config_with_gdf("test_box.gdf", "config_test_box.yml", "TestBox")
    
    # Create config for spar
    create_config_with_gdf("spar.gdf", "config_spar.yml", "Spar")
    
    # 4. Fix the OC4 semi-sub config
    if "oc4_semisub" in example_files:
        print("\n4. FIXING OC4 SEMI-SUB CONFIG:")
        print("-" * 40)
        create_config_with_gdf(
            example_files["oc4_semisub"],
            "config_oc4_fixed.yml",
            "OC4SemiSub"
        )
    
    print("\n" + "="*60)
    print(" SUMMARY ")
    print("="*60)
    print("\nGenerated GDF files:")
    for f in created_files:
        print(f"  - {Path(f).name}")
    
    print("\nCreated configs:")
    print("  - config_barge.yml")
    print("  - config_test_box.yml") 
    print("  - config_spar.yml")
    print("  - config_oc4_fixed.yml (if OC4 GDF exists)")
    
    print("\nThese files use consistent GDF format based on")
    print("working examples from docs/domains/orcawave/examples")
    
    return 0

if __name__ == "__main__":
    main()