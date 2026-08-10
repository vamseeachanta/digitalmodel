#!/usr/bin/env python
"""
Test OrcaWave validation tab features
Simulates what OrcaWave checks in its Validation tab
"""

import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Any
import yaml

class OrcaWaveValidationTab:
    """Simulates OrcaWave's Validation tab checks"""
    
    def __init__(self):
        self.warnings = []
        self.errors = []
        self.info = []
        
    def parse_gdf_mesh(self, gdf_path: str) -> Dict[str, Any]:
        """Parse GDF mesh file and extract panel information"""
        
        mesh_data = {
            "file": gdf_path,
            "exists": False,
            "panels": [],
            "vertices": [],
            "symmetry": [0, 0],
            "stats": {}
        }
        
        gdf = Path(gdf_path)
        if not gdf.exists():
            gdf = Path(gdf.name)
            
        if not gdf.exists():
            return mesh_data
            
        mesh_data["exists"] = True
        
        with open(gdf, 'r') as f:
            lines = f.readlines()
            
        if len(lines) < 4:
            return mesh_data
            
        # Parse header
        # Line 2: ULEN GRAV
        # Line 3: ISX ISY (symmetry)
        symmetry_line = lines[2].strip().split()
        mesh_data["symmetry"] = [int(symmetry_line[0]), int(symmetry_line[1])]
        
        # Line 4: Number of panels
        n_panels = int(lines[3].strip())
        
        # Parse panel vertices (triangular panels, 3 vertices each)
        panels = []
        for i in range(n_panels):
            if 4 + i < len(lines):
                coords = lines[4 + i].strip().split()
                if len(coords) >= 3:
                    vertex = [float(coords[0]), float(coords[1]), float(coords[2])]
                    panels.append(vertex)
                    
        # Group into triangles (every 3 vertices)
        triangles = []
        for i in range(0, len(panels), 3):
            if i + 2 < len(panels):
                triangles.append([panels[i], panels[i+1], panels[i+2]])
                
        mesh_data["panels"] = triangles
        mesh_data["vertices"] = panels
        
        return mesh_data
    
    def calculate_panel_metrics(self, triangle: List[List[float]]) -> Dict[str, float]:
        """Calculate quality metrics for a triangular panel"""
        
        if len(triangle) != 3:
            return {}
            
        v1, v2, v3 = [np.array(v) for v in triangle]
        
        # Edge lengths
        edge1 = np.linalg.norm(v2 - v1)
        edge2 = np.linalg.norm(v3 - v2)
        edge3 = np.linalg.norm(v1 - v3)
        
        # Area (using cross product)
        area = 0.5 * np.linalg.norm(np.cross(v2 - v1, v3 - v1))
        
        # Aspect ratio (longest edge / shortest edge)
        aspect_ratio = max(edge1, edge2, edge3) / min(edge1, edge2, edge3) if min(edge1, edge2, edge3) > 0 else np.inf
        
        # Normal vector
        normal = np.cross(v2 - v1, v3 - v1)
        if np.linalg.norm(normal) > 0:
            normal = normal / np.linalg.norm(normal)
        
        # Skewness (deviation from equilateral)
        perimeter = edge1 + edge2 + edge3
        ideal_area = (np.sqrt(3) / 4) * (perimeter / 3) ** 2
        skewness = 1 - (area / ideal_area) if ideal_area > 0 else 1
        
        return {
            "area": area,
            "aspect_ratio": aspect_ratio,
            "max_edge": max(edge1, edge2, edge3),
            "min_edge": min(edge1, edge2, edge3),
            "skewness": skewness,
            "normal": normal.tolist() if isinstance(normal, np.ndarray) else [0, 0, 0]
        }
    
    def check_waterline_intersection(self, mesh_data: Dict) -> Dict[str, Any]:
        """Check for panels crossing the waterline"""
        
        result = {
            "panels_above": 0,
            "panels_below": 0,
            "panels_crossing": 0,
            "waterline_gap": None
        }
        
        for triangle in mesh_data["panels"]:
            z_values = [v[2] for v in triangle]
            
            if all(z > 0 for z in z_values):
                result["panels_above"] += 1
            elif all(z < 0 for z in z_values):
                result["panels_below"] += 1
            else:
                result["panels_crossing"] += 1
                
        # Check for gap at waterline
        all_z = [v[2] for panel in mesh_data["panels"] for v in panel]
        if all_z:
            z_near_waterline = [z for z in all_z if abs(z) < 0.1]
            if not z_near_waterline:
                min_above = min([z for z in all_z if z > 0], default=None)
                max_below = max([z for z in all_z if z < 0], default=None)
                if min_above is not None and max_below is not None:
                    result["waterline_gap"] = min_above - max_below
                    
        return result
    
    def calculate_body_properties(self, mesh_data: Dict) -> Dict[str, float]:
        """Calculate body volume and center of buoyancy"""
        
        properties = {
            "volume": 0,
            "center_of_buoyancy": [0, 0, 0],
            "wetted_area": 0,
            "bbox": {
                "min": [0, 0, 0],
                "max": [0, 0, 0]
            }
        }
        
        if not mesh_data["panels"]:
            return properties
            
        # Calculate bounding box
        all_vertices = [v for panel in mesh_data["panels"] for v in panel]
        x_coords = [v[0] for v in all_vertices]
        y_coords = [v[1] for v in all_vertices]
        z_coords = [v[2] for v in all_vertices]
        
        properties["bbox"]["min"] = [min(x_coords), min(y_coords), min(z_coords)]
        properties["bbox"]["max"] = [max(x_coords), max(y_coords), max(z_coords)]
        
        # Calculate volume (simplified - assumes closed surface)
        volume = 0
        cb = np.zeros(3)
        
        for triangle in mesh_data["panels"]:
            if triangle[0][2] < 0:  # Below waterline
                metrics = self.calculate_panel_metrics(triangle)
                properties["wetted_area"] += metrics.get("area", 0)
                
                # Approximate volume contribution
                center = np.mean(triangle, axis=0)
                vol_contrib = metrics.get("area", 0) * abs(center[2]) / 3
                volume += vol_contrib
                cb += center * vol_contrib
                
        properties["volume"] = volume
        if volume > 0:
            properties["center_of_buoyancy"] = (cb / volume).tolist()
            
        return properties
    
    def validate_mesh(self, gdf_path: str) -> Dict[str, Any]:
        """Run full mesh validation checks"""
        
        print(f"\nValidating mesh: {Path(gdf_path).name}")
        print("-" * 40)
        
        # Parse mesh
        mesh_data = self.parse_gdf_mesh(gdf_path)
        
        if not mesh_data["exists"]:
            print("  [ERROR] Mesh file not found")
            return {"valid": False, "errors": ["File not found"]}
            
        n_panels = len(mesh_data["panels"])
        print(f"  Panels: {n_panels}")
        
        if mesh_data["symmetry"][0] or mesh_data["symmetry"][1]:
            sym_desc = []
            if mesh_data["symmetry"][0]:
                sym_desc.append("xz-plane")
            if mesh_data["symmetry"][1]:
                sym_desc.append("yz-plane")
            print(f"  Symmetry: {', '.join(sym_desc)}")
            
        # Panel quality checks
        print("\n  Panel Quality Checks:")
        
        aspect_ratios = []
        areas = []
        skewness_values = []
        
        for triangle in mesh_data["panels"]:
            metrics = self.calculate_panel_metrics(triangle)
            if metrics:
                aspect_ratios.append(metrics["aspect_ratio"])
                areas.append(metrics["area"])
                skewness_values.append(metrics["skewness"])
                
        if aspect_ratios:
            max_ar = max(aspect_ratios)
            avg_ar = np.mean(aspect_ratios)
            
            if max_ar > 25:
                print(f"    [WARNING] High aspect ratio: {max_ar:.1f} (limit: 25)")
            else:
                print(f"    [OK] Aspect ratios: max={max_ar:.1f}, avg={avg_ar:.1f}")
                
            if min(areas) < 0.001:
                print(f"    [WARNING] Very small panels detected: {min(areas):.6f} m²")
                
            high_skew = sum(1 for s in skewness_values if s > 0.8)
            if high_skew > 0:
                print(f"    [WARNING] {high_skew} highly skewed panels (>0.8)")
                
        # Waterline checks
        print("\n  Waterline Checks:")
        waterline = self.check_waterline_intersection(mesh_data)
        
        print(f"    Panels below waterline: {waterline['panels_below']}")
        print(f"    Panels above waterline: {waterline['panels_above']}")
        
        if waterline["panels_crossing"] > 0:
            print(f"    [WARNING] {waterline['panels_crossing']} panels cross waterline")
            
        if waterline["waterline_gap"] and waterline["waterline_gap"] > 0.1:
            print(f"    [WARNING] Gap at waterline: {waterline['waterline_gap']:.3f} m")
            
        # Body properties
        print("\n  Body Properties:")
        props = self.calculate_body_properties(mesh_data)
        
        dimensions = [
            props["bbox"]["max"][0] - props["bbox"]["min"][0],
            props["bbox"]["max"][1] - props["bbox"]["min"][1],
            props["bbox"]["max"][2] - props["bbox"]["min"][2]
        ]
        
        print(f"    Dimensions: {dimensions[0]:.1f} x {dimensions[1]:.1f} x {dimensions[2]:.1f} m")
        print(f"    Draft: {abs(props['bbox']['min'][2]):.1f} m")
        print(f"    Wetted area: {props['wetted_area']:.1f} m²")
        
        if props["volume"] > 0:
            print(f"    Approx. volume: {props['volume']:.1f} m³")
            cb = props["center_of_buoyancy"]
            print(f"    Center of buoyancy: ({cb[0]:.2f}, {cb[1]:.2f}, {cb[2]:.2f})")
            
        # Panel density check
        if n_panels > 0 and props["wetted_area"] > 0:
            panel_density = n_panels / props["wetted_area"]
            print(f"\n  Panel Density: {panel_density:.2f} panels/m²")
            
            if panel_density < 0.1:
                print(f"    [WARNING] Low panel density")
            elif panel_density > 10:
                print(f"    [WARNING] High panel density")
                
        return {"valid": True, "mesh_data": mesh_data}
    
    def validate_config(self, config_path: str):
        """Validate full OrcaWave configuration"""
        
        print("\n" + "="*60)
        print(f" VALIDATING: {config_path}")
        print("="*60)
        
        # Load configuration
        with open(config_path, 'r', encoding='utf-8-sig') as f:
            content = f.read()
            
        lines = content.split('\n')
        yaml_content = '\n'.join([line for line in lines if not line.startswith('%YAML')])
        config = yaml.safe_load(yaml_content)
        
        # Check each body
        if 'Bodies' in config:
            for i, body in enumerate(config['Bodies']):
                if 'BodyMeshFileName' in body:
                    self.validate_mesh(body['BodyMeshFileName'])
                    
        # Wavelength checks
        if 'PeriodOrFrequency' in config:
            periods = config['PeriodOrFrequency']
            print(f"\n  Wave Periods: {periods}")
            
            # Check panel size vs wavelength
            # λ = gT²/(2π) for deep water
            g = 9.81
            for T in periods:
                wavelength = g * T**2 / (2 * np.pi)
                print(f"    T={T}s: wavelength={wavelength:.1f}m")
                
def main():
    validator = OrcaWaveValidationTab()
    
    # Test configurations
    test_configs = [
        "config_test_box.yml",
        "example_3_minimal_test.yml",
        "config_barge.yml"
    ]
    
    for config in test_configs:
        if Path(config).exists():
            validator.validate_config(config)
            
    print("\n" + "="*60)
    print(" VALIDATION COMPLETE ")
    print("="*60)
    print("\nThese checks simulate OrcaWave's Validation tab")
    print("Load the configs in OrcaWave to see actual validation")
    
    return 0

if __name__ == "__main__":
    main()