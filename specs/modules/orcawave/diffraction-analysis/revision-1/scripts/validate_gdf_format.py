#!/usr/bin/env python3
"""
Comprehensive GDF Format Validator for OrcaWave
Validates GDF files against known working examples
"""

import numpy as np
from pathlib import Path
import re
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class GDFValidator:
    """Validate GDF format for OrcaWave compatibility"""
    
    def __init__(self, gdf_file):
        self.gdf_file = Path(gdf_file)
        self.errors = []
        self.warnings = []
        self.info = {}
        
    def validate(self):
        """Run all validation checks"""
        logger.info(f"Validating GDF file: {self.gdf_file}")
        
        if not self.gdf_file.exists():
            self.errors.append(f"File not found: {self.gdf_file}")
            return False
        
        # Read file
        with open(self.gdf_file, 'r') as f:
            self.lines = f.readlines()
        
        # Run validation checks
        self._check_header()
        self._check_units_gravity()
        self._check_symmetry()
        self._check_vertex_count()
        self._check_vertex_format()
        self._check_geometry_bounds()
        self._check_panel_consistency()
        
        # Report results
        self._report_results()
        
        return len(self.errors) == 0
    
    def _check_header(self):
        """Check header format"""
        if len(self.lines) < 1:
            self.errors.append("File is empty")
            return
        
        header = self.lines[0].strip()
        
        # Known valid headers
        valid_headers = [
            "Rhino->WAMIT file export (mesh)",
            "WAMIT geometry file",
            "GDF file"
        ]
        
        if "WAMIT" in header.upper() or "GDF" in header.upper():
            self.info['header'] = header
            logger.info(f"[OK] Header format recognized: {header}")
        else:
            self.warnings.append(f"Unusual header: {header}")
    
    def _check_units_gravity(self):
        """Check units and gravity line"""
        if len(self.lines) < 2:
            self.errors.append("Missing units/gravity line")
            return
        
        line = self.lines[1].strip()
        
        # Parse units and gravity
        parts = line.split()
        if len(parts) < 2:
            self.errors.append(f"Invalid units/gravity line: {line}")
            return
        
        try:
            ulen = float(parts[0])
            grav = float(parts[1])
            
            self.info['length_scale'] = ulen
            self.info['gravity'] = grav
            
            if abs(grav - 9.80665) > 0.01:
                self.warnings.append(f"Non-standard gravity: {grav}")
            
            if ulen != 1.0:
                self.info['scaled_units'] = True
                logger.info(f"Length scale: {ulen}")
            
            logger.info(f"[OK] Units/gravity valid: ULEN={ulen}, GRAV={grav}")
            
        except ValueError:
            self.errors.append(f"Cannot parse units/gravity: {line}")
    
    def _check_symmetry(self):
        """Check symmetry flags"""
        if len(self.lines) < 3:
            self.errors.append("Missing symmetry line")
            return
        
        line = self.lines[2].strip()
        parts = line.split()
        
        if len(parts) < 2:
            self.errors.append(f"Invalid symmetry line: {line}")
            return
        
        try:
            isx = int(parts[0])
            isy = int(parts[1])
            
            self.info['x_symmetry'] = isx
            self.info['y_symmetry'] = isy
            
            if isx not in [0, 1]:
                self.errors.append(f"Invalid X symmetry flag: {isx}")
            if isy not in [0, 1]:
                self.errors.append(f"Invalid Y symmetry flag: {isy}")
            
            logger.info(f"[OK] Symmetry flags: ISX={isx}, ISY={isy}")
            
        except ValueError:
            self.errors.append(f"Cannot parse symmetry flags: {line}")
    
    def _check_vertex_count(self):
        """Check vertex count and consistency"""
        if len(self.lines) < 4:
            self.errors.append("Missing vertex count line")
            return
        
        line = self.lines[3].strip()
        
        try:
            declared_vertices = int(line)
            self.info['declared_vertices'] = declared_vertices
            
            # Count actual vertex lines
            vertex_lines = 0
            for i in range(4, len(self.lines)):
                if self.lines[i].strip() and not self.lines[i].strip().startswith('#'):
                    vertex_lines += 1
            
            self.info['actual_vertices'] = vertex_lines
            
            if declared_vertices != vertex_lines:
                self.errors.append(
                    f"Vertex count mismatch: declared {declared_vertices}, "
                    f"actual {vertex_lines}"
                )
            else:
                logger.info(f"[OK] Vertex count consistent: {declared_vertices}")
            
            # Check if vertices are multiple of 3 (triangles) or 4 (quads)
            if declared_vertices % 3 == 0:
                self.info['panel_type'] = 'triangles'
                self.info['num_panels'] = declared_vertices // 3
            elif declared_vertices % 4 == 0:
                self.info['panel_type'] = 'quadrilaterals'
                self.info['num_panels'] = declared_vertices // 4
            else:
                self.warnings.append(
                    f"Vertex count {declared_vertices} not divisible by 3 or 4"
                )
            
        except ValueError:
            self.errors.append(f"Cannot parse vertex count: {line}")
    
    def _check_vertex_format(self):
        """Check vertex coordinate format"""
        if len(self.lines) < 5:
            return
        
        # Sample first few vertex lines
        sample_size = min(10, len(self.lines) - 4)
        
        for i in range(4, 4 + sample_size):
            line = self.lines[i].strip()
            if not line:
                continue
            
            # Try to parse as 3 floats
            parts = line.split()
            if len(parts) < 3:
                self.errors.append(f"Line {i+1}: Invalid vertex format: {line}")
                continue
            
            try:
                x, y, z = float(parts[0]), float(parts[1]), float(parts[2])
            except ValueError:
                self.errors.append(f"Line {i+1}: Cannot parse coordinates: {line}")
        
        logger.info(f"[OK] Vertex format check passed")
    
    def _check_geometry_bounds(self):
        """Check geometry dimensions and bounds"""
        vertices = []
        
        for i in range(4, len(self.lines)):
            line = self.lines[i].strip()
            if not line:
                continue
            
            parts = line.split()
            if len(parts) >= 3:
                try:
                    x, y, z = float(parts[0]), float(parts[1]), float(parts[2])
                    vertices.append([x, y, z])
                except ValueError:
                    continue
        
        if not vertices:
            self.errors.append("No valid vertices found")
            return
        
        vertices = np.array(vertices)
        
        # Calculate bounds
        min_coords = vertices.min(axis=0)
        max_coords = vertices.max(axis=0)
        dimensions = max_coords - min_coords
        
        self.info['min_coords'] = min_coords
        self.info['max_coords'] = max_coords
        self.info['dimensions'] = dimensions
        
        logger.info(f"[OK] Geometry bounds:")
        logger.info(f"  X: {min_coords[0]:.3f} to {max_coords[0]:.3f} (L={dimensions[0]:.3f}m)")
        logger.info(f"  Y: {min_coords[1]:.3f} to {max_coords[1]:.3f} (B={dimensions[1]:.3f}m)")
        logger.info(f"  Z: {min_coords[2]:.3f} to {max_coords[2]:.3f} (D={dimensions[2]:.3f}m)")
        
        # Check for reasonable dimensions
        if dimensions[0] < 0.1 or dimensions[0] > 1000:
            self.warnings.append(f"Unusual length: {dimensions[0]:.3f}m")
        if dimensions[1] < 0.1 or dimensions[1] > 500:
            self.warnings.append(f"Unusual beam: {dimensions[1]:.3f}m")
        if dimensions[2] < 0.01 or dimensions[2] > 100:
            self.warnings.append(f"Unusual draft: {dimensions[2]:.3f}m")
        
        # Check waterline
        if max_coords[2] > 0.1:
            self.warnings.append(
                f"Geometry extends above z=0: max z={max_coords[2]:.3f}m"
            )
    
    def _check_panel_consistency(self):
        """Check panel arrangement consistency"""
        if 'num_panels' not in self.info:
            return
        
        num_panels = self.info['num_panels']
        panel_type = self.info.get('panel_type', 'unknown')
        
        logger.info(f"[OK] Panel arrangement: {num_panels} {panel_type}")
        
        # Check for reasonable panel count
        if num_panels < 10:
            self.warnings.append(f"Very few panels: {num_panels}")
        elif num_panels > 100000:
            self.warnings.append(f"Very many panels: {num_panels}")
    
    def _report_results(self):
        """Report validation results"""
        print("\n" + "="*60)
        print(f"GDF VALIDATION REPORT: {self.gdf_file.name}")
        print("="*60)
        
        if self.info:
            print("\n[INFO] File Information:")
            for key, value in self.info.items():
                if isinstance(value, np.ndarray):
                    print(f"  {key}: {value}")
                else:
                    print(f"  {key}: {value}")
        
        if self.errors:
            print("\n[ERRORS] (must fix):")
            for error in self.errors:
                print(f"  - {error}")
        
        if self.warnings:
            print("\n[WARNINGS] (should review):")
            for warning in self.warnings:
                print(f"  - {warning}")
        
        if not self.errors:
            print("\n[PASSED] VALIDATION PASSED")
            print("This file should be compatible with OrcaWave")
        else:
            print("\n[FAILED] VALIDATION FAILED")
            print("Fix the errors above before using in OrcaWave")
        
        print("="*60)

def compare_gdf_files(file1, file2):
    """Compare two GDF files to identify differences"""
    logger.info(f"\nComparing GDF files:")
    logger.info(f"  File 1: {file1}")
    logger.info(f"  File 2: {file2}")
    
    validator1 = GDFValidator(file1)
    validator2 = GDFValidator(file2)
    
    validator1.validate()
    validator2.validate()
    
    # Compare key metrics
    print("\n" + "="*60)
    print("GDF FILE COMPARISON")
    print("="*60)
    
    metrics = ['declared_vertices', 'num_panels', 'panel_type', 'dimensions']
    
    for metric in metrics:
        val1 = validator1.info.get(metric, 'N/A')
        val2 = validator2.info.get(metric, 'N/A')
        
        if isinstance(val1, np.ndarray):
            val1_str = f"[{val1[0]:.2f}, {val1[1]:.2f}, {val1[2]:.2f}]"
            val2_str = f"[{val2[0]:.2f}, {val2[1]:.2f}, {val2[2]:.2f}]"
            print(f"{metric}:")
            print(f"  File 1: {val1_str}")
            print(f"  File 2: {val2_str}")
        else:
            print(f"{metric}:")
            print(f"  File 1: {val1}")
            print(f"  File 2: {val2}")

def main():
    """Validate all GDF files"""
    
    base_dir = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/inputs/geometry")
    
    # List of GDF files to validate
    gdf_files = [
        base_dir / "simple_box_test.gdf",
        base_dir / "small_box_test.gdf",
        base_dir / "sea_cypress_orcawave.gdf",
        base_dir / "sea_cypress_trimesh.gdf"
    ]
    
    # Also check example file
    example_file = Path("D:/github/digitalmodel/docs/modules/orcawave/examples/L01_default_vessel/L01 Vessel mesh.gdf")
    
    print("\n" + "="*60)
    print("VALIDATING ALL GDF FILES")
    print("="*60)
    
    results = {}
    
    # Validate each file
    for gdf_file in gdf_files:
        if gdf_file.exists():
            validator = GDFValidator(gdf_file)
            results[gdf_file.name] = validator.validate()
        else:
            logger.warning(f"File not found: {gdf_file}")
    
    # Validate example
    if example_file.exists():
        print("\n[EXAMPLE] Validating OrcaWave Example File:")
        validator = GDFValidator(example_file)
        results["L01 Example"] = validator.validate()
    
    # Summary
    print("\n" + "="*60)
    print("VALIDATION SUMMARY")
    print("="*60)
    
    for filename, passed in results.items():
        status = "[PASSED]" if passed else "[FAILED]"
        print(f"{filename}: {status}")
    
    # Compare with example
    if example_file.exists() and (base_dir / "sea_cypress_orcawave.gdf").exists():
        print("\n[COMPARE] Comparing our file with OrcaWave example:")
        compare_gdf_files(
            base_dir / "sea_cypress_orcawave.gdf",
            example_file
        )

if __name__ == "__main__":
    main()