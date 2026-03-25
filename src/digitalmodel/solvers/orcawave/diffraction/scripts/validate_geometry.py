#!/usr/bin/env python3
"""
Geometry Validation Script for Sea Cypress Vessel
Validates STL and OBJ mesh files for OrcaWave analysis
"""

import os
import sys
import struct
import logging
from pathlib import Path
from typing import Dict, List, Tuple, Optional
import numpy as np
from dataclasses import dataclass
from concurrent.futures import ProcessPoolExecutor, as_completed
import time

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

@dataclass
class MeshStatistics:
    """Container for mesh statistics"""
    filename: str
    format: str
    num_vertices: int
    num_faces: int
    num_edges: int
    bbox_min: np.ndarray
    bbox_max: np.ndarray
    volume: float
    surface_area: float
    is_watertight: bool
    has_duplicate_vertices: bool
    has_degenerate_faces: bool
    max_aspect_ratio: float
    min_panel_area: float
    max_panel_area: float
    centroid: np.ndarray
    waterline_area: Optional[float] = None
    
class GeometryValidator:
    """Validates geometry files for OrcaWave analysis"""
    
    def __init__(self, geometry_path: str):
        self.geometry_path = Path(geometry_path)
        self.results = {}
        
    def validate_all(self, parallel: bool = True) -> Dict[str, MeshStatistics]:
        """Validate all geometry files in parallel"""

        geometry_files = {
            "Sea Cypress_0.25 Mesh_Ascii.stl": "stl_ascii",
            "Sea Cypress_0.25 Mesh_Binary.stl": "stl_binary",
            "Sea Cypress_0.25 Mesh_Binary.obj": "obj"
        }

        logger.info(f"Validating {len(geometry_files)} geometry files...")
        logger.info(f"Geometry path: {self.geometry_path}")

        if parallel:
            try:
                with ProcessPoolExecutor(max_workers=3) as executor:
                    futures = {}
                    for filename, format_type in geometry_files.items():
                        filepath = self.geometry_path / filename
                        if filepath.exists():
                            future = executor.submit(self.validate_file, filepath, format_type)
                            futures[future] = filename
                        else:
                            logger.warning(f"File not found: {filepath}")

                    for future in as_completed(futures):
                        filename = futures[future]
                        try:
                            stats = future.result()
                            self.results[filename] = stats
                            logger.info(f"Validated: {filename}")
                        except Exception as e:
                            logger.error(f"Failed to validate {filename}: {e}")
            except (OSError, PermissionError) as e:
                logger.warning(f"Parallel validation failed: {e}. Retrying sequentially.")
                return self.validate_all(parallel=False)
        else:
            for filename, format_type in geometry_files.items():
                filepath = self.geometry_path / filename
                if filepath.exists():
                    try:
                        stats = self.validate_file(filepath, format_type)
                        self.results[filename] = stats
                        logger.info(f"Validated: {filename}")
                    except Exception as e:
                        logger.error(f"Failed to validate {filename}: {e}")
                else:
                    logger.warning(f"File not found: {filepath}")

        return self.results

    def validate_file(self, filepath: Path, format_type: str) -> MeshStatistics:
        """Validate a single geometry file"""
        
        logger.info(f"Processing: {filepath.name}")
        
        if format_type == "stl_ascii":
            return self._validate_stl_ascii(filepath)
        elif format_type == "stl_binary":
            return self._validate_stl_binary(filepath)
        elif format_type == "obj":
            return self._validate_obj(filepath)
        else:
            raise ValueError(f"Unknown format type: {format_type}")
    
    def _validate_stl_binary(self, filepath: Path) -> MeshStatistics:
        """Validate binary STL file"""
        
        vertices = []
        faces = []
        normals = []
        
        with open(filepath, 'rb') as f:
            # Read header (80 bytes)
            header = f.read(80)
            
            # Read number of triangles
            num_triangles = struct.unpack('<I', f.read(4))[0]
            
            for i in range(num_triangles):
                # Read normal vector (3 floats)
                normal = struct.unpack('<fff', f.read(12))
                normals.append(normal)
                
                # Read vertices (3 vertices × 3 coordinates)
                triangle_vertices = []
                for j in range(3):
                    vertex = struct.unpack('<fff', f.read(12))
                    triangle_vertices.append(vertex)
                vertices.extend(triangle_vertices)
                
                # Read attribute byte count (2 bytes)
                f.read(2)
                
                # Create face (indices)
                base_idx = i * 3
                faces.append([base_idx, base_idx + 1, base_idx + 2])
        
        vertices = np.array(vertices)
        faces = np.array(faces, dtype=object)
        
        return self._compute_mesh_statistics(
            filepath.name, "stl_binary", vertices, faces
        )
    
    def _validate_stl_ascii(self, filepath: Path) -> MeshStatistics:
        """Validate ASCII STL file"""
        
        vertices = []
        faces = []
        current_triangle = []
        
        with open(filepath, 'r') as f:
            for line in f:
                line = line.strip()
                if line.startswith('vertex'):
                    coords = line.split()[1:]
                    vertex = [float(c) for c in coords]
                    current_triangle.append(vertex)
                    
                    if len(current_triangle) == 3:
                        base_idx = len(vertices)
                        vertices.extend(current_triangle)
                        faces.append([base_idx, base_idx + 1, base_idx + 2])
                        current_triangle = []
        
        vertices = np.array(vertices)
        faces = np.array(faces, dtype=object)
        
        return self._compute_mesh_statistics(
            filepath.name, "stl_ascii", vertices, faces
        )
    
    def _validate_obj(self, filepath: Path) -> MeshStatistics:
        """Validate OBJ file"""
        
        vertices = []
        faces = []
        
        with open(filepath, 'r') as f:
            for line in f:
                line = line.strip()
                if line.startswith('v '):
                    coords = line.split()[1:]
                    vertex = [float(c) for c in coords]
                    vertices.append(vertex)
                elif line.startswith('f '):
                    # Handle face indices (OBJ uses 1-based indexing)
                    indices = []
                    for token in line.split()[1:]:
                        # Handle v/vt/vn format
                        idx = int(token.split('/')[0]) - 1
                        indices.append(idx)
                    faces.append(indices)
        
        vertices = np.array(vertices)
        faces = np.array(faces, dtype=object)
        
        return self._compute_mesh_statistics(
            filepath.name, "obj", vertices, faces
        )
    
    def _compute_mesh_statistics(
        self, filename: str, format_type: str, 
        vertices: np.ndarray, faces: np.ndarray
    ) -> MeshStatistics:
        """Compute comprehensive mesh statistics"""
        
        # Basic counts
        num_vertices = len(vertices)
        num_faces = len(faces)
        num_edges = num_faces * 3  # Approximate for triangular mesh
        
        # Bounding box
        bbox_min = vertices.min(axis=0)
        bbox_max = vertices.max(axis=0)
        
        # Centroid
        centroid = vertices.mean(axis=0)
        
        # Volume and surface area
        volume = 0.0
        surface_area = 0.0
        min_area = float('inf')
        max_area = 0.0
        max_aspect = 0.0
        
        for face in faces:
            if len(face) >= 3:
                v0, v1, v2 = vertices[face[0]], vertices[face[1]], vertices[face[2]]
                
                # Triangle area (Heron's formula)
                edge1 = v1 - v0
                edge2 = v2 - v0
                cross = np.cross(edge1, edge2)
                area = 0.5 * np.linalg.norm(cross)
                surface_area += area
                
                min_area = min(min_area, area)
                max_area = max(max_area, area)
                
                # Volume contribution (signed volume)
                volume += np.dot(v0, cross) / 6.0
                
                # Aspect ratio
                edges = [
                    np.linalg.norm(v1 - v0),
                    np.linalg.norm(v2 - v1),
                    np.linalg.norm(v0 - v2)
                ]
                if min(edges) > 0:
                    aspect = max(edges) / min(edges)
                    max_aspect = max(max_aspect, aspect)
        
        volume = abs(volume)
        
        # Check for duplicate vertices
        unique_vertices = np.unique(vertices, axis=0)
        has_duplicates = len(unique_vertices) < len(vertices)
        
        # Check for degenerate faces (zero area)
        has_degenerate = min_area < 1e-10
        
        # Estimate if watertight (closed surface)
        # Simple check: volume should be non-zero for closed mesh
        is_watertight = volume > 1e-6
        
        # Waterline area (assuming z=0 is waterline)
        waterline_vertices = vertices[np.abs(vertices[:, 2]) < 0.1]
        if len(waterline_vertices) > 0:
            # Approximate waterline area using convex hull projection
            from scipy.spatial import ConvexHull
            try:
                hull_2d = ConvexHull(waterline_vertices[:, :2])
                waterline_area = hull_2d.volume  # In 2D, volume is area
            except:
                waterline_area = None
        else:
            waterline_area = None
        
        return MeshStatistics(
            filename=filename,
            format=format_type,
            num_vertices=num_vertices,
            num_faces=num_faces,
            num_edges=num_edges,
            bbox_min=bbox_min,
            bbox_max=bbox_max,
            volume=volume,
            surface_area=surface_area,
            is_watertight=is_watertight,
            has_duplicate_vertices=has_duplicates,
            has_degenerate_faces=has_degenerate,
            max_aspect_ratio=max_aspect,
            min_panel_area=min_area,
            max_panel_area=max_area,
            centroid=centroid,
            waterline_area=waterline_area
        )
    
    def generate_report(self) -> str:
        """Generate validation report"""
        
        report = []
        report.append("=" * 80)
        report.append("GEOMETRY VALIDATION REPORT - SEA CYPRESS VESSEL")
        report.append("=" * 80)
        report.append("")
        
        # Recommendation
        best_format = self._recommend_format()
        report.append(f"RECOMMENDED FORMAT: {best_format}")
        report.append("")
        
        # Detailed statistics for each file
        for filename, stats in self.results.items():
            report.append(f"\nFile: {filename}")
            report.append("-" * 40)
            report.append(f"Format: {stats.format}")
            report.append(f"Vertices: {stats.num_vertices:,}")
            report.append(f"Faces: {stats.num_faces:,}")
            report.append(f"Volume: {stats.volume:.2f} m³")
            report.append(f"Surface Area: {stats.surface_area:.2f} m²")
            report.append(f"Watertight: {'yes' if stats.is_watertight else 'no'}")
            report.append(
                f"Duplicates: {'has duplicates' if stats.has_duplicate_vertices else 'no duplicates'}"
            )
            report.append(
                f"Degenerate Faces: {'has degenerate' if stats.has_degenerate_faces else 'no degenerate'}"
            )
            report.append(f"Max Aspect Ratio: {stats.max_aspect_ratio:.2f}")
            report.append(f"Panel Area Range: {stats.min_panel_area:.4f} - {stats.max_panel_area:.2f} m²")
            report.append(f"Bounding Box: [{stats.bbox_min[0]:.1f}, {stats.bbox_max[0]:.1f}] × "
                         f"[{stats.bbox_min[1]:.1f}, {stats.bbox_max[1]:.1f}] × "
                         f"[{stats.bbox_min[2]:.1f}, {stats.bbox_max[2]:.1f}] m")
            report.append(f"Centroid: ({stats.centroid[0]:.2f}, {stats.centroid[1]:.2f}, {stats.centroid[2]:.2f}) m")
            if stats.waterline_area:
                report.append(f"Waterline Area: {stats.waterline_area:.2f} m²")
            
            # Quality assessment
            quality_score = self._assess_quality(stats)
            report.append(f"Quality Score: {quality_score}/10")
            report.append("")
        
        report.append("=" * 80)
        report.append("VALIDATION COMPLETE")
        report.append("=" * 80)
        
        return "\n".join(report)
    
    def _recommend_format(self) -> str:
        """Recommend best format based on validation results"""
        
        scores = {}
        for filename, stats in self.results.items():
            score = self._assess_quality(stats)
            
            # Bonus for binary STL (efficiency)
            if "Binary.stl" in filename:
                score += 1
            
            scores[filename] = score
        
        if scores:
            best = max(scores, key=scores.get)
            return best
        return "No valid geometry found"
    
    def _assess_quality(self, stats: MeshStatistics) -> float:
        """Assess mesh quality (0-10 scale)"""
        
        score = 10.0
        
        # Penalties
        if not stats.is_watertight:
            score -= 3.0
        if stats.has_duplicate_vertices:
            score -= 1.0
        if stats.has_degenerate_faces:
            score -= 2.0
        if stats.max_aspect_ratio > 5.0:
            score -= 1.0
        if stats.max_aspect_ratio > 10.0:
            score -= 1.0
        if stats.min_panel_area < 0.001:
            score -= 1.0
        
        return max(0, score)
    
    def save_report(self, output_path: str):
        """Save validation report to file"""
        
        report = self.generate_report()
        output_file = Path(output_path)
        output_file.parent.mkdir(parents=True, exist_ok=True)
        
        with open(output_file, 'w') as f:
            f.write(report)
        
        logger.info(f"Report saved to: {output_file}")

def main():
    """Main execution function"""
    
    import argparse
    
    parser = argparse.ArgumentParser(
        description="Validate geometry files for OrcaWave analysis"
    )
    parser.add_argument(
        '--path', '-p',
        help='Path to geometry files directory',
        default=None
    )
    parser.add_argument(
        '--vessel', '-v',
        help='Vessel name for output organization',
        default='generic_vessel'
    )
    parser.add_argument(
        '--no-parallel',
        action='store_true',
        help='Disable parallel validation'
    )
    
    args = parser.parse_args()
    
    # Determine geometry path
    if args.path:
        GEOMETRY_PATH = Path(args.path)
    else:
        # Default to specs location relative to repository root
        repo_root = None
        for parent in [Path(__file__).resolve()] + list(Path(__file__).resolve().parents):
            if (parent / "pyproject.toml").exists() or (parent / ".git").exists():
                repo_root = parent
                break
        if repo_root is None:
            repo_root = Path(__file__).resolve().parents[6]
        GEOMETRY_PATH = repo_root / "specs" / "modules" / "orcawave" / "diffraction-analysis" / "inputs" / "geometry"
    
    # Make path absolute if relative
    if not GEOMETRY_PATH.is_absolute():
        GEOMETRY_PATH = Path.cwd() / GEOMETRY_PATH
    
    OUTPUT_DIR = Path(__file__).parent.parent / "results" / args.vessel / "validation"
    
    # Create output directory
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    
    # Check if geometry path exists
    if not GEOMETRY_PATH.exists():
        logger.error(f"Geometry path does not exist: {GEOMETRY_PATH}")
        logger.info("Please provide a valid path using --path argument")
        return 1
    
    # Run validation
    logger.info(f"Starting geometry validation for {args.vessel}...")
    logger.info(f"Geometry path: {GEOMETRY_PATH}")
    start_time = time.time()
    
    validator = GeometryValidator(str(GEOMETRY_PATH))
    
    try:
        # Validate all geometries in parallel
        results = validator.validate_all(parallel=not args.no_parallel)
        
        # Generate and display report
        report = validator.generate_report()
        print("\n" + report)
        
        # Save report
        timestamp = time.strftime("%Y%m%d_%H%M%S")
        report_file = OUTPUT_DIR / f"validation_report_{timestamp}.txt"
        validator.save_report(report_file)
        
        # Save detailed results as JSON
        import json
        json_file = OUTPUT_DIR / f"validation_results_{timestamp}.json"
        
        # Convert numpy arrays to lists for JSON serialization
        json_results = {}
        for filename, stats in results.items():
            json_results[filename] = {
                'format': stats.format,
                'num_vertices': stats.num_vertices,
                'num_faces': stats.num_faces,
                'volume': float(stats.volume),
                'surface_area': float(stats.surface_area),
                'is_watertight': bool(stats.is_watertight),
                'max_aspect_ratio': float(stats.max_aspect_ratio),
                'quality_score': validator._assess_quality(stats)
            }
        
        with open(json_file, 'w') as f:
            json.dump(json_results, f, indent=2)
        
        logger.info(f"JSON results saved to: {json_file}")
        
    except Exception as e:
        logger.error(f"Validation failed: {e}")
        return 1
    
    elapsed = time.time() - start_time
    logger.info(f"Validation completed in {elapsed:.2f} seconds")
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
