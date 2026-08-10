#!/usr/bin/env python
"""
Test various open-source methods for converting STL to GDF format for OrcaWave.
Evaluates mesh quality metrics for each method.
"""

import os
import sys
import time
import numpy as np
from pathlib import Path
from typing import Dict, Any, Tuple, List
import logging

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# Input geometry path
GEOMETRY_PATH = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/inputs/geometry")
OUTPUT_PATH = Path("D:/github/digitalmodel/src/modules/orcawave/gdf_outputs")
OUTPUT_PATH.mkdir(exist_ok=True)

class MeshQualityAnalyzer:
    """Analyze mesh quality metrics."""
    
    @staticmethod
    def calculate_aspect_ratios(vertices: np.ndarray, faces: np.ndarray) -> Dict[str, float]:
        """Calculate panel aspect ratios."""
        aspect_ratios = []
        
        for face in faces:
            if len(face) >= 3:
                # Get vertices of the face
                v = vertices[face[:3]]
                
                # Calculate edge lengths
                edges = [
                    np.linalg.norm(v[1] - v[0]),
                    np.linalg.norm(v[2] - v[1]),
                    np.linalg.norm(v[0] - v[2])
                ]
                
                # Aspect ratio = longest edge / shortest edge
                if min(edges) > 0:
                    aspect_ratios.append(max(edges) / min(edges))
        
        return {
            'mean_aspect_ratio': np.mean(aspect_ratios) if aspect_ratios else 0,
            'max_aspect_ratio': np.max(aspect_ratios) if aspect_ratios else 0,
            'min_aspect_ratio': np.min(aspect_ratios) if aspect_ratios else 0,
            'std_aspect_ratio': np.std(aspect_ratios) if aspect_ratios else 0
        }
    
    @staticmethod
    def check_normals(vertices: np.ndarray, faces: np.ndarray) -> Dict[str, Any]:
        """Check if normals point outward."""
        normals = []
        centroids = []
        
        for face in faces:
            if len(face) >= 3:
                v = vertices[face[:3]]
                
                # Calculate normal using cross product
                edge1 = v[1] - v[0]
                edge2 = v[2] - v[0]
                normal = np.cross(edge1, edge2)
                norm = np.linalg.norm(normal)
                
                if norm > 0:
                    normal = normal / norm
                    normals.append(normal)
                    
                    # Calculate centroid
                    centroid = np.mean(v, axis=0)
                    centroids.append(centroid)
        
        # Check if normals point outward (assuming origin is inside)
        mesh_center = np.mean(vertices, axis=0)
        outward_count = 0
        
        for normal, centroid in zip(normals, centroids):
            to_centroid = centroid - mesh_center
            if np.dot(normal, to_centroid) > 0:
                outward_count += 1
        
        return {
            'total_panels': len(normals),
            'outward_pointing': outward_count,
            'percentage_correct': (outward_count / len(normals) * 100) if normals else 0
        }
    
    @staticmethod
    def calculate_volume(vertices: np.ndarray, faces: np.ndarray) -> float:
        """Calculate mesh volume using divergence theorem."""
        volume = 0.0
        
        for face in faces:
            if len(face) >= 3:
                v = vertices[face[:3]]
                # Volume of tetrahedron with origin
                volume += np.abs(np.dot(v[0], np.cross(v[1], v[2]))) / 6.0
        
        return volume
    
    @staticmethod
    def check_watertight(vertices: np.ndarray, faces: np.ndarray) -> bool:
        """Check if mesh is watertight."""
        # A simple check: each edge should be shared by exactly 2 faces
        edge_count = {}
        
        for face in faces:
            if len(face) >= 3:
                for i in range(len(face)):
                    edge = tuple(sorted([face[i], face[(i+1)%len(face)]]))
                    edge_count[edge] = edge_count.get(edge, 0) + 1
        
        # Check if all edges are shared by exactly 2 faces
        non_manifold = [count for count in edge_count.values() if count != 2]
        
        return len(non_manifold) == 0


def test_meshmagick_conversion():
    """Test Meshmagick library for STL to GDF conversion."""
    logger.info("Testing Meshmagick conversion...")
    
    try:
        import meshmagick as mm
        import meshmagick.mesh as mmesh
        
        results = {}
        stl_file = GEOMETRY_PATH / "Sea Cypress_0.25 Mesh_Binary.stl"
        
        # Load STL
        start_time = time.time()
        mesh = mmesh.Mesh(filename=str(stl_file))
        load_time = time.time() - start_time
        
        # Analyze mesh quality
        analyzer = MeshQualityAnalyzer()
        
        # Get vertices and faces
        vertices = mesh.vertices
        faces = mesh.faces
        
        quality_metrics = {
            'num_vertices': len(vertices),
            'num_panels': len(faces),
            'aspect_ratios': analyzer.calculate_aspect_ratios(vertices, faces),
            'normals_check': analyzer.check_normals(vertices, faces),
            'volume': analyzer.calculate_volume(vertices, faces),
            'is_watertight': analyzer.check_watertight(vertices, faces),
            'load_time': load_time
        }
        
        # Convert to GDF
        start_time = time.time()
        gdf_file = OUTPUT_PATH / "sea_cypress_meshmagick.gdf"
        mesh.write_GDF(str(gdf_file))
        conversion_time = time.time() - start_time
        
        quality_metrics['conversion_time'] = conversion_time
        quality_metrics['output_file'] = str(gdf_file)
        quality_metrics['file_size'] = os.path.getsize(gdf_file) if gdf_file.exists() else 0
        
        results['meshmagick'] = quality_metrics
        logger.info(f"Meshmagick conversion successful: {gdf_file}")
        
        return results
        
    except ImportError:
        logger.warning("Meshmagick not installed. Skipping...")
        return {'meshmagick': {'error': 'Package not installed'}}
    except Exception as e:
        logger.error(f"Meshmagick conversion failed: {e}")
        return {'meshmagick': {'error': str(e)}}


def test_capytaine_conversion():
    """Test Capytaine library for STL to GDF conversion."""
    logger.info("Testing Capytaine conversion...")
    
    try:
        import capytaine as cpt
        from capytaine.io.mesh_loaders import load_mesh
        from capytaine.io.mesh_writers import write_GDF
        
        results = {}
        stl_file = GEOMETRY_PATH / "Sea Cypress_0.25 Mesh_Binary.stl"
        
        # Load STL
        start_time = time.time()
        mesh = cpt.load_mesh(str(stl_file), file_format='stl')
        load_time = time.time() - start_time
        
        # Analyze mesh quality
        analyzer = MeshQualityAnalyzer()
        
        # Get vertices and faces
        vertices = mesh.vertices
        faces = mesh.faces
        
        quality_metrics = {
            'num_vertices': mesh.nb_vertices,
            'num_panels': mesh.nb_faces,
            'aspect_ratios': analyzer.calculate_aspect_ratios(vertices, faces),
            'normals_check': analyzer.check_normals(vertices, faces),
            'volume': mesh.volume if hasattr(mesh, 'volume') else analyzer.calculate_volume(vertices, faces),
            'is_watertight': mesh.is_mesh_closed if hasattr(mesh, 'is_mesh_closed') else analyzer.check_watertight(vertices, faces),
            'load_time': load_time
        }
        
        # Convert to GDF
        start_time = time.time()
        gdf_file = OUTPUT_PATH / "sea_cypress_capytaine.gdf"
        mesh.export(str(gdf_file), file_format='GDF')
        conversion_time = time.time() - start_time
        
        quality_metrics['conversion_time'] = conversion_time
        quality_metrics['output_file'] = str(gdf_file)
        quality_metrics['file_size'] = os.path.getsize(gdf_file) if gdf_file.exists() else 0
        
        results['capytaine'] = quality_metrics
        logger.info(f"Capytaine conversion successful: {gdf_file}")
        
        return results
        
    except ImportError:
        logger.warning("Capytaine not installed. Skipping...")
        return {'capytaine': {'error': 'Package not installed'}}
    except Exception as e:
        logger.error(f"Capytaine conversion failed: {e}")
        return {'capytaine': {'error': str(e)}}


def test_gmsh_conversion():
    """Test GMsh for mesh generation and conversion to GDF."""
    logger.info("Testing GMsh conversion...")
    
    try:
        import gmsh
        import numpy as np
        
        results = {}
        stl_file = GEOMETRY_PATH / "Sea Cypress_0.25 Mesh_Binary.stl"
        
        # Initialize GMsh
        gmsh.initialize()
        gmsh.clear()
        
        start_time = time.time()
        
        # Import STL
        gmsh.merge(str(stl_file))
        
        # Create surface mesh
        gmsh.model.mesh.createTopology()
        
        # Classify surfaces
        gmsh.model.mesh.classifySurfaces(angle=40)
        
        # Create geometry from mesh
        gmsh.model.mesh.createGeometry()
        
        # Optional: remesh with controlled size
        gmsh.option.setNumber("Mesh.MeshSizeMin", 0.2)
        gmsh.option.setNumber("Mesh.MeshSizeMax", 0.5)
        gmsh.option.setNumber("Mesh.Algorithm", 6)  # Frontal-Delaunay
        gmsh.option.setNumber("Mesh.OptimizeNetgen", 1)
        
        # Generate 2D mesh
        gmsh.model.mesh.generate(2)
        
        # Optimize mesh
        gmsh.model.mesh.optimize("Netgen")
        
        load_time = time.time() - start_time
        
        # Extract mesh data
        node_tags, node_coords, _ = gmsh.model.mesh.getNodes()
        vertices = node_coords.reshape(-1, 3)
        
        # Get all 2D elements (triangles)
        element_types, element_tags, node_tags_per_element = gmsh.model.mesh.getElements(2)
        
        faces = []
        for elem_type, elem_tags, elem_nodes in zip(element_types, element_tags, node_tags_per_element):
            if elem_type == 2:  # Triangles
                elem_nodes = elem_nodes.reshape(-1, 3)
                for face_nodes in elem_nodes:
                    # Convert to 0-based indexing
                    face = [np.where(node_tags == node)[0][0] for node in face_nodes]
                    faces.append(face)
        
        faces = np.array(faces)
        
        # Analyze mesh quality
        analyzer = MeshQualityAnalyzer()
        
        quality_metrics = {
            'num_vertices': len(vertices),
            'num_panels': len(faces),
            'aspect_ratios': analyzer.calculate_aspect_ratios(vertices, faces),
            'normals_check': analyzer.check_normals(vertices, faces),
            'volume': analyzer.calculate_volume(vertices, faces),
            'is_watertight': analyzer.check_watertight(vertices, faces),
            'load_time': load_time
        }
        
        # Get mesh quality metrics from GMsh
        gmsh.model.mesh.getElementQualities(2)
        
        # Convert to GDF format
        start_time = time.time()
        gdf_file = OUTPUT_PATH / "sea_cypress_gmsh.gdf"
        
        # Write GDF file
        with open(gdf_file, 'w') as f:
            # Header: ulen (length scale), grav
            f.write("1.0 9.80665\n")
            
            # ISX, ISY (symmetry flags)
            f.write("0 0\n")
            
            # Number of vertices
            f.write(f"{len(vertices)}\n")
            
            # Write vertices
            for vertex in vertices:
                f.write(f"{vertex[0]:12.6f} {vertex[1]:12.6f} {vertex[2]:12.6f}\n")
            
            # Number of panels
            f.write(f"{len(faces)}\n")
            
            # Write panels (triangles as quads with repeated vertex)
            for face in faces:
                # GDF uses 1-based indexing
                f.write(f"{face[0]+1:6d} {face[1]+1:6d} {face[2]+1:6d} {face[2]+1:6d}\n")
        
        conversion_time = time.time() - start_time
        
        # Also save as MSH for verification
        msh_file = OUTPUT_PATH / "sea_cypress_gmsh.msh"
        gmsh.write(str(msh_file))
        
        quality_metrics['conversion_time'] = conversion_time
        quality_metrics['output_file'] = str(gdf_file)
        quality_metrics['file_size'] = os.path.getsize(gdf_file)
        quality_metrics['msh_file'] = str(msh_file)
        
        # Cleanup
        gmsh.finalize()
        
        results['gmsh'] = quality_metrics
        logger.info(f"GMsh conversion successful: {gdf_file}")
        
        return results
        
    except ImportError:
        logger.warning("GMsh not installed. Skipping...")
        return {'gmsh': {'error': 'Package not installed'}}
    except Exception as e:
        logger.error(f"GMsh conversion failed: {e}")
        if 'gmsh' in locals():
            gmsh.finalize()
        return {'gmsh': {'error': str(e)}}


def test_custom_trimesh_conversion():
    """Test custom conversion using trimesh library."""
    logger.info("Testing custom trimesh conversion...")
    
    try:
        import trimesh
        
        results = {}
        stl_file = GEOMETRY_PATH / "Sea Cypress_0.25 Mesh_Binary.stl"
        
        # Load STL
        start_time = time.time()
        mesh = trimesh.load(str(stl_file))
        load_time = time.time() - start_time
        
        # Ensure consistent winding
        mesh.fix_normals()
        
        # Analyze mesh quality
        analyzer = MeshQualityAnalyzer()
        
        quality_metrics = {
            'num_vertices': len(mesh.vertices),
            'num_panels': len(mesh.faces),
            'aspect_ratios': analyzer.calculate_aspect_ratios(mesh.vertices, mesh.faces),
            'normals_check': analyzer.check_normals(mesh.vertices, mesh.faces),
            'volume': float(mesh.volume),
            'is_watertight': mesh.is_watertight,
            'is_winding_consistent': mesh.is_winding_consistent,
            'euler_number': mesh.euler_number,
            'load_time': load_time
        }
        
        # Convert to GDF format
        start_time = time.time()
        gdf_file = OUTPUT_PATH / "sea_cypress_trimesh.gdf"
        
        # Write GDF file (WAMIT format)
        with open(gdf_file, 'w') as f:
            # Header: ulen (length scale), grav
            f.write("1.0 9.80665\n")
            
            # ISX, ISY (symmetry flags)
            f.write("0 0\n")
            
            # Number of vertices
            f.write(f"{len(mesh.vertices)}\n")
            
            # Write vertices
            for vertex in mesh.vertices:
                f.write(f"{vertex[0]:12.6f} {vertex[1]:12.6f} {vertex[2]:12.6f}\n")
            
            # Number of panels
            f.write(f"{len(mesh.faces)}\n")
            
            # Write panels (quadrilaterals or triangles)
            for face in mesh.faces:
                # GDF uses 1-based indexing
                if len(face) == 3:
                    # Triangle - duplicate last vertex for quad format
                    f.write(f"{face[0]+1:6d} {face[1]+1:6d} {face[2]+1:6d} {face[2]+1:6d}\n")
                elif len(face) == 4:
                    f.write(f"{face[0]+1:6d} {face[1]+1:6d} {face[2]+1:6d} {face[3]+1:6d}\n")
        
        conversion_time = time.time() - start_time
        
        quality_metrics['conversion_time'] = conversion_time
        quality_metrics['output_file'] = str(gdf_file)
        quality_metrics['file_size'] = os.path.getsize(gdf_file)
        
        results['trimesh_custom'] = quality_metrics
        logger.info(f"Custom trimesh conversion successful: {gdf_file}")
        
        return results
        
    except ImportError:
        logger.warning("Trimesh not installed. Trying to install...")
        os.system("pip install trimesh")
        try:
            import trimesh
            return test_custom_trimesh_conversion()
        except:
            return {'trimesh_custom': {'error': 'Package not installed'}}
    except Exception as e:
        logger.error(f"Custom trimesh conversion failed: {e}")
        return {'trimesh_custom': {'error': str(e)}}


def compare_results(all_results: Dict[str, Dict]) -> None:
    """Compare and display results from all conversion methods."""
    logger.info("\n" + "="*80)
    logger.info("MESH CONVERSION COMPARISON RESULTS")
    logger.info("="*80)
    
    for method, metrics in all_results.items():
        logger.info(f"\n{method.upper()} Method:")
        logger.info("-" * 40)
        
        if 'error' in metrics:
            logger.error(f"  Error: {metrics['error']}")
            continue
        
        # Basic metrics
        logger.info(f"  Vertices: {metrics.get('num_vertices', 'N/A')}")
        logger.info(f"  Panels: {metrics.get('num_panels', 'N/A')}")
        logger.info(f"  Volume: {metrics.get('volume', 'N/A'):.3f} m³" if metrics.get('volume') else "  Volume: N/A")
        logger.info(f"  Watertight: {metrics.get('is_watertight', 'N/A')}")
        
        # Aspect ratios
        if 'aspect_ratios' in metrics:
            ar = metrics['aspect_ratios']
            logger.info(f"  Aspect Ratios:")
            logger.info(f"    Mean: {ar.get('mean_aspect_ratio', 0):.2f}")
            logger.info(f"    Max: {ar.get('max_aspect_ratio', 0):.2f}")
            logger.info(f"    Min: {ar.get('min_aspect_ratio', 0):.2f}")
        
        # Normals
        if 'normals_check' in metrics:
            nc = metrics['normals_check']
            logger.info(f"  Normal Orientation:")
            logger.info(f"    Outward pointing: {nc.get('percentage_correct', 0):.1f}%")
        
        # Performance
        logger.info(f"  Load Time: {metrics.get('load_time', 0):.3f}s")
        logger.info(f"  Conversion Time: {metrics.get('conversion_time', 0):.3f}s")
        logger.info(f"  Output File Size: {metrics.get('file_size', 0) / 1024:.1f} KB")
    
    # Summary recommendation
    logger.info("\n" + "="*80)
    logger.info("RECOMMENDATION:")
    
    valid_methods = [m for m, metrics in all_results.items() if 'error' not in metrics]
    
    if valid_methods:
        # Find best method based on criteria
        best_quality = None
        best_speed = None
        best_overall = None
        
        for method in valid_methods:
            metrics = all_results[method]
            
            # Check quality (watertight, normals)
            if metrics.get('is_watertight') and metrics.get('normals_check', {}).get('percentage_correct', 0) > 90:
                if not best_quality or metrics.get('aspect_ratios', {}).get('mean_aspect_ratio', 999) < \
                   all_results[best_quality].get('aspect_ratios', {}).get('mean_aspect_ratio', 999):
                    best_quality = method
            
            # Check speed
            total_time = metrics.get('load_time', 999) + metrics.get('conversion_time', 999)
            if not best_speed or total_time < (all_results[best_speed].get('load_time', 999) + 
                                               all_results[best_speed].get('conversion_time', 999)):
                best_speed = method
        
        # Overall recommendation
        if best_quality:
            best_overall = best_quality
            logger.info(f"  Best Overall: {best_overall} (best mesh quality)")
        elif best_speed:
            best_overall = best_speed
            logger.info(f"  Best Overall: {best_speed} (fastest conversion)")
        
        logger.info(f"  Best Quality: {best_quality or 'None met quality criteria'}")
        logger.info(f"  Fastest: {best_speed}")
    else:
        logger.error("  No methods succeeded. Check installations and input file.")
    
    logger.info("="*80 + "\n")


def main():
    """Main execution function."""
    logger.info("Starting STL to GDF conversion tests...")
    logger.info(f"Input geometry: {GEOMETRY_PATH}")
    logger.info(f"Output directory: {OUTPUT_PATH}\n")
    
    all_results = {}
    
    # Test each method
    methods = [
        ("Meshmagick", test_meshmagick_conversion),
        ("Capytaine", test_capytaine_conversion),
        ("GMsh", test_gmsh_conversion),
        ("Custom Trimesh", test_custom_trimesh_conversion)
    ]
    
    for name, test_func in methods:
        logger.info(f"\n{'='*40}")
        logger.info(f"Testing {name}...")
        logger.info('='*40)
        
        result = test_func()
        all_results.update(result)
    
    # Compare results
    compare_results(all_results)
    
    # Save detailed results to JSON
    import json
    results_file = OUTPUT_PATH / "conversion_comparison_results.json"
    
    # Convert numpy types to native Python types for JSON serialization
    def convert_to_json_serializable(obj):
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        elif isinstance(obj, (np.integer, np.floating)):
            return float(obj)
        elif isinstance(obj, dict):
            return {k: convert_to_json_serializable(v) for k, v in obj.items()}
        elif isinstance(obj, (list, tuple)):
            return [convert_to_json_serializable(item) for item in obj]
        elif isinstance(obj, Path):
            return str(obj)
        else:
            return obj
    
    json_results = convert_to_json_serializable(all_results)
    
    with open(results_file, 'w') as f:
        json.dump(json_results, f, indent=2)
    
    logger.info(f"\nDetailed results saved to: {results_file}")


if __name__ == "__main__":
    main()