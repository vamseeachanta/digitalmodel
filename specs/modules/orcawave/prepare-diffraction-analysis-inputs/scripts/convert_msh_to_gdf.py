"""
Convert GMsh .msh files to OrcaWave GDF format.
"""

import numpy as np
from pathlib import Path
import logging
import argparse
from typing import List, Tuple, Dict, Any
import re

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class MshToGdfConverter:
    """Convert GMsh mesh files to WAMIT GDF format."""
    
    def __init__(self, msh_path: Path):
        """Initialize converter with mesh file path."""
        self.msh_path = Path(msh_path)
        self.nodes = {}
        self.elements = []
        self.surfaces = []
        
    def read_msh_file(self) -> bool:
        """Read and parse GMsh file."""
        try:
            with open(self.msh_path, 'r') as f:
                lines = f.readlines()
                
            logger.info(f"Reading mesh file: {self.msh_path}")
            
            # Parse sections
            i = 0
            while i < len(lines):
                line = lines[i].strip()
                
                if line == '$MeshFormat':
                    i = self._parse_format(lines, i + 1)
                elif line == '$Nodes':
                    i = self._parse_nodes(lines, i + 1)
                elif line == '$Elements':
                    i = self._parse_elements(lines, i + 1)
                else:
                    i += 1
                    
            logger.info(f"Loaded {len(self.nodes)} nodes and {len(self.elements)} elements")
            return True
            
        except Exception as e:
            logger.error(f"Error reading MSH file: {e}")
            return False
            
    def _parse_format(self, lines: List[str], start: int) -> int:
        """Parse mesh format section."""
        i = start
        while i < len(lines) and lines[i].strip() != '$EndMeshFormat':
            # Format: version file-type data-size
            parts = lines[i].strip().split()
            if len(parts) >= 3:
                version = float(parts[0])
                logger.info(f"GMsh format version: {version}")
            i += 1
        return i + 1
        
    def _parse_nodes(self, lines: List[str], start: int) -> int:
        """Parse nodes section."""
        i = start
        
        # Get number of nodes
        num_nodes = int(lines[i].strip())
        i += 1
        
        # Parse each node
        for _ in range(num_nodes):
            parts = lines[i].strip().split()
            node_id = int(parts[0])
            x, y, z = float(parts[1]), float(parts[2]), float(parts[3])
            self.nodes[node_id] = (x, y, z)
            i += 1
            
        # Skip $EndNodes
        while i < len(lines) and lines[i].strip() != '$EndNodes':
            i += 1
            
        return i + 1
        
    def _parse_elements(self, lines: List[str], start: int) -> int:
        """Parse elements section."""
        i = start
        
        # Get number of elements
        num_elements = int(lines[i].strip())
        i += 1
        
        # Parse each element
        for _ in range(num_elements):
            parts = lines[i].strip().split()
            elem_id = int(parts[0])
            elem_type = int(parts[1])
            
            # Type 2 = 3-node triangle
            # Type 3 = 4-node quadrilateral
            if elem_type == 2:  # Triangle
                # Format: id type num_tags [tags] node1 node2 node3
                num_tags = int(parts[2])
                node_start = 3 + num_tags
                nodes = [int(parts[j]) for j in range(node_start, node_start + 3)]
                self.elements.append(('triangle', nodes))
                
            elif elem_type == 3:  # Quadrilateral
                num_tags = int(parts[2])
                node_start = 3 + num_tags
                nodes = [int(parts[j]) for j in range(node_start, node_start + 4)]
                self.elements.append(('quad', nodes))
                
            i += 1
            
        # Skip $EndElements
        while i < len(lines) and lines[i].strip() != '$EndElements':
            i += 1
            
        logger.info(f"Parsed {len(self.elements)} surface elements")
        return i + 1
        
    def convert_to_gdf(self, output_path: Path, scale: float = 1.0):
        """
        Convert loaded mesh to GDF format.
        
        Args:
            output_path: Output GDF file path
            scale: Scale factor for coordinates
        """
        try:
            # Filter only surface elements (triangles and quads)
            surface_elements = [e for e in self.elements if e[0] in ['triangle', 'quad']]
            
            if not surface_elements:
                logger.error("No surface elements found in mesh")
                return False
                
            with open(output_path, 'w') as f:
                # Write header
                f.write(f"    {scale:.6f}     {9.80665:.6f}\n")  # Scale and gravity
                f.write(f"    {0:.6f}     {0:.6f}\n")  # ISX, ISY (no symmetry)
                f.write(f"    {len(surface_elements)}\n")  # Number of panels
                
                # Write panels
                for elem_type, node_ids in surface_elements:
                    if elem_type == 'triangle':
                        # Duplicate last node for triangle (make it quad-compatible)
                        coords = []
                        for node_id in node_ids:
                            x, y, z = self.nodes[node_id]
                            coords.extend([x * scale, y * scale, z * scale])
                        # Duplicate last vertex
                        coords.extend(coords[-3:])
                        
                    elif elem_type == 'quad':
                        coords = []
                        for node_id in node_ids:
                            x, y, z = self.nodes[node_id]
                            coords.extend([x * scale, y * scale, z * scale])
                            
                    # Write 12 values (4 vertices × 3 coordinates)
                    f.write("    ")
                    for i in range(0, 12, 3):
                        f.write(f"{coords[i]:12.6f} {coords[i+1]:12.6f} {coords[i+2]:12.6f}\n")
                        if i < 9:
                            f.write("    ")
                            
            logger.info(f"Successfully wrote GDF file: {output_path}")
            logger.info(f"  - Panels: {len(surface_elements)}")
            logger.info(f"  - Scale factor: {scale}")
            return True
            
        except Exception as e:
            logger.error(f"Error writing GDF file: {e}")
            return False
            
    def validate_mesh(self) -> Dict[str, Any]:
        """Validate mesh properties for OrcaWave."""
        validation = {
            'is_valid': True,
            'warnings': [],
            'errors': [],
            'statistics': {}
        }
        
        # Check if mesh has nodes and elements
        if not self.nodes:
            validation['errors'].append("No nodes found in mesh")
            validation['is_valid'] = False
            
        if not self.elements:
            validation['errors'].append("No elements found in mesh")
            validation['is_valid'] = False
            
        # Calculate mesh statistics
        if self.nodes:
            coords = np.array(list(self.nodes.values()))
            validation['statistics'] = {
                'num_nodes': len(self.nodes),
                'num_elements': len(self.elements),
                'min_coords': coords.min(axis=0).tolist(),
                'max_coords': coords.max(axis=0).tolist(),
                'mesh_size': (coords.max(axis=0) - coords.min(axis=0)).tolist()
            }
            
            # Check if mesh crosses waterline
            z_min, z_max = coords[:, 2].min(), coords[:, 2].max()
            if z_min > 0:
                validation['warnings'].append(f"Mesh entirely above waterline (z_min={z_min:.3f})")
            elif z_max < 0:
                validation['warnings'].append(f"Mesh entirely below waterline (z_max={z_max:.3f})")
            else:
                validation['statistics']['crosses_waterline'] = True
                
        # Check element quality
        if self.elements:
            surface_elems = [e for e in self.elements if e[0] in ['triangle', 'quad']]
            validation['statistics']['surface_elements'] = len(surface_elems)
            
            if len(surface_elems) == 0:
                validation['errors'].append("No surface elements (triangles/quads) found")
                validation['is_valid'] = False
            elif len(surface_elems) < 100:
                validation['warnings'].append(f"Low panel count ({len(surface_elems)}), may affect accuracy")
                
        return validation


def main():
    """Main execution function."""
    parser = argparse.ArgumentParser(description='Convert GMsh .msh to OrcaWave GDF')
    parser.add_argument('msh_file', help='Input GMsh .msh file path')
    parser.add_argument('--output', help='Output GDF file path')
    parser.add_argument('--scale', type=float, default=1.0, help='Scale factor for coordinates')
    parser.add_argument('--validate', action='store_true', help='Validate mesh properties')
    parser.add_argument('--verbose', action='store_true', help='Enable verbose logging')
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
        
    # Setup paths
    msh_path = Path(args.msh_file)
    if args.output:
        output_path = Path(args.output)
    else:
        output_path = msh_path.with_suffix('.gdf')
        
    # Convert mesh
    converter = MshToGdfConverter(msh_path)
    
    if converter.read_msh_file():
        # Validate if requested
        if args.validate:
            validation = converter.validate_mesh()
            print("\n=== Mesh Validation ===")
            print(f"Valid: {validation['is_valid']}")
            
            if validation['errors']:
                print("\nErrors:")
                for error in validation['errors']:
                    print(f"  ❌ {error}")
                    
            if validation['warnings']:
                print("\nWarnings:")
                for warning in validation['warnings']:
                    print(f"  ⚠️ {warning}")
                    
            print("\nStatistics:")
            for key, value in validation['statistics'].items():
                print(f"  {key}: {value}")
                
        # Convert to GDF
        if converter.convert_to_gdf(output_path, args.scale):
            print(f"\n✅ Successfully converted to GDF: {output_path}")
        else:
            print(f"\n❌ Failed to convert to GDF")
    else:
        print(f"\n❌ Failed to read MSH file")
        

if __name__ == "__main__":
    main()