# API Specification

This is the API specification for the spec detailed in @specs/modules/design-tools/software-interchangeability-format/spec.md

> Created: 2025-08-20
> Version: 1.0.0

## Core API Endpoints

### Python API

#### InterchangeDocument Class
```python
class InterchangeDocument:
    """Main document container for interchange format"""
    
    def __init__(self, version: str = "1.0.0"):
        """Initialize new interchange document"""
        
    def add_geometry(self, geometry: Geometry, name: str = None) -> str:
        """Add geometry object to document, returns UUID"""
        
    def add_material(self, material: Material) -> str:
        """Add material definition, returns material ID"""
        
    def add_metadata(self, key: str, value: Any, namespace: str = "user") -> None:
        """Add metadata key-value pair"""
        
    def validate(self) -> ValidationResult:
        """Validate document against schema"""
        
    def to_file(self, filepath: str, compress: bool = False) -> None:
        """Write document to file"""
        
    @classmethod
    def from_file(cls, filepath: str) -> 'InterchangeDocument':
        """Load document from file"""
```

#### Geometry Classes
```python
class NurbsSurface(Geometry):
    """NURBS surface representation"""
    
    def __init__(self, 
                 control_points: np.ndarray,
                 knots_u: List[float],
                 knots_v: List[float],
                 degree_u: int,
                 degree_v: int):
        """Initialize NURBS surface"""
        
    def to_mesh(self, u_divisions: int = 20, v_divisions: int = 20) -> Mesh:
        """Convert NURBS to mesh representation"""

class Mesh(Geometry):
    """Triangulated mesh representation"""
    
    def __init__(self,
                 vertices: np.ndarray,
                 faces: np.ndarray,
                 normals: np.ndarray = None):
        """Initialize mesh"""
        
    def compute_normals(self) -> None:
        """Compute vertex normals if not provided"""
        
    def validate_topology(self) -> bool:
        """Check mesh for holes, non-manifold edges"""
```

#### Converter Interface
```python
class ConverterBase(ABC):
    """Abstract base class for format converters"""
    
    @abstractmethod
    def import_model(self, filepath: str) -> InterchangeDocument:
        """Import from native format to interchange"""
        
    @abstractmethod
    def export_model(self, document: InterchangeDocument, filepath: str) -> None:
        """Export from interchange to native format"""
        
    @abstractmethod
    def supported_extensions(self) -> List[str]:
        """Return list of supported file extensions"""
```

### Rhino3D Integration API

#### Rhino Python Script Commands
```python
def ExportInterchangeFormat():
    """Rhino command to export selected objects"""
    # Get selected objects
    objects = rs.SelectedObjects()
    if not objects:
        print("No objects selected")
        return
    
    # Create interchange document
    doc = InterchangeDocument()
    
    # Convert each object
    for obj_id in objects:
        geometry = convert_rhino_geometry(obj_id)
        doc.add_geometry(geometry, rs.ObjectName(obj_id))
    
    # Save file
    filepath = rs.SaveFileName("Save Interchange Format", "Interchange Files (*.icf)|*.icf")
    if filepath:
        doc.to_file(filepath)

def ImportInterchangeFormat():
    """Rhino command to import interchange format"""
    filepath = rs.OpenFileName("Open Interchange Format", "Interchange Files (*.icf)|*.icf")
    if not filepath:
        return
    
    # Load document
    doc = InterchangeDocument.from_file(filepath)
    
    # Create Rhino objects
    for geom_id, geometry in doc.geometries.items():
        create_rhino_object(geometry)
```

### OrcaWave Integration API

#### Command-Line Interface
```bash
# Convert interchange format to OrcaWave mesh
icf2orcawave input.icf -o output.mesh \
    --mesh-size 0.5 \
    --growth-rate 1.2 \
    --feature-angle 30

# Convert OrcaWave results back to interchange
orcawave2icf results.dat -o results.icf \
    --include-pressures \
    --include-motions
```

#### Python Integration
```python
class OrcaWaveConverter(ConverterBase):
    """OrcaWave specific converter implementation"""
    
    def import_model(self, filepath: str) -> InterchangeDocument:
        """Import OrcaWave model to interchange format"""
        # Read OrcaWave data files
        geometry = read_orcawave_geometry(filepath)
        properties = read_orcawave_properties(filepath)
        
        # Create interchange document
        doc = InterchangeDocument()
        doc.add_geometry(geometry)
        doc.add_metadata("orcawave_properties", properties)
        
        return doc
    
    def export_model(self, document: InterchangeDocument, filepath: str) -> None:
        """Export interchange format to OrcaWave"""
        # Generate panel mesh from geometry
        mesh = generate_panel_mesh(document)
        
        # Write OrcaWave input files
        write_geometry_file(mesh, filepath)
        write_control_file(document.metadata, filepath)
```

## REST API Specification (Future Enhancement)

### Endpoints
```yaml
openapi: 3.0.0
info:
  title: Interchange Format API
  version: 1.0.0

paths:
  /convert:
    post:
      summary: Convert between formats
      requestBody:
        content:
          multipart/form-data:
            schema:
              type: object
              properties:
                file:
                  type: string
                  format: binary
                from_format:
                  type: string
                  enum: [rhino3d, orcawave, autocad, solidworks]
                to_format:
                  type: string
                  enum: [interchange, rhino3d, orcawave]
      responses:
        200:
          description: Successful conversion
          content:
            application/octet-stream:
              schema:
                type: string
                format: binary
                
  /validate:
    post:
      summary: Validate interchange format file
      requestBody:
        content:
          multipart/form-data:
            schema:
              type: object
              properties:
                file:
                  type: string
                  format: binary
      responses:
        200:
          description: Validation result
          content:
            application/json:
              schema:
                type: object
                properties:
                  valid:
                    type: boolean
                  errors:
                    type: array
                    items:
                      type: string
```

## Error Handling

All API methods should handle the following error conditions:
- `FileNotFoundError`: Input file does not exist
- `ValidationError`: Data fails schema validation
- `ConversionError`: Unable to convert between formats
- `GeometryError`: Invalid geometry detected
- `MemoryError`: File too large for available memory

## Performance Considerations

- Lazy loading for large files
- Streaming API for memory-efficient processing
- Parallel processing for mesh generation
- Caching for repeated conversions