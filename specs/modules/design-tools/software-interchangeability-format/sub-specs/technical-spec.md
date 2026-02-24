# Technical Specification

This is the technical specification for the spec detailed in @specs/modules/design-tools/software-interchangeability-format/spec.md

> Created: 2025-08-20
> Version: 1.0.0

## Technical Requirements

### Core Format Architecture

- **Format Type**: JSON-based with optional binary attachments for large datasets
- **Schema Definition**: JSON Schema Draft 2020-12 for validation
- **Versioning**: Semantic versioning with backward compatibility for minor versions
- **Compression**: Optional gzip compression for large geometry datasets
- **Coordinate Systems**: Support for multiple coordinate system definitions with transformation matrices

### Geometry Representation

- **NURBS Surfaces**: Full control point, knot vector, and degree preservation
- **Mesh Data**: Vertices, faces, normals, and UV coordinates
- **Curves**: Polylines, splines, and NURBS curves with parameterization
- **Solids**: Boundary representation (B-rep) and constructive solid geometry (CSG)
- **Assemblies**: Hierarchical structure with transformation matrices

### Material and Property Systems

- **Material Library**: Extensible material definitions with physical properties
- **Metadata Framework**: Key-value pairs with namespace support
- **Units System**: Explicit unit definitions with automatic conversion
- **Naming Conventions**: UUID-based identification with human-readable aliases

### Performance Specifications

- **File Size**: Optimize for files up to 1GB uncompressed
- **Conversion Speed**: Target < 10 seconds for 100MB models
- **Memory Usage**: Streaming parser for large files to minimize memory footprint
- **Validation**: Real-time schema validation during read/write operations

## Approach Options

**Option A: Single Monolithic Format**
- Pros: Simple implementation, single schema to maintain, easy validation
- Cons: May become bloated, difficult to extend, version conflicts

**Option B: Modular Component System** (Selected)
- Pros: Extensible, allows partial implementation, easier to maintain
- Cons: More complex initial setup, requires component registry

**Option C: Binary-First Format**
- Pros: Compact file sizes, fast parsing, efficient for large datasets
- Cons: Difficult to debug, requires specialized tools, less portable

**Rationale:** The modular component system provides the best balance of extensibility and maintainability. It allows tools to implement only the components they need while maintaining compatibility.

## Implementation Architecture

### Core Library Structure
```
interchange_format/
├── core/
│   ├── schema.py          # Schema definitions and validation
│   ├── reader.py          # Format reading and parsing
│   ├── writer.py          # Format writing and serialization
│   └── validator.py       # Data validation and integrity checks
├── geometry/
│   ├── nurbs.py          # NURBS surface handling
│   ├── mesh.py           # Mesh data structures
│   ├── curves.py         # Curve representations
│   └── transforms.py     # Coordinate transformations
├── converters/
│   ├── rhino3d/          # Rhino3D specific converters
│   ├── orcawave/         # OrcaWave specific converters
│   └── base.py           # Base converter interface
└── utils/
    ├── units.py          # Unit conversion utilities
    └── compression.py    # Compression handling
```

### Data Flow Architecture
1. **Export Pipeline**: Native Format → Parser → Interchange Model → Serializer → File
2. **Import Pipeline**: File → Deserializer → Interchange Model → Generator → Native Format
3. **Validation Pipeline**: Each step includes validation to ensure data integrity

## External Dependencies

- **rhino3dm** (v7.15.0+) - Python library for reading/writing Rhino3D files
  - Justification: Official library from McNeel for 3DM file format access
  
- **numpy** (v1.24.0+) - Numerical computing for geometry operations
  - Justification: Industry standard for efficient array operations
  
- **pydantic** (v2.0+) - Data validation using Python type annotations
  - Justification: Robust schema validation with automatic error messages
  
- **msgpack** (v1.0.5+) - Efficient binary serialization
  - Justification: Optional binary format for performance-critical applications

- **trimesh** (v3.23.0+) - Mesh processing and validation
  - Justification: Comprehensive mesh utilities and format conversions

## Integration Points

### Rhino3D Integration
- **Plugin Type**: RhinoPython script with UI panel
- **Commands**: ExportInterchange, ImportInterchange
- **Grasshopper**: Custom components for parametric workflows

### OrcaWave Integration
- **Interface**: File-based exchange with command-line tools
- **Mesh Generation**: Automatic panel mesh from NURBS surfaces
- **Property Mapping**: Material to hydrodynamic property conversion

## Security Considerations

- Input validation to prevent injection attacks
- File size limits to prevent denial of service
- Sandboxed execution for untrusted conversions
- Digital signatures for authenticated exchanges