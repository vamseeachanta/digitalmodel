# Prompt Documentation

> Spec: Software Interchangeability Format
> Created: 2025-08-20
> Module: design-tools

## Original User Request

```
To the module: specs/modules/design-tools, create a software interchangeability format. Start with rhino3 and orcawave. Following this we will add autocad, solidworks, freecad, blender tools to research the landscape.
```

## Interpreted Requirements

1. **Primary Goal**: Create a universal format for exchanging engineering design data between different CAD and analysis tools
2. **Initial Focus**: Rhino3D (CAD/modeling) and OrcaWave (hydrodynamic analysis)
3. **Future Expansion**: AutoCAD, SolidWorks, FreeCAD, Blender
4. **Domain**: Offshore engineering and marine design
5. **Key Challenge**: Preserving geometry fidelity and engineering metadata across different tool paradigms

## Key Design Decisions

### 1. Format Architecture
- **Decision**: JSON-based with optional binary attachments
- **Rationale**: Human-readable for debugging, extensible schema, efficient binary option for large datasets
- **Alternative Considered**: Pure binary format (rejected for lack of transparency)

### 2. Geometry Representation
- **Decision**: Support multiple representations (NURBS, mesh, B-rep)
- **Rationale**: Different tools have different native representations; preserving the original gives best fidelity
- **Alternative Considered**: Convert everything to mesh (rejected due to loss of parametric information)

### 3. Integration Approach
- **Decision**: Plugin/script-based for each tool
- **Rationale**: Native integration provides best user experience and access to tool-specific features
- **Alternative Considered**: Standalone converter application (rejected for workflow disruption)

### 4. Schema Design
- **Decision**: Modular component system
- **Rationale**: Tools can implement only what they need, easier to extend
- **Alternative Considered**: Monolithic schema (rejected for complexity and versioning issues)

## Technical Highlights

### Core Innovation
The interchange format acts as a "hub" rather than requiring N×N converters between tools. Each tool only needs to implement conversion to/from the interchange format.

### Key Features
1. **Geometry Preservation**: NURBS surfaces maintain full mathematical definition
2. **Material System**: Extensible properties for both structural and visual attributes
3. **Coordinate Systems**: Multiple coordinate system support with transformations
4. **Validation**: Schema-based validation ensures data integrity
5. **Performance**: Streaming parsers and optional compression for large models

### Integration Points
- **Rhino3D**: RhinoPython scripts with UI integration
- **OrcaWave**: Command-line tools with automatic mesh generation
- **Future Tools**: Defined API for adding new converters

## Implementation Strategy

### Phase 1: Foundation (Weeks 1-2)
- Core schema definition
- Basic Python library
- Validation framework

### Phase 2: Rhino3D Integration (Weeks 3-4)
- Rhino3dm library integration
- Export/import commands
- Grasshopper components

### Phase 3: OrcaWave Integration (Weeks 5-6)
- Mesh generation algorithms
- Hydrodynamic property mapping
- Quality validation

### Phase 4: Testing & Documentation (Week 7)
- Comprehensive test suite
- User documentation
- Example workflows

### Phase 5: Research Additional Tools (Week 8)
- AutoCAD feasibility study
- SolidWorks API investigation
- Open-source tool integration planning

## Challenges and Mitigations

### Challenge 1: Geometry Fidelity
- **Issue**: Loss of precision in conversions
- **Mitigation**: Use high-precision floating point, validate tolerances

### Challenge 2: Tool-Specific Features
- **Issue**: Not all features translate between tools
- **Mitigation**: Extensible metadata system, clear documentation of limitations

### Challenge 3: Performance with Large Models
- **Issue**: Ship hulls and offshore structures can be very large
- **Mitigation**: Streaming parsers, binary attachments, lazy loading

### Challenge 4: Version Compatibility
- **Issue**: Tools and formats evolve
- **Mitigation**: Semantic versioning, migration scripts, backward compatibility

## Success Metrics

1. **Geometry Accuracy**: <0.001mm deviation in round-trip conversions
2. **Performance**: <10 seconds for 100MB models
3. **Adoption**: Successfully used in at least 3 real projects
4. **Extensibility**: Add support for 2 additional tools without schema changes
5. **Reliability**: 99.9% successful conversions in production use

## Future Enhancements

1. **Cloud Service**: REST API for format conversion
2. **Version Control Integration**: Git-friendly diff format
3. **Parametric Preservation**: Maintain design history and constraints
4. **Analysis Results**: Include simulation results in interchange format
5. **AI-Assisted Conversion**: Machine learning for optimal mesh generation

## Curated Reuse Prompt

```
Create a comprehensive software interchangeability format specification for engineering design tools. The format should:

1. Support geometry exchange between CAD tools (Rhino3D, AutoCAD, SolidWorks, FreeCAD, Blender) and analysis tools (OrcaWave, OrcaFlex)
2. Use JSON schema with optional binary attachments for large datasets
3. Preserve NURBS surfaces, meshes, materials, and metadata
4. Include plugin/script integrations for each tool
5. Provide validation, unit conversion, and coordinate transformation
6. Target offshore engineering workflows but be extensible to other domains

Focus on practical implementation with Python libraries, clear API design, and comprehensive testing. Prioritize geometry fidelity and performance for large models (100MB+). Include research pathways for expanding tool support.
```

## References

- Rhino3dm Python Documentation: https://github.com/mcneel/rhino3dm
- OrcaWave User Manual: [Internal reference]
- JSON Schema Specification: https://json-schema.org/
- ISO 10303 (STEP) standard for inspiration
- OpenCASCADE for geometry algorithms