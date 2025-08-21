# Tests Specification

This is the tests coverage details for the spec detailed in @specs/modules/design-tools/software-interchangeability-format/spec.md

> Created: 2025-08-20
> Version: 1.0.0

## Test Coverage

### Unit Tests

**InterchangeDocument Class**
- Test document initialization with default and custom versions
- Test adding geometry objects with automatic UUID generation
- Test adding materials and retrieving by ID
- Test metadata addition with namespace support
- Test schema validation for valid and invalid documents
- Test file serialization and deserialization
- Test compression and decompression

**Geometry Classes**
- Test NURBS surface creation with valid control points
- Test NURBS to mesh conversion with different resolutions
- Test mesh initialization and normal computation
- Test mesh topology validation (manifold checks)
- Test curve representations (polyline, NURBS, arc)
- Test coordinate transformations
- Test bounding box calculations

**Converter Base Class**
- Test abstract method enforcement
- Test file extension validation
- Test error handling for unsupported formats

**Validation Module**
- Test JSON schema validation
- Test geometry validation (valid NURBS parameters)
- Test material property validation
- Test unit system validation
- Test coordinate system validation

**Unit Conversion**
- Test length unit conversions (meter, millimeter, inch, foot)
- Test mass unit conversions
- Test angle conversions (radians to degrees)
- Test compound unit conversions

### Integration Tests

**Rhino3D Integration**
- Test exporting simple geometry from Rhino3D
- Test exporting complex NURBS surfaces with trimming
- Test exporting assemblies with nested components
- Test importing interchange format to Rhino3D
- Test round-trip conversion (Rhino → Interchange → Rhino)
- Test layer and material preservation
- Test handling of Rhino-specific attributes

**OrcaWave Integration**
- Test mesh generation from NURBS surfaces
- Test panel quality for hydrodynamic analysis
- Test property mapping from materials to hydrodynamic properties
- Test coordinate system alignment with OrcaWave conventions
- Test handling of symmetry planes
- Test load point preservation

**File Format Tests**
- Test reading/writing JSON format
- Test binary attachment handling
- Test compressed file operations
- Test large file streaming (>100MB)
- Test concurrent file access
- Test file corruption recovery

### Feature Tests

**End-to-End Workflow: Rhino3D to OrcaWave**
1. Create vessel hull in Rhino3D
2. Export to interchange format
3. Validate interchange document
4. Import into OrcaWave converter
5. Generate panel mesh
6. Verify mesh quality metrics
7. Export OrcaWave input files

**Multi-Tool Pipeline**
1. Create geometry in Rhino3D
2. Export to interchange format
3. Modify in FreeCAD (future)
4. Import back to Rhino3D
5. Verify geometry preservation

**Version Migration**
1. Create document in version 1.0.0
2. Upgrade to version 1.1.0
3. Verify backward compatibility
4. Test deprecated feature handling

### Performance Tests

**Large Model Handling**
- Test 100MB model conversion time (<10 seconds target)
- Test memory usage during conversion
- Test streaming parser with 1GB file
- Test parallel processing for mesh generation

**Batch Processing**
- Test converting 100 small files
- Test converting 10 large files
- Test error recovery in batch operations

### Mocking Requirements

**External Services**
- **Rhino3D COM Interface**: Mock for CI/CD environments without Rhino installation
- **OrcaWave License Server**: Mock license validation for testing
- **File System**: Mock for testing file operations without disk I/O

**Test Data Generation**
- **Geometry Factory**: Generate test NURBS surfaces, meshes, curves
- **Material Library**: Standard test materials with known properties
- **Document Builder**: Create valid test documents programmatically

## Test Implementation Examples

### Unit Test Example
```python
import unittest
from interchange_format.core import InterchangeDocument
from interchange_format.geometry import NurbsSurface
import numpy as np

class TestInterchangeDocument(unittest.TestCase):
    
    def setUp(self):
        self.doc = InterchangeDocument(version="1.0.0")
    
    def test_add_geometry(self):
        # Create test NURBS surface
        control_points = np.random.rand(4, 4, 4)  # 4x4 control points
        knots_u = [0, 0, 0, 0, 1, 1, 1, 1]
        knots_v = [0, 0, 0, 0, 1, 1, 1, 1]
        
        surface = NurbsSurface(
            control_points=control_points,
            knots_u=knots_u,
            knots_v=knots_v,
            degree_u=3,
            degree_v=3
        )
        
        # Add to document
        geom_id = self.doc.add_geometry(surface, name="TestSurface")
        
        # Verify
        self.assertIsNotNone(geom_id)
        self.assertIn(geom_id, self.doc.geometries)
        self.assertEqual(self.doc.geometries[geom_id].name, "TestSurface")
    
    def test_validation(self):
        # Empty document should fail validation
        result = self.doc.validate()
        self.assertFalse(result.is_valid)
        
        # Add required geometry
        mesh = create_test_mesh()
        self.doc.add_geometry(mesh)
        
        # Should now pass validation
        result = self.doc.validate()
        self.assertTrue(result.is_valid)
```

### Integration Test Example
```python
import pytest
from pathlib import Path
from interchange_format.converters.rhino3d import Rhino3DConverter
from interchange_format.converters.orcawave import OrcaWaveConverter

@pytest.mark.integration
class TestRhinoToOrcaWaveWorkflow:
    
    def test_hull_conversion(self, sample_rhino_file):
        # Step 1: Import from Rhino3D
        rhino_converter = Rhino3DConverter()
        doc = rhino_converter.import_model(sample_rhino_file)
        
        # Step 2: Validate interchange document
        validation = doc.validate()
        assert validation.is_valid, f"Validation errors: {validation.errors}"
        
        # Step 3: Export to OrcaWave
        orcawave_converter = OrcaWaveConverter()
        output_path = Path("test_output/hull.dat")
        orcawave_converter.export_model(doc, output_path)
        
        # Step 4: Verify OrcaWave files created
        assert output_path.exists()
        assert output_path.with_suffix('.ctrl').exists()
        
        # Step 5: Check mesh quality
        mesh_quality = orcawave_converter.check_mesh_quality(output_path)
        assert mesh_quality['max_aspect_ratio'] < 5.0
        assert mesh_quality['min_angle'] > 30.0
```

### Mock Test Example
```python
from unittest.mock import Mock, patch
from interchange_format.converters.rhino3d import Rhino3DConverter

class TestRhino3DConverterMocked:
    
    @patch('rhino3dm.File3dm.Read')
    def test_import_without_rhino(self, mock_read):
        # Setup mock
        mock_file = Mock()
        mock_file.Objects = [create_mock_rhino_surface()]
        mock_read.return_value = mock_file
        
        # Test import
        converter = Rhino3DConverter()
        doc = converter.import_model("test.3dm")
        
        # Verify
        assert len(doc.geometries) == 1
        assert list(doc.geometries.values())[0].type == "nurbs_surface"
```

## Test Data

### Sample Files
- `test_data/simple_surface.3dm` - Single NURBS surface
- `test_data/vessel_hull.3dm` - Complex vessel geometry
- `test_data/assembly.3dm` - Multi-component assembly
- `test_data/large_model.3dm` - 100MB test file
- `test_data/corrupted.icf` - Corrupted interchange file for error testing

### Expected Results
- Conversion accuracy: ±0.001mm for geometry
- Mesh quality: Aspect ratio < 5.0, minimum angle > 30°
- Performance: <10 seconds for 100MB file
- Memory usage: <2x file size