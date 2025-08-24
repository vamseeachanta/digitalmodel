"""
Test suite for FreeCAD API wrapper
"""

import sys
import os
import pytest
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.api.wrapper import FreeCADAPIWrapper
from src.api.geometry import GeometryHandler
from src.core.logging_config import setup_logging


class TestFreeCADAPIWrapper:
    """Test FreeCAD API wrapper functionality"""
    
    @pytest.fixture
    def wrapper(self):
        """Create API wrapper instance"""
        config = {
            'settings': {
                'log_level': 'DEBUG',
                'log_file': 'tests/test.log'
            }
        }
        setup_logging(config)
        return FreeCADAPIWrapper(config)
    
    @pytest.fixture
    def geometry(self, wrapper):
        """Create geometry handler"""
        if wrapper.freecad_available:
            doc = wrapper.create_document("TestDoc")
            return GeometryHandler(doc)
        return GeometryHandler()
    
    def test_wrapper_initialization(self, wrapper):
        """Test wrapper initialization"""
        assert wrapper is not None
        assert isinstance(wrapper.documents, dict)
        print(f"FreeCAD available: {wrapper.freecad_available}")
    
    def test_create_document(self, wrapper):
        """Test document creation"""
        if not wrapper.freecad_available:
            pytest.skip("FreeCAD not available")
        
        doc = wrapper.create_document("TestDocument")
        assert doc is not None
        assert "TestDocument" in wrapper.documents
        wrapper.close_document("TestDocument")
    
    def test_create_basic_shapes(self, wrapper):
        """Test basic shape creation"""
        if not wrapper.freecad_available:
            pytest.skip("FreeCAD not available")
        
        doc = wrapper.create_document("ShapeTest")
        
        # Test box creation
        box = wrapper.create_box(100, 50, 25, name="TestBox")
        assert box is not None
        
        # Test cylinder creation
        cylinder = wrapper.create_cylinder(20, 100, position=(100, 0, 0), name="TestCylinder")
        assert cylinder is not None
        
        # Test sphere creation
        sphere = wrapper.create_sphere(30, position=(0, 100, 0), name="TestSphere")
        assert sphere is not None
        
        # Verify objects exist
        objects = wrapper.list_objects()
        assert "TestBox" in objects
        assert "TestCylinder" in objects
        assert "TestSphere" in objects
        
        wrapper.close_document("ShapeTest")
    
    def test_object_manipulation(self, wrapper):
        """Test object manipulation"""
        if not wrapper.freecad_available:
            pytest.skip("FreeCAD not available")
        
        doc = wrapper.create_document("ManipTest")
        box = wrapper.create_box(50, 50, 50, name="MovableBox")
        
        # Test moving
        assert wrapper.move_object(box, (100, 100, 0))
        
        # Test rotation
        assert wrapper.rotate_object(box, (0, 0, 1), 45)
        
        # Test scaling
        assert wrapper.scale_object(box, 2.0)
        
        wrapper.close_document("ManipTest")
    
    def test_sketch_creation(self, geometry):
        """Test sketch creation and constraints"""
        if not geometry.freecad_available:
            pytest.skip("FreeCAD not available")
        
        sketch = geometry.create_sketch("TestSketch")
        assert sketch is not None
        
        # Add geometry to sketch
        line1 = geometry.add_line_to_sketch(sketch, (0, 0), (100, 0))
        line2 = geometry.add_line_to_sketch(sketch, (100, 0), (100, 50))
        line3 = geometry.add_line_to_sketch(sketch, (100, 50), (0, 50))
        line4 = geometry.add_line_to_sketch(sketch, (0, 50), (0, 0))
        
        assert line1 >= 0
        assert line2 >= 0
        assert line3 >= 0
        assert line4 >= 0
        
        # Add constraints
        assert geometry.add_constraint_horizontal(sketch, line1)
        assert geometry.add_constraint_vertical(sketch, line2)
        assert geometry.add_constraint_distance(sketch, line1, 100)
    
    def test_extrusion(self, geometry):
        """Test sketch extrusion"""
        if not geometry.freecad_available:
            pytest.skip("FreeCAD not available")
        
        sketch = geometry.create_sketch("ExtrudeSketch")
        
        # Create a rectangle
        geometry.add_line_to_sketch(sketch, (0, 0), (50, 0))
        geometry.add_line_to_sketch(sketch, (50, 0), (50, 30))
        geometry.add_line_to_sketch(sketch, (50, 30), (0, 30))
        geometry.add_line_to_sketch(sketch, (0, 30), (0, 0))
        
        # Extrude the sketch
        extrusion = geometry.extrude_sketch(sketch, 100, "TestExtrusion")
        assert extrusion is not None
    
    def test_export_functionality(self, wrapper):
        """Test export to different formats"""
        if not wrapper.freecad_available:
            pytest.skip("FreeCAD not available")
        
        doc = wrapper.create_document("ExportTest")
        box = wrapper.create_box(100, 100, 100, name="ExportBox")
        
        # Test export to STEP
        step_file = "tests/test_export.step"
        assert wrapper.export_document(step_file, "STEP")
        
        # Clean up
        if Path(step_file).exists():
            Path(step_file).unlink()
        
        wrapper.close_document("ExportTest")
    
    def test_error_handling(self, wrapper):
        """Test error handling"""
        # Test operations without document
        assert wrapper.save_document() is False
        assert wrapper.close_document("NonExistent") is False
        
        # Test invalid operations
        if wrapper.freecad_available:
            doc = wrapper.create_document("ErrorTest")
            assert wrapper.export_document("test.xyz") is False  # Invalid format
            wrapper.close_document("ErrorTest")


def run_tests():
    """Run all tests and return results"""
    import subprocess
    
    # Use uv to run pytest
    result = subprocess.run(
        ["uv", "run", "pytest", __file__, "-v", "--tb=short"],
        capture_output=True,
        text=True
    )
    
    print(result.stdout)
    if result.stderr:
        print(result.stderr)
    
    return result.returncode == 0


if __name__ == "__main__":
    # If run directly, execute tests
    success = run_tests()
    sys.exit(0 if success else 1)