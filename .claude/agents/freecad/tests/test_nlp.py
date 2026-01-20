"""
Test suite for NLP components
"""

import sys
from pathlib import Path
import pytest

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.nlp.parser import NaturalLanguageParser, CommandIntent, ObjectType
from src.nlp.generator import ScriptGenerator
from src.nlp.templates import TemplateLibrary
from src.nlp.ai_integration import AIIntegration


class TestNaturalLanguageParser:
    """Test natural language parser"""
    
    @pytest.fixture
    def parser(self):
        return NaturalLanguageParser()
    
    def test_parse_create_box(self, parser):
        """Test parsing box creation command"""
        command = parser.parse("Create a box 100x50x25")
        
        assert command.intent == CommandIntent.CREATE_OBJECT
        assert command.object_type == ObjectType.BOX
        assert command.parameters['length'] == 100
        assert command.parameters['width'] == 50
        assert command.parameters['height'] == 25
        assert command.confidence > 0.5
    
    def test_parse_create_cylinder(self, parser):
        """Test parsing cylinder creation"""
        command = parser.parse("Make a cylinder with radius 30 and height 100")
        
        assert command.intent == CommandIntent.CREATE_OBJECT
        assert command.object_type == ObjectType.CYLINDER
        assert command.parameters['radius'] == 30
        assert command.parameters['height'] == 100
    
    def test_parse_create_sphere(self, parser):
        """Test parsing sphere creation"""
        command = parser.parse("Generate a sphere radius 50mm")
        
        assert command.intent == CommandIntent.CREATE_OBJECT
        assert command.object_type == ObjectType.SPHERE
        assert command.parameters['radius'] == 50
    
    def test_parse_hull_creation(self, parser):
        """Test parsing marine hull creation"""
        command = parser.parse("Create a ship hull 150m length with 25m beam")
        
        assert command.intent == CommandIntent.CREATE_OBJECT
        assert command.object_type == ObjectType.HULL
        assert command.parameters['length'] == 150000  # Converted to mm
        assert command.parameters['width'] == 25000  # Beam converted to width
    
    def test_parse_export_command(self, parser):
        """Test parsing export command"""
        command = parser.parse("Export to STEP format")
        
        assert command.intent == CommandIntent.EXPORT_FILE
    
    def test_parse_modify_command(self, parser):
        """Test parsing modification command"""
        command = parser.parse("Move the box to position (100, 50, 0)")
        
        assert command.intent == CommandIntent.MODIFY_OBJECT
        assert command.parameters['position'] == (100, 50, 0)
    
    def test_parse_with_name(self, parser):
        """Test parsing with object name"""
        command = parser.parse('Create a box called "MyBox" 100x100x100')
        
        assert command.parameters.get('name') == 'MyBox'
    
    def test_unit_conversion(self, parser):
        """Test unit conversion"""
        command = parser.parse("Create a cylinder radius 2 inches height 10 cm")
        
        assert command.parameters['radius'] == pytest.approx(50.8, rel=0.1)  # 2 inches in mm
        assert command.parameters['height'] == 100  # 10 cm in mm
    
    def test_context_management(self, parser):
        """Test context preservation"""
        command1 = parser.parse("Create a box named TestBox")
        command2 = parser.parse("Create another box")
        
        assert parser.context['last_object_name'] == 'TestBox'
        assert parser.context['command_count'] == 2


class TestScriptGenerator:
    """Test script generator"""
    
    @pytest.fixture
    def generator(self):
        return ScriptGenerator()
    
    @pytest.fixture
    def parser(self):
        return NaturalLanguageParser()
    
    def test_generate_box_script(self, generator, parser):
        """Test box script generation"""
        command = parser.parse("Create a box 100x50x25")
        script = generator.generate(command)
        
        assert "import FreeCAD" in script
        assert "import Part" in script
        assert "Part.makeBox(100, 50, 25)" in script
        assert 'addObject("Part::Feature"' in script
    
    def test_generate_cylinder_script(self, generator, parser):
        """Test cylinder script generation"""
        command = parser.parse("Create a cylinder radius 30 height 100")
        script = generator.generate(command)
        
        assert "Part.makeCylinder(30, 100)" in script
    
    def test_generate_hull_script(self, generator, parser):
        """Test hull script generation"""
        command = parser.parse("Create a hull 150m long")
        script = generator.generate(command)
        
        assert "Hull parameters" in script
        assert "makeLoft" in script or "BSpline" in script
    
    def test_script_validation(self, generator, parser):
        """Test script validation"""
        command = parser.parse("Create a box")
        script = generator.generate(command)
        
        # Should compile without syntax errors
        compile(script, '<string>', 'exec')
    
    def test_batch_generation(self, generator, parser):
        """Test batch script generation"""
        commands = [
            parser.parse("Create a box 100x100x100"),
            parser.parse("Create a cylinder radius 50 height 200")
        ]
        
        script = generator.batch_generate(commands)
        
        assert "# Batch generated script" in script
        assert "# Command 1:" in script
        assert "# Command 2:" in script
        assert script.count("import FreeCAD") == 1  # Single import


class TestTemplateLibrary:
    """Test template library"""
    
    @pytest.fixture
    def library(self):
        return TemplateLibrary()
    
    def test_builtin_templates(self, library):
        """Test built-in templates are loaded"""
        templates = library.list_templates()
        
        assert len(templates) > 0
        template_names = [t['name'] for t in templates]
        assert 'parametric_box' in template_names
        assert 'gear' in template_names
        assert 'pipe' in template_names
    
    def test_get_template(self, library):
        """Test retrieving a template"""
        template = library.get_template('pipe')
        
        assert template is not None
        assert 'outer_radius' in template.parameters
        assert 'inner_radius' in template.parameters
    
    def test_render_template(self, library):
        """Test rendering a template"""
        template = library.get_template('pipe')
        
        params = {
            'name': 'TestPipe',
            'outer_radius': 50,
            'inner_radius': 40,
            'length': 1000,
            'position': '(0, 0, 0)'
        }
        
        script = template.render(params)
        
        assert 'TestPipe' in script
        assert 'outer_radius = 50' in script
        assert 'inner_radius = 40' in script


class TestAIIntegration:
    """Test AI integration layer"""
    
    @pytest.fixture
    def ai(self):
        return AIIntegration(model='local')
    
    @pytest.fixture
    def parser(self):
        return NaturalLanguageParser()
    
    def test_enhance_command(self, ai, parser):
        """Test command enhancement"""
        command = parser.parse("Create a hull")
        enhanced = ai.enhance_command(command)
        
        # Should add default dimensions
        assert 'length' in enhanced.parameters
        assert 'width' in enhanced.parameters
    
    def test_design_suggestions(self, ai, parser):
        """Test design suggestion generation"""
        command = parser.parse("Create a hull 150m long")
        enhanced = ai.enhance_command(command)
        
        suggestions = enhanced.parameters.get('suggestions', [])
        assert len(suggestions) > 0
    
    def test_error_interpretation(self, ai):
        """Test error message interpretation"""
        error = Exception("No active document")
        context = {'intent': CommandIntent.CREATE_OBJECT}
        
        guidance = ai.interpret_error(error, context)
        
        assert "Create a new document" in guidance
    
    def test_learning_feedback(self, ai, parser):
        """Test learning from feedback"""
        command = parser.parse("Create a box")
        
        ai.learn_from_feedback(command, success=True)
        ai.learn_from_feedback(command, success=False, feedback="Dimensions too small")
        
        summary = ai.get_learning_summary()
        assert summary['total_commands'] == 2
        assert summary['successful_commands'] == 1
    
    def test_suggest_next_action(self, ai, parser):
        """Test next action suggestions"""
        command = parser.parse("Create a box named TestBox")
        suggestions = ai.suggest_next_action(command)
        
        assert len(suggestions) > 0
        assert any("fillet" in s.lower() for s in suggestions)


def run_tests():
    """Run all NLP tests"""
    import subprocess
    
    result = subprocess.run(
        ["python", "-m", "pytest", __file__, "-v", "--tb=short"],
        capture_output=True,
        text=True
    )
    
    print(result.stdout)
    if result.stderr:
        print(result.stderr)
    
    return result.returncode == 0


if __name__ == "__main__":
    success = run_tests()
    sys.exit(0 if success else 1)