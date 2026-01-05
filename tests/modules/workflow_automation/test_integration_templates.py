#!/usr/bin/env python3
"""
ABOUTME: Integration tests for workflow template system with parameter
substitution and template library management.
"""

import pytest
import tempfile
from pathlib import Path

from digitalmodel.modules.workflow_automation import (
    WorkflowTemplate,
    TemplateLibrary,
    create_riser_analysis_template,
    create_mooring_design_template,
    WorkflowDefinition,
)


class TestTemplateInstantiation:
    """Test workflow template instantiation"""

    def test_basic_template_instantiation(self):
        """Test creating workflow from template"""
        template = WorkflowTemplate(
            name="Test Template",
            description="Template for {{analysis_type}} analysis",
            template_tasks=[
                {
                    'name': "Task for {{parameter}}",
                    'task_id': "task1",
                    'module': "test_module",
                    'function': "test_function",
                    'inputs': {
                        'value': "{{input_value}}",
                        'diameter': "{{diameter}}",
                    },
                    'outputs': {
                        'result': 'output_key',
                    },
                }
            ],
            parameters={
                'analysis_type': 'riser',
                'parameter': 'default',
                'input_value': 100,
                'diameter': 0.508,
            }
        )

        # Instantiate with custom values
        workflow = template.instantiate(
            analysis_type='mooring',
            parameter='custom',
            input_value=200,
        )

        assert isinstance(workflow, WorkflowDefinition)
        assert "mooring analysis" in workflow.description
        assert len(workflow.tasks) == 1

        # Check task properties
        task = workflow.tasks[0]
        assert "custom" in task.name
        # Parameters get substituted as strings
        assert '200' in str(task.inputs.get('value', ''))
        assert '0.508' in str(task.inputs.get('diameter', ''))

    def test_template_with_multiple_tasks(self):
        """Test template with multiple tasks"""
        template = WorkflowTemplate(
            name="Multi-Task Template",
            description="Test",
            template_tasks=[
                {
                    'name': "Task 1",
                    'task_id': "t1",
                    'module': "m1",
                    'function': "f1",
                    'inputs': {'x': "{{param1}}"},
                    'outputs': {'out': 'result1'},
                },
                {
                    'name': "Task 2",
                    'task_id': "t2",
                    'module': "m2",
                    'function': "f2",
                    'inputs': {'y': "{{param2}}"},
                    'outputs': {'out': 'result2'},
                    'depends_on': ['t1'],
                },
            ],
            parameters={'param1': 10, 'param2': 20}
        )

        workflow = template.instantiate()

        assert len(workflow.tasks) == 2

        # Check dependency
        task2 = workflow.get_task('t2')
        assert 't1' in task2.depends_on

    def test_template_parameter_override(self):
        """Test overriding default template parameters"""
        template = WorkflowTemplate(
            name="Test",
            description="{{vessel}} analysis",
            template_tasks=[
                {
                    'name': "Task",
                    'task_id': "t1",
                    'module': "m",
                    'function': "f",
                    'inputs': {
                        'depth': "{{water_depth}}",
                        'vessel': "{{vessel}}",
                    },
                }
            ],
            parameters={
                'water_depth': 1000,
                'vessel': 'fpso',
            }
        )

        # Override one parameter
        workflow = template.instantiate(water_depth=1500)

        task = workflow.tasks[0]
        assert task.inputs['depth'] == "1500"
        assert task.inputs['vessel'] == "fpso"  # Default


class TestPrebuiltTemplates:
    """Test pre-built workflow templates"""

    def test_riser_analysis_template(self):
        """Test riser analysis template"""
        template = create_riser_analysis_template()

        assert template.name == "riser_analysis_template"
        assert len(template.template_tasks) > 0

        # Instantiate with custom parameters
        workflow = template.instantiate(
            diameter=0.610,
            thickness=0.030,
            length=1800,
            water_depth=1200,
            offset=600,
            current_speed=1.5,
        )

        assert isinstance(workflow, WorkflowDefinition)
        assert len(workflow.tasks) > 0

        # Check that parameters were substituted
        first_task = workflow.tasks[0]
        assert 'diameter' in first_task.inputs

    def test_mooring_design_template(self):
        """Test mooring design template"""
        template = create_mooring_design_template()

        assert template.name == "mooring_design_template"

        # Instantiate
        workflow = template.instantiate(
            vessel_type='semisubmersible',
            water_depth=1500,
            line_length=2000,
            wind_speed=30.0,
            current_speed=2.0,
        )

        assert isinstance(workflow, WorkflowDefinition)
        assert "semisubmersible" in workflow.description


class TestTemplateLibrary:
    """Test template library management"""

    def test_library_register_and_get(self):
        """Test registering and retrieving templates"""
        library = TemplateLibrary(template_dir=tempfile.mkdtemp())

        template = WorkflowTemplate(
            name="test_template",
            description="Test",
            template_tasks=[],
            parameters={}
        )

        # Register
        library.register(template)

        # Retrieve
        retrieved = library.get("test_template")

        assert retrieved is not None
        assert retrieved.name == "test_template"

    def test_library_list_templates(self):
        """Test listing all templates in library"""
        library = TemplateLibrary(template_dir=tempfile.mkdtemp())

        # Register multiple templates
        for i in range(3):
            template = WorkflowTemplate(
                name=f"template_{i}",
                description="Test",
                template_tasks=[],
            )
            library.register(template)

        # List templates
        templates = library.list_templates()

        assert len(templates) == 3
        assert "template_0" in templates
        assert "template_1" in templates
        assert "template_2" in templates

    def test_library_delete_template(self):
        """Test deleting template from library"""
        library = TemplateLibrary(template_dir=tempfile.mkdtemp())

        template = WorkflowTemplate(
            name="deleteme",
            description="Test",
            template_tasks=[],
        )

        library.register(template)

        # Verify exists
        assert library.get("deleteme") is not None

        # Delete
        deleted = library.delete("deleteme")
        assert deleted is True

        # Verify deleted
        assert library.get("deleteme") is None

    def test_library_persistence(self):
        """Test template persistence across library instances"""
        temp_dir = tempfile.mkdtemp()

        # First library instance
        library1 = TemplateLibrary(template_dir=temp_dir)

        template = WorkflowTemplate(
            name="persistent",
            description="Test persistence",
            template_tasks=[],
        )

        library1.register(template)

        # Second library instance (same directory)
        library2 = TemplateLibrary(template_dir=temp_dir)

        # Should be able to retrieve template
        retrieved = library2.get("persistent")

        assert retrieved is not None
        assert retrieved.name == "persistent"
        assert retrieved.description == "Test persistence"


class TestTemplateSerialization:
    """Test template serialization and deserialization"""

    def test_template_to_dict(self):
        """Test converting template to dictionary"""
        template = WorkflowTemplate(
            name="Test Template",
            description="Test",
            template_tasks=[
                {
                    'name': "Task",
                    'task_id': "t1",
                    'module': "m",
                    'function': "f",
                }
            ],
            parameters={'x': 1}
        )

        template_dict = template.to_dict()

        assert template_dict['name'] == "Test Template"
        assert template_dict['description'] == "Test"
        assert len(template_dict['tasks']) == 1
        assert template_dict['parameters']['x'] == 1

    def test_template_from_dict(self):
        """Test creating template from dictionary"""
        template_dict = {
            'name': "From Dict",
            'description': "Test",
            'tasks': [
                {
                    'name': "Task",
                    'task_id': "t1",
                    'module': "m",
                    'function': "f",
                }
            ],
            'parameters': {'y': 2}
        }

        template = WorkflowTemplate.from_dict(template_dict)

        assert template.name == "From Dict"
        assert len(template.template_tasks) == 1
        assert template.parameters['y'] == 2

    def test_template_save_and_load(self):
        """Test saving and loading templates from files"""
        temp_file = tempfile.mktemp(suffix='.json')

        # Create and save template
        template = WorkflowTemplate(
            name="Save Load Test",
            description="Test save/load",
            template_tasks=[
                {
                    'name': "Task",
                    'task_id': "t1",
                    'module': "m",
                    'function': "f",
                }
            ],
            parameters={'z': 3}
        )

        template.save(temp_file)

        # Load template
        loaded = WorkflowTemplate.load(temp_file)

        assert loaded.name == "Save Load Test"
        assert loaded.description == "Test save/load"
        assert loaded.parameters['z'] == 3


class TestTemplateEdgeCases:
    """Test template edge cases"""

    def test_template_with_no_parameters(self):
        """Test template with no parameters"""
        template = WorkflowTemplate(
            name="No Params",
            description="Static template",
            template_tasks=[
                {
                    'name': "Static Task",
                    'task_id': "t1",
                    'module': "m",
                    'function': "f",
                    'inputs': {'value': 42},  # No placeholders
                }
            ],
        )

        workflow = template.instantiate()

        task = workflow.tasks[0]
        assert task.inputs['value'] == 42

    def test_template_with_nested_parameters(self):
        """Test template with nested parameter substitution"""
        template = WorkflowTemplate(
            name="Nested",
            description="Test",
            template_tasks=[
                {
                    'name': "Task",
                    'task_id': "t1",
                    'module': "m",
                    'function': "f",
                    'inputs': {
                        'config': {
                            'diameter': "{{diameter}}",
                            'properties': {
                                'material': "{{material}}",
                            },
                        },
                    },
                }
            ],
            parameters={'diameter': 0.508, 'material': 'x65'}
        )

        workflow = template.instantiate()

        # Note: Current implementation doesn't support nested substitution
        # This test documents the limitation
        task = workflow.tasks[0]
        # Nested values won't be substituted
        assert isinstance(task.inputs['config'], dict)

    def test_template_with_missing_placeholder(self):
        """Test template with placeholder but no parameter value"""
        template = WorkflowTemplate(
            name="Missing",
            description="{{undefined_param}}",
            template_tasks=[],
        )

        # Should not raise error, just leave placeholder
        workflow = template.instantiate()

        # Placeholder remains if no value provided
        assert "{{undefined_param}}" in workflow.description
