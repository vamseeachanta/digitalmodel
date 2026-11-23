#!/usr/bin/env python3
"""
ABOUTME: Workflow template system for creating reusable workflow patterns
with parameter substitution and customization support.
"""

from typing import Dict, Any, Optional, List
from pathlib import Path
import json

from .models import WorkflowDefinition, WorkflowTask


class WorkflowTemplate:
    """
    Reusable workflow template with parameter substitution

    Templates allow creating workflows with placeholder values that
    can be substituted at instantiation time.
    """

    def __init__(
        self,
        name: str,
        description: str,
        template_tasks: List[Dict[str, Any]],
        parameters: Optional[Dict[str, Any]] = None,
    ):
        """
        Initialize workflow template

        Args:
            name: Template name
            description: Template description
            template_tasks: List of task specifications with placeholders
            parameters: Default parameter values
        """
        self.name = name
        self.description = description
        self.template_tasks = template_tasks
        self.parameters = parameters or {}

    def instantiate(self, **kwargs) -> WorkflowDefinition:
        """
        Create workflow instance from template

        Args:
            **kwargs: Parameter values to substitute

        Returns:
            WorkflowDefinition with substituted parameters
        """
        # Merge default parameters with provided values
        params = {**self.parameters, **kwargs}

        # Create workflow
        workflow = WorkflowDefinition(
            name=self._substitute(self.name, params),
            description=self._substitute(self.description, params),
        )

        # Create tasks with parameter substitution
        for task_spec in self.template_tasks:
            task = WorkflowTask(
                name=self._substitute(task_spec['name'], params),
                task_id=task_spec['task_id'],
                module=task_spec['module'],
                function=task_spec['function'],
                inputs=self._substitute_dict(task_spec.get('inputs', {}), params),
                outputs=task_spec.get('outputs', {}),
                depends_on=task_spec.get('depends_on', []),
                required=task_spec.get('required', True),
            )
            workflow.add_task(task)

        return workflow

    def _substitute(self, value: Any, params: Dict[str, Any]) -> Any:
        """
        Substitute placeholders in value

        Args:
            value: Value to substitute
            params: Parameter values

        Returns:
            Substituted value
        """
        if isinstance(value, str):
            # Replace {{param}} with param value
            result = value
            for key, val in params.items():
                placeholder = f"{{{{{key}}}}}"
                if placeholder in result:
                    result = result.replace(placeholder, str(val))
            return result
        else:
            return value

    def _substitute_dict(self, d: Dict[str, Any], params: Dict[str, Any]) -> Dict[str, Any]:
        """
        Substitute placeholders in dictionary

        Args:
            d: Dictionary to substitute
            params: Parameter values

        Returns:
            Dictionary with substituted values
        """
        result = {}
        for key, value in d.items():
            if isinstance(value, str):
                result[key] = self._substitute(value, params)
            elif isinstance(value, dict):
                result[key] = self._substitute_dict(value, params)
            else:
                result[key] = value
        return result

    def to_dict(self) -> Dict[str, Any]:
        """Convert template to dictionary"""
        return {
            'name': self.name,
            'description': self.description,
            'tasks': self.template_tasks,
            'parameters': self.parameters,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'WorkflowTemplate':
        """Create template from dictionary"""
        return cls(
            name=data['name'],
            description=data['description'],
            template_tasks=data['tasks'],
            parameters=data.get('parameters', {}),
        )

    def save(self, filepath: str) -> None:
        """
        Save template to JSON file

        Args:
            filepath: Path to save template
        """
        path = Path(filepath)
        path.parent.mkdir(parents=True, exist_ok=True)

        with open(path, 'w') as f:
            json.dump(self.to_dict(), f, indent=2)

    @classmethod
    def load(cls, filepath: str) -> 'WorkflowTemplate':
        """
        Load template from JSON file

        Args:
            filepath: Path to template file

        Returns:
            WorkflowTemplate instance
        """
        with open(filepath, 'r') as f:
            data = json.load(f)

        return cls.from_dict(data)


class TemplateLibrary:
    """
    Library of workflow templates

    Manages collection of reusable workflow templates.
    """

    def __init__(self, template_dir: Optional[str] = None):
        """
        Initialize template library

        Args:
            template_dir: Directory for template storage
        """
        self.template_dir = Path(template_dir) if template_dir else Path(".workflow_templates")
        self.template_dir.mkdir(parents=True, exist_ok=True)

        self._templates: Dict[str, WorkflowTemplate] = {}

    def register(self, template: WorkflowTemplate) -> None:
        """
        Register template in library

        Args:
            template: WorkflowTemplate to register
        """
        self._templates[template.name] = template

        # Save to disk
        template.save(str(self.template_dir / f"{template.name}.json"))

    def get(self, name: str) -> Optional[WorkflowTemplate]:
        """
        Get template by name

        Args:
            name: Template name

        Returns:
            WorkflowTemplate if found, None otherwise
        """
        # Check memory first
        if name in self._templates:
            return self._templates[name]

        # Check disk
        template_file = self.template_dir / f"{name}.json"
        if template_file.exists():
            template = WorkflowTemplate.load(str(template_file))
            self._templates[name] = template
            return template

        return None

    def list_templates(self) -> List[str]:
        """
        List all available template names

        Returns:
            List of template names
        """
        # Load from disk
        template_files = self.template_dir.glob("*.json")
        return [f.stem for f in template_files]

    def delete(self, name: str) -> bool:
        """
        Delete template

        Args:
            name: Template name

        Returns:
            True if deleted, False if not found
        """
        # Remove from memory
        self._templates.pop(name, None)

        # Remove from disk
        template_file = self.template_dir / f"{name}.json"
        if template_file.exists():
            template_file.unlink()
            return True

        return False


def create_riser_analysis_template() -> WorkflowTemplate:
    """Create riser analysis workflow template"""
    return WorkflowTemplate(
        name="riser_analysis_template",
        description="Riser analysis for {{riser_type}} riser",
        template_tasks=[
            {
                'name': "Catenary Analysis",
                'task_id': "catenary",
                'module': "catenary_riser.simple_catenary",
                'function': "analyze_simple_catenary",
                'inputs': {
                    'diameter': "{{diameter}}",
                    'thickness': "{{thickness}}",
                    'length': "{{length}}",
                    'water_depth': "{{water_depth}}",
                    'offset': "{{offset}}",
                },
                'outputs': {
                    'top_tension': 'riser_top_tension',
                },
            },
            {
                'name': "VIV Screening",
                'task_id': "viv_screen",
                'module': "viv_analysis.screening",
                'function': "screen_viv_susceptibility",
                'inputs': {
                    'current_speed': "{{current_speed}}",
                    'diameter': "{{diameter}}",
                },
                'outputs': {
                    'viv_status': 'viv_risk_status',
                },
                'depends_on': ['catenary'],
            },
        ],
        parameters={
            'riser_type': 'production',
            'diameter': 0.508,
            'thickness': 0.025,
            'length': 1500,
            'water_depth': 1000,
            'offset': 500,
            'current_speed': 1.0,
        }
    )


def create_mooring_design_template() -> WorkflowTemplate:
    """Create mooring design workflow template"""
    return WorkflowTemplate(
        name="mooring_design_template",
        description="Mooring design for {{vessel_type}} vessel",
        template_tasks=[
            {
                'name': "Environmental Loads",
                'task_id': "env_loads",
                'module': "hydrodynamics.ocimf_loading",
                'function': "calculate_combined_loads",
                'inputs': {
                    'vessel_type': "{{vessel_type}}",
                    'wind_speed': "{{wind_speed}}",
                    'current_speed': "{{current_speed}}",
                },
                'outputs': {
                    'horizontal_load': 'mooring_horizontal_load',
                },
            },
            {
                'name': "Catenary Mooring Analysis",
                'task_id': "catenary_mooring",
                'module': "mooring_analysis.catenary",
                'function': "analyze_catenary_mooring",
                'inputs': {
                    'water_depth': "{{water_depth}}",
                    'line_length': "{{line_length}}",
                    'horizontal_load': '$mooring_horizontal_load',
                },
                'outputs': {
                    'line_tension': 'mooring_line_tension',
                },
                'depends_on': ['env_loads'],
            },
        ],
        parameters={
            'vessel_type': 'fpso',
            'water_depth': 1500,
            'line_length': 2000,
            'wind_speed': 25.0,
            'current_speed': 1.5,
        }
    )
