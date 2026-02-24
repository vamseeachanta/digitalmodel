#!/usr/bin/env python3
"""
Variable substitution engine for OrcaWave templates.
Supports nested variables, conditional sections, and functions.
"""

import yaml
import re
from pathlib import Path
from typing import Dict, Any, List, Union
import logging
from copy import deepcopy

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class SubstitutionEngine:
    """Handles variable substitution in OrcaWave templates."""
    
    def __init__(self):
        """Initialize substitution engine."""
        self.variables = {}
        self.functions = {
            'upper': lambda x: str(x).upper(),
            'lower': lambda x: str(x).lower(),
            'round': lambda x, n=2: round(float(x), int(n)),
            'format': lambda x, fmt: fmt.format(x),
            'default': lambda x, default: x if x else default,
        }
        # Pattern for variables: {{variable}} or {{object.property}}
        self.var_pattern = re.compile(r'\{\{([^}]+)\}\}')
        # Pattern for conditionals: {% if condition %}...{% endif %}
        self.cond_pattern = re.compile(r'\{%\s*if\s+([^%]+)\s*%\}(.*?)\{%\s*endif\s*%\}', re.DOTALL)
        
    def set_variables(self, variables: Dict[str, Any]):
        """Set variables for substitution.
        
        Args:
            variables: Dictionary of variable values
        """
        self.variables = variables
        logger.info(f"Set {len(variables)} variables for substitution")
        
    def add_variable(self, name: str, value: Any):
        """Add a single variable.
        
        Args:
            name: Variable name
            value: Variable value
        """
        self.variables[name] = value
        
    def resolve_variable(self, var_path: str) -> Any:
        """Resolve a variable path (supports nested properties).
        
        Args:
            var_path: Variable path like 'vessel.mass' or 'config[0].name'
            
        Returns:
            Resolved value or None if not found
        """
        # Check for function call
        if '(' in var_path:
            return self.resolve_function(var_path)
            
        # Split path into components
        parts = re.split(r'[.\[\]]', var_path)
        parts = [p for p in parts if p]  # Remove empty parts
        
        # Start with root variables
        current = self.variables
        
        for part in parts:
            if current is None:
                return None
                
            # Handle dictionary access
            if isinstance(current, dict):
                current = current.get(part)
            # Handle list access
            elif isinstance(current, list):
                try:
                    index = int(part)
                    current = current[index] if index < len(current) else None
                except (ValueError, IndexError):
                    return None
            else:
                # Try attribute access
                current = getattr(current, part, None)
                
        return current
        
    def resolve_function(self, func_call: str) -> Any:
        """Resolve a function call.
        
        Args:
            func_call: Function call like 'upper(vessel.name)'
            
        Returns:
            Function result or None
        """
        match = re.match(r'(\w+)\((.*)\)', func_call)
        if not match:
            return None
            
        func_name = match.group(1)
        args_str = match.group(2)
        
        if func_name not in self.functions:
            logger.warning(f"Unknown function: {func_name}")
            return None
            
        # Parse arguments
        args = []
        for arg in args_str.split(','):
            arg = arg.strip()
            # Try to resolve as variable first
            resolved = self.resolve_variable(arg)
            if resolved is not None:
                args.append(resolved)
            else:
                # Treat as literal
                args.append(arg.strip('"\''))
                
        # Call function
        try:
            return self.functions[func_name](*args)
        except Exception as e:
            logger.warning(f"Error calling function {func_name}: {e}")
            return None
            
    def evaluate_condition(self, condition: str) -> bool:
        """Evaluate a conditional expression.
        
        Args:
            condition: Condition string like 'vessel.type == "tanker"'
            
        Returns:
            Boolean result
        """
        # Replace variables in condition
        def replace_var(match):
            var_name = match.group(1)
            value = self.resolve_variable(var_name)
            if isinstance(value, str):
                return f'"{value}"'
            return str(value) if value is not None else 'None'
            
        condition = re.sub(r'\b(\w+(?:\.\w+)*)\b', replace_var, condition)
        
        # Safely evaluate condition
        try:
            return eval(condition, {"__builtins__": {}}, {})
        except Exception as e:
            logger.warning(f"Error evaluating condition '{condition}': {e}")
            return False
            
    def substitute_string(self, text: str) -> str:
        """Substitute variables in a string.
        
        Args:
            text: String with variable placeholders
            
        Returns:
            String with substituted values
        """
        if not isinstance(text, str):
            return text
            
        # Handle conditionals first
        def process_conditional(match):
            condition = match.group(1)
            content = match.group(2)
            if self.evaluate_condition(condition):
                return self.substitute_string(content)
            return ''
            
        text = self.cond_pattern.sub(process_conditional, text)
        
        # Handle variable substitutions
        def replace_var(match):
            var_path = match.group(1).strip()
            value = self.resolve_variable(var_path)
            return str(value) if value is not None else match.group(0)
            
        return self.var_pattern.sub(replace_var, text)
        
    def substitute_value(self, value: Any) -> Any:
        """Substitute variables in any value type.
        
        Args:
            value: Value to process (string, dict, list, etc.)
            
        Returns:
            Processed value with substitutions
        """
        if isinstance(value, str):
            return self.substitute_string(value)
        elif isinstance(value, dict):
            return {k: self.substitute_value(v) for k, v in value.items()}
        elif isinstance(value, list):
            return [self.substitute_value(item) for item in value]
        else:
            return value
            
    def substitute_template(self, template: Dict[str, Any]) -> Dict[str, Any]:
        """Substitute all variables in a template.
        
        Args:
            template: Template dictionary
            
        Returns:
            New dictionary with substituted values
        """
        # Deep copy to avoid modifying original
        result = deepcopy(template)
        
        # Recursively substitute
        result = self.substitute_value(result)
        
        logger.info("Template substitution completed")
        return result
        
    def validate_substitution(self, template: Dict[str, Any]) -> List[str]:
        """Check for unresolved variables in template.
        
        Args:
            template: Template after substitution
            
        Returns:
            List of unresolved variable references
        """
        unresolved = []
        
        def check_value(value: Any, path: str = ""):
            if isinstance(value, str):
                matches = self.var_pattern.findall(value)
                for match in matches:
                    unresolved.append(f"{path}: {{{{ {match} }}}}")
            elif isinstance(value, dict):
                for k, v in value.items():
                    check_value(v, f"{path}.{k}" if path else k)
            elif isinstance(value, list):
                for i, item in enumerate(value):
                    check_value(item, f"{path}[{i}]")
                    
        check_value(template)
        return unresolved


def main():
    """Test substitution engine functionality."""
    engine = SubstitutionEngine()
    
    # Set test variables
    test_vars = {
        'vessel': {
            'name': 'SeaCypress',
            'mass': 300912.45,
            'cog': [11.2911, -0.0005, 2.4331],
            'draft': 1.981,
        },
        'analysis': {
            'water_depth': 500,
            'wave_periods': [4, 5, 6, 7, 8, 9, 10],
        },
        'mesh_file': 'Sea_Cypress.msh',
    }
    
    engine.set_variables(test_vars)
    
    # Test string substitution
    test_string = "Vessel {{vessel.name}} with mass {{vessel.mass}} kg"
    result = engine.substitute_string(test_string)
    print(f"String substitution: {result}")
    
    # Test nested substitution
    test_dict = {
        'name': '{{vessel.name}}',
        'properties': {
            'mass': '{{vessel.mass}}',
            'draft': '{{vessel.draft}}',
        },
        'cog_x': '{{vessel.cog[0]}}',
    }
    
    result = engine.substitute_value(test_dict)
    print(f"\nNested substitution: {yaml.dump(result, default_flow_style=False)}")
    
    # Test conditional
    test_cond = "{% if vessel.mass > 100000 %}Large vessel{% endif %}"
    result = engine.substitute_string(test_cond)
    print(f"Conditional result: {result}")
    
    # Validate for unresolved variables
    test_template = {
        'resolved': '{{vessel.name}}',
        'unresolved': '{{undefined.variable}}',
    }
    result = engine.substitute_value(test_template)
    unresolved = engine.validate_substitution(result)
    if unresolved:
        print(f"\nUnresolved variables found:")
        for item in unresolved:
            print(f"  - {item}")


if __name__ == "__main__":
    main()