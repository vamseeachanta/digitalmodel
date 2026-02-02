"""
ABOUTME: Level 1 YAML syntax and structure validation
ABOUTME: Validates YAML files, includefile references, and module dependencies
"""

from pathlib import Path
from typing import Set
from .models import Level1Result, ValidationStatus, ValidationIssue, ValidationLevel, Severity
from .utils import load_yaml_file, extract_includefiles, resolve_include_path


class Level1YAMLValidator:
    """
    Level 1 validator for YAML syntax and structure.

    Validates:
    - YAML syntax correctness
    - File existence
    - Includefile reference resolution
    - Section structure
    - Module dependency graph
    """

    def __init__(self):
        """Initialize Level 1 validator."""
        self.issues = []

    def validate(self, file_path: Path) -> Level1Result:
        """
        Run Level 1 YAML validation.

        Args:
            file_path: Path to base YAML file

        Returns:
            Level1Result with validation findings
        """
        file_path = Path(file_path)
        result = Level1Result(
            yaml_valid=False,
            file_exists=file_path.exists(),
            includes_resolved=False
        )

        # Check file existence
        if not result.file_exists:
            result.syntax_errors.append(f"File not found: {file_path}")
            result.status = ValidationStatus.FAIL
            return result

        # Validate YAML syntax
        valid, content, error = load_yaml_file(file_path)
        result.yaml_valid = valid

        if not valid:
            result.syntax_errors.append(error)
            result.status = ValidationStatus.FAIL
            return result

        # Extract sections
        if content and isinstance(content, dict):
            result.sections = list(content.keys())

        # Extract and validate includefiles
        includefiles = extract_includefiles(content)
        missing_includes = []

        for include_file in includefiles:
            include_path = resolve_include_path(file_path, include_file)

            if not include_path.exists():
                missing_includes.append(include_file)
                result.syntax_errors.append(f"Missing include file: {include_file}")

        result.total_modules = len(includefiles)
        result.missing_includes = missing_includes
        result.includes_resolved = len(missing_includes) == 0

        # Determine overall status
        if result.yaml_valid and result.includes_resolved:
            result.status = ValidationStatus.PASS
        elif result.yaml_valid:
            result.status = ValidationStatus.WARN
        else:
            result.status = ValidationStatus.FAIL

        return result

    def validate_module(self, file_path: Path) -> dict:
        """
        Validate an individual module file.

        Args:
            file_path: Path to module YAML file

        Returns:
            Dictionary with validation results
        """
        result = {
            'file': str(file_path),
            'exists': file_path.exists(),
            'valid_syntax': False,
            'size_bytes': 0,
            'line_count': 0,
            'has_content': False,
            'nested_includes': [],
            'status': 'FAIL'
        }

        if not result['exists']:
            return result

        result['size_bytes'] = file_path.stat().st_size

        # Count lines
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                lines = f.readlines()
                result['line_count'] = len(lines)
                result['has_content'] = any(line.strip() for line in lines)
        except Exception as e:
            result['error'] = f"Could not read file: {str(e)}"
            return result

        # Validate YAML syntax
        valid, content, error = load_yaml_file(file_path)
        result['valid_syntax'] = valid

        if not valid:
            result['error'] = error
            return result

        # Check for nested includefiles
        if content:
            result['nested_includes'] = extract_includefiles(content)

        # Determine status
        if valid and result['has_content']:
            result['status'] = 'PASS'
        elif valid and not result['has_content']:
            result['status'] = 'WARN'

        return result

    def build_dependency_graph(self, base_file: Path) -> dict:
        """
        Build dependency graph of module includes.

        Args:
            base_file: Path to base YAML file

        Returns:
            Dictionary mapping files to their includes
        """
        graph = {}
        visited = set()

        def traverse(file_path: Path, depth: int = 0):
            """Recursively traverse include dependencies."""
            if depth > 10:  # Prevent infinite recursion
                return

            file_key = str(file_path)
            if file_key in visited:
                return

            visited.add(file_key)

            valid, content, _ = load_yaml_file(file_path)
            if not valid or not content:
                return

            includes = extract_includefiles(content)
            graph[file_key] = includes

            # Recursively check included files
            for include_file in includes:
                include_path = resolve_include_path(file_path, include_file)
                if include_path.exists():
                    traverse(include_path, depth + 1)

        traverse(base_file)
        return graph

    def detect_circular_dependencies(self, dependency_graph: dict) -> list:
        """
        Detect circular dependencies in module includes.

        Args:
            dependency_graph: Dependency graph from build_dependency_graph

        Returns:
            List of circular dependency chains
        """
        circular = []
        visited = set()
        path = []

        def dfs(node: str):
            """Depth-first search for cycles."""
            if node in path:
                # Found cycle
                cycle_start = path.index(node)
                circular.append(path[cycle_start:] + [node])
                return

            if node in visited:
                return

            visited.add(node)
            path.append(node)

            for neighbor in dependency_graph.get(node, []):
                dfs(neighbor)

            path.pop()

        for node in dependency_graph:
            if node not in visited:
                dfs(node)

        return circular
