"""Validate NUL-delimited staged spec paths with the audit's input schema.

The shell entry point selects paths. Git index blobs are validated instead of
working files so partially staged commits are checked against committed input.
Generation and licensed solver imports are deliberately outside this hook.
"""

from pathlib import Path
import os
import subprocess
import sys

import yaml
from pydantic import ValidationError
from yaml.nodes import MappingNode, SequenceNode

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


def source_line(source, location):
    """Locate a schema error in YAML; root/missing fields use their parent."""
    node = yaml.compose(source, Loader=yaml.SafeLoader)
    for part in location:
        if isinstance(node, MappingNode):
            child = next((value for key, value in node.value if key.value == part), None)
        elif isinstance(node, SequenceNode) and isinstance(part, int):
            child = node.value[part] if 0 <= part < len(node.value) else None
        else:
            break
        if child is None:
            break
        node = child
    return node.start_mark.line + 1 if node is not None else 1


def validate_staged_spec(path):
    """Return a file:line:reason diagnostic, or None on schema success."""
    blob = subprocess.run(
        ["git", "show", f":0:{path}"], capture_output=True, check=True,
    ).stdout
    try:
        source = blob.decode("utf-8")
        data = yaml.safe_load(source)
        if not isinstance(data, dict):
            return f"{path}:1: expected a YAML mapping"
        # Same constructor and schema as scripts/audit_all_specs.py, stage 2.
        ProjectInputSpec(**data)
    except UnicodeError:
        return f"{path}:1: expected UTF-8 YAML"
    except yaml.YAMLError as error:
        mark = getattr(error, "problem_mark", None)
        line = mark.line + 1 if mark is not None else 1
        return f"{path}:{line}: {getattr(error, 'problem', None) or str(error)}"
    except ValidationError as error:
        first = error.errors(include_input=False)[0]
        location = first["loc"]
        field = ".".join(map(str, location)) or "spec"
        return f"{path}:{source_line(source, location)}: {field}: {first['msg']}"
    except (TypeError, ValueError) as error:
        return f"{path}:1: {error}"
    return None


def main():
    """Fail on the first invalid staged input; report the success count."""
    paths = [os.fsdecode(item) for item in sys.stdin.buffer.read().split(b"\0") if item]
    for path in paths:
        try:
            diagnostic = validate_staged_spec(path)
        except subprocess.CalledProcessError:
            diagnostic = f"{path}:1: cannot read staged Git blob"
        if diagnostic:
            print(diagnostic, file=sys.stderr)
            return 1
    print(f"OK: {len(paths)} specs validated")
    return 0


if __name__ == "__main__":
    sys.exit(main())
