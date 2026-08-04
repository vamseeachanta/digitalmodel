"""Size limits for the modules #1575 touches.

Mirrors tests/workflows/test_openfoam_batch_structure.py, the convention #1565
established, so the same literal 400/50 limits apply on both sides of the
batch -> workflow -> case boundary.
"""

from __future__ import annotations

import ast
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
FILES = [
    ROOT / "src/digitalmodel/solvers/openfoam/case_definition.py",
    ROOT / "src/digitalmodel/solvers/openfoam/_case_definition_validation.py",
    ROOT / "src/digitalmodel/solvers/openfoam/block_mesh.py",
    ROOT / "src/digitalmodel/solvers/openfoam/case_builder.py",
    ROOT / "src/digitalmodel/solvers/openfoam/workflow.py",
]

MAX_FILE_LINES = 400
MAX_FUNCTION_LINES = 50


def test_touched_modules_stay_bounded() -> None:
    for path in FILES:
        lines = path.read_text().splitlines()
        assert len(lines) <= MAX_FILE_LINES, (path.name, len(lines))
        tree = ast.parse("\n".join(lines))
        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                end = getattr(node, "end_lineno", node.lineno)
                span = end - node.lineno + 1
                assert span <= MAX_FUNCTION_LINES, (path.name, node.name, span)
