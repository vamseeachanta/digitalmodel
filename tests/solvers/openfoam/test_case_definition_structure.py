# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Function-length checks for the case-definition modules.

There is no fixed line limit; a reviewer may comment on a module that is hard to read.
The 50-line function limit is unchanged.
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

MAX_FUNCTION_LINES = 50


def test_touched_modules_stay_bounded() -> None:
    for path in FILES:
        lines = path.read_text().splitlines()
        tree = ast.parse("\n".join(lines))
        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                end = getattr(node, "end_lineno", node.lineno)
                span = end - node.lineno + 1
                assert span <= MAX_FUNCTION_LINES, (path.name, node.name, span)
