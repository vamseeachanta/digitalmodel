from __future__ import annotations

import ast
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
FILES = [
    ROOT / "src/digitalmodel/workflows/openfoam_batch_config.py",
    ROOT / "src/digitalmodel/workflows/openfoam_batch_identity.py",
    ROOT / "src/digitalmodel/workflows/openfoam_batch_layout.py",
    ROOT / "src/digitalmodel/workflows/openfoam_batch_execution.py",
    ROOT / "src/digitalmodel/workflows/openfoam_batch_results.py",
    ROOT / "src/digitalmodel/workflows/openfoam_run_batch.py",
]


def test_touched_modules_stay_bounded() -> None:
    for path in FILES:
        lines = path.read_text().splitlines()
        assert len(lines) <= 400, (path, len(lines))
        tree = ast.parse("\n".join(lines))
        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                end = getattr(node, "end_lineno", node.lineno)
                assert end - node.lineno + 1 <= 50, (path, node.name, end - node.lineno + 1)
