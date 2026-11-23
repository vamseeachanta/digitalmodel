"""Offline readers must not load commercial interfaces or the global engine."""
import ast
from pathlib import Path


def test_offline_modules_have_no_eager_solver_imports():
    root = Path(__file__).resolve().parents[3] / "src/digitalmodel/hydrodynamics/aqwa"
    for name in ("aqwa_utilities.py", "aqwa_analysis_raos.py"):
        tree = ast.parse((root / name).read_text(encoding="utf-8"))
        for node in tree.body:
            if isinstance(node, (ast.FunctionDef, ast.ClassDef)):
                continue
            for child in ast.walk(node):
                if isinstance(child, ast.Import):
                    assert all(alias.name != "OrcFxAPI" for alias in child.names)
                if isinstance(child, ast.ImportFrom):
                    assert child.module != "digitalmodel.engine"
