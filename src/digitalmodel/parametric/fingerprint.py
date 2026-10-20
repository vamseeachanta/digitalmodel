"""Read a response wrapper and its module-local dependencies without execution."""
import ast
from pathlib import Path


def response_source(basename: str, repo_root: Path) -> str:
    from digitalmodel.parametric.generate import RESPONSE_FUNCS

    source = (repo_root / "src/digitalmodel/parametric/generate.py").read_text(encoding="utf-8")
    tree = ast.parse(source)
    definitions = {}
    for node in tree.body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            definitions[node.name] = node
        elif isinstance(node, (ast.Assign, ast.AnnAssign)):
            targets = node.targets if isinstance(node, ast.Assign) else [node.target]
            for target in targets:
                if isinstance(target, ast.Name):
                    definitions[target.id] = node
    pending = [RESPONSE_FUNCS[basename].__name__]
    included = {}
    while pending:
        name = pending.pop()
        if name in included:
            continue
        node = definitions[name]
        included[name] = ast.get_source_segment(source, node)
        pending.extend(ref.id for ref in ast.walk(node)
                       if isinstance(ref, ast.Name) and isinstance(ref.ctx, ast.Load)
                       and ref.id in definitions and ref.id not in included)
    return "\n".join(included[name] for name in sorted(included))
