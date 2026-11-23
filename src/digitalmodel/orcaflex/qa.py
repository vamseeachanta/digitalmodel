"""OrcaFlex QA facade."""
from __future__ import annotations

from importlib.util import module_from_spec, spec_from_file_location
from pathlib import Path
from types import ModuleType


def _load_examples_qa_module() -> ModuleType:
    script = (
        Path(__file__).resolve().parents[3]
        / "docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py"
    )
    if not script.exists():
        raise ImportError(
            f"QA script not found at expected path: {script}. "
            "Run WRK-325 setup to generate example QA module."
        )

    spec = spec_from_file_location("digitalmodel.orcaflex.examples_qa", script)
    if spec is None or spec.loader is None:
        raise ImportError(f"Unable to load QA module spec from: {script}")

    module = module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def run_orcaflex_qa(example_ids: list[str] | None = None):
    """Run OrcaFlex example QA checks.

    This facade keeps the public import stable:
    `from digitalmodel.orcaflex.qa import run_orcaflex_qa`
    while delegating to the docs QA implementation from WRK-325.
    """
    module = _load_examples_qa_module()
    if not hasattr(module, "run_orcaflex_qa"):
        raise ImportError("Loaded QA module does not expose run_orcaflex_qa")
    return module.run_orcaflex_qa(example_ids)
