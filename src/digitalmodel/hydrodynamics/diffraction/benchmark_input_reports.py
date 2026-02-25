"""benchmark_input_reports — backward-compat shim."""
from .benchmark_input_comparison import (  # noqa: F401
    build_input_comparison_html, build_semantic_equivalence_html,
)
from .benchmark_input_files import build_input_files_html  # noqa: F401
from .benchmark_mesh_schematic import build_mesh_schematic_html  # noqa: F401
__all__ = [
    "build_input_comparison_html", "build_semantic_equivalence_html",
    "build_input_files_html", "build_mesh_schematic_html",
]
