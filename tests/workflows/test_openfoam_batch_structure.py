"""Structural contract for the OpenFOAM batch module decomposition."""

import ast
import inspect
from pathlib import Path

from digitalmodel.workflows import openfoam_run_batch as facade


def test_config_surface_is_extracted_and_reexported():
    """Legacy facade imports will remain stable after config extraction."""
    from digitalmodel.workflows import openfoam_batch_config as config

    assert facade.default_workers is config.default_workers
    assert facade.resolve_workers is config.resolve_workers
    assert facade._resolve_case_matrix is config.resolve_case_matrix
    assert facade._render_cases is config.render_cases
    assert facade._resolve_dir is config.resolve_dir
    assert facade._resolve_path is config.resolve_path


def test_execution_layout_and_result_surfaces_are_reexported():
    from digitalmodel.workflows import openfoam_batch_execution as execution
    from digitalmodel.workflows import openfoam_batch_layout as layout
    from digitalmodel.workflows import openfoam_batch_results as results

    assert facade._run_case_mpi is execution.run_case_mpi
    assert facade._solver_ready is execution.solver_ready
    assert facade.mpi_command_plan is execution.mpi_command_plan
    assert facade._clean_case_dir is layout.clean_case_dir
    assert facade._load_checkpoint is results.load_checkpoint
    assert facade._write_summary is results.write_summary


def test_split_modules_obey_file_and_function_limits():
    modules = (
        facade,
        __import__(
            "digitalmodel.workflows.openfoam_batch_config", fromlist=["*"]
        ),
        __import__(
            "digitalmodel.workflows.openfoam_batch_execution", fromlist=["*"]
        ),
        __import__(
            "digitalmodel.workflows.openfoam_batch_layout", fromlist=["*"]
        ),
        __import__(
            "digitalmodel.workflows.openfoam_batch_results", fromlist=["*"]
        ),
    )
    for module in modules:
        path = Path(inspect.getsourcefile(module))
        source = path.read_text()
        assert len(source.splitlines()) <= 400, path
        for node in ast.walk(ast.parse(source)):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                assert node.end_lineno - node.lineno + 1 <= 50, (
                    path,
                    node.name,
                )
