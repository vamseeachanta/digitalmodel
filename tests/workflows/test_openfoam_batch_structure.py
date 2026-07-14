"""Structural contract for the OpenFOAM batch module decomposition."""

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
