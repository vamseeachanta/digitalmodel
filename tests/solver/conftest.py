"""Solver test configuration.

Tests in this directory that need the licensed OrcFxAPI carry
``pytest.mark.solver`` in their own module (``pytestmark``), so the marker is
visible to the static scan in ``tests/contracts/test_solver_marker_coverage.py``
and is applied to those modules only.  ``test_diffraction_pipeline_e2e.py`` and
``test_parametric_spec_generator.py`` need no licence and are not marked.

This file used to auto-mark every collected item whose path contained the
substring ``solver``.  ``pytest_collection_modifyitems`` is a session-wide hook,
so it marked every test under ``tests/solvers/`` and any test in a checkout
whose directory name contained ``solver``, not only this directory.
"""
