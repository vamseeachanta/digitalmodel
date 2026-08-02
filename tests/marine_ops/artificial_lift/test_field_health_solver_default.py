#!/usr/bin/env python3
"""
ABOUTME: Guards the solver default in the field-health entry point against
ABOUTME: silently falling back to a solver documented as unusable for diagnosis.
"""

import inspect

import pytest

from digitalmodel.marine_ops.artificial_lift import field_health
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow


# The solvers that actually reconstruct a downhole card. 'gibbs' does not
# transform the load at all (dm#1857) and 'finite_difference' diverges; the
# DynacardWorkflow docstring says neither should be used for diagnosis.
DIAGNOSTIC_SOLVERS = {"everitt_jennings"}


def _default_of(func, param):
    return inspect.signature(func).parameters[param].default


class TestFieldHealthSolverDefault:
    """`run_field_troubleshooter` must not fall back to a non-diagnostic solver."""

    def test_fallback_is_a_diagnostic_solver(self):
        """Settings without an explicit solver_method must not land on gibbs.

        The shipped base config sets everitt_jennings explicitly, so this
        fallback is latent rather than live -- which is exactly why it needs a
        test. Any caller building a settings dict by hand (a notebook, a new
        workflow, a test) silently gets the solver the module documents as
        unusable, and nothing in the output says so.
        """
        source = inspect.getsource(field_health.run_field_troubleshooter)
        assert 'settings.get("solver_method", "gibbs")' not in source, (
            "run_field_troubleshooter falls back to 'gibbs', which does not "
            "transform load (dm#1857) and must not be used for diagnosis"
        )

    def test_fallback_matches_the_workflow_default(self):
        """The two defaults must not disagree.

        DynacardWorkflow defaults to everitt_jennings. A caller-side default
        that differs is worse than no default: both values are valid members
        of the Literal, so type checking and tests pass either way, and the
        disagreement is only visible by reading both files.
        """
        workflow_default = _default_of(DynacardWorkflow.__init__, "solver_method")
        assert workflow_default in DIAGNOSTIC_SOLVERS

        source = inspect.getsource(field_health.run_field_troubleshooter)
        assert f'"solver_method", "{workflow_default}"' in source, (
            f"field_health fallback disagrees with DynacardWorkflow's default "
            f"({workflow_default!r})"
        )

    @pytest.mark.parametrize("bad_solver", ["gibbs", "finite_difference"])
    def test_non_diagnostic_solvers_are_not_defaults_anywhere(self, bad_solver):
        """Neither non-diagnostic solver may be a default in this call path."""
        workflow_default = _default_of(DynacardWorkflow.__init__, "solver_method")
        assert workflow_default != bad_solver

        source = inspect.getsource(field_health.run_field_troubleshooter)
        assert f'"solver_method", "{bad_solver}"' not in source
