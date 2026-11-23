#!/usr/bin/env python3
"""
ABOUTME: Tests for DesignTable FEM mode — batch FEM parameter variations with
plate-with-hole stress concentration factor comparison.
"""

import shutil

import pytest

from digitalmodel.solvers.calculix.fem_chain import FEMChain, is_calculix_available
from digitalmodel.visualization.design_tools.design_table import DesignTable

try:
    import gmsh  # noqa: F401
    GMSH_AVAILABLE = True
except (ImportError, OSError):
    GMSH_AVAILABLE = False

REQUIRES_FEM = pytest.mark.skipif(
    not (GMSH_AVAILABLE and is_calculix_available()),
    reason="Requires gmsh and CalculiX (ccx)",
)


class TestDesignTableFEMMode:
    """Tests for FEM parameter variation via DesignTable."""

    def test_fem_parameter_validation(self):
        """FEM parameters must be recognized."""
        dt = DesignTable.for_fem()
        dt.add_fem_parameter("hole_radius", [8.0, 10.0, 12.0])
        assert "hole_radius" in dt._fem_parameters

    def test_unknown_fem_parameter_raises(self):
        """Unrecognized FEM parameters are rejected."""
        dt = DesignTable.for_fem()
        with pytest.raises(ValueError, match="Unknown FEM parameter"):
            dt.add_fem_parameter("bogus_param", [1.0])

    def test_negative_fem_values_rejected(self):
        """FEM parameter values must be positive."""
        dt = DesignTable.for_fem()
        with pytest.raises(ValueError, match="positive"):
            dt.add_fem_parameter("hole_radius", [-5.0])

    @REQUIRES_FEM
    def test_batch_fem_runs(self, tmp_path):
        """3+ variations produce results with Kt values."""
        dt = DesignTable.for_fem()
        dt.add_fem_parameter(
            "hole_radius", [8.0, 10.0, 15.0]
        )
        results = dt.run_batch_fem(work_dir=tmp_path)
        assert len(results) >= 3
        for r in results:
            assert "kt" in r
            assert r["kt"] > 0

    @REQUIRES_FEM
    def test_kt_monotonic_with_hole_ratio(self, tmp_path):
        """AC: Kt increases monotonically with d/W ratio.

        For a plate of fixed width, increasing hole radius increases
        the stress concentration factor.
        """
        dt = DesignTable.for_fem()
        # Small to large hole in fixed 200mm plate (d/W from 0.08 to 0.15)
        dt.add_fem_parameter("hole_radius", [8.0, 10.0, 15.0])
        results = dt.run_batch_fem(work_dir=tmp_path)
        kts = [r["kt"] for r in results]
        for i in range(len(kts) - 1):
            assert kts[i] < kts[i + 1], (
                f"Kt not monotonic: Kt[{i}]={kts[i]:.3f} >= "
                f"Kt[{i+1}]={kts[i+1]:.3f}"
            )

    @REQUIRES_FEM
    def test_export_results_yaml(self, tmp_path):
        """Batch FEM results export to YAML."""
        dt = DesignTable.for_fem()
        dt.add_fem_parameter("hole_radius", [8.0, 10.0])
        dt.run_batch_fem(work_dir=tmp_path)
        out = dt.export_fem_results_yaml(tmp_path / "fem_results.yaml")
        assert out.exists()
        import yaml
        with open(out) as f:
            data = yaml.safe_load(f)
        assert "variations" in data
        assert len(data["variations"]) == 2
