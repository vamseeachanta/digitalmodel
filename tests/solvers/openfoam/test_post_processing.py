#!/usr/bin/env python3
"""
ABOUTME: Tests for OpenFOAM post-processing pipeline covering force file parsing,
free surface extraction, and probe point reading.
"""

import pytest
import tempfile
import textwrap
from pathlib import Path

from digitalmodel.solvers.openfoam.post_processing import OpenFOAMPostProcessor
from digitalmodel.solvers.openfoam.results_models import (
    ForceTimeSeries,
    FreeSurfaceElevation,
    ProbeTimeSeries,
)


# ============================================================================
# Sample data fixtures
# ============================================================================


SAMPLE_FORCE_DATA = textwrap.dedent("""\
# Force data for OpenFOAM forces function object
# Time      Fp(x)   Fp(y)   Fp(z)   Fv(x)   Fv(y)   Fv(z)
0.0         100.0   0.0     -500.0  10.0    0.0     -50.0
0.5         110.0   0.0     -510.0  11.0    0.0     -51.0
1.0         105.0   0.0     -505.0  10.5    0.0     -50.5
""")

SAMPLE_MOMENT_DATA = textwrap.dedent("""\
# Moment data for OpenFOAM forces function object
# Time      Mp(x)   Mp(y)   Mp(z)   Mv(x)   Mv(y)   Mv(z)
0.0         0.0     1000.0  0.0     0.0     100.0   0.0
0.5         0.0     1100.0  0.0     0.0     110.0   0.0
1.0         0.0     1050.0  0.0     0.0     105.0   0.0
""")


# ============================================================================
# ForceTimeSeries model tests
# ============================================================================


class TestForceTimeSeries:
    """Test ForceTimeSeries result dataclass."""

    def test_force_time_series_creation(self):
        """ForceTimeSeries can be created with arrays."""
        import numpy as np
        fts = ForceTimeSeries(
            times=np.array([0.0, 0.5, 1.0]),
            fx=np.array([100.0, 110.0, 105.0]),
            fy=np.array([0.0, 0.0, 0.0]),
            fz=np.array([-500.0, -510.0, -505.0]),
        )
        assert len(fts.times) == 3

    def test_total_force_combines_pressure_and_viscous(self):
        """Total force sums pressure and viscous contributions."""
        import numpy as np
        fts = ForceTimeSeries(
            times=np.array([0.0, 1.0]),
            fx=np.array([100.0, 105.0]),
            fy=np.array([0.0, 0.0]),
            fz=np.array([-500.0, -505.0]),
            fx_viscous=np.array([10.0, 10.5]),
            fz_viscous=np.array([-50.0, -50.5]),
        )
        total_fx = fts.total_fx
        assert total_fx[0] == pytest.approx(110.0)
        assert total_fx[1] == pytest.approx(115.5)

    def test_peak_force(self):
        """ForceTimeSeries computes peak (max abs) force."""
        import numpy as np
        fts = ForceTimeSeries(
            times=np.array([0.0, 0.5, 1.0]),
            fx=np.array([100.0, 150.0, 120.0]),
            fy=np.array([0.0, 0.0, 0.0]),
            fz=np.array([0.0, 0.0, 0.0]),
        )
        assert fts.peak_fx == pytest.approx(150.0)

    def test_mean_force(self):
        """ForceTimeSeries computes mean force."""
        import numpy as np
        fts = ForceTimeSeries(
            times=np.array([0.0, 1.0, 2.0]),
            fx=np.array([100.0, 200.0, 300.0]),
            fy=np.array([0.0, 0.0, 0.0]),
            fz=np.array([0.0, 0.0, 0.0]),
        )
        assert fts.mean_fx == pytest.approx(200.0)


# ============================================================================
# Force file parsing tests
# ============================================================================


class TestOpenFOAMForceFileParsing:
    """Test parsing of OpenFOAM forces function object output files."""

    def test_parse_force_file_returns_force_time_series(self, tmp_path):
        """Parsing a forces file returns ForceTimeSeries."""
        force_file = tmp_path / "force.dat"
        force_file.write_text(SAMPLE_FORCE_DATA)
        pp = OpenFOAMPostProcessor(case_dir=tmp_path)
        fts = pp.parse_force_file(force_file)
        assert isinstance(fts, ForceTimeSeries)

    def test_parse_force_file_time_column(self, tmp_path):
        """Parsed force time series has correct time values."""
        force_file = tmp_path / "force.dat"
        force_file.write_text(SAMPLE_FORCE_DATA)
        pp = OpenFOAMPostProcessor(case_dir=tmp_path)
        fts = pp.parse_force_file(force_file)
        import numpy as np
        assert fts.times[0] == pytest.approx(0.0)
        assert fts.times[-1] == pytest.approx(1.0)

    def test_parse_force_file_fx_column(self, tmp_path):
        """Parsed force file extracts Fx pressure column correctly."""
        force_file = tmp_path / "force.dat"
        force_file.write_text(SAMPLE_FORCE_DATA)
        pp = OpenFOAMPostProcessor(case_dir=tmp_path)
        fts = pp.parse_force_file(force_file)
        assert fts.fx[0] == pytest.approx(100.0)
        assert fts.fx[1] == pytest.approx(110.0)

    def test_parse_force_file_three_timesteps(self, tmp_path):
        """Parsed force file contains three timestep rows."""
        force_file = tmp_path / "force.dat"
        force_file.write_text(SAMPLE_FORCE_DATA)
        pp = OpenFOAMPostProcessor(case_dir=tmp_path)
        fts = pp.parse_force_file(force_file)
        assert len(fts.times) == 3

    def test_parse_force_file_skips_comment_lines(self, tmp_path):
        """Parser skips lines starting with '#'."""
        force_file = tmp_path / "force_with_comments.dat"
        force_file.write_text(SAMPLE_FORCE_DATA)
        pp = OpenFOAMPostProcessor(case_dir=tmp_path)
        fts = pp.parse_force_file(force_file)
        # 3 data lines — not 5 (2 comment lines ignored)
        assert len(fts.times) == 3


# ============================================================================
# ProbeTimeSeries tests
# ============================================================================


class TestProbeTimeSeries:
    """Test probe point result dataclass."""

    def test_probe_time_series_creation(self):
        """ProbeTimeSeries can be created with probe coordinates and values."""
        import numpy as np
        pts = ProbeTimeSeries(
            times=np.array([0.0, 1.0, 2.0]),
            probe_coords=[[0.0, 0.0, 0.0], [10.0, 0.0, 0.0]],
            values=np.array([[1.0, 1.1], [0.9, 1.0], [1.05, 1.15]]),
            field_name="p",
        )
        assert pts.n_probes == 2
        assert pts.n_timesteps == 3

    def test_probe_time_series_at_probe(self):
        """ProbeTimeSeries returns time series for a specific probe index."""
        import numpy as np
        pts = ProbeTimeSeries(
            times=np.array([0.0, 1.0]),
            probe_coords=[[0.0, 0.0, 0.0]],
            values=np.array([[5.0], [6.0]]),
            field_name="p",
        )
        ts = pts.at_probe(0)
        assert ts[0] == pytest.approx(5.0)
        assert ts[1] == pytest.approx(6.0)


# ============================================================================
# FreeSurfaceElevation tests
# ============================================================================


class TestFreeSurfaceElevation:
    """Test free surface elevation result dataclass."""

    def test_free_surface_elevation_creation(self):
        """FreeSurfaceElevation can be created with position and eta arrays."""
        import numpy as np
        fse = FreeSurfaceElevation(
            times=np.array([0.0, 0.5, 1.0]),
            x_positions=np.array([0.0, 10.0, 20.0]),
            elevations=np.array([
                [0.0, 0.1, 0.0],
                [0.5, 0.4, 0.3],
                [0.0, -0.1, 0.0],
            ]),
        )
        assert fse.n_positions == 3
        assert fse.n_timesteps == 3

    def test_max_elevation(self):
        """FreeSurfaceElevation computes maximum elevation across all times."""
        import numpy as np
        fse = FreeSurfaceElevation(
            times=np.array([0.0, 1.0]),
            x_positions=np.array([0.0]),
            elevations=np.array([[1.5], [2.0]]),
        )
        assert fse.max_elevation == pytest.approx(2.0)
