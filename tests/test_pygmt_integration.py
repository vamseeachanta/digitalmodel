"""PyGMT integration tests for offshore/subsea mapping.

Requires GMT system library to be installed.
Install: sudo apt-get install gmt gmt-gshhg-low
"""

import os
import shutil
import tempfile

import pytest

# Skip entire module if GMT system library is not available
pytestmark = pytest.mark.skipif(
    shutil.which("gmt") is None,
    reason="GMT system library not installed (sudo apt-get install gmt gmt-gshhg-low)",
)


@pytest.mark.integration
def test_pygmt_imports():
    """Verify pygmt can be imported and reports a version string."""
    import pygmt

    assert hasattr(pygmt, "__version__")
    assert isinstance(pygmt.__version__, str)
    assert len(pygmt.__version__) > 0


@pytest.mark.integration
def test_pygmt_create_figure():
    """Verify a Figure object can be created and has expected methods."""
    import pygmt

    fig = pygmt.Figure()
    assert hasattr(fig, "coast")
    assert hasattr(fig, "grdimage")
    assert hasattr(fig, "plot")
    assert hasattr(fig, "savefig")


@pytest.mark.integration
def test_pygmt_load_earth_relief():
    """Load built-in earth relief for a small Gulf of Mexico region.

    Uses 30 arc-minute resolution to minimize download (~1 MB).
    Region: -98/-88/24/31 (western Gulf of Mexico).
    Note: PyGMT downloads data on first use; cached at ~/.gmt/server/.
    """
    import pygmt

    # Small bbox, coarse resolution to keep download minimal
    grid = pygmt.datasets.load_earth_relief(
        resolution="30m",  # 30 arc-minute (~55 km)
        region=[-98, -88, 24, 31],
    )
    assert grid is not None
    assert grid.shape[0] > 0
    assert grid.shape[1] > 0
    # Relief values should include negative (ocean depth)
    assert float(grid.min()) < 0, "Expected negative values for ocean bathymetry"


@pytest.mark.integration
def test_pygmt_save_figure_to_png():
    """Create a simple coastline figure and save to PNG without error."""
    import pygmt

    fig = pygmt.Figure()
    fig.coast(
        region=[-98, -88, 24, 31],
        projection="M15c",
        land="lightgray",
        water="lightblue",
        shorelines=True,
        frame=True,
    )

    with tempfile.TemporaryDirectory() as tmpdir:
        outpath = os.path.join(tmpdir, "test_coast.png")
        fig.savefig(outpath)
        assert os.path.exists(outpath)
        assert os.path.getsize(outpath) > 1000, "PNG file suspiciously small"
