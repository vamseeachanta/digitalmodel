"""OCIMF import purity and caller-directed chart output regressions."""

import os
from pathlib import Path
import subprocess
import sys
import textwrap

import pytest

MODULE = "digitalmodel.marine_ops.marine_analysis.visualization.ocimf_charts"

IMPORT_PROBE = """
import importlib
import os
from pathlib import Path
import re
import sys
from unittest.mock import patch

# Initialize third-party plotting caches before observing application imports.
import matplotlib.pyplot as plt
import numpy
import pandas
import seaborn as sns
from scipy.interpolate import griddata
from mpl_toolkits.mplot3d import Axes3D

before = sorted(str(p.relative_to(Path.cwd())) for p in Path.cwd().rglob('*'))
settings = dict(plt.rcParams)
figures = plt.get_fignums()

def audit(event, args):
    if event == 'os.mkdir':
        raise AssertionError('import attempted directory creation')
    if event == 'open':
        mode, flags = args[1:3]
        if (mode and any(c in mode for c in 'wax+')) or (
            flags and flags & (os.O_WRONLY | os.O_RDWR | os.O_CREAT)
        ):
            raise AssertionError('import attempted file output')

original_stat = os.stat

def stat(path, *args, **kwargs):
    if isinstance(path, (str, bytes, os.PathLike)):
        value = os.fsdecode(path)
        assert not re.match(r'^[A-Za-z]:[/\\\\]$', value), 'fixed drive root probed'
    return original_stat(path, *args, **kwargs)

sys.addaudithook(audit)
with patch('os.stat', stat):
    if sys.argv[1] == 'direct':
        spec = importlib.util.spec_from_file_location('chart_import_probe', sys.argv[2])
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
    elif sys.argv[1] == 'charts':
        importlib.import_module(sys.argv[2])
    else:
        from digitalmodel.parametric.generate import RESPONSE_FUNCS
        # Import the workflow reached by the response without executing it.
        importlib.import_module(
            'digitalmodel.marine_ops.marine_engineering.mooring_analysis.fpso_full_workflow'
        )
if sys.argv[1] == 'direct':
    assert dict(plt.rcParams) == settings, 'import changed plotting configuration'
assert plt.get_fignums() == figures, 'import started chart work'
after = sorted(str(p.relative_to(Path.cwd())) for p in Path.cwd().rglob('*'))
assert after == before, 'import changed temporary directory contents'
"""


@pytest.mark.parametrize("entry", ["direct", "charts", "atlas"])
def test_import_performs_no_output_or_plot_setup(tmp_path, entry):
    """The atlas entry exercises the lazy FPSO import from the existing pin."""
    work = tmp_path / "work"
    work.mkdir()
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE="1", MPLBACKEND="Agg")
    env["MPLCONFIGDIR"] = str(tmp_path / "plot-cache")
    source = Path(__file__).resolve().parents[3] / "src"
    env["PYTHONPATH"] = os.pathsep.join([str(source), env.get("PYTHONPATH", "")])
    target = str(source / (MODULE.replace(".", "/") + ".py")) if entry == "direct" else MODULE
    before = list(work.iterdir())
    result = subprocess.run(
        [sys.executable, "-c", textwrap.dedent(IMPORT_PROBE), entry, target],
        cwd=work, env=env, capture_output=True, text=True, timeout=120,
    )
    assert result.returncode == 0, result.stderr
    assert list(work.iterdir()) == before


@pytest.fixture
def coefficient_frame():
    """Synthetic rectangular data tests file routing, not engineering inputs."""
    import pandas as pd

    coefficients = ["CXw", "CYw", "CMw", "CXc", "CYc", "CMc"]
    rows = []
    for disp_index in range(3):
        for heading_index in range(3):
            rows.append({
                "heading": heading_index * 90,
                "displacement": (disp_index + 1) * 100000,
                "vessel_type": "synthetic",
                "displacement_ratio": 1,
                **{name: (index + 1) * (heading_index - 1) / 10 + disp_index / 100
                   for index, name in enumerate(coefficients)},
            })
    return pd.DataFrame(rows)


def test_explicit_generation_uses_caller_paths(tmp_path, coefficient_frame):
    from digitalmodel.marine_ops.marine_analysis.visualization import ocimf_charts

    csv_file = tmp_path / "coefficients.csv"
    coefficient_frame.to_csv(csv_file, index=False)
    output = tmp_path / "charts"
    ocimf_charts.main(csv_file=csv_file, output_dir=output)
    names = {
        "3d_cxw.png", "3d_cyw.png", "3d_cmw.png", "3d_cxc.png", "3d_cyc.png",
        "3d_cmc.png", "polar_diagrams.png", "vector_field.png", "heatmaps.png",
        "heading_sensitivity.png", "ocimf_extraction_report.html",
    }
    assert {p.name for p in output.iterdir()} == names
    for name in names - {"ocimf_extraction_report.html"}:
        assert (output / name).read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
    report = (output / "ocimf_extraction_report.html").read_text()
    assert "Total Entries:</strong> 9" in report
    assert all(f'src="{name}"' in report for name in names if name.endswith('.png'))
    assert {p.name for p in tmp_path.iterdir()} == {"coefficients.csv", "charts"}


@pytest.mark.parametrize("name,kwargs", [
    ("plot_3d_surface", {"coefficient": "CXw"}),
    ("plot_polar_diagrams", {}),
    ("plot_vector_field", {"displacement": 100000}),
    ("plot_heatmaps", {}),
    ("plot_heading_sensitivity", {}),
    ("create_html_report", {}),
])
def test_standalone_chart_uses_caller_output(tmp_path, coefficient_frame, name, kwargs):
    from digitalmodel.marine_ops.marine_analysis.visualization import ocimf_charts

    output = tmp_path / "nested" / "charts"
    if name != "create_html_report":
        kwargs = dict(kwargs, filename="chart.png")
    getattr(ocimf_charts, name)(coefficient_frame, output_dir=output, **kwargs)
    expected = "ocimf_extraction_report.html" if name == "create_html_report" else "chart.png"
    assert {p.name for p in output.iterdir()} == {expected}
    assert (output / expected).stat().st_size > 0
