# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Cold imports and lazy exports for the reusable chart entry points.

The independent oracle is the output-free import contract in issue #2071:
no filesystem mutations, no plotting imports, and unchanged interpreter state.
No physical quantities or engineering acceptance limits are introduced.
"""

import os
from pathlib import Path
import subprocess
import sys
import textwrap

import pytest


VISUALIZATION = (
    Path(__file__).resolve().parents[3]
    / "src/digitalmodel/marine_ops/marine_analysis/visualization"
)

COLD_IMPORT_PROBE = """
import importlib.abc
import importlib.util
import os
from pathlib import Path
import sys
import warnings

# Fail on attempted imports, including optional imports hidden by try/except.
# These are the plotting/data dependencies deferred by ocimf_charts.py.
blocked = {'matplotlib', 'seaborn', 'numpy', 'pandas', 'scipy', 'plotly'}
attempts = []

class NoPlotDependencies(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in blocked:
            attempts.append(fullname)
            raise AssertionError('chart dependency requested during import')

sys.meta_path.insert(0, NoPlotDependencies())

def audit(event, args):
    if event == 'os.mkdir':
        raise AssertionError('import attempted directory creation')
    if event == 'open':
        mode, flags = args[1:3]
        if (mode and any(char in mode for char in 'wax+')) or (
            flags and flags & (os.O_WRONLY | os.O_RDWR | os.O_CREAT)
        ):
            raise AssertionError('import attempted file output')

sys.addaudithook(audit)
paths = list(sys.path)
filters = list(warnings.filters)
contents = list(Path.cwd().iterdir())
folder, entry = Path(sys.argv[1]), sys.argv[2]

# Load the chart boundary in isolation: ancestor packages have separate legacy
# plotting imports, covered by the existing atlas import regression.
name = 'chart_contract_probe'
file = folder / ('__init__.py' if entry == 'package' else 'ocimf_charts.py')
options = {'submodule_search_locations': [str(folder)]} if entry == 'package' else {}
spec = importlib.util.spec_from_file_location(name, file, **options)
module = importlib.util.module_from_spec(spec)
sys.modules[name] = module
spec.loader.exec_module(module)

if entry == 'package':
    # Public names follow visualization/__init__.py; accessing one must not
    # import its sibling or restore the historical eager wildcard exports.
    assert module.__all__ == ['integration_charts', 'ocimf_charts']
    assert name + '.integration_charts' not in sys.modules
    assert name + '.ocimf_charts' not in sys.modules
    try:
        getattr(module, 'missing_chart')
    except AttributeError:
        pass
    else:
        raise AssertionError('unknown attribute did not raise AttributeError')
    assert name + '.integration_charts' not in sys.modules
    assert name + '.ocimf_charts' not in sys.modules
    chart = module.ocimf_charts
    assert callable(chart.main)
    assert chart is module.ocimf_charts
    assert chart is sys.modules[name + '.ocimf_charts']
    assert name + '.integration_charts' not in sys.modules
else:
    assert callable(module.main)

assert not attempts, 'import attempted a plotting/data dependency'
assert not (blocked & {key.split('.')[0] for key in sys.modules})
assert sys.path == paths, 'import changed the module search path'
assert warnings.filters == filters, 'import changed warning filters'
assert list(Path.cwd().iterdir()) == contents, 'import changed working directory'
"""


@pytest.mark.parametrize("entry", ["module", "package"])
def test_cold_chart_import_defers_dependencies_and_output(tmp_path, entry):
    """An isolated import has no writes even before plotting caches exist.

    Expected empty output follows from issue #2071, acceptance criterion for
    subprocess imports. The package case also checks its existing lazy API.
    """
    work = tmp_path / "work"
    work.mkdir()
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE="1")
    env["MPLCONFIGDIR"] = str(tmp_path / "unused-plot-cache")
    result = subprocess.run(
        [
            sys.executable,
            "-c",
            textwrap.dedent(COLD_IMPORT_PROBE),
            str(VISUALIZATION),
            entry,
        ],
        cwd=work,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert list(tmp_path.iterdir()) == [work]


@pytest.mark.parametrize("fail", [False, True], ids=["success", "failure"])
def test_chart_call_restores_caller_plot_state(tmp_path, monkeypatch, fail):
    """Chart style is local even when chart creation raises.

    The oracle is the caller's settings before the call, not a chart result.
    Synthetic rows are only rendering input: headings are the endpoints and
    midpoint of a half turn; every coefficient is a dimensionless zero. No
    physical response or acceptance limit is inferred from those values.
    """
    import importlib.util
    import warnings

    import matplotlib.pyplot as plt
    from cycler import cycler
    import pandas as pd
    import seaborn as sns

    spec = importlib.util.spec_from_file_location(
        "style_contract_probe", VISUALIZATION / "ocimf_charts.py"
    )
    charts = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(charts)
    frame = pd.DataFrame(
        {
            "heading": [0, 90, 180],
            "displacement": [1, 1, 1],
            **{name: [0, 0, 0] for name in ("CXw", "CYw", "CMw", "CXc", "CYc", "CMc")},
        }
    )
    real_subplots = plt.subplots
    calls = []

    class ChartFailure(Exception):
        pass

    def observed_subplots(*args, **kwargs):
        calls.append(True)
        style = plt.style.library["seaborn-v0_8-darkgrid"]
        assert plt.rcParams["axes.facecolor"] == style["axes.facecolor"]
        assert sns.color_palette() == sns.color_palette("husl")
        if fail:
            raise ChartFailure("synthetic plotting failure")
        return real_subplots(*args, **kwargs)

    monkeypatch.setattr(plt, "subplots", observed_subplots)
    with (
        plt.rc_context(
            {
                "axes.facecolor": "yellow",
                "axes.prop_cycle": cycler(color=["cyan", "magenta"]),
            }
        ),
        warnings.catch_warnings(),
    ):
        warnings.simplefilter("error", RuntimeWarning)
        before = dict(plt.rcParams)
        filters = list(warnings.filters)
        figures = plt.get_fignums()
        kwargs = dict(filename="chart.png", output_dir=tmp_path / "charts")
        if fail:
            with pytest.raises(ChartFailure, match="synthetic plotting failure"):
                charts.plot_heading_sensitivity(frame, **kwargs)
            assert not (tmp_path / "charts").exists()
        else:
            charts.plot_heading_sensitivity(frame, **kwargs)
            assert (tmp_path / "charts/chart.png").is_file()
        assert calls == [True]
        assert dict(plt.rcParams) == before
        assert warnings.filters == filters
        assert plt.get_fignums() == figures
