"""Import regression coverage for the wave energy converter example."""

import runpy
import sys
from dataclasses import is_dataclass
from pathlib import Path
from typing import Dict, get_type_hints


def test_wave_energy_converter_imports_and_resolves_annotations(monkeypatch):
    """Expect the declared device and Dict return type, per issue #2619.

    Load the real dependencies without executing main or evaluating the
    illustrative engineering equations. No numerical reference is required.
    """
    example = (
        Path(__file__).resolve().parent.parent.parent
        / "examples/domains/workflows/07_wave_energy_converter.py"
    )
    # The example prepends a source path; keep that side effect local to this test.
    monkeypatch.setattr(sys, "path", sys.path.copy())
    namespace = runpy.run_path(str(example))

    assert is_dataclass(namespace["WECDevice"])
    assert callable(namespace["analyze_wec_mooring"])
    assert get_type_hints(namespace["analyze_wec_mooring"])["return"] == Dict
