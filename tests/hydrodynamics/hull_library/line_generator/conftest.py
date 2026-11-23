"""
ABOUTME: Shared fixtures for line_generator tests — standard hull forms used
across all phases of testing: line parser, surface interpolation, panelisation,
and output.
"""

from __future__ import annotations

import json
import textwrap
from pathlib import Path

import pytest


# ---------------------------------------------------------------------------
# CSV / JSON / YAML payload helpers
# ---------------------------------------------------------------------------

BOX_BARGE_CSV_STATIONS = """\
x,z,y
0.0,0.0,10.0
0.0,5.0,10.0
50.0,0.0,10.0
50.0,5.0,10.0
100.0,0.0,10.0
100.0,5.0,10.0
"""

BOX_BARGE_JSON_DEFINITION = {
    "name": "box_barge",
    "hull_type": "barge",
    "length_bp": 100.0,
    "beam": 20.0,
    "draft": 5.0,
    "depth": 6.0,
    "source": "unit_test",
    "stations": [
        {"x": 0.0, "offsets": [[0.0, 10.0], [5.0, 10.0]]},
        {"x": 50.0, "offsets": [[0.0, 10.0], [5.0, 10.0]]},
        {"x": 100.0, "offsets": [[0.0, 10.0], [5.0, 10.0]]},
    ],
    "waterlines": [],
    "profile": [],
}

SHIP_SHAPED_JSON_DEFINITION = {
    "name": "ship_shaped",
    "hull_type": "ship",
    "length_bp": 100.0,
    "beam": 20.0,
    "draft": 8.0,
    "depth": 10.0,
    "source": "unit_test",
    "stations": [
        {"x": 0.0, "offsets": [[0.0, 0.0], [4.0, 3.0], [8.0, 5.0]]},
        {"x": 25.0, "offsets": [[0.0, 0.0], [4.0, 7.0], [8.0, 10.0]]},
        {"x": 50.0, "offsets": [[0.0, 0.0], [4.0, 8.0], [8.0, 10.0]]},
        {"x": 75.0, "offsets": [[0.0, 0.0], [4.0, 7.0], [8.0, 10.0]]},
        {"x": 100.0, "offsets": [[0.0, 0.0], [4.0, 3.0], [8.0, 4.0]]},
    ],
    "waterlines": [],
    "profile": [],
}

SEMI_PONTOON_JSON_DEFINITION = {
    "name": "semi_pontoon",
    "hull_type": "semi_pontoon",
    "length_bp": 80.0,
    "beam": 15.0,
    "draft": 20.0,
    "depth": 25.0,
    "source": "unit_test",
    "stations": [
        {"x": 0.0, "offsets": [[0.0, 7.5], [20.0, 7.5]]},
        {"x": 40.0, "offsets": [[0.0, 7.5], [20.0, 7.5]]},
        {"x": 80.0, "offsets": [[0.0, 7.5], [20.0, 7.5]]},
    ],
    "waterlines": [],
    "profile": [],
}


@pytest.fixture
def box_barge_json() -> dict:
    """JSON-style hull definition for a box barge."""
    return BOX_BARGE_JSON_DEFINITION


@pytest.fixture
def ship_shaped_json() -> dict:
    """JSON-style hull definition for a ship-shaped hull."""
    return SHIP_SHAPED_JSON_DEFINITION


@pytest.fixture
def semi_pontoon_json() -> dict:
    """JSON-style hull definition for a semi-sub pontoon."""
    return SEMI_PONTOON_JSON_DEFINITION


@pytest.fixture
def box_barge_csv_path(tmp_path: Path) -> Path:
    """CSV file with box-barge station offsets."""
    p = tmp_path / "box_barge.csv"
    p.write_text(BOX_BARGE_CSV_STATIONS)
    return p


@pytest.fixture
def box_barge_json_path(tmp_path: Path) -> Path:
    """JSON file with box-barge hull definition."""
    p = tmp_path / "box_barge.json"
    p.write_text(json.dumps(BOX_BARGE_JSON_DEFINITION, indent=2))
    return p


@pytest.fixture
def box_barge_yaml_path(tmp_path: Path) -> Path:
    """YAML file with box-barge hull definition."""
    import yaml

    p = tmp_path / "box_barge.yaml"
    p.write_text(yaml.dump(BOX_BARGE_JSON_DEFINITION, default_flow_style=False))
    return p
