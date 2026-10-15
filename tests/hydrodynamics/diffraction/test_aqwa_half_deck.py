"""Solver-free text checks for an xz-symmetric AQWA half-deck."""

import hashlib
import re
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec


def _box_panels():
    """Twelve wetted quads, split at y=0; six on either side."""
    panels = []
    for x0, x1 in [(-2.0, 0.0), (0.0, 2.0)]:
        for y0, y1 in [(-1.0, 0.0), (0.0, 1.0)]:
            panels.append([(x0, y0, -1), (x0, y1, -1),
                           (x1, y1, -1), (x1, y0, -1)])
        panels.append([(x0, -1, -1), (x1, -1, -1),
                       (x1, -1, 0), (x0, -1, 0)])
        panels.append([(x1, 1, -1), (x0, 1, -1),
                       (x0, 1, 0), (x1, 1, 0)])
    for y0, y1 in [(-1.0, 0.0), (0.0, 1.0)]:
        panels.append([(-2, y1, -1), (-2, y0, -1),
                       (-2, y0, 0), (-2, y1, 0)])
        panels.append([(2, y0, -1), (2, y1, -1),
                       (2, y1, 0), (2, y0, 0)])
    # Negative-side nodes come first, so retaining old indices leaves gaps.
    return np.array(sorted(panels, key=lambda p: min(v[1] for v in p)), dtype=float)


def _spec(tmp_path, symmetry="xz", lid=True, panels=None):
    panels = _box_panels() if panels is None else panels
    mesh_file = tmp_path / "synthetic.gdf"
    records = ["Synthetic symmetric box", "1.0 9.81", "0 0", str(len(panels))]
    records.extend(" ".join(map(str, vertex)) for panel in panels for vertex in panel)
    mesh_file.write_text("\n".join(records) + "\n")
    return DiffractionSpec.model_validate({
        "version": "1.0",
        "analysis_type": "diffraction",
        "vessel": {
            "name": "synthetic",
            "type": "barge",
            "geometry": {"mesh_file": str(mesh_file), "symmetry": symmetry},
            "inertia": {
                "mass": 8200.0,
                "centre_of_gravity": [0.0, 0.0, -0.5],
                "radii_of_gyration": [1.0, 2.0, 2.0],
            },
        },
        "environment": {"water_depth": 50.0},
        "frequencies": {"input_type": "frequency", "values": [0.5]},
        "wave_headings": {"values": [0.0, 45.0, 90.0, 135.0, 180.0],
                          "symmetry": True},
        "solver_options": {"remove_irregular_frequencies": lid},
    })


def _nodes(cards):
    return {
        int(line[6:11]): tuple(float(line[start:start + 10]) for start in (20, 30, 40))
        for line in cards
        if line[:6].strip().isdigit() and line[6:11].strip().isdigit()
        and int(line[6:11]) != 98000
    }


def _elements(cards):
    return [
        [int(node) for node in re.findall(r"\(\s*(\d+)\)", line)][1:]
        for line in cards if "QPPL DIFF" in line or "TPPL DIFF" in line
    ]


def test_half_deck_retains_nonnegative_panels_and_compacts_nodes(tmp_path):
    spec = _spec(tmp_path)
    backend = AQWABackend()
    nodes = _nodes(backend.build_deck1(spec))
    deck2 = backend.build_deck2(spec)
    elements = _elements(deck2)
    expected = [panel for panel in _box_panels() if np.all(panel[:, 1] >= 0)]
    assert len(elements) == len(expected) == 6
    assert all(vertex[1] >= 0 for vertex in nodes.values())
    assert sorted(nodes) == list(range(1, len(nodes) + 1))
    assert {node for element in elements for node in element} == set(nodes)
    assert [[nodes[node] for node in element] for element in elements] == [
        [tuple(vertex) for vertex in panel] for panel in expected
    ]
    assert [int(line.split()[-1]) for line in deck2 if "Aqwa Elem No:" in line] == list(range(1, 7))


@pytest.mark.parametrize("lid", [False, True])
def test_symx_card_layout_preserves_lid_and_none_output(tmp_path, lid):
    spec = _spec(tmp_path, lid=lid)
    backend = AQWABackend()
    cards = backend.build_deck2(spec)
    assert cards.count("     1SYMX") == 1
    assert cards.index("          ELM1") < cards.index("     1SYMX")
    assert cards.index("     1SYMX") < next(i for i, line in enumerate(cards) if "SEAG" in line)
    assert sum("ILID AUTO" in line for line in cards) == int(lid)
    assert cards[-2:] == [" END", "          FINI"]
    spec.vessel.geometry.symmetry = "none"
    none_cards = backend.build_deck2(spec)
    assert not any("SYMX" in line for line in none_cards)
    assert sum("ILID AUTO" in line for line in none_cards) == int(lid)
    assert len(_elements(none_cards)) == 12


def test_half_mesh_tolerance_snaps_plane_nodes_without_mutating_source(tmp_path):
    panels = _box_panels()
    panels[:, :, 1][panels[:, :, 1] == 0] = -5e-9
    spec = _spec(tmp_path, panels=panels)
    backend = AQWABackend()
    half_nodes = _nodes(backend.build_deck1(spec))
    assert all(vertex[1] >= 0 for vertex in half_nodes.values())
    assert any(vertex[1] == 0 for vertex in half_nodes.values())
    assert len(_elements(backend.build_deck2(spec))) == 6
    spec.vessel.geometry.symmetry = "none"
    full_nodes = _nodes(backend.build_deck1(spec))
    assert any(vertex[1] == -5e-9 for vertex in full_nodes.values())
    assert len(_elements(backend.build_deck2(spec))) == 12


@pytest.mark.parametrize("deck", [1, 2])
def test_half_deck_reports_straddling_panels(tmp_path, deck):
    panels = _box_panels()
    panels[0, 0, 1] = 0.25
    spec = _spec(tmp_path, panels=panels)
    with pytest.raises(ValueError, match=r"xz.*straddl.*1"):
        getattr(AQWABackend(), f"build_deck{deck}")(spec)


def test_plane_panels_and_triangles_keep_connectivity(tmp_path):
    panels = _box_panels()
    panels[6, 3] = panels[6, 2]
    plane = np.array([[(0, 0, -1), (1, 0, -1), (1, 0, 0), (0, 0, 0)]])
    spec = _spec(tmp_path, panels=np.concatenate([panels, plane]))
    backend = AQWABackend()
    deck2 = backend.build_deck2(spec)
    assert len(_elements(deck2)) == 7
    assert sum("TPPL DIFF" in line for line in deck2) == 1
    nodes = _nodes(backend.build_deck1(spec))
    assert all(node in nodes for panel in _elements(deck2) for node in panel)


@pytest.mark.parametrize("heading_symmetry", [False, True])
def test_symmetric_headings_end_at_180_but_none_still_expands(tmp_path, heading_symmetry):
    spec = _spec(tmp_path)
    spec.wave_headings.symmetry = heading_symmetry
    backend = AQWABackend()
    headings = lambda: [float(line.split()[-1]) for line in backend.build_deck6(spec) if "DIRN" in line]
    assert headings() == [0, 45, 90, 135, 180]
    spec.vessel.geometry.symmetry = "none"
    assert headings() == [-180, -135, -90, -45, 0, 45, 90, 135, 180]


@pytest.mark.parametrize("headings, expected", [
    ([0, 45, 90, 135, 180], [0, 45, 90, 135, 180]),
    ([-180, -90, 0, 45, 270, 360], [0, 45, 90, 180]),
])
def test_heading_helper_limits_symmetric_range(headings, expected):
    assert AQWABackend._expand_headings_for_aqwa(headings, symmetry=True) == expected
    assert AQWABackend._expand_headings_for_aqwa([0, 90, 180]) == [-180, -90, 0, 90, 180]


# CRLF single-deck digests captured at 47876058 before changing the writer,
# with geometry.symmetry="none" and the existing heading flags unchanged.
_NONE_DIGESTS = {
    "spec_ship_raos.yml": (
        "fced3caccca828ff595cd0bd6261da338c7ab25ece791c12a6b828924fe01efe",
        "0f123afd600194b27b9d8ce786599edf7a17b1248f3a0960c44d5bb3e73a148e",
    ),
    "spec_semisub.yml": (
        "efea213dcf72da37adb61a6bce49a69864d8bf3df7231bde03d2a34b4b57de8a",
        "6b44238a436779bbb38424ecc15a884d71b32b7e1094658d6cbf653d53e0e2f4",
    ),
    "spec_fpso_turret.yml": (
        "f791f9b96f9d244dbefe822aafb12976ab51c244576721a2adfc1e479ec91a82",
        "7f602e7f45772fa39558e660cd13863ce83a959827336858227ce133a325d9fe",
    ),
}


@pytest.mark.parametrize("fixture", sorted(_NONE_DIGESTS))
def test_none_decks_match_base_bytes_and_half_builds_repeat(tmp_path, fixture):
    fixtures = Path(__file__).parent / "fixtures"
    spec = DiffractionSpec.from_yaml(fixtures / fixture)
    for body in spec.get_bodies():
        body.vessel.geometry.symmetry = "none"
    for resolved, digest in enumerate(_NONE_DIGESTS[fixture]):
        backend = AQWABackend()
        backend._spec_dir = fixtures if resolved else None
        first = ("\r\n".join(backend._assemble_all_decks(spec)) + "\r\n").encode()
        second = ("\r\n".join(backend._assemble_all_decks(spec)) + "\r\n").encode()
        assert first == second
        assert hashlib.sha256(first).hexdigest() == digest
    half = _spec(tmp_path)
    backend = AQWABackend()
    first = backend.generate_single(half, tmp_path / "first").read_bytes()
    second = backend.generate_single(half, tmp_path / "second").read_bytes()
    assert first == second
    assert first.count(b"SYMX") == 1
