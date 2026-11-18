# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

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
    if panels is None:
        panels = _box_panels()
        if symmetry == "xz":
            panels = panels[np.all(panels[:, :, 1] >= 0, axis=1)]
    flags = "0 1" if symmetry == "xz" else "0 0"  # WAMIT v7.4, section 6.1.
    mesh_file = tmp_path / "synthetic.gdf"
    records = ["Synthetic symmetric box", "1.0 9.81", flags, str(len(panels))]
    records.extend(" ".join(map(str, vertex)) for panel in panels for vertex in panel)
    mesh_file.write_text("\n".join(records) + "\n", encoding="utf-8", newline="\n")
    return DiffractionSpec.model_validate({
        "version": "1.0",
        "analysis_type": "diffraction",
        "vessel": {
            "name": "synthetic",
            "type": "barge",
            "geometry": {"mesh_file": mesh_file.as_posix(), "symmetry": symmetry},
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
    spec = _spec(tmp_path, symmetry="none", lid=lid)
    backend = AQWABackend()
    none_cards = backend.build_deck2(spec)
    assert not any("SYMX" in line for line in none_cards)
    assert sum("ILID AUTO" in line for line in none_cards) == int(lid)
    assert len(_elements(none_cards)) == 12


def test_half_mesh_tolerance_snaps_plane_nodes_without_mutating_source(tmp_path):
    panels = _box_panels()
    panels = panels[np.all(panels[:, :, 1] >= 0, axis=1)]
    panels[:, :, 1][panels[:, :, 1] == 0] = 5e-9
    spec = _spec(tmp_path, panels=panels)
    backend = AQWABackend()
    half_nodes = _nodes(backend.build_deck1(spec))
    assert all(vertex[1] >= 0 for vertex in half_nodes.values())
    assert any(vertex[1] == 0 for vertex in half_nodes.values())
    assert len(_elements(backend.build_deck2(spec))) == 6
    source = backend._load_mesh(spec.vessel.geometry.mesh_file)
    assert np.any(source.vertices[:, 1] == 5e-9)


@pytest.mark.parametrize("deck", [1, 2])
def test_half_deck_reports_straddling_panels(tmp_path, deck):
    panels = _box_panels()
    panels[0, 0, 1] = 0.25
    spec = _spec(tmp_path, panels=panels)
    with pytest.raises(ValueError, match="both sides"):
        getattr(AQWABackend(), f"build_deck{deck}")(spec)


def test_plane_panels_are_rejected(tmp_path):
    panels = _box_panels()
    panels = panels[np.all(panels[:, :, 1] >= 0, axis=1)]
    plane = np.array([[(0, 0, -1), (1, 0, -1), (1, 0, 0), (0, 0, 0)]])
    spec = _spec(tmp_path, panels=np.concatenate([panels, plane]))
    with pytest.raises(ValueError, match=r"plane.*7"):
        AQWABackend().build_deck2(spec)


def test_mirrored_triangles_allow_different_repeated_corners(tmp_path):
    panels = np.array([
        [(0, 0, -1), (1, 0, -1), (0, 1, -1), (0, 1, -1)],
        [(0, 0, -1), (0, -1, -1), (1, 0, -1), (0, 0, -1)],
    ], dtype=float)
    spec = _spec(tmp_path, panels=panels[:1])
    backend = AQWABackend()
    cards = backend.build_deck2(spec)
    assert sum("TPPL DIFF" in line for line in cards) == 1
    nodes = _nodes(backend.build_deck1(spec))
    assert all(node in nodes for panel in _elements(cards) for node in panel)


def test_full_coordinates_are_rejected_for_a_declared_cut(tmp_path):
    with pytest.raises(ValueError, match="both sides"):
        AQWABackend().build_deck2(_spec(tmp_path, panels=_box_panels()))


@pytest.mark.parametrize("offset, accepted", [(0.00032717, True), (-0.01, False)])
def test_body_relative_plane_tolerance(tmp_path, offset, accepted):
    panels = _box_panels()
    panels = panels[np.all(panels[:, :, 1] >= 0, axis=1)]
    panels[:, :, 0] *= 45  # 4 m * 45 = 180 m body length.
    # Move one shared centreline node, consistently in every incident panel.
    moved = np.all(panels == [-90, 0, -1], axis=2)
    panels[:, :, 1][moved] = offset
    spec = _spec(tmp_path, panels=panels)
    if accepted:
        nodes = _nodes(AQWABackend().build_deck1(spec))
        assert (-90.0, 0.0, -1.0) in nodes.values()
    else:
        with pytest.raises(ValueError, match="both sides"):
            AQWABackend().build_deck1(spec)


def _hydrostatic_sums(vertices, panels):
    """Volume = sum(z n_z A); waterplane = abs(sum(n_z A))."""
    points = vertices[panels]
    area_vectors = (
        np.cross(points[:, 1] - points[:, 0], points[:, 2] - points[:, 0])
        + np.cross(points[:, 2] - points[:, 0], points[:, 3] - points[:, 0])
    ) / 2
    return np.array([
        np.sum(points[:, :, 2].mean(axis=1) * area_vectors[:, 2]),
        abs(np.sum(area_vectors[:, 2])),
    ])


def test_half_volume_and_waterplane_recover_full_body(tmp_path):
    spec = _spec(tmp_path)
    backend = AQWABackend()
    stored = backend._load_mesh(spec.vessel.geometry.mesh_file)
    half = backend._load_body_mesh(spec.vessel.geometry)
    assert _hydrostatic_sums(stored.vertices, stored.panels) == pytest.approx([4, 4])
    assert 2 * _hydrostatic_sums(half.vertices, half.panels) == pytest.approx([8, 8])


def test_committed_offset_box_requires_full_deck(tmp_path):
    root = Path(__file__).resolve().parents[3]
    spec = DiffractionSpec.from_yaml(
        root / "examples/workflows/aqwa-diffraction-deck-prep/spec.yml"
    )
    backend = AQWABackend()
    backend._spec_dir = root / "examples/workflows/aqwa-diffraction-deck-prep"
    mesh = backend._load_mesh(spec.vessel.geometry.mesh_file)
    assert _hydrostatic_sums(mesh.vertices, mesh.panels) == pytest.approx([1, 1])
    cards = backend.build_deck2(spec)
    assert not any("SYMX" in line for line in cards)
    assert len(_elements(cards)) == 5
    spec.vessel.geometry.symmetry = "xz"
    with pytest.raises(ValueError):
        backend.build_deck2(spec)


@pytest.mark.parametrize("headings, expected", [
    ([10.1, -10.1], [0, 10.1, 180]),
    ([-30.3, 30.3], [0, 30.3, 180]),
    ([180.00000001], [0, 180]),
])
def test_folded_dirn_values_are_unique(tmp_path, headings, expected):
    spec = _spec(tmp_path)
    spec.wave_headings.values = headings
    cards = AQWABackend().build_deck6(spec)
    values = [float(line.split()[-1]) for line in cards if "DIRN" in line]
    assert values == expected


def test_dirn_precision_collision_is_rejected(tmp_path):
    spec = _spec(tmp_path)
    spec.wave_headings.values = [10.123451, 10.123452]
    with pytest.raises(ValueError, match="unique after DIRN formatting"):
        AQWABackend().build_deck6(spec)


@pytest.mark.parametrize("heading_symmetry", [False, True])
def test_symmetric_headings_end_at_180_but_none_still_expands(tmp_path, heading_symmetry):
    spec = _spec(tmp_path)
    spec.wave_headings.symmetry = heading_symmetry
    backend = AQWABackend()
    headings = lambda: [float(line.split()[-1]) for line in backend.build_deck6(spec) if "DIRN" in line]
    assert headings() == [0, 45, 90, 135, 180]
    spec = _spec(tmp_path, symmetry="none")
    backend = AQWABackend()
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
# The multi-body digests include one empty WFS block per body; removing the
# additional block recovers both earlier digests without any other byte change.
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
        "e18a2d721648d951f48e8ff24dbe0758abe89f1322fe22b5c07e139ed9bd4114",
        "404eef2ade678b95cc4876548903473c18abc912a3b5ef8bf3034d035a9e2a15",
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
        if not resolved:
            with pytest.raises(ValueError, match="loaded and verified"):
                backend._assemble_all_decks(spec)
            continue
        first = ("\r\n".join(backend._assemble_all_decks(spec)) + "\r\n").encode()
        second = ("\r\n".join(backend._assemble_all_decks(spec)) + "\r\n").encode()
        assert first == second
        if len(spec.get_bodies()) == 2:
            assert backend.build_deck7(spec)[-4:] == [
                "          WFS1", " END", "          WFS2", " END",
            ]
        assert hashlib.sha256(first).hexdigest() == digest, (
            "Rebuild and inspect with: python -m pytest "
            "tests/hydrodynamics/diffraction/test_aqwa_half_deck.py "
            "-k test_none_decks_match_base_bytes_and_half_builds_repeat -vv"
        )
    half = _spec(tmp_path)
    backend = AQWABackend()
    first = backend.generate_single(half, tmp_path / "first").read_bytes()
    second = backend.generate_single(half, tmp_path / "second").read_bytes()
    assert first == second
    assert first.count(b"SYMX") == 1


@pytest.mark.parametrize("deck", [1, 2, 6])
@pytest.mark.parametrize("mesh_case", ["missing", "relative", "non_gdf"])
def test_xz_requires_a_loadable_verified_mesh(tmp_path, deck, mesh_case):
    spec = _spec(tmp_path)
    if mesh_case == "missing":
        spec.vessel.geometry.mesh_file = (tmp_path / "missing.gdf").as_posix()
    elif mesh_case == "relative":
        # The file exists beside the spec, but no spec directory is supplied.
        spec.vessel.geometry.mesh_file = Path(spec.vessel.geometry.mesh_file).name
    else:
        mesh_file = tmp_path / "synthetic.dat"
        mesh_file.write_text("unsupported mesh format\n", encoding="utf-8", newline="\n")
        spec.vessel.geometry.mesh_file = mesh_file.as_posix()
        spec.vessel.geometry.mesh_format = "dat"
    with pytest.raises(ValueError, match="xz.*verified"):
        getattr(AQWABackend(), f"build_deck{deck}")(spec)


def test_mixed_bodies_do_not_hide_an_unverified_xz_mesh(tmp_path):
    spec = _spec(tmp_path)
    data = spec.model_dump(mode="json", exclude_none=True)
    vessel = data.pop("vessel")
    data["bodies"] = [{"vessel": vessel}, {"vessel": vessel}]
    mixed = DiffractionSpec.model_validate(data)
    mixed.bodies[0].vessel.geometry.symmetry = "none"
    mixed.bodies[1].vessel.geometry.mesh_file = (tmp_path / "missing.gdf").as_posix()
    with pytest.raises(ValueError, match="xz.*verified"):
        AQWABackend().build_deck6(mixed)


_OFFSET_SPEC_PATHS = [
    "tests/hydrodynamics/diffraction/fixtures/spec_ship_raos.yml",
    "tests/hydrodynamics/diffraction/fixtures/spec_fpso_turret.yml",
    "tests/hydrodynamics/diffraction/fixtures/benchmarks/multibody_fpso_turret_v1/spec.yml",
    "tests/hydrodynamics/diffraction/fixtures/acceptance_610/spec.yml",
    "examples/hydrodynamics/diffraction/unit_box_rao/spec.yml",
    "examples/workflows/orcawave-input-prep/spec.yml",
    "examples/workflows/orcawave-diffraction-solve/spec.yml",
    "examples/workflows/orcawave-field-points/spec.yml",
    "examples/workflows/orcawave-mean-drift/spec.yml",
    "examples/workflows/orcawave-control-surface/spec.yml",
]


@pytest.mark.parametrize("spec_path", _OFFSET_SPEC_PATHS)
def test_offset_box_specs_use_full_body_decks(spec_path):
    path = Path(__file__).resolve().parents[3] / spec_path
    spec = DiffractionSpec.from_yaml(path)
    backend = AQWABackend()
    backend._spec_dir = path.parent
    assert all(body.vessel.geometry.symmetry == "none" for body in spec.get_bodies())
    assert "SYMX" not in "\n".join(backend.build_deck2(spec))


def test_orcawave_preserves_matching_half_mesh(tmp_path):
    import yaml

    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend

    spec = _spec(tmp_path, lid=False)
    mesh = Path(spec.vessel.geometry.mesh_file)
    original = mesh.read_bytes()
    # Both backends retain the stored half and request its declared reflection.
    assert len(_elements(AQWABackend().build_deck2(spec))) == 6
    output = OrcaWaveBackend().generate_single(spec, tmp_path / "orcawave")
    body = yaml.safe_load(output.read_text(encoding="utf-8"))["Bodies"][0]
    assert body["BodyMeshSymmetry"] == "xz plane"
    assert body["BodyMeshFileName"] == mesh.name
    assert mesh.read_bytes() == original
    assert int(original.decode("utf-8").splitlines()[3]) == len(_box_panels()) // 2


@pytest.mark.parametrize("mesh_format", [None, "gdf"])
def test_orcawave_box_writes_concrete_mesh_format(tmp_path, mesh_format):
    import yaml

    from digitalmodel.hydrodynamics.diffraction.input_schemas import MeshFormatType
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend

    spec = _spec(tmp_path, lid=False)
    if mesh_format is not None:
        spec.vessel.geometry.mesh_format = MeshFormatType(mesh_format)
    output = OrcaWaveBackend().generate_single(spec, tmp_path / "orcawave")
    text = output.read_text(encoding="utf-8")
    assert yaml.safe_load(text)["Bodies"][0]["BodyMeshFormat"] == "Wamit gdf"
    assert "BodyMeshFormat: Auto" not in text


@pytest.mark.parametrize("mesh_format, extension, expected", [
    ("auto", "gdf", "Wamit gdf"), ("gdf", "GDF", "Wamit gdf"),
    ("auto", "MSH", "Gmsh msh"), ("msh", "msh", "Gmsh msh"),
    ("auto", "obj", "Wavefront obj"), ("obj", "obj", "Wavefront obj"),
    ("aqwa_dat", "dat", "Aqwa dat"),
])
def test_orcawave_mesh_formats_across_sections(tmp_path, mesh_format, extension, expected):
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import _build_bodies_section

    spec = _format_spec(extension, mesh_format)
    if extension.lower() == "gdf":
        spec.vessel.geometry.mesh_file = _spec(tmp_path, symmetry="none").vessel.geometry.mesh_file
    assert _build_bodies_section(spec)["Bodies"][0]["BodyMeshFormat"] == expected


@pytest.mark.parametrize("mesh_format", ["auto", "gdf", "GDF", "Auto"])
def test_orcawave_lid_and_zone_use_native_gdf(mesh_format):
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import (
        _build_damping_lid_section, _build_qtf_section,
    )

    spec = _format_spec("gdf", "gdf")
    spec.damping_lid.mesh_format = mesh_format
    spec.free_surface_zone.mesh_format = mesh_format
    assert _build_damping_lid_section(spec)["DampingLidMeshFormat"] == "Wamit gdf"
    assert _build_qtf_section(spec)["FreeSurfacePanelledZoneMeshFormat"] == "Wamit gdf"


@pytest.mark.parametrize("extension", ["msh", "obj"])
@pytest.mark.parametrize("section", ["lid", "zone"])
def test_orcawave_auxiliary_formats_require_native_evidence(extension, section):
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import (
        _build_damping_lid_section, _build_qtf_section,
    )

    build = {"lid": _build_damping_lid_section, "zone": _build_qtf_section}[section]
    with pytest.raises(ValueError, match="mesh format"):
        build(_format_spec(extension, "auto"))


@pytest.mark.parametrize("extension, mesh_format", [("obj", "gdf"), ("gdf", "msh"), ("gdf", "aqwa_dat")])
def test_orcawave_refuses_contradictory_extensions(extension, mesh_format):
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import _build_bodies_section

    with pytest.raises(ValueError, match="contradicts.*synthetic"):
        _build_bodies_section(_format_spec(extension, mesh_format))


@pytest.mark.parametrize("mesh_format, extension", [("gdf", "gdf"), ("msh", "msh"), ("obj", "obj"), ("aqwa_dat", "dat")])
def test_orcawave_mesh_format_round_trip(tmp_path, mesh_format, extension):
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import _build_bodies_section
    from digitalmodel.hydrodynamics.diffraction.reverse_parsers import OrcaWaveInputParser

    spec = _format_spec(extension, mesh_format)
    if extension == "gdf":
        spec.vessel.geometry.mesh_file = _spec(tmp_path, symmetry="none").vessel.geometry.mesh_file
    native = _build_bodies_section(spec)["Bodies"][0]["BodyMeshFormat"]
    assert OrcaWaveInputParser._reverse_mesh_format(native) == mesh_format


def test_orcawave_unknown_reverse_mesh_format_is_refused():
    from digitalmodel.hydrodynamics.diffraction.reverse_parsers import OrcaWaveInputParser

    with pytest.raises(ValueError, match="mesh format"):
        OrcaWaveInputParser._reverse_mesh_format("unknown")


def _format_spec(extension, mesh_format):
    return DiffractionSpec.model_validate({
        "vessel": {
            "name": "synthetic", "type": "barge",
            "geometry": {"mesh_file": f"synthetic.{extension}",
                         "mesh_format": mesh_format},
            "inertia": {"mass": 8200.0, "centre_of_gravity": [0, 0, -0.5],
                        "radii_of_gyration": [1, 2, 2]},
        },
        "environment": {"water_depth": 50.0},
        "frequencies": {"input_type": "frequency", "values": [0.5]},
        "wave_headings": {"values": [0.0]},
        "solver_options": {"solve_type": "full_qtf"},
        "damping_lid": {"mesh_file": f"lid.{extension}",
                        "mesh_format": mesh_format, "damping_factor": 0.1},
        "free_surface_zone": {"mesh_file": f"zone.{extension}",
                              "mesh_format": mesh_format},
    })


@pytest.mark.parametrize("extension, mesh_format", [
    ("stl", "auto"), ("dat", "auto"), ("unknown", "auto"),
    ("stl", "stl"), ("dat", "dat"),
])
@pytest.mark.parametrize("section", ["body", "lid", "zone"])
def test_orcawave_refuses_unsupported_mesh_formats(extension, mesh_format, section):
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import (
        _build_bodies_section, _build_damping_lid_section, _build_qtf_section,
    )

    build = {"body": _build_bodies_section, "lid": _build_damping_lid_section,
             "zone": _build_qtf_section}[section]
    with pytest.raises(ValueError, match="mesh format"):
        build(_format_spec(extension, mesh_format))


@pytest.mark.parametrize("section", ["lid", "zone"])
def test_orcawave_auxiliary_mesh_extension_conflict(section):
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import (
        _build_damping_lid_section, _build_qtf_section,
    )

    spec = _format_spec("gdf", "gdf")
    if section == "lid":
        spec.damping_lid.mesh_file = "lid.fdf"
        build = _build_damping_lid_section
    else:
        spec.free_surface_zone.mesh_format = "fdf"
        build = _build_qtf_section
    with pytest.raises(ValueError, match="contradicts"):
        build(spec)


@pytest.mark.parametrize("mesh_format", ["fdf", "FDF", "auto", "Auto"])
def test_orcawave_zone_preserves_fdf(mesh_format):
    from digitalmodel.hydrodynamics.diffraction.orcawave_backend import _build_qtf_section

    spec = _format_spec("gdf", "gdf")
    spec.free_surface_zone.mesh_file = "zone.fdf"
    spec.free_surface_zone.mesh_format = mesh_format
    assert _build_qtf_section(spec)["FreeSurfacePanelledZoneMeshFormat"] == "Wamit fdf"
