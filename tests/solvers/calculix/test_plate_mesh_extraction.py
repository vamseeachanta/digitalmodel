# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Solid-only plate extraction contracts using synthetic Gmsh responses."""

from types import SimpleNamespace

import numpy as np
import pytest

from digitalmodel.solvers.calculix.fem_chain import FEMChain


class FakeMesh:
    """Expose boundary cells alongside a tetrahedron, as Gmsh does."""

    def __init__(self):
        self.tags = np.arange(1, 5)
        self.coords = np.array([[0., 0., 0.], [1., 0., 0.],
                                [0., 1., 0.], [0., 0., 1.]])
        self.types = {15: ("Point", 0, 1), 1: ("Line 2", 1, 2),
                      2: ("Triangle 3", 2, 3), 4: ("Tetrahedron 4", 3, 4)}
        self.cells = {15: [1], 1: [1, 2], 2: [1, 2, 3], 4: [1, 2, 3, 4]}
        self.dimensions = []
        self.requested_types = []

    def getNodes(self):
        return self.tags, self.coords.ravel(), np.array([])

    def getElementTypes(self, dim=-1):
        self.dimensions.append(dim)
        return np.array([t for t, p in self.types.items() if dim in (-1, p[1])])

    def getElementsByType(self, element_type):
        self.requested_types.append(element_type)
        return np.array([1]), np.array(self.cells[element_type], dtype=np.uint64)

    def getElementProperties(self, element_type):
        name, dim, count = self.types[element_type]
        return name, dim, 1, count, np.array([]), count


def extract(chain, mesh):
    chain._extract_gmsh_data(SimpleNamespace(model=SimpleNamespace(mesh=mesh)))


def test_extraction_exports_volume_cells_only(tmp_path):
    mesh = FakeMesh()
    chain = FEMChain(tmp_path)
    extract(chain, mesh)
    assert mesh.dimensions == [3]
    assert mesh.requested_types == [4]
    assert set(chain._elements) == {"Tetrahedron 4"}
    assert chain._elements["Tetrahedron 4"]["dimension"] == 3
    np.testing.assert_array_equal(chain._nodes, mesh.coords)
    np.testing.assert_array_equal(
        chain._elements["Tetrahedron 4"]["connectivity"], [[0, 1, 2, 3]],
    )


@pytest.mark.parametrize("defect, message", [
    ("sparse", "node tags"),
    ("shuffled", "node tags"),
    ("duplicate", "node tags"),
    ("absent-high", "absent node"),
    ("absent-zero", "absent node"),
    ("no-volume", "volume elements"),
    ("empty-volume", "volume elements"),
])
def test_invalid_mesh_preserves_previous_state(tmp_path, defect, message):
    mesh = FakeMesh()
    if defect == "sparse":
        mesh.tags = np.array([1, 2, 5, 6])
    elif defect == "shuffled":
        mesh.tags = np.array([2, 1, 3, 4])
    elif defect == "duplicate":
        mesh.tags = np.array([1, 2, 3, 3])
    elif defect.startswith("absent"):
        mesh.cells[4][-1] = 5 if defect == "absent-high" else 0
    elif defect == "no-volume":
        del mesh.types[4]
    else:
        mesh.cells[4] = []
    chain = FEMChain(tmp_path)
    chain.set_mesh(mesh.coords + 10., {
        "Tetrahedron 4": {"connectivity": np.array([[0, 1, 2, 3]])},
    })
    previous_nodes, previous_elements = chain._nodes, chain._elements
    nodes_copy = previous_nodes.copy()
    connectivity_copy = previous_elements["Tetrahedron 4"]["connectivity"].copy()
    with pytest.raises(ValueError, match=message):
        extract(chain, mesh)
    assert chain._nodes is previous_nodes
    assert chain._elements is previous_elements
    np.testing.assert_array_equal(chain._nodes, nodes_copy)
    np.testing.assert_array_equal(
        chain._elements["Tetrahedron 4"]["connectivity"], connectivity_copy,
    )


def test_gmsh_error_preserves_previous_state(tmp_path, monkeypatch):
    mesh = FakeMesh()
    chain = FEMChain(tmp_path)
    extract(chain, mesh)
    previous_nodes, previous_elements = chain._nodes, chain._elements

    def fail_properties(element_type):
        raise RuntimeError("Element properties unavailable")

    monkeypatch.setattr(mesh, "getElementProperties", fail_properties)
    with pytest.raises(RuntimeError, match="Element properties unavailable"):
        extract(chain, mesh)
    assert chain._nodes is previous_nodes
    assert chain._elements is previous_elements


def test_generated_plate_exports_only_volume_elements(tmp_path):
    pytest.importorskip("gmsh")
    chain = FEMChain(tmp_path)
    chain.create_plate_with_hole(200., 200., 10., 1., 8.)
    assert {data["dimension"] for data in chain._elements.values()} == {3}
    used_nodes = set().union(*(
        set(data["connectivity"].ravel()) for data in chain._elements.values()
    ))
    assert used_nodes == set(range(len(chain._nodes)))
    deck = chain.setup_analysis(
        material={"name": "MAT", "E": 210000., "nu": 0.3},
        loads=[], boundary_conditions=[],
    ).read_text(encoding="utf-8")
    assert "TYPE=C3D4" in deck
    assert "TYPE=B31" not in deck
