# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check interior-panel coverage of the held public reference geometry.

Rectangular controls are synthetic, assumed, based on previous experience.
"""

from pathlib import Path

import pytest

# Hosted checks do not install Capytaine, so this file is skipped there.
# Local: PYTHONPATH=src uv run --no-sources --with capytaine==3.0.0 python -m pytest \
#   tests/hydrodynamics/capytaine/test_reference_lid.py -q
pytest.importorskip('capytaine')
pytestmark = pytest.mark.slow


def test_reference_lid_covers_waterplane():
    """A resolved native lid covers the waterplane within five percent."""
    from digitalmodel.hydrodynamics.capytaine.mesh_adapter import create_floating_body
    from digitalmodel.hydrodynamics.capytaine.models import BodyConfig, MeshConfig
    mesh = Path('docs/domains/orcawave/examples/L02 OC4 Semi-sub/L02 OC4 Semi-sub mesh.gdf')
    body = create_floating_body(BodyConfig(
        mesh=MeshConfig(path=mesh, name='OC4'), use_lid=True, lid_z=0.0,
        lid_faces_max_radius=0.5,
    ))
    assert sum(body.lid_mesh.faces_areas) == pytest.approx(body.waterplane_area, rel=0.05)


def test_lid_rectangle_merging_preserves_area_and_normals():
    """Coplanar interior cells merge without changing the covered domain."""
    import numpy as np
    import capytaine as cpt
    from digitalmodel.hydrodynamics.capytaine.lid import merge_lid_cells
    source = cpt.mesh_rectangle(size=(2, 3), resolution=(12, 18), normal=(0, 0, -1))
    merged = merge_lid_cells(source, maximum_edge=1.0)
    assert merged.nb_faces < source.nb_faces
    assert sum(merged.faces_areas) == pytest.approx(sum(source.faces_areas), rel=1e-12)
    np.testing.assert_allclose(merged.faces_normals[:, 2], -1.0)
