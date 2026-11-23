"""Dimension convenience API retains the existing catalogue metric."""
import math
import pytest

from digitalmodel.hydrodynamics.hull_library.lookup import closest_hull
from digitalmodel.hydrodynamics.hull_library.panel_catalog import PanelCatalog, PanelCatalogEntry, PanelFormat
from digitalmodel.hydrodynamics.hull_library.profile_schema import HullType


def test_known_catalogue_entry():
    entry = PanelCatalogEntry(hull_id="test-box", name="Test box", source="synthetic test",
                             hull_type=HullType.BARGE, panel_format=PanelFormat.GDF,
                             file_path="test.gdf", length_m=100, beam_m=20, draft_m=5)
    catalog = PanelCatalog(entries=[entry])
    exact = closest_hull(100, 20, 5, catalog=catalog)
    assert exact.hull_id == "test-box"
    assert exact.similarity_score == 1
    assert exact.source == "panel_catalog"
    shifted = closest_hull(200, 20, 5, catalog=catalog)
    assert shifted.similarity_score == pytest.approx(math.exp(-0.5))
    assert shifted.scaling_factors["loa"] == 2


@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf")])
def test_invalid_dimensions(value):
    with pytest.raises(ValueError):
        closest_hull(value, 20, 5)
