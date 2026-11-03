# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Keep the existing lifting-lug atlas usable after shared-mechanics extraction."""

from digitalmodel.parametric import refresh
from digitalmodel.parametric.atlas import Atlas


def test_lifting_lug_fingerprint_tracks_shared_mechanics():
    assert "src/digitalmodel/lifting_lug/mechanics.py" in refresh.SOURCE_FILES["lifting_lug"]


def test_refreshed_atlas_preserves_previous_grid():
    """Regression only: every grid record must match the preceding stored atlas.

    Independent engineering arithmetic is tested in test_mechanics and
    test_assessment. This guards preservation of legacy data after extraction;
    it makes no claim about the legacy allowable's code applicability.
    """
    assert refresh.refresh_status("lifting-lug-design")["stale"] is False
    atlas = Atlas.load(refresh.DEFAULT_ATLAS_ROOT, "lifting_lug")
    previous = Atlas.load(
        refresh.DEFAULT_ATLAS_ROOT, "lifting_lug", atlas_id="d807d703f351"
    )
    assert atlas.grid.equals(previous.grid)
    assert atlas.validation["passes"] is True
