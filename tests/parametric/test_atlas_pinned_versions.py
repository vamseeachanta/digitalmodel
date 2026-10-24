"""Previously published atlas versions stay loadable by explicit ID (PR #2164 r1)."""

from pathlib import Path

import pytest

from digitalmodel.parametric.atlas import Atlas

ROOT = Path(__file__).resolve().parents[2] / "atlases"


@pytest.mark.parametrize("atlas_id", ["229487678a1a", "bcc370f46b9c", "4ccd156870c1"])
def test_drilling_riser_operability_versions_load(atlas_id):
    atlas = Atlas.load(ROOT, "drilling_riser_operability", atlas_id)
    assert atlas.atlas_id == atlas_id


def test_default_points_at_current_version():
    from digitalmodel.drilling_riser.operability_atlas import content_fingerprint

    atlas = Atlas.load(ROOT, "drilling_riser_operability")
    assert atlas.atlas_id == content_fingerprint()[:12]
