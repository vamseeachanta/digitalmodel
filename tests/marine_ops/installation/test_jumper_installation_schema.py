"""Tests for JumperInstallationSpec and PassingShipSpec validation (#506).

Covers:
- Each existing sample spec.yml validates against its Pydantic schema.
- A malformed spec raises a clear ValidationError.
- dict / YAML round-trip preserves the key fields.
"""

from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.marine_ops.installation.jumper_installation_schema import (
    JumperInstallationSpec,
)
from digitalmodel.hydrodynamics.passing_ship.input_schemas import PassingShipSpec


# Repo root: tests/marine_ops/installation/ -> up 3 levels
REPO_ROOT = Path(__file__).resolve().parents[3]
JUMPER_DIR = REPO_ROOT / "docs/domains/orcaflex/subsea/jumper/installation"
PASSING_SHIP_SPEC = (
    REPO_ROOT / "docs/domains/orcaflex/passing_ship/sample/spec.yml"
)

JUMPER_SPECS = [
    JUMPER_DIR / "ballymore_mf_plet" / "spec.yml",
    JUMPER_DIR / "ballymore_plet_plem" / "spec.yml",
]


# ---------------------------------------------------------------------------
# JumperInstallationSpec — sample specs validate
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("spec_path", JUMPER_SPECS, ids=lambda p: p.parent.name)
def test_jumper_sample_spec_validates(spec_path):
    assert spec_path.exists(), f"sample spec missing: {spec_path}"
    spec = JumperInstallationSpec.from_yaml(spec_path)
    assert spec.metadata.structure == "jumper"
    assert spec.pipe.outer_diameter > 0
    assert spec.environment.water.depth == 1400
    # Crane configurations present and typed
    assert "SZ" in spec.crane_configuration
    assert spec.crane_configuration["SZ"].swl_te == 77.5
    # All 7 segment lengths captured
    assert spec.jumper.seg_a_inch == 336.0
    assert spec.jumper.seg_g_inch == 352.0


def test_jumper_mf_plet_optional_metocean_present():
    """MF-PLET spec carries environment.metocean; PLET-PLEM does not."""
    mf = JumperInstallationSpec.from_yaml(JUMPER_SPECS[0])
    plet = JumperInstallationSpec.from_yaml(JUMPER_SPECS[1])
    assert mf.environment.metocean is not None
    assert mf.environment.metocean.wave.significant == 4.0
    assert plet.environment.metocean is None  # absent in PLET-PLEM spec


def test_jumper_dz_ahc_offset():
    """PLET-PLEM DZ crane sets ahc_offset; MF-PLET DZ leaves it unset."""
    mf = JumperInstallationSpec.from_yaml(JUMPER_SPECS[0])
    plet = JumperInstallationSpec.from_yaml(JUMPER_SPECS[1])
    assert plet.crane_configuration["DZ"].ahc_offset is True
    assert mf.crane_configuration["DZ"].ahc_offset is None


def test_jumper_extra_blocks_ignored():
    """Unmodeled top-level blocks (rigging, weight_check) don't break load."""
    spec = JumperInstallationSpec.from_yaml(JUMPER_SPECS[0])
    # weight_check and rigging exist in the YAML but are not modeled fields
    assert not hasattr(spec, "weight_check")


# ---------------------------------------------------------------------------
# JumperInstallationSpec — malformed raises
# ---------------------------------------------------------------------------


def test_jumper_missing_required_field_raises():
    with open(JUMPER_SPECS[0]) as fh:
        data = yaml.safe_load(fh)
    del data["pipe"]["outer_diameter"]
    with pytest.raises(ValidationError) as exc:
        JumperInstallationSpec.from_dict(data)
    assert "outer_diameter" in str(exc.value)


def test_jumper_negative_diameter_raises():
    with open(JUMPER_SPECS[0]) as fh:
        data = yaml.safe_load(fh)
    data["pipe"]["outer_diameter"] = -1.0
    with pytest.raises(ValidationError) as exc:
        JumperInstallationSpec.from_dict(data)
    assert "outer_diameter" in str(exc.value)


def test_jumper_empty_crane_config_raises():
    with open(JUMPER_SPECS[0]) as fh:
        data = yaml.safe_load(fh)
    data["crane_configuration"] = {}
    with pytest.raises(ValidationError) as exc:
        JumperInstallationSpec.from_dict(data)
    assert "at least one crane" in str(exc.value)


# ---------------------------------------------------------------------------
# JumperInstallationSpec — round-trip
# ---------------------------------------------------------------------------


def test_jumper_dict_round_trip():
    spec = JumperInstallationSpec.from_yaml(JUMPER_SPECS[0])
    rebuilt = JumperInstallationSpec.from_dict(spec.to_dict())
    assert rebuilt.pipe.outer_diameter == spec.pipe.outer_diameter
    assert rebuilt.jumper.config_name == spec.jumper.config_name
    assert (
        rebuilt.crane_configuration["SZ"].swl_te
        == spec.crane_configuration["SZ"].swl_te
    )


def test_jumper_yaml_round_trip(tmp_path):
    spec = JumperInstallationSpec.from_yaml(JUMPER_SPECS[0])
    out = tmp_path / "rt.yml"
    spec.to_yaml(out)
    reloaded = JumperInstallationSpec.from_yaml(out)
    assert reloaded.pipe.outer_diameter == spec.pipe.outer_diameter
    assert reloaded.environment.water.depth == spec.environment.water.depth


# ---------------------------------------------------------------------------
# PassingShipSpec — sample spec validates + malformed raises
# ---------------------------------------------------------------------------


def test_passing_ship_sample_spec_validates():
    assert PASSING_SHIP_SPEC.exists()
    spec = PassingShipSpec.from_yaml(PASSING_SHIP_SPEC)
    assert spec.name == "tanker_passing_vlcc_sample"
    assert len(spec.moored_vessel.mooring_lines) == 6
    assert spec.passing_ship.track.speed_knots == 5.0
    assert spec.environment.water_depth == 40.0
    assert len(spec.analysis_cases) == 5


def test_passing_ship_malformed_raises():
    with open(PASSING_SHIP_SPEC) as fh:
        data = yaml.safe_load(fh)
    # Invalid line_type triggers the field validator
    data["moored_vessel"]["mooring_lines"][0]["line_type"] = "nonsense"
    with pytest.raises(ValidationError) as exc:
        PassingShipSpec.from_dict(data)
    assert "line_type" in str(exc.value)


def test_passing_ship_round_trip():
    spec = PassingShipSpec.from_yaml(PASSING_SHIP_SPEC)
    rebuilt = PassingShipSpec.from_dict(spec.to_dict())
    assert rebuilt.name == spec.name
    assert (
        rebuilt.passing_ship.track.separation_m
        == spec.passing_ship.track.separation_m
    )
