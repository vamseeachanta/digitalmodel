"""Synthetic software checks; no site parameters or published correlations."""
import builtins
from dataclasses import FrozenInstanceError, replace

import pytest
import yaml

from digitalmodel import geotechnical as geo
from digitalmodel.geotechnical.cpt import CPTData
from digitalmodel.geotechnical.soil_profile import SoilLayer, SoilProfile, SoilType
from digitalmodel.geotechnical.soil_profile_adapters import (
    from_cpt, to_cpt, to_slope_profile, to_spudcan_layers,
)
from digitalmodel.geotechnical.soil_profile_yaml import dump_soil_profile, load_soil_profile
from digitalmodel.geotechnical.spudcan import SoilLayer as NativeLayer


@pytest.fixture
def profile():
    return SoilProfile([SoilLayer("clay", 0, 2, su_pa=12000,
                                 bulk_weight_n_m3=18000, submerged_weight_n_m3=7000),
                        SoilLayer("clay", 2, 5, su_pa=0, bulk_weight_n_m3=18000)])


def slope(profile, **kwargs):
    inputs = dict(x_extent_m=(-10, 30), mudline_elevation_m=100,
                  water_unit_weight_n_m3=9810,
                  circles=[dict(x_m=10, y_m=105, radius_m=20)])
    return to_slope_profile(profile, **(inputs | kwargs))


def test_round_trip_nulls_zeros_and_detached_inputs(profile):
    depths = [1, 3]
    cpt = CPTData(depths, [1000000, 2000000], [10000, 20000],
                  u2=[None, 0], area_ratio=0.8, pressure_unit="Pa")
    layers = list(profile.layers)
    attached = from_cpt(cpt, layers)
    depths[0] = 99
    layers.clear()
    assert to_cpt(attached) is cpt
    assert cpt.qc_pa == (1000000, 2000000) and cpt.depth_m == (1, 3)
    text = dump_soil_profile(attached)
    restored = load_soil_profile(text)
    assert load_soil_profile(dump_soil_profile(restored)) == attached
    assert restored.cpt.u2_pa == (None, 0)
    assert restored.layers[1].su_pa == 0 and restored.layers[1].submerged_weight_n_m3 is None
    assert "null" in text and "\r" not in text
    with pytest.raises(FrozenInstanceError):
        attached.layers[0].su_pa = 99
    assert geo.SoilLayer is SoilLayer and geo.SoilType is SoilType
    assert geo.CPTData is CPTData and NativeLayer is not SoilLayer


@pytest.mark.parametrize("field", ["top_m", "bottom_m", "su_pa", "cohesion_pa",
                                   "friction_deg", "bulk_weight_n_m3", "submerged_weight_n_m3"])
@pytest.mark.parametrize("value", [True, float("nan"), float("inf"), -1, "12"])
def test_invalid_numbers(field, value):
    with pytest.raises(ValueError, match=field):
        SoilLayer(**(dict(kind="unknown", top_m=0, bottom_m=2) | {field: value}))


@pytest.mark.parametrize("changes", [dict(bottom_m=0), dict(top_m=3), dict(kind="silt"),
    dict(su_pa=1, friction_deg=0), dict(su_pa=1, cohesion_pa=0), dict(friction_deg=90)])
def test_invalid_layers(changes):
    with pytest.raises(ValueError):
        SoilLayer(**(dict(kind="unknown", top_m=0, bottom_m=2) | changes))


@pytest.mark.parametrize("layers", [[], [SoilLayer("unknown", 1, 2)],
    [SoilLayer("unknown", 0, 2), SoilLayer("unknown", 3, 5)],
    [SoilLayer("unknown", 0, 2), SoilLayer("unknown", 1, 5)], [object()]])
def test_invalid_profiles(layers):
    with pytest.raises(ValueError):
        SoilProfile(layers)


@pytest.mark.parametrize("text", ["schema_version: 1\nschema_version: 1", "!!python/object:bad {}",
    "schema_version: true\nunits: SI\nlayers: []", "schema_version: 2\nunits: SI\nlayers: []",
    "schema_version: 1\nunits: kPa\nlayers: []", "units: SI\nlayers: []", "[]",
    "schema_version: 1.0\nunits: SI\nlayers: []",
    "schema_version: 1\nunits: SI\nlayers: [{kind: clay, kind: sand, top_m: 0, bottom_m: 2}]"])
def test_invalid_yaml(text):
    with pytest.raises((ValueError, yaml.YAMLError)):
        load_soil_profile(text)


@pytest.mark.parametrize("target,field,value", [("root", "extra", 1), ("root", "units", None),
    ("layer", "su_kpa", 12), ("layer", "su_gradient_pa_per_m", 1),
    ("layer", "top_boundary_m", [[0, 0], [1, -1]]), ("cpt", "pressure_unit", "kPa")])
def test_unknown_fields_and_units(profile, target, field, value):
    data = yaml.safe_load(dump_soil_profile(profile))
    data["cpt"] = dict(depth_m=[1], qc_pa=[1000], fs_pa=[10], area_ratio=0.8)
    mapping = data if target == "root" else data["layers"][0] if target == "layer" else data["cpt"]
    mapping[field] = value
    with pytest.raises(ValueError):
        load_soil_profile(yaml.safe_dump(data))


def test_spudcan_conversion_and_missing_properties(profile):
    layers = to_spudcan_layers(SoilProfile(profile.layers[:1]))
    assert layers == (NativeLayer("clay", 0, 2, 7, su_kpa=12),)
    sand = SoilLayer("sand", 0, 2, submerged_weight_n_m3=10000, friction_deg=30, cohesion_pa=0)
    assert to_spudcan_layers(SoilProfile([sand, replace(profile.layers[0], top_m=2, bottom_m=5)]))[0] == NativeLayer("sand", 0, 2, 10, friction_deg=30)
    with pytest.raises(ValueError, match="submerged_weight_n_m3"):
        to_spudcan_layers(profile)
    with pytest.raises(ValueError, match="su_pa"):
        to_spudcan_layers(SoilProfile([replace(profile.layers[0], su_pa=None)]))


def test_slope_geometry_units_and_no_solver_import(profile, monkeypatch):
    original_import = builtins.__import__
    def guarded(name, *args, **kwargs):
        assert name.split(".")[0] not in {"OrcFxAPI", "xslope", "gmsh"}
        return original_import(name, *args, **kwargs)
    monkeypatch.setattr(builtins, "__import__", guarded)
    result = slope(profile)
    assert result["ground_surface_m"] == [[-10, 100], [30, 100]]
    assert result["bottom_elevation_m"] == 95 and result["water_unit_weight_kn_m3"] == 9.81
    assert result["layers"][1]["top_boundary_m"] == [[-10, 98], [30, 98]]
    assert result["layers"][0]["su_kpa"] == 12 and result["layers"][0]["unit_weight_kn_m3"] == 18
    assert result["layers"][1]["su_kpa"] == 0 and "c_kpa" not in result["layers"][1]
    drained = SoilProfile([SoilLayer("unknown", 0, 5, cohesion_pa=0, friction_deg=30, bulk_weight_n_m3=18000)])
    assert slope(drained)["layers"][0]["c_kpa"] == 0
    with pytest.raises(ValueError, match="bulk_weight_n_m3"):
        slope(SoilProfile([replace(profile.layers[0], bulk_weight_n_m3=None)]))


def test_shared_records_exported():
    assert all(hasattr(geo, name) for name in ("SoilProfile", "SoilLayer", "SoilType"))


@pytest.mark.parametrize("changes", [dict(kind="unknown"), dict(su_pa=0),
    dict(kind="sand", su_pa=None), dict(kind="sand", su_pa=None, friction_deg=15),
    dict(kind="sand", su_pa=None, friction_deg=30, cohesion_pa=1)])
def test_spudcan_unsupported_properties(profile, changes):
    with pytest.raises(ValueError):
        to_spudcan_layers(SoilProfile([replace(profile.layers[0], **changes)]))


@pytest.mark.parametrize("changes,field", [(dict(su_pa=None), "cohesion_pa"),
    (dict(su_pa=None, cohesion_pa=0), "friction_deg")])
def test_slope_requires_explicit_strength(profile, changes, field):
    with pytest.raises(ValueError, match=field):
        slope(SoilProfile([replace(profile.layers[0], **changes)]))


@pytest.mark.parametrize("kwargs", [dict(water_unit_weight_n_m3=True), dict(x_extent_m=(0, 0)),
    dict(mudline_elevation_m=True), dict(su_gradient_pa_per_m=1), dict(top_boundary_m=[[0, 0], [30, -1]])])
def test_slope_rejects_invalid_or_unsupported_geometry(profile, kwargs):
    with pytest.raises((ValueError, TypeError)):
        slope(profile, **kwargs)


@pytest.mark.parametrize("data", [None, object()])
def test_missing_or_wrong_cpt_is_not_inferred(profile, data):
    with pytest.raises(ValueError, match="cpt"):
        to_cpt(profile)
    with pytest.raises(ValueError, match="CPTData"):
        from_cpt(data, profile.layers)
