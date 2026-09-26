from pathlib import Path

from digitalmodel.asset_integrity.common.ymlInput import ymlInput


def test_fracture_mechanics_fixture_is_loadable():
    ymlfile = (
        Path(__file__).parent
        / "test_data"
        / "fracture_mechanics"
        / "fracture_mechanics_py_ecs_2500ft_buoy_jt.yml"
    )
    cfg = ymlInput(str(ymlfile), updateYml=None)
    assert isinstance(cfg, dict)
    # This is an input-override fixture (the engine merges it over the
    # module's base config), so it carries only the fields the legacy
    # fracture-mechanics engine iterates over.  Assert those (#2160).
    assert {"default", "loading", "Outer_Pipe"} <= set(cfg)
    settings = cfg["default"]["settings"]
    assert set(settings["location_array"]) <= {
        "internal_surface", "external_surface", "embedded"
    }
    assert set(settings["orientation_array"]) <= {"axial", "circumferential"}
    assert settings["c_array"] == sorted(settings["c_array"])
    assert all(c > 0 for c in settings["c_array"])
    assert settings["factor_of_safety"] >= 1
    assert cfg["loading"]["primary_membrane_stress"]["value"] > 0
    geometry = cfg["Outer_Pipe"]["Geometry"]
    assert 0 < geometry["Design_WT"] < geometry["Nominal_OD"]
