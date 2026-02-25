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
