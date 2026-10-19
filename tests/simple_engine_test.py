"""Engine routing smoke tests with per-test dependency doubles."""

from unittest.mock import patch

import pytest

from tests.engine_test_support import AttributeDict
from tests.engine_test_support import engine_test_doubles as engine_test_doubles

pytestmark = pytest.mark.usefixtures("engine_test_doubles")


def test_engine_basic(engine_test_doubles):
    """The transformation router's exact return value must be returned."""
    cfg = AttributeDict(basename="transformation")
    engine_test_doubles.Transformation.return_value.router.return_value = cfg
    assert engine_test_doubles.engine(cfg=cfg, config_flag=False) is cfg
    engine_test_doubles.app_manager.save_cfg.assert_called_once_with(cfg_base=cfg)


def test_engine_error_case(engine_test_doubles):
    with pytest.raises(
        Exception, match="Analysis for basename: unsupported_module not found"
    ):
        engine_test_doubles.engine(
            cfg={"basename": "unsupported_module"}, config_flag=False
        )


def test_engine_missing_basename(engine_test_doubles):
    with pytest.raises(ValueError, match="basename not found"):
        engine_test_doubles.engine(cfg={"inputs": {}}, config_flag=False)


@pytest.mark.parametrize(
    "basename,handler,method",
    [
        ("orcaflex", "OrcaFlex", "router"),
        ("aqwa", "Aqwa", "router"),
        ("modal_analysis", "OrcModalAnalysis", "run_modal_analysis"),
        ("fatigue_analysis", "FatigueAnalysis", "router"),
        ("pipeline", "Pipeline", "router"),
        ("transformation", "Transformation", "router"),
        ("plate_buckling", "PlateBuckling", "router"),
        ("viv_analysis", "VIVAnalysis", "router"),
        ("time_series", "TimeSeriesAnalysis", "router"),
        ("pipe_capacity", "PipeCapacity", "router"),
        ("ship_design", "ShipDesign", "router"),
    ],
)
def test_engine_all_basenames(engine_test_doubles, basename, handler, method):
    """Routing expectations follow the named branches in engine.py."""
    cfg = {"basename": basename}
    target = getattr(engine_test_doubles, handler)
    router = getattr(target.return_value, method)
    router.return_value = cfg
    assert engine_test_doubles.engine(cfg=cfg, config_flag=False) is cfg
    target.assert_called_once_with()
    router.assert_called_once_with(cfg)


def test_cathodic_protection_adapter(engine_test_doubles):
    cfg = {"basename": "cathodic_protection"}
    engine_test_doubles.run_cathodic_protection.return_value = cfg
    assert engine_test_doubles.engine(cfg=cfg, config_flag=False) is cfg
    engine_test_doubles.run_cathodic_protection.assert_called_once_with(cfg)


def test_mooring_requires_direct_api(engine_test_doubles):
    with pytest.raises(NotImplementedError, match="Mooring via engine requires"):
        engine_test_doubles.engine(cfg={"basename": "mooring"}, config_flag=False)


@pytest.mark.parametrize("shape", ["rectangular", "circular"])
def test_code_dnvrph103_shapes(engine_test_doubles, shape):
    cfg = {"basename": "code_dnvrph103", "inputs": {"shape": shape}}
    handler = getattr(engine_test_doubles, f"DNVRPH103_hydrodynamics_{shape}")
    handler.return_value.get_orcaflex_6dbuoy.return_value = cfg
    assert engine_test_doubles.engine(cfg=cfg, config_flag=False) is cfg
    handler.return_value.get_orcaflex_6dbuoy.assert_called_once_with(cfg)


@pytest.mark.parametrize("flag", [True, False])
def test_installation_with_flags(engine_test_doubles, flag):
    cfg = {"basename": "installation", "structure": {"flag": flag}}
    with patch.object(engine_test_doubles, "OrcInstallation") as handler:
        handler.return_value.create_model_for_water_depth.return_value = cfg
        assert engine_test_doubles.engine(cfg=cfg, config_flag=False) is cfg
        if flag:
            handler.return_value.create_model_for_water_depth.assert_called_once_with(
                cfg
            )
        else:
            handler.return_value.create_model_for_water_depth.assert_not_called()
