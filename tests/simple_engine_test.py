"""Engine routing smoke tests with scoped, explicitly configured handlers."""

import pytest

from tests.engine_test_support import MockAttributeDict, engine
from tests.engine_test_support import mocked_engine as mocked_engine


pytestmark = pytest.mark.usefixtures("mocked_engine")


def test_engine_basic(mocked_engine):
    """The configured handler returns the original configuration object."""
    cfg = MockAttributeDict({"basename": "transformation"})
    router = mocked_engine.Transformation.return_value.router
    router.return_value = cfg

    assert engine(cfg=cfg, config_flag=False) is cfg
    router.assert_called_once_with(cfg)
    mocked_engine.app_manager.save_cfg.assert_called_once_with(cfg_base=cfg)


def test_engine_error_case():
    """Unsupported routes retain the engine's error contract."""
    with pytest.raises(
        Exception, match="Analysis for basename: unsupported_module not found"
    ):
        engine(cfg={"basename": "unsupported_module"}, config_flag=False)


def test_engine_missing_basename():
    """A configuration without a route fails before dispatch."""
    with pytest.raises(ValueError, match="basename not found"):
        engine(cfg={"inputs": {"test": "value"}}, config_flag=False)


@pytest.mark.parametrize(
    "basename,handler,method",
    [
        ("vertical_riser", "vertical_riser", None),
        ("orcaflex", "OrcaFlex", "router"),
        ("aqwa", "Aqwa", "router"),
        ("modal_analysis", "OrcModalAnalysis", "run_modal_analysis"),
        ("fatigue_analysis", "FatigueAnalysis", "router"),
        ("pipeline", "Pipeline", "router"),
        ("transformation", "Transformation", "router"),
        ("cathodic_protection", "run_cathodic_protection", None),
        ("plate_buckling", "PlateBuckling", "router"),
        ("viv_analysis", "VIVAnalysis", "router"),
        ("time_series", "TimeSeriesAnalysis", "router"),
        ("pipe_capacity", "PipeCapacity", "router"),
        ("ship_design", "ShipDesign", "router"),
    ],
)
def test_engine_all_basenames(basename, handler, method, mocked_engine):
    """Expected handler names are read from engine.py's dispatch branches."""
    cfg = MockAttributeDict({"basename": basename})
    target = getattr(mocked_engine, handler)
    call = getattr(target.return_value, method) if method else target
    call.return_value = cfg

    assert engine(cfg=cfg, config_flag=False) is cfg
    call.assert_called_once_with(cfg)


def test_engine_mooring_requires_cli():
    """The legacy mooring route explicitly directs callers to its CLI."""
    with pytest.raises(NotImplementedError, match="mooring_analysis CLI"):
        engine(cfg={"basename": "mooring"}, config_flag=False)


@pytest.mark.parametrize("shape", ["rectangular", "circular"])
def test_code_dnvrph103_shapes(shape, mocked_engine):
    """Check routing only; no hydrodynamic equation is evaluated."""
    cfg = MockAttributeDict({"basename": "code_dnvrph103", "inputs": {"shape": shape}})
    handler = getattr(mocked_engine, f"DNVRPH103_hydrodynamics_{shape}")
    call = handler.return_value.get_orcaflex_6dbuoy
    call.return_value = cfg

    assert engine(cfg=cfg, config_flag=False) is cfg
    call.assert_called_once_with(cfg)


@pytest.mark.parametrize("flag", [True, False])
def test_installation_with_flags(flag, mocked_engine):
    """The structure flag alone controls dispatch to the installation handler."""
    cfg = MockAttributeDict({"basename": "installation", "structure": {"flag": flag}})
    call = mocked_engine.OrcInstallation.return_value.create_model_for_water_depth
    call.return_value = cfg

    assert engine(cfg=cfg, config_flag=False) is cfg
    if flag:
        call.assert_called_once_with(cfg)
    else:
        call.assert_not_called()
