"""Engine routing smoke tests with test-scoped dependency doubles."""

import pytest

from tests.engine_test_support import MockAttributeDict, engine
from tests.engine_test_support import mocked_engine as mocked_engine


pytestmark = pytest.mark.usefixtures("mocked_engine")


def test_engine_import_and_basic_functionality(mocked_engine):
    """The freshly loaded engine uses this test's configured handler."""
    cfg = MockAttributeDict({"basename": "transformation", "inputs": {"test": "value"}})
    router = mocked_engine.Transformation.return_value.router
    router.return_value = cfg

    assert engine(cfg=cfg, config_flag=False) is cfg
    router.assert_called_once_with(cfg)


def test_engine_basename_extraction():
    """Missing basename retains the production validation error."""
    with pytest.raises(ValueError, match="basename not found in cfg"):
        engine(cfg={"inputs": {"test": "value"}}, config_flag=False)


def test_engine_unsupported_basename():
    """Unsupported basename retains the production routing error."""
    with pytest.raises(
        Exception, match="Analysis for basename: unsupported_module not found"
    ):
        engine(cfg={"basename": "unsupported_module"}, config_flag=False)


@pytest.mark.parametrize(
    "basename,handler,method",
    [
        ("vertical_riser", "vertical_riser", None),
        ("orcaflex", "OrcaFlex", "router"),
        ("aqwa", "Aqwa", "router"),
        ("modal_analysis", "OrcModalAnalysis", "run_modal_analysis"),
        ("fatigue_analysis", "FatigueAnalysis", "router"),
        ("pipeline", "Pipeline", "router"),
    ],
)
def test_engine_coverage_analysis(basename, handler, method, mocked_engine):
    """Expected calls follow the dispatch table in engine.py; failures propagate."""
    cfg = MockAttributeDict({"basename": basename})
    target = getattr(mocked_engine, handler)
    call = getattr(target.return_value, method) if method else target
    call.return_value = cfg

    assert engine(cfg=cfg, config_flag=False) is cfg
    call.assert_called_once_with(cfg)


def test_engine_mooring_requires_cli():
    """The mooring route raises its documented CLI guidance."""
    with pytest.raises(NotImplementedError, match="mooring_analysis CLI"):
        engine(cfg={"basename": "mooring"}, config_flag=False)
