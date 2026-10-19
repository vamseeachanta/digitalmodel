"""Engine import tests using dependency doubles scoped to each test."""

import pytest

from tests.engine_test_support import AttributeDict
from tests.engine_test_support import engine_test_doubles as engine_test_doubles

pytestmark = pytest.mark.usefixtures("engine_test_doubles")


def test_engine_import_and_basic_functionality(engine_test_doubles):
    cfg = AttributeDict(basename="transformation", inputs={"test": "value"})
    handler = engine_test_doubles.Transformation.return_value.router
    handler.return_value = cfg
    assert engine_test_doubles.engine(cfg=cfg, config_flag=False) is cfg
    handler.assert_called_once_with(cfg)


def test_engine_basename_extraction(engine_test_doubles):
    with pytest.raises(ValueError, match="basename not found in cfg"):
        engine_test_doubles.engine(cfg={"inputs": {"test": "value"}}, config_flag=False)


def test_engine_unsupported_basename(engine_test_doubles):
    with pytest.raises(
        Exception, match="Analysis for basename: unsupported_module not found"
    ):
        engine_test_doubles.engine(
            cfg={"basename": "unsupported_module"}, config_flag=False
        )


@pytest.mark.parametrize(
    "basename,handler,method",
    [
        ("orcaflex", "OrcaFlex", "router"),
        ("aqwa", "Aqwa", "router"),
        ("modal_analysis", "OrcModalAnalysis", "run_modal_analysis"),
        ("fatigue_analysis", "FatigueAnalysis", "router"),
        ("pipeline", "Pipeline", "router"),
    ],
)
def test_engine_coverage_analysis(engine_test_doubles, basename, handler, method):
    """Named routes must return the handler result, without suppressing errors."""
    cfg = {"basename": basename}
    router = getattr(getattr(engine_test_doubles, handler).return_value, method)
    router.return_value = cfg
    assert engine_test_doubles.engine(cfg=cfg, config_flag=False) is cfg
    router.assert_called_once_with(cfg)
