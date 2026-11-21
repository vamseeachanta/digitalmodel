"""Characterisation of the OrcaFlex report sections that query model objects.

ABOUTME: Pins the exact HTML that the model summary, static configuration,
time series, range graph and mooring load sections produce, and the full
report built from them, on the fake OrcFxAPI objects in ``tests/orcaflex/
conftest.py``. No licensed solver is used. The expected strings live in
``tests/orcaflex/fixtures/reporting_sections_golden.json`` and were captured
from the section code at the base commit before the shared helpers were
introduced; a refactor of those sections must leave every string unchanged.
"""
from __future__ import annotations

import json
import types
from pathlib import Path

import pytest

from digitalmodel.orcaflex.reporting import sections
from digitalmodel.orcaflex.reporting.builder import OrcaFlexReportBuilder
from digitalmodel.orcaflex.reporting.config import ReportConfig

GOLDEN_PATH = Path(__file__).resolve().parents[1] / "fixtures" / "reporting_sections_golden.json"

SECTION_BUILDERS = {
    "model_summary": sections.build_model_summary,
    "static_config": sections.build_static_config,
    "time_series": sections.build_time_series,
    "range_graphs": sections.build_range_graphs,
    "mooring_loads": sections.build_mooring_loads,
}


# ---------------------------------------------------------------------------
# Model variants built from the fake OrcFxAPI namespace
# ---------------------------------------------------------------------------

def _default_model(api):
    return api.Model()


def _empty_model(api):
    model = api.Model()
    model.objects = []
    return model


def _crowded_model(api):
    """More objects than the per-section caps, interleaved with non-matches."""
    model = api.Model()
    buoy_3d = type("Buoy3DType", (api.Buoy,), {})
    buoy_6d = type("Buoy6DType", (api.Buoy,), {})
    plain_buoy = type("PlainBuoy", (api.Buoy,), {})
    model.objects = [
        api.Vessel("V1"),
        api.Line("L1"),
        object(),
        api.Line("L2"),
        buoy_3d(),
        api.Vessel("V2"),
        api.Line("L3"),
        buoy_6d(),
        api.Line("L4"),
        plain_buoy(),
        api.Vessel("V3"),
        api.Line("L5"),
        buoy_3d(),
    ]
    return model


def _raising_model(api):
    """Lines and vessels whose result calls raise."""

    class RaisingLine(api.Line):
        def StaticResult(self, *args):
            raise RuntimeError("static unavailable")

        def TimeHistory(self, *args):
            raise RuntimeError("history unavailable")

        def RangeGraph(self, *args):
            raise RuntimeError("range unavailable")

    class DynamicFailLine(api.Line):
        def TimeHistory(self, *args):
            raise RuntimeError("history unavailable")

    class RaisingVessel(api.Vessel):
        def StaticResult(self, *args):
            raise RuntimeError("vessel static unavailable")

        def TimeHistory(self, *args):
            raise RuntimeError("vessel history unavailable")

    model = api.Model()
    model.objects = [RaisingLine("Bad1"), DynamicFailLine("Dyn1"), RaisingVessel("BadV")]
    return model


def _sparse_results_model(api):
    """Empty range-graph envelopes, no mean, mismatched time histories."""

    class NoMeanRange:
        X = [0.0, 10.0]
        Min = [1.0, 2.0]
        Max = [3.0, 4.0]

    class EmptyRange:
        X = [0.0]
        Min = []
        Max = [1.0]

    class SparseLine(api.Line):
        def RangeGraph(self, variable, *args):
            return NoMeanRange() if variable == "Effective Tension" else EmptyRange()

        def TimeHistory(self, *args):
            return [1.0, 2.0]  # shorter than SampleTimes(): skipped

    class ShortVessel(api.Vessel):
        def TimeHistory(self, dof, *args):
            return [0.0] * (5 if dof == "Z" else 3)

    model = api.Model()
    model.objects = [SparseLine("Sparse"), ShortVessel("Short")]
    model.general = types.SimpleNamespace(OrcaFlexVersion="11.5")  # others absent
    model.environment = types.SimpleNamespace()
    del model.simulationFileName
    return model


MODEL_VARIANTS = {
    "default": _default_model,
    "empty": _empty_model,
    "crowded": _crowded_model,
    "raising": _raising_model,
    "sparse": _sparse_results_model,
}


def render_section_outputs(api) -> dict[str, str]:
    """Render every pinned section for every model variant.

    Keys are ``"<variant>/<section>"``. A section that raises is recorded as
    ``"RAISES <ExceptionType>: <message>"`` so that the failure itself is pinned.
    """
    cfg = ReportConfig()
    outputs: dict[str, str] = {}
    for variant, make_model in MODEL_VARIANTS.items():
        for section, build in SECTION_BUILDERS.items():
            model = make_model(api)
            try:
                outputs[f"{variant}/{section}"] = build(model, getattr(cfg, section))
            except Exception as exc:  # pinned, not swallowed
                outputs[f"{variant}/{section}"] = f"RAISES {type(exc).__name__}: {exc}"
    return outputs


def render_full_report(api) -> str:
    """Full report on the default fake model, QA section disabled.

    The QA section reads JSON files from a directory resolved relative to the
    installed source file, so its output depends on the checkout location; it
    is disabled here to keep the pinned string location-independent.
    """
    cfg = ReportConfig()
    cfg.qa_summary.enabled = False
    return OrcaFlexReportBuilder(sim_path=Path("pinned_case.sim"), config=cfg).build()


@pytest.fixture(scope="module")
def golden() -> dict[str, str]:
    return json.loads(GOLDEN_PATH.read_text(encoding="utf-8"))


def test_golden_covers_every_variant_and_section(golden):
    expected_keys = {f"{v}/{s}" for v in MODEL_VARIANTS for s in SECTION_BUILDERS}
    expected_keys |= {"full_report"}
    assert set(golden) == expected_keys


@pytest.mark.parametrize("variant", sorted(MODEL_VARIANTS))
@pytest.mark.parametrize("section", sorted(SECTION_BUILDERS))
def test_section_html_is_unchanged(fake_orcfxapi, golden, variant, section):
    outputs = render_section_outputs(fake_orcfxapi)
    assert outputs[f"{variant}/{section}"] == golden[f"{variant}/{section}"]


def test_full_report_html_is_unchanged(fake_orcfxapi, golden):
    assert render_full_report(fake_orcfxapi) == golden["full_report"]


def test_sections_raise_import_error_without_orcfxapi(monkeypatch):
    """Without OrcFxAPI the object-querying sections raise ImportError."""
    import sys

    monkeypatch.setitem(sys.modules, "OrcFxAPI", None)
    model = types.SimpleNamespace(objects=[])
    for build in SECTION_BUILDERS.values():
        with pytest.raises(ImportError):
            build(model, None)


class TestLegacyPrivateHelpers:
    """The module-private helpers stay importable from their old modules."""

    def test_model_summary_render_table(self):
        from digitalmodel.orcaflex.reporting.sections.model_summary import _render_table

        assert _render_table([("A", "1"), ("B", "<b>")]) == (
            '<table class="ofx">\n'
            "    <tr><td>A</td><td><strong>1</strong></td></tr>\n"
            "    <tr><td>B</td><td><strong><b></strong></td></tr>\n"
            "</table>"
        )

    def test_static_config_render_table(self):
        from digitalmodel.orcaflex.reporting.sections.static_config import _render_table

        assert _render_table([]) == '<table class="ofx">\n\n</table>'

    def test_model_summary_safe_get(self):
        from digitalmodel.orcaflex.reporting.sections.model_summary import _safe_get

        obj = types.SimpleNamespace(value=2.5)
        assert _safe_get(obj, "value") == "2.5"
        assert _safe_get(obj, "missing") == "N/A"
        assert _safe_get(obj, "missing", default="-") == "-"
