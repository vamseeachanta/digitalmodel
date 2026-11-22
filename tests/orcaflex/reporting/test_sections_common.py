"""Tests for the shared helpers of the OrcaFlex report sections.

ABOUTME: ``digitalmodel.orcaflex.reporting.sections._common`` is the single
place where the sections select model objects by OrcFxAPI type and render the
shared HTML fragments (key/value table, Plotly chart). Uses the fake OrcFxAPI
from ``tests/orcaflex/conftest.py``; no licensed solver.
"""
from __future__ import annotations

import json
import sys
import types

import pytest

from digitalmodel.orcaflex.reporting.sections import _common


class TestObjectSelection:
    def test_lines_in_model_order(self, fake_orcfxapi):
        api = fake_orcfxapi
        a, b = api.Line("A"), api.Line("B")
        model = types.SimpleNamespace(objects=[api.Vessel("V"), a, object(), b])
        assert _common.lines(model) == [a, b]

    def test_vessels(self, fake_orcfxapi):
        api = fake_orcfxapi
        v1, v2 = api.Vessel("V1"), api.Vessel("V2")
        model = types.SimpleNamespace(objects=[v1, api.Line("L"), v2])
        assert _common.vessels(model) == [v1, v2]

    def test_objects_of_type_by_orcfxapi_name(self, fake_orcfxapi):
        api = fake_orcfxapi
        line = api.Line("L")
        model = types.SimpleNamespace(objects=[line, api.Vessel("V")])
        assert _common.objects_of_type(model, "Line") == [line]

    def test_unknown_type_name_raises_attribute_error(self, fake_orcfxapi):
        model = types.SimpleNamespace(objects=[])
        with pytest.raises(AttributeError):
            _common.objects_of_type(model, "NoSuchType")

    def test_buoys_filtered_by_class_name_kind(self, fake_orcfxapi):
        api = fake_orcfxapi
        b3 = type("Buoy3DThing", (api.Buoy,), {})()
        b6 = type("Buoy6DThing", (api.Buoy,), {})()
        plain = type("PlainBuoy", (api.Buoy,), {})()
        model = types.SimpleNamespace(objects=[b6, plain, b3, api.Line("L")])
        assert _common.buoys(model, "3D") == [b3]
        assert _common.buoys(model, "6D") == [b6]

    def test_orcfxapi_is_imported_at_call_time(self, monkeypatch):
        monkeypatch.setitem(sys.modules, "OrcFxAPI", None)
        with pytest.raises(ImportError):
            _common.lines(types.SimpleNamespace(objects=[]))


class TestHtmlFragments:
    def test_kv_table(self):
        assert _common.kv_table([("Depth (m)", "100.0")]) == (
            '<table class="ofx">\n'
            "    <tr><td>Depth (m)</td><td><strong>100.0</strong></td></tr>\n"
            "</table>"
        )

    def test_attr_text(self):
        obj = types.SimpleNamespace(n=3)
        assert _common.attr_text(obj, "n") == "3"
        assert _common.attr_text(obj, "missing") == "N/A"
        assert _common.attr_text(obj, "missing", default="?") == "?"

    def test_attr_text_swallows_property_errors(self):
        class Boom:
            @property
            def value(self):
                raise RuntimeError("no licence")

        assert _common.attr_text(Boom(), "value") == "N/A"

    def test_plotly_chart(self):
        traces = [{"x": [0], "y": [1]}]
        layout = {"height": 250}
        assert _common.plotly_chart("c_0", traces, layout) == (
            '<div id="c_0" style="width:100%;margin-bottom:1rem;"></div>\n'
            '<script>Plotly.newPlot("c_0", '
            f"{json.dumps(traces)}, {json.dumps(layout)}, "
            "{responsive:true, displayModeBar:false});</script>"
        )


class TestDeprecatedAliases:
    """Old private helper names resolve to the shared helpers, with a warning."""

    @pytest.mark.parametrize(
        "module_name, alias, target",
        [
            ("model_summary", "_render_table", "kv_table"),
            ("model_summary", "_safe_get", "attr_text"),
            ("static_config", "_render_table", "kv_table"),
        ],
    )
    def test_alias_warns_and_returns_shared_helper(self, module_name, alias, target):
        import importlib

        module = importlib.import_module(
            f"digitalmodel.orcaflex.reporting.sections.{module_name}"
        )
        with pytest.warns(DeprecationWarning, match=target):
            resolved = getattr(module, alias)
        assert resolved is getattr(_common, target)

    def test_unknown_attribute_still_raises(self):
        from digitalmodel.orcaflex.reporting.sections import model_summary

        with pytest.raises(AttributeError):
            model_summary.no_such_name  # noqa: B018
