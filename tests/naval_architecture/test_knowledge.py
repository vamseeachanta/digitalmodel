# ABOUTME: Tests for knowledge query interface
# ABOUTME: Module routing, ship data search, worked example lookup
"""Tests for knowledge module — naval architecture knowledge query."""

import json
import os
import tempfile

import pytest


class TestModuleRouting:
    """Route topic keywords to calculation modules."""

    def test_stability_routes_to_stability(self):
        from digitalmodel.naval_architecture.knowledge import find_module

        assert find_module("stability analysis") == "stability"

    def test_holtrop_routes_to_holtrop(self):
        from digitalmodel.naval_architecture.knowledge import find_module

        assert find_module("holtrop resistance") == "holtrop_mennen"

    def test_imo_routes_to_compliance(self):
        from digitalmodel.naval_architecture.knowledge import find_module

        assert find_module("IMO criteria") == "compliance"

    def test_unknown_returns_none(self):
        from digitalmodel.naval_architecture.knowledge import find_module

        assert find_module("random unrelated topic") is None

    def test_rudder_routes_to_maneuverability(self):
        from digitalmodel.naval_architecture.knowledge import find_module

        assert find_module("rudder force") == "maneuverability"


class TestShipDataSearch:
    """Search ship registry."""

    def test_find_ddg51(self):
        from digitalmodel.naval_architecture.knowledge import (
            find_ship_data,
        )

        results = find_ship_data("DDG-51")
        assert len(results) == 1
        assert results[0]["hull_id"] == "DDG-51"
        assert results[0]["has_cross_curves"] is True

    def test_find_by_class_name(self):
        from digitalmodel.naval_architecture.knowledge import (
            find_ship_data,
        )

        results = find_ship_data("Burke")
        assert len(results) == 1

    def test_no_match(self):
        from digitalmodel.naval_architecture.knowledge import (
            find_ship_data,
        )

        results = find_ship_data("Bismarck")
        assert len(results) == 0


class TestKnowledgeQuery:
    """Full knowledge query integration."""

    def test_query_with_vessel(self):
        from digitalmodel.naval_architecture.knowledge import (
            query_knowledge,
        )

        result = query_knowledge("stability", vessel="DDG-51")
        assert result["module"] == "stability"
        assert len(result["ship_data"]) == 1

    def test_query_with_examples(self):
        from digitalmodel.naval_architecture.knowledge import (
            query_knowledge,
        )

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".jsonl", delete=False
        ) as f:
            f.write(json.dumps({
                "title": "GZ from cross curves",
                "expected_value": 0.94,
            }) + "\n")
            f.write(json.dumps({
                "title": "Reynolds number calculation",
                "expected_value": 8.91e8,
            }) + "\n")
            path = f.name

        try:
            result = query_knowledge("cross curves", jsonl_path=path)
            assert len(result["worked_examples"]) == 1
            assert result["worked_examples"][0]["title"] == "GZ from cross curves"
        finally:
            os.unlink(path)

    def test_query_suggests_module(self):
        from digitalmodel.naval_architecture.knowledge import (
            query_knowledge,
        )

        result = query_knowledge("hull form design")
        assert any("hull_form" in s for s in result["suggestions"])
