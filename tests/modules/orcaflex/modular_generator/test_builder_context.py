"""Tests for BuilderContext typed dataclass."""

from __future__ import annotations


class TestBuilderContext:
    """Test the typed BuilderContext dataclass."""

    def test_default_values(self, builder_context):
        assert builder_context.coating_names == []
        assert builder_context.line_type_names == []
        assert builder_context.buoy_names_6d == []
        assert builder_context.end_buoy_name == "6D buoy1"
        assert builder_context.bm_buoy_name == "BM"
        assert builder_context.main_pipeline_name == ""

    def test_set_coating_names(self, builder_context):
        builder_context.coating_names = ["coating+CWC120", "coating"]
        assert builder_context.coating_names == ["coating+CWC120", "coating"]

    def test_set_line_names(self, builder_context):
        builder_context.line_names = ["30'' Line"]
        builder_context.main_pipeline_name = "30'' Line"
        assert builder_context.line_names == ["30'' Line"]
        assert builder_context.main_pipeline_name == "30'' Line"

    def test_to_dict_only_non_defaults(self, builder_context):
        builder_context.coating_names = ["coating+CWC120"]
        d = builder_context.to_dict()
        assert "coating_names" in d
        assert d["coating_names"] == ["coating+CWC120"]
        # Default values should not be in dict
        assert "line_type_names" not in d

    def test_update_from_dict(self, builder_context):
        d = {
            "coating_names": ["c1", "c2"],
            "line_type_names": ["lt1"],
            "unknown_key": "ignored",
        }
        builder_context.update_from_dict(d)
        assert builder_context.coating_names == ["c1", "c2"]
        assert builder_context.line_type_names == ["lt1"]

    def test_multiple_builders_share_context(self):
        from digitalmodel.modules.orcaflex.modular_generator.builders.context import (
            BuilderContext,
        )

        ctx = BuilderContext()
        # Simulate VarDataBuilder
        ctx.coating_names = ["coating+CWC120", "coating"]
        # Simulate LineTypeBuilder
        ctx.line_type_names = ["X65+coating+CWC120", "Winch wire_LT"]
        # Simulate BuoysBuilder
        ctx.buoy_names_6d = ["Rollers", "Tug1", "BM", "6D buoy1"]
        ctx.buoy_names_3d = ["Mid-pipe"]
        ctx.end_buoy_name = "6D buoy1"

        # GroupsBuilder can now read all
        assert len(ctx.buoy_names_6d) == 4
        assert ctx.end_buoy_name == "6D buoy1"
        assert len(ctx.coating_names) == 2
