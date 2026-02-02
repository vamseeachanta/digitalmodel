"""Tests for S-lay pipeline installation builders."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.orcaflex.modular_generator.builders.vessel_type_builder import VesselTypeBuilder
from digitalmodel.orcaflex.modular_generator.builders.vessel_builder import VesselBuilder
from digitalmodel.orcaflex.modular_generator.builders.winch_builder import WinchBuilder
from digitalmodel.orcaflex.modular_generator.builders.buoys_builder import BuoysBuilder
from digitalmodel.orcaflex.modular_generator.builders.lines_builder import LinesBuilder


SLAY_SPEC_DIR = Path(__file__).parent.parent.parent.parent.parent / (
    "docs/modules/orcaflex/pipeline/installation/s-lay"
)
FLOATING_SPEC_FILE = Path(__file__).parent.parent.parent.parent.parent / (
    "docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml"
)


@pytest.fixture
def slay_spec():
    """Load and validate the SB-SA S-lay spec."""
    spec_file = SLAY_SPEC_DIR / "SB-SA" / "spec.yml"
    with open(spec_file) as f:
        data = yaml.safe_load(f)
    return ProjectInputSpec(**data)


@pytest.fixture
def floating_spec():
    """Load and validate the floating 30in spec."""
    with open(FLOATING_SPEC_FILE) as f:
        data = yaml.safe_load(f)
    return ProjectInputSpec(**data)


class TestShouldGenerate:
    """Tests for should_generate() on S-lay vs floating models."""

    def test_vessel_type_builder_generates_for_slay(self, slay_spec):
        ctx = BuilderContext()
        builder = VesselTypeBuilder(slay_spec, ctx)
        assert builder.should_generate() is True

    def test_vessel_type_builder_skips_for_floating(self, floating_spec):
        ctx = BuilderContext()
        builder = VesselTypeBuilder(floating_spec, ctx)
        assert builder.should_generate() is False

    def test_vessel_builder_generates_for_slay(self, slay_spec):
        ctx = BuilderContext()
        builder = VesselBuilder(slay_spec, ctx)
        assert builder.should_generate() is True

    def test_vessel_builder_skips_for_floating(self, floating_spec):
        ctx = BuilderContext()
        builder = VesselBuilder(floating_spec, ctx)
        assert builder.should_generate() is False

    def test_winch_builder_generates_for_slay(self, slay_spec):
        ctx = BuilderContext()
        builder = WinchBuilder(slay_spec, ctx)
        assert builder.should_generate() is True

    def test_winch_builder_skips_for_floating(self, floating_spec):
        ctx = BuilderContext()
        builder = WinchBuilder(floating_spec, ctx)
        assert builder.should_generate() is False

    def test_buoys_builder_skips_for_slay(self, slay_spec):
        ctx = BuilderContext()
        builder = BuoysBuilder(slay_spec, ctx)
        assert builder.should_generate() is False

    def test_buoys_builder_generates_for_floating(self, floating_spec):
        ctx = BuilderContext()
        builder = BuoysBuilder(floating_spec, ctx)
        assert builder.should_generate() is True


class TestVesselTypeBuilder:
    """Tests for VesselTypeBuilder output."""

    def test_output_has_vessel_types_key(self, slay_spec):
        ctx = BuilderContext()
        builder = VesselTypeBuilder(slay_spec, ctx)
        result = builder.build()
        assert "VesselTypes" in result

    def test_vessel_type_name(self, slay_spec):
        ctx = BuilderContext()
        builder = VesselTypeBuilder(slay_spec, ctx)
        result = builder.build()
        vt = result["VesselTypes"][0]
        assert vt["Name"] == "Eclipse Type"

    def test_vessel_type_length(self, slay_spec):
        ctx = BuilderContext()
        builder = VesselTypeBuilder(slay_spec, ctx)
        result = builder.build()
        vt = result["VesselTypes"][0]
        assert vt["Length"] == 130

    def test_registers_vessel_type_name(self, slay_spec):
        ctx = BuilderContext()
        builder = VesselTypeBuilder(slay_spec, ctx)
        builder.build()
        assert ctx.vessel_type_names == ["Eclipse Type"]


class TestVesselBuilder:
    """Tests for VesselBuilder output."""

    def test_output_has_vessels_key(self, slay_spec):
        ctx = BuilderContext()
        ctx.vessel_type_names = ["Eclipse Type"]
        builder = VesselBuilder(slay_spec, ctx)
        result = builder.build()
        assert "Vessels" in result

    def test_vessel_name(self, slay_spec):
        ctx = BuilderContext()
        ctx.vessel_type_names = ["Eclipse Type"]
        builder = VesselBuilder(slay_spec, ctx)
        result = builder.build()
        vessel = result["Vessels"][0]
        assert vessel["Name"] == "Eclipse"

    def test_vessel_type_reference(self, slay_spec):
        ctx = BuilderContext()
        ctx.vessel_type_names = ["Eclipse Type"]
        builder = VesselBuilder(slay_spec, ctx)
        result = builder.build()
        vessel = result["Vessels"][0]
        assert vessel["VesselType"] == "Eclipse Type"

    def test_registers_vessel_name(self, slay_spec):
        ctx = BuilderContext()
        ctx.vessel_type_names = ["Eclipse Type"]
        builder = VesselBuilder(slay_spec, ctx)
        builder.build()
        assert ctx.vessel_names == ["Eclipse"]
        assert ctx.main_vessel_name == "Eclipse"

    def test_stinger_support_path(self, slay_spec):
        ctx = BuilderContext()
        ctx.vessel_type_names = ["Eclipse Type"]
        builder = VesselBuilder(slay_spec, ctx)
        result = builder.build()
        vessel = result["Vessels"][0]
        assert "SupportGeometrySpecification" in vessel


class TestWinchBuilder:
    """Tests for WinchBuilder output."""

    def test_output_has_winches_key(self, slay_spec):
        ctx = BuilderContext()
        ctx.main_pipeline_name = "SB-SA Pipeline"
        builder = WinchBuilder(slay_spec, ctx)
        result = builder.build()
        assert "Winches" in result

    def test_winch_tension_value(self, slay_spec):
        ctx = BuilderContext()
        ctx.main_pipeline_name = "SB-SA Pipeline"
        builder = WinchBuilder(slay_spec, ctx)
        result = builder.build()
        winch = result["Winches"][0]
        assert winch["StageTension"][1] == 585.4

    def test_winch_connects_vessel_to_pipeline(self, slay_spec):
        ctx = BuilderContext()
        ctx.main_pipeline_name = "SB-SA Pipeline"
        builder = WinchBuilder(slay_spec, ctx)
        result = builder.build()
        winch = result["Winches"][0]
        assert winch["Connection"] == ["Eclipse", "SB-SA Pipeline"]

    def test_registers_winch_name(self, slay_spec):
        ctx = BuilderContext()
        ctx.main_pipeline_name = "SB-SA Pipeline"
        builder = WinchBuilder(slay_spec, ctx)
        builder.build()
        assert ctx.winch_names == ["Tensioner"]


class TestLinesBuilderSlay:
    """Tests for LinesBuilder S-lay vs floating connection logic."""

    def test_slay_end_a_connects_to_vessel(self, slay_spec):
        ctx = BuilderContext()
        builder = LinesBuilder(slay_spec, ctx)
        result = builder.build()
        line = result["Lines"][0]
        # Get connection data - first entry is End A
        conn_key = (
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo"
        )
        connections = line[conn_key]
        end_a = connections[0]
        assert end_a[0] == "Eclipse"  # Connected to vessel

    def test_slay_end_b_is_anchored(self, slay_spec):
        ctx = BuilderContext()
        builder = LinesBuilder(slay_spec, ctx)
        result = builder.build()
        line = result["Lines"][0]
        conn_key = (
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo"
        )
        connections = line[conn_key]
        end_b = connections[1]
        assert end_b[0] == "Anchored"

    def test_floating_end_a_is_fixed(self, floating_spec):
        ctx = BuilderContext()
        builder = LinesBuilder(floating_spec, ctx)
        result = builder.build()
        line = result["Lines"][0]
        conn_key = (
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo"
        )
        connections = line[conn_key]
        end_a = connections[0]
        assert end_a[0] == "Fixed"

    def test_floating_end_b_is_buoy(self, floating_spec):
        ctx = BuilderContext()
        builder = LinesBuilder(floating_spec, ctx)
        result = builder.build()
        line = result["Lines"][0]
        conn_key = (
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo"
        )
        connections = line[conn_key]
        end_b = connections[1]
        assert end_b[0] == "6D buoy1"


class TestSlayEndToEnd:
    """End-to-end test: generate S-lay model from spec."""

    def test_generate_slay_model(self, slay_spec, tmp_path):
        from digitalmodel.orcaflex.modular_generator import ModularModelGenerator

        spec_file = SLAY_SPEC_DIR / "SB-SA" / "spec.yml"
        generator = ModularModelGenerator(spec_file)
        output_dir = tmp_path / "slay_output"
        generator.generate(output_dir)

        includes_dir = output_dir / "includes"
        assert includes_dir.exists()

        # S-lay specific files should exist
        assert (includes_dir / "04_vessel_types.yml").exists()
        assert (includes_dir / "06_vessels.yml").exists()
        assert (includes_dir / "11_winches.yml").exists()

        # Buoys file should NOT exist (skipped for S-lay)
        assert not (includes_dir / "08_buoys.yml").exists()

    def test_floating_model_unchanged(self, floating_spec, tmp_path):
        """Floating model generation should be unaffected by S-lay changes."""
        from digitalmodel.orcaflex.modular_generator import ModularModelGenerator

        generator = ModularModelGenerator(FLOATING_SPEC_FILE)
        output_dir = tmp_path / "floating_output"
        generator.generate(output_dir)

        includes_dir = output_dir / "includes"

        # Floating specific files should exist
        assert (includes_dir / "08_buoys.yml").exists()

        # S-lay specific files should NOT exist
        assert not (includes_dir / "04_vessel_types.yml").exists()
        assert not (includes_dir / "06_vessels.yml").exists()
        assert not (includes_dir / "11_winches.yml").exists()
