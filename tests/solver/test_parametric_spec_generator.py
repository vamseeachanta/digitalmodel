"""Tests for parametric spec.yml generator (TDD — tests written first).

The generator creates DiffractionSpec-compliant spec files from sweep
definitions, supporting frequency, heading, and hull-parameter sweeps.

Issue: #1596
"""
from __future__ import annotations

import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec


# ---------------------------------------------------------------------------
# Lazy import of the generator (implementation follows these tests)
# ---------------------------------------------------------------------------


def _get_generator():
    """Import the parametric generator module."""
    from digitalmodel.solvers.parametric_spec_generator import (
        ParametricSpecGenerator,
        FrequencySweep,
        HeadingSweep,
        HullParameterSweep,
    )
    return ParametricSpecGenerator, FrequencySweep, HeadingSweep, HullParameterSweep


# ---------------------------------------------------------------------------
# Base spec fixture (used by all tests)
# ---------------------------------------------------------------------------


@pytest.fixture
def base_spec_data():
    """Minimal valid DiffractionSpec data suitable for parametric sweeps."""
    return {
        "version": "1.0",
        "analysis_type": "diffraction",
        "vessel": {
            "name": "TestCylinder",
            "type": "cylinder",
            "geometry": {
                "mesh_file": "test_cylinder.gdf",
                "mesh_format": "gdf",
                "symmetry": "xz+yz",
                "reference_point": [0.0, 0.0, 0.0],
            },
            "inertia": {
                "mode": "free_floating",
                "mass": 1000.0,
                "centre_of_gravity": [0.0, 0.0, 0.0],
                "radii_of_gyration": [1.0, 1.0, 1.0],
            },
        },
        "environment": {
            "water_depth": "infinite",
            "water_density": 1025.0,
        },
        "frequencies": {
            "input_type": "frequency",
            "values": [0.1, 0.5, 1.0],
        },
        "wave_headings": {
            "values": [0.0, 180.0],
        },
    }


# ---------------------------------------------------------------------------
# Test 1: Single spec generation
# ---------------------------------------------------------------------------


class TestSingleSpecGeneration:
    """Generate one spec from known parameters — no sweep applied."""

    def test_single_spec_from_base(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        gen = PSG(base_spec_data)
        specs = gen.generate()
        assert len(specs) == 1
        assert isinstance(specs[0], DiffractionSpec)
        assert specs[0].vessel.name == "TestCylinder"

    def test_single_spec_validates_against_schema(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        gen = PSG(base_spec_data)
        specs = gen.generate()
        # If it's a DiffractionSpec instance, it already validated.
        spec = specs[0]
        assert spec.environment.water_density == 1025.0
        freqs = spec.frequencies.to_frequencies_rad_s()
        assert len(freqs) == 3

    def test_single_spec_to_yaml(self, base_spec_data, tmp_path):
        PSG, FS, HS, HPS = _get_generator()
        gen = PSG(base_spec_data)
        paths = gen.generate_to_yaml(tmp_path)
        assert len(paths) == 1
        assert paths[0].exists()
        # Parse back and validate
        loaded = DiffractionSpec.from_yaml(paths[0])
        assert loaded.vessel.name == "TestCylinder"


# ---------------------------------------------------------------------------
# Test 2: Frequency sweep
# ---------------------------------------------------------------------------


class TestFrequencySweep:
    """Parametric sweep over frequency ranges."""

    def test_frequency_sweep_generates_multiple_specs(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        sweep = FS(
            start=0.1,
            end=2.0,
            steps=3,
        )
        gen = PSG(base_spec_data, frequency_sweeps=[sweep])
        specs = gen.generate()
        assert len(specs) == 3

    def test_frequency_sweep_range_values(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        sweep = FS(start=0.5, end=1.5, steps=3)
        gen = PSG(base_spec_data, frequency_sweeps=[sweep])
        specs = gen.generate()
        # Each spec should have different frequency ranges
        freq_sets = [
            tuple(s.frequencies.to_frequencies_rad_s()) for s in specs
        ]
        # All should be different
        assert len(set(freq_sets)) == 3

    def test_frequency_sweep_schema_compliance(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        sweep = FS(start=0.1, end=3.0, steps=5)
        gen = PSG(base_spec_data, frequency_sweeps=[sweep])
        specs = gen.generate()
        for spec in specs:
            assert isinstance(spec, DiffractionSpec)
            freqs = spec.frequencies.to_frequencies_rad_s()
            assert all(f > 0 for f in freqs)


# ---------------------------------------------------------------------------
# Test 3: Heading sweep
# ---------------------------------------------------------------------------


class TestHeadingSweep:
    """Parametric sweep over wave headings."""

    def test_heading_sweep_generates_specs(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        sweep = HS(values=[[0.0], [0.0, 90.0], [0.0, 90.0, 180.0]])
        gen = PSG(base_spec_data, heading_sweeps=[sweep])
        specs = gen.generate()
        assert len(specs) == 3

    def test_heading_sweep_values_correct(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        sweep = HS(values=[[0.0, 180.0], [0.0, 90.0, 180.0, 270.0]])
        gen = PSG(base_spec_data, heading_sweeps=[sweep])
        specs = gen.generate()
        h0 = specs[0].wave_headings.to_heading_list()
        h1 = specs[1].wave_headings.to_heading_list()
        assert h0 == [0.0, 180.0]
        assert h1 == [0.0, 90.0, 180.0, 270.0]

    def test_heading_sweep_schema_compliance(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        sweep = HS(values=[[0.0, 45.0, 90.0, 135.0, 180.0]])
        gen = PSG(base_spec_data, heading_sweeps=[sweep])
        specs = gen.generate()
        for spec in specs:
            assert isinstance(spec, DiffractionSpec)


# ---------------------------------------------------------------------------
# Test 4: Hull parameter sweep
# ---------------------------------------------------------------------------


class TestHullParameterSweep:
    """Parametric sweep varying hull dimensions (mass, draft, etc.)."""

    def test_hull_mass_sweep(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        sweep = HPS(
            parameter="vessel.inertia.mass",
            values=[500.0, 1000.0, 2000.0],
        )
        gen = PSG(base_spec_data, hull_sweeps=[sweep])
        specs = gen.generate()
        assert len(specs) == 3
        masses = [s.vessel.inertia.mass for s in specs]
        assert masses == [500.0, 1000.0, 2000.0]

    def test_hull_sweep_schema_compliance(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        sweep = HPS(parameter="vessel.inertia.mass", values=[800.0, 1200.0])
        gen = PSG(base_spec_data, hull_sweeps=[sweep])
        specs = gen.generate()
        for spec in specs:
            assert isinstance(spec, DiffractionSpec)
            assert spec.vessel.inertia.mass > 0


# ---------------------------------------------------------------------------
# Test 5: Spec schema compliance (comprehensive)
# ---------------------------------------------------------------------------


class TestSpecSchemaCompliance:
    """Ensure all generated specs validate against DiffractionSpec."""

    def test_generated_specs_have_required_fields(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        gen = PSG(base_spec_data)
        specs = gen.generate()
        for spec in specs:
            assert spec.vessel is not None
            assert spec.environment is not None
            assert spec.frequencies is not None
            assert spec.wave_headings is not None

    def test_generated_specs_round_trip_yaml(self, base_spec_data, tmp_path):
        PSG, FS, HS, HPS = _get_generator()
        sweep = FS(start=0.5, end=1.5, steps=2)
        gen = PSG(base_spec_data, frequency_sweeps=[sweep])
        paths = gen.generate_to_yaml(tmp_path)
        for p in paths:
            restored = DiffractionSpec.from_yaml(p)
            assert restored.vessel.name == "TestCylinder"

    def test_metadata_includes_sweep_info(self, base_spec_data):
        PSG, FS, HS, HPS = _get_generator()
        sweep = FS(start=0.5, end=1.5, steps=2)
        gen = PSG(base_spec_data, frequency_sweeps=[sweep])
        specs = gen.generate()
        for i, spec in enumerate(specs):
            tags = spec.metadata.tags
            assert any("sweep" in t.lower() or "parametric" in t.lower() for t in tags)
