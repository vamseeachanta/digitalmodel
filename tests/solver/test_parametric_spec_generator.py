"""Tests for parametric spec.yml generator (TDD — tests written first).

The generator creates DiffractionSpec-compliant spec files from sweep
definitions, supporting frequency, heading, and hull-parameter sweeps.

Issue: #1596
"""
from __future__ import annotations

import copy
import itertools
import os
import subprocess
import sys
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


def test_max_specs_rejects_before_expansion(base_spec_data, tmp_path, monkeypatch):
    """A hand-counted product must be bounded before any expansion or output."""
    PSG, FS, HS, HPS = _get_generator()
    # Synthetic axes: two frequency bands times three heading sets = six specs.
    headings = [[0.0], [90.0], [180.0]]
    steps = 2
    expected_count = steps * len(headings)
    gen = PSG(base_spec_data, [FS(0.5, 1.5, steps)], [HS(headings)],
              max_specs=expected_count - 1)
    output = tmp_path / "rejected"
    with monkeypatch.context() as patch:
        def unexpected(*args, **kwargs):
            pytest.fail("Oversized product reached materialization")

        patch.setattr(gen, "_expand", unexpected)
        patch.setattr(FS, "generate_ranges", unexpected)
        with pytest.raises(ValueError, match="max_specs"):
            gen.generate_to_yaml(output)
    assert not output.exists()
    # Existing output must also remain byte-for-byte unchanged on rejection.
    output.mkdir()
    sentinel = output / "keep.yml"
    sentinel.write_text("unchanged\n")
    with pytest.raises(ValueError, match="max_specs"):
        gen.generate_to_yaml(output)
    assert list(output.iterdir()) == [sentinel]
    assert sentinel.read_text() == "unchanged\n"
    boundary = PSG(base_spec_data, [FS(0.5, 1.5, steps)], [HS(headings)],
                   max_specs=expected_count)
    assert len(boundary.generate_to_yaml(tmp_path / "boundary")) == expected_count


def test_combined_sweeps_have_cartesian_membership(base_spec_data, tmp_path):
    """Independent synthetic frequency, heading and mass axes form a product."""
    PSG, FS, HS, HPS = _get_generator()
    base_spec_data["metadata"] = {"description": "Synthetic / base"}
    original = copy.deepcopy(base_spec_data)
    # Hand calculation using the existing band rule: spacing=1, half-band=.4;
    # five equally spaced samples around .5 and 1.5, with no lower clipping.
    frequencies = [(0.1, 0.3, 0.5, 0.7, 0.9), (1.1, 1.3, 1.5, 1.7, 1.9)]
    headings = [[0.0], [90.0], [180.0]]
    masses = [800.0, 1200.0]  # Synthetic fixture masses; no solver result basis.
    expected_count = len(frequencies) * len(headings) * len(masses)
    expected = set(itertools.product(frequencies, map(tuple, headings), masses))
    gen = PSG(base_spec_data, [FS(0.5, 1.5, len(frequencies))], [HS(headings)],
              [HPS("vessel.inertia.mass", masses)], max_specs=expected_count)
    specs = gen.generate()
    actual = [(tuple(s.frequencies.values), tuple(s.wave_headings.values),
               s.vessel.inertia.mass) for s in specs]
    assert len(actual) == expected_count
    assert len(set(actual)) == expected_count
    assert set(actual) == expected
    paths = gen.generate_to_yaml(tmp_path / "first")
    repeated = gen.generate_to_yaml(tmp_path / "second")
    expected_names = [
        f"spec_synthetic-base_f0-{f}_h0-{h}_p0-{p}.yml"
        for p in range(len(masses)) for h in range(len(headings))
        for f in range(len(frequencies))
    ]
    assert [p.name for p in paths] == expected_names
    assert [p.name for p in repeated] == expected_names
    assert len(set(expected_names)) == expected_count
    for spec, path, repeat in zip(specs, paths, repeated):
        assert DiffractionSpec.from_yaml(path).model_dump() == spec.model_dump()
        assert path.read_bytes() == repeat.read_bytes()
    specs[0].frequencies.values.append(2.0)
    specs[0].wave_headings.values.append(45.0)
    specs[0].metadata.tags.append("changed")
    assert specs[1].frequencies.values == list(frequencies[1])
    assert specs[1].wave_headings.values == headings[0]
    assert "changed" not in specs[1].metadata.tags
    assert base_spec_data == original
    assert gen.generate()[0].frequencies.values == list(frequencies[0])


@pytest.mark.parametrize("limit", [0, -1, True, False, 1.5, "6", None])
def test_invalid_max_specs(base_spec_data, limit):
    """Only positive integer limits are meaningful (issue #1711)."""
    PSG, *_ = _get_generator()
    with pytest.raises(ValueError, match="max_specs"):
        PSG(base_spec_data, max_specs=limit)


def test_default_max_specs(base_spec_data, tmp_path, monkeypatch):
    """Issue #1711 sets the default bound at one thousand specs."""
    PSG, FS, HS, HPS = _get_generator()
    default_bound = 1000  # Issue #1711, max_specs acceptance requirement.
    gen = PSG(base_spec_data, heading_sweeps=[HS([[0.0]] * (default_bound + 1))])
    monkeypatch.setattr(gen, "_build_variants", lambda: pytest.fail("Expanded"))
    with pytest.raises(ValueError, match="max_specs"):
        gen.generate_to_yaml(tmp_path / "oversized")
    assert not (tmp_path / "oversized").exists()
    assert len(PSG(base_spec_data, max_specs=1).generate()) == 1  # Empty product.
    assert len(PSG(base_spec_data, heading_sweeps=[HS([[0.0]] * default_bound)]).generate()) == default_bound


def test_empty_axis_does_not_materialize_prior_axes(base_spec_data, monkeypatch):
    """A zero-size product must not allocate a large intermediate product."""
    PSG, FS, HS, HPS = _get_generator()
    # Synthetic product: two frequency bands times zero heading sets = zero.
    gen = PSG(base_spec_data, [FS(0.5, 1.5, 2)], [HS([])], max_specs=1)
    monkeypatch.setattr(FS, "generate_ranges", lambda self: pytest.fail("Allocated"))
    assert gen.generate() == []


def test_duplicate_coordinates_have_unique_names(base_spec_data, tmp_path):
    """Two equal heading values still occupy two distinct sweep coordinates."""
    PSG, FS, HS, HPS = _get_generator()
    gen = PSG(base_spec_data, heading_sweeps=[HS([[0.0], [0.0]])])
    paths = gen.generate_to_yaml(tmp_path)
    assert [p.name for p in paths] == ["spec_base_h0-0.yml", "spec_base_h0-1.yml"]


def test_invalid_spec_creates_no_output(base_spec_data, tmp_path):
    """Schema validation completes before an output directory is created."""
    PSG, FS, HS, HPS = _get_generator()
    gen = PSG(base_spec_data, hull_sweeps=[HPS("vessel.inertia.mass", [-1.0])])
    output = tmp_path / "invalid"
    with pytest.raises(ValueError):
        gen.generate_to_yaml(output)
    assert not output.exists()


def _run_cli(*args):
    """Use the same interpreter and source checkout for a module subprocess."""
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join(
        [str(Path(__file__).resolve().parents[2] / "src"), env.get("PYTHONPATH", "")]
    )
    return subprocess.run(
        [sys.executable, "-m", "digitalmodel.solvers.parametric_spec_generator", *args],
        capture_output=True, text=True, env=env, check=False,
    )


def test_module_cli_help():
    result = _run_cli("--help")
    assert result.returncode == 0, result.stderr
    for option in ("--base", "--freq-sweep", "--heading-sweep", "--output-dir", "--max-specs"):
        assert option in result.stdout


def test_module_cli_combined_sweep(base_spec_data, tmp_path):
    """Two bands times two headings produce four independently checked files."""
    base_spec_data["metadata"] = {"description": "Synthetic base"}
    base = tmp_path / "base.yml"
    base.write_text(yaml.safe_dump(base_spec_data))
    output = tmp_path / "output"
    result = _run_cli("--base", str(base), "--freq-sweep", "0.5:1.5:2",
                      "--heading-sweep", "0:90:2", "--output-dir", str(output))
    assert result.returncode == 0, result.stderr
    # Same hand-calculated bands as the Cartesian API test; singleton headings.
    frequencies = [(0.1, 0.3, 0.5, 0.7, 0.9), (1.1, 1.3, 1.5, 1.7, 1.9)]
    headings = [0.0, 90.0]
    expected_count = len(frequencies) * len(headings)
    expected_names = {f"spec_synthetic-base_f0-{f}_h0-{h}.yml"
                      for h in range(len(headings)) for f in range(len(frequencies))}
    paths = sorted(output.glob("*.yml"))
    assert len(paths) == expected_count
    assert {p.name for p in paths} == expected_names
    restored = [DiffractionSpec.from_yaml(p) for p in paths]
    assert {(tuple(s.frequencies.values), s.wave_headings.values[0]) for s in restored} == set(
        itertools.product(frequencies, headings)
    )
    for spec, path in zip(restored, paths):
        assert yaml.safe_load(path.read_text()) == spec.model_dump(mode="json", exclude_none=True)


@pytest.mark.parametrize("option,value", [
    ("--freq-sweep", "bad"), ("--freq-sweep", "1:2:0"),
    ("--freq-sweep", "2:1:2"), ("--freq-sweep", "0:1:2"),
    ("--freq-sweep", "nan:1:2"), ("--freq-sweep", "1:inf:2"),
    ("--freq-sweep", "1:2:1.5"), ("--heading-sweep", "90:0:2"),
    ("--heading-sweep", "0:nan:2"), ("--heading-sweep", "0:90:0"),
    ("--heading-sweep", "0:90:1001"),  # Issue #1711 default bound.
    ("--max-specs", "0"), ("--max-specs", "3"),
])
def test_module_cli_rejects_before_output(base_spec_data, tmp_path, option, value):
    """Malformed ranges and oversized products cannot leave partial files."""
    base = tmp_path / "base.yml"
    base.write_text(yaml.safe_dump(base_spec_data))
    output = tmp_path / "rejected"
    result = _run_cli("--base", str(base), "--freq-sweep", "0.5:1.5:2",
                      "--heading-sweep", "0:90:2", "--output-dir", str(output),
                      option, value)
    assert result.returncode != 0
    assert "error:" in result.stderr
    assert not output.exists()
