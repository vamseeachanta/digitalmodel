"""Tests for semantic roundtrip fidelity.

Verifies that extract -> spec -> generate -> validate produces
semantically equivalent YAML to the original monolithic export.
Tests cover:
- 6DBuoy mass/volume survival through roundtrip
- Current profile depth-varying levels
- Cross-reference consistency (line names match LineContactData refs)
- Boolean type consistency (True/False, not "Yes"/"No")
"""

import tempfile
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator.extractor import (
    MonolithicExtractor,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import (
    ProjectInputSpec,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _write_mono_yaml(tmpdir: Path, sections: dict) -> Path:
    """Write a minimal monolithic YAML file from section dicts."""
    yml_path = tmpdir / "mono.yml"
    with open(yml_path, "w", encoding="utf-8") as f:
        yaml.dump(sections, f, default_flow_style=False)
    return yml_path


def _roundtrip(mono_path: Path) -> tuple[dict, dict]:
    """Run full roundtrip: extract -> spec -> generate -> load modular.

    Returns (mono_yaml, modular_yaml) for comparison.
    """
    extractor = MonolithicExtractor(mono_path)
    spec_dict = extractor.extract()
    spec = ProjectInputSpec(**spec_dict)

    with tempfile.TemporaryDirectory() as tmpdir:
        mod_dir = Path(tmpdir) / "modular"
        gen = ModularModelGenerator.from_spec(spec)
        gen.generate(mod_dir)

        # Load modular output
        merged: dict = {}
        includes = mod_dir / "includes"
        for yml in sorted(includes.glob("*.yml")):
            with open(yml, encoding="utf-8") as f:
                data = yaml.safe_load(f)
            if isinstance(data, dict):
                for key, val in data.items():
                    if key in merged:
                        existing = merged[key]
                        if isinstance(existing, dict) and isinstance(val, dict):
                            existing.update(val)
                            continue
                        if isinstance(existing, list) and isinstance(val, list):
                            existing.extend(val)
                            continue
                    merged[key] = val

    # Load monolithic
    text = mono_path.read_text(encoding="utf-8-sig")
    mono_yaml: dict = {}
    for doc in yaml.safe_load_all(text):
        if isinstance(doc, dict):
            mono_yaml.update(doc)

    return mono_yaml, merged


# ---------------------------------------------------------------------------
# 6DBuoy mass/volume roundtrip
# ---------------------------------------------------------------------------


class TestBuoy6DMassRoundtrip:
    """Verify 6DBuoy mass and volume fields survive roundtrip."""

    def test_mass_field_present_in_output(self, tmp_path):
        """6DBuoy mass field must appear in modular output, not be silently dropped."""
        sections = {
            "General": {
                "UnitsSystem": "SI",
                "StageDuration": [8, 16],
                "ImplicitConstantTimeStep": 0.1,
            },
            "Environment": {
                "SeabedOriginDepth": 100,
                "Density": 1.025,
                "SeabedSlope": 0,
                "SeabedNormalStiffness": 100,
                "SeabedShearStiffness": 100,
                "WaveTrains": [{"Name": "Wave 1", "WaveType": "Airy",
                                "WaveHeight": 2, "WavePeriod": 8,
                                "WaveDirection": 0}],
                "RefCurrentSpeed": 0,
                "RefCurrentDirection": 0,
                "CurrentDepth, CurrentFactor, CurrentRotation": [[0, 1, 0], [100, 1, 0]],
                "WindSpeed": 0,
                "WindDirection": 0,
            },
            "6DBuoys": [
                {
                    "Name": "TestBuoy",
                    "BuoyType": "Spar buoy",
                    "Connection": "Fixed",
                    "InitialPosition": [0, 0, -50],
                    "Mass": 0.47,
                    "Volume": 0.5,
                    "Height": 10,
                    "DegreesOfFreedomInStatics": "None",
                }
            ],
        }
        yml_path = _write_mono_yaml(tmp_path, sections)
        mono, mod = _roundtrip(yml_path)

        # Verify mass and volume in modular output
        buoys = mod.get("6DBuoys", [])
        assert len(buoys) >= 1, "Expected at least 1 6DBuoy in output"
        buoy = buoys[0]
        assert "Mass" in buoy, "Mass field missing from 6DBuoy (Pydantic silently dropped?)"
        assert buoy["Mass"] == 0.47, f"Mass mismatch: expected 0.47, got {buoy['Mass']}"
        assert "Volume" in buoy, "Volume field missing from 6DBuoy"
        assert buoy["Volume"] == 0.5


# ---------------------------------------------------------------------------
# Current profile roundtrip
# ---------------------------------------------------------------------------


class TestCurrentProfileRoundtrip:
    """Verify depth-varying current profiles survive roundtrip."""

    def test_multi_level_profile_preserved(self, tmp_path):
        """Multi-level current profile must survive extract -> generate."""
        sections = {
            "General": {
                "UnitsSystem": "SI",
                "StageDuration": [8, 16],
                "ImplicitConstantTimeStep": 0.1,
            },
            "Environment": {
                "SeabedOriginDepth": 100,
                "Density": 1.025,
                "SeabedSlope": 0,
                "SeabedNormalStiffness": 100,
                "SeabedShearStiffness": 100,
                "WaveTrains": [{"Name": "Wave 1", "WaveType": "Airy",
                                "WaveHeight": 2, "WavePeriod": 8,
                                "WaveDirection": 0}],
                "RefCurrentSpeed": 1.0,
                "RefCurrentDirection": 180,
                "CurrentDepth, CurrentFactor, CurrentRotation": [
                    [0, 1.0, 0],
                    [50, 0.6, 0],
                    [100, 0.2, 0],
                ],
                "WindSpeed": 0,
                "WindDirection": 0,
            },
        }
        yml_path = _write_mono_yaml(tmp_path, sections)
        mono, mod = _roundtrip(yml_path)

        env = mod.get("Environment", {})
        profile_key = "CurrentDepth, CurrentFactor, CurrentRotation"
        profile = env.get(profile_key, [])
        assert len(profile) >= 3, (
            f"Expected >= 3 current levels, got {len(profile)}. "
            f"Profile lost during roundtrip."
        )
        # Check depth values
        depths = [row[0] for row in profile]
        assert 0 in depths, "Surface level (depth=0) missing"
        assert 50 in depths, "Mid-depth level (depth=50) missing"
        assert 100 in depths, "Seabed level (depth=100) missing"

    def test_single_level_padded_to_minimum(self, tmp_path):
        """Single-level current profile must be padded to >=2 levels."""
        sections = {
            "General": {
                "UnitsSystem": "SI",
                "StageDuration": [8, 16],
                "ImplicitConstantTimeStep": 0.1,
            },
            "Environment": {
                "SeabedOriginDepth": 200,
                "Density": 1.025,
                "SeabedSlope": 0,
                "SeabedNormalStiffness": 100,
                "SeabedShearStiffness": 100,
                "WaveTrains": [{"Name": "Wave 1", "WaveType": "Airy",
                                "WaveHeight": 2, "WavePeriod": 8,
                                "WaveDirection": 0}],
                "RefCurrentSpeed": 0.5,
                "RefCurrentDirection": 0,
                "CurrentDepth, CurrentFactor, CurrentRotation": [
                    [0, 1.0, 0],
                ],
                "WindSpeed": 0,
                "WindDirection": 0,
            },
        }
        yml_path = _write_mono_yaml(tmp_path, sections)
        mono, mod = _roundtrip(yml_path)

        env = mod.get("Environment", {})
        profile_key = "CurrentDepth, CurrentFactor, CurrentRotation"
        profile = env.get(profile_key, [])
        assert len(profile) >= 2, (
            f"Expected >= 2 current levels (OrcaFlex minimum), got {len(profile)}"
        )


# ---------------------------------------------------------------------------
# Boolean type consistency
# ---------------------------------------------------------------------------


class TestBooleanTypeConsistency:
    """Verify General section uses native booleans, not strings."""

    def test_general_booleans_not_strings(self, tmp_path):
        """WholeSystemStaticsEnabled and ImplicitUseVariableTimeStep must be bools."""
        sections = {
            "General": {
                "UnitsSystem": "SI",
                "StageDuration": [8, 16],
                "ImplicitConstantTimeStep": 0.1,
                "WholeSystemStaticsEnabled": True,
                "ImplicitUseVariableTimeStep": False,
            },
            "Environment": {
                "SeabedOriginDepth": 100,
                "Density": 1.025,
                "SeabedSlope": 0,
                "SeabedNormalStiffness": 100,
                "SeabedShearStiffness": 100,
                "WaveTrains": [{"Name": "Wave 1", "WaveType": "Airy",
                                "WaveHeight": 2, "WavePeriod": 8,
                                "WaveDirection": 0}],
                "RefCurrentSpeed": 0,
                "RefCurrentDirection": 0,
                "CurrentDepth, CurrentFactor, CurrentRotation": [[0, 1, 0], [100, 1, 0]],
                "WindSpeed": 0,
                "WindDirection": 0,
            },
        }
        yml_path = _write_mono_yaml(tmp_path, sections)
        mono, mod = _roundtrip(yml_path)

        gen = mod.get("General", {})
        assert gen.get("WholeSystemStaticsEnabled") is True, (
            f"Expected True (bool), got {gen.get('WholeSystemStaticsEnabled')!r}"
        )
        assert gen.get("ImplicitUseVariableTimeStep") is False, (
            f"Expected False (bool), got {gen.get('ImplicitUseVariableTimeStep')!r}"
        )


# ---------------------------------------------------------------------------
# Cross-reference consistency
# ---------------------------------------------------------------------------


class TestCrossReferenceConsistency:
    """Verify that generated object names are consistent across sections."""

    def test_line_references_valid_line_type(self, tmp_path):
        """Lines must reference LineTypes that exist in the output."""
        sections = {
            "General": {
                "UnitsSystem": "SI",
                "StageDuration": [8, 16],
                "ImplicitConstantTimeStep": 0.1,
            },
            "Environment": {
                "SeabedOriginDepth": 100,
                "Density": 1.025,
                "SeabedSlope": 0,
                "SeabedNormalStiffness": 100,
                "SeabedShearStiffness": 100,
                "WaveTrains": [{"Name": "Wave 1", "WaveType": "Airy",
                                "WaveHeight": 2, "WavePeriod": 8,
                                "WaveDirection": 0}],
                "RefCurrentSpeed": 0,
                "RefCurrentDirection": 0,
                "CurrentDepth, CurrentFactor, CurrentRotation": [[0, 1, 0], [100, 1, 0]],
                "WindSpeed": 0,
                "WindDirection": 0,
            },
            "LineTypes": [
                {
                    "Name": "Steel Pipe",
                    "Category": "General",
                    "MassPerUnitLength": 0.1,
                    "OD": 0.3,
                    "EA": 1e6,
                    "EI": 100,
                },
            ],
            "Lines": [
                {
                    "Name": "Riser1",
                    "LineType, Length, TargetSegmentLength": [
                        ["Steel Pipe", 200, 5],
                    ],
                    "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo": [
                        ["Fixed", 0, 0, 0, 0, 0, 0, None, "a]"],
                        ["Anchored", 200, 0, -100, 0, 0, 0, None, "a]"],
                    ],
                },
            ],
        }
        yml_path = _write_mono_yaml(tmp_path, sections)
        mono, mod = _roundtrip(yml_path)

        # Extract LineType names from output
        line_type_names = {lt["Name"] for lt in mod.get("LineTypes", [])}
        assert len(line_type_names) > 0, "No LineTypes in output"

        # Check that Lines reference existing LineTypes
        for line in mod.get("Lines", []):
            lt_key = "LineType, Length, TargetSegmentLength"
            sections_data = line.get(lt_key, [])
            for sec in sections_data:
                if isinstance(sec, list) and len(sec) >= 1:
                    lt_ref = sec[0]
                    assert lt_ref in line_type_names, (
                        f"Line '{line.get('Name')}' references LineType '{lt_ref}' "
                        f"which doesn't exist. Available: {line_type_names}"
                    )


# ---------------------------------------------------------------------------
# Semantic validate exclusion list
# ---------------------------------------------------------------------------


class TestGroupsStructureOrdering:
    """Verify Groups.Structure topological ordering in extractor."""

    def test_topo_sort_parents_before_children(self):
        """Parent groups must appear before child groups after topo sort."""
        from digitalmodel.solvers.orcaflex.modular_generator.extractor import (
            _topo_sort_groups,
        )

        structure = {
            "Child A": "Parent",
            "Grandchild": "Child A",
            "Parent": "Model",
            "Child B": "Parent",
        }
        result = _topo_sort_groups(structure)
        keys = list(result.keys())

        # Parent must come before both children and grandchild
        assert keys.index("Parent") < keys.index("Child A")
        assert keys.index("Parent") < keys.index("Child B")
        assert keys.index("Child A") < keys.index("Grandchild")

    def test_topo_sort_preserves_all_entries(self):
        """Topo sort must not lose or duplicate any entries."""
        from digitalmodel.solvers.orcaflex.modular_generator.extractor import (
            _topo_sort_groups,
        )

        structure = {"A": "Model", "B": "A", "C": "B", "D": "A"}
        result = _topo_sort_groups(structure)
        assert set(result.keys()) == set(structure.keys())
        assert len(result) == len(structure)

    def test_topo_sort_empty(self):
        """Empty structure should pass through unchanged."""
        from digitalmodel.solvers.orcaflex.modular_generator.extractor import (
            _topo_sort_groups,
        )

        assert _topo_sort_groups({}) == {}


class TestWaterDepthZero:
    """Verify WaterDepth=0 does not fall through to SeabedOriginDepth."""

    def test_zero_depth_preserved(self):
        """WaterDepth=0 must be extracted as 0, not fall through."""
        ext = MonolithicExtractor.__new__(MonolithicExtractor)
        env = {"WaterDepth": 0, "SeabedOriginDepth": 500.0, "Density": 1.025}
        result = ext._extract_water(env)
        assert result["depth"] == 0, f"Expected 0, got {result['depth']}"

    def test_missing_water_depth_uses_seabed(self):
        """When WaterDepth is absent, SeabedOriginDepth should be used."""
        ext = MonolithicExtractor.__new__(MonolithicExtractor)
        env = {"SeabedOriginDepth": 350.0, "Density": 1.025}
        result = ext._extract_water(env)
        assert result["depth"] == 350.0


class TestSemanticExclusionList:
    """Verify the cosmetic exclusion list in semantic_validate.py."""

    def test_allowed_diff_props_contains_view_settings(self):
        """ALLOWED_DIFF_PROPS must include standard OrcaFlex view properties."""
        import sys
        sys.path.insert(0, str(Path(__file__).parents[4] / "scripts"))
        from semantic_validate import ALLOWED_DIFF_PROPS

        expected = {
            "DefaultViewAzimuth",
            "DefaultViewElevation",
            "DefaultViewMode",
            "DefaultViewSize",
            "DefaultViewCentre",
            "DrawNodesAsDiscs",
            "DrawShadedNodesAsSpheres",
            "ShadedDrawingCullingMode",
            "ContactPen",
            "State",
            "Structure",
        }
        missing = expected - ALLOWED_DIFF_PROPS
        assert not missing, f"Missing from ALLOWED_DIFF_PROPS: {missing}"

    def test_cosmetic_diffs_downgraded(self):
        """Diffs for allowed properties should be downgraded to COSMETIC."""
        import sys
        sys.path.insert(0, str(Path(__file__).parents[4] / "scripts"))
        from semantic_validate import (
            _downgrade_allowed_diffs,
            SectionResult,
            PropertyDiff,
            Significance,
        )

        result = SectionResult(name="General", section_type="flat")
        result.diffs = [
            PropertyDiff(key="DefaultViewAzimuth", mono_val=270, mod_val=0,
                         significance=Significance.SIGNIFICANT),
            PropertyDiff(key="StageDuration", mono_val=[7, 35], mod_val=[8, 16],
                         significance=Significance.SIGNIFICANT),
        ]
        result.missing_in_mod = [
            PropertyDiff(key="State", mono_val={"Collapsed": []},
                         significance=Significance.MISSING),
        ]

        _downgrade_allowed_diffs(result)

        assert result.diffs[0].significance == Significance.COSMETIC, (
            "DefaultViewAzimuth should be downgraded to COSMETIC"
        )
        assert result.diffs[1].significance == Significance.SIGNIFICANT, (
            "StageDuration should remain SIGNIFICANT"
        )
        assert result.missing_in_mod[0].significance == Significance.COSMETIC, (
            "State should be downgraded to COSMETIC"
        )
