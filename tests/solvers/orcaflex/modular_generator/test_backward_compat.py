"""Backward compatibility tests - verify refactored output matches original."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml


class TestGeneratorBackwardCompat:
    """Verify that the refactored generator produces identical output."""

    def test_generates_all_include_files(self, golden_output):
        expected_files = {
            "01_general.yml",
            "02_var_data.yml",
            "03_environment.yml",
            "05_line_types.yml",
            "07_lines.yml",
            "08_buoys.yml",
            "09_shapes.yml",
            "10_groups.yml",
            "13_supports.yml",
            "14_morison.yml",
        }
        include_files = {k for k in golden_output if k.endswith(".yml") and k != "parameters.yml" and k != "master.yml"}
        assert include_files == expected_files

    def test_generates_master_yml(self, golden_output):
        assert "master.yml" in golden_output
        master = golden_output["master.yml"]
        assert "%YAML 1.1" in master
        assert "includefile: includes/01_general.yml" in master
        assert "includefile: includes/10_groups.yml" in master

    def test_generates_parameters_yml(self, golden_output):
        assert "parameters.yml" in golden_output
        params = golden_output["parameters.yml"]
        assert params["water_depth"] == 8
        assert params["water_density"] == 1.03
        assert params["current_speed"] == 1
        assert params["wind_speed"] == 8.87
        assert params["time_step"] == 0.1

    def test_general_section_structure(self, golden_output):
        data = golden_output["01_general.yml"]
        assert "General" in data
        general = data["General"]
        assert general["UnitsSystem"] == "SI"
        assert general["ImplicitConstantTimeStep"] == 0.1
        assert general["NorthDirection"] == 70
        assert general["StageDuration"] == [8, 16]

    def test_environment_section_structure(self, golden_output):
        data = golden_output["03_environment.yml"]
        assert "Environment" in data
        env = data["Environment"]
        assert env["Density"] == 1.03
        assert env["SeabedOriginDepth"] == 8
        assert env["SeabedSlope"] == 0.57
        assert env["WindSpeed"] == 8.87
        assert env["RefCurrentSpeed"] == 1

    def test_vardata_section_structure(self, golden_output):
        data = golden_output["02_var_data.yml"]
        assert "VariableData" in data
        vd = data["VariableData"]
        assert "Coatingsorlinings" in vd
        coatings = vd["Coatingsorlinings"]
        # Should have CWC120, CWC90, and base coating entries
        assert len(coatings) == 3
        names = [c["Name"] for c in coatings]
        assert "coating+CWC120" in names
        assert "coating+CWC90" in names
        assert "coating" in names

    def test_line_types_section_structure(self, golden_output):
        data = golden_output["05_line_types.yml"]
        assert "LineTypes" in data
        lt = data["LineTypes"]
        # Two pipeline types + winch wire
        assert len(lt) == 3
        names = [t["Name"] for t in lt]
        assert "X65+coating+CWC120" in names
        assert "X65+coating+CWC90" in names
        assert "Winch wire_LT" in names

    def test_supports_section_structure(self, golden_output):
        data = golden_output["13_supports.yml"]
        assert "SupportTypes" in data
        st = data["SupportTypes"]
        assert len(st) == 2
        names = [s["Name"] for s in st]
        assert "Support type1" in names
        assert "Support type2" in names

    def test_morison_section_structure(self, golden_output):
        data = golden_output["14_morison.yml"]
        assert "MorisonElementTypes" in data
        mt = data["MorisonElementTypes"]
        assert len(mt) == 1
        assert mt[0]["Name"] == "Morison element type1"

    def test_shapes_section_structure(self, golden_output):
        data = golden_output["09_shapes.yml"]
        assert "Shapes" in data
        shapes = data["Shapes"]
        assert len(shapes) == 2
        names = [s["Name"] for s in shapes]
        assert "Ramp inclined" in names
        assert "Ramp-curve" in names

    def test_buoys_section_structure(self, golden_output):
        data = golden_output["08_buoys.yml"]
        assert "6DBuoys" in data
        assert "3DBuoys" in data
        buoys_6d = data["6DBuoys"]
        buoys_3d = data["3DBuoys"]
        # Rollers + 5 tugs + BM + 6D buoy1 = 8
        assert len(buoys_6d) == 8
        assert len(buoys_3d) == 1
        assert buoys_3d[0]["Name"] == "Mid-pipe"

    def test_lines_section_structure(self, golden_output):
        data = golden_output["07_lines.yml"]
        assert "Lines" in data
        lines = data["Lines"]
        assert len(lines) == 1
        assert lines[0]["Name"] == "30'' Line"

    def test_groups_section_structure(self, golden_output):
        data = golden_output["10_groups.yml"]
        assert "Groups" in data
        groups = data["Groups"]
        assert "Structure" in groups
        structure = groups["Structure"]
        # Pipeline + rollers + 5 tugs + BM + 6D buoy1 + mid-pipe + 2 shapes = 11
        assert "30'' Line" in structure
        assert "Rollers" in structure
        assert "Tug1" in structure
        assert "6D buoy1" in structure

    def test_round_trip_generation(self, validated_spec, tmp_path):
        """Generate twice and verify output is identical."""
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )

        spec_file = Path(__file__).parent.parent.parent.parent.parent / (
            "docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml"
        )

        dir1 = tmp_path / "gen1"
        dir2 = tmp_path / "gen2"

        gen1 = ModularModelGenerator(spec_file)
        gen1.generate(dir1)

        gen2 = ModularModelGenerator(spec_file)
        gen2.generate(dir2)

        # Compare all generated files
        for yml_file in sorted((dir1 / "includes").glob("*.yml")):
            with open(yml_file) as f:
                data1 = yaml.safe_load(f)
            with open(dir2 / "includes" / yml_file.name) as f:
                data2 = yaml.safe_load(f)
            assert data1 == data2, f"Mismatch in {yml_file.name}"
