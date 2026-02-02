"""Round-trip tests: single -> modular -> single must be semantically identical."""

from pathlib import Path

import pytest
import yaml

from digitalmodel.orcaflex.format_converter.single_to_modular import (
    SingleToModularConverter,
)
from digitalmodel.orcaflex.format_converter.modular_to_single import (
    ModularToSingleConverter,
)


class TestRoundTrip:
    """Test single -> modular -> single round-trip fidelity."""

    def test_roundtrip_synthetic_data(
        self, tmp_path: Path, sample_single_data: dict
    ):
        """Synthetic data round-trips correctly."""
        # Write source
        source = tmp_path / "original.yml"
        with open(source, "w") as f:
            yaml.dump(sample_single_data, f, default_flow_style=False)

        # single -> modular
        modular_dir = tmp_path / "modular"
        s2m = SingleToModularConverter(source, modular_dir)
        report1 = s2m.convert()
        assert report1.success

        # modular -> single
        roundtrip_file = tmp_path / "roundtrip.yml"
        m2s = ModularToSingleConverter()
        report2 = m2s.convert(
            source=modular_dir / "master.yml", target=roundtrip_file
        )
        assert report2.success

        # Compare: load both and check semantic equality
        with open(source) as f:
            original = yaml.safe_load(f)
        with open(roundtrip_file) as f:
            roundtrip = yaml.safe_load(f)

        # Each original key should be present
        for key in original:
            assert key in roundtrip, f"Missing key after round-trip: {key}"
            assert roundtrip[key] == original[key], (
                f"Value mismatch for '{key}': "
                f"{original[key]} != {roundtrip[key]}"
            )

    def test_roundtrip_real_a01(self, a01_single_file: Path, tmp_path: Path):
        """Real A01 file round-trips correctly (semantic equality)."""
        # Load original
        with open(a01_single_file, encoding="utf-8-sig") as f:
            content = f.read()
        original = yaml.safe_load(content)

        # single -> modular
        modular_dir = tmp_path / "a01_modular"
        s2m = SingleToModularConverter(a01_single_file, modular_dir)
        report1 = s2m.convert()
        assert report1.success

        # modular -> single
        roundtrip_file = tmp_path / "a01_roundtrip.yml"
        m2s = ModularToSingleConverter()
        report2 = m2s.convert(
            source=modular_dir / "master.yml", target=roundtrip_file
        )
        assert report2.success

        # Load roundtrip
        with open(roundtrip_file) as f:
            roundtrip = yaml.safe_load(f)

        # Check all top-level sections present
        for key in original:
            assert key in roundtrip, f"Missing section after round-trip: {key}"

        # Deep compare critical sections
        assert roundtrip["General"] == original["General"]
        assert roundtrip["Environment"] == original["Environment"]

        if "LineTypes" in original:
            assert roundtrip["LineTypes"] == original["LineTypes"]

    def test_roundtrip_with_multiple_buoy_sections(self, tmp_path: Path):
        """Sections that share a file (e.g., buoys) round-trip correctly."""
        original_data = {
            "General": {"UnitsSystem": "SI"},
            "6DBuoys": [{"Name": "Buoy6D"}],
            "Buoys": [{"Name": "BuoyClump"}],
            "3DBuoys": [{"Name": "Buoy3D"}],
        }

        source = tmp_path / "buoys_test.yml"
        with open(source, "w") as f:
            yaml.dump(original_data, f, default_flow_style=False)

        # single -> modular
        modular_dir = tmp_path / "modular"
        s2m = SingleToModularConverter(source, modular_dir)
        report1 = s2m.convert()
        assert report1.success

        # modular -> single
        roundtrip_file = tmp_path / "roundtrip.yml"
        m2s = ModularToSingleConverter()
        report2 = m2s.convert(
            source=modular_dir / "master.yml", target=roundtrip_file
        )
        assert report2.success

        with open(roundtrip_file) as f:
            roundtrip = yaml.safe_load(f)

        for key in original_data:
            assert key in roundtrip, f"Missing key: {key}"
            assert roundtrip[key] == original_data[key], (
                f"Mismatch for '{key}'"
            )

    def test_roundtrip_preserves_nested_data(self, tmp_path: Path):
        """Deeply nested data is preserved through round-trip."""
        original_data = {
            "General": {
                "UnitsSystem": "SI",
                "StageDuration": [7, 35],
                "ImplicitConstantTimeStep": 0.1,
            },
            "Environment": {
                "WaterDepth": 100,
                "WaveTrains": [
                    {
                        "Name": "Wave1",
                        "WaveHeight": 6,
                        "WavePeriod": 7,
                        "WaveDirection": 180,
                    }
                ],
            },
        }

        source = tmp_path / "nested.yml"
        with open(source, "w") as f:
            yaml.dump(original_data, f, default_flow_style=False)

        modular_dir = tmp_path / "modular"
        s2m = SingleToModularConverter(source, modular_dir)
        s2m.convert()

        roundtrip_file = tmp_path / "roundtrip.yml"
        m2s = ModularToSingleConverter()
        m2s.convert(source=modular_dir / "master.yml", target=roundtrip_file)

        with open(roundtrip_file) as f:
            roundtrip = yaml.safe_load(f)

        assert roundtrip == original_data
