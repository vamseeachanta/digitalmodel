"""Tests for the AQWA backend that generates .dat files from DiffractionSpec.

WRK-058: TDD tests define the expected behavior of the AQWABackend class.

The backend converts a solver-agnostic DiffractionSpec into AQWA-specific
.dat input files, supporting both single-file and modular output modes.
"""

import math
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.diffraction.input_schemas import DiffractionSpec


# Fixtures directory for YAML specs
FIXTURES_DIR = Path(__file__).parent / "fixtures"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _load_ship_spec() -> DiffractionSpec:
    """Load the ship RAO fixture spec."""
    return DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_ship_raos.yml")


def _load_semisub_spec() -> DiffractionSpec:
    """Load the semi-sub fixture spec."""
    return DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_semisub.yml")


def _load_fpso_turret_spec() -> DiffractionSpec:
    """Load the multi-body FPSO turret fixture spec."""
    return DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_fpso_turret.yml")


# ---------------------------------------------------------------------------
# Single-file generation tests
# ---------------------------------------------------------------------------


class TestAQWABackendSingleFile:
    """Test single .dat file generation from DiffractionSpec."""

    def test_generate_single_creates_dat_file(self, tmp_path: Path):
        """generate_single produces a .dat file in output directory."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)

        assert result_path.exists()
        assert result_path.suffix == ".dat"
        assert result_path.parent == tmp_path

    def test_single_file_contains_all_decks(self, tmp_path: Path):
        """Single .dat file must contain DECK 0 through DECK 6 markers."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        for deck_num in [0, 1, 2, 3, 4, 5, 6]:
            assert f"DECK{deck_num:>3d}" in content or f"DECK  {deck_num}" in content, (
                f"DECK {deck_num} not found in output"
            )

    def test_single_file_contains_job_card(self, tmp_path: Path):
        """Deck 0 must contain a JOB AQWA card."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        assert "JOB AQWA" in content

    def test_single_file_contains_end_cards(self, tmp_path: Path):
        """Each deck body should be terminated with END."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        # At minimum the environment, frequency, and mass decks need END
        end_count = content.count(" END")
        assert end_count >= 4, f"Expected at least 4 END cards, got {end_count}"


# ---------------------------------------------------------------------------
# Modular generation tests
# ---------------------------------------------------------------------------


class TestAQWABackendModular:
    """Test modular deck file generation from DiffractionSpec."""

    def test_generate_modular_creates_directory(self, tmp_path: Path):
        """generate_modular produces multiple deck files in output directory."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_modular(spec, tmp_path)

        assert result_path.exists()
        assert result_path.is_dir()

    def test_modular_creates_expected_deck_files(self, tmp_path: Path):
        """Modular output creates individual files per deck."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_modular(spec, tmp_path)

        deck_files = sorted(result_path.glob("deck_*.dat"))
        assert len(deck_files) >= 7, (
            f"Expected at least 7 deck files, got {len(deck_files)}"
        )

    def test_modular_concatenation_matches_single(self, tmp_path: Path):
        """Concatenating modular deck files should produce content equivalent
        to the single-file output (same deck structure and card content)."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()

        single_dir = tmp_path / "single"
        single_dir.mkdir()
        single_path = backend.generate_single(spec, single_dir)
        single_content = single_path.read_text()

        modular_dir = tmp_path / "modular"
        modular_dir.mkdir()
        modular_path = backend.generate_modular(spec, modular_dir)

        # Concatenate modular files in order
        deck_files = sorted(modular_path.glob("deck_*.dat"))
        concatenated = ""
        for f in deck_files:
            concatenated += f.read_text()

        # Strip whitespace differences for comparison
        single_lines = [
            line for line in single_content.splitlines() if line.strip()
        ]
        concat_lines = [
            line for line in concatenated.splitlines() if line.strip()
        ]
        assert single_lines == concat_lines


# ---------------------------------------------------------------------------
# Water depth mapping tests
# ---------------------------------------------------------------------------


class TestWaterDepthMapping:
    """Test conversion of water depth from spec to AQWA format."""

    def test_finite_water_depth(self, tmp_path: Path):
        """Finite water depth appears in DPTH card."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        # Spec has water_depth=500.0
        assert "DPTH" in content
        # The depth value should appear on the DPTH line
        dpth_lines = [
            line for line in content.splitlines() if "DPTH" in line
        ]
        assert len(dpth_lines) == 1
        assert "500" in dpth_lines[0]

    def test_infinite_water_depth(self, tmp_path: Path):
        """Infinite water depth uses large value in DPTH card."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_fpso_turret_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        dpth_lines = [
            line for line in content.splitlines() if "DPTH" in line
        ]
        assert len(dpth_lines) == 1
        # AQWA uses a large depth value (e.g., 10000) for deep water
        dpth_value = float(dpth_lines[0].split()[-1].replace(".", "").replace(",", ""))
        # Accept anything >= 5000 as "deep water" representation
        assert dpth_value >= 5000


# ---------------------------------------------------------------------------
# Frequency mapping tests
# ---------------------------------------------------------------------------


class TestFrequencyMapping:
    """Test conversion of frequencies from spec to AQWA HRTZ cards."""

    def test_explicit_frequencies_to_hrtz(self, tmp_path: Path):
        """Explicit frequency values appear as HRTZ cards in deck 6."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        hrtz_lines = [
            line for line in content.splitlines() if "HRTZ" in line
        ]
        # Ship spec has 15 explicit frequencies
        assert len(hrtz_lines) == 15

    def test_frequency_range_to_hrtz(self, tmp_path: Path):
        """Frequency range generates correct count of HRTZ cards."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_semisub_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        hrtz_lines = [
            line for line in content.splitlines() if "HRTZ" in line
        ]
        # Semi-sub spec has range count=50
        assert len(hrtz_lines) == 50

    def test_frequencies_converted_to_hz(self, tmp_path: Path):
        """AQWA HRTZ cards use Hz (not rad/s). Verify conversion."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        hrtz_lines = [
            line for line in content.splitlines() if "HRTZ" in line
        ]
        # First frequency in spec is 0.1 rad/s -> 0.1/(2*pi) Hz
        first_freq_hz = 0.1 / (2.0 * math.pi)
        first_line = hrtz_lines[0]
        # Extract the frequency value (last numeric token)
        tokens = first_line.split()
        freq_value = float(tokens[-1])
        assert abs(freq_value - first_freq_hz) < 1e-4


# ---------------------------------------------------------------------------
# Heading mapping tests
# ---------------------------------------------------------------------------


class TestHeadingMapping:
    """Test conversion of wave headings from spec to AQWA DIRN cards."""

    def test_explicit_headings_to_dirn(self, tmp_path: Path):
        """Explicit heading values appear as DIRN cards in deck 6."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        dirn_lines = [
            line for line in content.splitlines() if "DIRN" in line
        ]
        # Ship spec has 5 explicit headings: 0, 45, 90, 135, 180
        assert len(dirn_lines) == 5

    def test_heading_range_to_dirn(self, tmp_path: Path):
        """Heading range generates correct DIRN card count."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_semisub_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        dirn_lines = [
            line for line in content.splitlines() if "DIRN" in line
        ]
        # Semi-sub has range start=0, end=180, increment=22.5 -> 9 headings
        assert len(dirn_lines) == 9

    def test_heading_values_in_degrees(self, tmp_path: Path):
        """DIRN cards contain heading values in degrees."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        dirn_lines = [
            line for line in content.splitlines() if "DIRN" in line
        ]
        # Check that 180 degrees appears in one of the lines
        found_180 = any("180" in line for line in dirn_lines)
        assert found_180, "Expected 180 degree heading in DIRN cards"


# ---------------------------------------------------------------------------
# Mass and inertia deck tests
# ---------------------------------------------------------------------------


class TestMassInertiaDeck:
    """Test deck 3 (mass) and deck 4 (inertia) generation."""

    def test_mass_card_contains_vessel_mass(self, tmp_path: Path):
        """Deck 3 MATE section has correct mass from spec."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        assert "MATE" in content
        # Ship mass is 84387300.0 kg
        mate_lines = [
            line
            for line in content.splitlines()
            if line.strip() and "MATE" not in line and "DECK" not in line
        ]
        # Find the line with mass value in deck 3 area
        found_mass = False
        for line in content.splitlines():
            if "84387300" in line or "8.439e+07" in line.lower() or "8.4387" in line:
                found_mass = True
                break
        assert found_mass, "Ship mass not found in deck 3"

    def test_inertia_deck_from_radii_of_gyration(self, tmp_path: Path):
        """Deck 4 GEOM/PMAS card computed from radii of gyration."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        assert "GEOM" in content
        # Ixx = mass * kxx^2 = 84387300 * 15^2 = 1.8987e+10
        # Iyy = mass * kyy^2 = 84387300 * 50^2 = 2.1097e+11
        # Izz = mass * kzz^2 = 84387300 * 50^2 = 2.1097e+11
        assert "PMAS" in content

    def test_inertia_deck_from_tensor(self, tmp_path: Path):
        """Deck 4 GEOM/PMAS card uses inertia tensor when provided."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_semisub_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        assert "PMAS" in content


# ---------------------------------------------------------------------------
# Multi-body structure count test
# ---------------------------------------------------------------------------


class TestMultiBodySpec:
    """Test multi-body spec generates correct structure count."""

    def test_multi_body_structure_count(self, tmp_path: Path):
        """FPSO turret spec with 2 bodies generates 2 STRC markers."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_fpso_turret_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        strc_lines = [
            line for line in content.splitlines() if "STRC" in line
        ]
        # Each body generates STRC in deck 1 and deck 3 and deck 4
        # At minimum there should be 2 unique structure numbers
        assert len(strc_lines) >= 2

    def test_multi_body_mass_per_structure(self, tmp_path: Path):
        """Each body in multi-body spec gets its own mass card."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_fpso_turret_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        # Should contain mass for both FPSO (250000000) and turret (5000000)
        found_fpso_mass = False
        found_turret_mass = False
        for line in content.splitlines():
            if "2.5" in line and "e+08" in line.lower():
                found_fpso_mass = True
            elif "250000000" in line:
                found_fpso_mass = True
            if "5000000" in line or "5.0" in line and "e+06" in line.lower():
                found_turret_mass = True
        assert found_fpso_mass, "FPSO mass not found"
        assert found_turret_mass, "Turret mass not found"


# ---------------------------------------------------------------------------
# Environment deck tests
# ---------------------------------------------------------------------------


class TestEnvironmentDeck:
    """Test deck 5 (GLOB) environment generation."""

    def test_glob_deck_has_water_density(self, tmp_path: Path):
        """Deck 5 GLOB contains DENS card with water density."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        dens_lines = [
            line for line in content.splitlines() if "DENS" in line
        ]
        assert len(dens_lines) == 1
        assert "1025" in dens_lines[0]

    def test_glob_deck_has_gravity(self, tmp_path: Path):
        """Deck 5 GLOB contains ACCG card with gravity."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        accg_lines = [
            line for line in content.splitlines() if "ACCG" in line
        ]
        assert len(accg_lines) == 1
        assert "9.80665" in accg_lines[0]


# ---------------------------------------------------------------------------
# Deck card generation unit tests
# ---------------------------------------------------------------------------


class TestDeckCardGeneration:
    """Test individual deck card generation methods."""

    def test_deck0_job_cards(self):
        """Deck 0 generates JOB, TITLE, and OPTIONS cards."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        cards = backend.build_deck0(spec)

        card_text = "\n".join(cards)
        assert "JOB AQWA" in card_text
        assert "TITLE" in card_text or "OPTIONS" in card_text

    def test_deck5_glob_cards(self):
        """Deck 5 generates GLOB, DPTH, DENS, ACCG cards."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        cards = backend.build_deck5(spec)

        card_text = "\n".join(cards)
        assert "GLOB" in card_text
        assert "DPTH" in card_text
        assert "DENS" in card_text
        assert "ACCG" in card_text
        assert "END" in card_text

    def test_deck6_frequency_cards(self):
        """Deck 6 generates FDR1, HRTZ, and DIRN cards."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        cards = backend.build_deck6(spec)

        card_text = "\n".join(cards)
        assert "FDR1" in card_text
        assert "HRTZ" in card_text
        assert "DIRN" in card_text
        assert "END" in card_text

    def test_deck3_mass_cards(self):
        """Deck 3 generates MATE and mass line cards."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        cards = backend.build_deck3(spec)

        card_text = "\n".join(cards)
        assert "MATE" in card_text
        assert "END" in card_text

    def test_deck4_inertia_cards(self):
        """Deck 4 generates GEOM and PMAS cards."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        cards = backend.build_deck4(spec)

        card_text = "\n".join(cards)
        assert "GEOM" in card_text
        assert "PMAS" in card_text
        assert "END" in card_text


# ---------------------------------------------------------------------------
# Options mapping tests
# ---------------------------------------------------------------------------


class TestOptionsMapping:
    """Test solver options are mapped to AQWA OPTIONS cards."""

    def test_lhfr_option_when_remove_irregular_frequencies(self, tmp_path: Path):
        """OPTIONS LHFR set when remove_irregular_frequencies is True."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        options_lines = [
            line for line in content.splitlines() if "OPTIONS" in line
        ]
        options_text = " ".join(options_lines)
        assert "LHFR" in options_text

    def test_mqtf_option_when_qtf_enabled(self, tmp_path: Path):
        """OPTIONS MQTF set when qtf_calculation is True."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_semisub_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        options_lines = [
            line for line in content.splitlines() if "OPTIONS" in line
        ]
        options_text = " ".join(options_lines)
        assert "MQTF" in options_text


# ---------------------------------------------------------------------------
# Title / metadata test
# ---------------------------------------------------------------------------


class TestMetadataInOutput:
    """Test that metadata from spec appears in AQWA output."""

    def test_title_card_contains_project_name(self, tmp_path: Path):
        """TITLE card should contain the spec project name."""
        from digitalmodel.diffraction.aqwa_backend import AQWABackend

        spec = _load_ship_spec()
        backend = AQWABackend()
        result_path = backend.generate_single(spec, tmp_path)
        content = result_path.read_text()

        title_lines = [
            line for line in content.splitlines() if "TITLE" in line
        ]
        assert len(title_lines) >= 1
        # Project is "test_ship_raos"
        title_text = " ".join(title_lines)
        assert "test_ship_raos" in title_text
