"""Tests for WamitReferenceLoader — YAML loading and non-dim conversions."""
from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    AddedMassSet,
    DampingSet,
    DiffractionResults,
    DOF,
    RAOSet,
)
from digitalmodel.hydrodynamics.diffraction.wamit_reference_loader import (
    WamitReferenceLoader,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _write_yaml(path: Path, data: dict) -> Path:
    """Write a dict as YAML to the given path and return it."""
    with open(path, "w", encoding="utf-8") as f:
        yaml.dump(data, f, default_flow_style=None)
    return path


def _minimal_yaml_data(
    nfreq: int = 3,
    nhead: int = 1,
) -> dict:
    """Return minimal valid YAML dict with only heave RAO."""
    freqs = [0.2 * (i + 1) for i in range(nfreq)]
    headings = [0.0] * nhead if nhead == 1 else [float(h) for h in range(nhead)]

    return {
        "vessel_name": "TestBox",
        "solver": "WAMIT v7.3",
        "water_depth": 50.0,
        "frequencies_rad_s": freqs,
        "headings_deg": headings[:nhead],
        "raos": {
            "heave": {
                "magnitude": [[1.0 / (i + 1)] * nhead for i in range(nfreq)],
                "phase": [[0.0] * nhead for i in range(nfreq)],
            },
        },
    }


def _full_yaml_data(nfreq: int = 3, nhead: int = 2) -> dict:
    """Return a full YAML dict with all 6 DOFs, added mass, and damping."""
    freqs = [0.2 * (i + 1) for i in range(nfreq)]
    headings = [0.0, 90.0][:nhead]

    raos = {}
    for dof_name in ("surge", "sway", "heave", "roll", "pitch", "yaw"):
        raos[dof_name] = {
            "magnitude": [
                [float(i + j + 1) for j in range(nhead)]
                for i in range(nfreq)
            ],
            "phase": [
                [float(10 * i + j) for j in range(nhead)]
                for i in range(nfreq)
            ],
        }

    # Build symmetric 6x6 added mass and damping matrices
    added_mass = []
    damping = []
    for i, f in enumerate(freqs):
        base = np.eye(6, dtype=float) * (i + 1) * 100.0
        sym = (base + base.T) / 2.0
        added_mass.append({"frequency": f, "matrix": sym.tolist()})
        damping.append({"frequency": f, "matrix": (sym * 0.1).tolist()})

    return {
        "vessel_name": "FullVessel",
        "solver": "WAMIT v7.4",
        "water_depth": 200.0,
        "frequencies_rad_s": freqs,
        "headings_deg": headings,
        "raos": raos,
        "added_mass": added_mass,
        "damping": damping,
    }


# ---------------------------------------------------------------------------
# 1. test_from_yaml_minimal
# ---------------------------------------------------------------------------


class TestFromYamlMinimal:
    """Load a minimal YAML with just heave RAO, verify DiffractionResults."""

    def test_returns_diffraction_results(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        path = _write_yaml(tmp_path / "minimal.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert isinstance(result, DiffractionResults)

    def test_vessel_name_matches(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        path = _write_yaml(tmp_path / "minimal.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert result.vessel_name == "TestBox"

    def test_analysis_tool_is_wamit(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        path = _write_yaml(tmp_path / "minimal.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert result.analysis_tool == "WAMIT"

    def test_water_depth_matches(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        path = _write_yaml(tmp_path / "minimal.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert result.water_depth == pytest.approx(50.0)

    def test_heave_rao_populated(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data(nfreq=3, nhead=1)
        path = _write_yaml(tmp_path / "minimal.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        heave = result.raos.heave
        assert heave.dof == DOF.HEAVE
        assert heave.magnitude.shape == (3, 1)
        assert heave.magnitude[0, 0] == pytest.approx(1.0)
        assert heave.magnitude[1, 0] == pytest.approx(0.5)
        assert heave.magnitude[2, 0] == pytest.approx(1.0 / 3.0)

    def test_missing_dofs_are_zeros(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data(nfreq=3, nhead=1)
        path = _write_yaml(tmp_path / "minimal.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        # surge, sway, roll, pitch, yaw should all be zero
        for dof_name in ("surge", "sway", "roll", "pitch", "yaw"):
            component = getattr(result.raos, dof_name)
            assert np.allclose(component.magnitude, 0.0), (
                f"{dof_name} magnitude should be zero"
            )
            assert np.allclose(component.phase, 0.0), (
                f"{dof_name} phase should be zero"
            )

    def test_added_mass_damping_are_zero_matrices(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data(nfreq=3)
        path = _write_yaml(tmp_path / "minimal.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert len(result.added_mass.matrices) == 3
        assert len(result.damping.matrices) == 3
        for mat in result.added_mass.matrices:
            assert np.allclose(mat.matrix, 0.0)
        for mat in result.damping.matrices:
            assert np.allclose(mat.matrix, 0.0)

    def test_unit_system_is_si(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        path = _write_yaml(tmp_path / "minimal.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert result.unit_system == "SI"


# ---------------------------------------------------------------------------
# 2. test_from_yaml_with_added_mass_damping
# ---------------------------------------------------------------------------


class TestFromYamlFull:
    """Full YAML with all 6 DOFs, added mass, and damping."""

    def test_all_six_dofs_populated(self, tmp_path: Path) -> None:
        data = _full_yaml_data(nfreq=3, nhead=2)
        path = _write_yaml(tmp_path / "full.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        for dof_name in ("surge", "sway", "heave", "roll", "pitch", "yaw"):
            component = getattr(result.raos, dof_name)
            assert component.magnitude.shape == (3, 2), (
                f"{dof_name} shape mismatch"
            )
            assert not np.allclose(component.magnitude, 0.0), (
                f"{dof_name} should not be all zero"
            )

    def test_added_mass_matrices_loaded(self, tmp_path: Path) -> None:
        data = _full_yaml_data(nfreq=3, nhead=2)
        path = _write_yaml(tmp_path / "full.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert isinstance(result.added_mass, AddedMassSet)
        assert len(result.added_mass.matrices) == 3
        for mat in result.added_mass.matrices:
            assert mat.matrix.shape == (6, 6)
            assert mat.matrix_type == "added_mass"

    def test_damping_matrices_loaded(self, tmp_path: Path) -> None:
        data = _full_yaml_data(nfreq=3, nhead=2)
        path = _write_yaml(tmp_path / "full.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert isinstance(result.damping, DampingSet)
        assert len(result.damping.matrices) == 3
        for mat in result.damping.matrices:
            assert mat.matrix.shape == (6, 6)
            assert mat.matrix_type == "damping"

    def test_added_mass_frequency_values(self, tmp_path: Path) -> None:
        data = _full_yaml_data(nfreq=3, nhead=2)
        path = _write_yaml(tmp_path / "full.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        for i, mat in enumerate(result.added_mass.matrices):
            expected_freq = 0.2 * (i + 1)
            assert mat.frequency == pytest.approx(expected_freq)

    def test_damping_matrix_values_are_tenth_of_added_mass(
        self, tmp_path: Path
    ) -> None:
        data = _full_yaml_data(nfreq=3, nhead=2)
        path = _write_yaml(tmp_path / "full.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        for am, damp in zip(
            result.added_mass.matrices, result.damping.matrices
        ):
            np.testing.assert_allclose(damp.matrix, am.matrix * 0.1, rtol=1e-12)


# ---------------------------------------------------------------------------
# 3. test_redimensionalize_added_mass
# ---------------------------------------------------------------------------


class TestRedimensionalizeAddedMass:
    """Verify non-dim to SI conversion for added mass coefficients."""

    def test_linear_added_mass(self) -> None:
        # A_bar = A / (rho * L^3) => A = A_bar * rho * L^3
        rho = 1025.0
        L = 5.0
        nondim = 2.0

        result = WamitReferenceLoader._redimensionalize_wamit(
            nondim_value=nondim,
            rho=rho,
            characteristic_length=L,
            omega=1.0,  # not used for added mass
            coefficient_type="added_mass_linear",
        )

        expected = nondim * rho * (L ** 3)
        assert result == pytest.approx(expected)

    def test_coupled_added_mass(self) -> None:
        # k=4 for linear-rotational coupling
        rho = 1025.0
        L = 5.0
        nondim = 0.5

        result = WamitReferenceLoader._redimensionalize_wamit(
            nondim_value=nondim,
            rho=rho,
            characteristic_length=L,
            omega=1.0,
            coefficient_type="added_mass_coupled",
        )

        expected = nondim * rho * (L ** 4)
        assert result == pytest.approx(expected)

    def test_rotational_added_mass(self) -> None:
        # k=5 for rotational-rotational coupling
        rho = 1025.0
        L = 3.0
        nondim = 1.5

        result = WamitReferenceLoader._redimensionalize_wamit(
            nondim_value=nondim,
            rho=rho,
            characteristic_length=L,
            omega=1.0,
            coefficient_type="added_mass_rotational",
        )

        expected = nondim * rho * (L ** 5)
        assert result == pytest.approx(expected)

    def test_zero_nondim_gives_zero(self) -> None:
        result = WamitReferenceLoader._redimensionalize_wamit(
            nondim_value=0.0,
            rho=1025.0,
            characteristic_length=5.0,
            omega=1.0,
            coefficient_type="added_mass_linear",
        )

        assert result == pytest.approx(0.0)


# ---------------------------------------------------------------------------
# 4. test_redimensionalize_damping
# ---------------------------------------------------------------------------


class TestRedimensionalizeDamping:
    """Verify non-dim to SI conversion for damping coefficients."""

    def test_linear_damping(self) -> None:
        # B = B_bar * rho * omega * L^3
        rho = 1025.0
        L = 5.0
        omega = 0.8
        nondim = 1.2

        result = WamitReferenceLoader._redimensionalize_wamit(
            nondim_value=nondim,
            rho=rho,
            characteristic_length=L,
            omega=omega,
            coefficient_type="damping_linear",
        )

        expected = nondim * rho * omega * (L ** 3)
        assert result == pytest.approx(expected)

    def test_coupled_damping(self) -> None:
        rho = 1025.0
        L = 4.0
        omega = 1.2
        nondim = 0.3

        result = WamitReferenceLoader._redimensionalize_wamit(
            nondim_value=nondim,
            rho=rho,
            characteristic_length=L,
            omega=omega,
            coefficient_type="damping_coupled",
        )

        expected = nondim * rho * omega * (L ** 4)
        assert result == pytest.approx(expected)

    def test_rotational_damping(self) -> None:
        rho = 1025.0
        L = 3.0
        omega = 0.5
        nondim = 0.8

        result = WamitReferenceLoader._redimensionalize_wamit(
            nondim_value=nondim,
            rho=rho,
            characteristic_length=L,
            omega=omega,
            coefficient_type="damping_rotational",
        )

        expected = nondim * rho * omega * (L ** 5)
        assert result == pytest.approx(expected)

    def test_damping_omega_dependency(self) -> None:
        """Damping scales linearly with omega at fixed non-dim value."""
        rho = 1025.0
        L = 5.0
        nondim = 1.0

        result_low = WamitReferenceLoader._redimensionalize_wamit(
            nondim, rho, L, omega=0.5, coefficient_type="damping_linear"
        )
        result_high = WamitReferenceLoader._redimensionalize_wamit(
            nondim, rho, L, omega=1.0, coefficient_type="damping_linear"
        )

        assert result_high == pytest.approx(2.0 * result_low)

    def test_unknown_coefficient_type_raises(self) -> None:
        with pytest.raises(ValueError, match="Unknown coefficient_type"):
            WamitReferenceLoader._redimensionalize_wamit(
                nondim_value=1.0,
                rho=1025.0,
                characteristic_length=5.0,
                omega=1.0,
                coefficient_type="invalid_type",
            )


# ---------------------------------------------------------------------------
# 5. test_from_yaml_infinite_depth
# ---------------------------------------------------------------------------


class TestFromYamlInfiniteDepth:
    """Test 'infinite' water depth handling in YAML."""

    def test_string_infinite(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        data["water_depth"] = "infinite"
        path = _write_yaml(tmp_path / "inf.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert result.water_depth == float("inf")

    def test_string_inf(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        data["water_depth"] = "inf"
        path = _write_yaml(tmp_path / "inf2.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert result.water_depth == float("inf")

    def test_string_deep(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        data["water_depth"] = "deep"
        path = _write_yaml(tmp_path / "deep.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert result.water_depth == float("inf")

    def test_numeric_depth_preserved(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        data["water_depth"] = 100.5
        path = _write_yaml(tmp_path / "numeric.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert result.water_depth == pytest.approx(100.5)

    def test_default_depth_is_infinite(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        del data["water_depth"]
        path = _write_yaml(tmp_path / "nowd.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert result.water_depth == float("inf")


# ---------------------------------------------------------------------------
# 6. test_from_yaml_missing_raos_raises
# ---------------------------------------------------------------------------


class TestFromYamlMissingRaisesError:
    """Missing required fields raise ValueError."""

    def test_missing_vessel_name(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        del data["vessel_name"]
        path = _write_yaml(tmp_path / "bad.yml", data)

        with pytest.raises(ValueError, match="vessel_name"):
            WamitReferenceLoader.from_yaml(path)

    def test_missing_frequencies(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        del data["frequencies_rad_s"]
        path = _write_yaml(tmp_path / "bad.yml", data)

        with pytest.raises(ValueError, match="frequencies_rad_s"):
            WamitReferenceLoader.from_yaml(path)

    def test_missing_headings(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        del data["headings_deg"]
        path = _write_yaml(tmp_path / "bad.yml", data)

        with pytest.raises(ValueError, match="headings_deg"):
            WamitReferenceLoader.from_yaml(path)

    def test_missing_raos(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        del data["raos"]
        path = _write_yaml(tmp_path / "bad.yml", data)

        with pytest.raises(ValueError, match="raos"):
            WamitReferenceLoader.from_yaml(path)

    def test_missing_magnitude_in_dof(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        del data["raos"]["heave"]["magnitude"]
        path = _write_yaml(tmp_path / "bad.yml", data)

        with pytest.raises(ValueError, match="magnitude"):
            WamitReferenceLoader.from_yaml(path)

    def test_missing_phase_in_dof(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data()
        del data["raos"]["heave"]["phase"]
        path = _write_yaml(tmp_path / "bad.yml", data)

        with pytest.raises(ValueError, match="phase"):
            WamitReferenceLoader.from_yaml(path)

    def test_shape_mismatch_raises(self, tmp_path: Path) -> None:
        data = _minimal_yaml_data(nfreq=3, nhead=1)
        # Give magnitude wrong number of rows
        data["raos"]["heave"]["magnitude"] = [[1.0], [2.0]]  # 2 rows instead of 3
        path = _write_yaml(tmp_path / "bad.yml", data)

        with pytest.raises(ValueError, match="shape"):
            WamitReferenceLoader.from_yaml(path)

    def test_file_not_found_raises(self) -> None:
        with pytest.raises(FileNotFoundError):
            WamitReferenceLoader.from_yaml(Path("/nonexistent/file.yml"))

    def test_non_mapping_yaml_raises(self, tmp_path: Path) -> None:
        path = tmp_path / "list.yml"
        with open(path, "w") as f:
            f.write("- item1\n- item2\n")

        with pytest.raises(ValueError, match="mapping"):
            WamitReferenceLoader.from_yaml(path)


# ---------------------------------------------------------------------------
# 7. test_build_results_shapes
# ---------------------------------------------------------------------------


class TestBuildResultsShapes:
    """Verify output array shapes match freq x heading."""

    @pytest.mark.parametrize(
        "nfreq,nhead",
        [(1, 1), (5, 3), (10, 7), (20, 1)],
        ids=["1x1", "5x3", "10x7", "20x1"],
    )
    def test_rao_shapes(self, tmp_path: Path, nfreq: int, nhead: int) -> None:
        data = _minimal_yaml_data()
        freqs = [0.1 * (i + 1) for i in range(nfreq)]
        headings = [float(h * 30) for h in range(nhead)]
        data["frequencies_rad_s"] = freqs
        data["headings_deg"] = headings
        data["raos"]["heave"]["magnitude"] = [
            [1.0 / (i + 1)] * nhead for i in range(nfreq)
        ]
        data["raos"]["heave"]["phase"] = [
            [0.0] * nhead for i in range(nfreq)
        ]
        path = _write_yaml(tmp_path / "shaped.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        for dof_name in ("surge", "sway", "heave", "roll", "pitch", "yaw"):
            component = getattr(result.raos, dof_name)
            assert component.magnitude.shape == (nfreq, nhead), (
                f"{dof_name} magnitude shape"
            )
            assert component.phase.shape == (nfreq, nhead), (
                f"{dof_name} phase shape"
            )

    @pytest.mark.parametrize(
        "nfreq,nhead",
        [(1, 1), (5, 3), (10, 7)],
        ids=["1x1", "5x3", "10x7"],
    )
    def test_matrix_shapes_zero_fill(
        self, tmp_path: Path, nfreq: int, nhead: int
    ) -> None:
        """When no added_mass/damping provided, zero matrices match nfreq."""
        data = _minimal_yaml_data()
        freqs = [0.1 * (i + 1) for i in range(nfreq)]
        headings = [float(h * 30) for h in range(nhead)]
        data["frequencies_rad_s"] = freqs
        data["headings_deg"] = headings
        data["raos"]["heave"]["magnitude"] = [
            [1.0] * nhead for _ in range(nfreq)
        ]
        data["raos"]["heave"]["phase"] = [
            [0.0] * nhead for _ in range(nfreq)
        ]
        path = _write_yaml(tmp_path / "shaped.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert len(result.added_mass.matrices) == nfreq
        assert len(result.damping.matrices) == nfreq
        for mat in result.added_mass.matrices:
            assert mat.matrix.shape == (6, 6)
        for mat in result.damping.matrices:
            assert mat.matrix.shape == (6, 6)

    def test_frequency_data_consistency(self, tmp_path: Path) -> None:
        data = _full_yaml_data(nfreq=4, nhead=2)
        path = _write_yaml(tmp_path / "freq.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        freq_values = result.raos.heave.frequencies.values
        assert len(freq_values) == 4
        assert result.raos.heave.frequencies.count == 4
        np.testing.assert_allclose(
            freq_values, [0.2, 0.4, 0.6, 0.8], rtol=1e-12
        )

    def test_heading_data_consistency(self, tmp_path: Path) -> None:
        data = _full_yaml_data(nfreq=3, nhead=2)
        path = _write_yaml(tmp_path / "head.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        head_values = result.raos.heave.headings.values
        assert len(head_values) == 2
        np.testing.assert_allclose(head_values, [0.0, 90.0], rtol=1e-12)

    def test_rao_set_type(self, tmp_path: Path) -> None:
        data = _full_yaml_data(nfreq=3, nhead=2)
        path = _write_yaml(tmp_path / "type.yml", data)

        result = WamitReferenceLoader.from_yaml(path)

        assert isinstance(result.raos, RAOSet)
        assert isinstance(result.added_mass, AddedMassSet)
        assert isinstance(result.damping, DampingSet)
