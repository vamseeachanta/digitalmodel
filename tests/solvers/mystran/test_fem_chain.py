#!/usr/bin/env python3
"""
ABOUTME: Tests for the MYSTRAN chain — structured mesh generators, BDF setup,
solver detection, and (when mystran is on PATH) cantilever validation and a
mesh-convergence sweep against PL^3/3EI.
"""

import numpy as np
import pytest

from digitalmodel.solvers.mystran.fem_chain import (
    MystranChain, find_mystran, is_mystran_available,
)

needs_mystran = pytest.mark.skipif(
    not is_mystran_available(), reason="Requires MYSTRAN (mystran on PATH)"
)


class TestSolverDetection:

    def test_is_available_returns_bool(self):
        assert isinstance(is_mystran_available(), bool)

    def test_env_override(self, tmp_path, monkeypatch):
        exe = tmp_path / "mystran-custom.exe"
        exe.write_text("")
        monkeypatch.setenv("MYSTRAN_EXE", str(exe))
        assert find_mystran() == str(exe)

    def test_env_override_missing_falls_back(self, tmp_path, monkeypatch):
        monkeypatch.setenv("MYSTRAN_EXE", str(tmp_path / "missing.exe"))
        monkeypatch.setenv("PATH", str(tmp_path))
        assert find_mystran() is None


class TestHexMesh:

    def test_counts_2x1x1(self, tmp_path):
        chain = MystranChain(tmp_path)
        stats = chain.create_cantilever_hex_mesh(1.0, 0.1, 0.1, 2, 1, 1)
        assert stats == {"n_nodes": 12, "n_elements": 2}
        sets = chain.node_sets
        assert len(sets["FIX"]) == 4
        assert len(sets["TIP"]) == 4
        assert len(sets["TIP_CENTER"]) == 1

    def test_counts_general(self, tmp_path):
        chain = MystranChain(tmp_path)
        stats = chain.create_cantilever_hex_mesh(2.0, 0.2, 0.3, 8, 2, 3)
        assert stats["n_nodes"] == 9 * 3 * 4
        assert stats["n_elements"] == 8 * 2 * 3

    def test_node_sets_geometry(self, tmp_path):
        chain = MystranChain(tmp_path)
        chain.create_cantilever_hex_mesh(1.0, 0.1, 0.1, 4, 2, 2)
        nodes = chain._nodes
        assert np.allclose(nodes[chain.node_sets["FIX"], 0], 0.0)
        assert np.allclose(nodes[chain.node_sets["TIP"], 0], 1.0)
        tc = nodes[chain.node_sets["TIP_CENTER"][0]]
        assert np.allclose(tc, [1.0, 0.05, 0.05])

    def test_connectivity_valid_and_ordered(self, tmp_path):
        chain = MystranChain(tmp_path)
        chain.create_cantilever_hex_mesh(1.0, 0.1, 0.1, 3, 2, 2)
        conn = chain._elements["Hexahedron 8"]["connectivity"]
        nodes = chain._nodes
        assert conn.min() == 0
        assert conn.max() == len(nodes) - 1
        for row in conn:
            z = nodes[row, 2]
            # bottom four share z, top four share z + dz
            assert np.allclose(z[:4], z[0])
            assert np.allclose(z[4:], z[0] + 0.05)
            # positive Jacobian: bottom face counter-clockwise in xy
            x, y = nodes[row[:4], 0], nodes[row[:4], 1]
            area = 0.5 * sum(
                x[i] * y[(i + 1) % 4] - x[(i + 1) % 4] * y[i] for i in range(4)
            )
            assert area > 0

    def test_invalid_divisions_raise(self, tmp_path):
        with pytest.raises(ValueError):
            MystranChain(tmp_path).create_cantilever_hex_mesh(1, 1, 1, 0, 1, 1)


class TestBarMesh:

    def test_counts_and_sets(self, tmp_path):
        chain = MystranChain(tmp_path)
        stats = chain.create_cantilever_bar_mesh(2.0, 5)
        assert stats == {"n_nodes": 6, "n_elements": 5}
        assert chain.node_sets["FIX"] == [0]
        assert chain.node_sets["TIP"] == [5]
        assert np.allclose(chain._nodes[-1], [2.0, 0.0, 0.0])

    def test_invalid_raises(self, tmp_path):
        with pytest.raises(ValueError):
            MystranChain(tmp_path).create_cantilever_bar_mesh(1.0, 0)


class TestSetup:

    def test_setup_writes_bdf(self, tmp_path):
        chain = MystranChain(tmp_path)
        chain.create_cantilever_hex_mesh(1.0, 0.1, 0.1, 2, 1, 1)
        bdf = chain.setup_analysis(
            material={"name": "STEEL", "E": 210e9, "nu": 0.3},
            sections=[{"element_type": "Hexahedron 8", "type": "solid"}],
            loads=[{"node_set": "TIP", "magnitude": 1000.0, "direction": (0, 0, 1),
                    "distribute": True}],
            boundary_conditions=[{"node_set": "FIX", "dofs": "123"}],
        )
        assert bdf.exists() and bdf.suffix == ".bdf"
        text = bdf.read_text()
        lines = text.splitlines()
        assert "SOL 101" in lines
        assert sum(l.startswith("CHEXA,") for l in lines) == 2
        assert "PSOLID,1,1,0,2" in lines
        assert "SPC1,1,123,1,2,3,4" in lines
        forces = [l for l in lines if l.startswith("FORCE,")]
        assert len(forces) == 4
        assert all(l.endswith(",250.0,0.0,0.0,1.0") for l in forces)

    def test_setup_bar_section(self, tmp_path):
        chain = MystranChain(tmp_path)
        chain.create_cantilever_bar_mesh(1.0, 2)
        bdf = chain.setup_analysis(
            material={"name": "STEEL", "E": 210e9, "nu": 0.3},
            sections=[{"element_type": "Line 2", "type": "bar",
                       "A": 0.01, "I1": 8.3333e-6, "I2": 8.3333e-6, "J": 1.4e-5,
                       "orientation": (0, 1, 0)}],
            loads=[{"node_set": "TIP", "magnitude": 1000.0, "direction": (0, 0, 1)}],
            boundary_conditions=[{"node_set": "FIX", "dofs": "123456"}],
        )
        lines = bdf.read_text().splitlines()
        assert "CBAR,1,1,1,2,0.0,1.0,0.0" in lines
        assert "SPC1,1,123456,1" in lines
        assert "FORCE,1,3,,1000.0,0.0,0.0,1.0" in lines

    def test_setup_without_mesh_raises(self, tmp_path):
        with pytest.raises(RuntimeError, match="No mesh"):
            MystranChain(tmp_path).setup_analysis(
                {"name": "M", "E": 1, "nu": 0.3}, [], [], [])

    def test_unknown_node_set_raises(self, tmp_path):
        chain = MystranChain(tmp_path)
        chain.create_cantilever_bar_mesh(1.0, 1)
        with pytest.raises(KeyError, match="Unknown node set"):
            chain.setup_analysis(
                {"name": "M", "E": 1, "nu": 0.3},
                [{"element_type": "Line 2", "type": "bar", "A": 1, "I1": 1, "I2": 1, "J": 1}],
                [{"node_set": "NOPE", "magnitude": 1.0}], [])

    def test_unknown_section_type_raises(self, tmp_path):
        chain = MystranChain(tmp_path)
        chain.create_cantilever_bar_mesh(1.0, 1)
        with pytest.raises(ValueError, match="Unknown section type"):
            chain.setup_analysis(
                {"name": "M", "E": 1, "nu": 0.3},
                [{"element_type": "Line 2", "type": "spring"}], [], [])

    def test_solve_before_setup_raises(self, tmp_path):
        with pytest.raises(RuntimeError):
            MystranChain(tmp_path).solve()

    def test_tip_deflection_formula(self):
        # L=1, b=h=0.1, E=210 GPa, P=1 kN -> 1.90476e-4 m
        I = 0.1 * 0.1 ** 3 / 12
        assert MystranChain.cantilever_tip_deflection(1000.0, 1.0, 210e9, I) == pytest.approx(
            1.9047619e-4, rel=1e-6)


@needs_mystran
class TestIntegration:

    def test_cbar_cantilever_matches_analytical(self, tmp_path):
        """AC: CBAR tip deflection within 0.1% of PL^3/3EI (beam element is exact)."""
        r = MystranChain(tmp_path).run_cantilever_bar_validation(n_elems=4)
        assert r["error_pct"] < 0.1, r
        assert r["reaction_sum"]["t3"] == pytest.approx(-1000.0, rel=1e-6)
        assert abs(r["epsilon"]) < 1e-8

    def test_hex_cantilever_coarse_runs(self, tmp_path):
        r = MystranChain(tmp_path).run_cantilever_hex_validation(2, 1, 1)
        assert r["n_elements"] == 2
        assert r["tip_displacement_mean"] > 0
        assert r["max_von_mises"] > 0
        assert r["error_pct"] < 30.0, r

    def test_fatal_error_is_reported_not_swallowed(self, tmp_path):
        """MYSTRAN exits 0 on FATAL errors; the chain must still flag failure."""
        chain = MystranChain(tmp_path)
        chain.create_cantilever_hex_mesh(1.0, 0.1, 0.1, 1, 1, 1)
        bdf = chain.setup_analysis(
            material={"name": "STEEL", "E": 210e9, "nu": 0.3},
            sections=[{"element_type": "Hexahedron 8", "type": "solid"}],
            loads=[{"node_set": "TIP", "magnitude": 1.0}],
            boundary_conditions=[{"node_set": "FIX", "dofs": "123"}],
        )
        # Strip the PSOLID integration order -> MYSTRAN *ERROR 1964
        bdf.write_text(bdf.read_text().replace("PSOLID,1,1,0,2", "PSOLID,1,1"))
        status = chain.solve()
        assert status["success"] is False
        assert status["errors"], status

    def test_mesh_convergence_sweep(self, tmp_path):
        """AC: tip deflection converges monotonically toward PL^3/3EI."""
        study = MystranChain(tmp_path).run_mesh_convergence(
            levels=((4, 1, 1), (8, 2, 2), (16, 2, 4)), tolerance=0.05,
        )
        assert len(study.levels) == 3
        errs = [l.error_vs_reference for l in study.levels]
        assert errs[0] > errs[1] > errs[2], errs
        assert study.is_monotone()
        assert errs[-1] < 0.05, study.summary_table("tip deflection")
