"""
Phase 1 Integration Test - End-to-End Workflow Validation

This test validates the complete Phase 1 workflow:
1. Load mooring component from database
2. Use component in catenary solver
3. Validate results against expected values

NOTE: These tests are skipped until marine_engineering.mooring modules are implemented.
"""

import sys
from pathlib import Path

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import numpy as np
import pytest

# Skip entire module if required modules not available
pytest.importorskip("digitalmodel.marine_engineering.mooring.catenary",
                    reason="digitalmodel.marine_engineering.mooring.catenary not implemented yet")

from digitalmodel.marine_engineering.mooring.catenary import CatenarySolver
from digitalmodel.marine_engineering.mooring.component_database import ComponentDatabase


class TestPhase1Integration:
    """Integration tests for Phase 1 Marine Engineering modules"""

    @pytest.fixture
    def database(self):
        """Load component database"""
        db = ComponentDatabase()
        db.load_default_database()
        return db

    @pytest.fixture
    def solver(self):
        """Create catenary solver instance"""
        return CatenarySolver(max_iterations=100, tolerance=1e-6)

    def test_chain_catenary_workflow(self, database, solver):
        """Test complete workflow: database -> catenary solver"""
        # Step 1: Get chain from database
        available_chains = database.list_chains(min_diameter=50, max_diameter=100)
        assert len(available_chains) > 0, "No chains available in size range"

        # Use first available chain
        chain = available_chains[0]
        print(f"\nUsing chain: {chain['description']}")
        print(f"  Diameter: {chain['nominal_diameter']}mm")
        print(f"  MBL: {chain['mbl']/1e6:.1f} MN")
        print(f"  Mass: {chain['mass_per_meter']:.1f} kg/m")

        # Step 2: Setup catenary problem with chain properties
        line_length = 500.0  # meters
        horizontal_distance = 400.0  # meters
        water_depth = 100.0  # meters

        # Calculate unit weight in water
        submerged_weight = chain['mass_per_meter'] * 9.81 * 0.87  # 87% for steel in seawater

        # Step 3: Solve catenary
        results = solver.solve(
            line_length=line_length,
            horizontal_distance=horizontal_distance,
            water_depth=water_depth,
            unit_weight=submerged_weight,
            EA=chain['axial_stiffness'],
            pretension=chain['mbl'] * 0.3  # 30% of MBL
        )

        # Step 4: Validate results
        assert results.converged, "Catenary solver did not converge"
        print(f"\nCatenary Results:")
        print(f"  Horizontal tension: {results.horizontal_tension/1e6:.2f} MN")
        print(f"  Total tension at fairlead: {results.total_tension_fairlead/1e6:.2f} MN")
        print(f"  Elongation: {results.elongation*100:.2f}%")

        # Check tensions are within chain capacity
        safety_factor = chain['mbl'] / results.total_tension_fairlead
        assert safety_factor > 1.5, f"Safety factor {safety_factor:.2f} < 1.5"
        print(f"  Safety factor: {safety_factor:.2f}")

        # Check physical constraints
        assert results.horizontal_tension > 0
        assert results.vertical_tension_fairlead > 0
        assert results.total_tension_fairlead > results.horizontal_tension
        assert 0 < results.elongation < 0.1  # Less than 10% elongation

    def test_database_lookup_performance(self, database):
        """Test that database lookups are efficient"""
        import time

        # Warm up
        _ = database.list_chains()

        # Time multiple lookups
        start = time.perf_counter()
        for _ in range(100):
            chains = database.list_chains(min_diameter=50, max_diameter=100)
        elapsed = time.perf_counter() - start

        avg_time = elapsed / 100
        print(f"\nDatabase lookup performance: {avg_time*1000:.2f} ms average")
        assert avg_time < 0.01, f"Database lookup too slow: {avg_time*1000:.1f} ms"

    def test_catenary_solver_convergence(self, solver):
        """Test catenary solver converges for various configurations"""
        test_cases = [
            # (line_length, horizontal_distance, water_depth, expected_convergence)
            (500, 400, 100, True),   # Normal case
            (300, 250, 50, True),    # Shallow case
            (1000, 800, 200, True),  # Deep case
            (200, 150, 100, True),   # Short line
        ]

        unit_weight = 500  # N/m (typical for 76mm chain)
        EA = 1.164e9  # N (typical stiffness)

        results_summary = []
        for line_len, h_dist, depth, should_converge in test_cases:
            results = solver.solve(
                line_length=line_len,
                horizontal_distance=h_dist,
                water_depth=depth,
                unit_weight=unit_weight,
                EA=EA
            )

            results_summary.append({
                'config': f"L={line_len}, H={h_dist}, D={depth}",
                'converged': results.converged,
                'iterations': results.iterations if hasattr(results, 'iterations') else 'N/A'
            })

            if should_converge:
                assert results.converged, f"Failed to converge for L={line_len}, H={h_dist}, D={depth}"

        # Print summary
        print("\nConvergence Summary:")
        for res in results_summary:
            status = "✓" if res['converged'] else "✗"
            print(f"  {status} {res['config']}: {res['iterations']} iterations")

    def test_material_properties_consistency(self, database):
        """Verify that material properties are physically consistent"""
        chains = database.list_chains()

        for chain in chains:
            # Basic property checks
            assert chain['nominal_diameter'] > 0
            assert chain['mbl'] > 0
            assert chain['mass_per_meter'] > 0
            assert chain['axial_stiffness'] > 0

            # Physical relationships
            diameter_m = chain['nominal_diameter'] / 1000.0
            area = np.pi * (diameter_m ** 2) / 4

            # Steel density check (approximately)
            expected_mass = 7850 * area  # kg/m for solid steel
            mass_ratio = chain['mass_per_meter'] / expected_mass

            # Chain is hollow, so mass should be 20-50% of solid steel
            assert 0.15 < mass_ratio < 0.6, (
                f"Unexpected mass ratio {mass_ratio:.2f} for {chain['description']}"
            )

    def test_integration_with_typical_mooring_system(self, database, solver):
        """Test a realistic mooring system configuration"""
        # Typical deepwater mooring configuration
        water_depth = 1500.0  # meters
        horizontal_offset = 1200.0  # meters

        # Find suitable chain
        chains = database.list_chains(min_diameter=76, max_diameter=120)
        assert len(chains) > 0, "No suitable chains found for deepwater mooring"

        chain = chains[0]

        # Calculate required line length (typically 1.5-2x water depth for catenary)
        line_length = 2.0 * water_depth

        # Solve catenary
        submerged_weight = chain['mass_per_meter'] * 9.81 * 0.87
        results = solver.solve(
            line_length=line_length,
            horizontal_distance=horizontal_offset,
            water_depth=water_depth,
            unit_weight=submerged_weight,
            EA=chain['axial_stiffness'],
            pretension=chain['mbl'] * 0.2  # 20% pretension
        )

        # Validation
        assert results.converged, "Deepwater catenary failed to converge"

        # Check safety factor
        safety_factor = chain['mbl'] / results.total_tension_fairlead
        print(f"\nDeepwater Mooring System:")
        print(f"  Water depth: {water_depth} m")
        print(f"  Horizontal offset: {horizontal_offset} m")
        print(f"  Chain: {chain['description']}")
        print(f"  Max tension: {results.total_tension_fairlead/1e6:.2f} MN")
        print(f"  Chain capacity: {chain['mbl']/1e6:.2f} MN")
        print(f"  Safety factor: {safety_factor:.2f}")

        assert safety_factor > 1.8, (
            f"Insufficient safety factor {safety_factor:.2f} for deepwater mooring"
        )


if __name__ == "__main__":
    # Run tests with verbose output
    pytest.main([__file__, "-v", "--tb=short", "-s"])
