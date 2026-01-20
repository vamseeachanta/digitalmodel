"""Comprehensive RAO Verification Across All Reading Routes.

This script verifies RAO reading consistency across:
1. NEW Unified Reader (v2.0) - All RAO types
2. Legacy AQWAReader (text parsing)
3. Legacy RAODataProcessor (with enhanced parser)

Tests real AQWA .lis files from the repository.
"""

import sys
import numpy as np
import pandas as pd
from pathlib import Path
from datetime import datetime
from typing import Dict, Any, List, Tuple

# Add source to path
repo_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(repo_root / 'src'))

# Import all reading routes
from digitalmodel.modules.marine_analysis import (
    # NEW v2.0 Unified Reader
    UnifiedRAOReader,
    read_rao_file,
    RAOType,

    # Legacy readers
    RAODataProcessor,
    AQWAReader as LegacyAQWAReader
)


class RAOVerification:
    """Verify RAO data consistency across all reading routes."""

    def __init__(self):
        """Initialize verification with all readers."""
        self.unified_reader = UnifiedRAOReader()
        self.legacy_processor = RAODataProcessor()

        # Find test files
        self.test_files = self._find_test_files()

    def _find_test_files(self) -> Dict[str, str]:
        """Find AQWA .lis files for testing."""
        test_files = {}

        # Primary test file (ship RAOs)
        ship_rao_files = [
            repo_root / 'specs' / 'modules' / 'aqwa' / 'ship-analysis' / 'go-by-ship-raos' / '001_SHIP_RAOS_REV3.LIS',
            repo_root / 'docs' / 'modules' / 'aqwa' / 'examples' / '03_dat' / '001_ship_raos' / '001_SHIP_RAOS.LIS',
            repo_root / 'docs' / 'modules' / 'orcawave' / 'L01_aqwa_benchmark' / '001_SHIP_RAOS_REV2.LIS'
        ]

        for f in ship_rao_files:
            if f.exists():
                test_files['ship_rao'] = str(f)
                break

        # FST files (floating structures)
        fst_file = repo_root / 'specs' / 'modules' / 'aqwa_to_orcaflex' / 'input' / 'FST2L015_FST1L015_HWL.LIS'
        if fst_file.exists():
            test_files['fst_hwl'] = str(fst_file)

        return test_files

    def verify_file(self, file_path: str, file_name: str) -> Dict[str, Any]:
        """Verify a single file across all reading routes.

        Args:
            file_path: Path to .lis file
            file_name: Descriptive name for reporting

        Returns:
            Verification results dictionary
        """
        print(f"\n{'='*80}")
        print(f"Verifying: {file_name}")
        print(f"File: {file_path}")
        print(f"{'='*80}\n")

        results = {
            'file_name': file_name,
            'file_path': file_path,
            'timestamp': datetime.now().isoformat(),
            'routes': {}
        }

        # Route 1: NEW Unified Reader (v2.0)
        print("[Route] Route 1: Unified Reader (v2.0 - NEW)")
        try:
            unified_data = self.unified_reader.read(file_path)
            results['routes']['unified_v2'] = self._extract_unified_data(unified_data)
            print(f"  [OK] Success - Available types: {[t.value for t in unified_data.get_available_types()]}")
        except Exception as e:
            results['routes']['unified_v2'] = {'error': str(e)}
            print(f"  [FAIL] Failed: {e}")

        # Route 2: Legacy RAODataProcessor
        print("\n[Route] Route 2: Legacy RAODataProcessor (v1.x)")
        try:
            legacy_data = self.legacy_processor.import_aqwa_lis_file(file_path)
            results['routes']['legacy_processor'] = self._extract_legacy_data(legacy_data)
            print(f"  [OK] Success - Displacement RAOs extracted")
        except Exception as e:
            results['routes']['legacy_processor'] = {'error': str(e)}
            print(f"  [FAIL] Failed: {e}")

        # Route 3: Convenience function
        print("\n[Route] Route 3: Convenience Function read_rao_file()")
        try:
            convenience_data = read_rao_file(file_path)
            results['routes']['convenience'] = self._extract_unified_data(convenience_data)
            print(f"  [OK] Success - Available types: {[t.value for t in convenience_data.get_available_types()]}")
        except Exception as e:
            results['routes']['convenience'] = {'error': str(e)}
            print(f"  [FAIL] Failed: {e}")

        # Compare results
        comparison = self._compare_results(results['routes'])
        results['comparison'] = comparison

        return results

    def _extract_unified_data(self, rao_data) -> Dict[str, Any]:
        """Extract data from unified reader format."""
        extracted = {
            'available_types': [t.value for t in rao_data.get_available_types()],
            'metadata': {
                'source_file': rao_data.metadata.source_file,
                'vessel_name': rao_data.metadata.vessel_name,
                'format': rao_data.metadata.source_format.value
            }
        }

        # Extract displacement RAOs
        if rao_data.has_displacement():
            disp = rao_data.displacement
            extracted['displacement'] = {
                'n_frequencies': len(disp.frequencies),
                'n_headings': len(disp.headings),
                'frequencies': disp.frequencies.tolist(),
                'headings': disp.headings.tolist(),
                'surge': {
                    'amplitude': {
                        'shape': disp.surge.amplitude.shape,
                        'min': float(np.min(disp.surge.amplitude)),
                        'max': float(np.max(disp.surge.amplitude)),
                        'mean': float(np.mean(disp.surge.amplitude)),
                        'sample_values': disp.surge.amplitude[0, :3].tolist()  # First freq, first 3 headings
                    },
                    'phase': {
                        'shape': disp.surge.phase.shape,
                        'min': float(np.min(disp.surge.phase)),
                        'max': float(np.max(disp.surge.phase)),
                        'sample_values': disp.surge.phase[0, :3].tolist()
                    }
                },
                'heave': {
                    'amplitude': {
                        'shape': disp.heave.amplitude.shape,
                        'min': float(np.min(disp.heave.amplitude)),
                        'max': float(np.max(disp.heave.amplitude)),
                        'mean': float(np.mean(disp.heave.amplitude))
                    }
                }
            }

        # Extract velocity RAOs if available
        if rao_data.has_velocity():
            vel = rao_data.velocity
            extracted['velocity'] = {
                'n_frequencies': len(vel.frequencies),
                'n_headings': len(vel.headings),
                'surge_amplitude_stats': {
                    'min': float(np.min(vel.surge.amplitude)),
                    'max': float(np.max(vel.surge.amplitude)),
                    'mean': float(np.mean(vel.surge.amplitude))
                }
            }

        # Extract acceleration RAOs if available
        if rao_data.has_acceleration():
            acc = rao_data.acceleration
            extracted['acceleration'] = {
                'n_frequencies': len(acc.frequencies),
                'n_headings': len(acc.headings),
                'surge_amplitude_stats': {
                    'min': float(np.min(acc.surge.amplitude)),
                    'max': float(np.max(acc.surge.amplitude)),
                    'mean': float(np.mean(acc.surge.amplitude))
                }
            }

        return extracted

    def _extract_legacy_data(self, rao_data) -> Dict[str, Any]:
        """Extract data from legacy reader format."""
        extracted = {
            'available_types': ['displacement'],  # Legacy only has displacement
            'n_frequencies': len(rao_data.frequencies),
            'n_headings': len(rao_data.headings),
            'frequencies': rao_data.frequencies.tolist(),
            'headings': rao_data.headings.tolist(),
            'displacement': {
                'surge': {
                    'amplitude': {
                        'shape': rao_data.raos['surge']['amplitude'].shape,
                        'min': float(np.min(rao_data.raos['surge']['amplitude'])),
                        'max': float(np.max(rao_data.raos['surge']['amplitude'])),
                        'mean': float(np.mean(rao_data.raos['surge']['amplitude'])),
                        'sample_values': rao_data.raos['surge']['amplitude'][0, :3].tolist()
                    },
                    'phase': {
                        'shape': rao_data.raos['surge']['phase'].shape,
                        'min': float(np.min(rao_data.raos['surge']['phase'])),
                        'max': float(np.max(rao_data.raos['surge']['phase'])),
                        'sample_values': rao_data.raos['surge']['phase'][0, :3].tolist()
                    }
                },
                'heave': {
                    'amplitude': {
                        'shape': rao_data.raos['heave']['amplitude'].shape,
                        'min': float(np.min(rao_data.raos['heave']['amplitude'])),
                        'max': float(np.max(rao_data.raos['heave']['amplitude'])),
                        'mean': float(np.mean(rao_data.raos['heave']['amplitude']))
                    }
                }
            }
        }

        return extracted

    def _compare_results(self, routes: Dict[str, Any]) -> Dict[str, Any]:
        """Compare results across different reading routes."""
        comparison = {
            'all_succeeded': all('error' not in route for route in routes.values()),
            'displacement_consistency': None,
            'differences': []
        }

        # Check if at least two routes succeeded
        successful_routes = {name: data for name, data in routes.items() if 'error' not in data}

        if len(successful_routes) < 2:
            comparison['note'] = "Not enough successful routes to compare"
            return comparison

        # Compare displacement RAOs
        if self._routes_have_displacement(successful_routes):
            comparison['displacement_consistency'] = self._check_displacement_consistency(successful_routes)

        return comparison

    def _routes_have_displacement(self, routes: Dict[str, Any]) -> bool:
        """Check if routes have displacement data."""
        for route_data in routes.values():
            if 'displacement' not in route_data:
                return False
        return True

    def _check_displacement_consistency(self, routes: Dict[str, Any]) -> Dict[str, Any]:
        """Check consistency of displacement RAOs across routes."""
        consistency = {
            'frequencies_match': True,
            'headings_match': True,
            'amplitudes_match': True,
            'details': []
        }

        # Get first route as reference
        route_names = list(routes.keys())
        ref_name = route_names[0]
        ref_data = routes[ref_name].get('displacement', routes[ref_name])
        
        # Handle different data structures
        ref_n_freq = ref_data.get('n_frequencies', len(ref_data.get('frequencies', [])))
        ref_n_head = ref_data.get('n_headings', len(ref_data.get('headings', [])))

        for route_name in route_names[1:]:
            route_data = routes[route_name].get('displacement', routes[route_name])
            
            # Handle different data structures
            route_n_freq = route_data.get('n_frequencies', len(route_data.get('frequencies', [])))
            route_n_head = route_data.get('n_headings', len(route_data.get('headings', [])))

            # Compare frequencies
            if ref_n_freq != route_n_freq:
                consistency['frequencies_match'] = False
                consistency['details'].append(
                    f"Frequency count mismatch: {ref_name}={ref_n_freq}, "
                    f"{route_name}={route_n_freq}"
                )

            # Compare headings
            if ref_n_head != route_n_head:
                consistency['headings_match'] = False
                consistency['details'].append(
                    f"Heading count mismatch: {ref_name}={ref_n_head}, "
                    f"{route_name}={route_n_head}"
                )

            # Compare surge amplitude statistics
            ref_surge = ref_data['surge']['amplitude']
            route_surge = route_data['surge']['amplitude']

            # Check if statistics are close (within 1%)
            for stat in ['min', 'max', 'mean']:
                ref_val = ref_surge[stat]
                route_val = route_surge[stat]
                if not np.isclose(ref_val, route_val, rtol=0.01):
                    consistency['amplitudes_match'] = False
                    consistency['details'].append(
                        f"Surge amplitude {stat} mismatch: {ref_name}={ref_val:.6f}, "
                        f"{route_name}={route_val:.6f} (diff={abs(ref_val-route_val):.6f})"
                    )

        return consistency

    def run_all_verifications(self) -> List[Dict[str, Any]]:
        """Run verifications on all test files."""
        print("\n" + "="*80)
        print("AQWA RAO VERIFICATION - ALL READING ROUTES")
        print("="*80)
        print(f"\nFound {len(self.test_files)} test file(s)")

        all_results = []

        for file_name, file_path in self.test_files.items():
            result = self.verify_file(file_path, file_name)
            all_results.append(result)

        return all_results

    def print_summary(self, all_results: List[Dict[str, Any]]):
        """Print verification summary."""
        print("\n" + "="*80)
        print("VERIFICATION SUMMARY")
        print("="*80)

        for result in all_results:
            print(f"\n[File] {result['file_name']}:")

            routes = result['routes']
            print(f"  Routes tested: {len(routes)}")
            print(f"  Successful: {sum(1 for r in routes.values() if 'error' not in r)}")
            print(f"  Failed: {sum(1 for r in routes.values() if 'error' in r)}")

            if result['comparison']['all_succeeded']:
                print(f"  [OK] All routes succeeded")

                if result['comparison'].get('displacement_consistency'):
                    cons = result['comparison']['displacement_consistency']
                    if cons['frequencies_match'] and cons['headings_match'] and cons['amplitudes_match']:
                        print(f"  [OK] Displacement RAOs consistent across all routes")
                    else:
                        print(f"  [WARN]  Some inconsistencies found:")
                        for detail in cons['details']:
                            print(f"      - {detail}")
            else:
                print(f"  [WARN]  Some routes failed")
                for route_name, route_data in routes.items():
                    if 'error' in route_data:
                        print(f"      [FAIL] {route_name}: {route_data['error']}")

    def export_results(self, all_results: List[Dict[str, Any]], output_path: str):
        """Export results to JSON file."""
        import json

        output_file = Path(output_path)
        output_file.parent.mkdir(parents=True, exist_ok=True)

        with open(output_file, 'w') as f:
            json.dump(all_results, f, indent=2)

        print(f"\n[Export] Results exported to: {output_file}")


def main():
    """Run verification and generate report."""
    verifier = RAOVerification()

    # Run all verifications
    results = verifier.run_all_verifications()

    # Print summary
    verifier.print_summary(results)

    # Export results
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_path = repo_root / 'tests' / 'marine_engineering' / f'verification_results_{timestamp}.json'
    verifier.export_results(results, str(output_path))

    print(f"\n{'='*80}")
    print("VERIFICATION COMPLETE")
    print(f"{'='*80}\n")


if __name__ == "__main__":
    main()
