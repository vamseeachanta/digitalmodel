#!/usr/bin/env python3
"""
ABOUTME: AQWA vs OrcaWave RAO Comparison - Heading-by-Heading for All DOFs
Uses displacement/motion RAOs (NOT load RAOs) for proper comparison.
"""

import sys
from pathlib import Path
import numpy as np
from datetime import datetime
from typing import Dict, List, Tuple
import json

# Add src and OrcFxAPI to path
repo_root = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(repo_root / "src"))

orcaflex_api_path = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python"
sys.path.insert(0, orcaflex_api_path)

from digitalmodel.modules.diffraction.aqwa_lis_parser import AQWALISParser
from digitalmodel.modules.diffraction.aqwa_converter import AQWAConverter
from digitalmodel.modules.diffraction.output_schemas import DiffractionResults, DOF
from loguru import logger

try:
    import OrcFxAPI
    logger.info("✓ OrcFxAPI loaded successfully")
except ImportError as e:
    logger.error(f"Failed to import OrcFxAPI: {e}")
    sys.exit(1)


class HeadingByHeadingComparator:
    """Compare AQWA and OrcaWave RAOs heading-by-heading for all DOFs"""

    def __init__(
        self,
        aqwa_results: DiffractionResults,
        orcawave_data: Dict,
        tolerance: float = 0.20  # 20% tolerance
    ):
        self.aqwa = aqwa_results
        self.orcawave = orcawave_data
        self.tolerance = tolerance

    def find_nearest_heading_index(self, target_heading: float, headings: np.ndarray) -> int:
        """Find index of nearest heading"""
        # Normalize to -180 to 180
        target = ((target_heading + 180) % 360) - 180
        headings_norm = ((headings + 180) % 360) - 180
        idx = np.argmin(np.abs(headings_norm - target))
        return idx

    def compare_all_headings(self) -> Dict:
        """
        Compare AQWA vs OrcaWave at all headings for all DOFs

        Returns:
            Dictionary with comparison results for each DOF
        """
        logger.info("\n" + "="*80)
        logger.info("HEADING-BY-HEADING COMPARISON (ALL DOFs)")
        logger.info("="*80)

        # AQWA grid
        aqwa_freq = self.aqwa.raos.surge.frequencies.values
        aqwa_periods = 2 * np.pi / aqwa_freq
        aqwa_headings = self.aqwa.raos.surge.headings.values

        # OrcaWave grid
        ow_freq_hz = self.orcawave['frequencies']  # OrcaWave uses Hz!
        ow_freq = ow_freq_hz * 2 * np.pi  # Convert Hz to rad/s for comparison with AQWA
        ow_periods = self.orcawave['periods']
        ow_headings = self.orcawave['headings']
        ow_raos = self.orcawave['motion_raos']  # Using MOTION RAOs now!

        logger.info(f"\nAQWA: {len(aqwa_freq)} frequencies × {len(aqwa_headings)} headings")
        logger.info(f"  Periods: {aqwa_periods.min():.2f} - {aqwa_periods.max():.2f} s")
        logger.info(f"  Headings: {aqwa_headings.min():.1f}° - {aqwa_headings.max():.1f}°")

        logger.info(f"\nOrcaWave: {len(ow_freq)} frequencies × {len(ow_headings)} headings")
        logger.info(f"  Periods: {ow_periods.min():.2f} - {ow_periods.max():.2f} s")
        logger.info(f"  Frequencies (original Hz): {ow_freq_hz.min():.4f} - {ow_freq_hz.max():.4f} Hz")
        logger.info(f"  Frequencies (converted rad/s): {ow_freq.min():.4f} - {ow_freq.max():.4f} rad/s")
        logger.info(f"  Headings: {ow_headings.min():.1f}° - {ow_headings.max():.1f}°")

        results = {}

        for dof in DOF:
            dof_name = dof.name
            logger.info(f"\n{'='*80}")
            logger.info(f"{dof_name}")
            logger.info(f"{'='*80}")

            # Get AQWA RAO for this DOF
            aqwa_rao = self.aqwa.raos.get_component(dof)
            aqwa_mag = aqwa_rao.magnitude  # (n_freq, n_heading)

            # Get OrcaWave RAO for this DOF
            dof_idx = list(DOF).index(dof)
            ow_rao_complex = ow_raos[:, :, dof_idx]  # (n_heading, n_freq)

            # Compare at all OrcaWave headings
            heading_comparisons = []
            all_errors = []

            for ow_heading_idx, ow_heading in enumerate(ow_headings):
                # Find nearest AQWA heading
                aqwa_heading_idx = self.find_nearest_heading_index(ow_heading, aqwa_headings)
                aqwa_heading_actual = aqwa_headings[aqwa_heading_idx]

                # Get AQWA RAO at this heading: (n_freq,)
                aqwa_rao_heading = aqwa_mag[:, aqwa_heading_idx]

                # Get OrcaWave RAO at this heading: (n_freq,)
                ow_rao_heading = ow_rao_complex[ow_heading_idx, :]
                ow_mag_heading = np.abs(ow_rao_heading)

                # DEBUG: Find peak in ORIGINAL OrcaWave data
                ow_peak_original = np.max(ow_mag_heading)
                ow_peak_original_idx = np.argmax(ow_mag_heading)

                # Interpolate OrcaWave to AQWA frequencies
                ow_mag_interp = np.interp(aqwa_freq, ow_freq, ow_mag_heading)

                # Calculate errors
                abs_errors = np.abs(ow_mag_interp - aqwa_rao_heading)
                rel_errors = abs_errors / np.maximum(aqwa_rao_heading, 1e-10)

                # Find peaks
                aqwa_peak = np.max(aqwa_rao_heading)
                ow_peak = np.max(ow_mag_interp)
                aqwa_peak_freq_idx = np.argmax(aqwa_rao_heading)
                ow_peak_freq_idx = np.argmax(ow_mag_interp)

                peak_diff = abs(ow_peak - aqwa_peak)
                peak_rel_error = peak_diff / aqwa_peak if aqwa_peak > 1e-6 else 0

                # Statistics
                within_tolerance = rel_errors <= self.tolerance
                pass_rate = np.sum(within_tolerance) / len(rel_errors)

                logger.info(f"\n  Heading {ow_heading:.1f}° (AQWA: {aqwa_heading_actual:.1f}°):")
                logger.info(f"    AQWA peak: {aqwa_peak:.4f} at period {aqwa_periods[aqwa_peak_freq_idx]:.2f}s (freq={aqwa_freq[aqwa_peak_freq_idx]:.4f} rad/s)")

                # DEBUG: Show original OrcaWave peak
                logger.info(f"    OrcaWave ORIGINAL peak: {ow_peak_original:.4f} at period {ow_periods[ow_peak_original_idx]:.2f}s (freq={ow_freq[ow_peak_original_idx]:.4f} rad/s)")

                logger.info(f"    OrcaWave INTERPOLATED peak: {ow_peak:.4f} at period {aqwa_periods[ow_peak_freq_idx]:.2f}s (freq={aqwa_freq[ow_peak_freq_idx]:.4f} rad/s)")
                logger.info(f"    Peak difference: {peak_rel_error*100:.1f}%")
                logger.info(f"    Pass rate: {pass_rate*100:.1f}%")

                heading_comparisons.append({
                    'ow_heading': ow_heading,
                    'aqwa_heading': aqwa_heading_actual,
                    'aqwa_peak': aqwa_peak,
                    'aqwa_peak_period': aqwa_periods[aqwa_peak_freq_idx],
                    'ow_peak': ow_peak,
                    'ow_peak_period': aqwa_periods[ow_peak_freq_idx],
                    'peak_difference': peak_diff,
                    'peak_rel_error': peak_rel_error,
                    'mean_error': np.mean(rel_errors),
                    'max_error': np.max(rel_errors),
                    'rms_error': np.sqrt(np.mean(rel_errors**2)),
                    'pass_rate': pass_rate,
                    'n_points': len(rel_errors)
                })

                all_errors.extend(rel_errors)

            # Overall statistics
            all_errors = np.array(all_errors)
            overall_pass_rate = np.sum(all_errors <= self.tolerance) / len(all_errors)

            passes = overall_pass_rate >= 0.90
            status = "✅ PASS" if passes else "❌ FAIL"

            logger.info(f"\n  Overall pass rate: {overall_pass_rate*100:.1f}%")
            logger.info(f"  Status: {status}")

            results[dof_name] = {
                'heading_comparisons': heading_comparisons,
                'overall_mean_error': np.mean(all_errors),
                'overall_max_error': np.max(all_errors),
                'overall_rms_error': np.sqrt(np.mean(all_errors**2)),
                'overall_pass_rate': overall_pass_rate,
                'passes': passes,
                'status': status
            }

        logger.info("\n" + "="*80)
        logger.info("✓ Heading-by-heading comparison complete!")
        logger.info("="*80 + "\n")

        return results

    def generate_html_report(self, comparison_results: Dict, output_file: Path):
        """Generate comprehensive HTML report"""

        passes = sum(1 for r in comparison_results.values() if r['passes'])
        total = len(comparison_results)

        html = f"""
<!DOCTYPE html>
<html>
<head>
    <title>AQWA vs OrcaWave - Heading-by-Heading Comparison</title>
    <meta charset="utf-8">
    <style>
        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            max-width: 1800px;
            margin: 0 auto;
            padding: 20px;
            background-color: #f5f5f5;
        }}
        .header {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 30px;
        }}
        .header h1 {{
            margin: 0;
            font-size: 2.5em;
        }}
        .subtitle {{
            margin-top: 10px;
            opacity: 0.9;
        }}
        .summary {{
            background-color: rgba(255,255,255,0.2);
            padding: 15px;
            border-radius: 5px;
            margin-top: 15px;
            font-size: 1.2em;
        }}
        .section {{
            background: white;
            padding: 25px;
            margin-bottom: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        .section h2 {{
            color: #667eea;
            border-bottom: 2px solid #667eea;
            padding-bottom: 10px;
            margin-top: 0;
        }}
        table {{
            width: 100%;
            border-collapse: collapse;
            margin-top: 15px;
            font-size: 0.9em;
        }}
        th, td {{
            padding: 10px;
            text-align: left;
            border-bottom: 1px solid #e5e7eb;
        }}
        th {{
            background-color: #f3f4f6;
            font-weight: 600;
            color: #374151;
            position: sticky;
            top: 0;
        }}
        tr:hover {{
            background-color: #f9fafb;
        }}
        .pass {{
            color: #10b981;
            font-weight: bold;
        }}
        .fail {{
            color: #ef4444;
            font-weight: bold;
        }}
        .info {{
            background-color: #dbeafe;
            border-left: 4px solid #3b82f6;
            padding: 15px;
            margin-bottom: 20px;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>🌊 AQWA vs OrcaWave - Heading-by-Heading Comparison</h1>
        <div class="subtitle">
            Motion/Displacement RAOs - All DOFs Compared at Each Heading<br>
            Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
        </div>
        <div class="summary">
            <strong>Overall Result:</strong> {passes}/{total} DOFs pass 20% tolerance ({'✅ PASS' if passes == total else '⚠️ PARTIAL' if passes > 0 else '❌ FAIL'})<br>
            <strong>Method:</strong> Heading-by-heading comparison with frequency interpolation<br>
            <strong>RAO Type:</strong> Motion/Displacement RAOs (displacementRAOsRelativeTo)<br>
            <strong>Tolerance:</strong> 20% with 90% pass rate required
        </div>
    </div>

    <div class="section">
        <h2>📊 Overall Summary</h2>
        <table>
            <thead>
                <tr>
                    <th>DOF</th>
                    <th>Overall Pass Rate</th>
                    <th>Mean Error</th>
                    <th>Max Error</th>
                    <th>RMS Error</th>
                    <th>Status</th>
                </tr>
            </thead>
            <tbody>
"""

        for dof_name, result in comparison_results.items():
            status_class = 'pass' if result['passes'] else 'fail'

            html += f"""
                <tr>
                    <td><strong>{dof_name}</strong></td>
                    <td class="{status_class}">{result['overall_pass_rate']*100:.1f}%</td>
                    <td>{result['overall_mean_error']*100:.1f}%</td>
                    <td>{result['overall_max_error']*100:.1f}%</td>
                    <td>{result['overall_rms_error']*100:.1f}%</td>
                    <td class="{status_class}">{result['status']}</td>
                </tr>
"""

        html += """
            </tbody>
        </table>
    </div>
"""

        # Detailed results for each DOF
        for dof_name, result in comparison_results.items():
            html += f"""
    <div class="section">
        <h2>{dof_name}</h2>
        <div class="info">
            <strong>Overall Status:</strong> {result['status']}<br>
            <strong>Overall Pass Rate:</strong> {result['overall_pass_rate']*100:.1f}%
        </div>

        <table>
            <thead>
                <tr>
                    <th>OW Heading</th>
                    <th>AQWA Heading</th>
                    <th>AQWA Peak</th>
                    <th>AQWA Peak Period (s)</th>
                    <th>OW Peak</th>
                    <th>OW Peak Period (s)</th>
                    <th>Peak Error %</th>
                    <th>Pass Rate</th>
                    <th>Mean Error</th>
                </tr>
            </thead>
            <tbody>
"""

            for hc in result['heading_comparisons']:
                peak_class = 'pass' if hc['peak_rel_error'] <= self.tolerance else 'fail'
                pass_class = 'pass' if hc['pass_rate'] >= 0.90 else 'fail'

                html += f"""
                <tr>
                    <td>{hc['ow_heading']:.1f}°</td>
                    <td>{hc['aqwa_heading']:.1f}°</td>
                    <td>{hc['aqwa_peak']:.4f}</td>
                    <td>{hc['aqwa_peak_period']:.2f}</td>
                    <td>{hc['ow_peak']:.4f}</td>
                    <td>{hc['ow_peak_period']:.2f}</td>
                    <td class="{peak_class}">{hc['peak_rel_error']*100:.1f}%</td>
                    <td class="{pass_class}">{hc['pass_rate']*100:.1f}%</td>
                    <td>{hc['mean_error']*100:.1f}%</td>
                </tr>
"""

            html += """
            </tbody>
        </table>
    </div>
"""

        html += """
</body>
</html>
"""

        output_file.write_text(html, encoding='utf-8')
        logger.info(f"✓ HTML report generated: {output_file}")


def extract_orcawave_motion_raos(owr_file: Path) -> Dict:
    """Extract MOTION RAO data from OrcaWave .owr file"""

    logger.info(f"Loading OrcaWave results: {owr_file.name}")

    diffraction = OrcFxAPI.Diffraction()
    diffraction.LoadResults(str(owr_file.absolute()))

    frequencies = diffraction.frequencies
    periods = diffraction.periods
    headings = diffraction.headings

    # Get displacement RAOs (simple property)
    try:
        # Try simple displacementRAOs property first
        motion_raos = diffraction.displacementRAOs
        rao_type = "displacementRAOs"
        logger.info(f"✓ Using displacementRAOs property")
    except AttributeError:
        logger.error("displacementRAOs property not found!")
        logger.error("Available RAO attributes:")
        for attr in dir(diffraction):
            if 'rao' in attr.lower() or 'displacement' in attr.lower():
                logger.error(f"  - {attr}")
        raise

    logger.info(f"✓ Extracted OrcaWave data:")
    logger.info(f"  - RAO type: {rao_type}")
    logger.info(f"  - {len(frequencies)} frequencies")
    logger.info(f"  - {len(headings)} headings")
    logger.info(f"  - Motion RAOs shape: {motion_raos.shape}")

    # Validate data ranges
    for dof_idx, dof_name in enumerate(['SURGE', 'SWAY', 'HEAVE', 'ROLL', 'PITCH', 'YAW']):
        mag = np.abs(motion_raos[:, :, dof_idx])
        logger.info(f"  - {dof_name}: max = {mag.max():.4f}, min = {mag.min():.4f}")

    return {
        'frequencies': frequencies,
        'periods': periods,
        'headings': headings,
        'motion_raos': motion_raos,
        'rao_type': rao_type,
        'n_frequencies': len(frequencies),
        'n_headings': len(headings),
        'n_dofs': motion_raos.shape[2]
    }


def main():
    """Main execution"""

    benchmark_dir = Path(__file__).parent
    output_dir = benchmark_dir / "comparison_results"
    output_dir.mkdir(exist_ok=True)

    logger.info("="*80)
    logger.info("HEADING-BY-HEADING AQWA VS ORCAWAVE COMPARISON")
    logger.info("Using Motion/Displacement RAOs")
    logger.info("="*80)

    # Extract AQWA data
    logger.info("\n[1/3] Extracting AQWA data...")
    converter = AQWAConverter(
        analysis_folder=benchmark_dir,
        vessel_name="SHIP_RAOS"
    )
    aqwa_results = converter.convert_to_unified_schema(water_depth=30.0)

    logger.info(f"✓ AQWA data extracted")
    logger.info(f"  - {aqwa_results.raos.surge.frequencies.count} frequencies")
    logger.info(f"  - {aqwa_results.raos.surge.headings.count} headings")

    # Extract OrcaWave data
    logger.info("\n[2/3] Extracting OrcaWave motion RAO data...")
    # Use matched model results (updated mass properties to match AQWA)
    owr_file = benchmark_dir / "orcawave_001_ship_raos_rev2_matched.owr"

    if not owr_file.exists():
        logger.error(f"OrcaWave matched results not found: {owr_file}")
        logger.error("Run: python run_orcawave_matched_api.py")
        return 1

    orcawave_data = extract_orcawave_motion_raos(owr_file)

    # Run heading-by-heading comparison
    logger.info("\n[3/3] Running heading-by-heading comparison...")
    comparator = HeadingByHeadingComparator(
        aqwa_results=aqwa_results,
        orcawave_data=orcawave_data,
        tolerance=0.20  # 20%
    )

    comparison_results = comparator.compare_all_headings()

    # Generate report
    html_report = output_dir / f"final_comparison_{datetime.now().strftime('%Y%m%d_%H%M%S')}.html"
    comparator.generate_html_report(comparison_results, html_report)

    # Summary
    passes = sum(1 for r in comparison_results.values() if r['passes'])
    total = len(comparison_results)

    logger.info("\n" + "="*80)
    logger.info("✓ COMPARISON COMPLETE!")
    logger.info(f"HTML Report: {html_report}")
    logger.info(f"\nResults: {passes}/{total} DOFs pass 20% tolerance")
    logger.info(f"RAO Type Used: {orcawave_data['rao_type']}")
    logger.info("="*80)

    return 0


if __name__ == "__main__":
    sys.exit(main())
