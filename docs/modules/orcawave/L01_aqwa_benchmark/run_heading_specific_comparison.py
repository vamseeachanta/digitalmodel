#!/usr/bin/env python3
"""
ABOUTME: AQWA vs OrcaWave RAO Comparison - Heading-Specific Analysis
Compares each DOF at its physically relevant headings.
"""

import sys
from pathlib import Path
import numpy as np
from scipy.interpolate import RegularGridInterpolator
from datetime import datetime
from typing import Dict, List, Tuple
import json

# Add src and OrcFxAPI to path
repo_root = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(repo_root / "src"))

orcaflex_api_path = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python"
sys.path.insert(0, orcaflex_api_path)

from digitalmodel.diffraction.aqwa_lis_parser import AQWALISParser
from digitalmodel.diffraction.aqwa_converter import AQWAConverter
from digitalmodel.diffraction.output_schemas import DiffractionResults, DOF
from loguru import logger

try:
    import OrcFxAPI
    logger.info("✓ OrcFxAPI loaded successfully")
except ImportError as e:
    logger.error(f"Failed to import OrcFxAPI: {e}")
    sys.exit(1)


# Heading-specific comparison configuration
DOF_HEADING_CONFIG = {
    'SURGE': {
        'headings': [0.0, 180.0],  # Head and following seas
        'description': 'Longitudinal motion (head/following seas)',
        'tolerance': 0.20  # 20% tolerance
    },
    'SWAY': {
        'headings': [90.0, -90.0],  # Beam seas (port/starboard)
        'description': 'Lateral motion (beam seas)',
        'tolerance': 0.20
    },
    'HEAVE': {
        'headings': 'all',  # Omnidirectional
        'description': 'Vertical motion (all headings)',
        'tolerance': 0.20
    },
    'ROLL': {
        'headings': [90.0, -90.0],  # Beam seas
        'description': 'Rolling motion (beam seas)',
        'tolerance': 0.20
    },
    'PITCH': {
        'headings': [0.0, 180.0],  # Head and following seas
        'description': 'Pitching motion (head/following seas)',
        'tolerance': 0.20
    },
    'YAW': {
        'headings': [45.0, 135.0, -45.0, -135.0],  # Oblique headings
        'description': 'Yawing motion (oblique seas)',
        'tolerance': 0.20
    }
}


class HeadingSpecificComparator:
    """Compare AQWA and OrcaWave RAOs at heading-specific conditions"""

    def __init__(
        self,
        aqwa_results: DiffractionResults,
        orcawave_data: Dict
    ):
        self.aqwa = aqwa_results
        self.orcawave = orcawave_data

    def find_nearest_heading_index(self, target_heading: float, headings: np.ndarray) -> int:
        """Find index of nearest heading"""
        # Normalize to -180 to 180
        target = ((target_heading + 180) % 360) - 180
        headings_norm = ((headings + 180) % 360) - 180

        idx = np.argmin(np.abs(headings_norm - target))
        return idx

    def interpolate_at_heading(
        self,
        ow_freq: np.ndarray,
        ow_headings: np.ndarray,
        ow_rao_complex: np.ndarray,
        target_freq: np.ndarray,
        target_heading: float
    ) -> np.ndarray:
        """
        Interpolate OrcaWave RAO to AQWA frequencies at a specific heading

        Args:
            ow_freq: OrcaWave frequencies
            ow_headings: OrcaWave headings
            ow_rao_complex: OrcaWave complex RAO (n_headings, n_frequencies)
            target_freq: Target frequencies (AQWA)
            target_heading: Target heading to extract

        Returns:
            Interpolated complex RAO at target frequencies and heading
        """
        # Find nearest OrcaWave heading
        heading_idx = self.find_nearest_heading_index(target_heading, ow_headings)
        ow_heading_actual = ow_headings[heading_idx]

        # Extract RAO at this heading: (n_frequencies,)
        rao_at_heading = ow_rao_complex[heading_idx, :]

        # Interpolate frequency dimension
        real_interp = np.interp(target_freq, ow_freq, np.real(rao_at_heading))
        imag_interp = np.interp(target_freq, ow_freq, np.imag(rao_at_heading))

        result = real_interp + 1j * imag_interp

        logger.debug(f"  Interpolated at heading {target_heading}° (using OW heading {ow_heading_actual:.1f}°)")

        return result

    def compare_at_specific_headings(self) -> Dict:
        """
        Compare AQWA vs OrcaWave at heading-specific conditions

        Returns:
            Dictionary with comparison results for each DOF
        """
        logger.info("\n" + "="*80)
        logger.info("HEADING-SPECIFIC COMPARISON")
        logger.info("="*80)

        # AQWA grid
        aqwa_freq = self.aqwa.raos.surge.frequencies.values
        aqwa_headings = self.aqwa.raos.surge.headings.values

        # OrcaWave grid
        ow_freq = self.orcawave['frequencies']
        ow_headings = self.orcawave['headings']
        ow_raos = self.orcawave['load_raos']  # Shape: (n_headings, n_frequencies, 6)

        logger.info(f"\nAQWA: {len(aqwa_freq)} frequencies × {len(aqwa_headings)} headings")
        logger.info(f"OrcaWave: {len(ow_freq)} frequencies × {len(ow_headings)} headings")

        results = {}

        for dof in DOF:
            dof_name = dof.name
            config = DOF_HEADING_CONFIG[dof_name]

            logger.info(f"\n{'='*80}")
            logger.info(f"{dof_name}: {config['description']}")
            logger.info(f"{'='*80}")

            # Get AQWA and OrcaWave RAOs for this DOF
            aqwa_rao = self.aqwa.raos.get_component(dof)
            aqwa_mag = aqwa_rao.magnitude  # (n_freq, n_heading)

            dof_idx = list(DOF).index(dof)
            ow_rao_complex = ow_raos[:, :, dof_idx]  # (n_heading, n_freq)

            # Determine which headings to compare
            if config['headings'] == 'all':
                # Compare at all AQWA headings
                compare_headings = aqwa_headings
                logger.info(f"Comparing at ALL {len(aqwa_headings)} headings")
            else:
                # Compare at specific headings
                compare_headings = config['headings']
                logger.info(f"Comparing at specific headings: {compare_headings}")

            # Collect comparison data
            heading_comparisons = []
            all_errors = []

            for target_heading in compare_headings:
                # Find AQWA heading index
                aqwa_heading_idx = self.find_nearest_heading_index(target_heading, aqwa_headings)
                aqwa_heading_actual = aqwa_headings[aqwa_heading_idx]

                # Get AQWA RAO at this heading: (n_freq,)
                aqwa_rao_heading = aqwa_mag[:, aqwa_heading_idx]

                # Interpolate OrcaWave to AQWA frequencies at this heading
                ow_rao_interp = self.interpolate_at_heading(
                    ow_freq, ow_headings, ow_rao_complex,
                    aqwa_freq, target_heading
                )
                ow_mag_interp = np.abs(ow_rao_interp)

                # Calculate errors
                abs_errors = np.abs(ow_mag_interp - aqwa_rao_heading)
                rel_errors = abs_errors / np.maximum(aqwa_rao_heading, 1e-10)

                # Find peaks
                aqwa_peak = np.max(aqwa_rao_heading)
                ow_peak = np.max(ow_mag_interp)
                aqwa_peak_freq_idx = np.argmax(aqwa_rao_heading)
                ow_peak_freq_idx = np.argmax(ow_mag_interp)

                peak_diff = abs(ow_peak - aqwa_peak)
                peak_rel_error = peak_diff / aqwa_peak if aqwa_peak > 0 else 0

                # Statistics
                within_tolerance = rel_errors <= config['tolerance']
                pass_rate = np.sum(within_tolerance) / len(rel_errors)

                logger.info(f"\n  Heading {target_heading}° (AQWA: {aqwa_heading_actual:.1f}°):")
                logger.info(f"    AQWA peak: {aqwa_peak:.4f} at freq_idx={aqwa_peak_freq_idx}")
                logger.info(f"    OrcaWave peak: {ow_peak:.4f} at freq_idx={ow_peak_freq_idx}")
                logger.info(f"    Peak difference: {peak_rel_error*100:.1f}%")
                logger.info(f"    Pass rate ({config['tolerance']*100:.0f}% tol): {pass_rate*100:.1f}%")

                heading_comparisons.append({
                    'target_heading': target_heading,
                    'aqwa_heading': aqwa_heading_actual,
                    'aqwa_peak': aqwa_peak,
                    'ow_peak': ow_peak,
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
            overall_pass_rate = np.sum(all_errors <= config['tolerance']) / len(all_errors)

            passes = overall_pass_rate >= 0.90
            status = "✅ PASS" if passes else "❌ FAIL"

            logger.info(f"\n  Overall pass rate: {overall_pass_rate*100:.1f}%")
            logger.info(f"  Status: {status}")

            results[dof_name] = {
                'config': config,
                'heading_comparisons': heading_comparisons,
                'overall_mean_error': np.mean(all_errors),
                'overall_max_error': np.max(all_errors),
                'overall_rms_error': np.sqrt(np.mean(all_errors**2)),
                'overall_pass_rate': overall_pass_rate,
                'passes': passes,
                'status': status
            }

        logger.info("\n" + "="*80)
        logger.info("✓ Heading-specific comparison complete!")
        logger.info("="*80 + "\n")

        return results

    def generate_html_report(self, comparison_results: Dict, output_file: Path):
        """Generate HTML report with heading-specific analysis"""

        passes = sum(1 for r in comparison_results.values() if r['passes'])
        total = len(comparison_results)

        html = f"""
<!DOCTYPE html>
<html>
<head>
    <title>AQWA vs OrcaWave - Heading-Specific Comparison</title>
    <meta charset="utf-8">
    <style>
        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            max-width: 1600px;
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
        .section h3 {{
            color: #764ba2;
            margin-top: 20px;
        }}
        table {{
            width: 100%;
            border-collapse: collapse;
            margin-top: 15px;
        }}
        th, td {{
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #e5e7eb;
        }}
        th {{
            background-color: #f3f4f6;
            font-weight: 600;
            color: #374151;
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
            margin-top: 15px;
        }}
        .methodology {{
            background-color: #f0fdf4;
            border-left: 4px solid #10b981;
            padding: 15px;
            margin-bottom: 20px;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>🌊 AQWA vs OrcaWave - Heading-Specific RAO Comparison</h1>
        <div class="subtitle">
            Physics-Based Heading Selection - Each DOF Compared at Relevant Conditions<br>
            Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
        </div>
        <div class="summary">
            <strong>Overall Result:</strong> {passes}/{total} DOFs pass 20% tolerance ({'✅ PASS' if passes == total else '⚠️ PARTIAL' if passes > 0 else '❌ FAIL'})<br>
            <strong>Method:</strong> Heading-specific comparison based on physical response characteristics<br>
            <strong>Tolerance:</strong> 20% with 90% pass rate required
        </div>
    </div>

    <div class="section">
        <h2>📋 Methodology</h2>
        <div class="methodology">
            <strong>Heading-Specific Approach:</strong><br><br>
            Different DOFs respond to different wave directions. This comparison evaluates each DOF
            at its physically relevant headings:<br><br>
            <ul>
                <li><strong>HEAVE:</strong> All headings (omnidirectional vertical motion)</li>
                <li><strong>SURGE:</strong> 0° and 180° (longitudinal motion in head/following seas)</li>
                <li><strong>SWAY:</strong> 90° and -90° (lateral motion in beam seas)</li>
                <li><strong>ROLL:</strong> 90° and -90° (rolling in beam seas)</li>
                <li><strong>PITCH:</strong> 0° and 180° (pitching in head/following seas)</li>
                <li><strong>YAW:</strong> Oblique headings (45°, 135°, etc.)</li>
            </ul>
        </div>
    </div>

    <div class="section">
        <h2>📊 Overall Summary</h2>
        <table>
            <thead>
                <tr>
                    <th>DOF</th>
                    <th>Comparison Headings</th>
                    <th>Overall Pass Rate</th>
                    <th>Mean Error</th>
                    <th>Max Error</th>
                    <th>Status</th>
                </tr>
            </thead>
            <tbody>
"""

        for dof_name, result in comparison_results.items():
            config = result['config']
            status_class = 'pass' if result['passes'] else 'fail'

            if config['headings'] == 'all':
                headings_str = "All headings"
            else:
                headings_str = ", ".join(f"{h}°" for h in config['headings'])

            html += f"""
                <tr>
                    <td><strong>{dof_name}</strong></td>
                    <td>{headings_str}</td>
                    <td class="{status_class}">{result['overall_pass_rate']*100:.1f}%</td>
                    <td>{result['overall_mean_error']*100:.1f}%</td>
                    <td>{result['overall_max_error']*100:.1f}%</td>
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
            config = result['config']

            html += f"""
    <div class="section">
        <h2>{dof_name}</h2>
        <div class="info">
            <strong>Description:</strong> {config['description']}<br>
            <strong>Tolerance:</strong> {config['tolerance']*100:.0f}%<br>
            <strong>Overall Status:</strong> {result['status']}
        </div>

        <h3>Heading-by-Heading Results</h3>
        <table>
            <thead>
                <tr>
                    <th>Target Heading</th>
                    <th>AQWA Heading</th>
                    <th>AQWA Peak</th>
                    <th>OrcaWave Peak</th>
                    <th>Peak Error %</th>
                    <th>Pass Rate</th>
                    <th>Mean Error</th>
                </tr>
            </thead>
            <tbody>
"""

            for hc in result['heading_comparisons']:
                peak_class = 'pass' if hc['peak_rel_error'] <= config['tolerance'] else 'fail'
                pass_class = 'pass' if hc['pass_rate'] >= 0.90 else 'fail'

                html += f"""
                <tr>
                    <td>{hc['target_heading']}°</td>
                    <td>{hc['aqwa_heading']:.1f}°</td>
                    <td>{hc['aqwa_peak']:.4f}</td>
                    <td>{hc['ow_peak']:.4f}</td>
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


def extract_orcawave_raos(owr_file: Path) -> Dict:
    """Extract RAO data from OrcaWave .owr file"""

    logger.info(f"Loading OrcaWave results: {owr_file.name}")

    diffraction = OrcFxAPI.Diffraction()
    diffraction.LoadResults(str(owr_file.absolute()))

    frequencies = diffraction.frequencies
    periods = diffraction.periods
    headings = diffraction.headings
    load_raos = diffraction.loadRAOsHaskind

    logger.info(f"✓ Extracted OrcaWave data:")
    logger.info(f"  - {len(frequencies)} frequencies")
    logger.info(f"  - {len(headings)} headings")
    logger.info(f"  - Load RAOs shape: {load_raos.shape}")

    return {
        'frequencies': frequencies,
        'periods': periods,
        'headings': headings,
        'load_raos': load_raos,
        'n_frequencies': len(frequencies),
        'n_headings': len(headings),
        'n_dofs': load_raos.shape[2]
    }


def main():
    """Main execution"""

    benchmark_dir = Path(__file__).parent
    output_dir = benchmark_dir / "comparison_results"
    output_dir.mkdir(exist_ok=True)

    logger.info("="*80)
    logger.info("HEADING-SPECIFIC AQWA VS ORCAWAVE COMPARISON")
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
    logger.info("\n[2/3] Extracting OrcaWave data...")
    owr_file = benchmark_dir / "orcawave_001_ship_raos_rev2.owr"

    if not owr_file.exists():
        logger.error(f"OrcaWave results not found: {owr_file}")
        logger.error("Run OrcaWave GUI analysis first to generate .owr file")
        return 1

    orcawave_data = extract_orcawave_raos(owr_file)

    # Run heading-specific comparison
    logger.info("\n[3/3] Running heading-specific comparison...")
    comparator = HeadingSpecificComparator(
        aqwa_results=aqwa_results,
        orcawave_data=orcawave_data
    )

    comparison_results = comparator.compare_at_specific_headings()

    # Generate report
    html_report = output_dir / f"heading_specific_{datetime.now().strftime('%Y%m%d_%H%M%S')}.html"
    comparator.generate_html_report(comparison_results, html_report)

    # Summary
    passes = sum(1 for r in comparison_results.values() if r['passes'])
    total = len(comparison_results)

    logger.info("\n" + "="*80)
    logger.info("✓ COMPARISON COMPLETE!")
    logger.info(f"HTML Report: {html_report}")
    logger.info(f"\nResults: {passes}/{total} DOFs pass 20% tolerance at relevant headings")
    logger.info("="*80)

    return 0


if __name__ == "__main__":
    sys.exit(main())
