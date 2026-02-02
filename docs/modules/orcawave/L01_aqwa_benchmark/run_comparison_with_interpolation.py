#!/usr/bin/env python3
"""
ABOUTME: AQWA vs OrcaWave RAO Comparison with Linear Interpolation
Handles configuration mismatch by interpolating to common grid.
"""

import sys
from pathlib import Path
import numpy as np
from scipy.interpolate import interp2d, RegularGridInterpolator
from datetime import datetime
from typing import Dict, Tuple
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


class InterpolatedRAOComparator:
    """Compare AQWA and OrcaWave RAOs using linear interpolation"""

    def __init__(
        self,
        aqwa_results: DiffractionResults,
        orcawave_data: Dict,
        tolerance: float = 0.05
    ):
        """
        Initialize comparator with interpolation support

        Args:
            aqwa_results: AQWA results (unified schema)
            orcawave_data: OrcaWave extracted data (from extract_orcawave_raos.py)
            tolerance: Acceptable relative error (default: 5%)
        """
        self.aqwa = aqwa_results
        self.orcawave = orcawave_data
        self.tolerance = tolerance

    def interpolate_to_common_grid(self) -> Tuple[np.ndarray, np.ndarray, Dict]:
        """
        Interpolate OrcaWave data to AQWA grid using 2D linear interpolation

        Returns:
            frequencies: Common frequency grid (AQWA)
            headings: Common heading grid (AQWA)
            interpolated_raos: Dictionary of interpolated OrcaWave RAOs by DOF
        """
        logger.info("\n" + "="*80)
        logger.info("INTERPOLATING ORCAWAVE TO AQWA GRID")
        logger.info("="*80)

        # AQWA grid (target)
        aqwa_freq = self.aqwa.raos.surge.frequencies.values  # rad/s
        aqwa_headings = self.aqwa.raos.surge.headings.values  # degrees

        # OrcaWave grid (source)
        ow_freq = self.orcawave['frequencies']  # rad/s
        ow_headings = self.orcawave['headings']  # degrees
        ow_raos = self.orcawave['load_raos']  # Shape: (n_headings, n_frequencies, 6)

        logger.info(f"\nSource (OrcaWave):")
        logger.info(f"  Frequencies: {len(ow_freq)} ({ow_freq.min():.4f} - {ow_freq.max():.4f} rad/s)")
        logger.info(f"  Headings: {len(ow_headings)} ({ow_headings.min():.1f}° - {ow_headings.max():.1f}°)")

        logger.info(f"\nTarget (AQWA):")
        logger.info(f"  Frequencies: {len(aqwa_freq)} ({aqwa_freq.min():.4f} - {aqwa_freq.max():.4f} rad/s)")
        logger.info(f"  Headings: {len(aqwa_headings)} ({aqwa_headings.min():.1f}° - {aqwa_headings.max():.1f}°)")

        # Interpolate each DOF
        interpolated_raos = {}
        dof_names = ['SURGE', 'SWAY', 'HEAVE', 'ROLL', 'PITCH', 'YAW']

        for i, dof_name in enumerate(dof_names):
            logger.info(f"\nInterpolating {dof_name}...")

            # Extract complex RAO for this DOF: (n_headings, n_frequencies)
            rao_complex = ow_raos[:, :, i]

            # Separate real and imaginary parts
            rao_real = np.real(rao_complex)
            rao_imag = np.imag(rao_complex)

            # Create 2D interpolators
            # Note: RegularGridInterpolator expects (freq, heading) ordering
            interp_real = RegularGridInterpolator(
                (ow_freq, ow_headings),
                rao_real.T,  # Transpose to match (freq, heading) ordering
                method='linear',
                bounds_error=False,
                fill_value=0.0
            )

            interp_imag = RegularGridInterpolator(
                (ow_freq, ow_headings),
                rao_imag.T,
                method='linear',
                bounds_error=False,
                fill_value=0.0
            )

            # Create meshgrid for AQWA grid
            freq_grid, heading_grid = np.meshgrid(aqwa_freq, aqwa_headings, indexing='ij')
            points = np.stack([freq_grid.ravel(), heading_grid.ravel()], axis=-1)

            # Interpolate
            real_interp = interp_real(points).reshape(len(aqwa_freq), len(aqwa_headings))
            imag_interp = interp_imag(points).reshape(len(aqwa_freq), len(aqwa_headings))

            # Combine back to complex
            rao_interp_complex = real_interp + 1j * imag_interp
            rao_interp_mag = np.abs(rao_interp_complex)

            interpolated_raos[dof_name] = {
                'magnitude': rao_interp_mag,
                'complex': rao_interp_complex,
                'real': real_interp,
                'imag': imag_interp
            }

            logger.info(f"  Original range: {np.abs(rao_complex).min():.4f} - {np.abs(rao_complex).max():.4f}")
            logger.info(f"  Interpolated range: {rao_interp_mag.min():.4f} - {rao_interp_mag.max():.4f}")

        logger.info("\n" + "="*80)
        logger.info("✓ Interpolation complete!")
        logger.info("="*80 + "\n")

        return aqwa_freq, aqwa_headings, interpolated_raos

    def compare_with_tolerance(self) -> Dict:
        """
        Compare AQWA vs interpolated OrcaWave with 5% tolerance

        Returns:
            Dictionary with comparison results for each DOF
        """
        logger.info("\n" + "="*80)
        logger.info("COMPARING AQWA VS ORCAWAVE (INTERPOLATED)")
        logger.info("="*80)

        # Interpolate OrcaWave to AQWA grid
        aqwa_freq, aqwa_headings, ow_interpolated = self.interpolate_to_common_grid()

        results = {}

        for dof in DOF:
            dof_name = dof.name

            logger.info(f"\n{dof_name}:")

            # Get AQWA RAO
            aqwa_rao = self.aqwa.raos.get_component(dof)
            aqwa_mag = aqwa_rao.magnitude  # Shape: (n_freq, n_heading)

            # Get interpolated OrcaWave RAO
            ow_mag = ow_interpolated[dof_name]['magnitude']

            # Calculate errors
            abs_errors = np.abs(ow_mag - aqwa_mag)
            rel_errors = abs_errors / np.maximum(aqwa_mag, 1e-10)

            # Statistics
            within_5pct = rel_errors <= self.tolerance
            pass_rate = np.sum(within_5pct) / rel_errors.size

            # Peak values
            aqwa_peak = np.max(aqwa_mag)
            ow_peak = np.max(ow_mag)
            peak_diff = abs(ow_peak - aqwa_peak)
            peak_rel_error = peak_diff / aqwa_peak if aqwa_peak > 0 else 0

            # Find peak locations
            aqwa_peak_idx = np.unravel_index(np.argmax(aqwa_mag), aqwa_mag.shape)
            ow_peak_idx = np.unravel_index(np.argmax(ow_mag), ow_mag.shape)

            # Significant values (≥10% of peak)
            threshold = aqwa_peak * 0.10
            significant_mask = aqwa_mag >= threshold

            if np.sum(significant_mask) > 0:
                sig_rel_errors = rel_errors[significant_mask]
                sig_pass_rate = np.sum(sig_rel_errors <= self.tolerance) / len(sig_rel_errors)

                sig_stats = {
                    'count': len(sig_rel_errors),
                    'mean_error': np.mean(sig_rel_errors),
                    'max_error': np.max(sig_rel_errors),
                    'rms_error': np.sqrt(np.mean(sig_rel_errors**2)),
                    'within_5pct': np.sum(sig_rel_errors <= self.tolerance),
                    'pass_rate': sig_pass_rate
                }
            else:
                sig_stats = None

            # Overall status
            passes = sig_stats['pass_rate'] >= 0.90 if sig_stats else False
            status = "✅ PASS" if passes else "❌ FAIL"

            logger.info(f"  AQWA peak: {aqwa_peak:.4f} at freq_idx={aqwa_peak_idx[0]}, heading_idx={aqwa_peak_idx[1]}")
            logger.info(f"  OrcaWave peak: {ow_peak:.4f} at freq_idx={ow_peak_idx[0]}, heading_idx={ow_peak_idx[1]}")
            logger.info(f"  Peak difference: {peak_rel_error*100:.2f}%")
            logger.info(f"  Overall pass rate: {pass_rate*100:.1f}%")
            if sig_stats:
                logger.info(f"  Significant values pass rate: {sig_stats['pass_rate']*100:.1f}% ({sig_stats['within_5pct']}/{sig_stats['count']})")
            logger.info(f"  Status: {status}")

            results[dof_name] = {
                'aqwa_peak': aqwa_peak,
                'orcawave_peak': ow_peak,
                'peak_difference': peak_diff,
                'peak_relative_error': peak_rel_error,
                'overall_pass_rate': pass_rate,
                'significant_stats': sig_stats,
                'passes': passes,
                'status': status,
                'mean_error': np.mean(rel_errors),
                'max_error': np.max(rel_errors),
                'rms_error': np.sqrt(np.mean(rel_errors**2))
            }

        logger.info("\n" + "="*80)
        logger.info("✓ Comparison complete!")
        logger.info("="*80 + "\n")

        return results

    def generate_html_report(self, comparison_results: Dict, output_file: Path):
        """Generate interactive HTML comparison report"""

        passes = sum(1 for r in comparison_results.values() if r['passes'])
        total = len(comparison_results)

        html = f"""
<!DOCTYPE html>
<html>
<head>
    <title>AQWA vs OrcaWave RAO Comparison (Interpolated)</title>
    <meta charset="utf-8">
    <style>
        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            max-width: 1400px;
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
        .highlight {{
            background-color: #fef3c7;
            padding: 2px 6px;
            border-radius: 3px;
        }}
        .info {{
            background-color: #dbeafe;
            border-left: 4px solid #3b82f6;
            padding: 15px;
            margin-top: 15px;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>🌊 AQWA vs OrcaWave RAO Comparison</h1>
        <div class="subtitle">
            Linear Interpolation Method - Mismatched Grids Aligned<br>
            Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
        </div>
        <div class="summary">
            <strong>Overall Result:</strong> {passes}/{total} DOFs pass 5% tolerance ({'✅ PASS' if passes == total else '⚠️ PARTIAL' if passes > 0 else '❌ FAIL'})<br>
            <strong>Method:</strong> OrcaWave data interpolated to AQWA frequency/heading grid<br>
            <strong>Tolerance:</strong> 5% for 90% of significant values (≥10% of peak)
        </div>
    </div>

    <div class="section">
        <h2>📊 Peak Value Comparison</h2>
        <div class="info">
            Peak values compared after interpolating OrcaWave to AQWA grid.
        </div>
        <table>
            <thead>
                <tr>
                    <th>DOF</th>
                    <th>AQWA Peak</th>
                    <th>OrcaWave Peak</th>
                    <th>Difference</th>
                    <th>Error %</th>
                    <th>Overall Pass Rate</th>
                    <th>Status</th>
                </tr>
            </thead>
            <tbody>
"""

        for dof_name, result in comparison_results.items():
            status_class = 'pass' if result['passes'] else 'fail'
            peak_class = 'pass' if result['peak_relative_error'] <= 0.05 else 'fail'

            html += f"""
                <tr>
                    <td><strong>{dof_name}</strong></td>
                    <td>{result['aqwa_peak']:.4f}</td>
                    <td>{result['orcawave_peak']:.4f}</td>
                    <td>{result['peak_difference']:.4f}</td>
                    <td class="{peak_class}">{result['peak_relative_error']*100:.2f}%</td>
                    <td class="{status_class}">{result['overall_pass_rate']*100:.1f}%</td>
                    <td class="{status_class}">{result['status']}</td>
                </tr>
"""

        html += """
            </tbody>
        </table>
    </div>

    <div class="section">
        <h2>📈 Significant Values Statistics</h2>
        <div class="info">
            Only values ≥10% of peak magnitude included. Pass requires 90% within 5% tolerance.
        </div>
        <table>
            <thead>
                <tr>
                    <th>DOF</th>
                    <th>Significant Points</th>
                    <th>Within 5%</th>
                    <th>Pass Rate</th>
                    <th>Mean Error</th>
                    <th>Max Error</th>
                    <th>RMS Error</th>
                </tr>
            </thead>
            <tbody>
"""

        for dof_name, result in comparison_results.items():
            sig_stats = result['significant_stats']
            if sig_stats:
                status_class = 'pass' if result['passes'] else 'fail'

                html += f"""
                <tr>
                    <td><strong>{dof_name}</strong></td>
                    <td>{sig_stats['count']}</td>
                    <td>{sig_stats['within_5pct']}</td>
                    <td class="{status_class}">{sig_stats['pass_rate']*100:.1f}%</td>
                    <td>{sig_stats['mean_error']*100:.2f}%</td>
                    <td>{sig_stats['max_error']*100:.2f}%</td>
                    <td>{sig_stats['rms_error']*100:.2f}%</td>
                </tr>
"""
            else:
                html += f"""
                <tr>
                    <td><strong>{dof_name}</strong></td>
                    <td colspan="6" style="text-align: center; color: #6b7280;">No significant values</td>
                </tr>
"""

        html += f"""
            </tbody>
        </table>
    </div>

    <div class="section">
        <h2>🔧 Interpolation Details</h2>
        <div class="info">
            <strong>Method:</strong> 2D Linear Interpolation (scipy.interpolate.RegularGridInterpolator)<br>
            <strong>Source Grid (OrcaWave):</strong> {self.orcawave['n_frequencies']} frequencies × {self.orcawave['n_headings']} headings = {self.orcawave['n_frequencies'] * self.orcawave['n_headings']} points<br>
            <strong>Target Grid (AQWA):</strong> {len(self.aqwa.raos.surge.frequencies.values)} frequencies × {len(self.aqwa.raos.surge.headings.values)} headings = {len(self.aqwa.raos.surge.frequencies.values) * len(self.aqwa.raos.surge.headings.values)} points<br>
            <strong>Interpolation:</strong> Complex RAOs interpolated separately (real + imaginary)<br>
            <strong>Bounds Handling:</strong> Zero-fill for out-of-bounds values
        </div>
    </div>

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
    added_mass = diffraction.addedMass
    damping = diffraction.damping

    logger.info(f"✓ Extracted OrcaWave data:")
    logger.info(f"  - {len(frequencies)} frequencies")
    logger.info(f"  - {len(headings)} headings")
    logger.info(f"  - Load RAOs shape: {load_raos.shape}")

    return {
        'frequencies': frequencies,
        'periods': periods,
        'headings': headings,
        'load_raos': load_raos,
        'added_mass': added_mass,
        'damping': damping,
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
    logger.info("AQWA vs ORCAWAVE COMPARISON WITH INTERPOLATION")
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

    # Run comparison with interpolation
    logger.info("\n[3/3] Comparing with interpolation...")
    comparator = InterpolatedRAOComparator(
        aqwa_results=aqwa_results,
        orcawave_data=orcawave_data,
        tolerance=0.05
    )

    comparison_results = comparator.compare_with_tolerance()

    # Generate report
    html_report = output_dir / f"interpolated_comparison_{datetime.now().strftime('%Y%m%d_%H%M%S')}.html"
    comparator.generate_html_report(comparison_results, html_report)

    # Save results as JSON
    json_report = output_dir / f"interpolated_comparison_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    json_results = {
        dof: {k: float(v) if isinstance(v, (np.floating, float)) else v
              for k, v in result.items() if k != 'significant_stats'}
        for dof, result in comparison_results.items()
    }
    json_report.write_text(json.dumps(json_results, indent=2))

    # Summary
    passes = sum(1 for r in comparison_results.values() if r['passes'])
    total = len(comparison_results)

    logger.info("\n" + "="*80)
    logger.info("✓ COMPARISON COMPLETE!")
    logger.info(f"HTML Report: {html_report}")
    logger.info(f"JSON Results: {json_report}")
    logger.info(f"\nResults: {passes}/{total} DOFs pass 5% tolerance on significant values")
    logger.info("="*80)

    return 0


if __name__ == "__main__":
    sys.exit(main())
