#!/usr/bin/env python3
"""
AQWA vs OrcaWave Peak RAO Comparison

ABOUTME: Enhanced comparison focusing on peak/significant RAO values across all periods.

Engineering Validation Criteria:
- 5% tolerance applies to PEAK values (resonance regions)
- Low-amplitude responses (<10% of peak) are excluded from validation
- Comparison focuses on periods where RAO magnitudes are significant
- Statistics provided for both "significant values" and "all values"

Usage:
    python run_comparison_peaks.py
"""

import sys
from pathlib import Path
import numpy as np
import json
from datetime import datetime
from typing import Dict, List, Tuple

# Add src to path
repo_root = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(repo_root / "src"))

from digitalmodel.modules.diffraction.aqwa_lis_parser import AQWALISParser
from digitalmodel.modules.diffraction.aqwa_converter import AQWAConverter
from digitalmodel.modules.diffraction.output_schemas import DiffractionResults, DOF
from loguru import logger


class PeakRAOComparator:
    """Compare peak/significant RAO values between AQWA and OrcaWave"""

    def __init__(
        self,
        aqwa_results: DiffractionResults,
        orcawave_results: DiffractionResults = None,
        peak_threshold: float = 0.10,  # 10% of max value
        tolerance: float = 0.05  # 5% tolerance
    ):
        """
        Initialize peak comparator

        Args:
            aqwa_results: AQWA results
            orcawave_results: OrcaWave results (optional)
            peak_threshold: Minimum value as fraction of peak to be considered "significant"
            tolerance: Acceptable relative error (default: 5%)
        """
        self.aqwa = aqwa_results
        self.orcawave = orcawave_results
        self.peak_threshold = peak_threshold
        self.tolerance = tolerance

    def identify_peak_regions(self, rao_magnitudes: np.ndarray, dof_name: str) -> Dict:
        """
        Identify peak/significant RAO values

        Args:
            rao_magnitudes: RAO magnitude array [nfreq x nheading]
            dof_name: DOF name for logging

        Returns:
            Dictionary with peak analysis:
            - peak_value: Maximum RAO magnitude
            - peak_location: (freq_idx, heading_idx) of peak
            - significant_mask: Boolean mask for significant values
            - significant_count: Number of significant points
            - threshold_value: Minimum value considered significant
        """
        # Find global peak
        peak_value = np.max(rao_magnitudes)
        peak_idx = np.unravel_index(np.argmax(rao_magnitudes), rao_magnitudes.shape)

        # Threshold for "significant" values
        threshold = peak_value * self.peak_threshold

        # Mask for significant values
        significant_mask = rao_magnitudes >= threshold
        significant_count = np.sum(significant_mask)

        logger.info(f"{dof_name}: Peak = {peak_value:.4f} at index {peak_idx}")
        logger.info(f"{dof_name}: {significant_count} significant points (>{threshold:.4f})")

        return {
            'peak_value': peak_value,
            'peak_location': peak_idx,
            'significant_mask': significant_mask,
            'significant_count': significant_count,
            'threshold_value': threshold,
            'total_points': rao_magnitudes.size
        }

    def compare_peaks(self) -> Dict:
        """
        Compare peak RAO values between AQWA and OrcaWave

        Returns:
            Dictionary with comparison results for each DOF
        """
        if self.orcawave is None:
            # Only AQWA data - just analyze peaks
            return self._analyze_aqwa_peaks_only()

        results = {}

        for dof in DOF:
            dof_name = dof.name

            # Get RAO components
            aqwa_rao = self.aqwa.raos.get_component(dof)
            orcawave_rao = self.orcawave.raos.get_component(dof)

            # Identify peaks in both datasets
            aqwa_peaks = self.identify_peak_regions(aqwa_rao.magnitude, f"AQWA {dof_name}")
            orcawave_peaks = self.identify_peak_regions(orcawave_rao.magnitude, f"OrcaWave {dof_name}")

            # Compare peak values
            peak_diff = abs(orcawave_peaks['peak_value'] - aqwa_peaks['peak_value'])
            peak_rel_error = peak_diff / aqwa_peaks['peak_value'] if aqwa_peaks['peak_value'] > 0 else 0

            # Compare at significant points only
            significant_mask = aqwa_peaks['significant_mask'] & orcawave_peaks['significant_mask']

            if np.sum(significant_mask) > 0:
                aqwa_sig = aqwa_rao.magnitude[significant_mask]
                orcawave_sig = orcawave_rao.magnitude[significant_mask]

                errors = np.abs(orcawave_sig - aqwa_sig)
                rel_errors = errors / np.maximum(aqwa_sig, 1e-10)

                sig_stats = {
                    'mean_error': np.mean(rel_errors),
                    'max_error': np.max(rel_errors),
                    'rms_error': np.sqrt(np.mean(rel_errors**2)),
                    'points_within_5pct': np.sum(rel_errors <= 0.05),
                    'points_total': len(rel_errors),
                    'pass_rate': np.sum(rel_errors <= 0.05) / len(rel_errors)
                }
            else:
                sig_stats = None

            # Overall statistics (all points)
            errors_all = np.abs(orcawave_rao.magnitude - aqwa_rao.magnitude)
            rel_errors_all = errors_all / np.maximum(aqwa_rao.magnitude, 1e-10)

            all_stats = {
                'mean_error': np.mean(rel_errors_all),
                'max_error': np.max(rel_errors_all),
                'rms_error': np.sqrt(np.mean(rel_errors_all**2))
            }

            # Determine pass/fail
            if sig_stats:
                passes_5pct = sig_stats['pass_rate'] >= 0.90  # 90% of significant points within 5%
                status = "✅ PASS" if passes_5pct else "⚠️ FAIL"
            else:
                passes_5pct = False
                status = "⚠️ NO DATA"

            results[dof_name] = {
                'aqwa_peaks': aqwa_peaks,
                'orcawave_peaks': orcawave_peaks,
                'peak_comparison': {
                    'aqwa_peak': aqwa_peaks['peak_value'],
                    'orcawave_peak': orcawave_peaks['peak_value'],
                    'difference': peak_diff,
                    'relative_error_pct': peak_rel_error * 100,
                    'within_5pct': peak_rel_error <= 0.05
                },
                'significant_values_stats': sig_stats,
                'all_values_stats': all_stats,
                'passes_5pct_tolerance': passes_5pct,
                'status': status
            }

        return results

    def _analyze_aqwa_peaks_only(self) -> Dict:
        """Analyze AQWA peaks when OrcaWave data not available"""
        results = {}

        for dof in DOF:
            dof_name = dof.name
            aqwa_rao = self.aqwa.raos.get_component(dof)

            peaks = self.identify_peak_regions(aqwa_rao.magnitude, f"AQWA {dof_name}")

            # Find peak frequency and heading
            peak_idx = peaks['peak_location']
            frequencies = aqwa_rao.frequencies.values
            headings = aqwa_rao.headings.values

            peak_freq = frequencies[peak_idx[0]]
            peak_period = 2 * np.pi / peak_freq
            peak_heading = headings[peak_idx[1]]

            results[dof_name] = {
                'aqwa_peaks': peaks,
                'peak_details': {
                    'magnitude': peaks['peak_value'],
                    'frequency_rad_s': peak_freq,
                    'period_s': peak_period,
                    'heading_deg': peak_heading,
                    'threshold': peaks['threshold_value'],
                    'significant_count': peaks['significant_count'],
                    'significant_pct': 100 * peaks['significant_count'] / peaks['total_points']
                },
                'orcawave_data': 'Not available',
                'status': 'Awaiting OrcaWave comparison'
            }

        return results

    def generate_peak_report_html(self, comparison_results: Dict, output_file: Path):
        """Generate HTML report focused on peak comparisons"""

        has_orcawave = self.orcawave is not None

        html = f"""
<!DOCTYPE html>
<html>
<head>
    <title>AQWA vs OrcaWave Peak RAO Comparison</title>
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
        .criteria {{
            background-color: rgba(255,255,255,0.2);
            padding: 15px;
            border-radius: 5px;
            margin-top: 15px;
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
        .metric {{
            display: inline-block;
            margin-right: 30px;
            margin-bottom: 10px;
        }}
        .metric-label {{
            color: #6b7280;
            font-size: 0.9em;
        }}
        .metric-value {{
            font-size: 1.3em;
            font-weight: bold;
            color: #111827;
        }}
        .note {{
            background-color: #fef3c7;
            border-left: 4px solid #f59e0b;
            padding: 15px;
            margin-top: 15px;
        }}
        .info {{
            background-color: #dbeafe;
            border-left: 4px solid #3b82f6;
            padding: 15px;
            margin-top: 15px;
        }}
        .highlight {{
            background-color: #fef3c7;
            padding: 2px 6px;
            border-radius: 3px;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>🌊 AQWA vs OrcaWave Peak RAO Comparison</h1>
        <div class="subtitle">
            Hydrodynamic Validation - Focus on Peak/Significant Values<br>
            Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
        </div>
        <div class="criteria">
            <strong>Validation Criteria:</strong><br>
            ✓ 5% tolerance applies to peak and significant values<br>
            ✓ "Significant" = values ≥ 10% of peak magnitude<br>
            ✓ Pass requires 90% of significant points within 5% tolerance
        </div>
    </div>
"""

        if has_orcawave:
            # Full comparison with OrcaWave
            html += self._generate_comparison_tables(comparison_results)
        else:
            # AQWA only - peak analysis
            html += self._generate_aqwa_only_tables(comparison_results)

        html += """
</body>
</html>
"""

        output_file.write_text(html, encoding='utf-8')
        logger.info(f"✓ Peak comparison HTML report generated: {output_file}")

    def _generate_comparison_tables(self, results: Dict) -> str:
        """Generate comparison tables when both datasets available"""

        html = """
    <div class="section">
        <h2>📊 Peak Value Comparison</h2>
        <table>
            <thead>
                <tr>
                    <th>DOF</th>
                    <th>AQWA Peak</th>
                    <th>OrcaWave Peak</th>
                    <th>Difference</th>
                    <th>Error %</th>
                    <th>Status</th>
                </tr>
            </thead>
            <tbody>
"""

        for dof_name, result in results.items():
            peak_comp = result['peak_comparison']
            status_class = 'pass' if peak_comp['within_5pct'] else 'fail'
            status_icon = '✅' if peak_comp['within_5pct'] else '❌'

            html += f"""
                <tr>
                    <td><strong>{dof_name}</strong></td>
                    <td>{peak_comp['aqwa_peak']:.4f}</td>
                    <td>{peak_comp['orcawave_peak']:.4f}</td>
                    <td>{peak_comp['difference']:.4f}</td>
                    <td class="{status_class}">{peak_comp['relative_error_pct']:.2f}%</td>
                    <td>{status_icon} {result['status']}</td>
                </tr>
"""

        html += """
            </tbody>
        </table>
    </div>

    <div class="section">
        <h2>📈 Significant Values Statistics</h2>
        <div class="info">
            Only values ≥10% of peak magnitude are included in this analysis.
            This focuses validation on resonance regions and operationally significant responses.
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
                    <th>Overall Status</th>
                </tr>
            </thead>
            <tbody>
"""

        for dof_name, result in results.items():
            sig_stats = result['significant_values_stats']
            if sig_stats:
                pass_rate = sig_stats['pass_rate'] * 100
                status_class = 'pass' if result['passes_5pct_tolerance'] else 'fail'

                html += f"""
                <tr>
                    <td><strong>{dof_name}</strong></td>
                    <td>{sig_stats['points_total']}</td>
                    <td>{sig_stats['points_within_5pct']}</td>
                    <td class="{status_class}">{pass_rate:.1f}%</td>
                    <td>{sig_stats['mean_error']*100:.2f}%</td>
                    <td>{sig_stats['max_error']*100:.2f}%</td>
                    <td>{result['status']}</td>
                </tr>
"""
            else:
                html += f"""
                <tr>
                    <td><strong>{dof_name}</strong></td>
                    <td colspan="6" style="text-align: center; color: #6b7280;">No significant values found</td>
                </tr>
"""

        html += """
            </tbody>
        </table>
    </div>
"""

        return html

    def _generate_aqwa_only_tables(self, results: Dict) -> str:
        """Generate peak analysis tables for AQWA data only"""

        html = """
    <div class="section">
        <h2>📊 AQWA Peak RAO Analysis</h2>
        <div class="note">
            <strong>Note:</strong> OrcaWave data not available. Showing AQWA peak analysis only.
        </div>
        <table>
            <thead>
                <tr>
                    <th>DOF</th>
                    <th>Peak Magnitude</th>
                    <th>Peak Period (s)</th>
                    <th>Peak Heading (°)</th>
                    <th>Threshold</th>
                    <th>Significant Points</th>
                    <th>% Significant</th>
                </tr>
            </thead>
            <tbody>
"""

        for dof_name, result in results.items():
            peak_det = result['peak_details']

            html += f"""
                <tr>
                    <td><strong>{dof_name}</strong></td>
                    <td class="highlight">{peak_det['magnitude']:.4f}</td>
                    <td>{peak_det['period_s']:.2f}</td>
                    <td>{peak_det['heading_deg']:.1f}</td>
                    <td>{peak_det['threshold']:.4f}</td>
                    <td>{peak_det['significant_count']}</td>
                    <td>{peak_det['significant_pct']:.1f}%</td>
                </tr>
"""

        html += """
            </tbody>
        </table>
    </div>

    <div class="section">
        <h2>🔧 Next Steps</h2>
        <div class="note">
            <strong>To complete the 5% tolerance comparison:</strong>
            <ol>
                <li>Run OrcaWave analysis using: <code>orcawave_001_ship_raos_rev2.yml</code></li>
                <li>Ensure OrcaWave uses same mesh and analysis parameters as AQWA</li>
                <li>Save OrcaWave results to .sim file</li>
                <li>Re-run this script: <code>python run_comparison_peaks.py</code></li>
                <li>Review peak comparison to verify 5% tolerance on significant values</li>
            </ol>
        </div>
        <div class="info">
            <strong>Peak values identified above will be compared</strong> when OrcaWave data becomes available.
            The comparison will focus on periods and headings where RAO magnitudes are ≥10% of peak.
        </div>
    </div>
"""

        return html


def main():
    """Main execution"""

    benchmark_dir = Path(__file__).parent
    output_dir = benchmark_dir / "comparison_results"
    output_dir.mkdir(exist_ok=True)

    logger.info("="*80)
    logger.info("AQWA vs OrcaWave Peak RAO Comparison")
    logger.info("Focus: Significant values (≥10% of peak)")
    logger.info("Tolerance: 5% for peak and significant values")
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

    # Check for OrcaWave results
    logger.info("\n[2/3] Checking for OrcaWave data...")
    sim_files = list(benchmark_dir.glob("*.sim"))
    orcawave_results = None

    if sim_files:
        logger.info(f"Found OrcaWave .sim file: {sim_files[0].name}")
        # Would extract here if OrcFxAPI available
        logger.warning("OrcFxAPI extraction not implemented in this example")
    else:
        logger.warning("No OrcaWave .sim file found")

    # Run peak comparison
    logger.info("\n[3/3] Analyzing peaks and generating report...")
    comparator = PeakRAOComparator(
        aqwa_results=aqwa_results,
        orcawave_results=orcawave_results,
        peak_threshold=0.10,  # 10% of peak
        tolerance=0.05  # 5%
    )

    comparison_results = comparator.compare_peaks()

    # Generate report
    html_report = output_dir / f"peak_comparison_{datetime.now().strftime('%Y%m%d_%H%M%S')}.html"
    comparator.generate_peak_report_html(comparison_results, html_report)

    # Summary
    logger.info("\n" + "="*80)
    logger.info("✓ Peak comparison complete!")
    logger.info(f"Report: {html_report}")

    if orcawave_results is None:
        logger.warning("⚠️  OrcaWave data not available")
        logger.warning("⚠️  Run OrcaWave analysis to complete 5% tolerance validation")
    else:
        # Count passes
        passes = sum(1 for r in comparison_results.values() if r['passes_5pct_tolerance'])
        total = len(comparison_results)
        logger.info(f"Results: {passes}/{total} DOFs pass 5% tolerance on significant values")

    logger.info("="*80)

    return 0


if __name__ == "__main__":
    sys.exit(main())
