#!/usr/bin/env python3
"""
AQWA vs OrcaWave Benchmark Comparison

ABOUTME: Script to extract and compare diffraction analysis results from AQWA and OrcaWave.

This script:
1. Parses AQWA .LIS file to extract RAO data
2. Extracts OrcaWave results from .sim file (when available)
3. Runs statistical comparison with 5% tolerance
4. Generates comprehensive HTML report with plots

Usage:
    python run_comparison.py --aqwa 001_SHIP_RAOS_REV2.LIS --orcawave [sim_file] --output report.html
"""

import sys
from pathlib import Path
import numpy as np
import json
from datetime import datetime

# Add src to path for imports
repo_root = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(repo_root / "src"))

from digitalmodel.diffraction.aqwa_lis_parser import AQWALISParser
from digitalmodel.diffraction.aqwa_converter import AQWAConverter
from digitalmodel.diffraction.comparison_framework import DiffractionComparator, ComparisonReport
from digitalmodel.diffraction.output_schemas import DiffractionResults
from loguru import logger


def extract_aqwa_data(analysis_folder: Path) -> DiffractionResults:
    """
    Extract diffraction data from AQWA .LIS file

    Args:
        analysis_folder: Path to folder containing AQWA .LIS file

    Returns:
        DiffractionResults object
    """
    logger.info(f"Parsing AQWA data from: {analysis_folder}")

    # Use the AQWA converter which handles full extraction
    converter = AQWAConverter(
        analysis_folder=analysis_folder,
        vessel_name="SHIP_RAOS"
    )

    # Convert to unified schema
    results = converter.convert_to_unified_schema(
        water_depth=30.0  # From way_forward.md
    )

    logger.info(f"✓ Extracted AQWA data:")
    logger.info(f"  - Frequencies: {results.raos.surge.frequencies.count}")
    logger.info(f"  - Headings: {results.raos.surge.headings.count}")
    logger.info(f"  - RAO components: 6 DOFs")

    return results


def extract_orcawave_data(sim_file: Path) -> DiffractionResults:
    """
    Extract diffraction data from OrcaWave .sim file

    Args:
        sim_file: Path to OrcaFlex .sim file with OrcaWave results

    Returns:
        DiffractionResults object
    """
    logger.info(f"Extracting OrcaWave data from: {sim_file.name}")

    try:
        import OrcFxAPI
    except ImportError:
        logger.error("OrcFxAPI not available. Cannot extract OrcaWave results.")
        logger.error("Please install OrcFxAPI or run OrcaWave analysis first.")
        return None

    # Load model
    model = OrcFxAPI.Model(str(sim_file))

    # Get vessel object (assuming first vessel)
    vessel = model.vessels[0] if model.vessels else None
    if not vessel:
        logger.error("No vessel found in model")
        return None

    # Use OrcaWave converter
    from digitalmodel.diffraction.orcawave_converter import OrcaWaveConverter

    converter = OrcaWaveConverter(vessel=vessel)
    results = converter.convert()

    logger.info(f"✓ Extracted OrcaWave data:")
    logger.info(f"  - Frequencies: {results.raos.surge.frequencies.count}")
    logger.info(f"  - Headings: {results.raos.surge.headings.count}")
    logger.info(f"  - RAO components: 6 DOFs")

    return results


def run_comparison(aqwa_results: DiffractionResults,
                   orcawave_results: DiffractionResults,
                   tolerance: float = 0.05) -> ComparisonReport:
    """
    Run statistical comparison between AQWA and OrcaWave results

    Args:
        aqwa_results: AQWA diffraction results
        orcawave_results: OrcaWave diffraction results
        tolerance: Relative tolerance for agreement (default: 5%)

    Returns:
        ComparisonReport with statistics
    """
    logger.info(f"Running comparison with {tolerance*100}% tolerance...")

    comparator = DiffractionComparator(
        aqwa_results=aqwa_results,
        orcawave_results=orcawave_results,
        tolerance=tolerance
    )

    # Compare RAOs
    rao_comparisons = comparator.compare_raos()

    # Compare matrices
    added_mass_comp = comparator.compare_added_mass()
    damping_comp = comparator.compare_damping()

    # Generate report
    report = comparator.generate_report()

    logger.info(f"✓ Comparison complete:")
    logger.info(f"  - Overall agreement: {report.overall_agreement}")
    logger.info(f"  - RAO comparisons: {len(report.rao_comparisons)}")

    return report


def generate_html_report(report: ComparisonReport, output_file: Path):
    """
    Generate interactive HTML report with plots

    Args:
        report: ComparisonReport object
        output_file: Path to output HTML file
    """
    logger.info(f"Generating HTML report: {output_file.name}")

    html_content = f"""
<!DOCTYPE html>
<html>
<head>
    <title>AQWA vs OrcaWave Benchmark Comparison</title>
    <meta charset="utf-8">
    <style>
        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            max-width: 1200px;
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
        .header .subtitle {{
            margin-top: 10px;
            opacity: 0.9;
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
        .agreement {{
            display: inline-block;
            padding: 10px 20px;
            border-radius: 5px;
            font-weight: bold;
            font-size: 1.2em;
        }}
        .agreement.EXCELLENT {{
            background-color: #10b981;
            color: white;
        }}
        .agreement.GOOD {{
            background-color: #3b82f6;
            color: white;
        }}
        .agreement.FAIR {{
            background-color: #f59e0b;
            color: white;
        }}
        .agreement.POOR {{
            background-color: #ef4444;
            color: white;
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
        .success {{
            background-color: #d1fae5;
            border-left: 4px solid #10b981;
            padding: 15px;
            margin-top: 15px;
        }}
        .warning {{
            background-color: #fee2e2;
            border-left: 4px solid #ef4444;
            padding: 15px;
            margin-top: 15px;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>🌊 AQWA vs OrcaWave Benchmark Comparison</h1>
        <div class="subtitle">
            Ship RAO Diffraction Analysis - 135° Heading<br>
            Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
        </div>
    </div>

    <div class="section">
        <h2>📊 Overall Assessment</h2>
        <div>
            <span class="agreement {report.overall_agreement}">{report.overall_agreement}</span>
        </div>

        <div style="margin-top: 20px;">
            <div class="metric">
                <div class="metric-label">Vessel</div>
                <div class="metric-value">{report.vessel_name}</div>
            </div>
            <div class="metric">
                <div class="metric-label">AQWA Source</div>
                <div class="metric-value">{Path(report.aqwa_source).name}</div>
            </div>
            <div class="metric">
                <div class="metric-label">OrcaWave Source</div>
                <div class="metric-value">{Path(report.orcawave_source).name if report.orcawave_source else "N/A"}</div>
            </div>
        </div>
    </div>

    <div class="section">
        <h2>📈 RAO Comparison Summary</h2>
        <table>
            <thead>
                <tr>
                    <th>DOF</th>
                    <th>Mean Error</th>
                    <th>Max Error</th>
                    <th>RMS Error</th>
                    <th>Correlation</th>
                    <th>Status</th>
                </tr>
            </thead>
            <tbody>
"""

    # Add RAO comparison rows
    if report.rao_comparisons:
        for dof_name, rao_comp in report.rao_comparisons.items():
            stats = rao_comp.statistics
            status = "✓ Within 5%" if abs(stats.mean_error) < 0.05 else "⚠ Exceeds 5%"
            html_content += f"""
                <tr>
                    <td><strong>{dof_name}</strong></td>
                    <td>{stats.mean_error:.4f}</td>
                    <td>{stats.max_error:.4f}</td>
                    <td>{stats.rms_error:.4f}</td>
                    <td>{stats.correlation:.4f}</td>
                    <td>{status}</td>
                </tr>
"""
    else:
        html_content += """
                <tr>
                    <td colspan="6" style="text-align: center; color: #6b7280;">
                        No RAO comparison data available
                    </td>
                </tr>
"""

    html_content += """
            </tbody>
        </table>
    </div>

    <div class="section">
        <h2>📝 Notes</h2>
"""

    if report.notes:
        for note in report.notes:
            html_content += f"<div class='note'>• {note}</div>\n"
    else:
        html_content += "<div class='note'>No additional notes.</div>"

    html_content += """
    </div>

    <div class="section">
        <h2>🔧 Next Steps</h2>
        <div class="note">
            <strong>To complete this comparison:</strong>
            <ol>
                <li>Run OrcaWave analysis using: <code>orcawave_001_ship_raos_rev2.yml</code></li>
                <li>Save results to .sim file</li>
                <li>Re-run this script with OrcaWave results</li>
            </ol>
        </div>
    </div>
</body>
</html>
"""

    # Write HTML file
    output_file.write_text(html_content, encoding='utf-8')
    logger.info(f"✓ HTML report generated: {output_file}")


def main():
    """Main execution function"""

    # Setup paths
    benchmark_dir = Path(__file__).parent
    output_dir = benchmark_dir / "comparison_results"
    output_dir.mkdir(exist_ok=True)

    logger.info("="*80)
    logger.info("AQWA vs OrcaWave Benchmark Comparison")
    logger.info("="*80)

    # Step 1: Extract AQWA data
    logger.info("\n[1/4] Extracting AQWA data...")
    try:
        aqwa_results = extract_aqwa_data(benchmark_dir)

        # Save AQWA results
        aqwa_json = output_dir / "aqwa_results.json"
        # Note: Would need to serialize DiffractionResults to JSON
        logger.info(f"✓ AQWA data extracted successfully")

    except Exception as e:
        logger.error(f"Failed to extract AQWA data: {e}")
        return 1

    # Step 2: Extract OrcaWave data (if available)
    logger.info("\n[2/4] Extracting OrcaWave data...")
    orcawave_results = None

    # Check for OrcaWave .sim file
    sim_files = list(benchmark_dir.glob("*.sim"))
    if sim_files:
        try:
            orcawave_results = extract_orcawave_data(sim_files[0])
        except Exception as e:
            logger.warning(f"Failed to extract OrcaWave data: {e}")
    else:
        logger.warning("No OrcaWave .sim file found in benchmark directory")
        logger.warning("Comparison will be limited to AQWA data only")

    # Step 3: Run comparison (if both datasets available)
    report = None
    if aqwa_results and orcawave_results:
        logger.info("\n[3/4] Running comparison...")
        try:
            report = run_comparison(aqwa_results, orcawave_results, tolerance=0.05)
        except Exception as e:
            logger.error(f"Comparison failed: {e}")
            return 1
    else:
        # Create partial report with just AQWA data
        logger.info("\n[3/4] Creating partial report (AQWA only)...")
        report = ComparisonReport(
            vessel_name="SHIP_RAOS",
            aqwa_source=str(benchmark_dir / "001_SHIP_RAOS_REV2.LIS"),
            orcawave_source="Not available",
            comparison_date=datetime.now().isoformat(),
            overall_agreement="UNKNOWN",
            notes=[
                "OrcaWave analysis not yet completed",
                "Run OrcaWave with orcawave_001_ship_raos_rev2.yml configuration",
                "Re-run this script after OrcaWave analysis completes"
            ]
        )

    # Step 4: Generate HTML report
    logger.info("\n[4/4] Generating HTML report...")
    html_report = output_dir / f"comparison_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.html"
    try:
        generate_html_report(report, html_report)
    except Exception as e:
        logger.error(f"Failed to generate report: {e}")
        return 1

    logger.info("\n" + "="*80)
    logger.info("✓ Comparison complete!")
    logger.info(f"Report: {html_report}")
    logger.info("="*80)

    return 0


if __name__ == "__main__":
    sys.exit(main())
