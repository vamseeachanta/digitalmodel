"""
Standalone OCIMF Interactive Report Generator.

Generates interactive HTML report with Plotly charts from OCIMF CSV data.
Run this script directly to generate the report.
"""

import sys
import os

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from digitalmodel.marine_analysis.reporting.ocimf_interactive_report import OCIMFInteractiveReport

def main():
    """Generate OCIMF report."""
    # Paths
    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    csv_file = os.path.join(base_dir, 'data', 'ocimf', 'ocimf_coefficients_production.csv')
    output_dir = os.path.join(base_dir, 'docs', 'reports', 'ocimf')
    log_file = os.path.join(output_dir, 'generation.log')

    # Ensure output directory exists
    os.makedirs(output_dir, exist_ok=True)

    # Generate report
    reporter = OCIMFInteractiveReport(csv_file, output_dir)

    try:
        output_file = reporter.generate_html_report()

        # Write log to file
        with open(log_file, 'w') as f:
            f.write("=" * 80 + "\n")
            f.write("OCIMF INTERACTIVE DATA ANALYSIS REPORT GENERATOR\n")
            f.write("=" * 80 + "\n")
            f.write(f"\nData source: {csv_file}\n")
            f.write(f"Output directory: {output_dir}\n")
            f.write("\n" + "=" * 80 + "\n")
            f.write("[SUCCESS] REPORT GENERATION COMPLETE\n")
            f.write("=" * 80 + "\n")
            f.write(f"\nGenerated report: {output_file}\n")
            f.write("\nOpen the HTML file in your browser to view the interactive report.\n")

        return 0
    except Exception as e:
        with open(log_file, 'w') as f:
            f.write(f"\n[ERROR] Report generation failed: {e}\n")
            import traceback
            traceback.print_exception(type(e), e, e.__traceback__, file=f)
        return 1

if __name__ == '__main__':
    sys.exit(main())
