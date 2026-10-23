"""Render the synthetic patch-test house report pack into a supplied directory.

Usage: PYTHONPATH=src python examples/workflows/survey-qa-report-pack/build_report.py
       --output-dir <output-directory>
The directory receives the md/html report, two CSVs and pack manifests; no PDF.
"""

import argparse
from pathlib import Path

from digitalmodel.subsea.survey_qa.report import write_example_report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    write_example_report(args.output_dir)


if __name__ == "__main__":
    main()
