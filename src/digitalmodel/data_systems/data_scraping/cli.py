#!/usr/bin/env python3
"""
Command Line Interface for Data Procurement

Provides CLI commands for running scrapers and validating data.
"""

import argparse
import sys
from pathlib import Path
from typing import Optional

from scrapers.fpso_scraper import scrape_fpso_database
from scrapers.drilling_rig_scraper import scrape_drilling_rigs
from scrapers.pipelay_scraper import PipelayVesselScraper
from validators.data_validator import DataValidator


def scrape_fpso_cmd(args):
    """Scrape FPSO database."""
    print(f"Scraping FPSO database...")
    if args.url:
        print(f"  Source: {args.url}")

    df = scrape_fpso_database(url=args.url, output_dir=args.output)

    if df is not None:
        print(f"✓ Successfully scraped {len(df)} FPSO records")

        if args.validate:
            validator = DataValidator()
            results = validator.validate_dataframe(df, unique_field='vessel_name')
            print(validator.generate_report(results))
    else:
        print("✗ Failed to scrape FPSO data")
        sys.exit(1)


def scrape_rigs_cmd(args):
    """Scrape drilling rigs database."""
    print(f"Scraping {args.type} drilling rigs...")
    if args.url:
        print(f"  Source: {args.url}")

    df = scrape_drilling_rigs(
        rig_type=args.type,
        url=args.url,
        output_dir=args.output
    )

    if df is not None:
        print(f"✓ Successfully scraped {len(df)} drilling rig records")

        if args.validate:
            validator = DataValidator()
            results = validator.validate_dataframe(df, unique_field='vessel_name')
            print(validator.generate_report(results))
    else:
        print("✗ Failed to scrape drilling rig data")
        sys.exit(1)


def scrape_pipelay_cmd(args):
    """Scrape pipelay vessels."""
    if not args.url:
        print("✗ Error: --url is required for pipelay vessels")
        sys.exit(1)

    print(f"Scraping pipelay vessels...")
    print(f"  Source: {args.url}")

    with PipelayVesselScraper(
        source_url=args.url,
        output_dir=Path(args.output)
    ) as scraper:
        df = scraper.scrape()

    if df is not None:
        print(f"✓ Successfully scraped {len(df)} pipelay vessel records")

        if args.validate:
            validator = DataValidator()
            results = validator.validate_dataframe(df, unique_field='vessel_name')
            print(validator.generate_report(results))
    else:
        print("✗ Failed to scrape pipelay vessel data")
        sys.exit(1)


def scrape_all_cmd(args):
    """Scrape all vessel types."""
    print("Scraping all vessel types...")
    print("="*60)

    success_count = 0
    total_records = 0

    # FPSO
    print("\n1. Scraping FPSOs...")
    df_fpso = scrape_fpso_database(output_dir=args.output)
    if df_fpso is not None:
        print(f"   ✓ {len(df_fpso)} FPSO records")
        success_count += 1
        total_records += len(df_fpso)
    else:
        print("   ✗ Failed")

    # Deepwater Rigs
    print("\n2. Scraping deepwater rigs...")
    df_deepwater = scrape_drilling_rigs(rig_type='deepwater', output_dir=args.output)
    if df_deepwater is not None:
        print(f"   ✓ {len(df_deepwater)} deepwater rig records")
        success_count += 1
        total_records += len(df_deepwater)
    else:
        print("   ✗ Failed")

    # Jackup Rigs
    print("\n3. Scraping jackup rigs...")
    df_jackup = scrape_drilling_rigs(rig_type='jackup', output_dir=args.output)
    if df_jackup is not None:
        print(f"   ✓ {len(df_jackup)} jackup rig records")
        success_count += 1
        total_records += len(df_jackup)
    else:
        print("   ✗ Failed")

    # Summary
    print("\n" + "="*60)
    print(f"SUMMARY: {success_count}/3 scrapers successful")
    print(f"Total records: {total_records}")
    print("="*60)


def validate_cmd(args):
    """Validate a CSV file."""
    import pandas as pd

    filepath = Path(args.file)
    if not filepath.exists():
        print(f"✗ Error: File not found: {filepath}")
        sys.exit(1)

    print(f"Validating: {filepath}")

    # Load CSV
    df = pd.read_csv(filepath)
    print(f"  Loaded {len(df)} rows, {len(df.columns)} columns")

    # Validate
    validator = DataValidator()

    required_fields = args.required.split(',') if args.required else None

    results = validator.validate_dataframe(
        df,
        required_fields=required_fields,
        unique_field=args.unique
    )

    # Print report
    print(validator.generate_report(results))

    # Exit with error code if validation failed
    if not results['valid']:
        sys.exit(1)


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description='Data Procurement CLI - Web scraping for marine engineering data',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Scrape FPSO database
  %(prog)s scrape-fpso

  # Scrape from custom URL
  %(prog)s scrape-fpso --url https://example.com/fpso-data

  # Scrape drilling rigs (deepwater only)
  %(prog)s scrape-rigs --type deepwater

  # Scrape all vessel types
  %(prog)s scrape-all

  # Validate a CSV file
  %(prog)s validate data/vessels/raw/fpso_database_2025.csv

  # Validate with required fields
  %(prog)s validate data.csv --required vessel_name,year_built --unique vessel_name
        """
    )

    subparsers = parser.add_subparsers(dest='command', help='Available commands')

    # Common arguments
    common_parser = argparse.ArgumentParser(add_help=False)
    common_parser.add_argument(
        '--output', '-o',
        default='data',
        help='Output directory (default: data)'
    )
    common_parser.add_argument(
        '--validate',
        action='store_true',
        help='Validate scraped data'
    )

    # scrape-fpso command
    fpso_parser = subparsers.add_parser(
        'scrape-fpso',
        parents=[common_parser],
        help='Scrape FPSO database'
    )
    fpso_parser.add_argument('--url', help='Custom URL to scrape')
    fpso_parser.set_defaults(func=scrape_fpso_cmd)

    # scrape-rigs command
    rigs_parser = subparsers.add_parser(
        'scrape-rigs',
        parents=[common_parser],
        help='Scrape drilling rigs database'
    )
    rigs_parser.add_argument(
        '--type',
        choices=['deepwater', 'jackup', 'both'],
        default='both',
        help='Rig type to scrape (default: both)'
    )
    rigs_parser.add_argument('--url', help='Custom URL to scrape')
    rigs_parser.set_defaults(func=scrape_rigs_cmd)

    # scrape-pipelay command
    pipelay_parser = subparsers.add_parser(
        'scrape-pipelay',
        parents=[common_parser],
        help='Scrape pipelay vessels'
    )
    pipelay_parser.add_argument(
        '--url',
        required=True,
        help='URL to scrape (required)'
    )
    pipelay_parser.set_defaults(func=scrape_pipelay_cmd)

    # scrape-all command
    all_parser = subparsers.add_parser(
        'scrape-all',
        parents=[common_parser],
        help='Scrape all vessel types'
    )
    all_parser.set_defaults(func=scrape_all_cmd)

    # validate command
    validate_parser = subparsers.add_parser(
        'validate',
        help='Validate a CSV file'
    )
    validate_parser.add_argument(
        'file',
        help='Path to CSV file to validate'
    )
    validate_parser.add_argument(
        '--required',
        help='Comma-separated list of required fields'
    )
    validate_parser.add_argument(
        '--unique',
        help='Field that should be unique'
    )
    validate_parser.set_defaults(func=validate_cmd)

    # Parse and execute
    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        sys.exit(1)

    args.func(args)


if __name__ == '__main__':
    main()
