"""
Example Usage: Web Scraping for Vessel Data

This script demonstrates how to use the vessel scrapers to collect
up-to-date vessel data from various online sources.
"""

import sys
from pathlib import Path

# Add parent directory to path to import scrapers
sys.path.insert(0, str(Path(__file__).parent.parent))

from scrapers.fpso_scraper import FPSOScraper, scrape_fpso_database
from scrapers.drilling_rig_scraper import DrillingRigScraper, scrape_drilling_rigs
from scrapers.pipelay_scraper import PipelayVesselScraper
from validators.data_validator import DataValidator


def example_1_fpso_basic():
    """Example 1: Basic FPSO scraping with default settings."""
    print("\n" + "="*60)
    print("EXAMPLE 1: Basic FPSO Scraping")
    print("="*60)

    # Using convenience function
    df = scrape_fpso_database()

    if df is not None:
        print(f"\n✓ Successfully scraped {len(df)} FPSO records")
        print(f"  Columns: {list(df.columns)}")
        print(f"  Sample data:\n{df.head()}")
    else:
        print("\n✗ Failed to scrape FPSO data")


def example_2_fpso_custom_url():
    """Example 2: FPSO scraping from custom URL with validation."""
    print("\n" + "="*60)
    print("EXAMPLE 2: FPSO Scraping from Custom URL with Validation")
    print("="*60)

    # Custom URL (example - replace with actual data source)
    custom_url = "https://www.offshore-technology.com/data-insights/fpso-database/"

    # Create scraper with custom settings
    with FPSOScraper(
        source_url=custom_url,
        rate_limit=3.0,  # 3 seconds between requests (more polite)
        output_dir=Path("data/vessels/raw")
    ) as scraper:

        # Scrape data
        df = scraper.scrape()

        if df is not None:
            print(f"\n✓ Scraped {len(df)} records")

            # Validate scraped data
            validator = DataValidator()
            results = validator.validate_dataframe(
                df,
                required_fields=['vessel_name', 'year_built'],
                unique_field='vessel_name'
            )

            # Print validation report
            print("\n" + validator.generate_report(results))

            if results['valid']:
                print("\n✓ Data validation PASSED")
            else:
                print("\n⚠ Data validation FAILED - review issues above")


def example_3_drilling_rigs_all_types():
    """Example 3: Scraping all drilling rig types."""
    print("\n" + "="*60)
    print("EXAMPLE 3: Scraping All Drilling Rig Types")
    print("="*60)

    # Scrape all rig types (deepwater + jackup combined)
    df_all = scrape_drilling_rigs(rig_type='both')

    if df_all is not None:
        print(f"\n✓ Scraped {len(df_all)} total drilling rig records")

        # Check if we can distinguish rig types
        if 'rig_type' in df_all.columns or 'vessel_type' in df_all.columns:
            type_col = 'rig_type' if 'rig_type' in df_all.columns else 'vessel_type'
            print(f"\nBreakdown by type:")
            print(df_all[type_col].value_counts())


def example_4_drilling_rigs_separate():
    """Example 4: Scraping drilling rigs separately by type."""
    print("\n" + "="*60)
    print("EXAMPLE 4: Scraping Drilling Rigs Separately by Type")
    print("="*60)

    # Scrape deepwater rigs
    print("\nScraping deepwater rigs...")
    df_deepwater = scrape_drilling_rigs(rig_type='deepwater')

    if df_deepwater is not None:
        print(f"✓ Deepwater rigs: {len(df_deepwater)} records")

    # Scrape jackup rigs
    print("\nScraping jackup rigs...")
    df_jackup = scrape_drilling_rigs(rig_type='jackup')

    if df_jackup is not None:
        print(f"✓ Jackup rigs: {len(df_jackup)} records")


def example_5_pipelay_vessels():
    """Example 5: Scraping pipelay vessels from custom source."""
    print("\n" + "="*60)
    print("EXAMPLE 5: Scraping Pipelay Vessels")
    print("="*60)

    # Custom URL for pipelay vessels
    custom_url = "https://www.offshore-mag.com/pipelay-vessels"

    with PipelayVesselScraper(
        source_url=custom_url,
        output_dir=Path("data/vessels/raw")
    ) as scraper:

        df = scraper.scrape()

        if df is not None:
            print(f"\n✓ Scraped {len(df)} pipelay vessel records")
            print(f"  Columns: {list(df.columns)}")


def example_6_batch_scraping_with_validation():
    """Example 6: Batch scraping all vessel types with validation."""
    print("\n" + "="*60)
    print("EXAMPLE 6: Batch Scraping All Vessel Types")
    print("="*60)

    vessel_scrapers = [
        ("FPSOs", scrape_fpso_database, {}),
        ("Deepwater Rigs", scrape_drilling_rigs, {'rig_type': 'deepwater'}),
        ("Jackup Rigs", scrape_drilling_rigs, {'rig_type': 'jackup'}),
    ]

    validator = DataValidator()
    results_summary = []

    for vessel_type, scraper_func, kwargs in vessel_scrapers:
        print(f"\nScraping {vessel_type}...")

        df = scraper_func(**kwargs)

        if df is not None:
            # Validate
            validation = validator.validate_dataframe(df)

            results_summary.append({
                'type': vessel_type,
                'records': len(df),
                'quality_score': validation['quality_score'],
                'valid': validation['valid']
            })

            print(f"  ✓ {len(df)} records, Quality: {validation['quality_score']:.1f}/100")
        else:
            print(f"  ✗ Failed to scrape")

    # Print summary
    print("\n" + "="*60)
    print("SCRAPING SUMMARY")
    print("="*60)
    for result in results_summary:
        status = "✓" if result['valid'] else "⚠"
        print(f"{status} {result['type']}: {result['records']} records, "
              f"Quality: {result['quality_score']:.1f}/100")


def example_7_error_handling():
    """Example 7: Demonstrating error handling and retries."""
    print("\n" + "="*60)
    print("EXAMPLE 7: Error Handling and Retries")
    print("="*60)

    # Try scraping from an invalid URL to demonstrate error handling
    invalid_url = "https://example.com/nonexistent-page"

    print(f"\nAttempting to scrape from invalid URL: {invalid_url}")

    with FPSOScraper(source_url=invalid_url, max_retries=2) as scraper:
        df = scraper.scrape()

        if df is None:
            print("✓ Error handling worked correctly - returned None for invalid URL")
        else:
            print("⚠ Unexpected: got data from invalid URL")


if __name__ == "__main__":
    print("\n" + "="*60)
    print("VESSEL WEB SCRAPING EXAMPLES")
    print("="*60)
    print("\nThese examples demonstrate the vessel scraping framework.")
    print("Note: Some examples may fail if websites are unavailable or")
    print("have changed their structure. This is expected behavior.")

    # Run all examples
    examples = [
        example_1_fpso_basic,
        example_2_fpso_custom_url,
        example_3_drilling_rigs_all_types,
        example_4_drilling_rigs_separate,
        example_5_pipelay_vessels,
        example_6_batch_scraping_with_validation,
        example_7_error_handling,
    ]

    for i, example_func in enumerate(examples, 1):
        try:
            example_func()
        except Exception as e:
            print(f"\n⚠ Example {i} failed with error: {e}")
            print("This is normal if the website is unavailable or structure changed.")

    print("\n" + "="*60)
    print("EXAMPLES COMPLETE")
    print("="*60)
    print("\nNext steps:")
    print("1. Review the generated CSV files in data/vessels/raw/")
    print("2. Check metadata JSON files for source attribution")
    print("3. Validate data quality using DataValidator")
    print("4. Customize URLs for your specific data sources")
