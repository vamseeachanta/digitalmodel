#!/usr/bin/env python3
"""
Test Fender Scraper

Quick test to verify fender scraping from manufacturer websites.
"""

import sys
from pathlib import Path

# Add parent to path
sys.path.insert(0, str(Path(__file__).parent))

from scrapers.fender_scraper import FenderScraper
from validators.data_validator import DataValidator


def test_nauticexpo_fenders():
    """Test fender scraping from NauticExpo."""
    print("="*60)
    print("TESTING FENDER SCRAPING - NAUTICEXPO")
    print("="*60)

    url = "https://www.nauticexpo.com/boat-manufacturer/fender-996.html"
    print(f"\nURL: {url}")
    print("Method: Static scraping (with Selenium fallback)")

    # Create output directory
    output_dir = Path("data/equipment/raw/fenders")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Use FenderScraper
    scraper = FenderScraper(
        base_url=url,
        output_dir=output_dir
    )

    print("\nScraping NauticExpo fender catalog...")
    df = scraper.scrape_nauticexpo(url)

    if df is not None and not df.empty:
        print(f"\n✓ Successfully scraped {len(df)} rows")
        print(f"  Columns ({len(df.columns)}): {list(df.columns)[:10]}")

        # Show first few rows
        print(f"\nFirst 3 rows:")
        print(df.head(3))

        # Validate
        validator = DataValidator()
        results = validator.validate_dataframe(df)

        print(f"\n--- Quality Report ---")
        print(f"Quality Score: {results['quality_score']:.1f}/100")
        print(f"Total Rows: {results['total_rows']}")
        print(f"Total Columns: {results['total_columns']}")

        if results['issues']:
            print(f"\nIssues:")
            for issue in results['issues']:
                print(f"  - {issue}")

        # Equipment-specific validation
        equipment_validation = scraper.validate_equipment_specs(df)
        print(f"\n--- Equipment Validation ---")
        print(f"Valid: {equipment_validation['valid']}")
        print(f"Rows: {equipment_validation['rows']}")
        print(f"Completeness: {equipment_validation.get('overall_completeness', 'N/A')}")

        if equipment_validation.get('completeness'):
            print("\nField Completeness:")
            for field, completeness in equipment_validation['completeness'].items():
                print(f"  {field}: {completeness}")

        # Save
        output_path = output_dir / "nauticexpo_fenders_test.csv"
        df.to_csv(output_path, index=False)
        print(f"\n✓ Saved to {output_path}")

    else:
        print("\n✗ Failed to scrape fender data")

    # Close scraper
    scraper.close()


def test_marine_fenders_intl():
    """Test fender scraping from Marine Fenders International."""
    print("\n" + "="*60)
    print("TESTING FENDER SCRAPING - MARINE FENDERS INTERNATIONAL")
    print("="*60)

    url = "https://www.marinefendersintl.com/"
    print(f"\nURL: {url}")

    output_dir = Path("data/equipment/raw/fenders")

    scraper = FenderScraper(
        base_url=url,
        output_dir=output_dir
    )

    print("\nScraping Marine Fenders International...")
    df = scraper.scrape_marine_fenders_intl(url)

    if df is not None and not df.empty:
        print(f"\n✓ Successfully scraped {len(df)} rows")
        print(f"  Columns ({len(df.columns)}): {list(df.columns)[:10]}")

        # Validate
        validator = DataValidator()
        results = validator.validate_dataframe(df)

        print(f"\n--- Quality Report ---")
        print(f"Quality Score: {results['quality_score']:.1f}/100")

        if results['issues']:
            print(f"\nIssues:")
            for issue in results['issues']:
                print(f"  - {issue}")

    else:
        print("\n✗ No data found or failed to scrape")

    scraper.close()


def test_combined_scraping():
    """Test scraping from multiple sources combined."""
    print("\n" + "="*60)
    print("TESTING COMBINED FENDER SCRAPING - MULTIPLE SOURCES")
    print("="*60)

    output_dir = Path("data/equipment/raw/fenders")

    scraper = FenderScraper(
        base_url="https://www.nauticexpo.com",
        output_dir=output_dir
    )

    print("\nScraping from multiple sources...")
    print("Sources: NauticExpo")

    # Scrape from default sources
    df = scraper.scrape(sources=['nauticexpo'])

    if df is not None and not df.empty:
        print(f"\n✓ Combined scraping successful: {len(df)} rows")
        print(f"  Columns: {list(df.columns)}")

        # Show fender type distribution
        if 'fender_type' in df.columns:
            print("\nFender Type Distribution:")
            type_counts = df['fender_type'].value_counts()
            for fender_type, count in type_counts.items():
                print(f"  {fender_type}: {count}")

        # Validate
        validator = DataValidator()
        results = validator.validate_dataframe(df)

        print(f"\n--- Final Quality Report ---")
        print(f"Quality Score: {results['quality_score']:.1f}/100")
        print(f"Total Rows: {results['total_rows']}")
        print(f"Completeness: {(1 - results['missing_percentage']/100)*100:.1f}%")

        # Check for output file
        output_files = list(output_dir.glob("fenders_*.csv"))
        if output_files:
            latest_file = max(output_files, key=lambda p: p.stat().st_mtime)
            print(f"\n✓ Data saved to: {latest_file}")

    else:
        print("\n✗ Combined scraping failed")

    scraper.close()


if __name__ == "__main__":
    try:
        # Test individual sources
        test_nauticexpo_fenders()

        # Uncomment to test additional sources
        # test_marine_fenders_intl()

        # Test combined scraping
        print("\n")
        test_combined_scraping()

        print("\n" + "="*60)
        print("FENDER SCRAPER TESTS COMPLETE")
        print("="*60)

    except KeyboardInterrupt:
        print("\n\nTest interrupted by user")
    except Exception as e:
        print(f"\n✗ Test failed with error: {e}")
        import traceback
        traceback.print_exc()
