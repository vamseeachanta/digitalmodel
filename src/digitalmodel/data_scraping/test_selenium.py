#!/usr/bin/env python3
"""
Test Selenium Dynamic Scraping

Quick test to verify Selenium can extract tables from offshore-fleet.com
"""

import sys
from pathlib import Path

# Add parent to path
sys.path.insert(0, str(Path(__file__).parent))

from scrapers.dynamic_scraper import DynamicScraper
from validators.data_validator import DataValidator


def test_selenium_fpso():
    """Test Selenium scraping of FPSO data."""
    print("="*60)
    print("TESTING SELENIUM DYNAMIC SCRAPING - FPSO")
    print("="*60)

    url = "http://offshore-fleet.com/data/fpso.htm"
    print(f"\nURL: {url}")
    print("Method: Selenium WebDriver (headless Chrome)")

    # Use DynamicScraper
    with DynamicScraper(
        base_url=url,
        headless=True,
        wait_for_tables=15  # Wait longer for tables
    ) as scraper:
        print("\nScraping with Selenium...")
        df = scraper.scrape_dynamic(url, min_rows=10)

        if df is not None:
            print(f"\n✓ Successfully scraped {len(df)} rows")
            print(f"  Columns ({len(df.columns)}): {list(df.columns)[:10]}")
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

            # Save
            output_path = Path("data/vessels/raw/fpso_selenium_test.csv")
            df.to_csv(output_path, index=False)
            print(f"\n✓ Saved to {output_path}")

        else:
            print("\n✗ Failed to scrape data")


def test_selenium_pipelay():
    """Test Selenium scraping of pipelay vessels."""
    print("\n" + "="*60)
    print("TESTING SELENIUM DYNAMIC SCRAPING - PIPELAY")
    print("="*60)

    url = "http://offshore-fleet.com/data/pipelay-vessel.htm"
    print(f"\nURL: {url}")

    with DynamicScraper(
        base_url=url,
        headless=True,
        wait_for_tables=15
    ) as scraper:
        print("\nScraping with Selenium...")
        df = scraper.scrape_dynamic(url, min_rows=10)

        if df is not None:
            print(f"\n✓ Successfully scraped {len(df)} rows")
            print(f"  Columns ({len(df.columns)}): {list(df.columns)[:10]}")
            print(f"\nFirst 3 rows:")
            print(df.head(3))

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
            print("\n✗ Failed to scrape data")


if __name__ == "__main__":
    try:
        test_selenium_fpso()
        print("\n")
        test_selenium_pipelay()

        print("\n" + "="*60)
        print("SELENIUM TESTS COMPLETE")
        print("="*60)

    except KeyboardInterrupt:
        print("\n\nTest interrupted by user")
    except Exception as e:
        print(f"\n✗ Test failed with error: {e}")
        import traceback
        traceback.print_exc()
