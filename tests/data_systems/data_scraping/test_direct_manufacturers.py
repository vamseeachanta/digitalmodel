#!/usr/bin/env python3
"""Test direct manufacturer website scraping."""

import sys
from pathlib import Path

# Add parent to path
sys.path.insert(0, str(Path(__file__).parent))

from scrapers.fender_scraper import FenderScraper
from validators.data_validator import DataValidator


def test_pacific_marine():
    """Test scraping Pacific Marine & Industrial website."""
    print("="*60)
    print("TESTING DIRECT MANUFACTURER - PACIFIC MARINE")
    print("="*60)

    url = "https://www.pacificmarine.net/marine-deck/marine-fenders.htm"
    print(f"\nURL: {url}")
    print("Expected: Simpler HTML structure than NauticExpo")

    output_dir = Path("data/equipment/raw/fenders")

    scraper = FenderScraper(
        base_url=url,
        output_dir=output_dir
    )

    print("\nScraping Pacific Marine...")
    df = scraper.scrape_pacific_marine(url)

    if df is not None and not df.empty:
        print(f"\n✓ Successfully scraped {len(df)} rows")
        print(f"  Columns ({len(df.columns)}): {list(df.columns)[:10]}")

        # Show sample
        print(f"\nFirst 3 rows:")
        print(df.head(3))

        # Validate
        validator = DataValidator()
        results = validator.validate_dataframe(df)

        print(f"\n--- Quality Report ---")
        print(f"Quality Score: {results.get('quality_score', 0):.1f}/100")
        print(f"Total Rows: {results.get('total_rows', 0)}")
        if 'missing_percentage' in results:
            print(f"Completeness: {(1 - results['missing_percentage']/100)*100:.1f}%")

        if results.get('issues'):
            print(f"\nIssues:")
            for issue in results['issues'][:5]:
                print(f"  - {issue}")

        # Save
        output_file = output_dir / "pacific_marine_test.csv"
        df.to_csv(output_file, index=False)
        print(f"\n✓ Saved to {output_file}")

    else:
        print("\n✗ No data scraped from Pacific Marine")

    scraper.close()


def test_marine_fenders_intl():
    """Test scraping Marine Fenders International website."""
    print("\n" + "="*60)
    print("TESTING DIRECT MANUFACTURER - MARINE FENDERS INTL")
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
        print(f"  Columns: {list(df.columns)[:10]}")

        # Validate
        validator = DataValidator()
        results = validator.validate_dataframe(df)

        print(f"\n--- Quality Report ---")
        print(f"Quality Score: {results.get('quality_score', 0):.1f}/100")

    else:
        print("\n✗ No data scraped")

    scraper.close()


def test_trelleborg():
    """Test scraping Trelleborg website."""
    print("\n" + "="*60)
    print("TESTING DIRECT MANUFACTURER - TRELLEBORG")
    print("="*60)

    # Try Trelleborg marine fenders page
    url = "https://www.trelleborg.com/en/marine-and-infrastructure/marine-and-infrastructure/products-and-solutions/marine-fenders"

    print(f"\nURL: {url}")

    output_dir = Path("data/equipment/raw/fenders")

    scraper = FenderScraper(
        base_url=url,
        output_dir=output_dir
    )

    print("\nAttempting to scrape Trelleborg...")

    # Try custom URL scraping
    df = scraper.scrape(custom_url=url, use_selenium=False)

    if df is not None and not df.empty:
        print(f"\n✓ Successfully scraped {len(df)} rows")
        print(f"  Columns: {list(df.columns)[:10]}")

        # Save
        output_file = output_dir / "trelleborg_test.csv"
        df.to_csv(output_file, index=False)
        print(f"\n✓ Saved to {output_file}")

    else:
        print("\n✗ No data scraped - may need Selenium or different URL")

    scraper.close()


if __name__ == "__main__":
    try:
        # Test Pacific Marine (most likely to work)
        test_pacific_marine()

        # Test Marine Fenders International
        test_marine_fenders_intl()

        # Test Trelleborg
        test_trelleborg()

        print("\n" + "="*60)
        print("DIRECT MANUFACTURER TESTS COMPLETE")
        print("="*60)

    except KeyboardInterrupt:
        print("\n\nTest interrupted by user")
    except Exception as e:
        print(f"\n✗ Test failed with error: {e}")
        import traceback
        traceback.print_exc()
