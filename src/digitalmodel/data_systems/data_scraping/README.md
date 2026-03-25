# Data Procurement Framework

Automated web scraping framework for collecting marine engineering data from online sources.

## Overview

This framework provides a systematic approach to collecting, validating, and organizing marine vessel and equipment data from various online sources. It implements polite web scraping practices with rate limiting, retry logic, and automatic data validation.

## Features

- **Base Scraper Framework**: Reusable HTTP client with retry logic and rate limiting
- **Vessel-Specific Scrapers**: Specialized scrapers for FPSOs, drilling rigs, and pipelay vessels
- **Data Validation**: Automatic quality scoring and validation
- **Metadata Generation**: Source attribution and provenance tracking
- **CSV Export**: Direct export to organized data directory structure

## Installation

### Requirements

```bash
pip install -r requirements-scraping.txt
```

Required packages:
- `requests` - HTTP client
- `pandas` - Data manipulation
- `beautifulsoup4` - HTML parsing
- `lxml` - XML/HTML parser (faster than html.parser)

### Project Structure

```
src/data_procurement/
├── scrapers/
│   ├── base_scraper.py          # Base HTTP scraper
│   ├── vessel_scraper.py        # Vessel-specific base class
│   ├── fpso_scraper.py          # FPSO scraper
│   ├── drilling_rig_scraper.py  # Drilling rig scraper
│   └── pipelay_scraper.py       # Pipelay vessel scraper
├── validators/
│   └── data_validator.py        # Data quality validation
├── examples/
│   └── scrape_vessels_example.py # Usage examples
└── README.md                     # This file
```

## Quick Start

### Example 1: Scrape FPSO Database

```python
from scrapers.fpso_scraper import scrape_fpso_database

# Scrape FPSO data (uses default source)
df = scrape_fpso_database()

if df is not None:
    print(f"Scraped {len(df)} FPSO records")
    print(df.head())
```

### Example 2: Scrape Drilling Rigs by Type

```python
from scrapers.drilling_rig_scraper import scrape_drilling_rigs

# Scrape deepwater rigs only
df_deepwater = scrape_drilling_rigs(rig_type='deepwater')

# Scrape jackup rigs only
df_jackup = scrape_drilling_rigs(rig_type='jackup')

# Scrape all types combined
df_all = scrape_drilling_rigs(rig_type='both')
```

### Example 3: Custom URL with Validation

```python
from pathlib import Path
from scrapers.fpso_scraper import FPSOScraper
from validators.data_validator import DataValidator

# Custom URL
url = "https://www.offshore-technology.com/fpso-database/"

# Scrape with custom settings
with FPSOScraper(
    source_url=url,
    rate_limit=3.0,  # 3 seconds between requests
    output_dir=Path("data/vessels/raw")
) as scraper:

    df = scraper.scrape()

    if df is not None:
        # Validate data quality
        validator = DataValidator()
        results = validator.validate_dataframe(
            df,
            required_fields=['vessel_name', 'year_built'],
            unique_field='vessel_name'
        )

        # Print validation report
        print(validator.generate_report(results))
```

## API Reference

### BaseScraper

Base class for all scrapers with HTTP handling and file management.

**Constructor:**
```python
BaseScraper(
    base_url: str,           # Base URL for scraping
    rate_limit: float = 2.0, # Seconds between requests (default: 2.0)
    timeout: int = 30,       # Request timeout (default: 30)
    max_retries: int = 3,    # Max retry attempts (default: 3)
    output_dir: Optional[Path] = None  # Output directory
)
```

**Methods:**
- `fetch_page(url, method='GET', params=None)` - Fetch a web page with retry logic
- `extract_tables(html)` - Extract all tables from HTML as DataFrames
- `save_csv(df, filename, domain='vessels', subdomain='raw')` - Save DataFrame to CSV
- `save_metadata(filepath, source_url, description)` - Generate metadata JSON

**Context Manager:**
```python
with BaseScraper(base_url="https://example.com") as scraper:
    response = scraper.fetch_page("/data")
```

### VesselScraper

Base class for vessel-specific scrapers with standardization methods.

**Methods:**
- `clean_vessel_name(name)` - Clean and standardize vessel names
- `parse_year(year_str)` - Extract 4-digit year from various formats
- `parse_dimension(dim_str)` - Parse dimensions with unit conversion (m/ft)
- `standardize_columns(df)` - Map common column name variations
- `validate_vessel_data(df)` - Clean and validate vessel data
- `save_vessel_data(df, filename, source_url, description)` - Save with metadata

### FPSOScraper

Scraper for FPSO vessel databases.

**Methods:**
- `scrape_offshore_magazine()` - Scrape from Offshore Magazine
- `scrape_custom_url(url)` - Scrape from any URL
- `scrape(custom_url=None)` - Main scraping method

**Convenience Function:**
```python
scrape_fpso_database(url=None, output_dir="data")
```

### DrillingRigScraper

Scraper for drilling rig databases (deepwater and jackup).

**Constructor:**
```python
DrillingRigScraper(
    rig_type: Literal['deepwater', 'jackup', 'both'] = 'both',
    source_url: Optional[str] = None,
    **kwargs
)
```

**Methods:**
- `scrape_rigzone()` - Scrape from Rigzone
- `scrape_custom_url(url)` - Scrape from any URL
- `scrape(custom_url=None)` - Main scraping method

**Convenience Function:**
```python
scrape_drilling_rigs(
    rig_type: Literal['deepwater', 'jackup', 'both'] = 'both',
    url: Optional[str] = None,
    output_dir: str = "data"
)
```

### PipelayVesselScraper

Scraper for pipelay vessel databases.

**Methods:**
- `scrape_custom_url(url)` - Scrape from any URL
- `scrape(custom_url=None)` - Main scraping method

### DataValidator

Validates scraped data and provides quality scoring.

**Methods:**
- `validate_dataframe(df, required_fields=None, unique_field=None)` - Validate DataFrame
  - Returns: `{'valid': bool, 'total_rows': int, 'quality_score': float, 'issues': list}`
- `generate_report(validation_results)` - Generate human-readable report

**Quality Scoring:**
- 100: Perfect data (no issues)
- 60-99: Acceptable (minor issues)
- 0-59: Failed (major issues)

**Validation Checks:**
- Empty DataFrame
- Missing required fields
- Missing data percentage
- Duplicate values
- Data type issues (numeric fields)

## Data Organization

Scraped data is automatically organized following the domain-driven structure:

```
data/
├── vessels/
│   └── raw/
│       ├── fpso_database_2025.csv
│       ├── fpso_database_2025.metadata.json
│       ├── drilling_rigs_deepwater_2025.csv
│       ├── drilling_rigs_jackup_2025.csv
│       └── pipelay_vessels_2025.csv
└── equipment/
    └── raw/
        └── ...
```

Each CSV file has an accompanying `.metadata.json` file with:
- Source URL
- Scrape timestamp
- Description
- Row/column counts

## Best Practices

### 1. Polite Scraping

**Always respect website terms of service and robots.txt:**

```python
# Use appropriate rate limiting (2-3 seconds minimum)
scraper = FPSOScraper(rate_limit=3.0)

# Identify your scraper with a custom user agent
scraper = FPSOScraper()
scraper.session.headers.update({
    'User-Agent': 'YourCompanyBot/1.0 (contact@example.com)'
})
```

### 2. Error Handling

**Handle failures gracefully:**

```python
df = scrape_fpso_database()

if df is None:
    print("Scraping failed - check logs")
    # Implement fallback or notification
else:
    print(f"Successfully scraped {len(df)} records")
```

### 3. Data Validation

**Always validate scraped data:**

```python
validator = DataValidator()
results = validator.validate_dataframe(
    df,
    required_fields=['vessel_name', 'year_built'],
    unique_field='vessel_name'
)

if results['quality_score'] < 60:
    print("Data quality too low - manual review needed")
    print(validator.generate_report(results))
```

### 4. Incremental Updates

**Scrape incrementally to avoid overwhelming servers:**

```python
# Scrape one vessel type at a time
df_fpso = scrape_fpso_database()
time.sleep(60)  # Wait 1 minute between different scrapers

df_rigs = scrape_drilling_rigs(rig_type='deepwater')
time.sleep(60)
```

## Workflow

### Complete Data Procurement Workflow

```python
from scrapers.fpso_scraper import scrape_fpso_database
from scrapers.drilling_rig_scraper import scrape_drilling_rigs
from validators.data_validator import DataValidator
import time

# Initialize validator
validator = DataValidator()

# 1. Scrape FPSOs
print("Scraping FPSOs...")
df_fpso = scrape_fpso_database()
if df_fpso is not None:
    results = validator.validate_dataframe(df_fpso)
    print(f"FPSOs: {len(df_fpso)} records, Quality: {results['quality_score']:.1f}/100")

time.sleep(60)  # Polite delay

# 2. Scrape Deepwater Rigs
print("Scraping deepwater rigs...")
df_deepwater = scrape_drilling_rigs(rig_type='deepwater')
if df_deepwater is not None:
    results = validator.validate_dataframe(df_deepwater)
    print(f"Deepwater: {len(df_deepwater)} records, Quality: {results['quality_score']:.1f}/100")

time.sleep(60)  # Polite delay

# 3. Scrape Jackup Rigs
print("Scraping jackup rigs...")
df_jackup = scrape_drilling_rigs(rig_type='jackup')
if df_jackup is not None:
    results = validator.validate_dataframe(df_jackup)
    print(f"Jackup: {len(df_jackup)} records, Quality: {results['quality_score']:.1f}/100")

print("\nData procurement complete! Check data/vessels/raw/ for results.")
```

## Troubleshooting

### Common Issues

**1. Import Errors**
```python
# If you get import errors, add project root to path
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent))
```

**2. No Tables Found**
- Website structure may have changed
- Use BeautifulSoup fallback in scraper classes
- Inspect HTML manually to identify data patterns

**3. Rate Limit Errors (HTTP 429)**
- Increase `rate_limit` parameter (e.g., 5.0 seconds)
- Add delays between scraper runs
- Check if website requires registration

**4. Validation Failures**
- Review validation report for specific issues
- Adjust `required_fields` if data structure differs
- Manual cleaning may be needed for some sources

## Legal and Ethical Considerations

1. **Respect robots.txt**: Check website's robots.txt file
2. **Terms of Service**: Review and comply with website ToS
3. **Rate Limiting**: Use appropriate delays (2-3s minimum)
4. **Attribution**: Metadata includes source URLs for proper attribution
5. **Fair Use**: Only scrape publicly available data
6. **Contact**: If scraping frequently, contact website owners

## Future Enhancements

- [ ] Add scrapers for equipment catalogs
- [ ] Implement OCIMF coefficient scrapers
- [ ] Add support for dynamic content (Selenium/Playwright)
- [ ] Create scheduling system for periodic updates
- [ ] Add data deduplication across scraping runs
- [ ] Implement change detection (delta updates)
- [ ] Add support for API-based data sources
- [ ] Create web dashboard for monitoring scraping jobs

## Support

For issues or questions:
1. Check `examples/scrape_vessels_example.py` for usage patterns
2. Review validation reports for data quality issues
3. Consult `docs/DATA_PROCUREMENT_STRATEGY.md` for overall strategy

## License

Internal use only. Respect third-party website terms of service.
