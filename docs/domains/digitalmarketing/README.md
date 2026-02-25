# Digital Marketing Module

SEO analysis and web analytics tools for digital marketing workflows.

## Overview

This module provides comprehensive SEO analysis capabilities including:

- **SEO Analysis**: Multi-tool website SEO evaluation using seolib, seoanalyzer, and seoaudit
- **Google Analytics**: Integration with Google Analytics Reporting API V4
- **SEO Tools**: Utilities for checking indexed pages, parsing robots.txt, and finding search rankings

## Installation

The module requires optional dependencies for full functionality:

```bash
pip install seolib seoanalyzer seoaudit google-api-python-client oauth2client
```

## Usage

### Basic SEO Analysis

```python
from digitalmodel.specialized.digitalmarketing import DigitalMarketing

cfg = {
    "basename": "digitalmarketing",
    "inputs": {
        "url": "https://example.com",
        "alias": "my_site"
    },
    "analysis": {
        "seo": {
            "pyseoanalyzer": True,
            "seolib": True,
            "seoaudit": True
        }
    },
    "calculation": {
        "name": "seo_analysis"
    }
}

dm = DigitalMarketing()
result = dm.router(cfg)
print(result["results"]["seo_analysis"])
```

### Using Individual Tools

```python
from digitalmodel.specialized.digitalmarketing.tools import (
    IndexPageCounter,
    RobotsParser,
    SearchRankFinder
)

# Check indexed pages
counter = IndexPageCounter()
count = counter.get_indexed_count("example.com")

# Parse robots.txt
parser = RobotsParser()
rules = parser.parse("https://example.com")

# Find search ranking
finder = SearchRankFinder()
ranks = finder.find_rank("my keyword", "example.com", num_results=30)
```

## Module Structure

```
digitalmarketing/
├── digitalmarketing.py      # Router class
├── seo/
│   └── seo_analysis.py      # Core SEO analysis
├── analytics/
│   └── google_analytics.py  # Google Analytics integration
├── tools/
│   ├── index_page_count.py  # Indexed page checker
│   ├── robots_parser.py     # Robots.txt parser
│   └── search_rank.py       # Search rank finder
└── config/
    └── analysis.yaml        # Default configuration
```

## Documentation

- [SEO Fundamentals](theory/seo-fundamentals.md)
- [Advanced SEO Overview](theory/seo-advanced-overview.md)
- [Web Analytics](theory/web-analytics.md)
- [SEO Epic](epics/seo-epic.md)
- [Python Libraries](tools/python-libraries.md)

## API Reference

### DigitalMarketing

Main router class that delegates to appropriate analysis tools.

#### `router(cfg: dict) -> dict`

Routes configuration to the appropriate analysis based on `cfg["calculation"]["name"]`.

**Supported calculations:**
- `seo_analysis`: Run comprehensive SEO analysis

### SEOAnalysis

Core SEO analysis class.

#### `run(cfg: dict) -> dict`

Run SEO analysis based on configuration.

**Configuration options:**
- `cfg["inputs"]["url"]`: Website URL to analyze (required)
- `cfg["inputs"]["alias"]`: Identifier for the analysis
- `cfg["analysis"]["seo"]["pyseoanalyzer"]`: Enable pyseoanalyzer
- `cfg["analysis"]["seo"]["seolib"]`: Enable seolib ranks
- `cfg["analysis"]["seo"]["seoaudit"]`: Enable SEO audit

## Contributing

See the [development guidelines](../../CONTRIBUTING.md) for information on contributing to this module.
