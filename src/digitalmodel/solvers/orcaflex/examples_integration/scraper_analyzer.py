#!/usr/bin/env python
"""
Analyze the Orcina examples website structure to understand how to scrape it.
"""

import requests
from bs4 import BeautifulSoup
import json
from urllib.parse import urljoin, urlparse
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def analyze_orcina_site():
    """Analyze the Orcina examples page structure."""
    
    base_url = "https://www.orcina.com/resources/examples/"
    
    # Headers to appear as a regular browser
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
    }
    
    try:
        logger.info(f"Fetching {base_url}")
        response = requests.get(base_url, headers=headers, timeout=10)
        response.raise_for_status()
        
        soup = BeautifulSoup(response.text, 'html.parser')
        
        analysis = {
            'title': soup.title.string if soup.title else 'No title',
            'links': [],
            'categories': [],
            'download_links': [],
            'structure': {}
        }
        
        # Find all links
        all_links = soup.find_all('a', href=True)
        logger.info(f"Found {len(all_links)} total links")
        
        # Look for example files
        for link in all_links:
            href = link.get('href', '')
            text = link.get_text(strip=True)
            
            # Check for OrcaFlex file extensions
            if any(ext in href.lower() for ext in ['.dat', '.sim', '.yml', '.yaml']):
                full_url = urljoin(base_url, href)
                analysis['download_links'].append({
                    'text': text,
                    'url': full_url,
                    'extension': href.split('.')[-1].lower()
                })
                logger.info(f"Found download: {text} -> {href}")
            
            # Check for category-like links
            if 'example' in text.lower() or 'sample' in text.lower():
                analysis['categories'].append({
                    'text': text,
                    'url': urljoin(base_url, href)
                })
        
        # Analyze page structure
        # Look for common patterns
        divs_with_class = soup.find_all('div', class_=True)
        class_names = set()
        for div in divs_with_class:
            classes = div.get('class', [])
            class_names.update(classes)
        
        analysis['structure']['div_classes'] = list(class_names)[:20]  # Top 20 classes
        
        # Look for tables (often used for file listings)
        tables = soup.find_all('table')
        analysis['structure']['table_count'] = len(tables)
        
        # Look for lists
        ul_lists = soup.find_all('ul')
        ol_lists = soup.find_all('ol')
        analysis['structure']['list_counts'] = {
            'ul': len(ul_lists),
            'ol': len(ol_lists)
        }
        
        # Look for specific patterns that might indicate examples
        # Check for sections or articles
        sections = soup.find_all(['section', 'article'])
        analysis['structure']['sections'] = len(sections)
        
        # Save analysis
        with open('orcina_site_analysis.json', 'w') as f:
            json.dump(analysis, f, indent=2)
        
        logger.info(f"Analysis saved to orcina_site_analysis.json")
        
        # Print summary
        print("\n" + "="*60)
        print("ORCINA WEBSITE ANALYSIS SUMMARY")
        print("="*60)
        print(f"Page Title: {analysis['title']}")
        print(f"Total Links: {len(all_links)}")
        print(f"Download Links Found: {len(analysis['download_links'])}")
        print(f"Potential Categories: {len(analysis['categories'])}")
        print(f"Tables on Page: {analysis['structure']['table_count']}")
        print(f"Lists: UL={analysis['structure']['list_counts']['ul']}, OL={analysis['structure']['list_counts']['ol']}")
        
        if analysis['download_links']:
            print("\nSample Download Links:")
            for dl in analysis['download_links'][:5]:
                print(f"  - {dl['text']} ({dl['extension']})")
        
        if analysis['categories']:
            print("\nPotential Categories:")
            for cat in analysis['categories'][:5]:
                print(f"  - {cat['text']}")
        
        return analysis
        
    except requests.RequestException as e:
        logger.error(f"Failed to fetch website: {e}")
        return None
    except Exception as e:
        logger.error(f"Analysis failed: {e}")
        return None


if __name__ == "__main__":
    analyze_orcina_site()