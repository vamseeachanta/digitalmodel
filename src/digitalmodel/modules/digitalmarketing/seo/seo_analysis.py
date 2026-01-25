"""SEO Analysis module for comprehensive website SEO evaluation."""

import os
import glob
import datetime
import json
import logging
from typing import Any

import pandas as pd

try:
    import seolib
except ImportError:
    seolib = None

try:
    from seoanalyzer import analyze
except ImportError:
    analyze = None

try:
    from seoaudit.analyzer.site_parser import SiteParser, LXMLPageParser
    from seoaudit.analyzer.seo_auditor import SEOAuditor
    import seoaudit.config as seoaudit_cfg
except ImportError:
    SiteParser = None
    SEOAuditor = None


class SEOAnalysis:
    """Comprehensive SEO analysis using multiple tools."""

    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.seo_columns = [
            'alias', 'seoanalyzer_analyses', 'seolib_ranks',
            'seo_audit', 'seo_audit_summary', 'updated_time', 'seo_rank'
        ]

    def run(self, cfg: dict) -> dict:
        """Run SEO analysis based on configuration.

        Args:
            cfg: Configuration dict with 'inputs' and 'analysis' sections.

        Returns:
            Updated cfg with 'results' containing SEO analysis data.
        """
        url = cfg.get("inputs", {}).get("url", "")
        alias = cfg.get("inputs", {}).get("alias", "")
        analysis_cfg = cfg.get("analysis", {}).get("seo", {})

        if not url:
            raise ValueError("URL is required in cfg['inputs']['url']")

        seo_results = self.perform_seo_analysis(url, analysis_cfg)
        seo_results["alias"] = alias
        seo_results["updated_time"] = datetime.datetime.now().strftime('%Y-%m-%d %H:%M')

        cfg["results"] = {"seo_analysis": seo_results}
        return cfg

    def perform_seo_analysis(self, url: str, analysis_cfg: dict) -> dict:
        """Perform SEO analysis on a URL.

        Args:
            url: Website URL to analyze.
            analysis_cfg: Configuration for which analyses to run.

        Returns:
            Dictionary with analysis results.
        """
        seo_analysis = {}

        if analysis_cfg.get('pyseoanalyzer') and analyze:
            self.logger.info(f"Running pyseoanalyzer on {url}")
            seoanalyzer_analyses = self.seoanalyzer_analyze(url)
            seo_analysis['seoanalyzer_analyses'] = seoanalyzer_analyses

        if analysis_cfg.get('seolib') and seolib:
            self.logger.info(f"Running seolib on {url}")
            seolib_ranks = self.get_seolib_ranks(url)
            seo_analysis['seolib_ranks'] = seolib_ranks

        if analysis_cfg.get('seoaudit') and SEOAuditor:
            self.logger.info(f"Running seoaudit on {url}")
            seo_audit = self.get_seo_audit(url)
            seo_analysis['seo_audit'] = seo_audit
            self._cleanup_seo_audit_outputs()

        seo_rank = self._calculate_seo_rank(seo_analysis)
        seo_analysis['seo_rank'] = seo_rank

        return seo_analysis

    def _calculate_seo_rank(self, seo_analysis: dict) -> float:
        """Calculate composite SEO rank from analysis results."""
        seo_audit_rank = 0
        seolib_rank = 0

        if 'seo_audit' in seo_analysis:
            summary = seo_analysis['seo_audit'].get('seo_audit_summary', {})
            seo_audit_rank = summary.get('seo_audit_rank', 0)

        if 'seolib_ranks' in seo_analysis:
            for item in seo_analysis['seolib_ranks']:
                if item.get('Description') == 'Seolib Rank':
                    seolib_rank = item.get('Value', 0) or 0

        seo_rank = 0.2 * seo_audit_rank + 0.8 * seolib_rank
        return round(seo_rank, 2)

    def seoanalyzer_analyze(self, site: str) -> dict:
        """Run seoanalyzer on a site."""
        if not analyze:
            self.logger.warning("seoanalyzer not installed")
            return {}
        return analyze(site)

    def get_seolib_ranks(self, site: str) -> list:
        """Get SEO ranks using seolib."""
        if not seolib:
            self.logger.warning("seolib not installed")
            return []

        ranks = []
        none_count = 0
        total_count = 0

        # Alexa rank
        alexa = seolib.get_alexa(site)
        total_count += 1
        if alexa is None:
            none_count += 1
        ranks.append({'Description': 'Alexa Rank', 'Value': alexa})

        # Google Plus
        google_plus = seolib.get_google_plus(site)
        total_count += 1
        if google_plus is None:
            none_count += 1
        ranks.append({'Description': 'Google Plus', 'Value': google_plus})

        # Semrush
        semrush = seolib.get_semrush(site)
        total_count += 1
        if semrush is None:
            none_count += 1
        ranks.append({'Description': 'Semrush Top 20', 'Value': semrush})

        # Tweets
        try:
            tweets = seolib.get_tweets(site)
        except Exception:
            tweets = None
        total_count += 1
        if tweets is None:
            none_count += 1
        ranks.append({'Description': 'Tweets', 'Value': tweets})

        # Facebook likes
        facebook_likes = seolib.get_facebook_likes(site)
        total_count += 1
        if facebook_likes is None:
            none_count += 1
        ranks.append({'Description': 'Facebook Likes', 'Value': facebook_likes})

        # SEOMoz data
        try:
            seomoz_data = seolib.get_seomoz_data(site)
        except Exception:
            seomoz_data = None
        total_count += 1
        if seomoz_data is None:
            none_count += 1
        ranks.append({'Description': 'Seomoz Data', 'Value': seomoz_data})

        # Compute overall rank
        seolib_rank = round((total_count - none_count) / total_count * 100, 3)
        ranks.append({'Description': 'Seolib Rank', 'Value': seolib_rank})

        return ranks

    def get_seo_audit(self, url: str) -> dict:
        """Run SEO audit on a URL."""
        if not SEOAuditor:
            self.logger.warning("seoaudit not installed")
            return {}

        site_parser = SiteParser(
            url, LXMLPageParser(url),
            urls=[url], sitemap_link=None, parse_sitemap_urls=False
        )
        auditer = SEOAuditor(
            url, site_parser,
            seoaudit_cfg.page_tests, seoaudit_cfg.element_tests
        )
        auditer.run_checks_for_site()
        seoaudit_results = auditer.get_results()[url]

        df = pd.DataFrame(seoaudit_results)
        seo_audit_summary = self._get_seo_audit_summary(df)

        return {
            'seoaudit_results': seoaudit_results,
            'seo_audit_summary': seo_audit_summary
        }

    def _get_seo_audit_summary(self, df: pd.DataFrame) -> dict:
        """Summarize SEO audit results."""
        if len(df) == 0:
            return {}

        true_count = df['result'][df['result'] == True].count()
        false_count = df['result'][df['result'] == False].count()
        total_count = true_count + false_count

        if total_count == 0:
            return {'true_count': 0, 'false_count': 0, 'seo_audit_rank': 0}

        seo_audit_rank = round(true_count / total_count * 100, 3)
        return {
            'true_count': int(true_count),
            'false_count': int(false_count),
            'seo_audit_rank': seo_audit_rank
        }

    def _cleanup_seo_audit_outputs(self):
        """Remove temporary SEO audit output files."""
        for filename in glob.glob("seo_audit_*.json"):
            os.remove(filename)
        for filename in glob.glob("seo_audit_*.html"):
            os.remove(filename)
