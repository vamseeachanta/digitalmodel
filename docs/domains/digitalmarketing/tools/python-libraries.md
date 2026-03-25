# Introduction

Digital marketing and associated activities has seen a burgeon of python libraries. This document briefly summarizes the library features and notes

# Summary

A brief summary is given below. For more details on seo libraries and the respective results, see below sections. 

| Library | Description | Area |
|--------|-------|----------|
| python-seo-analyzer | Analyze all the pages and sub pages of a website for content, errors, duplicate pages etc. | seo |
| seolib | Analyze website social media ranking or standing from various social platforms | seo |
| seoaudit | performs the seo related audit tests on content, html elements etc.  | seo |
| advertools | sem: Generate keywords, create text ads (google & facebook) <br>seo: sitemaps, crawler & crawl strategies <br>content analysis: url, emoji tools, stop words, abs & weighted word count <br>social media data APIs: twitter, youtube  | sem, seo & smm |
| scrapy | extracts url data and get insights from data | smm data analysis |

## Python Libraries in Detail

### advertools

 - SEO, Text Content Analysis, Social Media (Twitter & Youtube APIs). Tool collection with lots of information to gather from.

### python-seo-analyzer

Analyzes all the pages and sub pages of a website. The detailed notes are below:

- analyze is the main (and only) function. Key outputs are summarized below:

| Keyword | type | Comments
|--------|-------|----------|
| pages | array | ? |
| keywords | array | ? |
| errors | array | ? |
| total_time | float | ? |
| duplicate_pages | array | ? |

- Key conclusions:
    - The keywords are stemmed.
    - Some stem words are not meaningful? eg. tongue vs. tongu. Understand them further
    
- Way forward
    - Research if this is how SEO (partial word) search works?. Studying other packages will give clues

[website link analysis python library](https://www.python.org/success-stories/python-seo-link-analyzer/)
https://github.com/sethblack/python-seo-analyzer
https://pypi.org/project/pyseoanalyzer/
https://youtu.be/f5ktiAmGFF8


### seolib

- Analyzes the popular social media ranking or standing from various social platforms. The key outputs are:
    - Google +1 count
    - Alexa Rank
    - Tweets count
    - Facebook likes count
    - SemRush Top 20 keywords count
    - SeoMoz.org free API data (Page Authority, Domain Authority)

Technical Problem:
- no module api found error. 
    - Had to add a hack code to the location where library is installed.
    - This error is worked around by adding path.

https://pypi.org/project/seolib/


### seoaudit

- performs the seo related audit tests on content, html elements etc. 
- The following audit tests are performed and the associated criteria are given below. 

| Check | Check Argument | Criteria |
|--------|-------|----------|
| TEXT_TO_CODE_RATIO | min_ratio | 
| DOM_SIZE | "max_size" | 1500
| ELEMENTS_SIMILARITY |"el1_query": "/*", "el2_query": "/html/head/title" | "match_most_common": 1
| ELEMENTS_SIMILARITY |"el1_query": "/*", "el2_query": "/html/head/meta[@name='description']/@content" | "match_most_common": 1
| ELEMENTS_SIMILARITY |"el1_query": "//h1", "el2_query": "/html/head/meta[@name='description']/@content" | "match_most_common": 1
| ELEMENTS_COUNT | "query": "(//h2)", "min_count" | 2
| STRUCTURED_DATA_FOUND | "type": "json-ld", "property": "@type", "value": "Organization"
| TITLE_REPETITION |
| DESCRIPTION_REPETITION |
| PAGE_IN_SITEMAP |
| PAGE_CRAWLABLE |
| ATTRIBUTE_FOUND |
| MIN_LENGTH | "/html/head/title", 'textContent' | 40
| MAX_LENGTH |"/html/head/title", 'textContent' | 70
| MIN_LENGTH | (/html/head/meta[@name='description'])", 'content', | 40
| ELEMENT_FOUND | @property='og:locale'])" | 
| ELEMENT_FOUND | @property='og:title'])" | 
| ELEMENT_FOUND | @property='og:description']) | 
| ELEMENT_FOUND | @property='og:type']) | 
| ELEMENT_FOUND | @property='og:url']) | 
| ELEMENT_FOUND | @property='og:image']) | 
| ELEMENT_FOUND | @name='twitter:title']) | 
| ELEMENT_FOUND | @name='twitter:description'] | 
| ELEMENT_FOUND | @name='twitter:image']) | 
| ELEMENT_FOUND | @name='twitter:card']) | 
| ELEMENT_FOUND | @rel='canonical'])", 'href')  | 

- The default pass-fail criteria are given in library config file i.e. "envs\digitalmarketing\Lib\site-packages\seoaudit\config.py"  
- These defaults pass-fail criteria can be overidden using a custom config file. Possibly make a copy of default config.py and pass it? Testing pending.

https://github.com/Guber/seoaudit
https://seoaudit.readthedocs.io/en/latest/api.html#

Way forward: 
- SEO rank can be defined by categorizing seoaudit outputs and counting the the audit pass/fail rates
- 


### Scrapy

- extracts url data and get insights from data.
- Combines, requests + pandas together.

### pytrend

Google trends can be used to determine the following:
- Interest Over Time
- Historical Hourly Interest
- Interest by Region
- Related Topics
- Related Queries
- Trending Searches
- Realtime Search Trends
- Top Charts
- Suggestions

https://pypi.org/project/pytrends/

https://towardsdatascience.com/google-trends-api-for-python-a84bc25db88f
https://lazarinastoy.com/the-ultimate-guide-to-pytrends-google-trends-api-with-python/


### Google search using Python

https://www.highervisibility.com/seo/learn/check-google-rankings/

https://developers.google.com/analytics/devguides/reporting/core/v4/quickstart/service-py

Service Accounts: Get .json file
https://console.cloud.google.com/

Get view ID: Error: "There was an error while requesting your accounts."
https://ga-dev-tools.web.app/account-explorer/


https://janakiev.com/blog/python-google-analytics/


https://medium.com/@tmmylo1021/extract-google-analytics-data-with-python-221626ed8975


https://www.geeksforgeeks.org/performing-google-search-using-python-code/

Check rank on Google
https://saradoesseo.com/wedding-seo-tips/check-ranking-on-google/

https://www.geeksforgeeks.org/how-to-get-rank-of-page-in-google-search-results-using-beautifulsoup/?ref=rp

