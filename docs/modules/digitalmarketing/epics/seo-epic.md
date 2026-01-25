# Introduction

The key properties for a good SEO site are documented in this document. A good course:
https://www.jcchouinard.com/python-for-seo/
https://www.seobility.net/en/wiki/HTML5


**Objective**
1. Google SEO … need current state and website improvement recommendations
    1. SEO ranking score … should highlight where the website is today. <35% poor; 35%-60% ok; 60%-80% better; 80%-100% good and target zone. Questions: how does Google calculate this - based on rank for each keyword that belongs to company industry/segment or some other calculation? Comments: A lot of companies are calculating this score at keyword level and at aggregate level. How are they doing it?
    2. Top 10 website attributes that influence the Google SEO rank … what are these attributes, to what extent do they impact the SEO rank, how long it takes for SEO rank to change after attribute changes are made and finally, how to programmatically quantify these attributes to illustrate as is vs future state. The quantification should also help in calculating SEO  rank eventually. Some attributes are: content quality, page loading speed, keywords presence, back links etc

Goal: The program when completed should output website attribute scores, and a Google SEO rank scores. Attribute scores will help explain the quality of the website along multiple dimensions and the Google SEO rank should help explain where the website will feature in the eyes of Google’s Organic Search


## Summary

- no.of. indexed pages for an URL
- BERT Score (TBA)
    - Defines content relavancy to user questions
    - Content is about answering user questions
    - https://www.jcchouinard.com/get-bert-score-for-seo-by-pierre-rouarch/
- Parse robots.txt (TBA)
    - Contains the Average Dload and current speeds
    - user-agent status and patterns
    - Exact use for SEO is to define pages for search engines and disallow unwanted (repetitive content) pages to avoid penalties
- Keyword density and entity calculation
    - https://www.jcchouinard.com/keyword-and-entity-calculator-using-knowledge-graph-api/
- 

Other potential parameters:
- Audit scores : Defines content quality. Need more analysis to understand correlation with seo rank.

Several tools are available to help perform seo analysis. 

The current health indicator analysis concept is given below:

Preliminary rank is evaluated using following formula. This will be further improved.
seo_rank = 0.8 * seolib_rank + 0.2 * seoaudit_rank

<img src="epic_seo_flowchart.svg" width=300, height=auto/>

https://www.singlegrain.com/seo/semrush-alternatives/

Feedback: Should be able to explain concepts for marketability (Think of end in mind).
ranks
seoaudit checks
content analysis
(back)link analysis

https://blog.adeptmarketing.com/how-to-turn-semrush-google-analytics-data-into-tables

## SEO 



## TODO - Programming

- automate 'alias' name for saving url results (use: if key not in dict_keys or key is None)
- save parquet files for:
    - single company results
    - for industry company records

## SEO Rank


https://www.woorank.com/ (SEO Checker)

https://www.duplichecker.com/website-seo-score-checker.php

https://www.crazyegg.com/blog/score-website-seo/


https://ahrefs.com/webmaster-tools

https://search.google.com/search-console/about

https://whatagraph.com/blog/articles/how-to-track-seo-ranking

https://youtu.be/3VFeN6XMXT0    export data from ahref and assess them

### Backlink


https://www.wpbeginner.com/glossary/backlinks/
https://www.searchenginejournal.com/competitor-backlinks-python/451132/#close
https://stackoverflow.com/questions/18234298/how-to-find-backlinks-in-a-website-with-python


### Semrush

https://www.semrush.com/blog/google-keyword-ranking/

### Other techniques

Regex, NLP tools/Stemming etc.

https://www.searchenginejournal.com/python-libraries-for-seo/399283/#close
https://python.plainenglish.io/seo-with-python-bf9ff4e93884
https://practicaldatascience.co.uk/data-science/19-python-seo-projects-that-will-improve-your-site

https://www.seoradar.com/how-to-use-python-seo/

https://www.jcchouinard.com/python-libraries-for-seo/
https://openbase.com/categories/python/best-python-seo-libraries

## References


[web analytics](https://www.fullstackpython.com/web-analytics.html)

