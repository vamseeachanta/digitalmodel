# Field-development source evidence pack

The pack structures the research note on the issue, pages checked 2026-10-11. Four distinct passages flagged by the repository writing-register rule were restated; facts, numbers, refs and locators are unchanged, and the note's exact wording is retained in the companion data file. Values are as stated by the source: no conversions, sums or inferences.

"Unverified" means the research pass did not open a page that states it (search-result snippet only, or the page would not load). Quotes are the fragments returned by the fetch tool; some are part-sentences. Re-open the URL before pasting any quote into a dataset as verbatim.

The owner choice accepts contractor award scope for tree and manifold counts labelled as-contracted and makes tieback distance optional. NO record is accepted in this pack.

## Outcome in one paragraph

**Zero of the ten provisional records pass the gate in this issue** (all of `num_trees`, `num_manifolds`, `tieback_distance_km`, `cost_usd_bn` from a durable primary source with one consistent definition). The threshold of 10 is not met, so the downstream benchmark issue should stay `status:needs-data`. What this pass did establish: tree and manifold counts from contractor award releases for two developments (Stones, Lucius); well counts from operator releases or SEC filings for four (Whale, Appomattox, Lucius, Na Kika); and one sanctioned cost (Mad Dog Phase 2, USD 9 billion), though only as trade press reporting the operator's statement. **No primary source opened in this pass states a well-to-host or cluster-to-host tieback distance for any of the ten**, and no primary source states a full-project cost for nine of the ten.

Scope followed: the ten records and the fields in the provisional dataset (`num_trees`, `num_manifolds`, `tieback_distance_km`, `cost_usd_bn`, plus water depth, host, capacity, start-up, operator). All pages checked 2026-10-11. Values are as stated by the source: no conversions, sums or inferences. "Unverified" means the research pass did not open a page that states it (search-result snippet only, or the page would not load).

## Access limits that shaped the result

- Operator sites (Shell, bp, BHP) and Subsea 7 returned script-only pages, HTTP 403 or a bot challenge. Shell releases were therefore read as wire-service copies (PR Newswire and its syndication); bp releases could not be opened at all, and the older bp release URLs now redirect to a listing page.
- OnePetro (OTC/SPE/JPT abstracts) returned HTTP 403. **No paper-class rows.**
- PDFs could not be parsed on this host (BSEE weekly deepwater activity reports, a co-owner start-up release, contractor PDF releases). **No regulator-class rows.**
- SEC filings opened, but long 20-F documents were read in 100,000-character windows through a summarising fetch tool; a sentence can be missed. Where a snippet claims a sentence the research pass did not see, it is marked unverified.
- Quotes are the fragments returned by the fetch tool; some are part-sentences. Re-open the URL before pasting any quote into a dataset as verbatim.

## Sources opened

| Ref | Source title | Publisher | Date | Class | Reuse terms as stated | URL |
|---|---|---|---|---|---|---|
| S1 | Shell invests in the Whale development in the Gulf of Mexico | Shell Offshore Inc., via PR Newswire | 2021-07-26 | operator (wire copy) | "Copyright © 2026 PR Newswire Europe Limited. All Rights Reserved." | https://www.prnewswire.co.uk/news-releases/shell-invests-in-the-whale-development-in-the-gulf-of-mexico-832964558 |
| S2 | Shell starts production at Whale in the United States Gulf of Mexico | Shell Offshore Inc., via PR Newswire (syndicated copy) | 2025-01-10 | operator (wire copy) | terms not stated | https://www.aap.com.au/aapreleases/cision20250109ae92810 |
| S3 | McDermott Completes EPCIC Project in Gulf of Mexico | McDermott International, Ltd. | 2025-03-05 | contractor | "Copyright @ 2026 McDermott. All rights reserved." | https://www.mcdermott.com/press-release-detail/123045/mcdermott-completes-epcic-project-gulf-mexico |
| S4 | Shell takes final investment decision for the Appomattox deep-water development in Gulf of Mexico | Royal Dutch Shell plc, via PR Newswire, reproduced by Water Online | 2015-07-01 | operator (release reproduced on a third-party site; not compared with the original) | terms not stated | https://www.wateronline.com/doc/shell-takes-final-investment-decision-appomattox-deep-water-gulf-of-mexico-0001 |
| S5 | Shell starts production at Appomattox (release of 23 May 2019) | Shell Offshore Inc., via PR Newswire / Kyodo News PR Wire | 2019-05-23 | operator (wire copy) | terms not stated | https://kyodonewsprwire.jp/release/201905246755 |
| S6 | FMC Technologies Awarded Contract for Shell Appomattox | FMC Technologies (TechnipFMC archive) | 2015-09-22 | contractor | terms not stated | https://www.technipfmc.com/en/investors/archives/fmc-technologies/press-releases/fmc-technologies-awarded-contract-for-shell-appomattox |
| S7 | Presentation to the Independent Directors of Shell Midstream Partners GP LLC, Exhibit (c)(3) to Schedule 13E-3 | SEC EDGAR (prepared by the committee's financial adviser; marked preliminary working draft) | 2022-03-28 | filing | terms not stated | https://www.sec.gov/Archives/edgar/data/1610466/000119312522223312/d367578dex99c3.htm |
| S8 | BP p.l.c. Form 6-K, period ended 31 December 2016 | SEC EDGAR | 2017-02 (board approval 6 February 2017) | filing | terms not stated | https://www.sec.gov/Archives/edgar/data/0000313807/000119312517032625/d344237d6k.htm |
| S9 | BP sanctions Mad Dog Phase 2 project | Offshore magazine | 2016-12-01 | secondary | terms not stated | https://www.offshore-mag.com/deepwater/article/16769564/bp-sanctions-mad-dog-phase-2-project |
| S10 | FMC Technologies Awarded Subsea Contract for BP's Mad Dog Phase 2 | FMC Technologies (TechnipFMC archive) | 2013-03-19 | contractor | terms not stated | https://www.technipfmc.com/en/investors/archives/fmc-technologies/press-releases/fmc-technologies-awarded-subsea-contract-for-bps-mad-dog-phase-2/ |
| S11 | BP Awards Schlumberger Contract for Mad Dog 2 Project | OneSubsea / SLB newsroom | 2017-03-21 | contractor | "Copyright © 2026 SLB. All rights reserved." | https://www.slb.com/zh-cn/newsroom/press-release/2017/pr-2017-0321-slb-bp |
| S12 | Shell starts production at Perdido | Oilfield Technology | 2010-04-06 | secondary | terms not stated | https://www.oilfieldtechnology.com/drilling-and-production/06042010/Shell_starts_production_at_Perdido/ |
| S13 | FMC Technologies Awarded Subsea Equipment Contract for Shell's Stones Field | FMC Technologies (TechnipFMC archive) | 2013-05-23 | contractor | terms not stated | https://www.technipfmc.com/en/investors/archives/fmc-technologies/press-releases/fmc-technologies-awarded-subsea-equipment-contract-for-shells-stones-field/ |
| S14 | First oil for Turritella | SBM Offshore | 2016-09-09 | contractor | "Copyright © SBM Offshore 2026. All Rights Reserved" | https://www.sbmoffshore.com/newsroom/first-oil-turritella/ |
| S15 | Anadarko Petroleum Corporation Form 10-K, fiscal year 2014 | SEC EDGAR | 2015 (cover share count as of 30 January 2015) | filing | terms not stated | https://www.sec.gov/Archives/edgar/data/0000773910/000077391015000012/apc201410k-10k.htm |
| S16 | FMC Technologies Awarded Subsea Systems Contract From Anadarko Petroleum Corporation | FMC Technologies (TechnipFMC archive) | 2012-01-10 | contractor | terms not stated | https://www.technipfmc.com/en/investors/archives/fmc-technologies/press-releases/fmc-technologies-awarded-subsea-systems-contract-from-anadarko-petroleum-corporation/ |
| S17 | BP p.l.c. Form 20-F, fiscal year 2007 | SEC EDGAR | 2008 | filing | terms not stated | https://www.sec.gov/Archives/edgar/data/0000313807/000115697308000263/u54999_20f.htm |
| S18 | BP p.l.c. Form 20-F, fiscal year 2003 | SEC EDGAR | 2004 | filing | terms not stated | https://www.sec.gov/Archives/edgar/data/0000313807/000119312504110485/d20f.htm |

## Evidence table

Columns: quantity; value and unit as stated; quoted fragment; source ref; class. Rows marked UNVERIFIED were not seen on an opened page.

### 1. Whale (semi-submersible host)

| # | Quantity | Value as stated | Quote | Ref | Class |
|---|---|---|---|---|---|
| 1 | Operator and partners | Shell Offshore Inc. 60% operator; Chevron U.S.A. Inc. 40% | "The Whale development, owned by Shell Offshore Inc. (60% operator) and Chevron U.S.A. Inc. (40%)" | S1 | operator |
| 2 | Peak production (at FID) | approximately 100,000 boe/d | "is expected to reach peak production of approximately 100,000 barrels of oil equivalent per day (boe/d)" | S1 | operator |
| 3 | Recoverable resource (at FID) | 490 million boe | "currently has an estimated, recoverable resource volume of 490 million boe." | S1 | operator |
| 4 | Scheduled first production (at FID) | 2024 | "is currently scheduled to begin production in 2024." | S1 | operator |
| 5 | Sanction economics (no cost stated) | internal rate of return greater than 25% | "Shell anticipates an internal rate of return estimated to be greater than 25%." | S1 | operator |
| 6 | Host type and water depth | more than 8,600 feet (2,600 metres) | "a semi-submersible production host in more than 8,600 feet (2,600 metres) of water" | S2 | operator |
| 7 | Wells | a total of 15 wells | "a total of 15 wells to be tied back to the host via subsea infrastructure" | S2 | operator |
| 8 | Peak production (at start-up) | 100,000 boe/d | "With an estimated peak production of 100,000 barrels of oil equivalent per day (boe/d)" | S2 | operator |
| 9 | Recoverable resource (at start-up) | 480 million boe (differs from row 3) | "Whale currently has an estimated recoverable resource volume of 480 million boe." | S2 | operator |
| 10 | Location | Alaminos Canyon Block 773 | "The Whale production facility is in the Alaminos Canyon Block 773" | S2 | operator |
| 11 | Pipeline installed (total, not a tieback distance) | approximately 50 kilometers (30 miles) | "installing approximately 50 kilometers (30 miles) of pipeline" | S3 | contractor |
| 12 | Umbilicals installed | 15 kilometers (9 miles) | fetch tool paraphrased this sentence; fragment "15 kilometers (9 miles) of umbilicals" not captured verbatim | S3 | contractor |
| 13 | Subsea drill centers | five | paraphrased by fetch tool: five subsea drill centers connected to the Whale floating production platform | S3 | contractor |
| 14 | Riser type and count | five steel catenary risers, 3,350 meters long (approximately 11,000 feet) | "it installed five 3,350 meters-long (approximately 11,000 feet), steel catenary risers" | S3 | contractor |
| 15 | Water depth of installation work | nearly 2,800 meters (9,100 feet) | "reaching water depths of nearly 2,800 meters (9,100 feet)" | S3 | contractor |
| 16 | Distance host to neighbouring spar | around 10 miles (16 kilometres) | UNVERIFIED: search snippet of the S2 release only; the fetched copy did not return this sentence | S2 | unverified |

No primary source found: tree count as such, manifold/PLEM/PLET count, drill-centre-to-host distance, sanctioned cost. Tried: both Shell releases, the McDermott release, shell.com and pages at shell.us (script-only).

### 2. Appomattox (semi-submersible host)

| # | Quantity | Value as stated | Quote | Ref | Class |
|---|---|---|---|---|---|
| 17 | Water depth (at FID) | approximately 7,200 feet (2,195 metres) | "in approximately 7,200 feet (2,195 metres) of water" | S4 | operator |
| 18 | Distance offshore | 80 miles (129 kilometres) | "located 80 miles offshore (129 kilometres) from the nearest shoreline in Louisiana" | S4 | operator |
| 19 | Drill centres and wells | six drill centres; 15 producing wells; five water injection wells | "a subsea system featuring six drill centres, 15 producing wells, and five water injection wells" | S4 | operator |
| 20 | Peak production (at FID) | approximately 175,000 boe per day | "average peak production estimated to reach approximately 175,000 barrels of oil equivalent (boe) per day" | S4 | operator |
| 21 | Resources sanctioned | 650 million boe | "capital for the development of 650 million boe resources at Appomattox and Vicksburg" | S4 | operator |
| 22 | Ownership | Shell 79%; Nexen Petroleum Offshore U.S.A. Inc. 21% | "will be owned by Shell (79%) and Nexen Petroleum Offshore U.S.A. Inc. (21%)" | S4 | operator |
| 23 | Export pipeline size | 24-inch | "a 24-inch corridor pipeline that will transport crude oil from the Appomattox host" | S4 | operator |
| 24 | Cost statement at FID (no absolute cost) | total project cost reduced by 20% | "Shell reduced the total project cost by 20% through supply chain savings" | S4 | operator |
| 25 | Breakeven at FID | around $55 per barrel Brent equivalent | "the go-forward project breakeven price is estimated to be around $55 per barrel Brent equivalent" | S4 | operator |
| 26 | Expected production (at start-up) | 175,000 boe/d | "Appomattox, which currently has an expected production of 175,000 barrels of oil equivalent per day (boe/d)," | S5 | operator |
| 27 | Cost statement at start-up (no absolute cost) | cost reductions of more than 40% | "Appomattox has realised cost reductions of more than 40% since taking final investment decision in 2015." | S5 | operator |
| 28 | Subsea equipment scope (no counts) | trees, subsea manifolds, controls, distribution | "will provide Enhanced Vertical Deepwater Trees, subsea manifolds, topside controls, a control system, and a distribution system" | S6 | contractor |
| 29 | Water depth and location (contractor) | 7,200 feet; 140 nautical miles southeast of New Orleans | "in 7,200 feet of water, 140 nautical miles southeast of New Orleans" | S6 | contractor |
| 30 | Export pipeline length, size, capacity | 90-mile, 24-inch, 300 MBbl/d | "90-mile, 24-inch system with capacity of 300 MBbl/d" | S7 | filing |
| 31 | Export pipeline ownership (2022) | 79.0% held by the midstream partnership | "Shell operates the Mattox pipeline system; 79.0% ownership interest held by SHLX" | S7 | filing |

No primary source found: tree count, manifold count, well-to-host distance, absolute project cost. The 7,400 feet (2,255 m) water depth in the provisional record appears only in trade coverage of the start-up (unverified); the opened start-up release (S5) states no depth.

### 3. Mad Dog Phase 2 / Argos (semi-submersible host)

| # | Quantity | Value as stated | Quote | Ref | Class |
|---|---|---|---|---|---|
| 32 | Operator and partners at sanction | BP operator 60.5%; BHP 23.9%; Chevron 15.6% | "the sanction of the Mad Dog Phase 2 project in the Gulf of Mexico (BP operator, 60.5%, BHP 23.9%, and Chevron 15.6%)." | S8 | filing |
| 33 | Sanctioned cost | $9-billion | "BP has approved the $9-billion Mad Dog Phase 2 project in the deepwater Gulf of Mexico." | S9 | secondary |
| 34 | Capacity and production wells | up to 140,000 b/d; up to 14 production wells | "with the capacity to produce up to 140,000 b/d of crude oil from up to 14 production wells" | S9 | secondary |
| 35 | Water depth | 4,500 ft (1,372 m) | "located in 4,500 ft (1,372 m) of water about 190 mi (306 km) south of New Orleans" | S9 | secondary |
| 36 | Separation from existing spar (host to host, not a tieback) | about 6 mi (10 km) | "about 6 mi (10 km) southwest of the existing Mad Dog platform" | S9 | secondary |
| 37 | First oil expected at sanction | late 2021 | "Oil production is expected to begin in late 2021." | S9 | secondary |
| 38 | Location and water depth (2013, before the redesign) | 150 miles (240 kilometers) south of New Orleans; about 5,100 feet (1,550 meters) | "150 miles (240 kilometers) south of New Orleans in about 5,100 feet (1,550 meters) of water" | S10 | contractor |
| 39 | Subsea scope, 2013 contract (no counts) | subsea trees, manifolds, jumper equipment | "Under the initial contract, FMC Technologies will supply subsea trees, manifolds, and jumper equipment." | S10 | contractor |
| 40 | Subsea production system scope, 2017 contract (no counts) | subsea manifolds, trees, control system | "The scope of this supplier-led solution includes subsea manifolds, trees, control system" | S11 | contractor |
| 41 | Water injection wells | eight | UNVERIFIED: snippet of a 2018 operator release; that URL now redirects to a listing page | - | unverified |
| 42 | Co-owner share of cost | US$2.2 billion | UNVERIFIED: snippet of the co-owner release of 9 February 2017; page returned HTTP 403 | - | unverified |
| 43 | Start-up | April 2023; up to 140,000 barrels of crude oil per day | UNVERIFIED: co-owner release of 14 April 2023 is a PDF that could not be parsed here | - | unverified |

Two different water depths are stated (rows 35 and 38) and neither matches the provisional 1,800 m. No primary source found: tree count, manifold count, drill-centre-to-host distance. Rows 33 to 37 are trade press reporting the operator's statement; the operator's own release could not be opened.

### 4. Perdido (spar host)

| # | Quantity | Value as stated | Quote | Ref | Class |
|---|---|---|---|---|---|
| 44 | Water depth | approximately 2450 m (8000 ft) | "The facility sits in approximately 2450 m (8000 ft) of water" | S12 | secondary |
| 45 | Wells over field life | perhaps as many as 35 wells | "requiring perhaps as many as 35 wells over the life of the fields." | S12 | secondary |
| 46 | Ownership | Shell 35%; Chevron 37.5%; BP 27.5% | "which is jointly owned by Shell (35%), Chevron (37.5%) and BP (27.5%)." | S12 | secondary |
| 47 | Peak production | more than 100 000 bpd of oil equivalent | "will ramp up to annual peak production of more than 100 000 bpd of oil equivalent." | S12 | secondary |
| 48 | Fields served | Great White, Silvertip, Tobago | "The facility will produce from the Great White, Silvertip and Tobago offshore fields," | S12 | secondary |
| 49 | Contractor subsea scope (earlier statement) | 17 subsea trees; two subsea manifolds; five subsea caisson separation and boosting systems | UNVERIFIED: trade-press snippet; contractor archive page not located | - | unverified |
| 50 | Contractor subsea scope (later statement) | 25 subsea trees; two subsea manifolds; five caisson systems | UNVERIFIED: trade-press snippet of a contractor release; the trade page did not load the article | - | unverified |
| 51 | Well split | 22 wells from the spar; 13 offset wells tied back | UNVERIFIED: trade-press snippet | - | unverified |

No primary source opened for any Perdido quantity. Tried: shell.com project page (script-only), contractor archive (slug guess returned 404; archive search returned nothing), OnePetro JPT/OTC pages (HTTP 403). Rows 49 and 50 disagree on tree count and are the best leads for a manifold count of two.

### 5. Stones (FPSO host)

| # | Quantity | Value as stated | Quote | Ref | Class |
|---|---|---|---|---|---|
| 52 | Subsea trees (contract scope) | eight | "scope of supply includes eight subsea trees, a subsea manifold, topside and subsea controls and associated equipment." | S13 | contractor |
| 53 | Subsea manifolds (contract scope) | a subsea manifold (one) | same sentence as row 52 | S13 | contractor |
| 54 | Water depth (contractor, 2013) | approximately 9,600 feet (2,926 meters) | "coast in approximately 9,600 feet (2,926 meters) of water." | S13 | contractor |
| 55 | Distance offshore | approximately 200 miles (322 kilometers) off the Louisiana coast | "approximately 200 miles (322 kilometers) off the Louisiana coast" | S13 | contractor |
| 56 | Water depth (FPSO contractor, 2016) | 2,900 metres (9,500 feet) | "At 2,900 metres (9,500 feet), it is the deepest production unit ever installed" | S14 | contractor |
| 57 | Topsides oil capacity | 60,000 barrels of oil per day | "processing facility capacity of 60,000 barrels of oil per day (bopd)" | S14 | contractor |
| 58 | Riser type | steel lazy-wave risers, disconnectable system | "the first disconnectable system with steel lazy-wave risers to go into use" | S14 | contractor |
| 59 | FPSO contract term | initial 10 years, extensions up to 20 years | "an initial period of 10 years with future extension options up to a total of 20 years." | S14 | contractor |
| 60 | First oil | announced 9 September 2016 | "has achieved first oil." | S14 | contractor |
| 61 | Well phasing and peak rate | two subsea production wells, then six more; around 50,000 boe/d | UNVERIFIED: snippets of trade reprints of the operator releases | - | unverified |

No primary source found: well-to-FPSO tieback distance, project cost. Two water depths stated (rows 54 and 56). Tried: OnePetro OTC page from the provisional record (HTTP 403), shell.com (script-only), archived operator investor handbook (redirects away).

### 6. Lucius (truss spar host)

| # | Quantity | Value as stated | Quote | Ref | Class |
|---|---|---|---|---|---|
| 62 | First production and operator | January 2015; Anadarko-operated | "The Company realized first production at the Anadarko-operated Lucius Spar in January 2015" | S15 | filing |
| 63 | Wells at start-up | three wells initially, additional three expected | "bringing on three wells initially and ramping up production with an additional three wells expected to come online" | S15 | filing |
| 64 | Capacity, block, water depth | 80-MBbls/d; Keathley Canyon Block 875; 7,100 feet | "The 80-MBbls/d spar resides in Keathley Canyon Block 875 with a water depth of 7,100 feet." | S15 | filing |
| 65 | Operator working interest | 23.8% | "the Company holds a 23.8% working interest in Lucius." | S15 | filing |
| 66 | Schedule | start-up three years from sanction | "developed with production startup only three years from sanction" | S15 | filing |
| 67 | Subsea trees and manifolds (contract scope) | five subsea production trees; two manifolds | "FMC's scope of supply includes five subsea production trees and two manifolds." | S16 | contractor |
| 68 | Location and water depth (contractor) | approximately 275 miles southeast of Galveston; approximately 7,100 feet (2,160 meters) | "in Keathley Canyon Block 875, in water depths of approximately 7,100 feet (2,160 meters)." | S16 | contractor |
| 69 | Water depth in the following year's 10-K | 7,000 feet | UNVERIFIED: search snippet of the fiscal 2015 10-K | - | unverified |

Rows 63 and 67 differ: six wells brought on (filing) against five production trees in the 2012 contract scope (contractor). Both are kept. No primary source found: well-to-spar tieback distance, project cost.

### 7. Thunder Horse (semi-submersible PDQ host)

| # | Quantity | Value as stated | Quote | Ref | Class |
|---|---|---|---|---|---|
| 70 | Operator interest | BP 75% and operator | "At Thunder Horse (BP 75% and operator), as a result of a metallurgical failure during pre-commissioning checks in 2006," | S17 | filing |
| 71 | Expected start-up (as of fiscal 2007) | by the end of 2008 | "Thunder Horse is expected to start production by the end of 2008." | S17 | filing |
| 72 | Topsides design capacity | 250,000 barrels of oil per day; 200 million cubic feet per day of natural gas | "Designed to process 250,000 barrels of oil per day and 200 million cubic feet per day of natural gas," | S17 | filing |
| 73 | Subsea wells | 25 | UNVERIFIED: a search snippet attributes "25 subsea wells" to S17; not seen in the first 300,000 of 353,714 characters read | S17 | unverified |
| 74 | Expansion project (2018) | new subsea manifold; two wells; about 2 miles north of the platform | UNVERIFIED: trade-press snippets of an operator release | - | unverified |

No primary source found: manifold count, base-development tieback distance, project cost, water depth. Tried: fiscal 2003 and 2007 20-F, operator press pages (HTTP 403 or script-only).

### 8. Atlantis (semi-submersible host)

| # | Quantity | Value as stated | Quote | Ref | Class |
|---|---|---|---|---|---|
| 75 | Operator interest | BP 56% and operator | "The Atlantis platform (BP 56% and operator) was successfully commissioned" | S17 | filing |
| 76 | First production | fourth quarter of 2007 | fragment: "during the fourth quarter of 2007." | S17 | filing |
| 77 | Phase 3 expansion cost and scope | $1.3 billion; eight new wells | UNVERIFIED: trade-press snippets of the January 2019 operator release; operator page did not render | - | unverified |

No primary source found: tree count, manifold count, tieback distance, base-development cost, capacity, water depth. Peak uplift for Phase 3 appears as 38,000 boe/d in 2019 coverage and about 36,000 boe/d in a 2020 operator page snippet (both unverified).

### 9. Mars / Mars B Olympus (TLP hosts)

| # | Quantity | Value as stated | Quote | Ref | Class |
|---|---|---|---|---|---|
| 78 | Non-operating partner interest | 28.5% | production-table row for Mars listing a 28.5% interest (table cell, as reported by the fetch tool) | S17 | filing |
| 79 | Second-TLP facts | 24 well slots; approximately 945 m (3,100 ft); estimated peak 100,000 boe/d; operator 71.5%, partner 28.5% | UNVERIFIED: trade-press snippets of the February 2014 operator release | - | unverified |

No primary source opened for any equipment, distance or cost quantity. Tried: wire-service search for the 2014 release (not found), archived operator investor handbook pages (redirect away).

### 10. Na Kika (semi-submersible hub)

| # | Quantity | Value as stated | Quote | Ref | Class |
|---|---|---|---|---|---|
| 80 | First production and operator | November 2003; BP 50% and operator | "Production from the Na Kika Development (BP 50% and operator) commenced in November 2003." | S18 | filing |
| 81 | Fields and subsea wells | 5 fields; 10 subsea wells | "The development consists of 5 fields and 10 subsea wells connected to a centrally-located floating host facility." | S18 | filing |
| 82 | Flowline length and well offsets | some 90 miles of flowlines; satellite wells five to 12 miles from the platform | UNVERIFIED: trade project-page snippet | - | unverified |

No primary source found: manifold count, tieback distance, project cost, water depth, capacity. Row 82 is the best lead for a tieback definition but is secondary and unverified.

## Row counts by source class

| Class | Rows |
|---|---|
| operator (all read as wire copies or reproductions) | 21 |
| contractor | 21 |
| filing | 16 |
| secondary (opened, reporting an operator statement) | 10 |
| regulator | 0 |
| paper | 0 |
| unverified | 14 |
| Total | 82 |

## Provisional dataset: confirmed, corrected, unsupported

Gate fields first, per record. "Unsupported" means no primary source found in this pass.

| Record | `num_trees` | `num_manifolds` | `tieback_distance_km` | `cost_usd_bn` |
|---|---|---|---|---|
| Perdido | 35: unsupported as a tree count; secondary wording is "perhaps as many as 35 wells" (row 45) | 3: unsupported (proxy); unverified leads say two (rows 49, 50) | 110: unsupported | 3.0: unsupported (seed) |
| Appomattox | 18: unsupported; operator states 15 producing wells and five water injection wells (row 19) | 4: unsupported; contractor release gives no count (row 28) | 145: wrong definition (export pipeline); filing states "90-mile" export line (row 30) | 4.0: unsupported; only percentage reductions stated (rows 24, 27) |
| Whale | 15: confirmed as a well count, "a total of 15 wells" (row 7) | 5: unsupported; five is the riser count and the drill-center count (rows 13, 14) | 16: wrong definition (host-to-neighbouring-host), and that sentence is unverified (row 16) | 5.0: unsupported (seed); release gives a rate of return only (row 5) |
| Atlantis | 18: unsupported | 2: unsupported (proxy) | 0: unsupported (placeholder) | 1.3: phase-only and unverified (row 77) |
| Thunder Horse | 25: unverified lead (row 73) | 2: unsupported (proxy) | 3.2: expansion-only and unverified (row 74) | 5.0: unsupported (seed) |
| Mars / Olympus | 24: unsupported as a tree count (well slots; unverified, row 79) | 2: unsupported (proxy) | 0: unsupported (placeholder) | 1.0: unsupported (trade page only) |
| Mad Dog Phase 2 | 22: the sum is not sourced; "up to 14 production wells" is secondary (row 34), eight injectors unverified (row 41) | 5: unsupported (drill-centre proxy) | 10: wrong definition (host-to-host separation, row 36) | 9.0: supported at secondary level only, "$9-billion" (row 33) |
| Stones | 8: confirmed, "eight subsea trees" in contract scope (row 52) | 2: corrected to one, "a subsea manifold" in contract scope (row 53) | 20: unsupported | 2.0: unsupported (seed) |
| Lucius | 6: confirmed as wells (row 63); contract scope states five subsea production trees (row 67); both kept | 2: confirmed, "two manifolds" in contract scope (row 67) | 0: unsupported (placeholder) | 2.0: unsupported (seed) |
| Na Kika | 10: confirmed as "10 subsea wells" (row 81) | 6: unsupported; filing states 5 fields, not six (row 81) | 0: unsupported (placeholder) | 1.3: unsupported (seed) |

Context fields:

- Confirmed: Whale water depth 2,600 m ("more than") and 2025 start-up; Stones water depth 2,900 m, 60,000 bopd and 2016 start-up; Lucius 2,160 m, 80,000 bopd and 2015 start-up; Thunder Horse 250,000 bopd design capacity; Na Kika 2003 start-up.
- Corrected: Appomattox capacity 125,000 bopd to "approximately 175,000 boe per day" (rows 20, 26); Appomattox water depth 2,255 m against "approximately 7,200 feet (2,195 metres)" at FID (row 17); Whale capacity unit is boe/d, not bopd (rows 2, 8); Mad Dog Phase 2 water depth 1,800 m against 4,500 ft (1,372 m) and about 5,100 feet (1,550 meters) (rows 35, 38); Lucius operator is the filer of S15, not the "context" pairing in the record (row 62); Na Kika operator at production is BP per S18 (row 80); Perdido capacity is stated as "more than 100 000 bpd of oil equivalent" (secondary, row 47).
- Unsupported: Atlantis 200,000 bopd and 2,156 m; Thunder Horse 1,844 m and actual 2008 start-up (filing gives an expectation only); Mars 896 m, 1996 start-up and 100,000 bopd; Na Kika 1,920 m and 110,000 bopd; Perdido 2,438 m (secondary states approximately 2450 m).

## Gap list

1. Tieback distance, all ten records: no primary source states a well, cluster or drill-centre distance to host. Needs OTC/SPE paper text or regulator pipeline-segment data, neither reachable here.
2. Manifold/PLEM/PLET count: missing for eight of ten (all but Stones and Lucius). Contractor releases for Appomattox and Mad Dog Phase 2 name manifolds without counts.
3. Tree count as such: stated only for Stones and Lucius. Elsewhere only well counts are stated, and wells are not trees.
4. Project cost with year and scope: no primary statement for any record. Mad Dog Phase 2 has a secondary report of the operator figure; Appomattox and Whale operators published percentages and a rate of return, not costs.
5. Regulator data (BSEE/BOEM water depths, structure and pipeline records): not read, PDF and interactive-query formats only.
6. Paper abstracts (OTC/SPE/JPT) for Perdido, Stones, Na Kika, Thunder Horse: blocked.
7. Operator originals for all Shell and bp releases: wire copies were used for Shell; nothing for bp. The Appomattox FID quotes (rows 17 to 25, source S4) come from a third-party reproduction and should be re-checked against the original.
8. Whale umbilical and drill-center sentences (rows 12, 13) need a verbatim re-capture from S3.

## Recommendation

Keep the parent data-backfill issue open and keep the downstream benchmark issue on `status:needs-data`: fewer than 10 records pass (0 pass all four fields; 2 pass trees plus manifolds at contractor-scope level). Two decisions would change what is achievable: whether contractor award scope counts as an accepted source for tree and manifold counts (as-contracted, not as-built), and whether the gate should require `tieback_distance_km` at all given no public primary statement was found for any of the ten. A follow-up pass from a host that can read PDFs and reach OnePetro and regulator data would close gaps 1, 5 and 6 fastest.
