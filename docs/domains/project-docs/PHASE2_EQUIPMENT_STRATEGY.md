# Phase 2: Equipment Catalog Scraping Strategy

**Date:** 2025-10-25
**Status:** 📋 Planning
**Previous Phase:** [Phase 1 - Vessel Database Scraping](./WEB_SCRAPING_FINDINGS.md)

---

## Executive Summary

After completing Phase 1 (vessel scraping) and validating the Selenium framework, Phase 2 focuses on **equipment catalog scraping** where manufacturer websites offer better data availability than vessel databases.

**Key Advantage:** Equipment manufacturers typically publish detailed product catalogs with specifications on their websites, making them more scrapable than proprietary vessel databases.

---

## Current Equipment Baseline

### Existing Data (as of 2025-10-25)

| Category | Files | Size | Coverage |
|----------|-------|------|----------|
| **Fenders** | 29 PDFs | 74MB | ✅ Comprehensive (design manuals, specs) |
| **Buoys** | 1 PDF | 7.1MB | ⚠️ Limited (SFT brochure only) |
| **Anchors** | 1 image | 264KB | ⚠️ Very limited |
| **Injector Heads** | ~5 PDFs | 3.8MB | ⚠️ Limited manufacturers |
| **Manifolds** | Unknown | 300KB | ⚠️ Limited |
| **SPM** | Text docs | 16KB | ⚠️ Contact info only |
| **X-Trees** | Unknown | 1.8MB | ⚠️ Limited |

**Gaps Identified:**
- Anchors: Need specifications for various types (Hall, Delta, Drag embedment)
- Buoys: Need additional manufacturers beyond SFT
- SPM: Need technical specifications, not just contact info
- All categories: Need 2024-2025 updated specifications

---

## Phase 2 Target Categories

### Priority 1: Fenders 🎯 **HIGH PRIORITY**
**Rationale:** Already have substantial baseline (29 files), easy to expand

**Target Manufacturers:**
- ✅ **NauticExpo** - 560 products from leading brands
  - URL: https://www.nauticexpo.com/boat-manufacturer/fender-996.html
  - Features: Product specs, PDF catalogs, contact manufacturers
  - Brands: JETFLOAT, EZ Dock, FENDERTEX, Maritime International

- ✅ **Marine Fenders International**
  - URL: https://www.marinefendersintl.com/
  - Coverage: Foam filled fenders, resilient buoys
  - PDF catalog available

- ✅ **Pacific Marine & Industrial**
  - URL: https://www.pacificmarine.net/marine-deck/marine-fenders.htm
  - Coverage: Foam, Pneumatic, Cone, Arch, Roller, Used Tire, UHMW, Extruded Rubber

- ✅ **Seazone Co., Ltd.**
  - URL: https://seazonefenders.com/
  - Established: 1997
  - Specialization: Ship berthing fenders

**Expected Data:**
- Fender types and specifications
- Dimensions and capacities
- Material specifications
- Installation requirements
- Performance characteristics

**Scraping Approach:**
- Start with NauticExpo (aggregator site with multiple brands)
- Use Selenium if dynamic content detected
- Extract product tables, specifications, PDF links
- Standardize fender specs across manufacturers

---

### Priority 2: Anchors 🎯 **HIGH PRIORITY**
**Rationale:** Very limited baseline (1 file), significant gap to fill

**Target Manufacturers:**
- ✅ **Fendercare**
  - URL: https://www.fendercare.com/marine-products/mooring-equipment/anchors/
  - Coverage: Hall anchors (50kg-29,000kg), HHP delta (250kg-20,000kg)
  - Detailed specifications available

- ✅ **NauticExpo Ship Anchors**
  - URL: https://www.nauticexpo.com/boat-manufacturer/ship-anchor-21086.html
  - Coverage: Multiple manufacturers, PDF catalogs

- ✅ **Asian Star Anchor Chain**
  - Coverage: Anchor chains with ISO 1704 standards

- ✅ **Vicinay Cadenas**
  - Coverage: Offshore mooring chains

- ✅ **Qingdao Anchor Chain**
  - Coverage: Chinese manufacturer specifications

**Expected Data:**
- Anchor types: Hall, Delta, Drag embedment, Suction
- Weight ranges: 50kg to 40 tonnes
- Holding power specifications
- Material grades and standards (ISO 1704)
- Installation requirements

**Scraping Approach:**
- Start with Fendercare (clean specifications)
- NauticExpo for aggregated data
- Selenium for dynamic manufacturer sites
- Extract weight, capacity, dimensions, standards

---

### Priority 3: Buoys 🔧 **MEDIUM PRIORITY**
**Rationale:** Limited baseline (1 SFT brochure), need diversity

**Target Manufacturers:**
- ✅ **SFT (Trelleborg)** - Already have one brochure
  - Expand with updated catalogs

- ✅ **Marine Fenders International**
  - URL: https://www.marinefendersintl.com/
  - Coverage: Resilient buoys alongside fenders

- ✅ **NauticExpo Buoy Manufacturers**
  - Coverage: Multiple manufacturers and types

**Expected Data:**
- Buoy types and configurations
- Buoyancy specifications
- Dimensions and weights
- Material specifications
- Environmental ratings

---

### Priority 4: SPM (Single Point Mooring) 🔧 **MEDIUM PRIORITY**
**Rationale:** Current data is just contact info, need technical specs

**Target Sources:**
- Technical documentation from major SPM manufacturers
- Engineering specifications
- Cost estimation databases (if publicly available)

**Expected Data:**
- SPM system configurations
- Capacity ratings
- Installation requirements
- Maintenance specifications
- Environmental limits

---

## Scraping Framework Reuse

### Existing Framework (from Phase 1)
✅ **BaseScraper** - HTTP handling, rate limiting, retry logic
✅ **DynamicScraper** - Selenium for JavaScript content
✅ **DataValidator** - Quality scoring and validation
✅ **CLI Interface** - Command-line execution

### New Components Needed

**EquipmentScraper (Base Class)**
```python
class EquipmentScraper(BaseScraper):
    """Base scraper for equipment manufacturer catalogs."""

    def standardize_equipment_specs(self, df: DataFrame) -> DataFrame:
        """Standardize equipment specifications across manufacturers."""
        # Common fields: product_name, type, dimensions, capacity, material, etc.
        pass

    def extract_pdf_catalog_links(self, html: str) -> List[str]:
        """Extract PDF catalog download links."""
        pass

    def validate_equipment_specs(self, df: DataFrame) -> Dict:
        """Validate equipment specifications completeness."""
        pass
```

**Specialized Scrapers:**
- `FenderScraper(EquipmentScraper)` - Fender-specific logic
- `AnchorScraper(EquipmentScraper)` - Anchor-specific logic
- `BuoyScraper(EquipmentScraper)` - Buoy-specific logic
- `SPMScraper(EquipmentScraper)` - SPM-specific logic

---

## Implementation Plan

### Phase 2.1: Framework Extension (1-2 days)
- [x] Analyze current equipment baseline
- [x] Research manufacturer websites
- [ ] Create `EquipmentScraper` base class
- [ ] Implement common equipment validation logic
- [ ] Add PDF catalog download capability

### Phase 2.2: Fender Scraping (1-2 days)
- [ ] Implement `FenderScraper` class
- [ ] Scrape NauticExpo fender listings
- [ ] Scrape Marine Fenders International
- [ ] Scrape Pacific Marine & Industrial
- [ ] Validate and standardize fender data
- [ ] Generate `fenders_2025.csv`

### Phase 2.3: Anchor Scraping (1-2 days)
- [ ] Implement `AnchorScraper` class
- [ ] Scrape Fendercare anchor specifications
- [ ] Scrape NauticExpo anchor listings
- [ ] Scrape major chain manufacturers
- [ ] Validate and standardize anchor data
- [ ] Generate `anchors_2025.csv`

### Phase 2.4: Buoy & SPM Scraping (1 day)
- [ ] Implement `BuoyScraper` class
- [ ] Scrape buoy manufacturer catalogs
- [ ] Implement `SPMScraper` class
- [ ] Scrape SPM technical specifications
- [ ] Generate `buoys_2025.csv` and `spm_2025.csv`

### Phase 2.5: Validation & Documentation (1 day)
- [ ] Run comprehensive validation tests
- [ ] Compare 2025 data vs baseline
- [ ] Document data quality metrics
- [ ] Create Phase 2 findings report
- [ ] Update equipment README

---

## Success Criteria

### Data Quality Targets
- **Coverage:** 90%+ of major manufacturers represented
- **Completeness:** 80%+ of specification fields populated
- **Freshness:** 2024-2025 catalog data
- **Validation:** Quality score ≥ 75/100

### Expected Outcomes
- **Fenders:** +200 product specifications from 5+ manufacturers
- **Anchors:** +100 anchor specifications (50kg to 40 tonnes range)
- **Buoys:** +50 buoy specifications from 3+ manufacturers
- **SPM:** +20 SPM system specifications

### Framework Benefits
- Reusable equipment scraping classes
- Standardized specification format
- Automated validation and quality checks
- CLI for easy execution and updates

---

## Risk Assessment

### Low Risk ✅
- **Fender scraping**: Manufacturers publish detailed specs online
- **Framework reuse**: Phase 1 framework proven to work
- **Data availability**: Multiple sources per category

### Medium Risk ⚠️
- **Dynamic content**: Some sites may require Selenium (already available)
- **PDF extraction**: May need additional parsing for some catalogs
- **Standardization**: Different manufacturers use different formats

### Mitigation Strategies
- Use Selenium framework for dynamic sites
- Implement manufacturer-specific parsers where needed
- Focus on web tables first, PDFs as secondary source
- Manual validation for critical specifications

---

## Comparison: Vessels vs Equipment

| Factor | Vessel Databases (Phase 1) | Equipment Catalogs (Phase 2) |
|--------|---------------------------|------------------------------|
| **Free Sources** | ❌ Scarce | ✅ Abundant (manufacturer sites) |
| **Data Quality** | ⚠️ 73-88% missing | ✅ Expected 80%+ complete |
| **Freshness** | ⚠️ Often outdated | ✅ Current manufacturer specs |
| **Scraping Difficulty** | 🔴 High (paywalls, AJAX) | 🟢 Low (public catalogs) |
| **Framework Reuse** | ✅ Static + Selenium | ✅ Static + Selenium |
| **Expected ROI** | ⚠️ Limited (need commercial) | ✅ High (comprehensive free data) |

**Conclusion:** Equipment scraping (Phase 2) has **significantly better prospects** than vessel scraping (Phase 1) due to manufacturer websites publishing specifications openly for marketing purposes.

---

## Next Steps

**Immediate Actions:**
1. ✅ Complete this strategy document
2. Create `EquipmentScraper` base class
3. Implement `FenderScraper` for NauticExpo
4. Test with real manufacturer websites
5. Iterate based on data quality results

**User Decision Required:**
- Approve Phase 2 priorities (Fenders → Anchors → Buoys → SPM)
- Confirm target manufacturers list
- Set quality thresholds

---

## References

- [Phase 1 Findings](./WEB_SCRAPING_FINDINGS.md)
- [Selenium Setup Guide](./SELENIUM_SETUP.md)
- [Selenium Final Results](./SELENIUM_FINAL_RESULTS.md)
- Equipment Baseline: `data/equipment/README.md`

---

**Status:** Ready to begin implementation pending user approval
