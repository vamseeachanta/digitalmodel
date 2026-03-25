# Data Procurement Strategy - Web Enhancement Plan

**Date:** 2025-10-24
**Status:** Active
**Purpose:** Systematic web data enhancement for marine engineering repository

---

## 📊 Current Data Assessment

### Data Freshness Analysis

| Domain | Current Data | Age | Priority | Status |
|--------|-------------|-----|----------|--------|
| **Vessels** | 2013-2018 | 7-12 years old | 🔴 **HIGH** | Outdated |
| **OCIMF** | Unknown year | Unknown | 🟡 **MEDIUM** | Validate |
| **Fatigue** | 1984-2020 standards | Current | 🟢 **LOW** | Up to date |
| **Hydrodynamic** | Unknown year | Unknown | 🟡 **MEDIUM** | Validate |
| **Equipment** | Mixed (PDFs from 2019-2021) | 3-5 years | 🟡 **MEDIUM** | Partial update |
| **Riser Systems** | Unknown year | Unknown | 🟡 **MEDIUM** | Validate |
| **Reference Materials** | 2014-2018 posters | 7-11 years old | 🟢 **LOW** | Historical |

### Identified Data Gaps

**Critical Gaps:**
1. ❌ **Vessel databases** - No data newer than 2018
2. ❌ **Equipment manufacturers** - Limited catalog coverage
3. ❌ **OCIMF coefficients** - Year unknown, possibly outdated
4. ❌ **Market data** - No vessel charter rates, construction costs
5. ❌ **Standards updates** - Need 2020-2025 revisions

**Missing Domains:**
- ⚠️ Production risers specifications
- ⚠️ Export risers specifications
- ⚠️ Current equipment pricing
- ⚠️ Environmental/metocean databases
- ⚠️ Regulatory compliance databases

---

## 🎯 Procurement Priorities (Phase-Based)

### Phase 1: Critical Updates (Week 1)

**Priority: Vessels Database Update**

**Target Data:**
- FPSOs commissioned 2019-2025
- Deepwater drilling rigs 2019-2025
- Jackup rigs 2019-2025
- Pipelay vessels 2019-2025

**Web Sources:**
1. **Offshore Magazine** - https://www.offshore-mag.com
   - Fleet databases
   - Vessel specifications
   - Format: Web tables → CSV export

2. **Rigzone** - https://www.rigzone.com
   - Rig specifications
   - Fleet status
   - Format: Database queries → CSV

3. **IHS Markit / S&P Global** (if accessible)
   - Comprehensive vessel databases
   - Format: CSV export

4. **Operator Websites**
   - Individual FPSO specifications
   - Format: PDF → manual CSV entry

**Expected Output:**
- `vessels/raw/fpso_database_2025.csv`
- `vessels/raw/drilling_rigs_deepwater_2025.csv`
- `vessels/raw/jackup_rigs_2025.csv`
- `vessels/raw/pipelay_vessels_2025.csv`

---

### Phase 2: Equipment Catalogs (Week 2)

**Priority: Equipment Specifications**

**Target Data:**
- Updated anchor manufacturers (2024-2025 catalogs)
- Buoy specifications (updated SFT, Trelleborg)
- Fender systems (latest Yokohama, SFT catalogs)
- SPM manufacturers and pricing
- Subsea equipment (manifolds, X-trees)

**Web Sources:**
1. **Manufacturer Websites:**
   - **Anchors:** Vryhof, Bruce, Stevpris
   - **Buoys:** SFT, Trelleborg, Sealite
   - **Fenders:** Yokohama, Trelleborg, SFT
   - **SPM:** SBM Offshore, Sofec
   - **Subsea:** TechnipFMC, Aker Solutions, OneSubsea

2. **Industry Catalogs:**
   - Equipment specification sheets (PDF → extract to CSV)
   - Product brochures with technical data

**Expected Output:**
- `equipment/raw/anchors/manufacturer_catalogs_2025.csv`
- `equipment/raw/buoys/specifications_2025.csv`
- `equipment/raw/fenders/updated_catalogs_2025.csv`
- `equipment/raw/spm/vendor_pricing_2025.csv`

---

### Phase 3: OCIMF & Mooring Updates (Week 3)

**Priority: OCIMF Coefficients Validation**

**Target Data:**
- Verify current OCIMF database version
- Check for coefficient updates (2020-2025)
- New vessel types (LNG carriers, FSRUs)
- Updated wind/current coefficients

**Web Sources:**
1. **OCIMF Publications** - https://www.ocimf.org
   - Mooring Equipment Guidelines (MEG4)
   - Prediction of Wind and Current Loads
   - Format: PDF reports → extract tables to CSV

2. **Classification Societies:**
   - DNV, ABS, BV, Lloyd's Register
   - Updated mooring guidelines
   - Format: Technical papers → CSV

**Expected Output:**
- `mooring/raw/ocimf/ocimf_coefficients_2025.csv`
- `mooring/raw/ocimf/new_vessel_types_2025.csv`
- `mooring/raw/components/updated_chain_specifications.csv`

---

### Phase 4: Standards & Regulations (Week 4)

**Priority: Updated Engineering Standards**

**Target Data:**
- API standards (2020-2025 revisions)
- DNV standards (updated classifications)
- ISO standards (offshore/maritime)
- Regional regulations (US, UK, Norway, Brazil)

**Web Sources:**
1. **API** - https://www.api.org/products-and-services/standards
2. **DNV** - https://www.dnv.com/rules-standards/
3. **ISO** - https://www.iso.org/standards.html
4. **NORSOK** - https://www.standard.no/norsok

**Expected Output:**
- `reference_materials/raw/standards/api_standards_2025.csv`
- `reference_materials/raw/standards/dnv_standards_2025.csv`
- Updated fatigue curves if new revisions available

---

### Phase 5: Market & Economic Data (Week 5)

**Priority: Vessel Economics**

**Target Data:**
- Dayrates for drilling rigs
- Charter rates for FPSOs, pipelay vessels
- Construction costs (newbuilds)
- Equipment pricing trends

**Web Sources:**
1. **IHS Markit / Bassoe Analytics**
2. **Clarksons Research**
3. **Offshore Magazine Market Reports**
4. **Industry Conference Proceedings**

**Expected Output:**
- `vessels/raw/charter_rates_2025.csv`
- `vessels/raw/construction_costs_2025.csv`
- `equipment/raw/market_pricing_2025.csv`

---

## 🛠️ Web Scraping Methodology

### Tools & Techniques

**1. Manual Data Collection (PDFs, Non-Scrapable Sites)**
```
Process:
1. Download PDF/document
2. Extract tables using Tabula or manual entry
3. Convert to CSV format
4. Validate against template
5. Save to domain/raw/ directory
```

**2. Web Table Extraction (HTML Tables)**
```python
import pandas as pd

# Direct table extraction
url = "https://example.com/vessel-database"
tables = pd.read_html(url)
df = tables[0]  # Select relevant table
df.to_csv('data/vessels/raw/source_data_2025.csv', index=False)
```

**3. API-Based Data Collection (If Available)**
```python
import requests
import pandas as pd

# Example: Vessel tracking API
response = requests.get('https://api.example.com/vessels')
data = response.json()
df = pd.DataFrame(data)
df.to_csv('data/vessels/raw/api_data_2025.csv', index=False)
```

**4. Selenium/Playwright (Dynamic Content)**
```python
from selenium import webdriver

# For JavaScript-heavy sites
driver = webdriver.Chrome()
driver.get('https://dynamic-content-site.com')
# Extract data from rendered page
```

---

## 📋 Data Quality Standards

### Required Metadata for All Collected Data

For each web-sourced dataset, create metadata file:

```json
{
  "dataset_name": "FPSO Database 2025",
  "domain": "vessels",
  "data_source": {
    "name": "Offshore Magazine",
    "url": "https://www.offshore-mag.com/fpso-database",
    "access_date": "2025-10-24",
    "license": "Public / Fair Use"
  },
  "collection_method": "Manual PDF extraction",
  "quality_checks": {
    "missing_data": "2%",
    "duplicates": "0",
    "validated": true
  }
}
```

### Validation Checklist

Before committing web-sourced data:
- ✅ **Format Check:** CSV with clear headers, consistent units
- ✅ **Completeness:** All required fields populated
- ✅ **Accuracy:** Cross-check against 2+ sources if possible
- ✅ **Metadata:** Source URL, access date, license documented
- ✅ **Template Compliance:** Matches domain template structure

---

## 🔄 Procurement Workflow

```
┌─────────────────────────────────────────────┐
│ 1. IDENTIFY DATA NEED                       │
│    - Review current data gaps               │
│    - Prioritize by project requirements     │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│ 2. LOCATE WEB SOURCES                       │
│    - Industry websites                      │
│    - Manufacturer catalogs                  │
│    - Standards organizations                │
│    - Academic databases                     │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│ 3. COLLECT DATA                             │
│    - Web scraping / API calls               │
│    - Manual PDF extraction                  │
│    - Table conversion to CSV                │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│ 4. VALIDATE & CLEAN                         │
│    - Check completeness                     │
│    - Verify accuracy                        │
│    - Standardize format                     │
│    - Create metadata file                   │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│ 5. SAVE TO REPOSITORY                       │
│    - Place in domain/raw/ directory         │
│    - Follow naming convention               │
│    - Include metadata JSON                  │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│ 6. DOCUMENT & COMMIT                        │
│    - Update domain README.md                │
│    - Add entry to METADATA.json             │
│    - Git commit with source attribution     │
└─────────────────┬───────────────────────────┘
                  │
┌─────────────────▼───────────────────────────┐
│ 7. PROCESS & ANALYZE                        │
│    - Run processing scripts                 │
│    - Save to domain/processed/              │
│    - Generate results/reports               │
└─────────────────────────────────────────────┘
```

---

## 📅 Implementation Timeline

| Week | Phase | Domains | Expected Output |
|------|-------|---------|-----------------|
| **1** | Vessels Update | vessels/ | 4 updated CSV databases |
| **2** | Equipment Catalogs | equipment/ | 5+ manufacturer catalogs |
| **3** | OCIMF & Mooring | mooring/ | Updated coefficients |
| **4** | Standards | reference_materials/ | Latest standards |
| **5** | Market Data | vessels/, equipment/ | Economic data |

**Estimated Total:** 20-30 new/updated datasets

---

## 🎯 Success Metrics

**Quantitative:**
- [ ] 90%+ of vessel databases updated to 2024-2025
- [ ] 10+ equipment manufacturer catalogs added
- [ ] OCIMF database validated/updated
- [ ] 5+ industry standards updated
- [ ] All data sources documented with metadata

**Qualitative:**
- [ ] Data freshness: Average age < 2 years
- [ ] Coverage: All major manufacturers represented
- [ ] Accessibility: All data in CSV/ASCII format
- [ ] Documentation: Every dataset has README entry
- [ ] Usability: Data ready for analysis without preprocessing

---

## 🔍 Recommended Web Sources by Domain

### Vessels
- Offshore Magazine: https://www.offshore-mag.com
- Rigzone: https://www.rigzone.com
- IHS Markit (Petrodata): https://ihsmarkit.com
- Clarksons Research: https://www.clarksons.com
- Bassoe Analytics: https://bassoe.no

### Equipment
- **Anchors:** Vryhof, Bruce Anchor, Stevpris
- **Mooring:** Vicinay, Ramnas, Asian Star
- **Fenders:** Yokohama, Trelleborg, SFT
- **Subsea:** TechnipFMC, Aker Solutions, OneSubsea, Dril-Quip

### Standards & Regulations
- API: https://www.api.org
- DNV: https://www.dnv.com
- ISO: https://www.iso.org
- NORSOK: https://www.standard.no/norsok
- OCIMF: https://www.ocimf.org

### Market Data
- Offshore Magazine Market Intelligence
- Energy Maritime Associates
- Infield Systems (Westwood Global)
- Douglas-Westwood Research

---

## 📝 Next Immediate Actions

**To Begin Phase 1 (This Week):**

1. **Start with FPSO Database Update**
   - Visit https://www.offshore-mag.com
   - Search for "FPSO database" or "FPSO fleet"
   - Extract data to CSV using template
   - Save as `data/vessels/raw/fpso_database_2025.csv`

2. **Update Drilling Rigs**
   - Visit https://www.rigzone.com
   - Navigate to rig database section
   - Export/extract deepwater and jackup rigs
   - Save as separate CSV files

3. **Document Sources**
   - Create metadata JSON for each dataset
   - Update `vessels/README.md` with new data sources
   - Update `data/METADATA.json` statistics

4. **Commit Progress**
   ```bash
   git add data/vessels/raw/*_2025.csv
   git commit -m "data: Add 2025 vessel database updates from web sources"
   ```

---

## 🚨 Important Notes

**Legal & Ethical Considerations:**
- ✅ Use publicly available data
- ✅ Respect robots.txt and terms of service
- ✅ Attribute sources properly
- ✅ Check data licenses before commercial use
- ❌ Avoid excessive automated requests
- ❌ Don't bypass paywalls or authentication

**Data Privacy:**
- No personal information
- No proprietary vendor pricing (unless public)
- Aggregate market data only

---

**Document Version:** 1.0
**Last Updated:** 2025-10-24
**Status:** Ready for Execution

**Related Documents:**
- data/README.md - Data catalog
- data/templates/README.md - Template usage
- docs/DATA_REORGANIZATION_PLAN.md - Structure reference
