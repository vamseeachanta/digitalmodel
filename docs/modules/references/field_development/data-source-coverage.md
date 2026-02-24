# Field Development Data Source Coverage Map

> Last reviewed: 2026-02-18

## Open Data Sources — Status

| Basin | Regulator / Source | Granularity | WRK | Status |
|-------|--------------------|-------------|-----|--------|
| GoM (US OCS) | BSEE | Well/lease monthly | WRK-019, WRK-077, WRK-111 | **DONE** |
| NCS (Norway) | NPD / Sodir Factpages | Field monthly | WRK-190 | Pending |
| UKCS (UK) | NSTA / OPRED | Field monthly | WRK-193 | Pending (after WRK-190) |
| Brazil pre-salt + Campos | ANP | **Well-level** monthly CSV | WRK-194 | Pending — **high priority** |
| US non-GoM (Permian, Alaska, onshore) | EIA API v2 | State/basin monthly | WRK-195 | Pending |
| Canada NL offshore | C-NLOER | Field monthly | WRK-196 (Part A) | Pending |

## Emerging Basins — Watch List (no open data yet)

| Basin | Operator / FID status | Est. first oil | Data pathway | WRK |
|-------|-----------------------|---------------|--------------|-----|
| Guyana Stabroek | ExxonMobil; 4 FPSOs ~900k bopd | Ongoing; Hammerhead ~2029 | EITI reports (18mo lag) | WRK-196 (Part B stub) |
| Suriname Block 58 | TotalEnergies; FID Oct 2024 | 2028 | Staatsolie annual reports | WRK-196 (Part B stub) |
| Namibia Orange Basin | TotalEnergies Venus; FID ~2026 | Late 2020s | None yet — monitor NAMCOR | WRK-196 (Part B) |
| Falkland Islands Sea Lion | Navitas; FID Dec 2025 | ~2028 | FIG regulator (unclear) | WRK-196 (Part B) |

## Dead Ends (not viable for integration)

| Source | Reason |
|--------|--------|
| Australia NOPTA | Production **permanently confidential** by statute (Regs 8.02/10.02) |
| Angola ANPG | Press releases only; no structured open data |
| IEA MODS | Commercial subscription (~€15,000/year); country-level aggregate only |
| OPEC MOMR/ASB | PDF-only; no API; country aggregate only |
| Nigeria NUPRC | PDF reports with blend-level monthly data — parseable but not field-level |

## Basin Priority Rationale

### Why Brazil ANP is Tier 1
Búzios alone (~800k bopd plateau) is the largest active deepwater development
globally. Brazil added ~0.5M bopd in 2025. ANP provides **well-level** monthly
CSVs with no registration — better granularity than NPD or NSTA. Pre-salt
PSA fiscal regime is also distinct (profit oil split vs royalty/tax), making it
a critical third data point for cross-basin economics.

### Why EIA non-GoM is Tier 1
Permian Basin ~6.6M bopd = ~50% of US total = largest single basin globally.
BSEE misses this entirely. EIA API v2 is the best-documented open API in the
sector. Alaska adds Arctic / harsh environment context. International country
aggregates from EIA complete the global picture where field-level data is unavailable.

### Why Guyana is watch-only
Stabroek production (~900k bopd) rivals the NCS total — but ExxonMobil and the
Guyanese government do not release well-level or field-level monthly data.
A December 2025 Stabroek News op-ed explicitly called on the government to
mandate data release. EITI reports (18+ month lag) are the only structured
pathway. Integrate when regulatory transparency improves.

## Dependency Chain

```
BSEE (done) ─────────────────────────────────────┐
                                                  ▼
NCS/NPD (WRK-190) ──► UKCS/NSTA (WRK-193)      worldenergydata
                                                  multi-basin
Brazil/ANP (WRK-194) ──────────────────────────► framework
                                                  │
EIA non-GoM (WRK-195) ─────────────────────────► │
                                                  ▼
Canada/C-NLOER (WRK-196A) ─────────────────────► Cross-basin
                                                  NPV/DCA
Emerging stubs (WRK-196B) ─── activate on FID ──► (WRK-191, WRK-192)
```
