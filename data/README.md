# digitalmodel — Engineering Reference Data (Tier 2)

> Policy: `workspace-hub/docs/DATA_RESIDENCE_POLICY.md` (ADR-004)

This directory contains engineering reference data from industry standards and codes.
Per the Data Residence Policy, this is **Tier 2** data — lookup tables and parameters
consumed by engineering analysis code.

## Directory Structure

| Directory | Contents | Sources |
|-----------|----------|---------|
| `fatigue/` | SN curve parameters | DNV-RP-C203, API RP 2A, BS 7608, AWS D1.1 |
| `materials/` | Steel grade properties | API 5L, ASTM A106 |

## Rules

- Data here comes from engineering standards, not external public sources (that's Tier 1 → worldenergydata)
- Data here is NOT project-specific (that's Tier 3 → project repos)
- External data dependencies are declared in `config/data_sources.yaml`
