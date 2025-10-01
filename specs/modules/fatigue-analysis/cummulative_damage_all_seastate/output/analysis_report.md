# Cumulative Fatigue Damage Analysis Report

**Date:** 2025-09-25 02:55:13
**Version:** 1.0.0

## Executive Summary

- **Total Combinations Analyzed:** 112
- **Average Fatigue Life:** inf years
- **Fatigue Life Range:** inf - inf years
- **Standard Deviation:** nan years

## Methodology

This analysis uses Miner's rule for cumulative damage calculation:
1. Individual damage rates from stress rainflow analysis
2. Weighted by sea state occurrence probabilities
3. Summed to get total annual damage rate
4. Fatigue life = 1 / total damage rate

## Results Overview

### By Configuration

| configuration            |   ('fatigue_life_years', 'mean') |   ('fatigue_life_years', 'min') |   ('fatigue_life_years', 'max') |   ('fatigue_life_years', 'std') |
|:-------------------------|---------------------------------:|--------------------------------:|--------------------------------:|--------------------------------:|
| fsts_l015                |                              inf |                             inf |                             inf |                             nan |
| fsts_l015_125km3_l100_pb |                              inf |                             inf |                             inf |                             nan |

## Critical Findings

### Components with Shortest Fatigue Life

| configuration   |   strut | location   |   fatigue_life_years | critical_fc   |   critical_fc_contribution |
|:----------------|--------:|:-----------|---------------------:|:--------------|---------------------------:|
| fsts_l015       |       1 | loc02      |                  inf | FC001         |                          0 |
| fsts_l015       |       1 | loc03      |                  inf | FC001         |                          0 |
| fsts_l015       |       1 | loc05      |                  inf | FC001         |                          0 |
| fsts_l015       |       1 | loc06      |                  inf | FC001         |                          0 |
| fsts_l015       |       1 | loc07      |                  inf | FC001         |                          0 |
| fsts_l015       |       1 | loc09      |                  inf | FC001         |                          0 |
| fsts_l015       |       1 | loc10      |                  inf | FC001         |                          0 |
| fsts_l015       |       2 | loc02      |                  inf | FC001         |                          0 |
| fsts_l015       |       2 | loc03      |                  inf | FC001         |                          0 |
| fsts_l015       |       2 | loc05      |                  inf | FC001         |                          0 |

### Most Influential Fatigue Conditions

| Fatigue Condition | Count as Critical |
|-------------------|------------------|
| FC001 | 112 |

## Recommendations

1. **Priority Inspection:** Focus on components with fatigue life < 20 years
2. **Design Review:** Consider strengthening locations with high damage rates
3. **Monitoring:** Implement continuous monitoring for critical components
4. **Maintenance Planning:** Schedule based on calculated fatigue life
5. **Further Analysis:** Consider more detailed FEA for critical locations

