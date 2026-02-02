# Cleansed Well Test Data for Artificial Lift

## Overview
This directory contains anonymized production well data derived from historical KBR and Oxy Cipher engineering projects. It is used to ensure the robustness of `digitalmodel` algorithms against real-world well configurations.

## Data Cleansing (Anonymization)
All files in this directory have been processed to remove proprietary information:
- **API14:** Replaced with generic identifiers (e.g., `API-CLEANSED-001`).
- **Well Names:** Replaced with generic aliases (e.g., `Well-A`).
- **Timestamps:** Preserved only where relative timing is critical for analysis.

## Dataset Contents
The dataset includes JSON files representing diverse well scenarios:
- **Vertical and Deviated Wellbores:** Including survey data.
- **Tapered Rod Strings:** Various diameters and lengths.
- **Surface Cards:** Raw dynamometer measurements including noise and harmonics.

## Usage
These files are automatically loaded by the `test_dynacard_cleansed.py` robustness suite. They can also be used for local troubleshooting and validation of new physics solvers.