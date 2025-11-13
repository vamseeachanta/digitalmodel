# Data Templates

This directory contains standardized templates for adding new data to the repository.

## Available Templates

### 1. vessel_template.csv
Template for vessel database entries.

**Usage:**
1. Copy template to appropriate domain folder (e.g., `vessels/raw/`)
2. Fill in vessel specifications
3. Rename to descriptive filename: `vessel_type_year.csv`

### 2. equipment_template.csv
Template for equipment specification entries.

**Usage:**
1. Copy template to equipment subdomain (e.g., `equipment/raw/anchors/`)
2. Fill in equipment specifications
3. Rename to descriptive filename: `equipment_type_manufacturer.csv`

### 3. metadata_template.json
Template for dataset metadata documentation.

**Usage:**
1. Copy template to same directory as dataset
2. Fill in metadata fields
3. Rename to match dataset: `dataset_name_metadata.json`

## Web Data Enhancement Workflow

When adding data from web sources:

1. **Download/Scrape Data**
   - Prefer CSV or ASCII format for direct analysis use
   - Save screenshots or PDFs as references

2. **Place in Domain Structure**
   ```
   data/{domain}/raw/{category}/filename.csv
   ```

3. **Create Metadata File**
   - Use `metadata_template.json`
   - Document source URL and access date
   - Include data license information

4. **Document in Domain README**
   - Add entry to domain's README.md
   - Include data source and date
   - Note any processing required

5. **Process Data**
   - Create processing script in `/src/{domain}/`
   - Apply standardization and validation
   - Save results to `processed/` directory

6. **Commit Changes**
   ```bash
   git add data/{domain}/raw/{category}/*
   git commit -m "data: Add {description} from {source} ({date})"
   ```

## Data Quality Standards

All data added to the repository should meet these standards:

- ✅ **CSV Format:** Tabular data in CSV with clear headers
- ✅ **ASCII Encoding:** Text files in ASCII or UTF-8
- ✅ **Units Specified:** Include units in column headers or metadata
- ✅ **Source Documented:** Record data source and access date
- ✅ **Quality Checked:** Validate data before committing

## Naming Conventions

Follow these patterns:

- **Vessels:** `vessel_type_year.csv` (e.g., `fpso_database_2024.csv`)
- **Equipment:** `equipment_type_manufacturer.csv` (e.g., `fenders_yokohama_2024.csv`)
- **Coefficients:** `coeff_type_parameter.csv` (e.g., `ocimf_coefficients_tankers.csv`)
- **Analysis:** `analysis_type_date.csv` (e.g., `hydrodynamic_added_mass_2024.csv`)

Avoid:
- Date prefixes (20241024_data.csv)
- Generic names (data.csv, output.csv)
- Special characters (data(final).csv)
