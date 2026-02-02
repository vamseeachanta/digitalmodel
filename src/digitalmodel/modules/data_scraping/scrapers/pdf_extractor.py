"""
PDF Extractor

Extracts tables and specifications from equipment PDF catalogs.
Uses pdfplumber for table extraction and text analysis.
"""

import pandas as pd
import pdfplumber
import re
from typing import List, Dict, Any, Optional
from pathlib import Path
import logging


class PDFExtractor:
    """Extract structured data from equipment PDF catalogs."""

    def __init__(self, output_dir: Optional[Path] = None):
        """
        Initialize PDF extractor.

        Args:
            output_dir: Directory to save extracted data
        """
        self.output_dir = output_dir or Path("data/equipment/processed")
        self.output_dir.mkdir(parents=True, exist_ok=True)

        # Setup logging
        self.logger = logging.getLogger(self.__class__.__name__)
        if not self.logger.handlers:
            handler = logging.StreamHandler()
            formatter = logging.Formatter(
                '%(asctime)s - %(name)s - %(levelname)s - %(message)s',
                datefmt='%Y-%m-%d %H:%M:%S'
            )
            handler.setFormatter(formatter)
            self.logger.addHandler(handler)
            self.logger.setLevel(logging.INFO)

    def extract_tables_from_pdf(self, pdf_path: Path) -> List[pd.DataFrame]:
        """
        Extract all tables from PDF file.

        Args:
            pdf_path: Path to PDF file

        Returns:
            List of DataFrames, one per table found
        """
        self.logger.info(f"Extracting tables from: {pdf_path.name}")
        tables = []

        try:
            with pdfplumber.open(pdf_path) as pdf:
                self.logger.info(f"PDF has {len(pdf.pages)} pages")

                for page_num, page in enumerate(pdf.pages, 1):
                    page_tables = page.extract_tables()

                    if page_tables:
                        self.logger.info(f"Page {page_num}: Found {len(page_tables)} tables")

                        for table_idx, table in enumerate(page_tables):
                            if not table or len(table) < 2:  # Skip empty or single-row tables
                                continue

                            # Convert to DataFrame
                            df = pd.DataFrame(table[1:], columns=table[0])

                            # Clean column names
                            df.columns = [str(col).strip() if col else f"Column_{i}"
                                         for i, col in enumerate(df.columns)]

                            # Remove completely empty rows/columns
                            df = df.dropna(how='all')
                            df = df.dropna(axis=1, how='all')

                            if not df.empty:
                                # Add metadata
                                df['pdf_source'] = pdf_path.name
                                df['pdf_page'] = page_num
                                df['table_index'] = table_idx

                                tables.append(df)
                                self.logger.info(f"  Table {table_idx + 1}: {len(df)} rows × {len(df.columns)} columns")

        except Exception as e:
            self.logger.error(f"Error extracting tables from {pdf_path.name}: {e}")

        if not tables:
            self.logger.warning(f"No tables found in {pdf_path.name}")

        return tables

    def extract_text_from_pdf(self, pdf_path: Path) -> str:
        """
        Extract all text from PDF file.

        Args:
            pdf_path: Path to PDF file

        Returns:
            Extracted text content
        """
        text = ""
        try:
            with pdfplumber.open(pdf_path) as pdf:
                for page in pdf.pages:
                    page_text = page.extract_text()
                    if page_text:
                        text += page_text + "\n"
        except Exception as e:
            self.logger.error(f"Error extracting text from {pdf_path.name}: {e}")

        return text

    def find_specification_tables(self, pdf_path: Path) -> List[pd.DataFrame]:
        """
        Find and extract specification tables from PDF.

        Looks for tables containing product specifications like dimensions,
        capacities, materials, etc.

        Args:
            pdf_path: Path to PDF file

        Returns:
            List of specification DataFrames
        """
        all_tables = self.extract_tables_from_pdf(pdf_path)

        if not all_tables:
            return []

        spec_tables = []

        # Keywords that indicate specification tables
        spec_keywords = [
            'specification', 'dimension', 'capacity', 'size', 'weight',
            'model', 'type', 'diameter', 'length', 'pressure', 'load'
        ]

        for df in all_tables:
            # Check if any column name contains specification keywords
            column_text = ' '.join([str(col).lower() for col in df.columns])

            if any(keyword in column_text for keyword in spec_keywords):
                spec_tables.append(df)
                continue

            # Check if table has multiple numeric columns (likely specs)
            numeric_cols = df.select_dtypes(include=['number']).columns
            if len(numeric_cols) >= 2:
                spec_tables.append(df)

        self.logger.info(f"Found {len(spec_tables)} specification tables out of {len(all_tables)} total tables")
        return spec_tables

    def parse_fender_specifications(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Parse and standardize fender specifications from DataFrame.

        Args:
            df: Raw DataFrame from PDF table

        Returns:
            Standardized DataFrame with fender specifications
        """
        df_clean = df.copy()

        # Common column mappings for fender specifications
        column_mappings = {
            'product_name': ['model', 'type', 'fender type', 'product', 'description', 'name'],
            'diameter_m': ['diameter', 'dia', 'd', 'dia. (m)', 'diameter (m)'],
            'length_m': ['length', 'len', 'l', 'length (m)', 'l (m)'],
            'width_m': ['width', 'w', 'b', 'width (m)', 'w (m)'],
            'height_m': ['height', 'h', 'height (m)', 'h (m)'],
            'weight_kg': ['weight', 'mass', 'weight (kg)', 'wt'],
            'deflection_percent': ['deflection', 'def', 'deflection (%)'],
            'reaction_force_kn': ['reaction', 'force', 'r', 'reaction (kn)'],
            'energy_absorption_kj': ['energy', 'e', 'energy (kj)', 'absorption'],
        }

        # Rename columns based on mappings
        for standard_col, variations in column_mappings.items():
            for col in df_clean.columns:
                col_lower = str(col).lower().strip()
                if col_lower in variations:
                    df_clean.rename(columns={col: standard_col}, inplace=True)
                    break

        # Clean numeric columns
        numeric_cols = ['diameter_m', 'length_m', 'width_m', 'height_m', 'weight_kg',
                       'deflection_percent', 'reaction_force_kn', 'energy_absorption_kj']

        for col in numeric_cols:
            if col in df_clean.columns:
                df_clean[col] = pd.to_numeric(df_clean[col], errors='coerce')

        return df_clean

    def extract_from_catalog(self, pdf_path: Path, equipment_type: str = "fender") -> Optional[pd.DataFrame]:
        """
        Extract equipment specifications from catalog PDF.

        Args:
            pdf_path: Path to catalog PDF
            equipment_type: Type of equipment (fender, anchor, buoy, etc.)

        Returns:
            Combined DataFrame with all extracted specifications
        """
        self.logger.info(f"Processing {equipment_type} catalog: {pdf_path.name}")

        # Extract specification tables
        spec_tables = self.find_specification_tables(pdf_path)

        if not spec_tables:
            self.logger.warning(f"No specification tables found in {pdf_path.name}")
            return None

        # Process based on equipment type
        processed_tables = []

        for df in spec_tables:
            if equipment_type == "fender":
                df_processed = self.parse_fender_specifications(df)
            else:
                df_processed = df  # Generic processing

            processed_tables.append(df_processed)

        # Combine all tables
        if len(processed_tables) == 1:
            combined_df = processed_tables[0]
        else:
            # Ensure unique column names before concatenation
            for i, df in enumerate(processed_tables):
                # Make column names unique by appending table index if duplicates exist
                cols = df.columns.tolist()
                if len(cols) != len(set(cols)):
                    # Has duplicates - make unique
                    new_cols = []
                    col_counts = {}
                    for col in cols:
                        if col in col_counts:
                            col_counts[col] += 1
                            new_cols.append(f"{col}_{col_counts[col]}")
                        else:
                            col_counts[col] = 0
                            new_cols.append(col)
                    df.columns = new_cols

            # Combine tables with similar columns
            combined_df = pd.concat(processed_tables, ignore_index=True, sort=False)

        # Add source metadata
        combined_df['catalog_source'] = pdf_path.name

        self.logger.info(f"Extracted {len(combined_df)} total rows from {len(spec_tables)} tables")

        return combined_df

    def process_directory(self, pdf_dir: Path, equipment_type: str = "fender",
                         pattern: str = "*.pdf") -> pd.DataFrame:
        """
        Process all PDF files in a directory.

        Args:
            pdf_dir: Directory containing PDF files
            equipment_type: Type of equipment
            pattern: File pattern to match

        Returns:
            Combined DataFrame with all extracted data
        """
        self.logger.info(f"Processing {equipment_type} PDFs in: {pdf_dir}")

        pdf_files = list(pdf_dir.glob(pattern))
        self.logger.info(f"Found {len(pdf_files)} PDF files")

        all_data = []

        for pdf_file in pdf_files:
            try:
                df = self.extract_from_catalog(pdf_file, equipment_type)
                if df is not None and not df.empty:
                    all_data.append(df)
            except Exception as e:
                self.logger.error(f"Error processing {pdf_file.name}: {e}")
                continue

        if not all_data:
            self.logger.warning("No data extracted from any PDFs")
            return pd.DataFrame()

        # Combine all data - use sort=False to avoid issues with mixed columns
        combined_df = pd.concat(all_data, ignore_index=True, sort=False)

        self.logger.info(f"Total extracted: {len(combined_df)} rows from {len(all_data)} PDFs")

        # Save to CSV
        output_file = self.output_dir / f"{equipment_type}s_extracted_{pd.Timestamp.now().strftime('%Y%m%d')}.csv"
        combined_df.to_csv(output_file, index=False)
        self.logger.info(f"Saved to: {output_file}")

        return combined_df

    def analyze_pdf_structure(self, pdf_path: Path) -> Dict[str, Any]:
        """
        Analyze PDF structure to understand content organization.

        Args:
            pdf_path: Path to PDF file

        Returns:
            Dictionary with structure analysis
        """
        analysis = {
            'filename': pdf_path.name,
            'pages': 0,
            'tables_found': 0,
            'tables_per_page': [],
            'has_headers': False,
            'sample_content': []
        }

        try:
            with pdfplumber.open(pdf_path) as pdf:
                analysis['pages'] = len(pdf.pages)

                for page_num, page in enumerate(pdf.pages, 1):
                    tables = page.extract_tables()
                    if tables:
                        analysis['tables_found'] += len(tables)
                        analysis['tables_per_page'].append((page_num, len(tables)))

                        # Sample first table
                        if page_num == 1 and tables[0] and len(analysis['sample_content']) == 0:
                            analysis['sample_content'] = tables[0][:5]  # First 5 rows
                            analysis['has_headers'] = bool(tables[0][0])

        except Exception as e:
            analysis['error'] = str(e)

        return analysis
