"""
Shared Utilities for Workflow Examples

Provides common functions for:
- Report generation (PDF/Excel)
- Plotting (publication quality)
- Unit conversions
- OrcaFlex export
- Standard compliance checks

Author: Marine Engineering Team
Date: 2025-10-03
"""

from typing import Dict, List, Tuple, Optional, Any
from pathlib import Path
from dataclasses import dataclass
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import json
import logging

logger = logging.getLogger(__name__)


# ============================================================================
# UNIT CONVERSIONS
# ============================================================================

class UnitConverter:
    """Unit conversion utilities for marine engineering."""

    # Length conversions
    @staticmethod
    def m_to_ft(meters: float) -> float:
        """Convert meters to feet."""
        return meters * 3.28084

    @staticmethod
    def ft_to_m(feet: float) -> float:
        """Convert feet to meters."""
        return feet / 3.28084

    # Force conversions
    @staticmethod
    def kn_to_lbf(kilonewtons: float) -> float:
        """Convert kilonewtons to pounds-force."""
        return kilonewtons * 224.809

    @staticmethod
    def lbf_to_kn(pounds: float) -> float:
        """Convert pounds-force to kilonewtons."""
        return pounds / 224.809

    # Weight conversions
    @staticmethod
    def kg_per_m_to_lb_per_ft(kg_m: float) -> float:
        """Convert kg/m to lb/ft."""
        return kg_m * 0.67197

    # Pressure conversions
    @staticmethod
    def mpa_to_psi(megapascals: float) -> float:
        """Convert MPa to PSI."""
        return megapascals * 145.038

    @staticmethod
    def psi_to_mpa(psi: float) -> float:
        """Convert PSI to MPa."""
        return psi / 145.038


# ============================================================================
# PLOTTING UTILITIES
# ============================================================================

class PlotConfig:
    """Standard plot configuration for publication quality."""

    # Figure settings
    DPI = 300
    FIGSIZE_SINGLE = (10, 8)
    FIGSIZE_DOUBLE = (16, 10)
    FIGSIZE_WIDE = (14, 6)

    # Colors (colorblind-friendly palette)
    COLOR_PRIMARY = '#0173B2'
    COLOR_SECONDARY = '#DE8F05'
    COLOR_SUCCESS = '#029E73'
    COLOR_WARNING = '#D55E00'
    COLOR_ERROR = '#CC78BC'

    # Fonts
    FONT_TITLE = 14
    FONT_LABEL = 12
    FONT_TICK = 10
    FONT_LEGEND = 10

    @staticmethod
    def setup_plot_style():
        """Configure matplotlib for publication-quality plots."""
        plt.rcParams.update({
            'font.size': PlotConfig.FONT_TICK,
            'axes.labelsize': PlotConfig.FONT_LABEL,
            'axes.titlesize': PlotConfig.FONT_TITLE,
            'xtick.labelsize': PlotConfig.FONT_TICK,
            'ytick.labelsize': PlotConfig.FONT_TICK,
            'legend.fontsize': PlotConfig.FONT_LEGEND,
            'figure.titlesize': PlotConfig.FONT_TITLE + 2,
            'lines.linewidth': 2,
            'axes.grid': True,
            'grid.alpha': 0.3,
            'axes.axisbelow': True,
            'figure.dpi': PlotConfig.DPI
        })


def save_plot(
    fig: plt.Figure,
    output_path: Path,
    dpi: int = PlotConfig.DPI,
    close: bool = True
) -> None:
    """
    Save plot with standard settings.

    Parameters
    ----------
    fig : matplotlib.Figure
        Figure to save
    output_path : Path
        Output file path
    dpi : int
        Resolution (dots per inch)
    close : bool
        Close figure after saving
    """
    output_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(output_path, dpi=dpi, bbox_inches='tight')
    logger.info(f"Plot saved: {output_path}")
    if close:
        plt.close(fig)


# ============================================================================
# EXCEL REPORT GENERATION
# ============================================================================

class ExcelReporter:
    """Generate formatted Excel reports."""

    @staticmethod
    def create_summary_report(
        data_dict: Dict[str, pd.DataFrame],
        output_path: Path,
        metadata: Optional[Dict] = None
    ) -> None:
        """
        Create multi-sheet Excel report.

        Parameters
        ----------
        data_dict : Dict[str, pd.DataFrame]
            Dictionary of sheet names to DataFrames
        output_path : Path
            Output Excel file path
        metadata : Dict, optional
            Report metadata (author, date, etc.)
        """
        output_path.parent.mkdir(parents=True, exist_ok=True)

        with pd.ExcelWriter(output_path, engine='xlsxwriter') as writer:
            workbook = writer.book

            # Define formats
            header_format = workbook.add_format({
                'bold': True,
                'bg_color': '#4472C4',
                'font_color': 'white',
                'border': 1
            })

            # Write each sheet
            for sheet_name, df in data_dict.items():
                df.to_excel(writer, sheet_name=sheet_name, index=False)

                # Format headers
                worksheet = writer.sheets[sheet_name]
                for col_num, value in enumerate(df.columns.values):
                    worksheet.write(0, col_num, value, header_format)
                    # Auto-adjust column width
                    max_len = max(df[value].astype(str).str.len().max(), len(value))
                    worksheet.set_column(col_num, col_num, min(max_len + 2, 50))

            # Add metadata sheet if provided
            if metadata:
                meta_df = pd.DataFrame(list(metadata.items()),
                                      columns=['Parameter', 'Value'])
                meta_df.to_excel(writer, sheet_name='Metadata', index=False)

        logger.info(f"Excel report saved: {output_path}")


# ============================================================================
# PDF REPORT GENERATION
# ============================================================================

class PDFReporter:
    """Generate PDF reports with plots and tables."""

    @staticmethod
    def create_report(
        figures: List[plt.Figure],
        output_path: Path,
        title: str = "Analysis Report"
    ) -> None:
        """
        Create PDF report from list of figures.

        Parameters
        ----------
        figures : List[plt.Figure]
            List of matplotlib figures
        output_path : Path
            Output PDF file path
        title : str
            Report title
        """
        output_path.parent.mkdir(parents=True, exist_ok=True)

        with PdfPages(output_path) as pdf:
            for fig in figures:
                pdf.savefig(fig, bbox_inches='tight')
                plt.close(fig)

            # Set PDF metadata
            d = pdf.infodict()
            d['Title'] = title
            d['Author'] = 'Marine Engineering Team'
            d['Subject'] = 'Workflow Analysis Report'
            d['Keywords'] = 'Marine Engineering, Mooring Analysis'

        logger.info(f"PDF report saved: {output_path}")


# ============================================================================
# ORCAFLEX EXPORT
# ============================================================================

class OrcaFlexExporter:
    """Export mooring system to OrcaFlex format."""

    @staticmethod
    def create_mooring_yml(
        mooring_config: Dict,
        output_path: Path
    ) -> None:
        """
        Create OrcaFlex .yml file for mooring system.

        Parameters
        ----------
        mooring_config : Dict
            Mooring configuration dictionary
        output_path : Path
            Output .yml file path
        """
        # This is a simplified example
        # Production version would use OrcFxAPI for proper model creation

        yml_template = {
            'General': {
                'Description': mooring_config.get('description', 'Mooring System'),
                'WaterDepth': mooring_config.get('water_depth', 1000.0)
            },
            'Lines': []
        }

        # Add mooring lines
        for i in range(mooring_config.get('num_lines', 8)):
            line = {
                'Name': f"Line_{i+1}",
                'LineType': mooring_config.get('line_type', 'Chain'),
                'Length': mooring_config.get('line_length', 2000.0),
                'TargetSegmentLength': 10.0
            }
            yml_template['Lines'].append(line)

        output_path.parent.mkdir(parents=True, exist_ok=True)
        with open(output_path, 'w') as f:
            json.dump(yml_template, f, indent=2)

        logger.info(f"OrcaFlex YML saved: {output_path}")


# ============================================================================
# STANDARD COMPLIANCE CHECKS
# ============================================================================

@dataclass
class ComplianceCheck:
    """Standard compliance check result."""
    standard: str
    parameter: str
    value: float
    limit: float
    unit: str
    status: bool
    margin: float


class StandardsChecker:
    """Check compliance with industry standards."""

    @staticmethod
    def check_api_rp_2sk_safety_factor(
        tension: float,
        mbl: float,
        condition: str = "intact"
    ) -> ComplianceCheck:
        """
        Check safety factor per API RP 2SK.

        Parameters
        ----------
        tension : float
            Maximum tension [kN]
        mbl : float
            Minimum breaking load [kN]
        condition : str
            "intact" or "damaged"

        Returns
        -------
        check : ComplianceCheck
            Compliance check result
        """
        required_sf = 1.67 if condition == "intact" else 1.25
        actual_sf = mbl / tension if tension > 0 else np.inf

        return ComplianceCheck(
            standard="API RP 2SK",
            parameter="Safety Factor",
            value=actual_sf,
            limit=required_sf,
            unit="-",
            status=actual_sf >= required_sf,
            margin=actual_sf - required_sf
        )

    @staticmethod
    def check_watch_circle(
        max_offset: float,
        water_depth: float,
        limit_pct: float = 0.10
    ) -> ComplianceCheck:
        """
        Check watch circle requirement.

        Parameters
        ----------
        max_offset : float
            Maximum vessel offset [m]
        water_depth : float
            Water depth [m]
        limit_pct : float
            Limit as percentage of water depth

        Returns
        -------
        check : ComplianceCheck
            Compliance check result
        """
        limit = limit_pct * water_depth
        offset_pct = max_offset / water_depth

        return ComplianceCheck(
            standard="API RP 2SK",
            parameter="Watch Circle",
            value=offset_pct * 100,
            limit=limit_pct * 100,
            unit="% of depth",
            status=max_offset <= limit,
            margin=(limit - max_offset) / water_depth * 100
        )


# ============================================================================
# DATA VALIDATION
# ============================================================================

class DataValidator:
    """Validate input data for analysis."""

    @staticmethod
    def validate_positive(value: float, name: str) -> None:
        """Validate that value is positive."""
        if value <= 0:
            raise ValueError(f"{name} must be positive, got {value}")

    @staticmethod
    def validate_range(
        value: float,
        name: str,
        min_val: float,
        max_val: float
    ) -> None:
        """Validate that value is within range."""
        if not min_val <= value <= max_val:
            raise ValueError(
                f"{name} must be between {min_val} and {max_val}, got {value}"
            )

    @staticmethod
    def validate_mooring_input(config: Dict) -> None:
        """Validate mooring configuration."""
        required_keys = ['water_depth', 'line_length', 'num_lines']
        for key in required_keys:
            if key not in config:
                raise ValueError(f"Missing required parameter: {key}")

        DataValidator.validate_positive(config['water_depth'], 'water_depth')
        DataValidator.validate_positive(config['line_length'], 'line_length')
        DataValidator.validate_range(
            config['num_lines'], 'num_lines', 3, 20
        )


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

def create_summary_table(results: Dict) -> pd.DataFrame:
    """Create summary table from results dictionary."""
    data = []
    for key, value in results.items():
        if isinstance(value, (int, float)):
            data.append({'Parameter': key, 'Value': f'{value:.2f}'})
        else:
            data.append({'Parameter': key, 'Value': str(value)})

    return pd.DataFrame(data)


def format_number(value: float, decimals: int = 2, unit: str = '') -> str:
    """Format number with unit."""
    return f"{value:.{decimals}f} {unit}".strip()


# ============================================================================
# CONFIGURATION
# ============================================================================

# Setup plot style on import
PlotConfig.setup_plot_style()


if __name__ == "__main__":
    # Example usage
    print("Workflow Utilities Module")
    print("=" * 60)

    # Unit conversions
    print(f"\n10 m = {UnitConverter.m_to_ft(10):.2f} ft")
    print(f"1000 kN = {UnitConverter.kn_to_lbf(1000):.0f} lbf")

    # Compliance check
    check = StandardsChecker.check_api_rp_2sk_safety_factor(
        tension=15000, mbl=25000, condition="intact"
    )
    print(f"\nSafety Factor Check: {check.status}")
    print(f"Value: {check.value:.2f}, Limit: {check.limit:.2f}")

    print("\nUtilities module ready for use.")
