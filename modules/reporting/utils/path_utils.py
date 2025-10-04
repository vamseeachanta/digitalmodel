"""
Path utilities for CSV data handling with relative paths.

This module provides utilities for resolving data file paths relative to
the project root, ensuring portable and consistent data access across all
repositories.
"""

from pathlib import Path
from typing import Union


def get_project_root() -> Path:
    """
    Get the project root directory.

    Assumes this file is located in modules/reporting/utils/
    and project root is 3 levels up.

    Returns:
        Path object pointing to project root
    """
    return Path(__file__).parent.parent.parent.parent


def get_data_path(filename: str, data_type: str = 'processed') -> Path:
    """
    Get path to data file relative to project root.

    Args:
        filename: Name of the CSV file
        data_type: Type of data directory ('raw', 'processed', or 'results')

    Returns:
        Path object to the data file

    Example:
        >>> path = get_data_path('measurements.csv', 'processed')
        >>> print(path)
        /path/to/project/data/processed/measurements.csv
    """
    valid_types = ['raw', 'processed', 'results']
    if data_type not in valid_types:
        raise ValueError(f"data_type must be one of {valid_types}")

    project_root = get_project_root()
    return project_root / 'data' / data_type / filename


def get_report_path(filename: str, subfolder: str = '') -> Path:
    """
    Get path to report file relative to project root.

    Args:
        filename: Name of the report HTML file
        subfolder: Optional subfolder within reports directory

    Returns:
        Path object to the report file

    Example:
        >>> path = get_report_path('analysis.html', 'monthly')
        >>> print(path)
        /path/to/project/reports/monthly/analysis.html
    """
    project_root = get_project_root()
    if subfolder:
        return project_root / 'reports' / subfolder / filename
    return project_root / 'reports' / filename


def ensure_report_dir(subfolder: str = '') -> Path:
    """
    Ensure report directory exists, create if needed.

    Args:
        subfolder: Optional subfolder within reports directory

    Returns:
        Path object to the report directory
    """
    report_dir = get_report_path('', subfolder).parent
    report_dir.mkdir(parents=True, exist_ok=True)
    return report_dir


def relative_path_from_report(data_file: Union[str, Path],
                               report_file: Union[str, Path]) -> str:
    """
    Calculate relative path from report to data file.

    Useful for embedding relative paths in HTML reports.

    Args:
        data_file: Path to data file
        report_file: Path to report file

    Returns:
        Relative path as string

    Example:
        >>> rel_path = relative_path_from_report(
        ...     'data/processed/data.csv',
        ...     'reports/analysis.html'
        ... )
        >>> print(rel_path)
        ../data/processed/data.csv
    """
    data_path = Path(data_file)
    report_path = Path(report_file)

    # Make absolute if not already
    if not data_path.is_absolute():
        data_path = get_project_root() / data_path
    if not report_path.is_absolute():
        report_path = get_project_root() / report_path

    # Calculate relative path
    try:
        rel_path = data_path.relative_to(report_path.parent)
        return str(rel_path)
    except ValueError:
        # Files are on different drives or paths don't share common base
        # Calculate using common parent
        common = Path(*os.path.commonprefix([data_path.parts, report_path.parts]))
        data_rel = data_path.relative_to(common)
        report_rel = report_path.parent.relative_to(common)

        # Go up from report to common, then down to data
        up_levels = len(report_rel.parts)
        rel_parts = ['..'] * up_levels + list(data_rel.parts)
        return '/'.join(rel_parts)


if __name__ == '__main__':
    # Example usage
    print("Project root:", get_project_root())
    print("Data path:", get_data_path('example.csv'))
    print("Report path:", get_report_path('report.html'))
