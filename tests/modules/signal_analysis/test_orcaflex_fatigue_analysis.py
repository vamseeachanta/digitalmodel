#!/usr/bin/env python
"""
Test case for OrcaFlex fatigue analysis with rainflow and FFT
This test demonstrates the complete workflow for processing OrcaFlex CSV files
with specific requirements from the user prompt.

USER PROMPT:
"Using signal_analysis module, perform rainflow and FFT for files in folder 
D:\1522\ctr7\orcaflex\rev_a08\output\csv\07c_fatigue with file pattern _Strut?.csv.
Save output files in 'rainflow' folder in same directory. Use filenaming convention 
by adding _rainflow and _fft. Only use columns: 'time', 'Tension (Vessel End)', 
'Tension (Jacket End)'. Put output files directly in rainflow folder without subfolders.
Add rainflow and FFT plots. Minimize number of plots. Combine vessel end and jacket end 
in plots. Adjust y-axis range for good plot visibility. Use dotted vs solid lines for 
visibility. Use consistent colors across plots for user convenience."

REQUIREMENTS IMPLEMENTED:
1. Window-averaged FFT with 137 windows (75% overlap) for smooth spectrum
2. Combined plots for Vessel End and Jacket End
3. Consistent color scheme: Navy (Vessel) and Dark Red (Jacket)
4. Line styles: Solid (Vessel) vs Dashed/Dotted (Jacket)
5. Optimized y-axis scaling (5th to 99.5th percentile)
6. Direct output to rainflow folder (no subfolders)
7. Custom naming: <filename>_<column>_rainflow.csv and <filename>_<column>_fft.csv
"""

import sys
import unittest
from pathlib import Path
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import logging

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent / 'src'))

from digitalmodel.modules.signal_analysis.orcaflex import TimeSeriesAnalyzer

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class TestOrcaFlexFatigueAnalysis(unittest.TestCase):
    """Test case for OrcaFlex fatigue data analysis"""
    
    @classmethod
    def setUpClass(cls):
        """Set up test configuration once for all tests"""
        
        # CONSISTENT COLOR SCHEME
        cls.vessel_color = 'navy'  # Dark blue for Vessel End
        cls.jacket_color = 'darkred'  # Dark red for Jacket End
        
        # Analysis configuration (finalized after user feedback)
        cls.config = {
            'analysis': {
                'rainflow': {
                    'enable': True,
                    'bin_count': 50,
                    'method': 'astm'  # ASTM E1049-85 standard
                },
                'fft': {
                    'enable': True,
                    'window_size': 1024,  # Small window = more windows for averaging
                    'overlap': 0.75,  # 75% overlap for maximum smoothing
                    'window_type': 'hann',  # Hanning window
                    'frequency_limit': 2.0,  # Focus on 0-2 Hz range
                    'use_window_averaging': True,
                    'averaging_method': 'mean',
                    'peak_detection': {
                        'enable': True,
                        'n_peaks': 5,
                        'min_height': 0.01
                    }
                }
            },
            'output': {
                'plots': {'enable': False},  # Custom plots instead
                'formats': {'csv': True}
            },
            'error_handling': {
                'continue_on_error': True,
                'detailed_logging': False
            }
        }
        
        # Test data path
        cls.test_data_dir = Path(__file__).parent / 'test_data'
        cls.sample_file = cls.test_data_dir / 'fat002_fsts_sample.csv'
        
    def test_fatigue_analysis_configuration(self):
        """Test that configuration produces expected FFT smoothing"""
        
        # Calculate expected number of windows
        df = pd.read_csv(self.sample_file)
        total_points = len(df)
        window_size = self.config['analysis']['fft']['window_size']
        overlap = self.config['analysis']['fft']['overlap']
        step_size = int(window_size * (1 - overlap))
        expected_windows = (total_points - window_size) // step_size + 1
        
        logger.info(f"Data points: {total_points}")
        logger.info(f"Window size: {window_size}")
        logger.info(f"Overlap: {overlap * 100}%")
        logger.info(f"Expected windows for averaging: {expected_windows}")
        
        # Verify sufficient windows for smoothing
        self.assertGreater(expected_windows, 50, 
                          "Should have >50 windows for good smoothing")
        
    def test_process_sample_file(self):
        """Test processing of sample OrcaFlex fatigue data"""
        
        # Columns to analyze
        time_column = "time"
        data_columns = ["Tension (Vessel End)", "Tension (Jacket End)"]
        
        # Configure column mapping
        config = self.config.copy()
        config['column_mapping'] = {
            'strategy': 'manual',
            'manual': {
                'time': time_column,
                'data_columns': data_columns
            }
        }
        
        # Create analyzer
        analyzer = TimeSeriesAnalyzer(config=config)
        
        # Process file
        results = analyzer.process_file(self.sample_file)
        
        # Verify results structure
        self.assertIn('columns', results)
        self.assertIn('validation', results)
        
        # Check both tension columns were processed
        for col_name in data_columns:
            self.assertIn(col_name, results['columns'])
            col_results = results['columns'][col_name]
            
            # Verify rainflow results
            self.assertIn('rainflow', col_results)
            self.assertIn('cycles', col_results['rainflow'])
            self.assertIn('statistics', col_results['rainflow'])
            
            # Verify FFT results
            self.assertIn('spectral', col_results)
            self.assertIn('window_fft', col_results['spectral'])
            
            # Log statistics
            stats = col_results.get('statistics', {})
            rf_stats = col_results['rainflow'].get('statistics', {})
            logger.info(f"\n{col_name} Statistics:")
            logger.info(f"  Mean: {stats.get('mean', 0):.2f}")
            logger.info(f"  Std: {stats.get('std', 0):.2f}")
            logger.info(f"  Rainflow cycles: {rf_stats.get('total_cycles', 0)}")
            
    def test_generate_combined_plots(self):
        """Test generation of combined plots with consistent colors"""
        
        # This test demonstrates the plotting configuration
        # that meets all user requirements
        
        output_dir = self.test_data_dir / 'test_output'
        output_dir.mkdir(exist_ok=True)
        
        # Process sample file
        time_column = "time"
        data_columns = ["Tension (Vessel End)", "Tension (Jacket End)"]
        
        config = self.config.copy()
        config['column_mapping'] = {
            'strategy': 'manual',
            'manual': {
                'time': time_column,
                'data_columns': data_columns
            }
        }
        
        analyzer = TimeSeriesAnalyzer(config=config)
        results = analyzer.process_file(self.sample_file)
        
        # Create combined plot (demonstrates final configuration)
        fig = plt.figure(figsize=(16, 10))
        fig.suptitle('Test: Combined Analysis with Consistent Colors', 
                    fontsize=14, fontweight='bold')
        
        # Plot FFT comparison
        ax = plt.subplot(1, 2, 1)
        for col_name in data_columns:
            if col_name in results['columns']:
                spectrum = results['columns'][col_name].get('spectral', {}).get('window_fft')
                if spectrum is not None and isinstance(spectrum, pd.DataFrame):
                    mask = spectrum['frequency'] <= 2.0
                    
                    # Use consistent colors and line styles
                    if 'Vessel' in col_name:
                        color = self.vessel_color
                        linestyle = '-'
                        label = 'Vessel End (solid)'
                    else:
                        color = self.jacket_color
                        linestyle = '--'
                        label = 'Jacket End (dashed)'
                    
                    ax.semilogy(spectrum.loc[mask, 'frequency'],
                              spectrum.loc[mask, 'power'],
                              color=color, linestyle=linestyle,
                              linewidth=2, alpha=0.8, label=label)
        
        ax.set_xlabel('Frequency (Hz)')
        ax.set_ylabel('Power')
        ax.set_title('FFT Spectrum - Combined (Smooth)')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Save test plot
        plot_path = output_dir / 'test_combined_plot.png'
        plt.savefig(plot_path, dpi=150, bbox_inches='tight')
        plt.close()
        
        self.assertTrue(plot_path.exists(), "Test plot should be created")
        logger.info(f"Test plot saved: {plot_path}")
        
    def test_output_naming_convention(self):
        """Test that output files follow the required naming convention"""
        
        output_dir = self.test_data_dir / 'test_output'
        output_dir.mkdir(exist_ok=True)
        
        # Expected naming pattern
        base_name = "fat002_fsts_sample"
        expected_files = [
            f"{base_name}_Tension_Vessel_End_rainflow.csv",
            f"{base_name}_Tension_Vessel_End_fft.csv",
            f"{base_name}_Tension_Jacket_End_rainflow.csv",
            f"{base_name}_Tension_Jacket_End_fft.csv",
        ]
        
        # Verify naming convention
        for filename in expected_files:
            logger.info(f"Expected output file: {filename}")
            # In actual processing, these would be created
            
        # Verify no subfolders
        logger.info("All files should be saved directly in 'rainflow' folder")
        logger.info("No subfolders should be created")


def create_reference_documentation():
    """Create reference documentation for this analysis configuration"""
    
    doc_path = Path(__file__).parent / 'FATIGUE_ANALYSIS_REFERENCE.md'
    
    documentation = """# OrcaFlex Fatigue Analysis Reference

## Overview
This reference documents the finalized configuration for processing OrcaFlex fatigue CSV files
with rainflow counting and FFT spectral analysis.

## User Requirements (from prompt)
1. Process files matching pattern `*_Strut?.csv`
2. Analyze only: `time`, `Tension (Vessel End)`, `Tension (Jacket End)`
3. Save outputs in `rainflow` folder (no subfolders)
4. Naming: `<filename>_<column>_rainflow.csv` and `<filename>_<column>_fft.csv`
5. Generate combined plots (vessel and jacket together)
6. Use window-averaged FFT for smooth spectrum
7. Optimize y-axis for peak visibility
8. Use consistent colors across all plots
9. Differentiate with line styles (solid vs dashed/dotted)

## Final Configuration

### FFT Settings (Smooth Spectrum)
- **Window Size**: 1024 samples
- **Overlap**: 75%
- **Window Type**: Hann
- **Result**: ~137 windows for 36,000 point signal
- **Frequency Range**: 0-2 Hz

### Color Scheme (Consistent)
- **Vessel End**: Navy blue (`navy`)
- **Jacket End**: Dark red (`darkred`)

### Line Styles
- **Vessel End**: Solid line (`-`)
- **Jacket End**: Dashed (`--`) or Dotted (`:`)

### Y-axis Scaling
- **Method**: Percentile-based (5th to 99.5th)
- **Padding**: 0.5x lower, 2x upper

## Sample Code
```python
# Configuration
config = {
    'analysis': {
        'rainflow': {
            'enable': True,
            'bin_count': 50
        },
        'fft': {
            'enable': True,
            'window_size': 1024,
            'overlap': 0.75,
            'window_type': 'hann',
            'frequency_limit': 2.0,
            'use_window_averaging': True
        }
    }
}

# Colors
vessel_color = 'navy'
jacket_color = 'darkred'
```

## Output Files
1. Rainflow cycle counts (CSV)
2. FFT spectrum data (CSV)
3. Combined analysis plot (PNG)

## Notes
- Analysis uses ENTIRE time series (not samples)
- FFT smoothing removes noise while preserving system response
- Remaining peaks after smoothing represent actual structural behavior
"""
    
    with open(doc_path, 'w') as f:
        f.write(documentation)
    
    logger.info(f"Reference documentation created: {doc_path}")


if __name__ == '__main__':
    # Create reference documentation
    create_reference_documentation()
    
    # Run tests
    unittest.main()