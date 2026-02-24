#!/usr/bin/env python3
"""
Test script to verify the visualization page loads correctly
"""

import webbrowser
import time

def main():
    # URL for the visualization page
    viz_url = "file:///D:/github/digitalmodel/specs/modules/orcaflex/browser-interface/implementation/api/plotly_visualization.html"
    
    print("Opening OrcaFlex Visualization Dashboard...")
    print(f"URL: {viz_url}")
    
    # Open in default browser
    webbrowser.open(viz_url)
    
    print("\nVisualization page opened in browser.")
    print("\nKey features to test:")
    print("1. Click 'Select Folder' to browse available data folders")
    print("2. Metadata selectors update when critical case loads")
    print("3. Environment selection updates direction options")
    print("4. Time Period Spectrum shows instead of Frequency Spectrum")
    print("5. Critical case has pulsing badge indicator")
    print("\nAPI Server should be running on http://localhost:8001")

if __name__ == "__main__":
    main()