"""
Basic Usage: API 579 Level 1 GML Assessment Config Structure
============================================================

Walks through the YAML config structure the asset_integrity engine
expects. Does not execute the engine (needs Excel data + matplotlib).

Entry point: engine(inputfile) in digitalmodel.asset_integrity.engine

Usage: python basic_usage.py
"""
import pprint
import textwrap


def build_example_config():
    """Build the config dict mirroring what the engine loads from YAML."""
    return {
        # 'API579' or 'fracture_mechanics' -- controls dispatch
        "basename": "API579",
        # Control flags: which sub-analyses to run
        "Default": {
            "Analysis": {
                "GML": {"Circumference": True, "Length": False},
                "LML": False,
            },
            "Units": "inch",
        },
        # Pipe geometry (NominalOD, DesignWT, tmin are critical)
        "Geometry": {
            "NominalOD": 16.0, "NominalID": None, "DesignWT": 0.625,
            "tmin": 0.526, "StartWTFactor": 1,
            "AssessmentLengthCeilingFactor_Circumference": None,
            "AssessmentLengthCeilingFactor_Length": 0.5,
            "CorrosionAllowance": 0.0,
        },
        # Load cases -- engine iterates this list
        "Design": [{
            "Load Condition": {"Outer_Pipe": "internal_pressure"},
            "InternalPressure": {"Outer_Pipe": 2220},
            "ExternalPressure": {"Outer_Pipe": 0},
            "InternalFluid": {"Outer_Pipe": 0.037},
            "ExternalFluid": {"Outer_Pipe": 0.037},
            "Temperature": {
                "Ambient": {"Outer_Pipe": 50},
                "Operating": {"Outer_Pipe": 82},
                "Maximum": None,
            },
            "BendingMoment": 0, "AxialForce": 0, "Torsion": 0,
            "Condition": "Restrained", "Water_Depth": 2460.63,
            "Code": [{"Outer_Pipe": "ASME B31.8-2016 Chapter VIII Pipeline"}],
        }],
        # Material library (SMYS drives burst pressure calc)
        "Material": {
            "E": 30_000_000.0, "rho": 7800, "Poissionsratio": 0.3,
            "SMYS": 65_000, "SMUS": None,
            "ThermalExpansionCoefficient": 6.5e-6,
            "WeldFactor": {"Seamless": 1.0},
        },
        # API 579 parameters: RSFa, Age, corrosion floor, Folias table
        "API579Parameters": {
            "RSFa": 0.9, "Age": 15, "FCARateFloor": 0.00118,
            "FoliasFactor": {
                "FlawParameter": [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0,
                    3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5,
                    8.0, 8.5, 9.0, 9.5, 10.0],
                "Mt": {"Cylindrical": [1.001, 1.056, 1.199, 1.394,
                    1.618, 1.857, 2.103, 2.351, 2.600, 2.847, 3.091,
                    3.331, 3.568, 3.801, 4.032, 4.262, 4.492, 4.727,
                    4.970, 5.225, 5.497],
                    "Conical": [], "Spherical": []},
            },
        },
        # WT grid references (Excel files with 2D thickness data)
        "ReadingSets": [{
            "io": "path/to/wt_grid.xlsx", "sheet_name": "Sheet1",
            "index_col": 0, "skiprows": 0, "skipfooter": 0,
            "DataCorrectionFactor": 1.0, "Label": "UT Scan, Feature 1",
            "Contour": {"xlim": None, "ylim": None, "zlim": [0.526, 0.625]},
            "FCARate": "Historical",
            "FCA": [0.00, 0.02, 0.04, 0.06, 0.08, 0.10],
        }],
        "DesignFactors": {"Pressure": 0.5, "Longitudinal": 0.9,
                          "EquivalentStress": 0.9, "TemperatureDerating": 1.0},
        "PlotSettings": {
            "Data": {"PltSupTitle": "Sample Pipeline FFS",
                     "PltTitle": "UT Wall Thickness",
                     "PltXLabel": "Circumference (inch)",
                     "PltYLabel": "Length (inch)"},
            "GML": {"PltSupTitle": "Sample Pipeline FFS",
                    "PltTitle": "General Metal Loss, Level 2",
                    "PltXLabel": "FCA (inch)", "PltYLabel": "MAWP (psi)",
                    "ylim": [8000, 20000]},
        },
    }


def explain_workflow():
    """Print engine workflow summary."""
    print(textwrap.dedent("""\
    Engine Workflow
    ===============
    1. engine(inputfile) loads YAML via yaml.safe_load
    2. Merges with package defaults (tests/test_data/API579.yml)
    3. ConfigureApplicationInputs sets up output folders
    4. basename='API579' dispatches to API579(cfg):
       a. GML Circumference=True -> api579_components.gml()
       b. LML=True               -> api579_components.lml()
       c. B31G=True              -> api579_components.b31g()
    5. Results saved as CSV tables, PNG plots, YAML summary
    """))


if __name__ == "__main__":
    print("=" * 60)
    print("API 579 FFS - Configuration Structure Reference")
    print("=" * 60 + "\n")

    cfg = build_example_config()
    pprint.pprint(cfg, width=72, depth=3)
    print()
    explain_workflow()

    print("To run: from digitalmodel.asset_integrity.engine import engine")
    print("        result = engine(inputfile='your_config.yml')")
