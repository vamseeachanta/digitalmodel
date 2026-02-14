"""
Pipe Capacity: ASME B31.4 and B31.8 (Modified Barlow Equation)
==============================================================

Demonstrates the YAML config for pipe burst pressure evaluation.
Supported codes: ASME B31.4/B31.8, API STD 2RD, API RP 1111,
API RP 16Q, 30 CFR Part 250.

Usage: python example_pipe_capacity.py
"""
import yaml

# Two configs: 12" oil (B31.4) and 16" gas (B31.8).
# Only key sections shown; the engine merges with package defaults.
CONFIGS = {
    "B31.4 Oil 12-inch": """\
basename: API579
default: {Analysis: {GML: {Circumference: False}, LML: False, B31G: False},
  Units: inch, config: {overwrite: {output: True}}, log_level: DEBUG,
  settings: {StartWTFactor: 1, CorrosionAllowance: 0.0}}
Outer_Pipe:
  Geometry: {Nominal_OD: 12.75, Nominal_ID: NULL, Design_WT: 0.350,
    tmin: 0.100, Corrosion_Allowance: 0.0}
  Material: {Material: Steel, Material_Grade: API 5L X42,
    WeldFactor: {Seamless: 1.0}, Insulation: NULL, Buoyancy: NULL}
  Code: [ASME B31.4-2016 Chapter IX Platform Piping]
  Manufacturing: {Coupling Mass Ratio: 0.0}
Inner_Pipe: NULL
Design:
  - Load Condition: {Outer_Pipe: internal_pressure}
    InternalPressure: {Outer_Pipe: 285}
    ExternalPressure: {Outer_Pipe: 0}
    InternalFluid: {Outer_Pipe: 0.037}
    ExternalFluid: {Outer_Pipe: 0.037}
    Temperature: {Ambient: {Outer_Pipe: 50}, Operating: {Outer_Pipe: 82}, Maximum: NULL}
    BendingMoment: 0
    AxialForce: 0
    Torsion: 0
    Condition: Restrained
    Water_Depth: 0
    Code: [{Outer_Pipe: ASME B31.4-2016 Chapter IX Platform Piping}]
Material:
  Steel: {E: 30000000.0, Rho: 0.2817929, Poissionsratio: 0.30,
    ThermalExpansionCoefficient: 6.5E-6,
    Grades: {API 5L X42: {SMYS: 42000, SMUS: 60000, Reference: NULL}}}
""",
    "B31.8 Gas 16-inch": """\
basename: API579
default: {Analysis: {GML: {Circumference: False}, LML: False, B31G: False},
  Units: inch, config: {overwrite: {output: True}}, log_level: DEBUG,
  settings: {StartWTFactor: 1, CorrosionAllowance: 0.0}}
Outer_Pipe:
  Geometry: {Nominal_OD: 16.0, Nominal_ID: NULL, Design_WT: 0.625,
    tmin: 0.526, Corrosion_Allowance: 0.0}
  Material: {Material: Steel, Material_Grade: API 5L X65,
    WeldFactor: {Seamless: 1.0}, Insulation: NULL, Buoyancy: NULL}
  Code: [ASME B31.8-2016 Chapter VIII Pipeline]
  Manufacturing: {Coupling Mass Ratio: 0.0}
Inner_Pipe: NULL
Design:
  - Load Condition: {Outer_Pipe: internal_pressure}
    InternalPressure: {Outer_Pipe: 2220}
    ExternalPressure: {Outer_Pipe: 0}
    InternalFluid: {Outer_Pipe: 0.037}
    ExternalFluid: {Outer_Pipe: 0.037}
    Temperature: {Ambient: {Outer_Pipe: 50}, Operating: {Outer_Pipe: 82}, Maximum: NULL}
    BendingMoment: 0
    AxialForce: 0
    Torsion: 0
    Condition: Restrained
    Water_Depth: 2460.63
    Code: [{Outer_Pipe: ASME B31.8-2016 Chapter VIII Pipeline}]
Material:
  Steel: {E: 30000000.0, Rho: 0.2817929, Poissionsratio: 0.30,
    ThermalExpansionCoefficient: 6.5E-6,
    Grades: {API 5L X65: {SMYS: 65300, SMUS: 77500, Reference: NULL}}}
""",
}


def check_barlow(label, cfg_text):
    """Parse config and verify with manual Barlow equation."""
    cfg = yaml.safe_load(cfg_text)
    pipe = cfg["Outer_Pipe"]["Geometry"]
    design = cfg["Design"][0]
    code = design["Code"][0]["Outer_Pipe"]
    grade = cfg["Outer_Pipe"]["Material"]["Material_Grade"]
    smys = list(cfg["Material"].values())[0]["Grades"][grade]["SMYS"]
    od, wt = pipe["Nominal_OD"], pipe["Design_WT"]
    d_over_t = od / wt
    f_design = 0.60 if "B31.4" in code else 0.72
    hoop = smys * f_design * 1.0 * 1.0  # weld_factor * temp_derating

    if d_over_t >= 30:
        mawp = 2 * wt * hoop / od
    else:
        mawp = 2 * wt * hoop / (od - wt)

    p_design = design["InternalPressure"]["Outer_Pipe"]
    t_min = p_design * od / (2 * hoop) if d_over_t >= 30 else \
        p_design * od / (2 * hoop + p_design)

    print(f"--- {label} ---")
    print(f"  Code: {code}  |  Grade: {grade}  |  SMYS: {smys:,} psi")
    print(f"  OD: {od}\"  |  WT: {wt}\"  |  D/t: {d_over_t:.1f}  |  F: {f_design}")
    print(f"  Design P: {p_design} psi  |  Barlow MAWP: {mawp:.0f} psi")
    print(f"  Barlow min WT: {t_min:.4f}\"")
    print()


if __name__ == "__main__":
    print("=" * 60)
    print("Pipe Capacity Examples (Modified Barlow Equation)")
    print("=" * 60 + "\n")
    for label, cfg_text in CONFIGS.items():
        check_barlow(label, cfg_text)

    print("Engine result path:")
    print("  cfg['Result']['Outer_Pipe']['internal_pressure']")
    print("      [<code>]['Design_WT_Max_Pressure']['Zero Corrosion Allowance']")
    print("\nTo run: engine(inputfile='pipe_capacity.yml')")
