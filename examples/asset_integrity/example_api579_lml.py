"""
API 579 Part 5: Local Metal Loss (LML) Assessment
==================================================

16" gas pipeline LTA (Local Thin Area) assessment.

Flaw defined by sIndex/cIndex ranges into a WT measurement grid.
Level 1: Rt screening + Lmsd spacing check
Level 2: RSF from Critical Thickness Profiles with Folias factor

MAWP reduction: MAWPr = MAWP * RSF / RSFa when RSF < RSFa

Usage: python example_api579_lml.py
"""
import math
import yaml

LML_CONFIG = """\
basename: API579
default:
  Analysis:
    GML: {data_source: xlsx, Circumference: False, Length: False}
    LML: True
    B31G: False
  Units: inch
  config: {overwrite: {output: True}}
  log_level: DEBUG
  settings: {StartWTFactor: 1, CorrosionAllowance: 0.0}

Outer_Pipe:
  Geometry: {Nominal_OD: 16.0, Nominal_ID: NULL, Design_WT: 0.625,
    tmin: 0.526, Corrosion_Allowance: 0.0}
  Material: {Material: Steel, Material_Grade: API 5L X65,
    WeldFactor: {Seamless: 1.0}, Insulation: NULL, Buoyancy: NULL}
  Code: [ASME B31.8-2016 Chapter VIII Pipeline]
  Manufacturing: {Coupling Mass Ratio: 0.0}
Inner_Pipe: NULL

Geometry: {NominalOD: 16, NominalID: NULL, DesignWT: 0.625, tmin: 0.526}

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

API579Parameters:
  RSFa: 0.9
  Age: 15
  FCARateFloor: 0.00118
  FoliasFactor:
    FlawParameter: [0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10.0]
    Mt:
      Cylindrical: [1.001,1.056,1.199,1.394,1.618,1.857,2.103,2.351,2.600,2.847,3.091,3.331,3.568,3.801,4.032,4.262,4.492,4.727,4.970,5.225,5.497]
      Conical: []
      Spherical: []

# Local Thin Area definition: indices into the Excel WT grid
LML:
  LTA:
    - io: path/to/wt_grid.xlsx
      sheet_name: Sheet1
      index_col: 0
      skiprows: 0
      skipfooter: 0
      DataCorrectionFactor: 1.00
      Label: Feature 3, Flaw Size 1
      sIndex: [8, 14]            # Axial row range
      cIndex: [47, 52]           # Circumferential column range
      s: NULL
      c: NULL
      Contour: {xlim: NULL, ylim: NULL, zlim: [0.538, 0.75]}
      FCA: [0.00, 0.02, 0.04, 0.06, 0.08]
      Lmsd: 15                   # Distance to nearest discontinuity (inch)
      MtType: Cylindrical
      FCAs: NULL
      FCAc: NULL
      FCANonFlawRatio: 0.25      # FCA outside flaw = 25% of flaw FCA

PlotSettings:
  Data: {PltSupTitle: Sample Pipeline LML, PltTitle: UT Wall Thickness,
    PltXLabel: Circumference (inch), PltYLabel: Length (inch)}
  LML: {PltSupTitle: Sample Pipeline LML, PltTitle: LML Levels 1 and 2,
    PltXLabel: FCA (inch), PltYLabel: MAWP (psi), ylim: [8000, 20000]}
"""


def demonstrate_lml_math():
    """Show LML screening and Folias factor MAWP reduction."""
    cfg = yaml.safe_load(LML_CONFIG)
    pipe = cfg["Outer_Pipe"]["Geometry"]
    lta = cfg["LML"]["LTA"][0]
    api = cfg["API579Parameters"]

    # Hypothetical measured values for illustration
    tmm, trd, fca = 0.538, 0.610, 0.0
    tc = trd - fca * lta["FCANonFlawRatio"]
    nominal_id = pipe["Nominal_OD"] - 2 * pipe["Design_WT"]
    s = lta["sIndex"][1] - lta["sIndex"][0]
    rt = (tmm - fca) / tc

    print(f"Pipe: {pipe['Nominal_OD']}\" OD, {pipe['Design_WT']}\" WT")
    print(f"Flaw: {lta['Label']}, {s}x{lta['cIndex'][1]-lta['cIndex'][0]} cells")
    print(f"tmm={tmm}\", trd={trd}\", tc={tc}\", Rt={rt:.3f}\n")

    # Level 1 screening
    ok_rt = rt >= 0.2
    ok_tmm = (tmm - fca) >= 0.05
    lmsd_lim = 1.8 * math.sqrt(nominal_id * tc)
    ok_lmsd = lta["Lmsd"] >= lmsd_lim
    print("Level 1 screening:")
    print(f"  Rt>=0.2: {ok_rt}  |  tmm-FCA>=0.05: {ok_tmm}  |  "
          f"Lmsd>={lmsd_lim:.1f}: {ok_lmsd}")

    # Folias factor and RSF
    import numpy as np
    lam = 1.285 * s / math.sqrt(nominal_id * tc)
    mt = float(np.interp(lam, api["FoliasFactor"]["FlawParameter"],
                          api["FoliasFactor"]["Mt"]["Cylindrical"]))
    rsf = rt / (1 - (1 - rt) / mt)
    print(f"\nLevel 1 MAWP check: lambda={lam:.3f}, Mt={mt:.3f}, RSF={rsf:.3f}")
    print(f"  RSF {'>=':} RSFa={api['RSFa']}  ->  "
          f"{'No reduction' if rsf >= api['RSFa'] else 'MAWPr = MAWP*RSF/RSFa'}")

    print("\nEngine outputs per FCA step:")
    for k in ["FCA", "tc", "s", "c", "tmm", "MAWP", "Rt",
              "Flaw Parameter", "Mt, L1", "RSF, L1", "MAWPr, L1",
              "MAWPr, L2", "RSF, L2", "tavg"]:
        print(f"  '{k}': ...")


if __name__ == "__main__":
    print("=" * 60)
    print("API 579 Part 5 - Local Metal Loss Assessment")
    print("=" * 60 + "\n")
    demonstrate_lml_math()
    print("\nTo run: engine(inputfile='lml_config.yml')")
