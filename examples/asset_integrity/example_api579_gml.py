"""
API 579 Part 4: General Metal Loss (GML) Assessment
====================================================

16" gas pipeline GML assessment with simulated wall-thickness grids.

Level 1: min measured WT vs code minimum (tmin)
Level 2: Remaining Strength Factor (RSF) via thickness profiles

Acceptance: tmm - FCA >= max(0.5*tmin, 0.2*t_nom, 0.1 inch)
            RSF >= RSFa (0.9); if not, MAWPr = MAWP * RSF / RSFa

Usage: python example_api579_gml.py
"""
import math
import yaml

# Uses 'simulated' data_source so no Excel files are needed.
GML_CONFIG = """\
basename: API579
default:
  Analysis:
    GML: {data_source: simulated, Circumference: True, Length: False,
      FCARate: Historical, FCA: [0.00, 0.02, 0.04, 0.06, 0.08, 0.10]}
    LML: False
    B31G: False
  Units: inch
  config: {overwrite: {output: True}}
  log_level: DEBUG
  settings: {StartWTFactor: 1, AssessmentLengthCeilingFactor_Circumference: NULL,
    AssessmentLengthCeilingFactor_Length: 1.0, CorrosionAllowance: 0.0}

gml_simulated_grid:
  - Label: Simulated Corrosion Feature 1
    grid_length: 24
    circumferencial_length: 8
    axial_length: 6
    wt_to_nominal_outside_loss_area: 0.98
    wt_to_nominal_loss_area: 0.84
    Contour: {xlim: NULL, ylim: NULL, zlim: [0.526, 0.625]}

Outer_Pipe:
  Geometry: {Nominal_OD: 16.0, Nominal_ID: NULL, Design_WT: 0.625,
    tmin: 0.526, Corrosion_Allowance: 0.0}
  Material: {Material: Steel, Material_Grade: API 5L X65,
    WeldFactor: {Seamless: 1.0}, Insulation: NULL, Buoyancy: NULL}
  Code: [ASME B31.8-2016 Chapter VIII Pipeline]
  Manufacturing: {Coupling Mass Ratio: 0.0}
Inner_Pipe: NULL

Geometry: {NominalOD: 16, NominalID: NULL, DesignWT: 0.625, tmin: 0.526,
  StartWTFactor: 1, AssessmentLengthCeilingFactor_Circumference: NULL,
  AssessmentLengthCeilingFactor_Length: 0.5, CorrosionAllowance: 0.0}

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

PlotSettings:
  Data: {PltSupTitle: Sample Pipeline GML, PltTitle: UT Wall Thickness,
    PltXLabel: Circumference (inch), PltYLabel: Length (inch)}
  GML: {PltSupTitle: Sample Pipeline GML, PltTitle: General Metal Loss Level 2,
    PltXLabel: FCA (inch), PltYLabel: MAWP (psi), ylim: [8000, 20000]}
"""


def demonstrate_gml_math():
    """Show GML Level 1 screening and Level 2 assessment length math."""
    cfg = yaml.safe_load(GML_CONFIG)
    pipe = cfg["Outer_Pipe"]["Geometry"]
    t_nom, t_min = pipe["Design_WT"], pipe["tmin"]
    rsfa = cfg["API579Parameters"]["RSFa"]
    tmm = t_nom * 0.84  # Simulated min WT (84% of nominal)

    print(f"Pipe: {pipe['Nominal_OD']}\" OD, {t_nom}\" WT, tmin={t_min}\"")
    print(f"Design P: {cfg['Design'][0]['InternalPressure']['Outer_Pipe']} psi\n")

    # Level 1 screening
    lhs, rhs = tmm, max(0.5 * t_min, max(0.2 * t_nom, 0.1))
    print(f"Level 1: tmm={tmm:.4f} >= max(0.5*tmin, 0.2*tnom, 0.1)={rhs:.4f}"
          f" -> {'PASS' if lhs >= rhs else 'FAIL'}")

    # Level 2 assessment length
    fca_ml = tmm - t_min
    tc = t_min - fca_ml
    nominal_id = pipe["Nominal_OD"] - 2 * t_nom
    id_ml = nominal_id + 2 * fca_ml
    rt = (tmm - fca_ml) / tc
    lp = 1.123 * math.sqrt(((1 - rt) / (1 - rt / rsfa)) ** 2 - 1) \
        if rt < rsfa else 50.0
    avg_len = lp * math.sqrt(id_ml * tc)
    print(f"Level 2: Rt={rt:.4f}, lambda={lp:.2f}, L_assess={avg_len:.1f}\"\n")

    print("Engine outputs: cfg['Result']['Circumference'][i] = {")
    for k in ["Description", "Min WT (inch)", "Avg. WT (inch)",
              "Max WT (inch)", "Corr. Rate (inch/year)",
              "Rem. Life (yrs)", "Len (inch)"]:
        print(f"  '{k}': ...,")
    print("}")


if __name__ == "__main__":
    print("=" * 60)
    print("API 579 Part 4 - General Metal Loss Assessment")
    print("=" * 60 + "\n")
    demonstrate_gml_math()
    print("\nTo run: engine(inputfile='gml_config.yml')")
