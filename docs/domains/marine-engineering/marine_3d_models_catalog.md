# Marine Engineering 3D Models & CAD Files Catalog

**ABOUTME:** Comprehensive catalog of marine engineering 3D models, CAD files, and objects available for offshore/marine engineering projects
**Created:** 2026-01-09

---

## Executive Summary

This catalog documents publicly available and commercial 3D models for marine and offshore engineering applications, organized by domain. Models are evaluated for compatibility with the digitalmodel project's focus on catenary/mooring analysis, hydrodynamic analysis, offshore platform design, and marine structural analysis.

---

## 1. Offshore Platforms

### 1.1 Fixed & Floating Production Systems

| Model Name | Description | Source/URL | Formats | License | Priority |
|------------|-------------|------------|---------|---------|----------|
| **FPSO Alvheim/Jotun/Balder Pack** | Production vessel models with 2048x textures | [Free3D](https://free3d.com/premium-3d-models/offshore) | 3DS, OBJ, FBX | Commercial/Free variants | **HIGH** |
| **Semi-Submersible Drilling Rig** | Detailed semi-submersible platform | [TurboSquid](https://www.turbosquid.com/3d-model/oil-platform) | MAX, FBX, OBJ | Commercial license | **HIGH** |
| **Jacket Platform Models** | Fixed steel jacket structures | [GrabCAD Offshore Tag](https://grabcad.com/library/tag/offshore) | STEP, IGES, STL | Various (check individual) | **HIGH** |
| **SPAR Platform** | Deep water SPAR design | [TurboSquid](https://www.turbosquid.com/3d-model/oil-platform) | MAX, OBJ, FBX | Commercial license | **MEDIUM** |
| **TLP (Tension Leg Platform)** | Tension leg platform | [TurboSquid](https://www.turbosquid.com/3d-model/oil-platform) | MAX, OBJ | Commercial license | **MEDIUM** |
| **Jotun B Offshore Platform** | Norwegian offshore platform model | [Free3D](https://free3d.com/premium-3d-models/offshore) | Multiple formats | Free/Commercial | **MEDIUM** |

**Relevance to digitalmodel:** Direct application for mooring analysis, wind/wave loading, hydrodynamic response analysis, and structural integrity assessment.

**Acquisition Notes:**
- GrabCAD requires free account registration
- TurboSquid models are commercial but high quality
- Free3D offers mix of free and premium models

---

## 2. Ships & Vessels

### 2.1 Benchmark Ship Hulls (High Priority for Hydrodynamics)

| Model Name | Description | Source/URL | Formats | License | Priority |
|------------|-------------|------------|---------|---------|----------|
| **KVLCC2 (KRISO VLCC)** | Very Large Crude Carrier benchmark hull | [GrabCAD](https://grabcad.com/library/kriso-kvlcc2-hull-1), [Marathon OS](https://marathon-os.com/library/krisokvlcc2-hull-downloadable-cad-681695b02d82a7a72cbe4130) | STEP, IGES, STL | Public domain (research) | **HIGHEST** |
| **KCS (KRISO Container Ship)** | Standard container ship hull | [GrabCAD](https://grabcad.com/library/kcs-hull-1), [Marathon OS](https://marathon-os.com/library/kcs-hull-highquality-cad-model-6816a5b22d82a7a72cbe4159) | STEP, IGES, STL | Public domain (research) | **HIGHEST** |
| **DTMB 5415** | Navy combatant hull form | [shipstab.org](http://www.shipstab.org/index.php/data-access/13-benchmarkingdata) | Various CAD formats | Research/Educational | **HIGH** |
| **DTC (Duisburg Test Case)** | Post-Panamax container ship | [shipstab.org](http://www.shipstab.org/index.php/data-access/13-benchmarkingdata) | STEP, IGES | Research/Educational | **HIGH** |
| **KVLCC1** | Original KRISO tanker hull | SIMMAN workshop archives | Various | Research (request access) | **MEDIUM** |

**Relevance to digitalmodel:** Critical for CFD validation, hydrodynamic coefficient validation, resistance/propulsion studies, and seakeeping analysis. These are industry-standard benchmark hulls with extensive experimental data available for validation.

**Acquisition Notes:**
- KVLCC2 and KCS widely available on GrabCAD and Marathon OS
- SIMMAN workshop data requires registration: [simman2014.dk](https://simman2014.dk/)
- shipstab.org offers benchmarking data portal with multiple vessel geometries

### 2.2 Vessel Types (Various Applications)

| Model Name | Description | Source/URL | Formats | License | Priority |
|------------|-------------|------------|---------|---------|----------|
| **Platform Supply Vessel (PSV)** | Offshore supply vessel | [TurboSquid](https://www.turbosquid.com/3d-model/platform-supply-vessel) | MAX, FBX, OBJ | Commercial | **MEDIUM** |
| **Tanker Models** | Various tanker designs | [GrabCAD Naval Tag](https://grabcad.com/library/tag/naval) | STEP, IGES, STL | Various | **MEDIUM** |
| **Drill Ship** | Offshore drilling vessel | [Free3D](https://free3d.com/premium-3d-models/offshore) | Multiple formats | Free/Commercial | **MEDIUM** |
| **FreeShip Hull Library** | Parametric ship hull designs | [FreeShip SourceForge](https://sourceforge.net/projects/freeship/) | FreeShip format, export to DXF/IGES | Open Source (GPL) | **HIGH** |
| **FLARE Cruise Ship** | Reference cruise vessel | [shipstab.org](http://www.shipstab.org/index.php/data-access/13-benchmarkingdata) | CAD formats | Research/Educational | **LOW** |

**Relevance to digitalmodel:** PSV and supply vessels relevant for dynamic positioning analysis, tankers for cargo operations, FreeShip for parametric hull generation.

---

## 3. Subsea Equipment

### 3.1 Wellhead & Production Equipment

| Model Name | Description | Source/URL | Formats | License | Priority |
|------------|-------------|------------|---------|---------|----------|
| **Subsea Christmas Tree** | Wellhead control equipment | [STLFinder](https://www.stlfinder.com/3dmodels/subsea-wellhead-christmas-tree/), [TurboSquid](https://www.turbosquid.com/3d-models/christmas-tree-shxt-subsea-3d-model/1114457) | STL, OBJ, MAX | STL: Free, MAX: Commercial | **MEDIUM** |
| **Subsea Manifold** | Production manifold models | [TurboSquid](https://www.turbosquid.com/Search/3D-Models/subsea) | MAX, FBX, OBJ | Commercial | **MEDIUM** |
| **Subsea Template** | Equipment foundation template | [TurboSquid](https://www.turbosquid.com/Search/3D-Models/subsea) | MAX, FBX | Commercial | **LOW** |
| **PLET (Pipeline End Termination)** | Pipeline connection equipment | [Free3D](https://free3d.com/premium-3d-models/wellhead) | Various | Free/Commercial | **LOW** |
| **Subsea BOP (Blowout Preventer)** | Well control equipment | [Free3D](https://free3d.com/premium-3d-models/wellhead) | Various | Free/Commercial | **LOW** |

**Relevance to digitalmodel:** Medium relevance - useful for subsea installation analysis, weight/buoyancy calculations, and structural support design.

**Acquisition Notes:**
- STLFinder offers 99,778+ subsea wellhead models for 3D printing (many free STL)
- TurboSquid models are high-detail commercial options
- Models typically have 70k+ polygons for detailed visualization

---

## 4. Mooring Systems

### 4.1 Anchors & Foundation Systems

| Model Name | Description | Source/URL | Formats | License | Priority |
|------------|-------------|------------|---------|---------|----------|
| **Drag Anchor Models** | Various drag anchor designs | [TurboSquid](https://www.turbosquid.com/3d-model/anchor), [Cults3D](https://cults3d.com/en/tags/anchor) | STL, OBJ, FBX | Free/Commercial mix | **HIGH** |
| **Suction Pile/Caisson** | Suction installed foundation | Technical literature + custom modeling | Custom CAD required | N/A | **HIGHEST** |
| **VLA (Vertically Loaded Anchor)** | High-capacity plate anchor | Technical specs from [Delmar Systems](https://delmarvryhof.com/service/geotechnical-and-structural/) | Custom modeling from specs | Commercial specs | **HIGH** |
| **Pile Anchor (Driven)** | Conventional driven pile | Generic CAD libraries | STEP, IGES | Public domain | **MEDIUM** |
| **SEPLA (Suction Embedded Plate)** | Acteon proprietary anchor | [Acteon SEPLA](https://acteon.com/solutions/project-lifecycle/offshore-construction/mooring-solutions/sepla) | Technical specs only | Commercial/confidential | **MEDIUM** |

**Relevance to digitalmodel:** **CRITICAL** - Direct application for mooring analysis module. Anchor geometry affects holding capacity calculations, soil interaction modeling, and installation analysis.

**Acquisition Notes:**
- Drag anchors: Multiple free STL models available on Cults3D and TurboSquid
- Suction piles: Parametric models can be created based on diameter/length specifications from technical papers
- Technical guidance available from:
  - [DNV/ABS Guidance Notes on Drag and Plate Anchors](https://ww2.eagle.org/content/dam/eagle/rules-and-guides/current/offshore/248_designinstallationdrag_and_plateanchors/Drag_and_Plate_Anchors_GN_e-Mar18.pdf)
  - [NREL Offshore Anchor Data Report](https://www.osti.gov/servlets/purl/1178273)

### 4.2 Mooring Lines & Hardware

| Model Name | Description | Source/URL | Formats | License | Priority |
|------------|-------------|------------|---------|---------|----------|
| **Chain Links** | Studded/studless chain | Generic CAD libraries, TurboSquid | STEP, OBJ | Various | **LOW** |
| **Mooring Buoys** | Surface/subsurface buoys | [GrabCAD](https://grabcad.com/library/tag/offshore) | STEP, IGES, STL | Various | **MEDIUM** |
| **Fairleads & Winches** | Deck mooring equipment | Generic marine equipment libraries | STEP, IGES | Various | **LOW** |

**Relevance to digitalmodel:** Low-medium priority - useful for visualization but geometry less critical for analysis compared to anchors and platforms.

---

## 5. Marine Structures

### 5.1 Offshore Wind Foundations

| Model Name | Description | Source/URL | Formats | License | Priority |
|------------|-------------|------------|---------|---------|----------|
| **Monopile Foundation** | Single pile wind turbine foundation | [GrabCAD](https://grabcad.com/library/tag/offshore), Technical specs | STEP, IGES, STL | Various | **MEDIUM** |
| **Jacket Foundation (Wind)** | Multi-leg lattice foundation | Technical literature + custom | Custom CAD | N/A | **MEDIUM** |
| **Floating Wind Platform** | Semi-submersible wind platform | [Free3D](https://free3d.com/premium-3d-models/offshore) | Various | Free/Commercial | **LOW** |
| **Wind Turbine Nacelle/Rotor** | Turbine components | Various manufacturers | Varies | Commercial/confidential | **LOW** |

**Relevance to digitalmodel:** Medium priority - growing application area for offshore analysis, particularly for floating wind mooring analysis.

**Acquisition Notes:**
- Search unavailable at time of catalog creation - recommend checking:
  - GrabCAD offshore wind section
  - NREL wind turbine databases
  - Major turbine manufacturer websites (Siemens, Vestas, GE)

### 5.2 Port & Harbor Structures

| Model Name | Description | Source/URL | Formats | License | Priority |
|------------|-------------|------------|---------|---------|----------|
| **Breakwater Sections** | Caisson/rubble mound breakwaters | Generic engineering libraries | STEP, IGES | Public domain | **LOW** |
| **Pier/Jetty Models** | Loading/unloading structures | [GrabCAD](https://grabcad.com/library/tag/offshore) | Various | Various | **LOW** |
| **Dolphin Mooring Posts** | Berthing/mooring dolphins | Generic libraries | STEP, IGES | Public domain | **LOW** |

**Relevance to digitalmodel:** Low priority - not core focus area but may be useful for coastal engineering extensions.

---

## 6. Hydrodynamic Test Models (Validation Critical)

### 6.1 ITTC/SIMMAN Benchmark Models

| Model Name | Description | Source/URL | Formats | License | Priority |
|------------|-------------|------------|---------|---------|----------|
| **KVLCC2** | Tanker maneuvering benchmark | [SIMMAN](https://simman2014.dk/), [GrabCAD](https://grabcad.com/library/kriso-kvlcc2-hull-1) | IGES, STEP | Research/Educational | **HIGHEST** |
| **KCS** | Container ship benchmark | [SIMMAN](https://simman2014.dk/), [GrabCAD](https://grabcad.com/library/kcs-hull-1) | IGES, STEP | Research/Educational | **HIGHEST** |
| **DTMB 5415** | Combatant maneuvering benchmark | [shipstab.org](http://www.shipstab.org/index.php/data-access/13-benchmarkingdata) | Various | Research/Educational | **HIGH** |
| **ITTC-Box (Aalto/NAPA)** | Simplified benchmark geometry | [shipstab.org](http://www.shipstab.org/index.php/data-access/13-benchmarkingdata) | STEP, IGES | Research/Educational | **MEDIUM** |

**Relevance to digitalmodel:** **CRITICAL** - These models have extensive experimental validation data (resistance, propulsion, maneuvering, seakeeping). Essential for validating any hydrodynamic analysis modules.

**Acquisition Notes:**
- SIMMAN workshop data: Register at [simman2014.dk](https://simman2014.dk/) for model test data access
- Ship Stability Research Group: [shipstab.org](http://www.shipstab.org/index.php/data-access/13-benchmarkingdata) benchmarking data portal
- ITTC procedures available at [ittc.info](https://ittc.info/)
- Many benchmark hulls also on GrabCAD with easier access

### 6.2 University/Research Models

| Model Name | Description | Source/URL | Formats | License | Priority |
|------------|-------------|------------|---------|---------|----------|
| **MIT Modular Ship Model** | Additively manufactured modular hull | [MIT DSpace](https://dspace.mit.edu/handle/1721.1/127043) | CAD files (check publication) | Research/Educational | **MEDIUM** |
| **FreeShip Hull Database** | Community-contributed designs | [FreeShip](https://sourceforge.net/projects/freeship/) | FreeShip native, export options | Open Source GPL | **HIGH** |

**Relevance to digitalmodel:** FreeShip particularly valuable as it's open-source parametric hull modeler that can generate custom geometries for analysis.

---

## 7. File Format Compatibility

### 7.1 Format Availability Summary

| Format | Prevalence | Blender Import | Recommended Usage |
|--------|------------|----------------|-------------------|
| **STEP (.stp, .step)** | Very High | Yes (via CAD Import addon) | **Preferred** - Industry standard, parametric data |
| **IGES (.igs, .iges)** | High | Yes (via CAD Import addon) | Good alternative to STEP |
| **STL (.stl)** | Very High | Native | Mesh-only, no parametrics, good for visualization |
| **OBJ (.obj)** | High | Native | Mesh with materials, good for rendering |
| **FBX (.fbx)** | Medium | Native | Animation-capable, commercial software |
| **3DS (.3ds)** | Medium | Native | Older format, still common |
| **Collada (.dae)** | Low | Native | XML-based, less common for engineering |

### 7.2 Blender Automation Module Compatibility

**Current Support:**
- Native: STL, OBJ, FBX, 3DS, Collada, PLY
- With CAD Import addon: STEP, IGES (requires BlenderBIM or similar)

**Recommended Acquisition Priority:**
1. **STEP/IGES** - Best for engineering analysis (preserves surfaces)
2. **STL** - Most widely available, sufficient for visualization/meshing
3. **OBJ** - Good for textured visualization

**Conversion Notes:**
- STEP/IGES → STL: Can convert via FreeCAD scripting
- Most commercial platforms (TurboSquid) offer multiple formats
- GrabCAD typically provides native CAD formats (STEP/IGES)

---

## 8. Licensing & Usage Rights

### 8.1 Open Source / Public Domain

| Source | License Type | Commercial Use | Restrictions |
|--------|--------------|----------------|--------------|
| **GrabCAD Community** | User-specified (check individual) | Varies by model | Often educational/non-commercial |
| **FreeShip** | GPL Open Source | Yes (with GPL compliance) | Derivative works must be GPL |
| **shipstab.org Benchmarks** | Research/Educational | Generally yes for research | Cite original sources |
| **SIMMAN Workshop** | Research/Educational | Research use typically allowed | Registration required |
| **MIT DSpace** | Research/Educational | Check individual publications | Often requires citation |
| **Cults3D (Free models)** | Creative Commons variants | Check specific license | Many allow commercial use |

### 8.2 Commercial / Restricted

| Source | License Type | Cost | Restrictions |
|--------|--------------|------|--------------|
| **TurboSquid** | Commercial royalty-free | $19-$500+ per model | Personal/commercial rights included |
| **Free3D Premium** | Commercial | Varies | Check individual model licenses |
| **Marathon OS** | Commercial | Varies | Engineering-grade models |
| **DNV OCX Models** | Commercial/Proprietary | Varies | Classification society standards |
| **Manufacturer Models** | Confidential/Proprietary | Often not public | Request from manufacturers |

### 8.3 Recommended Strategy for digitalmodel Project

1. **Phase 1 - Immediate (Free/Open)**
   - Acquire KVLCC2, KCS, DTMB 5415 from GrabCAD/shipstab.org
   - Download FreeShip and explore hull database
   - Acquire free drag anchor STL models from Cults3D
   - Get basic platform models from GrabCAD

2. **Phase 2 - Research Access (Registration Required)**
   - Register for SIMMAN workshop data portal
   - Access ITTC recommended procedures and benchmark data
   - Join shipstab.org for additional vessel data

3. **Phase 3 - Commercial (Budget Required)**
   - Purchase high-quality FPSO/platform models from TurboSquid if needed
   - Consider Marathon OS for engineering-grade ship models
   - Invest in specialized subsea equipment models if subsea focus expands

4. **Phase 4 - Custom Development**
   - Use FreeShip/FreeCAD to generate parametric models
   - Develop in-house suction pile parametric generator
   - Create mooring component library from technical specs

---

## 9. Integration with digitalmodel Project

### 9.1 Immediate Applications

**High Priority Acquisitions:**

1. **Catenary/Mooring Analysis Module:**
   - FPSO/platform models (GrabCAD, Free3D) - **Week 1**
   - Drag anchor STL models (Cults3D) - **Week 1**
   - Develop suction pile parametric generator - **Week 2-3**
   - Mooring buoy models (GrabCAD) - **Week 2**

2. **Hydrodynamic Analysis Module:**
   - KVLCC2 hull (GrabCAD/Marathon OS) - **Week 1**
   - KCS hull (GrabCAD/Marathon OS) - **Week 1**
   - DTMB 5415 (shipstab.org) - **Week 2**
   - Register for SIMMAN data access - **Week 2**

3. **Offshore Platform Design Module:**
   - Semi-submersible models (TurboSquid/Free3D) - **Week 3**
   - Jacket platform models (GrabCAD) - **Week 3**
   - SPAR/TLP models (TurboSquid) - **Week 4**

4. **Blender Automation Integration:**
   - Install BlenderBIM or CAD Import addon for STEP/IGES - **Week 1**
   - Test import pipeline with KVLCC2 STEP file - **Week 1**
   - Develop batch conversion scripts (STEP → STL) - **Week 2**

### 9.2 Model Processing Pipeline

**Recommended Workflow:**

```
1. Acquisition:
   └─> Download STEP/IGES (preferred) or STL

2. Validation:
   └─> Check geometry integrity in FreeCAD/Blender
   └─> Verify scale/units (common issue!)
   └─> Inspect for mesh errors (if STL)

3. Conversion (if needed):
   └─> STEP/IGES → FreeCAD → STL export
   └─> Apply mesh cleanup (non-manifold, normals)

4. Import to digitalmodel:
   └─> Blender automation module ingestion
   └─> Generate mesh for CFD/FEA
   └─> Extract hydrodynamic properties

5. Validation:
   └─> Compare extracted properties vs. published data
   └─> Check mass/displacement calculations
   └─> Verify principal dimensions
```

### 9.3 Critical Next Steps

**Immediate Actions (Week 1-2):**

1. ✅ Create GrabCAD account
2. ✅ Download KVLCC2 and KCS STEP files from GrabCAD
3. ✅ Download Marathon OS models (KVLCC2, KCS)
4. ✅ Test STEP import in Blender with CAD addon
5. ✅ Download 3-5 drag anchor STL models from Cults3D
6. ✅ Acquire 2-3 FPSO models from Free3D
7. ✅ Download FreeShip and explore hull generation
8. ✅ Document model library structure in digitalmodel repo

**Short-term (Week 3-4):**

1. Register for SIMMAN workshop data access
2. Access shipstab.org benchmark portal
3. Purchase 1-2 high-quality platform models from TurboSquid (if budget allows)
4. Develop parametric suction pile generator in FreeCAD
5. Create model metadata database (JSON/YAML catalog)
6. Integrate with Blender automation module

**Medium-term (Month 2-3):**

1. Develop automated model processing pipeline
2. Create validation suite using benchmark hull data
3. Build mooring component library
4. Implement model quality checks and unit tests
5. Generate technical documentation for model usage

---

## 10. Resources & References

### 10.1 Key Websites

**Free/Open Source:**
- [GrabCAD Library](https://grabcad.com/library) - Millions of free CAD models
- [Free3D](https://free3d.com) - Mix of free and premium models
- [Cults3D](https://cults3d.com) - 3D printable models (many free)
- [STLFinder](https://www.stlfinder.com) - STL model search engine
- [FreeShip SourceForge](https://sourceforge.net/projects/freeship/) - Open source ship design
- [shipstab.org Benchmarking Data](http://www.shipstab.org/index.php/data-access/13-benchmarkingdata) - Research vessel data
- [Open Simulation Platform](https://open-simulation-platform.github.io/demo-cases) - Maritime reference models

**Research & Standards:**
- [SIMMAN Workshop](https://simman2014.dk/) - Ship maneuvering benchmark models
- [ITTC](https://ittc.info/) - International Towing Tank Conference procedures
- [SINTEF Ocean](https://www.sintef.no/en/expert-list/ocean/cfd-computational-fluid-dynamics/) - Marine CFD research

**Commercial:**
- [TurboSquid](https://www.turbosquid.com) - High-quality commercial 3D models
- [Marathon OS](https://marathon-os.com) - Engineering-grade CAD models
- [3D Models DB](https://3dmdb.com) - Commercial model marketplace

**Technical Guidance:**
- [DNV Anchor Guidance Notes](https://ww2.eagle.org/content/dam/eagle/rules-and-guides/current/offshore/248_designinstallationdrag_and_plateanchors/Drag_and_Plate_Anchors_GN_e-Mar18.pdf)
- [NREL Offshore Anchor Data](https://www.osti.gov/servlets/purl/1178273)
- [DNV 3D Approval Platform](https://www.dnv.com/services/dnv-3d-approval-platform-248148/) - OCX standard info
- [Acteon SEPLA](https://acteon.com/solutions/project-lifecycle/offshore-construction/mooring-solutions/sepla) - Advanced anchor technology

### 10.2 Software Tools

**Free/Open Source:**
- **Blender** - 3D modeling and automation (core to digitalmodel)
- **FreeCAD** - Parametric CAD, STEP/IGES conversion
- **FreeShip** - Ship hull design and hydrostatics
- **MeshLab** - Mesh processing and cleanup
- **OpenFOAM** - CFD (potential future integration)

**Commercial (for reference):**
- **Rhino + Orca3D** - Marine design suite
- **MAXSURF** - Naval architecture
- **ANSYS AQWA** - Hydrodynamic analysis
- **DNV Sesam** - Offshore structural analysis

### 10.3 Documentation Standards

When adding models to digitalmodel repository, include:

**Metadata Requirements:**
```yaml
model_name: "KVLCC2_Hull"
source: "GrabCAD"
source_url: "https://grabcad.com/library/kriso-kvlcc2-hull-1"
file_format: ["STEP", "IGES", "STL"]
license: "Educational/Research"
principal_dimensions:
  length_pp: 320.0  # m
  beam: 58.0        # m
  draft: 20.8       # m
  displacement: 312622  # tonnes
validation_data_available: true
experimental_data_source: "SIMMAN 2008 Workshop"
digitalmodel_applications:
  - hydrodynamic_analysis
  - resistance_prediction
  - CFD_validation
acquisition_date: "2026-01-09"
notes: "Benchmark tanker hull with extensive validation data"
```

---

## 11. Summary & Recommendations

### 11.1 Highest Priority Models for Immediate Acquisition

**Must-Have (Week 1):**
1. ✅ KVLCC2 hull (STEP/IGES) - GrabCAD/Marathon OS
2. ✅ KCS hull (STEP/IGES) - GrabCAD/Marathon OS
3. ✅ 3x Drag anchor models (STL) - Cults3D
4. ✅ 2x FPSO models (various formats) - Free3D/GrabCAD
5. ✅ FreeShip software + hull database

**High Priority (Week 2-4):**
1. DTMB 5415 combatant hull - shipstab.org
2. Semi-submersible platform model - TurboSquid/GrabCAD
3. Jacket platform model - GrabCAD
4. Mooring buoy models - GrabCAD
5. Subsea Christmas tree (visualization) - STLFinder

### 11.2 Model Library Structure (Proposed)

```
digitalmodel/
└── data/
    └── 3d_models/
        ├── ships/
        │   ├── benchmarks/
        │   │   ├── KVLCC2/
        │   │   ├── KCS/
        │   │   └── DTMB_5415/
        │   └── vessels/
        │       ├── tankers/
        │       ├── PSV/
        │       └── containerships/
        ├── platforms/
        │   ├── FPSO/
        │   ├── semi_submersible/
        │   ├── jacket/
        │   ├── SPAR/
        │   └── TLP/
        ├── mooring/
        │   ├── anchors/
        │   │   ├── drag/
        │   │   ├── suction_pile/
        │   │   ├── VLA/
        │   │   └── driven_pile/
        │   ├── buoys/
        │   └── hardware/
        ├── subsea/
        │   ├── trees/
        │   ├── manifolds/
        │   └── templates/
        └── offshore_wind/
            ├── monopile/
            ├── jacket/
            └── floating/

        └── metadata/
            └── model_catalog.yaml
```

### 11.3 Quality Criteria for Model Selection

**Technical Requirements:**
- ✅ Available in STEP/IGES (preferred) or high-quality STL
- ✅ Proper scale and units specified
- ✅ Principal dimensions documented
- ✅ Clean geometry (minimal mesh errors)
- ✅ Manifold/watertight (for STL)

**Validation Requirements:**
- ✅ Benchmark models: Published experimental data available
- ✅ Platform models: Dimensions match industry standards
- ✅ Component models: Geometrically representative of class

**Licensing Requirements:**
- ✅ Free for research/educational use
- ✅ Or commercial license within project budget
- ✅ Clear attribution requirements understood

### 11.4 Cost Estimate (Optional Commercial Acquisitions)

**Free Models:** $0
- GrabCAD, Free3D, Cults3D, shipstab.org, FreeShip
- Estimated value: 90% of needed models available free

**Commercial Models (Optional):**
- High-quality FPSO model: $50-$150
- Semi-submersible detailed model: $100-$300
- Complete subsea equipment set: $200-$500
- Engineering-grade ship models (Marathon OS): $50-$200 each

**Total Budget (if all commercial options pursued):** $400-$1,150
**Recommended Budget (selective commercial):** $150-$300 for 2-3 key models

---

## 12. Next Steps & Action Items

### Immediate (This Week)

- [ ] Create accounts: GrabCAD, Marathon OS, shipstab.org
- [ ] Download KVLCC2 STEP file from GrabCAD
- [ ] Download KCS STEP file from GrabCAD
- [ ] Test STEP import in Blender (install CAD addon if needed)
- [ ] Download 5x drag anchor STL models from Cults3D
- [ ] Download 3x FPSO models from Free3D
- [ ] Install FreeShip and explore example hulls
- [ ] Create `/data/3d_models/` directory structure in repository

### Short-term (Next 2 Weeks)

- [ ] Register for SIMMAN workshop data access
- [ ] Download DTMB 5415 from shipstab.org
- [ ] Test STEP → STL conversion pipeline using FreeCAD
- [ ] Verify KVLCC2/KCS dimensions against published data
- [ ] Create model metadata catalog (YAML/JSON)
- [ ] Document model acquisition process in repository
- [ ] Integrate first benchmark hull with Blender automation module
- [ ] Develop parametric suction pile generator (FreeCAD script)

### Medium-term (Next Month)

- [ ] Complete benchmark hull library (KVLCC2, KCS, DTMB)
- [ ] Acquire/model 5+ platform types (FPSO, semi, jacket, SPAR, TLP)
- [ ] Build complete mooring anchor library (4+ types)
- [ ] Implement automated model validation checks
- [ ] Create visualization gallery of all acquired models
- [ ] Write technical documentation for model usage
- [ ] Develop mesh quality assessment tools
- [ ] Integrate models with analysis modules

---

## Document Revision History

| Version | Date | Changes | Author |
|---------|------|---------|--------|
| 1.0 | 2026-01-09 | Initial catalog creation | Research Agent |

---

## Sources

This catalog was compiled from research conducted on 2026-01-09 using the following primary sources:

**Open Source & Community:**
- [Open Simulation Platform Demo Cases](https://open-simulation-platform.github.io/demo-cases)
- [GrabCAD Library - Offshore Models](https://grabcad.com/library/tag/offshore)
- [GrabCAD Library - Marine Engineering](https://grabcad.com/library/tag/marine%20engineering)
- [GrabCAD - KVLCC2 Hull](https://grabcad.com/library/kriso-kvlcc2-hull-1)
- [GrabCAD - KCS Hull](https://grabcad.com/library/kcs-hull-1)
- [Free3D Offshore Models](https://free3d.com/premium-3d-models/offshore)
- [FreeShip Open Source](https://sourceforge.net/projects/freeship/)
- [Cults3D Anchor Models](https://cults3d.com/en/tags/anchor)

**Research & Benchmarks:**
- [SIMMAN 2014 Workshop](https://simman2014.dk/)
- [Ship Stability Research Group Benchmarking Data](http://www.shipstab.org/index.php/data-access/13-benchmarkingdata)
- [ITTC Procedures](https://ittc.info/)
- [MIT DSpace - Modular Ship Model](https://dspace.mit.edu/handle/1721.1/127043)
- [SINTEF Ocean CFD Research](https://www.sintef.no/en/expert-list/ocean/cfd-computational-fluid-dynamics/)

**Commercial Platforms:**
- [TurboSquid 3D Models](https://www.turbosquid.com)
- [Marathon OS CAD Models](https://marathon-os.com)
- [STLFinder Model Search](https://www.stlfinder.com)

**Technical References:**
- [DNV Anchor Guidance Notes (ABS)](https://ww2.eagle.org/content/dam/eagle/rules-and-guides/current/offshore/248_designinstallationdrag_and_plateanchors/Drag_and_Plate_Anchors_GN_e-Mar18.pdf)
- [NREL Offshore Anchor Data Report](https://www.osti.gov/servlets/purl/1178273)
- [DNV 3D Approval Platform](https://www.dnv.com/services/dnv-3d-approval-platform-248148/)
- [Acteon SEPLA Technology](https://acteon.com/solutions/project-lifecycle/offshore-construction/mooring-solutions/sepla)
- [Delmar Systems Geotechnical](https://delmarvryhof.com/service/geotechnical-and-structural/)

---

**END OF CATALOG**
