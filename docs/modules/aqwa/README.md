# AQWA Documentation

Comprehensive documentation for ANSYS AQWA hydrodynamic analysis software.

> **Note:** This is the consolidated AQWA documentation. The duplicate folder at `/docs/modules/ansys/aqwa` has been merged and removed.

## 📚 Quick Navigation

### ⚡ Quick Start & CLI Tools
- **[QUICK START GUIDE](QUICK_START.md)** - Fast setup and CLI usage
- **Command Line Interface** - Run analyses from terminal:
  ```bash
  # AQWA CLI
  python src/digitalmodel/modules/aqwa/aqwa_cli.py --method raos --folder <path>

  # Unified Diffraction CLI (AQWA + OrcaWave)
  python src/digitalmodel/modules/diffraction_cli.py aqwa --method raos --folder <path>
  ```

### 🚀 Getting Started
- [AQWA Overview](getting-started/aqwa-overview.md) - Introduction to AQWA capabilities
- [Introduction](getting-started/introduction.md) - Getting started with AQWA
- [Geometry Basics](getting-started/geometry-basics.md) - Understanding geometry in AQWA
- [File Preparation](getting-started/file-preparation.md) - Preparing input files
- [DAT and Workbench](getting-started/dat-and-workbench.md) - Working with DAT files and Workbench
- [DAT to Workbench Migration](getting-started/dat-to-workbench.md) - Converting DAT files to Workbench

### 📖 Reference Manuals
Official ANSYS AQWA documentation:
- [User's Manual](reference-manuals/Aqwa_Users_Manual.pdf) - Complete user guide
- [Theory Manual](reference-manuals/Aqwa_Theory_Manual.pdf) - Theoretical background
- [Reference Manual](reference-manuals/Aqwa_Reference_Manual.pdf) - Detailed reference
- [Graphical Supervisor Guide](reference-manuals/Aqwa_Graphical_Supervisor_Users_Guide.pdf) - GUI usage
- [AQL Manual](reference-manuals/Aqwa_AQL_Manual.pdf) - AQWA Query Language

### 🔄 Workflows & Analysis
- [General Instructions](workflows/general-instructions.md) - General workflow guidelines
- [Model Checks](workflows/model-checks.md) - Basic model validation
- [Convergence Analysis](workflows/convergence-analysis.md) - Convergence studies
- [RAO Analysis](workflows/rao-analysis.md) - Response Amplitude Operators
- [Fender Analysis](workflows/fender-analysis.md) - Fender system modeling
- [Post-Processing](workflows/post-processing.md) - Results post-processing

### ⚙️ Advanced Topics
- [Advanced Techniques](workflows/advanced-techniques.md) - Advanced modeling techniques
- [AQWA-GS](workflows/aqwa-gs.md) - Graphical Supervisor details
- [AQWA Reader](workflows/aqwa-reader.md) - Reading AQWA output files
- [Scripting Guide](workflows/scripting-guide.md) - Automation and scripting
- [External Forces](workflows/external-forces/) - External force implementation

### 🚨 Troubleshooting
- [Error Handling](workflows/error-handling.md) - Common errors and solutions
- [Warning Handling](workflows/warning-handling.md) - Understanding warnings
- [Lessons Learned](workflows/lessons-learned.md) - Best practices from experience

### 💻 Scripts & Tools
- [Analysis Scripts](scripts/) - Ready-to-use analysis scripts
  - [Mooring Analysis](scripts/mooring_analysis/) - Mooring system analysis tools
  - [RAO Calculations](scripts/RAOs/) - RAO extraction and processing
  - [Templates](scripts/templates/) - Template files for common analyses

### 📊 Examples & Tutorials
- [Tutorial Examples](examples/) - Step-by-step tutorials
  - [Geometry Creation](examples/01_geometry/) - Geometry modeling tutorials
  - [Workbench Examples](examples/02_wb/) - Workbench workflow examples
  - [DAT File Examples](examples/03_dat/) - DAT file analysis examples
    - Ship RAOs
    - Ship with Pier
    - FPSO Turret System
  - [Restart Analysis](examples/102_restart/) - Restart and continuation runs
  - [SPAR Example](examples/spar-example/) - SPAR platform analysis
  - [Training Materials](examples/training-materials/) - AQWA training PDFs and workshops

### 🎨 Assets & Diagrams
- [Flowcharts & Diagrams](assets/) - Visual guides and workflow diagrams
  - Geometry creation diagrams (PNG, SVG, PlantUML)
  - Modular analysis workflows (PNG, SVG, PlantUML)
  - Process flowcharts and visualizations
- Images and illustrations for documentation

## 📋 Documentation Structure

```
aqwa/
├── README.md                    # This file
├── getting-started/             # Introductory materials
├── reference-manuals/           # Official ANSYS documentation
├── workflows/                   # Analysis workflows and procedures
│   └── external-forces/         # External force implementations
├── scripts/                     # Analysis and automation scripts
├── examples/                    # Tutorial examples and case studies
└── assets/                      # Images, diagrams, and flowcharts
```

## 🔗 Quick Links

- **New to AQWA?** Start with [Getting Started](getting-started/introduction.md)
- **Need reference?** Check [Reference Manuals](reference-manuals/)
- **Running analysis?** See [Workflows](workflows/general-instructions.md)
- **Troubleshooting?** Visit [Error Handling](workflows/error-handling.md)
- **Examples?** Browse [Tutorials](examples/)

## 📝 Notes

- All PDF manuals are official ANSYS documentation
- Scripts are provided as examples and should be validated for your specific use case
- External force implementations may require version-specific adjustments

## 🤝 Contributing

When adding new documentation:
1. Place files in the appropriate category folder
2. Update this README with links to new content
3. Follow existing naming conventions (kebab-case for files)
4. Include clear descriptions and cross-references