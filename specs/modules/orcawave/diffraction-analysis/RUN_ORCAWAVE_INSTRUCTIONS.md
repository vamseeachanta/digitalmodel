# OrcaWave Analysis Execution Instructions

## ✅ Setup Complete

All configurations and scripts are ready for OrcaWave analysis of the Sea Cypress vessel.

## 📁 Files Ready

1. **Geometry**: `inputs/geometry/sea_cypress_trimesh.gdf` (24,332 panels)
2. **Configuration**: `configs/sea_cypress_diffraction.yml`
3. **Scripts**: Multiple execution options available

## 🚀 How to Run OrcaWave Analysis

### Option 1: Windows Batch File (Recommended)
```batch
cd specs\modules\orcawave\diffraction-analysis\scripts
run_orcawave_analysis.bat
```
- Interactive mode with progress display
- Automatic result processing after completion
- Full logging

### Option 2: Background Execution
```batch
cd specs\modules\orcawave\diffraction-analysis\scripts
start_orcawave_background.bat
```
- Runs in background (you can continue working)
- Check logs for progress
- Results appear in outputs folder when done

### Option 3: Python Direct
```bash
cd specs/modules/orcawave/diffraction-analysis/scripts
python execute_orcawave.py
```
- Direct Python interface
- Real-time output
- Automatic result processing

### Option 4: Manual OrcaWave GUI
1. Open OrcaWave from: `C:\Program Files (x86)\Orcina\OrcaFlex\11.5\OrcaWave.exe`
2. Import configuration: `configs/sea_cypress_diffraction.yml`
3. Load mesh: `inputs/geometry/sea_cypress_trimesh.gdf`
4. Run analysis (Solve → Run Full Analysis)
5. Export results to `outputs/` folder

## ⏱️ Expected Runtime

- **Total Calculations**: 162 (18 frequencies × 9 headings)
- **Estimated Time**: 2-3 hours
- **Panel Count**: 24,332
- **Memory Required**: ~4-8 GB RAM

## 📊 Output Files Expected

After successful completion, these files will be created in `outputs/`:

1. **Hydrodynamic Coefficients**:
   - `sea_cypress_added_mass.csv`
   - `sea_cypress_damping.csv`

2. **Wave Loads**:
   - `sea_cypress_excitation.csv`
   - `sea_cypress_RAOs.xlsx`

3. **Second-Order** (if QTF enabled):
   - `sea_cypress_QTF_sum.csv`
   - `sea_cypress_mean_drift.csv`

## 🔄 Process Results After Analysis

Once OrcaWave completes, process the results:

```bash
cd specs/modules/orcawave/diffraction-analysis/scripts
python process_orcawave_results.py --output-dir ../outputs
```

This will:
- Validate hydrodynamic coefficients
- Create visualization plots
- Generate OrcaFlex-compatible files
- Produce analysis report

## 🎯 Verification Checklist

Before running:
- [x] GDF file exists: `sea_cypress_trimesh.gdf`
- [x] Configuration ready: `sea_cypress_diffraction.yml`
- [x] OrcaWave installed: `C:\Program Files (x86)\Orcina\OrcaFlex\11.5\`
- [ ] License available (check with IT/licensing)
- [x] Output directory created: `outputs/`
- [x] ~10 GB disk space available

## ⚠️ Troubleshooting

### License Not Available
- Contact IT for license server access
- Check VPN connection if remote
- Verify license features include OrcaWave

### Analysis Fails to Start
- Check Windows Defender/Antivirus exceptions
- Verify file permissions on output directory
- Ensure sufficient RAM available

### Results Not Generated
- Check log files in `logs/` directory
- Verify mesh file is valid GDF format
- Reduce frequency/heading points if memory limited

## 📈 Current Status

✅ **Ready to Run** - All preparations complete:
- Geometry converted and validated
- Configuration optimized
- Scripts tested with mock data
- Processing pipeline verified

⏸️ **Waiting For**:
- OrcaWave license availability
- User to initiate analysis

## 💡 Quick Test with Mock Data

To test the complete pipeline without OrcaWave license:

```bash
# Generate mock data
cd scripts
python create_mock_orcawave_data.py ../outputs

# Process mock results
python process_orcawave_results.py --output-dir ../outputs
```

This will demonstrate the full processing pipeline and generate sample plots.

---

## 📞 Support

If you encounter issues:
1. Check log files in `logs/` directory
2. Verify OrcaWave installation path
3. Ensure license server connectivity
4. Review error messages in console output

The analysis is fully configured and ready to execute as soon as the OrcaWave license is available.