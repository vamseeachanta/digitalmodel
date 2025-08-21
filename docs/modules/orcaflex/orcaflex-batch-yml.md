# OrcaFlex Batch Files for Running .yml Files

This guide demonstrates various ways to write batch files to run OrcaFlex .yml files, based on the official OrcaFlex documentation.

## 1. Simple Windows Batch File (.bat) to Run Multiple .yml Files

```batch
@echo off
REM Simple batch to run OrcaFlex .yml files sequentially

echo Running OrcaFlex simulations...

"C:\Program Files\Orcina\OrcaFlex\11.0\OrcaFlex.exe" "C:\MyProject\model1.yml"
"C:\Program Files\Orcina\OrcaFlex\11.0\OrcaFlex.exe" "C:\MyProject\model2.yml"
"C:\Program Files\Orcina\OrcaFlex\11.0\OrcaFlex.exe" "C:\MyProject\model3.yml"

echo All simulations complete!
pause
```

## 2. Batch File with Command Line Options

```batch
@echo off
REM Run OrcaFlex with specific options

set ORCAFLEX="C:\Program Files\Orcina\OrcaFlex\11.0\OrcaFlex.exe"
set PROJECT_DIR=C:\MyProject

REM Run with disabled dynamics (statics only)
%ORCAFLEX% "%PROJECT_DIR%\statics_only.yml" /DisableDynamics

REM Run with specific thread count
%ORCAFLEX% "%PROJECT_DIR%\parallel_run.yml" /ThreadCount=8

REM Run with FlexNet license only
%ORCAFLEX% "%PROJECT_DIR%\model.yml" /FlexNetOnly

pause
```

## 3. OrcaFlex Batch Script File (.txt) for Processing .yml Files

Create a text file named `batch_script.txt` with OrcaFlex script commands:

```
// OrcaFlex Batch Script
// Load and run multiple .yml files

LoadData "C:\MyProject\base_model.yml"
RunStatics
SaveData "C:\MyProject\base_model_statics.yml"
Reset

LoadData "C:\MyProject\variation1.yml"
RunDynamics
Save "C:\MyProject\variation1_results.sim"
Reset

LoadData "C:\MyProject\variation2.yml"
RunDynamics
Save "C:\MyProject\variation2_results.sim"
```

## 4. Batch File Using a List File (.lst)

OrcaFlex supports .lst files containing a list of files to be processed. 

First, create a list file `models.lst`:

```
model1.yml
model2.yml
subfolder\model3.yml
subfolder\model4.yml
```

Then create a batch file to use it:

```batch
@echo off
"C:\Program Files\Orcina\OrcaFlex\11.0\OrcaFlex.exe" /batch "C:\MyProject\models.lst"
```

## 5. Batch Script for Parametric Studies

Create a batch script file `parametric_study.txt`:

```
// Load base model
LoadData "base_model.yml"

// Variation 1: Change wave height
Select Environment
WaveHeight = 2.0
SaveData "case_H2.yml"

// Variation 2: Change wave height
Select Environment  
WaveHeight = 3.0
SaveData "case_H3.yml"

// Variation 3: Change wave period
Select Environment
WaveHeight = 2.5
WavePeriod = 8.0
SaveData "case_H2.5_T8.yml"
```

## 6. Advanced Batch File with Error Handling

```batch
@echo off
setlocal enabledelayedexpansion

set ORCAFLEX="C:\Program Files\Orcina\OrcaFlex\11.0\OrcaFlex.exe"
set LOG_FILE=simulation_log.txt
set ERROR_COUNT=0

echo Simulation started at %date% %time% > %LOG_FILE%

for %%f in (*.yml) do (
    echo Processing %%f...
    echo Processing %%f >> %LOG_FILE%
    
    %ORCAFLEX% "%%f"
    
    if !errorlevel! neq 0 (
        echo ERROR: Failed to process %%f >> %LOG_FILE%
        set /a ERROR_COUNT+=1
    ) else (
        echo SUCCESS: %%f completed >> %LOG_FILE%
    )
)

echo.
echo Simulation completed with !ERROR_COUNT! errors
echo Simulation completed at %date% %time% with !ERROR_COUNT! errors >> %LOG_FILE%

pause
```

## 7. Python Script for Batch Processing (Alternative to .bat)

```python
import subprocess
import os
from pathlib import Path

# OrcaFlex executable path
orcaflex_exe = r"C:\Program Files\Orcina\OrcaFlex\11.0\OrcaFlex.exe"

# Directory containing .yml files
project_dir = Path(r"C:\MyProject")

# Find all .yml files
yml_files = list(project_dir.glob("*.yml"))

# Process each file
for yml_file in yml_files:
    print(f"Processing {yml_file.name}...")
    
    # Run OrcaFlex
    result = subprocess.run([orcaflex_exe, str(yml_file)], 
                          capture_output=True, text=True)
    
    if result.returncode == 0:
        print(f"✓ {yml_file.name} completed successfully")
    else:
        print(f"✗ {yml_file.name} failed with error")
        print(result.stderr)
```

## 8. Batch File for Restart Analysis

For restart models that have parent dependencies:

```batch
@echo off
REM Process restart chain in correct order

set ORCAFLEX="C:\Program Files\Orcina\OrcaFlex\11.0\OrcaFlex.exe"

echo Running base model...
%ORCAFLEX% "base.dat"

echo Running first restart...
%ORCAFLEX% "m1.yml"

echo Running second restart...
%ORCAFLEX% "m2.yml"

pause
```

## Key Command Line Options

### License Options
- `/FlexNetOnly` - Only search for FlexNet licences
- `/DongleOnly` - Only search for dongle licences

### Performance Options
- `/ThreadCount=N` - Set number of execution threads (e.g., `/ThreadCount=8`)
- `/DisableDynamics` - Run statics analysis only

### Display Options
- `/ThickLines=N` - Set minimum line thickness in 3D views

## Important Notes

### File Paths
- Always use quotes around file paths that contain spaces
- Relative paths are interpreted relative to the script location

### Script Commands for .yml Files
- `LoadData "filename.yml"` - Load data from a .yml file
- `SaveData "filename.yml"` - Save data to a .yml file (text format)
- `Save "filename.sim"` - Save complete simulation results

### Batch Processing Features
- OrcaFlex can use multiple processor cores for parallel processing
- Auto-save functionality available for long-running simulations
- Failed jobs are abandoned but other jobs continue

### Error Handling
- Check `errorlevel` after each OrcaFlex execution in batch files
- Use logging to track successful and failed runs
- Consider implementing retry logic for critical simulations

## Example Directory Structure

```
C:\MyProject\
├── base_model.yml
├── variations\
│   ├── case1.yml
│   ├── case2.yml
│   └── case3.yml
├── scripts\
│   ├── run_all.bat
│   ├── parametric_study.txt
│   └── models.lst
└── results\
    └── (simulation output files)
```

## References

- [OrcaFlex Batch Processing Documentation](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Batchprocessing,Introduction.htm)
- [OrcaFlex Script Commands](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Batchscript,Scriptcommands.htm)
- [OrcaFlex Command Line Parameters](https://www.orcina.com/webhelp/OrcaFlex/Content/html/RunningOrcaFlex.htm)