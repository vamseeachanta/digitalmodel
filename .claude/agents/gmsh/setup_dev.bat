@echo off
REM GMSH Agent Development Environment Setup Script for Windows

echo Setting up GMSH Agent development environment...

REM Check Python version
python --version 2>&1 | findstr /R "3\.1[0-9]" >nul
if errorlevel 1 (
    echo Error: Python 3.10+ is required.
    exit /b 1
)

REM Check if uv is available
where uv >nul 2>&1
if %errorlevel% == 0 (
    echo Using uv for package management...
    
    REM Install dependencies
    echo Installing GMSH and dependencies...
    uv pip install gmsh numpy scipy pyvista pyyaml click
    
    REM Install development dependencies
    echo Installing development tools...
    uv pip install pytest pytest-cov black flake8 mypy
) else (
    echo uv not found, using pip...
    
    REM Create virtual environment if not exists
    if not exist "venv" (
        echo Creating virtual environment...
        python -m venv venv
    )
    
    REM Activate virtual environment
    call venv\Scripts\activate.bat
    
    REM Upgrade pip
    python -m pip install --upgrade pip
    
    REM Install dependencies
    echo Installing GMSH and dependencies...
    pip install gmsh numpy scipy pyvista pyyaml click
    
    REM Install development dependencies
    echo Installing development tools...
    pip install pytest pytest-cov black flake8 mypy
)

REM Verify GMSH installation
echo Verifying GMSH installation...
python -c "import gmsh; print(f'GMSH installed successfully')" 2>nul
if %errorlevel% == 0 (
    echo GMSH successfully installed!
) else (
    echo Warning: GMSH installation verification failed
    echo You may need to install GMSH system libraries
    echo Visit: https://gmsh.info/#Download
)

REM Create necessary directories
echo Creating directory structure...
if not exist "workflows" mkdir workflows
if not exist "templates" mkdir templates
if not exist "utilities" mkdir utilities
if not exist "tests\unit" mkdir tests\unit
if not exist "tests\integration" mkdir tests\integration

REM Copy environment file if not exists
if not exist ".env" (
    echo Creating .env file from template...
    copy .env.example .env
)

echo Setup complete!
echo.
echo To activate the environment:
echo   venv\Scripts\activate.bat  (if using pip)
echo.
echo To run the agent:
echo   python run_gmsh_agent.py --help
echo.
echo To run tests:
echo   pytest tests\