@echo off
REM Quick Start Script for OrcaWave MCP Server
REM Run this to quickly set up and start the server

echo ============================================
echo OrcaWave MCP Server Quick Start
echo ============================================
echo.

REM Check Python installation
python --version >nul 2>&1
if errorlevel 1 (
    echo [ERROR] Python is not installed or not in PATH
    echo Please install Python 3.9+ and try again
    pause
    exit /b 1
)

echo [1/5] Python installation found
python --version

REM Check if virtual environment exists
if not exist "..\..\..\.venv" (
    echo [2/5] Creating virtual environment...
    python -m venv ..\..\..\.venv
) else (
    echo [2/5] Virtual environment already exists
)

REM Activate virtual environment
echo [3/5] Activating virtual environment...
call ..\..\..\.venv\Scripts\activate.bat

REM Install dependencies
echo [4/5] Installing dependencies...
echo.

REM Core dependencies
pip install --quiet fastmcp pyyaml 2>nul
if errorlevel 0 echo   - Core dependencies installed

REM Try to install optional dependencies
pip install --quiet pywin32 2>nul
if errorlevel 0 (
    echo   - Windows COM support installed
) else (
    echo   - Windows COM support not available
)

pip install --quiet opencv-python pillow numpy 2>nul
if errorlevel 0 (
    echo   - Vision features installed
) else (
    echo   - Vision features not available
)

pip install --quiet fastapi uvicorn websockets 2>nul
if errorlevel 0 (
    echo   - WebSocket monitoring installed
) else (
    echo   - WebSocket monitoring not available
)

echo.
echo [5/5] Starting server...
echo.

REM Check which mode to run
if "%1"=="--mcp" (
    echo Starting in MCP mode (requires FastMCP)...
    python ..\..\..\mcp\orcawave\run_server.py
) else (
    echo Starting in standalone mode...
    echo.
    echo Server will be available at:
    echo   - Status: http://localhost:3100/
    echo   - WebSocket: ws://localhost:8765/
    echo.
    echo Press Ctrl+C to stop the server
    echo ============================================
    echo.
    python ..\..\..\mcp\orcawave\run_server.py --standalone
)

pause