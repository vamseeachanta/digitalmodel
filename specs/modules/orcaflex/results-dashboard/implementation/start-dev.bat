@echo off
echo ================================================
echo     OrcaFlex Dashboard - Development Mode
echo ================================================
echo.

REM Navigate to the dashboard directory
cd /d "%~dp0"

echo Checking Docker installation...
docker --version >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: Docker is not installed or not running!
    echo Please install Docker Desktop and ensure it's running.
    pause
    exit /b 1
)

echo Docker is installed and running.
echo.

echo Starting development environment...
echo This will install dependencies on first run...
echo.

REM Start with docker compose in dev mode
docker compose -f docker-compose.dev.yml up

echo.
echo Development environment stopped.
pause