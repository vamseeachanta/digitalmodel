@echo off
echo ================================================
echo     OrcaFlex Dashboard - Docker Startup
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

echo Building and starting services...
echo This may take a few minutes on first run...
echo.

REM Build and start with docker compose
docker compose up --build -d

if %errorlevel% neq 0 (
    echo.
    echo ERROR: Failed to start services!
    echo Please check the error messages above.
    pause
    exit /b 1
)

echo.
echo ================================================
echo     OrcaFlex Dashboard is starting up!
echo ================================================
echo.
echo Services are initializing. Please wait...
timeout /t 10 /nobreak >nul

echo.
echo Checking service status...
docker compose ps

echo.
echo ================================================
echo     Dashboard is ready!
echo ================================================
echo.
echo Access the application at:
echo   - Frontend: http://localhost:3000
echo   - API Docs: http://localhost:8000/docs
echo   - Database: localhost:5432
echo.
echo Useful commands:
echo   View logs:    docker compose logs -f
echo   Stop:         docker compose down
echo   Restart:      docker compose restart
echo   Shell:        docker compose exec backend bash
echo.
pause