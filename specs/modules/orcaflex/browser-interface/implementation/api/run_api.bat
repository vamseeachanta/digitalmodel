@echo off
echo ========================================
echo OrcaFlex Browser API Server
echo ========================================
echo.

REM Check if FastAPI is installed
python -c "import fastapi" 2>nul
if errorlevel 1 (
    echo Installing required packages...
    pip install -r requirements.txt
    echo.
)

echo Starting API server...
echo.
echo API will be available at:
echo   - http://localhost:8000
echo   - http://localhost:8000/docs (Swagger UI)
echo   - http://localhost:8000/redoc (ReDoc)
echo.
echo Press Ctrl+C to stop the server
echo ========================================
echo.

python main.py