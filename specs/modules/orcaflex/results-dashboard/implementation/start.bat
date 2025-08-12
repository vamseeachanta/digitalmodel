@echo off
REM OrcaFlex Dashboard Startup Script for Windows

echo Starting OrcaFlex Dashboard...
echo.

REM Check if Docker is installed
docker --version >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: Docker is not installed. Please install Docker Desktop first.
    pause
    exit /b 1
)

REM Check if Docker Compose is installed
docker-compose --version >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: Docker Compose is not installed. Please install Docker Compose first.
    pause
    exit /b 1
)

REM Check if .env file exists
if not exist .env (
    echo Creating .env file from template...
    copy config\.env.example .env
    echo Please edit .env file with your configuration
)

REM Build and start services
echo Building Docker images...
docker-compose build

echo Starting services...
docker-compose up -d

REM Wait for services to be ready
echo Waiting for services to be ready...
timeout /t 10 /nobreak >nul

REM Check service status
echo Checking service status...
docker-compose ps

REM Run database migrations
echo Running database migrations...
docker-compose exec -T backend alembic upgrade head 2>nul || echo Migrations may need to be created

REM Display access URLs
echo.
echo OrcaFlex Dashboard is running!
echo.
echo Access the application at:
echo   Frontend: http://localhost:3000
echo   API Docs: http://localhost:8000/docs
echo.
echo Useful commands:
echo   View logs:    docker-compose logs -f
echo   Stop:         docker-compose down
echo   Restart:      docker-compose restart
echo.
pause