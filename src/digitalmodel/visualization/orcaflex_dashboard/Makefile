# OrcaFlex Results Visualization Dashboard - Development Commands
.PHONY: help build start stop clean test lint format install dev logs ps health

# Default target
help: ## Show this help message
	@echo "OrcaFlex Results Visualization Dashboard - Development Commands"
	@echo ""
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

# Development Commands
dev: ## Start development environment
	@echo "Starting development environment..."
	docker-compose up -d --build
	@echo "Backend available at: http://localhost:8000"
	@echo "Frontend available at: http://localhost:3000"
	@echo "API docs available at: http://localhost:8000/api/docs"

start: dev ## Alias for dev

stop: ## Stop development environment
	@echo "Stopping development environment..."
	docker-compose down

restart: ## Restart development environment
	@echo "Restarting development environment..."
	docker-compose down
	docker-compose up -d --build

clean: ## Clean up containers, images, and volumes
	@echo "Cleaning up..."
	docker-compose down -v --remove-orphans
	docker system prune -f
	docker volume prune -f

# Build Commands
build: ## Build all images
	@echo "Building all images..."
	docker-compose build

build-backend: ## Build backend image only
	@echo "Building backend image..."
	docker-compose build backend

build-frontend: ## Build frontend image only
	@echo "Building frontend image..."
	docker-compose build frontend

# Production Commands
prod-start: ## Start production environment
	@echo "Starting production environment..."
	docker-compose -f docker-compose.prod.yml up -d --build

prod-stop: ## Stop production environment
	@echo "Stopping production environment..."
	docker-compose -f docker-compose.prod.yml down

prod-logs: ## View production logs
	docker-compose -f docker-compose.prod.yml logs -f

# Testing Commands
test: ## Run all tests
	@echo "Running all tests..."
	$(MAKE) test-backend
	$(MAKE) test-frontend

test-backend: ## Run backend tests
	@echo "Running backend tests..."
	cd backend && python -m pytest --cov=app --cov-report=term-missing

test-frontend: ## Run frontend tests
	@echo "Running frontend tests..."
	cd frontend && npm test -- --coverage --watchAll=false

test-e2e: ## Run end-to-end tests
	@echo "Running E2E tests..."
	# Add your E2E test commands here

# Code Quality Commands
lint: ## Run all linting
	@echo "Running linting..."
	$(MAKE) lint-backend
	$(MAKE) lint-frontend

lint-backend: ## Lint backend code
	@echo "Linting backend..."
	cd backend && flake8 app
	cd backend && mypy app --ignore-missing-imports

lint-frontend: ## Lint frontend code
	@echo "Linting frontend..."
	cd frontend && npm run lint

format: ## Format all code
	@echo "Formatting code..."
	$(MAKE) format-backend
	$(MAKE) format-frontend

format-backend: ## Format backend code
	@echo "Formatting backend..."
	cd backend && black app
	cd backend && isort app

format-frontend: ## Format frontend code
	@echo "Formatting frontend..."
	cd frontend && npm run format

# Installation Commands
install: ## Install all dependencies
	@echo "Installing dependencies..."
	$(MAKE) install-backend
	$(MAKE) install-frontend

install-backend: ## Install backend dependencies
	@echo "Installing backend dependencies..."
	cd backend && pip install -r requirements.txt

install-frontend: ## Install frontend dependencies
	@echo "Installing frontend dependencies..."
	cd frontend && npm install

# Monitoring Commands
logs: ## View application logs
	docker-compose logs -f

logs-backend: ## View backend logs only
	docker-compose logs -f backend

logs-frontend: ## View frontend logs only
	docker-compose logs -f frontend

ps: ## Show running containers
	docker-compose ps

health: ## Check application health
	@echo "Checking application health..."
	@echo "Backend health:"
	@curl -s http://localhost:8000/api/health | jq . || echo "Backend not accessible"
	@echo ""
	@echo "Frontend health:"
	@curl -s http://localhost:3000/api/health | jq . || echo "Frontend not accessible"

# Database Commands
db-init: ## Initialize database
	@echo "Initializing database..."
	docker-compose exec backend python -c "from app.database import init_db; init_db()"

db-migrate: ## Run database migrations
	@echo "Running database migrations..."
	docker-compose exec backend alembic upgrade head

db-reset: ## Reset database
	@echo "Resetting database..."
	docker-compose down -v postgres
	docker-compose up -d postgres
	sleep 5
	$(MAKE) db-init

# Data Commands
generate-sample-data: ## Generate sample data for development
	@echo "Generating sample data..."
	docker-compose exec backend python -c "from scripts.generate_sample_data import main; main()"

backup-data: ## Backup database
	@echo "Backing up database..."
	docker-compose exec postgres pg_dump -U orcaflex orcaflex_dashboard > backup_$(shell date +%Y%m%d_%H%M%S).sql

# SSL Commands
generate-ssl: ## Generate SSL certificates for development
	@echo "Generating SSL certificates..."
	mkdir -p nginx/ssl
	openssl req -x509 -nodes -days 365 -newkey rsa:2048 \
		-keyout nginx/ssl/cert.key \
		-out nginx/ssl/cert.crt \
		-subj "/C=US/ST=State/L=City/O=Organization/CN=localhost"

# Security Commands
security-scan: ## Run security scans
	@echo "Running security scans..."
	docker run --rm -v "$(PWD):/src" aquasec/trivy fs /src

# Documentation Commands
docs: ## Generate documentation
	@echo "Generating documentation..."
	cd backend && python -c "import app.main; print('Backend API docs: http://localhost:8000/api/docs')"

# Environment Commands
env-setup: ## Set up environment files
	@echo "Setting up environment files..."
	cp backend/.env.example backend/.env
	cp frontend/.env.example frontend/.env
	@echo "Please edit the .env files with your configuration"

# Analytics Commands
analyze-bundle: ## Analyze frontend bundle size
	@echo "Analyzing bundle size..."
	cd frontend && npm run analyze

monitor: ## Start monitoring stack
	@echo "Starting monitoring stack..."
	docker-compose -f docker-compose.prod.yml up -d prometheus grafana
	@echo "Prometheus: http://localhost:9090"
	@echo "Grafana: http://localhost:3001"

# Maintenance Commands
update-deps: ## Update dependencies
	@echo "Updating dependencies..."
	cd backend && pip-compile --upgrade requirements.in
	cd frontend && npm update

check-updates: ## Check for dependency updates
	@echo "Checking for updates..."
	cd backend && pip list --outdated
	cd frontend && npm outdated