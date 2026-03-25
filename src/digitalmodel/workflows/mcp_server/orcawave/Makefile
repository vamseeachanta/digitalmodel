# OrcaWave MCP Server Makefile
# Provides convenient commands for development, testing, and deployment

.PHONY: help install test run deploy clean docker-build docker-up docker-down

# Variables
PYTHON := python
PIP := pip
PYTEST := pytest
DOCKER := docker
DOCKER_COMPOSE := docker-compose
PROJECT_NAME := orcawave-mcp
VENV_DIR := .venv
SRC_DIR := src/mcp/orcawave
TEST_DIR := $(SRC_DIR)/tests
CLIENT_DIR := $(SRC_DIR)/client

# Colors for output
RED := \033[0;31m
GREEN := \033[0;32m
YELLOW := \033[1;33m
NC := \033[0m # No Color

# Default target
help:
	@echo "$(GREEN)OrcaWave MCP Server - Available Commands$(NC)"
	@echo "================================================"
	@echo ""
	@echo "$(YELLOW)Development:$(NC)"
	@echo "  make install        - Install all dependencies"
	@echo "  make install-dev    - Install with development dependencies"
	@echo "  make venv          - Create virtual environment"
	@echo "  make clean         - Clean build artifacts and cache"
	@echo ""
	@echo "$(YELLOW)Testing:$(NC)"
	@echo "  make test          - Run all tests"
	@echo "  make test-unit     - Run unit tests only"
	@echo "  make test-integration - Run integration tests"
	@echo "  make test-coverage - Run tests with coverage report"
	@echo "  make lint          - Run code linting"
	@echo "  make format        - Format code with black"
	@echo ""
	@echo "$(YELLOW)Running:$(NC)"
	@echo "  make run           - Run MCP server (standalone mode)"
	@echo "  make run-mcp       - Run MCP server (FastMCP mode)"
	@echo "  make run-dashboard - Run dashboard server"
	@echo "  make run-cli       - Run CLI client"
	@echo "  make run-all       - Run complete stack"
	@echo ""
	@echo "$(YELLOW)Docker:$(NC)"
	@echo "  make docker-build  - Build Docker images"
	@echo "  make docker-up     - Start Docker containers"
	@echo "  make docker-down   - Stop Docker containers"
	@echo "  make docker-logs   - Show container logs"
	@echo "  make docker-clean  - Clean Docker resources"
	@echo ""
	@echo "$(YELLOW)Deployment:$(NC)"
	@echo "  make deploy-staging - Deploy to staging"
	@echo "  make deploy-prod   - Deploy to production"
	@echo "  make backup        - Backup production data"
	@echo ""
	@echo "$(YELLOW)Documentation:$(NC)"
	@echo "  make docs          - Generate documentation"
	@echo "  make docs-serve    - Serve documentation locally"

# Virtual environment
venv:
	@echo "$(GREEN)Creating virtual environment...$(NC)"
	$(PYTHON) -m venv $(VENV_DIR)
	@echo "$(GREEN)Virtual environment created at $(VENV_DIR)$(NC)"
	@echo "Activate with: source $(VENV_DIR)/bin/activate (Linux/Mac) or $(VENV_DIR)\\Scripts\\activate (Windows)"

# Installation targets
install: venv
	@echo "$(GREEN)Installing dependencies...$(NC)"
	$(VENV_DIR)/bin/pip install --upgrade pip
	$(VENV_DIR)/bin/pip install -r requirements.txt
	@echo "$(GREEN)Dependencies installed successfully$(NC)"

install-dev: install
	@echo "$(GREEN)Installing development dependencies...$(NC)"
	$(VENV_DIR)/bin/pip install pytest pytest-cov pytest-asyncio pytest-mock
	$(VENV_DIR)/bin/pip install black ruff isort mypy
	$(VENV_DIR)/bin/pip install sphinx mkdocs mkdocs-material
	@echo "$(GREEN)Development dependencies installed$(NC)"

install-windows:
	@echo "$(GREEN)Installing Windows-specific dependencies...$(NC)"
	$(PIP) install pywin32
	$(PYTHON) Scripts/pywin32_postinstall.py -install
	@echo "$(GREEN)Windows dependencies installed$(NC)"

# Testing targets
test:
	@echo "$(GREEN)Running all tests...$(NC)"
	$(PYTEST) $(TEST_DIR) -v

test-unit:
	@echo "$(GREEN)Running unit tests...$(NC)"
	$(PYTEST) $(TEST_DIR)/test_integration.py -v

test-integration:
	@echo "$(GREEN)Running integration tests...$(NC)"
	$(PYTEST) $(TEST_DIR)/test_end_to_end.py -v

test-coverage:
	@echo "$(GREEN)Running tests with coverage...$(NC)"
	$(PYTEST) $(TEST_DIR) --cov=$(SRC_DIR) --cov-report=html --cov-report=term

test-performance:
	@echo "$(GREEN)Running performance tests...$(NC)"
	$(PYTHON) $(TEST_DIR)/test_performance.py

# Code quality targets
lint:
	@echo "$(GREEN)Running linters...$(NC)"
	ruff check $(SRC_DIR)
	mypy $(SRC_DIR) --ignore-missing-imports

format:
	@echo "$(GREEN)Formatting code...$(NC)"
	black $(SRC_DIR)
	isort $(SRC_DIR)

check-format:
	@echo "$(GREEN)Checking code format...$(NC)"
	black --check $(SRC_DIR)
	isort --check-only $(SRC_DIR)

# Running targets
run: run-standalone

run-standalone:
	@echo "$(GREEN)Starting OrcaWave MCP Server (Standalone)...$(NC)"
	$(PYTHON) mcp/orcawave/run_server.py --standalone

run-mcp:
	@echo "$(GREEN)Starting OrcaWave MCP Server (FastMCP)...$(NC)"
	$(PYTHON) mcp/orcawave/run_server.py

run-dashboard:
	@echo "$(GREEN)Starting Dashboard Server...$(NC)"
	$(PYTHON) $(CLIENT_DIR)/serve_dashboard.py

run-cli:
	@echo "$(GREEN)Starting CLI Client...$(NC)"
	$(PYTHON) $(CLIENT_DIR)/cli_client.py

run-all:
	@echo "$(GREEN)Starting complete stack...$(NC)"
	@echo "Starting MCP server in background..."
	$(PYTHON) mcp/orcawave/run_server.py --standalone &
	@sleep 3
	@echo "Starting dashboard server in background..."
	$(PYTHON) $(CLIENT_DIR)/serve_dashboard.py &
	@sleep 2
	@echo "$(GREEN)Stack is running!$(NC)"
	@echo "MCP Server: http://localhost:3100"
	@echo "WebSocket: ws://localhost:8765"
	@echo "Dashboard: http://localhost:8080"
	@echo ""
	@echo "Press Ctrl+C to stop all services"
	@wait

quick-start:
	@echo "$(GREEN)Running Quick Start...$(NC)"
	$(PYTHON) $(SRC_DIR)/quick_start.py

# Docker targets
docker-build:
	@echo "$(GREEN)Building Docker images...$(NC)"
	$(DOCKER_COMPOSE) build

docker-up:
	@echo "$(GREEN)Starting Docker containers...$(NC)"
	$(DOCKER_COMPOSE) up -d
	@echo "$(GREEN)Containers started successfully$(NC)"
	@echo "Dashboard: http://localhost:8080"
	@echo "Grafana: http://localhost:3000 (admin/admin)"
	@echo "Jupyter: http://localhost:8888 (token: orcawave)"

docker-down:
	@echo "$(YELLOW)Stopping Docker containers...$(NC)"
	$(DOCKER_COMPOSE) down

docker-logs:
	$(DOCKER_COMPOSE) logs -f

docker-clean:
	@echo "$(YELLOW)Cleaning Docker resources...$(NC)"
	$(DOCKER_COMPOSE) down -v
	$(DOCKER) system prune -f

docker-shell:
	@echo "$(GREEN)Opening shell in MCP container...$(NC)"
	$(DOCKER) exec -it orcawave-mcp-server /bin/bash

# Deployment targets
deploy-staging:
	@echo "$(YELLOW)Deploying to staging...$(NC)"
	@echo "This would deploy to staging server"
	# Add actual deployment commands here

deploy-prod:
	@echo "$(RED)Deploying to production...$(NC)"
	@echo "Are you sure? [y/N] " && read ans && [ $${ans:-N} = y ]
	@echo "This would deploy to production server"
	# Add actual deployment commands here

backup:
	@echo "$(GREEN)Creating backup...$(NC)"
	@mkdir -p backups
	@tar -czf backups/orcawave-backup-$(shell date +%Y%m%d-%H%M%S).tar.gz \
		config.yml \
		$(SRC_DIR)/data \
		$(SRC_DIR)/logs
	@echo "$(GREEN)Backup created in backups/$(NC)"

restore:
	@echo "$(YELLOW)Restoring from backup...$(NC)"
	@echo "Select backup file to restore from backups/"
	# Add restore logic here

# Documentation targets
docs:
	@echo "$(GREEN)Generating documentation...$(NC)"
	sphinx-apidoc -o docs/api $(SRC_DIR)
	cd docs && make html

docs-serve:
	@echo "$(GREEN)Serving documentation...$(NC)"
	$(PYTHON) -m http.server 8000 --directory docs/_build/html

# Monitoring targets
monitor:
	@echo "$(GREEN)Opening monitoring dashboard...$(NC)"
	@echo "Grafana: http://localhost:3000"
	@echo "Prometheus: http://localhost:9090"
	@open http://localhost:3000 || xdg-open http://localhost:3000 || echo "Open browser manually"

logs:
	@echo "$(GREEN)Showing recent logs...$(NC)"
	@tail -n 50 logs/orcawave-mcp.log

# Utility targets
clean:
	@echo "$(YELLOW)Cleaning build artifacts...$(NC)"
	find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -delete
	find . -type f -name "*.pyo" -delete
	find . -type f -name "*.coverage" -delete
	find . -type d -name "*.egg-info" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name ".mypy_cache" -exec rm -rf {} + 2>/dev/null || true
	rm -rf htmlcov/
	rm -rf dist/
	rm -rf build/
	@echo "$(GREEN)Clean completed$(NC)"

version:
	@echo "$(GREEN)OrcaWave MCP Server$(NC)"
	@$(PYTHON) -c "import json; print('Version:', json.load(open('package.json'))['version'])" 2>/dev/null || echo "Version: 1.0.0"
	@echo "Python: $(shell $(PYTHON) --version)"
	@echo "Docker: $(shell $(DOCKER) --version)"

status:
	@echo "$(GREEN)System Status:$(NC)"
	@echo "Checking MCP Server..." 
	@curl -s http://localhost:3100/health > /dev/null && echo "  ✓ MCP Server: Running" || echo "  ✗ MCP Server: Not running"
	@echo "Checking WebSocket..."
	@nc -zv localhost 8765 2>/dev/null && echo "  ✓ WebSocket: Open" || echo "  ✗ WebSocket: Closed"
	@echo "Checking Dashboard..."
	@curl -s http://localhost:8080 > /dev/null && echo "  ✓ Dashboard: Running" || echo "  ✗ Dashboard: Not running"

# Development shortcuts
dev: install-dev format lint test

ci: lint test-coverage

release:
	@echo "$(GREEN)Creating release...$(NC)"
	@echo "Current version: $$(git describe --tags --abbrev=0 2>/dev/null || echo 'v0.0.0')"
	@echo "Enter new version (e.g., v1.0.1): " && read VERSION && \
		git tag -a $$VERSION -m "Release $$VERSION" && \
		git push origin $$VERSION && \
		echo "$(GREEN)Release $$VERSION created$(NC)"

.DEFAULT_GOAL := help