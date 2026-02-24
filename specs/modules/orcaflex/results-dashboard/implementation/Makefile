# OrcaFlex Dashboard Makefile

.PHONY: help install dev build test clean docker-up docker-down docker-logs

help: ## Show this help message
	@echo "OrcaFlex Dashboard - Available Commands"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

install: ## Install all dependencies
	cd src/backend && pip install -e .
	cd src/frontend && npm install

dev: ## Start development servers
	@echo "Starting development servers..."
	@make -j 2 dev-backend dev-frontend

dev-backend: ## Start backend development server
	cd src/backend && uvicorn main:app --reload --host 0.0.0.0 --port 8000

dev-frontend: ## Start frontend development server
	cd src/frontend && npm run dev

build: ## Build production images
	docker-compose build

test: ## Run all tests
	@make test-backend
	@make test-frontend

test-backend: ## Run backend tests
	cd src/backend && pytest

test-frontend: ## Run frontend tests
	cd src/frontend && npm test

lint: ## Run linters
	cd src/backend && ruff check . && mypy .
	cd src/frontend && npm run lint

format: ## Format code
	cd src/backend && black . && isort .
	cd src/frontend && npm run format

docker-up: ## Start Docker services
	docker-compose up -d

docker-down: ## Stop Docker services
	docker-compose down

docker-logs: ## View Docker logs
	docker-compose logs -f

docker-restart: ## Restart Docker services
	docker-compose restart

migrate: ## Run database migrations
	docker-compose exec backend alembic upgrade head

clean: ## Clean build artifacts
	find . -type d -name "__pycache__" -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete
	rm -rf src/frontend/node_modules
	rm -rf src/frontend/build
	rm -rf src/backend/*.egg-info

setup: ## Complete setup from scratch
	@make install
	@make docker-up
	@sleep 10
	@make migrate
	@echo "Setup complete! Access the app at http://localhost:3000"