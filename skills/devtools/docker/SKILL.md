---
name: docker
version: 1.0.0
description: Complete Docker containerization patterns for development and production workflows
author: workspace-hub
category: devtools
capabilities:
  - Dockerfile best practices and multi-stage builds
  - Docker Compose orchestration and networking
  - Volume management and data persistence
  - Development vs production configurations
  - Container debugging and optimization
  - Registry management and image distribution
tools:
  - docker
  - docker-compose
  - buildx
  - dive
  - hadolint
tags: [docker, containers, devops, orchestration, microservices, compose]
platforms: [linux, macos, windows]
related_skills:
  - cli-productivity
  - git-advanced
  - kubernetes
---

# Docker Containerization Skill

Master Docker containerization for consistent, reproducible development and production environments. This skill covers Dockerfile best practices, multi-stage builds, Docker Compose orchestration, and production-ready configurations.

## When to Use This Skill

### USE when:
- Building reproducible development environments
- Creating consistent CI/CD pipelines
- Deploying microservices architectures
- Isolating application dependencies
- Packaging applications for distribution
- Setting up local development with multiple services
- Need portable environments across teams

### DON'T USE when:
- Simple scripts that don't need isolation
- Applications that require direct hardware access
- Environments where containers aren't permitted
- Tasks better suited for virtual machines (full OS isolation)
- When simpler alternatives like venv suffice

## Prerequisites

### Installation

**Linux (Ubuntu/Debian):**
```bash
# Install Docker Engine
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Add user to docker group
sudo usermod -aG docker $USER
newgrp docker

# Install Docker Compose plugin
sudo apt-get update
sudo apt-get install docker-compose-plugin

# Verify installation
docker --version
docker compose version
```

**macOS:**
```bash
# Install Docker Desktop
brew install --cask docker

# Or download from https://www.docker.com/products/docker-desktop

# Verify installation
docker --version
docker compose version
```

**Windows:**
```powershell
# Install Docker Desktop from https://www.docker.com/products/docker-desktop
# Enable WSL 2 backend for best performance

# Verify installation
docker --version
docker compose version
```

**Additional Tools:**
```bash
# Dockerfile linter
brew install hadolint  # macOS
# Or: docker run --rm -i hadolint/hadolint < Dockerfile

# Image analyzer (inspect layers)
brew install dive  # macOS
# Or: docker run --rm -it wagoodman/dive:latest <image>

# Build with BuildKit (enhanced features)
export DOCKER_BUILDKIT=1
```

## Core Capabilities

### 1. Basic Dockerfile Patterns

**Simple Application Dockerfile:**
```dockerfile
# Base image with specific version
FROM python:3.12-slim

# Set working directory
WORKDIR /app

# Set environment variables
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy dependency files first (better caching)
COPY requirements.txt .

# Install Python dependencies
RUN pip install --no-cache-dir -r requirements.txt

# Copy application code
COPY . .

# Create non-root user
RUN useradd --create-home appuser && chown -R appuser:appuser /app
USER appuser

# Expose port
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8000/health || exit 1

# Default command
CMD ["python", "main.py"]
```

**Node.js Application Dockerfile:**
```dockerfile
FROM node:20-alpine

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy application
COPY . .

# Non-root user (alpine already has 'node' user)
USER node

EXPOSE 3000

HEALTHCHECK --interval=30s --timeout=3s \
    CMD wget --no-verbose --tries=1 --spider http://localhost:3000/health || exit 1

CMD ["node", "server.js"]
```

### 2. Multi-Stage Builds

**Python Multi-Stage Build:**
```dockerfile
# Stage 1: Build dependencies
FROM python:3.12-slim AS builder

WORKDIR /app

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Create virtual environment
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Install dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Stage 2: Production image
FROM python:3.12-slim AS production

WORKDIR /app

# Install runtime dependencies only
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Copy virtual environment from builder
COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Copy application code
COPY . .

# Create non-root user
RUN useradd --create-home --shell /bin/bash appuser \
    && chown -R appuser:appuser /app
USER appuser

EXPOSE 8000

HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:8000/health || exit 1

CMD ["gunicorn", "--bind", "0.0.0.0:8000", "app:app"]
```

**Node.js Multi-Stage Build:**
```dockerfile
# Stage 1: Install dependencies
FROM node:20-alpine AS deps

WORKDIR /app

COPY package*.json ./
RUN npm ci

# Stage 2: Build application
FROM node:20-alpine AS builder

WORKDIR /app

COPY --from=deps /app/node_modules ./node_modules
COPY . .

RUN npm run build

# Stage 3: Production image
FROM node:20-alpine AS production

WORKDIR /app

ENV NODE_ENV=production

# Copy only production dependencies
COPY --from=deps /app/node_modules ./node_modules
COPY --from=builder /app/dist ./dist
COPY --from=builder /app/package*.json ./

USER node

EXPOSE 3000

CMD ["node", "dist/server.js"]
```

**Go Multi-Stage Build (minimal image):**
```dockerfile
# Stage 1: Build
FROM golang:1.22-alpine AS builder

WORKDIR /app

# Download dependencies
COPY go.mod go.sum ./
RUN go mod download

# Copy source and build
COPY . .
RUN CGO_ENABLED=0 GOOS=linux go build -ldflags="-w -s" -o /app/main .

# Stage 2: Minimal production image
FROM scratch

# Copy SSL certificates for HTTPS
COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/

# Copy binary
COPY --from=builder /app/main /main

EXPOSE 8080

ENTRYPOINT ["/main"]
```

### 3. Docker Compose for Development

**Full Stack Development Environment:**
```yaml
# docker-compose.yml
version: '3.8'

services:
  # Application service
  app:
    build:
      context: .
      dockerfile: Dockerfile
      target: builder  # Use builder stage for development
    volumes:
      - .:/app
      - /app/node_modules  # Exclude node_modules from bind mount
    ports:
      - "3000:3000"
    environment:
      - NODE_ENV=development
      - DATABASE_URL=postgres://devuser:devpass@db:5432/devdb
      - REDIS_URL=redis://redis:6379
    depends_on:
      db:
        condition: service_healthy
      redis:
        condition: service_started
    command: npm run dev
    networks:
      - app-network

  # Database service
  db:
    image: postgres:16-alpine
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ./scripts/init.sql:/docker-entrypoint-initdb.d/init.sql
    environment:
      POSTGRES_DB: devdb
      POSTGRES_USER: devuser
      POSTGRES_PASSWORD: devpass
    ports:
      - "5432:5432"
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U devuser -d devdb"]
      interval: 5s
      timeout: 5s
      retries: 5
    networks:
      - app-network

  # Redis cache
  redis:
    image: redis:7-alpine
    volumes:
      - redis_data:/data
    ports:
      - "6379:6379"
    command: redis-server --appendonly yes
    networks:
      - app-network

  # Adminer for database management
  adminer:
    image: adminer:latest
    ports:
      - "8080:8080"
    depends_on:
      - db
    networks:
      - app-network

  # Nginx reverse proxy
  nginx:
    image: nginx:alpine
    volumes:
      - ./nginx/nginx.conf:/etc/nginx/nginx.conf:ro
      - ./nginx/conf.d:/etc/nginx/conf.d:ro
    ports:
      - "80:80"
      - "443:443"
    depends_on:
      - app
    networks:
      - app-network

networks:
  app-network:
    driver: bridge

volumes:
  postgres_data:
  redis_data:
```

**Development Override File:**
```yaml
# docker-compose.override.yml (automatically applied)
version: '3.8'

services:
  app:
    build:
      target: builder
    volumes:
      - .:/app
      - /app/node_modules
    environment:
      - DEBUG=true
      - LOG_LEVEL=debug
    command: npm run dev:watch

  db:
    ports:
      - "5432:5432"  # Expose for local tools

  redis:
    ports:
      - "6379:6379"  # Expose for local tools
```

**Production Compose File:**
```yaml
# docker-compose.prod.yml
version: '3.8'

services:
  app:
    build:
      context: .
      dockerfile: Dockerfile
      target: production
    restart: always
    environment:
      - NODE_ENV=production
      - DATABASE_URL=${DATABASE_URL}
      - REDIS_URL=${REDIS_URL}
    deploy:
      replicas: 3
      resources:
        limits:
          cpus: '0.5'
          memory: 512M
        reservations:
          cpus: '0.25'
          memory: 256M
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:3000/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s
    logging:
      driver: "json-file"
      options:
        max-size: "10m"
        max-file: "3"

  db:
    restart: always
    volumes:
      - postgres_data:/var/lib/postgresql/data
    deploy:
      resources:
        limits:
          cpus: '1'
          memory: 1G
```

### 4. Networking Patterns

**Custom Network Configuration:**
```yaml
version: '3.8'

services:
  frontend:
    build: ./frontend
    networks:
      - frontend-network

  backend:
    build: ./backend
    networks:
      - frontend-network
      - backend-network

  db:
    image: postgres:16-alpine
    networks:
      - backend-network

networks:
  frontend-network:
    driver: bridge
  backend-network:
    driver: bridge
    internal: true  # No external access
```

**Network Commands:**
```bash
# List networks
docker network ls

# Inspect network
docker network inspect app-network

# Create custom network
docker network create --driver bridge my-network

# Connect container to network
docker network connect my-network container-name

# Disconnect container
docker network disconnect my-network container-name
```

### 5. Volume Management

**Volume Types and Usage:**
```yaml
version: '3.8'

services:
  app:
    image: myapp:latest
    volumes:
      # Named volume (managed by Docker)
      - app_data:/app/data

      # Bind mount (host directory)
      - ./config:/app/config:ro

      # Anonymous volume (for excluding from bind mount)
      - /app/node_modules

      # tmpfs mount (in-memory)
      - type: tmpfs
        target: /app/tmp
        tmpfs:
          size: 100M

volumes:
  app_data:
    driver: local
    driver_opts:
      type: none
      device: /data/app
      o: bind
```

**Volume Commands:**
```bash
# List volumes
docker volume ls

# Create volume
docker volume create my-volume

# Inspect volume
docker volume inspect my-volume

# Remove unused volumes
docker volume prune

# Backup volume
docker run --rm -v my-volume:/data -v $(pwd):/backup alpine \
    tar czf /backup/volume-backup.tar.gz -C /data .

# Restore volume
docker run --rm -v my-volume:/data -v $(pwd):/backup alpine \
    tar xzf /backup/volume-backup.tar.gz -C /data
```

### 6. Development Workflow Scripts

**Makefile for Docker Operations:**
```makefile
# Makefile
.PHONY: build up down logs shell test clean

# Variables
COMPOSE := docker compose
PROJECT := myapp

# Build images
build:
	$(COMPOSE) build --no-cache

# Start services
up:
	$(COMPOSE) up -d

# Start with logs
up-logs:
	$(COMPOSE) up

# Stop services
down:
	$(COMPOSE) down

# Stop and remove volumes
down-clean:
	$(COMPOSE) down -v --remove-orphans

# View logs
logs:
	$(COMPOSE) logs -f

# Logs for specific service
logs-%:
	$(COMPOSE) logs -f $*

# Shell into app container
shell:
	$(COMPOSE) exec app sh

# Run tests
test:
	$(COMPOSE) exec app npm test

# Lint Dockerfiles
lint:
	hadolint Dockerfile
	hadolint Dockerfile.prod

# Analyze image
analyze:
	dive $(PROJECT):latest

# Clean up
clean:
	docker system prune -f
	docker volume prune -f

# Production build and push
prod-build:
	docker build -t $(PROJECT):latest -f Dockerfile.prod .

prod-push:
	docker push $(PROJECT):latest
```

**Development Helper Script:**
```bash
#!/bin/bash
# scripts/docker-dev.sh
# ABOUTME: Docker development helper script
# ABOUTME: Provides common Docker operations for development

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $*"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*" >&2; }

# Commands
case "$1" in
    start)
        log_info "Starting development environment..."
        docker compose up -d
        log_info "Services started. Run 'docker compose logs -f' to view logs."
        ;;
    stop)
        log_info "Stopping development environment..."
        docker compose down
        ;;
    restart)
        log_info "Restarting services..."
        docker compose restart
        ;;
    rebuild)
        log_info "Rebuilding images..."
        docker compose build --no-cache
        docker compose up -d
        ;;
    logs)
        docker compose logs -f "${2:-}"
        ;;
    shell)
        SERVICE="${2:-app}"
        log_info "Opening shell in $SERVICE..."
        docker compose exec "$SERVICE" sh
        ;;
    db)
        log_info "Connecting to database..."
        docker compose exec db psql -U devuser -d devdb
        ;;
    reset-db)
        log_warn "This will delete all data. Continue? (y/N)"
        read -r response
        if [[ "$response" =~ ^[Yy]$ ]]; then
            docker compose down -v
            docker compose up -d db
            log_info "Database reset complete."
        fi
        ;;
    clean)
        log_warn "Cleaning up Docker resources..."
        docker compose down -v --remove-orphans
        docker system prune -f
        ;;
    status)
        docker compose ps
        ;;
    *)
        echo "Usage: $0 {start|stop|restart|rebuild|logs|shell|db|reset-db|clean|status}"
        echo ""
        echo "Commands:"
        echo "  start     - Start all services"
        echo "  stop      - Stop all services"
        echo "  restart   - Restart all services"
        echo "  rebuild   - Rebuild images and restart"
        echo "  logs      - View logs (optional: service name)"
        echo "  shell     - Open shell in container (default: app)"
        echo "  db        - Connect to database"
        echo "  reset-db  - Reset database (deletes all data)"
        echo "  clean     - Clean up all Docker resources"
        echo "  status    - Show service status"
        exit 1
        ;;
esac
```

## Integration Examples

### 1. CI/CD Pipeline Integration

**GitHub Actions Workflow:**
```yaml
# .github/workflows/docker.yml
name: Docker Build and Push

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

env:
  REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Lint Dockerfile
        uses: hadolint/hadolint-action@v3.1.0
        with:
          dockerfile: Dockerfile

  build:
    runs-on: ubuntu-latest
    needs: lint
    permissions:
      contents: read
      packages: write

    steps:
      - uses: actions/checkout@v4

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Log in to Container Registry
        if: github.event_name != 'pull_request'
        uses: docker/login-action@v3
        with:
          registry: ${{ env.REGISTRY }}
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Extract metadata
        id: meta
        uses: docker/metadata-action@v5
        with:
          images: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}
          tags: |
            type=ref,event=branch
            type=ref,event=pr
            type=sha,prefix=
            type=raw,value=latest,enable=${{ github.ref == 'refs/heads/main' }}

      - name: Build and push
        uses: docker/build-push-action@v5
        with:
          context: .
          push: ${{ github.event_name != 'pull_request' }}
          tags: ${{ steps.meta.outputs.tags }}
          labels: ${{ steps.meta.outputs.labels }}
          cache-from: type=gha
          cache-to: type=gha,mode=max
          platforms: linux/amd64,linux/arm64
```

### 2. Local Development with Hot Reload

**Development Dockerfile:**
```dockerfile
# Dockerfile.dev
FROM node:20-alpine

WORKDIR /app

# Install development dependencies
RUN apk add --no-cache git

# Install nodemon globally for hot reload
RUN npm install -g nodemon

# Copy package files
COPY package*.json ./

# Install all dependencies (including devDependencies)
RUN npm install

# Don't copy source - use volume mount instead
# Source will be mounted at runtime

EXPOSE 3000

# Use nodemon for hot reload
CMD ["nodemon", "--watch", "src", "--ext", "js,ts,json", "src/index.js"]
```

**Development Compose:**
```yaml
# docker-compose.dev.yml
version: '3.8'

services:
  app:
    build:
      context: .
      dockerfile: Dockerfile.dev
    volumes:
      - ./src:/app/src
      - ./package.json:/app/package.json
    ports:
      - "3000:3000"
      - "9229:9229"  # Debug port
    environment:
      - NODE_ENV=development
      - DEBUG=app:*
    command: nodemon --inspect=0.0.0.0:9229 src/index.js
```

### 3. Multi-Environment Configuration

**Environment-Specific Compose Files:**
```bash
# Directory structure
docker/
├── docker-compose.yml          # Base configuration
├── docker-compose.dev.yml      # Development overrides
├── docker-compose.test.yml     # Test environment
├── docker-compose.prod.yml     # Production configuration
└── .env.example                # Environment template
```

**Usage:**
```bash
# Development
docker compose -f docker-compose.yml -f docker-compose.dev.yml up

# Testing
docker compose -f docker-compose.yml -f docker-compose.test.yml up

# Production
docker compose -f docker-compose.yml -f docker-compose.prod.yml up -d
```

### 4. Database Migration Pattern

**Migration Service:**
```yaml
# docker-compose.yml
services:
  migrate:
    build:
      context: .
      dockerfile: Dockerfile
    command: npm run migrate
    environment:
      - DATABASE_URL=postgres://user:pass@db:5432/mydb
    depends_on:
      db:
        condition: service_healthy
    profiles:
      - migrate  # Only run when explicitly requested

  seed:
    build:
      context: .
      dockerfile: Dockerfile
    command: npm run seed
    environment:
      - DATABASE_URL=postgres://user:pass@db:5432/mydb
    depends_on:
      - migrate
    profiles:
      - seed
```

**Usage:**
```bash
# Run migrations
docker compose --profile migrate up migrate

# Run migrations and seed
docker compose --profile migrate --profile seed up
```

## Best Practices

### 1. Image Optimization

```dockerfile
# Use specific versions
FROM python:3.12.1-slim  # Not :latest

# Combine RUN commands to reduce layers
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        gcc \
        libpq-dev \
    && rm -rf /var/lib/apt/lists/* \
    && pip install --no-cache-dir -r requirements.txt

# Use .dockerignore
# .dockerignore
.git
.gitignore
node_modules
npm-debug.log
Dockerfile*
docker-compose*
.dockerignore
.env*
*.md
.pytest_cache
__pycache__
*.pyc
.coverage
htmlcov
```

### 2. Security Best Practices

```dockerfile
# Run as non-root user
RUN useradd --create-home --shell /bin/bash appuser
USER appuser

# Don't store secrets in images
# Use environment variables or secrets management

# Scan images for vulnerabilities
# docker scan myimage:latest

# Use read-only filesystem where possible
# docker run --read-only myimage
```

### 3. Layer Caching Strategy

```dockerfile
# Order from least to most frequently changed
FROM node:20-alpine

# 1. System dependencies (rarely change)
RUN apk add --no-cache git

# 2. Package manifests (change sometimes)
COPY package*.json ./
RUN npm ci

# 3. Application code (changes often)
COPY . .

# 4. Build step
RUN npm run build
```

### 4. Health Checks

```dockerfile
# HTTP health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:8000/health || exit 1

# TCP health check
HEALTHCHECK --interval=30s --timeout=3s \
    CMD nc -z localhost 5432 || exit 1

# Custom script
HEALTHCHECK --interval=30s --timeout=10s \
    CMD /app/healthcheck.sh || exit 1
```

### 5. Logging Best Practices

```yaml
services:
  app:
    logging:
      driver: "json-file"
      options:
        max-size: "10m"
        max-file: "3"
        labels: "service,environment"
        env: "NODE_ENV"
```

## Troubleshooting

### Common Issues and Solutions

**Container won't start:**
```bash
# Check logs
docker logs container-name

# Check container status
docker inspect container-name

# Run interactively to debug
docker run -it --entrypoint sh image-name
```

**Permission denied errors:**
```bash
# Fix file ownership
docker run --rm -v $(pwd):/app alpine chown -R $(id -u):$(id -g) /app

# Or use user namespace remapping
```

**Out of disk space:**
```bash
# Clean up unused resources
docker system prune -a --volumes

# Check disk usage
docker system df
```

**Slow builds:**
```bash
# Enable BuildKit
export DOCKER_BUILDKIT=1

# Use cache mounts
RUN --mount=type=cache,target=/root/.cache/pip pip install -r requirements.txt
```

**Network connectivity issues:**
```bash
# Check network
docker network inspect bridge

# Test connectivity
docker exec container-name ping other-container

# Check DNS resolution
docker exec container-name nslookup service-name
```

### Debug Commands

```bash
# Shell into running container
docker exec -it container-name sh

# Copy files from container
docker cp container-name:/app/logs ./logs

# View container processes
docker top container-name

# Monitor resource usage
docker stats

# View container changes
docker diff container-name

# Export container filesystem
docker export container-name > container.tar
```

## Version History

- **1.0.0** (2026-01-17): Initial release
  - Dockerfile best practices and multi-stage builds
  - Docker Compose orchestration patterns
  - Development and production configurations
  - CI/CD integration examples
  - Networking and volume management
  - Troubleshooting guide

---

**Use this skill to build consistent, reproducible containerized environments across development, testing, and production!**
