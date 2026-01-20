# Docker Containerization Skill

> **Quick Reference Guide**

## Overview

Docker containerization patterns for development and production workflows including Dockerfile best practices, multi-stage builds, and Docker Compose orchestration.

**Version**: 1.0.0 | **Category**: devtools | **Platforms**: Linux, macOS, Windows

## Quick Start

```bash
# Build image
docker build -t myapp:latest .

# Run container
docker run -d -p 3000:3000 myapp:latest

# Start development environment
docker compose up -d
```

## Key Features

- Multi-stage builds for optimized images
- Docker Compose for multi-service orchestration
- Development vs production configurations
- Volume management and networking
- CI/CD integration patterns

## Common Commands

```bash
docker compose up -d          # Start services
docker compose logs -f        # View logs
docker compose exec app sh    # Shell access
docker compose down           # Stop services
docker build --no-cache .     # Fresh build
docker system prune -a        # Cleanup
```

## File Structure

```
project/
├── Dockerfile
├── docker-compose.yml
├── docker-compose.dev.yml
└── .dockerignore
```

## Related Skills

- **cli-productivity** - Shell efficiency patterns
- **git-advanced** - Version control workflows

---

For complete documentation, see [SKILL.md](./SKILL.md).
