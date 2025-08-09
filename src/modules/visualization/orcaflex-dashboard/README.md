# OrcaFlex Results Visualization Dashboard

A production-ready full-stack application for visualizing and analyzing OrcaFlex simulation results. Built with FastAPI backend and Next.js frontend, designed for marine engineering applications.

[![CI/CD Pipeline](https://github.com/your-org/orcaflex-dashboard/workflows/CI/CD%20Pipeline/badge.svg)](https://github.com/your-org/orcaflex-dashboard/actions)
[![codecov](https://codecov.io/gh/your-org/orcaflex-dashboard/branch/main/graph/badge.svg)](https://codecov.io/gh/your-org/orcaflex-dashboard)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## 🚀 Features

### Core Functionality
- **Analysis Management**: Create, run, and monitor OrcaFlex analyses
- **Real-time Monitoring**: Track analysis progress with live updates
- **Data Visualization**: Interactive charts using Plotly.js and Recharts
- **File Management**: Upload and manage OrcaFlex simulation files
- **Results Export**: Export data in CSV, JSON, and Excel formats
- **Statistical Analysis**: Comprehensive statistical summaries

### Technical Features
- **RESTful API**: FastAPI with automatic OpenAPI documentation
- **Type Safety**: Full TypeScript support throughout the stack
- **Real-time Updates**: WebSocket support for live analysis monitoring
- **Caching**: Redis integration for improved performance
- **Database**: PostgreSQL with SQLAlchemy ORM
- **Authentication**: JWT-based authentication system
- **Monitoring**: Prometheus metrics and Grafana dashboards
- **Container Ready**: Docker and docker-compose configurations

## 🏗️ Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Frontend      │    │    Backend      │    │   Database      │
│   (Next.js)     │◄──►│   (FastAPI)     │◄──►│ (PostgreSQL)    │
│   Port: 3000    │    │   Port: 8000    │    │   Port: 5432    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                        │                        │
         │                        ▼                        │
         │              ┌─────────────────┐                │
         │              │     Redis       │                │
         │              │   (Cache)       │                │
         │              │   Port: 6379    │                │
         │              └─────────────────┘                │
         │                                                 │
         ▼                                                 ▼
┌─────────────────┐                            ┌─────────────────┐
│   Monitoring    │                            │  File Storage   │
│ (Prometheus)    │                            │   (Volumes)     │
│   Port: 9090    │                            │                 │
└─────────────────┘                            └─────────────────┘
```

## 📋 Prerequisites

- **Docker**: 20.10+ and Docker Compose 2.0+
- **Node.js**: 18+ (for local development)
- **Python**: 3.11+ (for local development)
- **OrcaFlex**: Valid license (for production analysis)

## 🚀 Quick Start

### Development Setup

1. **Clone the repository**
   ```bash
   git clone https://github.com/your-org/orcaflex-dashboard.git
   cd orcaflex-dashboard
   ```

2. **Set up environment files**
   ```bash
   make env-setup
   ```

3. **Start development environment**
   ```bash
   make dev
   ```

4. **Access the applications**
   - Frontend: http://localhost:3000
   - Backend API: http://localhost:8000
   - API Documentation: http://localhost:8000/api/docs
   - Interactive API: http://localhost:8000/api/redoc

### Production Deployment

1. **Configure environment variables**
   ```bash
   cp backend/.env.example backend/.env
   cp frontend/.env.example frontend/.env
   # Edit the files with production values
   ```

2. **Start production stack**
   ```bash
   make prod-start
   ```

3. **Enable monitoring (optional)**
   ```bash
   make monitor
   ```

## 🛠️ Development

### Available Commands

```bash
# Development
make dev          # Start development environment
make stop         # Stop all services
make restart      # Restart all services
make logs         # View logs

# Testing
make test         # Run all tests
make test-backend # Run backend tests only
make test-frontend# Run frontend tests only

# Code Quality
make lint         # Run all linting
make format       # Format all code
make security-scan# Run security scans

# Database
make db-init      # Initialize database
make db-migrate   # Run migrations
make db-reset     # Reset database

# Utilities
make health       # Check application health
make ps           # Show running containers
make clean        # Clean up containers and volumes
```

### Backend Development

The backend is built with FastAPI and follows modern Python practices:

```bash
cd backend

# Install dependencies
pip install -r requirements.txt

# Run development server
uvicorn app.main:app --reload --host 0.0.0.0 --port 8000

# Run tests
pytest --cov=app

# Lint code
flake8 app
mypy app --ignore-missing-imports
black app
```

### Frontend Development

The frontend is built with Next.js 14 and TypeScript:

```bash
cd frontend

# Install dependencies
npm install

# Run development server
npm run dev

# Run tests
npm test

# Lint code
npm run lint
npm run type-check
```

## 📊 API Documentation

The API is automatically documented using FastAPI's built-in support for OpenAPI:

- **Swagger UI**: http://localhost:8000/api/docs
- **ReDoc**: http://localhost:8000/api/redoc
- **OpenAPI JSON**: http://localhost:8000/api/openapi.json

### Key Endpoints

```
POST   /api/analysis/           # Create new analysis
GET    /api/analysis/           # List analyses
GET    /api/analysis/{id}       # Get analysis details
POST   /api/analysis/{id}/run   # Run analysis
GET    /api/analysis/{id}/progress # Get analysis progress
GET    /api/analysis/{id}/results  # Get analysis results

GET    /api/results/            # List results
GET    /api/results/{id}        # Get result details
POST   /api/results/{id}/query  # Query result data
GET    /api/results/{id}/export # Export result data

GET    /api/health/             # Health check
GET    /api/health/detailed     # Detailed health info
```

## 🔧 Configuration

### Environment Variables

**Backend (.env)**:
```bash
ORCAFLEX_ENVIRONMENT=development
ORCAFLEX_DATABASE_URL=postgresql://user:pass@localhost:5432/db
ORCAFLEX_REDIS_URL=redis://localhost:6379/0
ORCAFLEX_SECRET_KEY=your-secret-key
ORCAFLEX_CORS_ORIGINS=http://localhost:3000
```

**Frontend (.env.local)**:
```bash
NEXT_PUBLIC_API_BASE_URL=http://localhost:8000
```

### OrcaFlex Configuration

For production use, configure OrcaFlex settings:

```bash
ORCAFLEX_LICENSE_PATH=/path/to/orcaflex/license
ORCAFLEX_TEMP_DIR=/tmp/orcaflex
ORCAFLEX_MAX_FILE_SIZE=104857600  # 100MB
```

## 📊 Monitoring

The application includes comprehensive monitoring:

### Metrics (Prometheus)
- Application performance metrics
- Database connection stats
- API request/response metrics
- Custom business metrics

### Dashboards (Grafana)
- System overview dashboard
- API performance dashboard
- Database metrics dashboard
- Custom analysis dashboards

Access monitoring:
- Prometheus: http://localhost:9090
- Grafana: http://localhost:3001 (admin/admin)

## 🧪 Testing

### Backend Testing
```bash
# Unit tests
pytest

# Coverage report
pytest --cov=app --cov-report=html

# Integration tests
pytest tests/integration/

# Load tests
locust -f tests/load/locustfile.py
```

### Frontend Testing
```bash
# Unit tests
npm test

# E2E tests
npm run test:e2e

# Visual regression tests
npm run test:visual
```

## 🚢 Deployment

### Docker Compose (Recommended)

```bash
# Development
docker-compose up -d

# Production
docker-compose -f docker-compose.prod.yml up -d
```

### Kubernetes

```bash
# Deploy to Kubernetes
kubectl apply -f k8s/

# Check status
kubectl get pods -n orcaflex
```

### Cloud Deployment

The application is designed to work with major cloud providers:

- **AWS**: ECS/EKS with RDS and ElastiCache
- **GCP**: Cloud Run with Cloud SQL and Memorystore
- **Azure**: Container Instances with Azure Database

## 🔒 Security

### Security Features
- JWT-based authentication
- CORS configuration
- Request rate limiting
- Input validation and sanitization
- SQL injection prevention
- XSS protection headers

### Security Scanning
```bash
# Run security scans
make security-scan

# Dependency vulnerability check
npm audit
pip-audit
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Commit your changes: `git commit -m 'Add amazing feature'`
4. Push to the branch: `git push origin feature/amazing-feature`
5. Open a Pull Request

### Code Standards
- Follow PEP 8 for Python code
- Use ESLint/Prettier for TypeScript/JavaScript
- Write tests for new features
- Update documentation for API changes

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 📞 Support

- 📧 Email: support@yourcompany.com
- 📖 Documentation: https://docs.yourcompany.com/orcaflex-dashboard
- 🐛 Issues: https://github.com/your-org/orcaflex-dashboard/issues
- 💬 Discussions: https://github.com/your-org/orcaflex-dashboard/discussions

## 🙏 Acknowledgments

- [FastAPI](https://fastapi.tiangolo.com/) for the excellent Python web framework
- [Next.js](https://nextjs.org/) for the React framework
- [Material-UI](https://mui.com/) for the React components
- [Plotly.js](https://plotly.com/javascript/) for data visualization
- [OrcaFlex](https://www.orcina.com/orcaflex/) for marine engineering analysis

---

**Built with ❤️ for the marine engineering community**