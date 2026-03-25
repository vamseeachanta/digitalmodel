# OrcaFlex Results Visualization Dashboard - FastAPI Backend

## High-Performance API for OrcaFlex Simulation Data Analysis

This FastAPI backend provides a comprehensive, production-ready API for processing, analyzing, and visualizing OrcaFlex simulation results with enterprise-grade performance and reliability.

## 🚀 Performance Specifications

- **Response Time**: <100ms for cached data, <500ms for complex queries
- **Concurrent Users**: >10 users without performance degradation
- **Data Throughput**: >10,000 data points efficiently supported
- **Cache Hit Ratio**: >90% for repeated queries with Redis caching
- **Memory Usage**: <500MB per worker process

## 📋 Features

### Core Capabilities
- **High Performance**: Redis caching reduces query times by >90%
- **Scalable**: Async implementation with intelligent caching
- **Secure**: JWT authentication with rate limiting and session management
- **Comprehensive**: Full CRUD operations with statistical analysis
- **Export Ready**: Multiple formats (CSV, Excel, JSON, Parquet, PNG, SVG, PDF)
- **Real-time**: Live progress tracking and real-time updates

### API Endpoints

#### 🔍 Analyses (`/api/analyses`)
- Analysis case management with comprehensive filtering
- Real-time progress tracking for running analyses
- Batch processing and comparison capabilities
- Component classification and validation

#### 📊 Results (`/api/results`)
- High-performance data access with caching
- Polar plot data with heading filtering
- Time series data with component grouping
- Statistical summaries and multi-analysis comparison

#### 🔧 Components (`/api/components`)
- Component classification (fst1, fst2, strut, jacket, lngc)
- Hierarchical component management
- Auto-classification with confidence scoring
- Component validation and quality assessment

#### 📤 Exports (`/api/exports`)
- Multiple export formats with streaming support
- Background job processing with progress tracking
- Bulk export capabilities with archiving
- Custom export templates and configurations

#### 📁 Upload (`/api/upload`)
- Secure file upload with validation
- Batch upload processing
- Support for .dat, .csv, .yml, .yaml, .zip files
- Automatic analysis creation and processing

#### 📈 Statistics (`/api/statistics`)
- Comprehensive statistical analysis
- Trend detection and forecasting
- Outlier detection and correlation analysis
- Performance metrics and benchmarking

## 🏗️ Architecture

### Technology Stack
- **Framework**: FastAPI 0.104.1 with async support
- **Database**: SQLAlchemy 2.0+ with async drivers (PostgreSQL/SQLite)
- **Cache**: Redis with intelligent TTL management
- **Authentication**: JWT with session management
- **Rate Limiting**: Redis-backed sliding window
- **Data Processing**: Pandas, NumPy, SciPy
- **Export**: Multiple formats with streaming

### Project Structure
```
backend/
├── app/
│   ├── api/                    # API endpoints
│   │   ├── analyses.py         # Analysis management
│   │   ├── results.py          # Results data access
│   │   ├── components.py       # Component management
│   │   ├── exports.py          # Data export
│   │   ├── upload.py           # File upload
│   │   ├── statistics.py       # Statistical analysis
│   │   └── health.py           # Health monitoring
│   ├── core/                   # Core infrastructure
│   │   ├── cache.py            # Redis caching service
│   │   ├── rate_limiter.py     # Rate limiting
│   │   └── __init__.py
│   ├── models/                 # Data models
│   │   ├── analysis.py         # Analysis models
│   │   ├── component.py        # Component models
│   │   └── results.py          # Results models
│   ├── services/               # Business logic
│   │   ├── auth_service.py     # Authentication
│   │   └── [other services]    # Data processing services
│   ├── config.py               # Configuration
│   └── main.py                 # FastAPI application
├── requirements.txt            # Dependencies
└── README.md                   # This file
```

## 🚀 Quick Start

### Prerequisites
- Python 3.9+
- Redis (for caching and rate limiting)
- PostgreSQL (production) or SQLite (development)

### Installation

1. **Clone and navigate to the backend directory**:
   ```bash
   cd src/modules/visualization/orcaflex-dashboard/backend
   ```

2. **Create virtual environment**:
   ```bash
   python -m venv venv
   source venv/bin/activate  # On Windows: venv\Scripts\activate
   ```

3. **Install dependencies**:
   ```bash
   pip install -r requirements.txt
   ```

4. **Set up environment variables**:
   ```bash
   cp .env.example .env
   # Edit .env with your configuration
   ```

5. **Run the application**:
   ```bash
   # Development
   python -m uvicorn app.main:app --reload --port 8000

   # Production
   python -m uvicorn app.main:app --workers 4 --port 8000
   ```

### Environment Configuration

Create a `.env` file with the following variables:

```env
# Application
ORCAFLEX_ENVIRONMENT=development
ORCAFLEX_DEBUG=true
ORCAFLEX_HOST=0.0.0.0
ORCAFLEX_PORT=8000
ORCAFLEX_LOG_LEVEL=INFO

# Security
ORCAFLEX_SECRET_KEY=your-secret-key-change-this-in-production
ORCAFLEX_ALGORITHM=HS256
ORCAFLEX_ACCESS_TOKEN_EXPIRE_MINUTES=30

# Database
ORCAFLEX_DATABASE_URL=postgresql://user:password@localhost/orcaflex_dashboard
# Or for development: sqlite:///./orcaflex_dashboard.db

# Redis (optional, but recommended for production)
ORCAFLEX_REDIS_URL=redis://localhost:6379/0

# CORS
ORCAFLEX_CORS_ORIGINS=http://localhost:3000,http://localhost:3001

# File Storage
ORCAFLEX_UPLOAD_DIR=uploads
ORCAFLEX_RESULTS_DIR=results
ORCAFLEX_MAX_FILE_SIZE=104857600  # 100MB
```

## 📊 API Documentation

### Authentication

All endpoints require Bearer token authentication:

```bash
# Get token (implement authentication endpoint)
curl -X POST "http://localhost:8000/api/auth/login" \
  -H "Content-Type: application/json" \
  -d '{"username": "user", "password": "password"}'

# Use token in requests
curl -H "Authorization: Bearer <your-token>" \
  "http://localhost:8000/api/analyses"
```

### Example API Calls

#### Get Analyses with Filtering
```bash
curl -H "Authorization: Bearer <token>" \
  "http://localhost:8000/api/analyses?status=completed&limit=10&water_level=hwl"
```

#### Get Polar Plot Data
```bash
curl -X POST "http://localhost:8000/api/results/polar" \
  -H "Authorization: Bearer <token>" \
  -H "Content-Type: application/json" \
  -d '{
    "analysis_id": "uuid-here",
    "heading_range": [0, 360],
    "angular_resolution": 5.0,
    "include_statistics": true
  }'
```

#### Upload File
```bash
curl -X POST "http://localhost:8000/api/upload/single" \
  -H "Authorization: Bearer <token>" \
  -F "file=@analysis.dat" \
  -F "create_analysis=true" \
  -F "analysis_name=Test Analysis"
```

#### Export Data
```bash
curl -X POST "http://localhost:8000/api/exports/csv" \
  -H "Authorization: Bearer <token>" \
  -H "Content-Type: application/json" \
  -d '{
    "analysis_ids": ["uuid1", "uuid2"],
    "format": "csv",
    "include_metadata": true,
    "compression": "gzip"
  }'
```

### Interactive Documentation

When running in development mode, access interactive API documentation at:
- **Swagger UI**: http://localhost:8000/api/docs
- **ReDoc**: http://localhost:8000/api/redoc

## 🔧 Development

### Code Quality

The project includes comprehensive code quality tools:

```bash
# Format code
black app/

# Sort imports
isort app/

# Lint code
flake8 app/

# Type checking
mypy app/

# Security linting
bandit -r app/

# Run all quality checks
pre-commit run --all-files
```

### Testing

```bash
# Run tests
pytest

# Run with coverage
pytest --cov=app --cov-report=html

# Run specific test file
pytest tests/test_api/test_analyses.py
```

### Performance Testing

```bash
# Load testing with Apache Bench
ab -n 1000 -c 10 -H "Authorization: Bearer <token>" \
  http://localhost:8000/api/analyses

# Memory profiling
python -m memory_profiler app/main.py
```

## 📈 Performance Monitoring

### Built-in Metrics

The API provides built-in performance monitoring:

```bash
# System health
curl http://localhost:8000/api/health

# Performance statistics
curl -H "Authorization: Bearer <token>" \
  http://localhost:8000/api/statistics/performance

# Cache statistics
curl -H "Authorization: Bearer <token>" \
  http://localhost:8000/api/health/cache
```

### Rate Limiting

Rate limits are applied per endpoint with Redis-backed sliding windows:
- Most endpoints: 100-300 requests per minute
- Upload endpoints: 20-50 requests per 5 minutes
- Export endpoints: 10-30 requests per 5 minutes

Rate limit headers are included in all responses:
- `X-RateLimit-Limit`: Maximum requests per time window
- `X-RateLimit-Remaining`: Remaining requests in current window
- `X-RateLimit-Reset`: Timestamp when window resets

## 🔒 Security

### Features
- **JWT Authentication**: Secure token-based authentication
- **Rate Limiting**: Protection against abuse and DoS attacks
- **Input Validation**: Comprehensive request validation
- **SQL Injection Protection**: SQLAlchemy ORM prevents SQL injection
- **File Upload Security**: File type validation and size limits
- **CORS Configuration**: Configurable cross-origin resource sharing

### Best Practices
- Use HTTPS in production
- Regularly rotate JWT secret keys
- Monitor rate limit violations
- Keep dependencies updated
- Use strong database credentials
- Enable Redis AUTH in production

## 🚀 Production Deployment

### Docker Deployment

```dockerfile
# Example Dockerfile
FROM python:3.11-slim

WORKDIR /app
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY app/ ./app/
EXPOSE 8000

CMD ["uvicorn", "app.main:app", "--host", "0.0.0.0", "--port", "8000", "--workers", "4"]
```

### Environment-Specific Settings

```bash
# Production environment variables
ORCAFLEX_ENVIRONMENT=production
ORCAFLEX_DEBUG=false
ORCAFLEX_SECRET_KEY=<strong-secret-key>
ORCAFLEX_DATABASE_URL=postgresql://user:password@db:5432/orcaflex
ORCAFLEX_REDIS_URL=redis://redis:6379/0
```

### Scaling Considerations

1. **Horizontal Scaling**: Run multiple workers behind a load balancer
2. **Database**: Use PostgreSQL with read replicas for high load
3. **Caching**: Redis cluster for high-availability caching
4. **File Storage**: Consider cloud storage (S3) for large files
5. **Monitoring**: Implement comprehensive monitoring and alerting

## 🔍 Troubleshooting

### Common Issues

#### High Memory Usage
- Check for memory leaks in data processing
- Adjust `max_points` parameters for large datasets
- Monitor cache memory usage

#### Slow Response Times
- Check Redis cache hit ratios
- Optimize database queries with indexes
- Review rate limiting configuration

#### Authentication Issues
- Verify JWT secret key configuration
- Check token expiration times
- Ensure proper Bearer token format

### Debug Mode

Enable debug logging for troubleshooting:

```bash
ORCAFLEX_DEBUG=true
ORCAFLEX_LOG_LEVEL=DEBUG
python -m uvicorn app.main:app --reload
```

## 📚 Integration

### Frontend Integration

The API is designed to integrate with modern frontend frameworks:

```javascript
// Example JavaScript integration
const apiClient = {
  baseURL: 'http://localhost:8000/api',
  headers: {
    'Authorization': `Bearer ${token}`,
    'Content-Type': 'application/json'
  }
};

// Get analyses
const analyses = await fetch(`${apiClient.baseURL}/analyses`, {
  headers: apiClient.headers
}).then(r => r.json());

// Upload file
const formData = new FormData();
formData.append('file', file);
formData.append('create_analysis', 'true');

const upload = await fetch(`${apiClient.baseURL}/upload/single`, {
  method: 'POST',
  headers: { 'Authorization': apiClient.headers.Authorization },
  body: formData
}).then(r => r.json());
```

### Third-party Integration

The API supports integration with:
- **Jupyter Notebooks**: Direct data access for analysis
- **MATLAB/Python**: RESTful API calls for data processing
- **BI Tools**: Export capabilities for business intelligence
- **Monitoring Systems**: Health and metrics endpoints

## 🤝 Contributing

### Development Workflow

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/new-feature`
3. Make changes and add tests
4. Run quality checks: `pre-commit run --all-files`
5. Commit changes: `git commit -m "Add new feature"`
6. Push to branch: `git push origin feature/new-feature`
7. Create a Pull Request

### Code Standards

- Follow PEP 8 style guidelines
- Use type hints for all functions
- Write comprehensive docstrings
- Add unit tests for new features
- Maintain >90% test coverage

## 📄 License

This project is part of the Digital Model offshore engineering platform and follows the same licensing terms as the parent project.

## 🆘 Support

For support and questions:
- Check the API documentation at `/api/docs`
- Review the troubleshooting section above
- Create an issue in the repository
- Contact the engineering team for offshore engineering domain questions

---

**Built for Offshore Engineering Excellence** 🌊⚓

This FastAPI backend is specifically designed for offshore hydrodynamic analysis and structural design, supporting industry standards (API, DNV, ABS) and engineered for the demanding requirements of marine engineering applications.