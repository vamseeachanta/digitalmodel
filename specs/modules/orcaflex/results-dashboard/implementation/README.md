# OrcaFlex Dashboard Visualization Module

## Overview
Comprehensive web-based dashboard for visualizing and analyzing OrcaFlex simulation results, providing real-time insights into offshore engineering analyses including mooring tensions, vessel motions, and structural responses.

## Quick Start

```bash
# Development setup
docker-compose up -d

# Install dependencies
cd src/frontend && npm install
cd ../backend && pip install -e .

# Run development servers
npm run dev:all
```

## Architecture

The dashboard follows a modern microservices architecture:

- **Frontend**: React/TypeScript SPA with real-time updates
- **Backend**: FastAPI with async Python processing
- **Data Pipeline**: Automated CSV parsing and analysis
- **Cache Layer**: Redis for performance optimization
- **Database**: PostgreSQL for metadata and results

## Key Features

### 1. Real-time Data Visualization
- Interactive polar plots for directional response
- Time trace analysis with statistical overlays
- Multi-case comparison capabilities
- 3D vessel motion visualization

### 2. Advanced Analysis
- Statistical analysis (mean, std dev, percentiles)
- Correlation and regression analysis
- Trend detection and anomaly identification
- Environmental sensitivity studies

### 3. Professional Export
- High-resolution chart export (PNG, SVG, PDF)
- Automated report generation
- Data export with complete metadata
- Batch processing capabilities

## Data Processing

The system processes OrcaFlex output files:

```
dm_* files → Parser → Validation → Database → API → Visualization
```

### Supported Data Types
- **Polar Data**: 24-point heading analysis (0° to 345°)
- **Time Traces**: Component response time series
- **Statistics**: Maximum, mean, standard deviation
- **Components**: fst1, fst2, strut, jacket, lngc

## Performance Metrics

- Initial load time: < 3 seconds
- Chart update time: < 500ms
- Concurrent users: ≥ 10
- Dataset capacity: > 10GB
- Memory usage: < 2GB

## Development

### Prerequisites
- Node.js 18+
- Python 3.11+
- Docker & Docker Compose
- Redis (via Docker)
- PostgreSQL (via Docker)

### Testing
```bash
# Run all tests
npm run test:all

# Backend tests
pytest src/backend/tests/

# Frontend tests
npm run test:frontend

# E2E tests
npm run test:e2e
```

## Deployment

### Production Setup
```bash
# Build containers
docker-compose -f docker-compose.prod.yml build

# Deploy with SSL
docker-compose -f docker-compose.prod.yml up -d

# Configure Nginx
cp config/nginx.conf /etc/nginx/sites-available/orcaflex-dashboard
```

## API Documentation

Interactive API documentation available at:
- Development: http://localhost:8000/docs
- Production: https://your-domain/api/docs

## License

Proprietary - Internal Use Only

## Support

For issues or questions, contact the Marine Engineering Team.