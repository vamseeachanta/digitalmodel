# OrcaFlex Dashboard - Technical Specification

## System Architecture

### Overview
The OrcaFlex Dashboard follows a modern microservices architecture with clear separation of concerns:

```
┌─────────────┐     ┌──────────────┐     ┌─────────────┐
│   Browser   │────▶│  Web Server  │────▶│     API     │
│   (React)   │◀────│   (Nginx)    │◀────│  (FastAPI)  │
└─────────────┘     └──────────────┘     └─────────────┘
                            │                     │
                            ▼                     ▼
                    ┌──────────────┐     ┌─────────────┐
                    │    Redis     │     │ PostgreSQL  │
                    │   (Cache)    │     │ (Database)  │
                    └──────────────┘     └─────────────┘
                                                 │
                                                 ▼
                                         ┌─────────────┐
                                         │  OrcaFlex   │
                                         │    Files    │
                                         └─────────────┘
```

### Component Architecture

#### Frontend Layer
- **Framework**: React 18 with TypeScript
- **State Management**: Redux Toolkit
- **UI Components**: Material-UI v5
- **Charting**: D3.js for custom visualizations, Plotly for standard charts
- **Real-time**: WebSocket via Socket.io-client

#### Backend Layer
- **Framework**: FastAPI (Python 3.11+)
- **Async Runtime**: Uvicorn with uvloop
- **ORM**: SQLAlchemy 2.0 with async support
- **Validation**: Pydantic v2
- **Task Queue**: Celery with Redis broker

#### Data Layer
- **Primary Database**: PostgreSQL 15
- **Cache**: Redis 7
- **File Storage**: Network-attached storage (NAS)
- **Time Series**: TimescaleDB extension

## Data Flow Architecture

### CSV Processing Pipeline
```python
# Data flow stages
CSV Files → Parser → Validator → Transformer → Database → Cache → API → UI

# Processing steps
1. File Detection (inotify/watchdog)
2. Async Parsing (pandas + numpy)
3. Data Validation (pydantic schemas)
4. Unit Conversion (SI standardization)
5. Database Storage (bulk inserts)
6. Cache Warming (Redis pipelines)
7. API Serving (FastAPI endpoints)
8. UI Rendering (React components)
```

### Real-time Updates
```javascript
// WebSocket message flow
Server File Monitor → Event Queue → WebSocket Server → Connected Clients
                         ↓
                    Database Update
                         ↓
                    Cache Invalidation
```

## API Specification

### RESTful Endpoints

#### Data Access
```yaml
GET /api/data/cases:
  description: List available analysis cases
  response:
    - case_id: string
      name: string
      timestamp: datetime
      status: enum[complete, processing, error]

POST /api/data/polar:
  description: Get polar plot data
  request:
    case: string
    component: string
    loading_condition: string
  response:
    headings: float[]  # 24 points: 0° to 345°
    values: float[]
    unit: string
    statistics: object

POST /api/data/time-trace:
  description: Get time series data
  request:
    case: string
    component: string
    heading: float
    time_range: [start, end]
  response:
    timestamps: float[]
    values: float[]
    unit: string
```

#### Analysis
```yaml
POST /api/analysis/correlation:
  description: Calculate correlation between variables
  request:
    variables: string[]
    cases: string[]
  response:
    correlation_matrix: float[][]
    p_values: float[][]

POST /api/analysis/sensitivity:
  description: Perform sensitivity analysis
  request:
    target_variable: string
    input_variables: string[]
  response:
    sensitivities: object
    tornado_chart: object
```

#### Export
```yaml
POST /api/export/chart:
  description: Export chart as image
  request:
    chart_config: object
    format: enum[png, svg, pdf]
  response:
    download_url: string

POST /api/export/report:
  description: Generate analysis report
  request:
    template: string
    data_queries: object[]
  response:
    report_url: string
```

### WebSocket Events

#### Client → Server
```typescript
interface ClientMessage {
  type: 'subscribe' | 'unsubscribe' | 'request';
  channel: 'data' | 'progress' | 'notifications';
  payload: any;
}
```

#### Server → Client
```typescript
interface ServerMessage {
  type: 'update' | 'progress' | 'notification' | 'error';
  channel: string;
  payload: {
    timestamp: number;
    data: any;
  };
}
```

## Data Models

### Database Schema
```sql
-- Core tables
CREATE TABLE analysis_cases (
    id UUID PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    created_at TIMESTAMP NOT NULL,
    status VARCHAR(50),
    metadata JSONB
);

CREATE TABLE components (
    id UUID PRIMARY KEY,
    case_id UUID REFERENCES analysis_cases(id),
    type VARCHAR(50),  -- fst1, fst2, strut, jacket, lngc
    name VARCHAR(255),
    properties JSONB
);

CREATE TABLE polar_data (
    id UUID PRIMARY KEY,
    component_id UUID REFERENCES components(id),
    loading_condition VARCHAR(100),
    heading FLOAT NOT NULL,
    value FLOAT NOT NULL,
    unit VARCHAR(20),
    timestamp TIMESTAMP
);

CREATE TABLE time_traces (
    id UUID PRIMARY KEY,
    component_id UUID REFERENCES components(id),
    heading FLOAT,
    time_points FLOAT[],
    values FLOAT[],
    unit VARCHAR(20),
    metadata JSONB
);

-- Indexes for performance
CREATE INDEX idx_polar_component ON polar_data(component_id);
CREATE INDEX idx_polar_heading ON polar_data(heading);
CREATE INDEX idx_traces_component ON time_traces(component_id);
```

### Cache Strategy
```python
# Cache key patterns
cache_keys = {
    "cases_list": "cases:all",
    "case_detail": "case:{case_id}",
    "polar_data": "polar:{case}:{component}:{loading}",
    "time_trace": "trace:{case}:{component}:{heading}",
    "statistics": "stats:{case}:{component}",
}

# TTL configuration (seconds)
cache_ttl = {
    "cases_list": 300,      # 5 minutes
    "case_detail": 600,     # 10 minutes
    "polar_data": 3600,     # 1 hour
    "time_trace": 3600,     # 1 hour
    "statistics": 1800,     # 30 minutes
}
```

## Performance Requirements

### Response Time Targets
```yaml
API Endpoints:
  - List operations: < 100ms
  - Single record fetch: < 200ms
  - Data aggregation: < 500ms
  - Chart generation: < 1000ms
  - Report generation: < 5000ms

Frontend Metrics:
  - Initial page load: < 3s
  - Route navigation: < 300ms
  - Chart rendering: < 500ms
  - Filter updates: < 200ms
```

### Scalability Targets
```yaml
Concurrent Users: 50
Requests per Second: 1000
Data Volume: 10GB per analysis case
File Processing: 100 files/minute
WebSocket Connections: 200 simultaneous
```

### Resource Limits
```yaml
Memory:
  - Backend service: 4GB max
  - Frontend build: 2GB max
  - Database connections: 100 max
  - Redis memory: 8GB max

CPU:
  - Backend workers: 4 cores
  - Database: 8 cores
  - File processing: 2 cores per worker

Storage:
  - Database: 500GB
  - File storage: 5TB
  - Cache: 16GB
```

## Security Specifications

### Authentication & Authorization
```python
# JWT token structure
{
  "sub": "user_id",
  "email": "user@example.com",
  "roles": ["engineer", "admin"],
  "exp": 1234567890,
  "iat": 1234567890
}

# Permission matrix
permissions = {
    "engineer": ["read", "export", "analyze"],
    "admin": ["read", "write", "delete", "export", "analyze", "configure"],
    "viewer": ["read", "export"],
}
```

### Data Security
- **Encryption at Rest**: AES-256 for sensitive data
- **Encryption in Transit**: TLS 1.3 for all connections
- **Input Validation**: Strict schema validation on all inputs
- **SQL Injection Prevention**: Parameterized queries only
- **XSS Protection**: Content Security Policy headers
- **CORS Configuration**: Whitelist allowed origins

### Audit Logging
```python
# Audit log structure
{
    "timestamp": "2025-01-10T10:30:00Z",
    "user_id": "uuid",
    "action": "data_export",
    "resource": "polar_data",
    "details": {
        "case_id": "uuid",
        "format": "csv",
        "rows": 1000
    },
    "ip_address": "192.168.1.100",
    "user_agent": "Mozilla/5.0..."
}
```

## Testing Strategy

### Unit Testing
- **Coverage Target**: 90% for critical paths
- **Framework**: pytest for backend, Vitest for frontend
- **Mocking**: All external dependencies

### Integration Testing
- **API Testing**: Full endpoint coverage
- **Database Testing**: Migration and query testing
- **Cache Testing**: Redis operations

### Performance Testing
- **Load Testing**: JMeter for API endpoints
- **Stress Testing**: Identify breaking points
- **Endurance Testing**: 24-hour continuous operation

### E2E Testing
- **Framework**: Playwright
- **Scenarios**: Complete user workflows
- **Cross-browser**: Chrome, Firefox, Safari

## Deployment Architecture

### Container Strategy
```yaml
services:
  frontend:
    image: orcaflex-dashboard-frontend:latest
    replicas: 2
    resources:
      limits:
        memory: 1Gi
        cpu: 500m

  backend:
    image: orcaflex-dashboard-backend:latest
    replicas: 3
    resources:
      limits:
        memory: 2Gi
        cpu: 1000m

  worker:
    image: orcaflex-dashboard-worker:latest
    replicas: 2
    resources:
      limits:
        memory: 4Gi
        cpu: 2000m
```

### CI/CD Pipeline
```yaml
stages:
  - lint:
      - ESLint for TypeScript
      - Ruff for Python
  - test:
      - Unit tests
      - Integration tests
  - build:
      - Docker images
      - Static assets
  - deploy:
      - Staging environment
      - Production (manual approval)
```

## Monitoring & Observability

### Metrics Collection
- **Application Metrics**: Prometheus format
- **Infrastructure Metrics**: Node exporter
- **Custom Metrics**: Business KPIs

### Logging Strategy
- **Centralized Logging**: ELK stack or similar
- **Log Levels**: ERROR, WARN, INFO, DEBUG
- **Structured Logging**: JSON format

### Alerting Rules
```yaml
alerts:
  - name: High Response Time
    condition: p95_response_time > 1000ms
    severity: warning
  
  - name: Error Rate
    condition: error_rate > 1%
    severity: critical
  
  - name: Memory Usage
    condition: memory_usage > 80%
    severity: warning
```

---

*For implementation details, see [Tasks Breakdown](tasks.md)*  
*For business context, see [Executive Summary](executive-summary.md)*