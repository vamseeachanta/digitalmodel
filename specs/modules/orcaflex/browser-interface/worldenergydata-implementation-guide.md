# WorldEnergyData Dashboard Implementation Guide

## Overview
This guide adapts the OrcaFlex Browser Interface architecture to create a data dashboard for the WorldEnergyData repository, focusing on energy data sources, visualization, and analysis.

## Architecture Overview

### Technology Stack
```yaml
Backend:
  - Python 3.9+
  - FastAPI for REST API
  - Pandas for data processing
  - SQLAlchemy for database ORM
  - Redis for caching

Frontend:
  - React 18+ with TypeScript
  - Vite for build tooling
  - AG-Grid for data tables
  - Chart.js/Recharts for visualizations
  - TailwindCSS for styling
  - Redux Toolkit for state management

Database:
  - PostgreSQL for structured data
  - TimescaleDB extension for time-series
  - MinIO for file storage

DevOps:
  - Docker & Docker Compose
  - GitHub Actions for CI/CD
  - Nginx for reverse proxy
```

## Implementation Phases

### Phase 1: Project Setup & Infrastructure (Week 1)

#### 1.1 Repository Structure
```
worldenergydata/
├── backend/
│   ├── app/
│   │   ├── api/
│   │   │   ├── endpoints/
│   │   │   ├── middleware/
│   │   │   └── deps.py
│   │   ├── core/
│   │   │   ├── config.py
│   │   │   ├── security.py
│   │   │   └── database.py
│   │   ├── models/
│   │   │   ├── energy_source.py
│   │   │   ├── consumption.py
│   │   │   └── production.py
│   │   ├── schemas/
│   │   ├── services/
│   │   │   ├── data_processor.py
│   │   │   ├── analyzer.py
│   │   │   └── aggregator.py
│   │   └── main.py
│   ├── tests/
│   ├── requirements.txt
│   └── Dockerfile
├── frontend/
│   ├── src/
│   │   ├── components/
│   │   │   ├── Dashboard/
│   │   │   ├── DataGrid/
│   │   │   ├── Charts/
│   │   │   └── Common/
│   │   ├── features/
│   │   │   ├── energy-sources/
│   │   │   ├── consumption/
│   │   │   └── analytics/
│   │   ├── services/
│   │   ├── store/
│   │   ├── types/
│   │   └── App.tsx
│   ├── package.json
│   └── Dockerfile
├── docker-compose.yml
├── docker-compose.dev.yml
└── README.md
```

#### 1.2 Development Environment Setup
```bash
# Clone repository
git clone https://github.com/yourusername/worldenergydata.git
cd worldenergydata

# Create Python virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install backend dependencies
cd backend
pip install -r requirements.txt

# Setup frontend
cd ../frontend
npm install

# Start development servers
docker-compose -f docker-compose.dev.yml up
```

### Phase 2: Backend Development (Week 1-2)

#### 2.1 Data Models
```python
# backend/app/models/energy_source.py
from sqlalchemy import Column, String, Float, DateTime, Integer
from app.core.database import Base

class EnergySource(Base):
    __tablename__ = "energy_sources"
    
    id = Column(Integer, primary_key=True, index=True)
    source_type = Column(String)  # solar, wind, hydro, nuclear, fossil
    country = Column(String, index=True)
    region = Column(String)
    capacity_mw = Column(Float)
    production_gwh = Column(Float)
    timestamp = Column(DateTime, index=True)
    metadata = Column(JSON)
```

#### 2.2 FastAPI Endpoints
```python
# backend/app/api/endpoints/energy_data.py
from fastapi import APIRouter, Depends, Query
from typing import List, Optional
from datetime import datetime

router = APIRouter()

@router.get("/sources")
async def get_energy_sources(
    country: Optional[str] = None,
    source_type: Optional[str] = None,
    start_date: Optional[datetime] = None,
    end_date: Optional[datetime] = None,
    limit: int = Query(100, le=1000)
):
    """Get energy sources with filtering"""
    pass

@router.get("/consumption")
async def get_consumption_data(
    country: str,
    granularity: str = "daily",  # hourly, daily, monthly, yearly
    start_date: Optional[datetime] = None,
    end_date: Optional[datetime] = None
):
    """Get energy consumption data"""
    pass

@router.post("/analyze")
async def analyze_data(
    analysis_type: str,  # trend, forecast, comparison
    parameters: dict
):
    """Run analysis on energy data"""
    pass
```

#### 2.3 Data Processing Service
```python
# backend/app/services/data_processor.py
import pandas as pd
from typing import Dict, List, Any
import numpy as np

class EnergyDataProcessor:
    def __init__(self):
        self.cache = {}
    
    def process_csv_batch(self, file_paths: List[str]) -> pd.DataFrame:
        """Process multiple CSV files"""
        dataframes = []
        for path in file_paths:
            df = pd.read_csv(path)
            df = self.standardize_columns(df)
            df = self.validate_data(df)
            dataframes.append(df)
        return pd.concat(dataframes, ignore_index=True)
    
    def aggregate_by_source(self, df: pd.DataFrame, 
                           groupby: List[str]) -> pd.DataFrame:
        """Aggregate data by energy source"""
        return df.groupby(groupby).agg({
            'production_gwh': 'sum',
            'capacity_mw': 'sum',
            'emissions_co2': 'mean'
        }).reset_index()
    
    def calculate_trends(self, df: pd.DataFrame, 
                        period: str = 'monthly') -> Dict[str, Any]:
        """Calculate production trends"""
        df['timestamp'] = pd.to_datetime(df['timestamp'])
        df.set_index('timestamp', inplace=True)
        
        resampled = df.resample(period[0].upper()).agg({
            'production_gwh': 'sum',
            'capacity_mw': 'mean'
        })
        
        return {
            'trend': resampled.to_dict(),
            'growth_rate': self.calculate_growth_rate(resampled),
            'forecast': self.simple_forecast(resampled)
        }
```

### Phase 3: Frontend Development (Week 2-3)

#### 3.1 Dashboard Component
```typescript
// frontend/src/components/Dashboard/EnergyDashboard.tsx
import React, { useEffect, useState } from 'react';
import { Grid, Card, Typography } from '@mui/material';
import { EnergySourceChart } from '../Charts/EnergySourceChart';
import { ConsumptionGrid } from '../DataGrid/ConsumptionGrid';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { fetchEnergySources } from '../../features/energy-sources/energySlice';

export const EnergyDashboard: React.FC = () => {
  const dispatch = useAppDispatch();
  const { sources, loading, filters } = useAppSelector(state => state.energy);
  
  useEffect(() => {
    dispatch(fetchEnergySources(filters));
  }, [filters]);
  
  return (
    <Grid container spacing={3}>
      <Grid item xs={12}>
        <Card>
          <Typography variant="h4">World Energy Data Dashboard</Typography>
        </Card>
      </Grid>
      
      <Grid item xs={12} md={8}>
        <Card>
          <EnergySourceChart data={sources} />
        </Card>
      </Grid>
      
      <Grid item xs={12} md={4}>
        <Card>
          <SummaryStats data={sources} />
        </Card>
      </Grid>
      
      <Grid item xs={12}>
        <ConsumptionGrid data={sources} loading={loading} />
      </Grid>
    </Grid>
  );
};
```

#### 3.2 Data Grid Component
```typescript
// frontend/src/components/DataGrid/ConsumptionGrid.tsx
import React, { useMemo } from 'react';
import { AgGridReact } from 'ag-grid-react';
import { ColDef } from 'ag-grid-community';
import 'ag-grid-community/styles/ag-grid.css';
import 'ag-grid-community/styles/ag-theme-material.css';

interface ConsumptionData {
  country: string;
  source: string;
  production: number;
  capacity: number;
  emissions: number;
  timestamp: string;
}

export const ConsumptionGrid: React.FC<{ data: ConsumptionData[] }> = ({ data }) => {
  const columnDefs: ColDef[] = useMemo(() => [
    { field: 'country', filter: true, sortable: true },
    { field: 'source', filter: true },
    { 
      field: 'production', 
      headerName: 'Production (GWh)',
      valueFormatter: params => params.value.toFixed(2)
    },
    { 
      field: 'capacity', 
      headerName: 'Capacity (MW)',
      valueFormatter: params => params.value.toFixed(0)
    },
    {
      field: 'emissions',
      headerName: 'CO2 (tons)',
      cellClassRules: {
        'high-emissions': params => params.value > 1000,
        'low-emissions': params => params.value < 100
      }
    }
  ], []);
  
  return (
    <div className="ag-theme-material" style={{ height: 600, width: '100%' }}>
      <AgGridReact
        rowData={data}
        columnDefs={columnDefs}
        pagination={true}
        paginationPageSize={20}
        enableCellExport={true}
        enableRangeSelection={true}
      />
    </div>
  );
};
```

#### 3.3 Visualization Components
```typescript
// frontend/src/components/Charts/EnergySourceChart.tsx
import React from 'react';
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  BarElement,
  LineElement,
  Title,
  Tooltip,
  Legend
} from 'chart.js';
import { Bar, Line } from 'react-chartjs-2';

ChartJS.register(
  CategoryScale,
  LinearScale,
  BarElement,
  LineElement,
  Title,
  Tooltip,
  Legend
);

export const EnergySourceChart: React.FC<{ data: any }> = ({ data }) => {
  const chartData = {
    labels: data.map(d => d.source),
    datasets: [
      {
        label: 'Production (GWh)',
        data: data.map(d => d.production),
        backgroundColor: 'rgba(75, 192, 192, 0.6)',
      },
      {
        label: 'Capacity (MW)',
        data: data.map(d => d.capacity),
        backgroundColor: 'rgba(255, 159, 64, 0.6)',
      }
    ]
  };
  
  const options = {
    responsive: true,
    plugins: {
      legend: { position: 'top' as const },
      title: {
        display: true,
        text: 'Energy Production by Source'
      }
    }
  };
  
  return <Bar data={chartData} options={options} />;
};
```

### Phase 4: Data Integration & APIs (Week 3)

#### 4.1 External Data Sources Integration
```python
# backend/app/services/data_sources.py
import aiohttp
import pandas as pd
from typing import Dict, Any

class DataSourceIntegrator:
    def __init__(self):
        self.sources = {
            'eia': 'https://api.eia.gov/v2/',
            'irena': 'https://www.irena.org/api/',
            'iea': 'https://api.iea.org/',
            'worldbank': 'https://api.worldbank.org/v2/'
        }
    
    async def fetch_eia_data(self, params: Dict[str, Any]):
        """Fetch from US Energy Information Administration"""
        async with aiohttp.ClientSession() as session:
            async with session.get(
                f"{self.sources['eia']}/electricity/retail-sales",
                params=params
            ) as response:
                return await response.json()
    
    async def fetch_renewable_capacity(self, country: str):
        """Fetch renewable capacity data from IRENA"""
        # Implementation for IRENA API
        pass
    
    def harmonize_data(self, source: str, data: Dict) -> pd.DataFrame:
        """Standardize data from different sources"""
        if source == 'eia':
            df = pd.DataFrame(data['data'])
            df.rename(columns={'period': 'timestamp'}, inplace=True)
        elif source == 'irena':
            df = pd.DataFrame(data['results'])
        
        # Standardize column names and units
        return self.standardize_dataframe(df)
```

#### 4.2 Real-time Data Updates
```python
# backend/app/services/realtime_updater.py
from apscheduler.schedulers.asyncio import AsyncIOScheduler
import asyncio

class RealtimeDataUpdater:
    def __init__(self):
        self.scheduler = AsyncIOScheduler()
        self.setup_jobs()
    
    def setup_jobs(self):
        # Update energy production every hour
        self.scheduler.add_job(
            self.update_production_data,
            'interval',
            hours=1,
            id='production_update'
        )
        
        # Update consumption data every 30 minutes
        self.scheduler.add_job(
            self.update_consumption_data,
            'interval',
            minutes=30,
            id='consumption_update'
        )
    
    async def update_production_data(self):
        """Fetch and update production data"""
        integrator = DataSourceIntegrator()
        data = await integrator.fetch_eia_data({'type': 'production'})
        # Process and store in database
        
    def start(self):
        self.scheduler.start()
```

### Phase 5: Deployment & DevOps (Week 4)

#### 5.1 Docker Configuration
```dockerfile
# backend/Dockerfile
FROM python:3.9-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

CMD ["uvicorn", "app.main:app", "--host", "0.0.0.0", "--port", "8000"]
```

```dockerfile
# frontend/Dockerfile
FROM node:18-alpine as builder

WORKDIR /app

COPY package*.json ./
RUN npm ci

COPY . .
RUN npm run build

FROM nginx:alpine
COPY --from=builder /app/dist /usr/share/nginx/html
COPY nginx.conf /etc/nginx/nginx.conf
```

#### 5.2 Docker Compose Configuration
```yaml
# docker-compose.yml
version: '3.8'

services:
  postgres:
    image: timescale/timescaledb:latest-pg14
    environment:
      POSTGRES_DB: worldenergydata
      POSTGRES_USER: energy_user
      POSTGRES_PASSWORD: ${DB_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"

  backend:
    build: ./backend
    environment:
      DATABASE_URL: postgresql://energy_user:${DB_PASSWORD}@postgres/worldenergydata
      REDIS_URL: redis://redis:6379
    depends_on:
      - postgres
      - redis
    ports:
      - "8000:8000"
    volumes:
      - ./backend:/app

  frontend:
    build: ./frontend
    depends_on:
      - backend
    ports:
      - "3000:80"
    environment:
      - VITE_API_URL=http://backend:8000

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./ssl:/etc/nginx/ssl
    depends_on:
      - frontend
      - backend

volumes:
  postgres_data:
```

#### 5.3 CI/CD Pipeline
```yaml
# .github/workflows/deploy.yml
name: Deploy WorldEnergyData Dashboard

on:
  push:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: '3.9'
      
      - name: Install dependencies
        run: |
          cd backend
          pip install -r requirements.txt
          pip install pytest
      
      - name: Run tests
        run: |
          cd backend
          pytest tests/

  build-and-deploy:
    needs: test
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Build Docker images
        run: |
          docker-compose build
      
      - name: Deploy to server
        run: |
          # SSH and deploy to production server
          ssh ${{ secrets.SERVER_USER }}@${{ secrets.SERVER_HOST }} << 'EOF'
            cd /var/www/worldenergydata
            git pull
            docker-compose down
            docker-compose up -d
          EOF
```

## Key Implementation Considerations

### 1. Data Sources
- **Primary Sources**: Government APIs (EIA, IEA)
- **Secondary Sources**: CSV uploads, Excel imports
- **Real-time Sources**: WebSocket feeds for live data

### 2. Performance Optimization
- **Caching Strategy**: Redis for frequently accessed data
- **Database Indexing**: Index on timestamp, country, source_type
- **Pagination**: Implement server-side pagination for large datasets
- **Lazy Loading**: Load charts and grids on demand

### 3. Security
- **Authentication**: JWT tokens for API access
- **Rate Limiting**: Implement rate limiting on API endpoints
- **Data Validation**: Validate all inputs on backend
- **CORS**: Configure CORS properly for production

### 4. Monitoring
- **Logging**: Structured logging with correlation IDs
- **Metrics**: Prometheus + Grafana for monitoring
- **Error Tracking**: Sentry for error tracking
- **Uptime Monitoring**: Use UptimeRobot or similar

## Migration from OrcaFlex Interface

### Key Adaptations:
1. **Data Model**: Replace tension/strut data with energy production/consumption
2. **File Processing**: Adapt CSV processing for energy data formats
3. **Analysis Engine**: Replace tension calculations with energy analytics
4. **Visualization**: Focus on time-series and geographical data
5. **Metadata**: Extract country, source type, time period instead of loading conditions

### Reusable Components:
1. File browser and pattern matching logic
2. Batch processing architecture
3. Caching strategy
4. Frontend grid components
5. API structure and error handling

## Getting Started

1. **Clone the repository**
```bash
git clone https://github.com/yourusername/worldenergydata.git
cd worldenergydata
```

2. **Set up environment variables**
```bash
cp .env.example .env
# Edit .env with your configuration
```

3. **Start development environment**
```bash
docker-compose -f docker-compose.dev.yml up
```

4. **Access the dashboard**
- Frontend: http://localhost:3000
- Backend API: http://localhost:8000/docs
- PostgreSQL: localhost:5432

## Testing Strategy

### Backend Tests
```python
# backend/tests/test_energy_api.py
import pytest
from fastapi.testclient import TestClient
from app.main import app

client = TestClient(app)

def test_get_energy_sources():
    response = client.get("/api/sources?country=USA")
    assert response.status_code == 200
    assert "sources" in response.json()

def test_analyze_data():
    response = client.post("/api/analyze", json={
        "analysis_type": "trend",
        "parameters": {"period": "monthly"}
    })
    assert response.status_code == 200
```

### Frontend Tests
```typescript
// frontend/src/components/Dashboard/EnergyDashboard.test.tsx
import { render, screen, waitFor } from '@testing-library/react';
import { EnergyDashboard } from './EnergyDashboard';
import { Provider } from 'react-redux';
import { store } from '../../store';

test('renders energy dashboard', async () => {
  render(
    <Provider store={store}>
      <EnergyDashboard />
    </Provider>
  );
  
  await waitFor(() => {
    expect(screen.getByText('World Energy Data Dashboard')).toBeInTheDocument();
  });
});
```

## Support & Documentation

- **API Documentation**: Available at `/docs` endpoint
- **User Guide**: See `docs/user-guide.md`
- **Development Guide**: See `docs/development.md`
- **Deployment Guide**: See `docs/deployment.md`

## Next Steps

1. Set up the repository structure
2. Implement core backend services
3. Create frontend components
4. Integrate data sources
5. Deploy to staging environment
6. Performance testing
7. Production deployment

---

*This implementation guide provides a complete roadmap for creating a data dashboard in the WorldEnergyData repository, adapted from the OrcaFlex Browser Interface architecture.*