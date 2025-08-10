# OrcaFlex Dashboard - Deployment Guide

## Prerequisites

### System Requirements
- **OS**: Ubuntu 22.04 LTS or RHEL 8+
- **CPU**: 8 cores minimum (16 recommended)
- **RAM**: 16GB minimum (32GB recommended)
- **Storage**: 500GB SSD for database and cache
- **Network**: 1Gbps connection

### Software Dependencies
```bash
# Required software versions
Docker: 24.0+
Docker Compose: 2.20+
Node.js: 18+ (for local development)
Python: 3.11+ (for local development)
PostgreSQL: 15+
Redis: 7+
Nginx: 1.24+
```

## Development Deployment

### 1. Clone Repository
```bash
git clone https://github.com/yourorg/digitalmodel.git
cd digitalmodel/specs/modules/visualization/orcaflex-dashboard
```

### 2. Environment Configuration
```bash
# Copy environment template
cp config/.env.example .env

# Edit configuration
nano .env

# Required variables:
DB_PASSWORD=secure-password-here
SECRET_KEY=$(openssl rand -hex 32)
CORS_ORIGINS=http://localhost:3000
WATCH_DIRECTORY=/path/to/orcaflex/results
```

### 3. Start Development Services
```bash
# Start all services
docker-compose up -d

# Verify services are running
docker-compose ps

# Check logs
docker-compose logs -f

# Expected output:
# orcaflex-db       running (healthy)
# orcaflex-cache    running (healthy)
# orcaflex-backend  running (healthy)
# orcaflex-frontend running (healthy)
```

### 4. Initialize Database
```bash
# Run database migrations
docker-compose exec backend alembic upgrade head

# Load sample data (optional)
docker-compose exec backend python scripts/load_sample_data.py
```

### 5. Access Application
```
Frontend: http://localhost:3000
API Docs: http://localhost:8000/docs
```

## Production Deployment

### 1. Server Preparation

#### Install Docker
```bash
# Ubuntu/Debian
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh
sudo usermod -aG docker $USER

# Verify installation
docker --version
docker-compose --version
```

#### Configure Firewall
```bash
# Allow required ports
sudo ufw allow 80/tcp    # HTTP
sudo ufw allow 443/tcp   # HTTPS
sudo ufw allow 22/tcp    # SSH
sudo ufw enable
```

### 2. SSL Certificate Setup

#### Using Let's Encrypt
```bash
# Install Certbot
sudo apt-get update
sudo apt-get install certbot python3-certbot-nginx

# Generate certificate
sudo certbot --nginx -d your-domain.com

# Auto-renewal
sudo systemctl enable certbot.timer
```

#### Using Custom Certificate
```bash
# Place certificates in config/ssl/
cp /path/to/cert.pem config/ssl/
cp /path/to/key.pem config/ssl/

# Update nginx.conf
nano config/nginx.conf
```

### 3. Production Configuration

#### Create Production Environment File
```bash
cat > .env.production << EOF
# Database
DB_PASSWORD=$(openssl rand -base64 32)
DATABASE_URL=postgresql+asyncpg://orcaflex:${DB_PASSWORD}@postgres:5432/orcaflex_dashboard

# Security
SECRET_KEY=$(openssl rand -hex 32)
ENVIRONMENT=production
DEBUG=false

# CORS
CORS_ORIGINS=https://your-domain.com

# Performance
MAX_WORKERS=8
CACHE_TTL=7200

# Monitoring
ENABLE_METRICS=true
EOF
```

#### Update Docker Compose for Production
```yaml
# docker-compose.prod.yml
version: '3.9'

services:
  backend:
    image: orcaflex-dashboard-backend:latest
    restart: always
    environment:
      - ENVIRONMENT=production
    deploy:
      replicas: 3
      resources:
        limits:
          memory: 2G
          cpus: '2'

  frontend:
    image: orcaflex-dashboard-frontend:latest
    restart: always
    deploy:
      replicas: 2
      resources:
        limits:
          memory: 1G
          cpus: '1'

  nginx:
    image: nginx:alpine
    restart: always
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./config/nginx.prod.conf:/etc/nginx/nginx.conf:ro
      - ./config/ssl:/etc/nginx/ssl:ro
```

### 4. Build and Deploy

#### Build Production Images
```bash
# Build backend
docker build -f Dockerfile.backend -t orcaflex-dashboard-backend:latest .

# Build frontend
docker build -f Dockerfile.frontend -t orcaflex-dashboard-frontend:latest .

# Push to registry (optional)
docker tag orcaflex-dashboard-backend:latest your-registry/orcaflex-dashboard-backend:latest
docker push your-registry/orcaflex-dashboard-backend:latest
```

#### Deploy Services
```bash
# Stop existing services
docker-compose -f docker-compose.prod.yml down

# Pull latest images
docker-compose -f docker-compose.prod.yml pull

# Start services
docker-compose -f docker-compose.prod.yml up -d

# Scale services
docker-compose -f docker-compose.prod.yml up -d --scale backend=3 --scale frontend=2
```

### 5. Database Setup

#### Production Database Configuration
```bash
# Connect to database container
docker-compose exec postgres psql -U orcaflex

# Create production database
CREATE DATABASE orcaflex_dashboard_prod;

# Configure connection pooling
ALTER SYSTEM SET max_connections = 200;
ALTER SYSTEM SET shared_buffers = '4GB';
ALTER SYSTEM SET effective_cache_size = '12GB';

# Reload configuration
SELECT pg_reload_conf();
```

#### Run Migrations
```bash
docker-compose exec backend alembic upgrade head
```

### 6. Monitoring Setup

#### Prometheus Configuration
```yaml
# prometheus.yml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'orcaflex-dashboard'
    static_configs:
      - targets: ['backend:9090']
```

#### Health Checks
```bash
# Backend health
curl http://localhost:8000/health

# Frontend health
curl http://localhost:3000/health

# Database health
docker-compose exec postgres pg_isready

# Redis health
docker-compose exec redis redis-cli ping
```

## Kubernetes Deployment

### 1. Prepare Kubernetes Manifests

#### Namespace
```yaml
# k8s/namespace.yaml
apiVersion: v1
kind: Namespace
metadata:
  name: orcaflex-dashboard
```

#### Backend Deployment
```yaml
# k8s/backend-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: backend
  namespace: orcaflex-dashboard
spec:
  replicas: 3
  selector:
    matchLabels:
      app: backend
  template:
    metadata:
      labels:
        app: backend
    spec:
      containers:
      - name: backend
        image: orcaflex-dashboard-backend:latest
        ports:
        - containerPort: 8000
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: url
        resources:
          requests:
            memory: "1Gi"
            cpu: "500m"
          limits:
            memory: "2Gi"
            cpu: "1000m"
```

#### Service
```yaml
# k8s/backend-service.yaml
apiVersion: v1
kind: Service
metadata:
  name: backend
  namespace: orcaflex-dashboard
spec:
  selector:
    app: backend
  ports:
  - port: 8000
    targetPort: 8000
  type: ClusterIP
```

### 2. Deploy to Kubernetes
```bash
# Create namespace
kubectl apply -f k8s/namespace.yaml

# Create secrets
kubectl create secret generic db-secret \
  --from-literal=url=postgresql://user:pass@host/db \
  -n orcaflex-dashboard

# Deploy applications
kubectl apply -f k8s/

# Check deployment status
kubectl get pods -n orcaflex-dashboard

# View logs
kubectl logs -f deployment/backend -n orcaflex-dashboard
```

## Backup and Recovery

### Database Backup
```bash
# Manual backup
docker-compose exec postgres pg_dump -U orcaflex orcaflex_dashboard > backup_$(date +%Y%m%d).sql

# Automated backup script
cat > backup.sh << 'EOF'
#!/bin/bash
BACKUP_DIR="/backups"
DATE=$(date +%Y%m%d_%H%M%S)
docker-compose exec -T postgres pg_dump -U orcaflex orcaflex_dashboard > $BACKUP_DIR/backup_$DATE.sql
find $BACKUP_DIR -name "backup_*.sql" -mtime +30 -delete
EOF

# Schedule with cron
crontab -e
# Add: 0 2 * * * /path/to/backup.sh
```

### Restore Database
```bash
# Restore from backup
docker-compose exec -T postgres psql -U orcaflex orcaflex_dashboard < backup_20250110.sql
```

## Troubleshooting

### Common Issues

#### 1. Database Connection Failed
```bash
# Check database status
docker-compose logs postgres

# Verify connection
docker-compose exec backend python -c "from services.database import engine; print(engine.url)"

# Reset database
docker-compose down -v
docker-compose up -d postgres
docker-compose exec backend alembic upgrade head
```

#### 2. Redis Connection Issues
```bash
# Check Redis status
docker-compose exec redis redis-cli ping

# Clear Redis cache
docker-compose exec redis redis-cli FLUSHALL

# Restart Redis
docker-compose restart redis
```

#### 3. Frontend Build Failures
```bash
# Clear node modules
docker-compose exec frontend rm -rf node_modules
docker-compose exec frontend npm install

# Clear build cache
docker-compose exec frontend npm run clean
docker-compose exec frontend npm run build
```

#### 4. Performance Issues
```bash
# Check resource usage
docker stats

# Analyze slow queries
docker-compose exec postgres psql -U orcaflex -c "SELECT * FROM pg_stat_statements ORDER BY total_time DESC LIMIT 10;"

# Optimize database
docker-compose exec postgres vacuumdb -U orcaflex -d orcaflex_dashboard -z
```

## Maintenance

### Regular Tasks
```bash
# Weekly: Update dependencies
docker-compose exec backend pip list --outdated
docker-compose exec frontend npm outdated

# Monthly: Database maintenance
docker-compose exec postgres vacuumdb -U orcaflex -d orcaflex_dashboard -z
docker-compose exec postgres reindexdb -U orcaflex -d orcaflex_dashboard

# Quarterly: Security updates
docker-compose pull
docker-compose up -d
```

### Monitoring Checklist
- [ ] CPU usage < 80%
- [ ] Memory usage < 80%
- [ ] Disk usage < 80%
- [ ] Response time < 1s
- [ ] Error rate < 1%
- [ ] Database connections < 80% of max
- [ ] Redis memory < 80% of max

## Rollback Procedure

### Quick Rollback
```bash
# Tag current version
docker tag orcaflex-dashboard-backend:latest orcaflex-dashboard-backend:rollback

# Deploy previous version
docker-compose down
docker tag orcaflex-dashboard-backend:v1.0.0 orcaflex-dashboard-backend:latest
docker-compose up -d
```

### Database Rollback
```bash
# Rollback migration
docker-compose exec backend alembic downgrade -1

# Restore from backup if needed
docker-compose exec -T postgres psql -U orcaflex orcaflex_dashboard < last_known_good.sql
```

---

*For additional support, contact the Marine Engineering Team or refer to the [Technical Specification](technical-specification.md).*