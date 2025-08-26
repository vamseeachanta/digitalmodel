# OrcaWave MCP Server Deployment Guide

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [Installation](#installation)
3. [Configuration](#configuration)
4. [Deployment Options](#deployment-options)
5. [Testing](#testing)
6. [Monitoring](#monitoring)
7. [Troubleshooting](#troubleshooting)
8. [Production Checklist](#production-checklist)

## Prerequisites

### System Requirements
- **OS**: Windows 10/11 or Windows Server 2019+
- **Python**: 3.9+ (3.11 recommended)
- **RAM**: Minimum 8GB, 16GB recommended
- **Storage**: 10GB free space
- **Network**: Ports 3100 (MCP) and 8765 (WebSocket)

### Software Requirements
- OrcaWave Professional (licensed)
- OrcaFlex (optional, for export)
- Git (for version control)

## Installation

### 1. Clone Repository
```bash
git clone https://github.com/your-org/digitalmodel.git
cd digitalmodel
```

### 2. Create Virtual Environment
```bash
# Using venv
python -m venv venv
venv\Scripts\activate

# Or using uv (recommended)
uv venv
uv sync
```

### 3. Install Dependencies

#### Core Dependencies
```bash
pip install fastmcp pyyaml structlog
```

#### Windows COM Support
```bash
pip install pywin32
python Scripts/pywin32_postinstall.py -install
```

#### Vision Features
```bash
pip install opencv-python pillow numpy pyautogui
```

#### WebSocket Monitoring
```bash
pip install fastapi uvicorn websockets
```

#### Optional OCR
```bash
pip install pytesseract
# Download Tesseract from: https://github.com/tesseract-ocr/tesseract
```

### 4. Verify Installation
```bash
python test_orcawave_integration.py
```

## Configuration

### 1. Edit Configuration File
Copy and modify the configuration:
```bash
cp mcp/orcawave/config.yml mcp/orcawave/config.local.yml
```

Edit `config.local.yml`:
```yaml
server:
  name: "orcawave-mcp-production"
  host: "0.0.0.0"  # Allow external connections
  port: 3100

orcawave:
  installation_path: "C:/Program Files/Orcina/OrcaWave"
  com_timeout: 60000  # Increase for production
  auto_connect: true
  license_server: "license.company.com:5053"

analysis:
  default_type: "diffraction"
  convergence_tolerance: 0.001  # Tighter for production
  max_iterations: 200
  parallel_frequencies: true
  default_water_depth: 200.0

websocket:
  port: 8765
  screenshot_interval: 10.0  # Less frequent in production
  progress_interval: 2.0
  max_clients: 50

performance:
  cache_results: true
  cache_ttl: 86400  # 24 hours
  max_parallel_analyses: 5

security:
  enable_auth: false  # Enable in production
  api_key: "your-secret-key"
  allowed_origins: ["http://localhost:*", "https://your-domain.com"]
```

### 2. Environment Variables
Create `.env` file:
```bash
ORCAWAVE_LICENSE_SERVER=license.company.com:5053
ORCAWAVE_INSTALL_PATH=C:/Program Files/Orcina/OrcaWave
MCP_SERVER_PORT=3100
WEBSOCKET_PORT=8765
LOG_LEVEL=INFO
```

## Deployment Options

### Option 1: Standalone Development
```bash
# Simple standalone mode
python mcp/orcawave/run_server.py --standalone

# With custom config
python mcp/orcawave/run_server.py --standalone --config config.local.yml
```

### Option 2: MCP Server Mode
```bash
# Start MCP server
python mcp/orcawave/run_server.py --config config.local.yml

# Connect with Claude CLI
claude --mcp-server localhost:3100
```

### Option 3: Windows Service

Create Windows service using NSSM:
```bash
# Download NSSM from https://nssm.cc/
nssm install OrcaWaveMCP

# Configure service
Application: C:\Python311\python.exe
Arguments: D:\digitalmodel\mcp\orcawave\run_server.py --config config.local.yml
Startup directory: D:\digitalmodel
```

### Option 4: Docker Container

Create `Dockerfile`:
```dockerfile
FROM python:3.11-windowsservercore

WORKDIR /app
COPY . .

RUN pip install -r requirements.txt

EXPOSE 3100 8765

CMD ["python", "mcp/orcawave/run_server.py"]
```

Build and run:
```bash
docker build -t orcawave-mcp .
docker run -p 3100:3100 -p 8765:8765 orcawave-mcp
```

### Option 5: Process Manager (PM2)

Using PM2 for process management:
```bash
# Install PM2
npm install -g pm2
npm install -g pm2-windows-startup

# Start server
pm2 start ecosystem.config.js
pm2 save
pm2 startup
```

`ecosystem.config.js`:
```javascript
module.exports = {
  apps: [{
    name: 'orcawave-mcp',
    script: 'python',
    args: 'mcp/orcawave/run_server.py',
    cwd: 'D:/digitalmodel',
    instances: 1,
    autorestart: true,
    watch: false,
    max_memory_restart: '2G',
    env: {
      MCP_SERVER_PORT: 3100,
      WEBSOCKET_PORT: 8765
    }
  }]
}
```

## Testing

### 1. Unit Tests
```bash
# Run all tests
python -m pytest src/mcp/orcawave/tests/

# Run specific test
python src/mcp/orcawave/tests/test_integration.py

# With coverage
python -m pytest --cov=src.mcp.orcawave --cov-report=html
```

### 2. Integration Testing
```bash
# Test server startup
python mcp/orcawave/run_server.py --standalone --test

# Test WebSocket connection
python -c "
import asyncio
import websockets
import json

async def test():
    uri = 'ws://localhost:8765/ws/test'
    async with websockets.connect(uri) as ws:
        await ws.send(json.dumps({'command': 'ping'}))
        response = await ws.recv()
        print(f'Response: {response}')

asyncio.run(test())
"
```

### 3. Load Testing
```python
# load_test.py
import asyncio
import aiohttp
import time

async def test_endpoint(session, url):
    async with session.get(url) as response:
        return response.status

async def load_test(concurrent=10, total=100):
    url = "http://localhost:3100/status"
    async with aiohttp.ClientSession() as session:
        start = time.time()
        tasks = []
        for i in range(total):
            task = test_endpoint(session, url)
            tasks.append(task)
            if len(tasks) >= concurrent:
                await asyncio.gather(*tasks)
                tasks = []
        if tasks:
            await asyncio.gather(*tasks)
        duration = time.time() - start
        print(f"Completed {total} requests in {duration:.2f}s")
        print(f"Rate: {total/duration:.2f} req/s")

asyncio.run(load_test())
```

## Monitoring

### 1. Health Check Endpoint
```python
# Add to integrated_server.py
@server.get("/health")
async def health_check():
    return {
        "status": "healthy",
        "timestamp": time.time(),
        "components": {
            "com_api": server.is_connected,
            "vision": server.screen_capture is not None,
            "websocket": server.websocket_monitor is not None
        }
    }
```

### 2. Logging Configuration
```python
import structlog

structlog.configure(
    processors=[
        structlog.stdlib.filter_by_level,
        structlog.stdlib.add_logger_name,
        structlog.stdlib.add_log_level,
        structlog.stdlib.PositionalArgumentsFormatter(),
        structlog.processors.TimeStamper(fmt="iso"),
        structlog.processors.StackInfoRenderer(),
        structlog.processors.format_exc_info,
        structlog.processors.UnicodeDecoder(),
        structlog.processors.JSONRenderer()
    ],
    context_class=dict,
    logger_factory=structlog.stdlib.LoggerFactory(),
    cache_logger_on_first_use=True,
)
```

### 3. Metrics Collection
```python
# metrics.py
from prometheus_client import Counter, Histogram, Gauge, start_http_server

# Define metrics
analysis_counter = Counter('orcawave_analyses_total', 'Total analyses run')
analysis_duration = Histogram('orcawave_analysis_duration_seconds', 'Analysis duration')
active_connections = Gauge('orcawave_active_connections', 'Active WebSocket connections')

# Start metrics server
start_http_server(8000)  # Prometheus metrics on port 8000
```

### 4. Windows Performance Monitoring
```powershell
# Monitor process
Get-Process python | Select-Object CPU, WorkingSet, HandleCount

# Monitor network
netstat -an | findstr :3100
netstat -an | findstr :8765
```

## Troubleshooting

### Common Issues and Solutions

#### 1. COM API Connection Failed
```
Error: Cannot connect to OrcaWave COM server
```
**Solution:**
- Ensure OrcaWave is installed and licensed
- Run as Administrator
- Check Windows Firewall settings
- Verify COM registration: `regsvr32 OrcaWave.dll`

#### 2. WebSocket Connection Refused
```
Error: WebSocket connection to ws://localhost:8765 failed
```
**Solution:**
- Check port availability: `netstat -an | findstr :8765`
- Verify firewall allows port 8765
- Check WebSocket dependencies installed

#### 3. Screenshot Capture Fails
```
Error: Could not capture OrcaWave window
```
**Solution:**
- Ensure OrcaWave window is visible
- Check display scaling settings (set to 100%)
- Verify pywin32 installed correctly
- Run with elevated permissions

#### 4. High Memory Usage
**Solution:**
- Limit parallel analyses in config
- Enable result caching
- Implement cleanup in long-running processes
- Monitor with: `python -m memory_profiler script.py`

#### 5. License Server Issues
```
Error: OrcaWave license not available
```
**Solution:**
- Verify license server connectivity
- Check license usage: `lmutil lmstat -a`
- Ensure correct license server in config
- Contact Orcina support

### Debug Mode
Enable detailed logging:
```bash
# Set environment variable
set ORCAWAVE_DEBUG=true
set LOG_LEVEL=DEBUG

# Run with debug output
python mcp/orcawave/run_server.py --debug
```

## Production Checklist

### Pre-Deployment
- [ ] All tests passing
- [ ] Configuration reviewed and secured
- [ ] Firewall rules configured
- [ ] SSL/TLS certificates (if needed)
- [ ] Backup strategy defined
- [ ] Monitoring configured
- [ ] Documentation updated

### Security
- [ ] API authentication enabled
- [ ] CORS properly configured
- [ ] Sensitive data encrypted
- [ ] Access logs enabled
- [ ] Rate limiting configured
- [ ] Input validation implemented

### Performance
- [ ] Caching configured
- [ ] Connection pooling enabled
- [ ] Resource limits set
- [ ] Cleanup jobs scheduled
- [ ] Database indexes created (if applicable)

### Operations
- [ ] Service auto-start configured
- [ ] Log rotation set up
- [ ] Alerting configured
- [ ] Backup procedures tested
- [ ] Recovery procedures documented
- [ ] Contact list updated

### Validation
- [ ] End-to-end testing completed
- [ ] Load testing performed
- [ ] Security scan completed
- [ ] User acceptance testing done
- [ ] Performance benchmarks met

## Support

### Logs Location
- Application logs: `logs/orcawave-mcp.log`
- Error logs: `logs/error.log`
- Access logs: `logs/access.log`

### Getting Help
1. Check logs for detailed error messages
2. Review troubleshooting section
3. Search existing issues on GitHub
4. Create new issue with:
   - Error message
   - Configuration (sanitized)
   - Steps to reproduce
   - System information

### Contact
- Internal Support: support@company.com
- Orcina Support: support@orcina.com
- Development Team: dev-team@company.com

## Appendix

### A. Performance Tuning

#### Windows TCP/IP Settings
```powershell
# Increase TCP connection limit
netsh int ipv4 set dynamicport tcp start=10000 num=50000

# Optimize TCP settings
netsh int tcp set global autotuninglevel=normal
netsh int tcp set global chimney=enabled
```

#### Python Optimization
```python
# Use process pool for CPU-bound tasks
from concurrent.futures import ProcessPoolExecutor

executor = ProcessPoolExecutor(max_workers=4)

# Use asyncio for I/O-bound tasks
import asyncio
import aiofiles

async def process_files():
    async with aiofiles.open('file.txt') as f:
        content = await f.read()
```

### B. Backup and Recovery

#### Backup Script
```powershell
# backup.ps1
$date = Get-Date -Format "yyyyMMdd"
$source = "D:\digitalmodel\mcp\orcawave"
$destination = "D:\backups\orcawave-mcp-$date.zip"

Compress-Archive -Path $source -DestinationPath $destination
```

#### Recovery Procedure
1. Stop the service
2. Restore from backup
3. Verify configuration
4. Test connectivity
5. Restart service
6. Validate operation