# Deployment Guide - Fatigue Analysis Module

**Module**: Stress Rainflow to Damage Analysis  
**Version**: 1.0.1  
**Status**: Production Ready  

---

## Quick Start

### 1. Installation

#### Option A: Using pip
```bash
cd specs/modules/fatigue-analysis/stress_rainflow_to_damage
pip install -e .
```

#### Option B: Using requirements.txt
```bash
cd specs/modules/fatigue-analysis/stress_rainflow_to_damage
pip install -r requirements.txt
```

#### Option C: Using Docker (recommended for production)
```bash
docker build -t fatigue-analysis:1.0.1 .
docker run -v /data:/data fatigue-analysis:1.0.1
```

### 2. Basic Usage

#### Run Damage Analysis
```bash
# Using script directly
python run_damage_analysis.py --config damage_analysis_config_production.yml

# Using installed command
fatigue-analyze --config damage_analysis_config_production.yml

# With custom parameters
fatigue-analyze --config my_config.yml --parallel 16 --verbose
```

#### Run Health Monitoring
```bash
# Using script directly
python monitor_fatigue_health.py --summary damage_analysis_summary.csv --display

# Using installed command
fatigue-monitor --summary damage_analysis_summary.csv --output reports/

# With custom thresholds
fatigue-monitor --summary summary.csv --thresholds custom_thresholds.json
```

---

## System Requirements

### Minimum Requirements
- Python 3.8 or higher
- 4 GB RAM
- 2 CPU cores
- 10 GB free disk space

### Recommended Requirements
- Python 3.10+
- 16 GB RAM
- 8+ CPU cores (for parallel processing)
- 50 GB free disk space
- SSD storage for faster I/O

### Operating Systems
- Linux (Ubuntu 20.04+, CentOS 8+)
- Windows 10/11
- macOS 10.15+

---

## Configuration

### 1. S-N Curve Parameters
Edit `damage_analysis_config_production.yml`:
```yaml
sn_curve:
  type: "ABS_E"
  parameters:
    log_a: 12.0170          # Critical: Must match ABS standard
    m: 3.0
    fatigue_limit_stress: 47.0  # MPa
    fatigue_limit_cycles: 1.0e7
    two_segment:
      enabled: true
      log_c: 15.378
      r: 5.0
```

### 2. Location Parameters
```yaml
locations:
  loc02:
    scf: 2.0               # Stress Concentration Factor
    thickness_mm: 25       # Plate thickness
    
  loc03:
    scf: 1.15
    thickness_mm: 25
```

### 3. Processing Options
```yaml
processing:
  parallel: true
  max_workers: 32          # Adjust based on CPU cores
  batch_size: 10
  memory_limit_gb: 8
```

### 4. Monitoring Thresholds
Create `thresholds.json`:
```json
{
  "critical_damage_rate": 0.1,
  "warning_damage_rate": 0.04,
  "notice_damage_rate": 0.02,
  "critical_stress_mpa": 45.0,
  "warning_stress_mpa": 40.0,
  "design_life_years": 25.0
}
```

---

## Directory Structure

### Expected Input Structure
```
project_root/
├── specs/
│   └── modules/
│       └── fatigue-analysis/
│           ├── stress_rainflow_to_damage/
│           │   ├── run_damage_analysis.py
│           │   ├── monitor_fatigue_health.py
│           │   └── damage_analysis_config_production.yml
│           └── reference-seastate-scale-load/
│               └── output/
│                   └── rainflow/
│                       └── stress_range/
│                           └── *.csv (rainflow input files)
```

### Generated Output Structure
```
output/
├── damage_results/
│   └── *_damage_rate.csv
├── visualizations/
│   └── *_damage_rate.png
├── reports/
│   └── damage_analysis_summary.csv
└── monitoring_reports/
    ├── fatigue_monitor_*.json
    └── fatigue_dashboard_*.txt
```

---

## Production Deployment Steps

### 1. Pre-Deployment Validation
```bash
# Run test suite
pytest tests/ -v --cov=.

# Validate configuration
python validate_config.py --config damage_analysis_config_production.yml

# Test with sample data
python run_damage_analysis.py --config damage_analysis_config_test.yml
```

### 2. Deploy to Production Server
```bash
# Clone repository
git clone https://github.com/company/digitalmodel.git
cd digitalmodel/specs/modules/fatigue-analysis/stress_rainflow_to_damage

# Create virtual environment
python -m venv venv
source venv/bin/activate  # Linux/Mac
# or
venv\Scripts\activate  # Windows

# Install module
pip install -e .
```

### 3. Set Up Automated Monitoring
```bash
# Create cron job (Linux/Mac)
crontab -e
# Add line:
0 6 * * * /path/to/venv/bin/python /path/to/monitor_fatigue_health.py --summary /path/to/summary.csv

# Or use systemd timer (Linux)
sudo cp fatigue-monitor.service /etc/systemd/system/
sudo cp fatigue-monitor.timer /etc/systemd/system/
sudo systemctl enable fatigue-monitor.timer
sudo systemctl start fatigue-monitor.timer
```

### 4. Configure Logging
```python
# In production_config.py
LOGGING = {
    'version': 1,
    'handlers': {
        'file': {
            'class': 'logging.handlers.RotatingFileHandler',
            'filename': '/var/log/fatigue-analysis.log',
            'maxBytes': 10485760,  # 10MB
            'backupCount': 5,
        }
    },
    'root': {
        'level': 'INFO',
        'handlers': ['file']
    }
}
```

---

## Performance Tuning

### CPU Optimization
```yaml
# Adjust based on available cores
processing:
  max_workers: 32  # Set to number of CPU cores
  chunk_size: 10   # Files per batch
```

### Memory Optimization
```python
# For low memory systems, process sequentially
processing:
  parallel: false
  
# Or limit workers
processing:
  max_workers: 4
```

### Disk I/O Optimization
- Use SSD for input/output directories
- Enable compression for large outputs:
```python
df.to_csv('output.csv.gz', compression='gzip')
```

---

## Monitoring & Maintenance

### Health Check Script
```bash
#!/bin/bash
# health_check.sh
python monitor_fatigue_health.py \
  --summary /data/latest/damage_analysis_summary.csv \
  --thresholds /config/thresholds.json \
  --output /reports/

if [ $? -eq 2 ]; then
  echo "CRITICAL: Immediate action required!"
  # Send alert
elif [ $? -eq 1 ]; then
  echo "WARNING: Review needed"
  # Send notification
fi
```

### Log Rotation
```bash
# /etc/logrotate.d/fatigue-analysis
/var/log/fatigue-analysis.log {
    daily
    rotate 7
    compress
    missingok
    notifempty
}
```

### Backup Strategy
```bash
# Daily backup of results
0 2 * * * tar -czf /backup/fatigue-$(date +\%Y\%m\%d).tar.gz /data/output/
```

---

## Troubleshooting

### Common Issues

#### Issue: "FileNotFoundError: Config file not found"
**Solution**: Ensure config file path is absolute or relative to current directory
```bash
python run_damage_analysis.py --config /absolute/path/to/config.yml
```

#### Issue: "MemoryError during processing"
**Solution**: Reduce parallel workers or process in batches
```yaml
processing:
  max_workers: 4  # Reduce from 32
  batch_size: 5   # Smaller batches
```

#### Issue: "ImportError: No module named pandas"
**Solution**: Install dependencies
```bash
pip install -r requirements.txt
```

#### Issue: "Permission denied" when writing outputs
**Solution**: Check directory permissions
```bash
chmod 755 /path/to/output/directory
chown user:group /path/to/output/directory
```

### Debug Mode
```bash
# Enable verbose logging
python run_damage_analysis.py --config config.yml --verbose --debug

# Test single file
python run_damage_analysis.py --config config.yml --single-file test.csv
```

---

## Security Considerations

### 1. Input Validation
- Validate all CSV inputs before processing
- Check file sizes to prevent DoS
- Sanitize filenames

### 2. Access Control
```bash
# Restrict access to configuration
chmod 600 damage_analysis_config_production.yml
chmod 600 thresholds.json
```

### 3. Sensitive Data
- Never commit production configs to version control
- Use environment variables for paths:
```bash
export FATIGUE_DATA_PATH=/secure/data/path
export FATIGUE_CONFIG=/secure/config/path
```

---

## Integration

### REST API Wrapper
```python
# api_wrapper.py
from flask import Flask, request, jsonify
import subprocess

app = Flask(__name__)

@app.route('/analyze', methods=['POST'])
def analyze():
    config = request.json['config']
    result = subprocess.run(
        ['fatigue-analyze', '--config', config],
        capture_output=True
    )
    return jsonify({'status': result.returncode, 'output': result.stdout})
```

### Database Integration
```python
# db_integration.py
import psycopg2
import pandas as pd

def save_to_database(summary_df, connection_string):
    conn = psycopg2.connect(connection_string)
    summary_df.to_sql('fatigue_results', conn, if_exists='append')
    conn.close()
```

---

## Validation Checklist

Before deploying to production, verify:

- [ ] All tests pass (100% critical path coverage)
- [ ] Configuration validated against ABS standard
- [ ] Sample data processed successfully
- [ ] Output directories exist and are writable
- [ ] Monitoring system generates alerts correctly
- [ ] Backup system configured
- [ ] Logging configured and working
- [ ] Performance meets requirements (>5 files/sec)
- [ ] Documentation updated
- [ ] Stakeholders notified

---

## Support

### Internal Resources
- Engineering Team: engineering@company.com
- IT Support: itsupport@company.com
- Documentation: wiki.company.com/fatigue-analysis

### External Resources
- ABS Guide for Fatigue Assessment
- Python documentation: docs.python.org
- GitHub repository: github.com/company/digitalmodel

### Emergency Contacts
- On-call Engineer: +1-XXX-XXX-XXXX
- System Administrator: +1-XXX-XXX-XXXX

---

**Deployment Version**: 1.0.1  
**Last Updated**: January 25, 2025  
**Next Review**: Quarterly or upon version update