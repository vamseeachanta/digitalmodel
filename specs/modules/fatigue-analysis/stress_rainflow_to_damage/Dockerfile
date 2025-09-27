# Dockerfile for Fatigue Analysis Module
FROM python:3.10-slim

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    gcc \
    g++ \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements
COPY requirements.txt .

# Install Python dependencies
RUN pip install --no-cache-dir -r requirements.txt

# Copy application code
COPY run_damage_analysis.py .
COPY monitor_fatigue_health.py .
COPY damage_analysis_config_production.yml .

# Copy any additional modules if needed
# COPY modules/ modules/

# Create output directories
RUN mkdir -p /data/input /data/output/damage_results /data/output/visualizations /data/output/reports /data/monitoring_reports

# Set environment variables
ENV PYTHONUNBUFFERED=1
ENV DATA_PATH=/data

# Default command
CMD ["python", "run_damage_analysis.py", "--config", "damage_analysis_config_production.yml"]

# Alternative: Run monitoring
# CMD ["python", "monitor_fatigue_health.py", "--summary", "/data/output/reports/damage_analysis_summary.csv"]