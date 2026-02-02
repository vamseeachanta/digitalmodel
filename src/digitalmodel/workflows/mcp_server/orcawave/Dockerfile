# Multi-stage build for OrcaWave MCP Server
# Stage 1: Builder
FROM python:3.11-slim as builder

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    gcc \
    g++ \
    make \
    libffi-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements
COPY requirements.txt .
COPY pyproject.toml .
COPY setup.py .

# Install Python dependencies
RUN pip install --upgrade pip setuptools wheel
RUN pip wheel --no-cache-dir --no-deps --wheel-dir /build/wheels -r requirements.txt

# Stage 2: Runtime
FROM python:3.11-slim

# Metadata
LABEL maintainer="OrcaWave MCP Team"
LABEL version="1.0.0"
LABEL description="OrcaWave MCP Server with hybrid control and monitoring"

# Create non-root user
RUN groupadd -r orcawave && useradd -r -g orcawave orcawave

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libgomp1 \
    libglib2.0-0 \
    libsm6 \
    libxext6 \
    libxrender-dev \
    libgomp1 \
    wget \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Copy wheels from builder
COPY --from=builder /build/wheels /wheels

# Install Python packages
RUN pip install --no-cache /wheels/*

# Copy application code
COPY --chown=orcawave:orcawave src/mcp/orcawave /app/src/mcp/orcawave
COPY --chown=orcawave:orcawave mcp/orcawave /app/mcp/orcawave

# Create necessary directories
RUN mkdir -p /app/logs /app/data /app/cache && \
    chown -R orcawave:orcawave /app

# Environment variables
ENV PYTHONPATH=/app:$PYTHONPATH
ENV ORCAWAVE_LOG_DIR=/app/logs
ENV ORCAWAVE_DATA_DIR=/app/data
ENV ORCAWAVE_CACHE_DIR=/app/cache

# Expose ports
EXPOSE 3100 8765 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:3100/health || exit 1

# Switch to non-root user
USER orcawave

# Volume mounts
VOLUME ["/app/logs", "/app/data", "/app/cache"]

# Default command
CMD ["python", "/app/mcp/orcawave/run_server.py", "--standalone"]