#!/bin/bash
# GMSH Agent Development Environment Setup Script

echo "Setting up GMSH Agent development environment..."

# Check Python version
python_version=$(python3 --version 2>&1 | grep -oE '[0-9]+\.[0-9]+')
required_version="3.10"

if [ "$(printf '%s\n' "$required_version" "$python_version" | sort -V | head -n1)" != "$required_version" ]; then
    echo "Error: Python 3.10+ is required. Current version: $python_version"
    exit 1
fi

# Check if uv is available
if command -v uv &> /dev/null; then
    echo "Using uv for package management..."
    
    # Install dependencies
    echo "Installing GMSH and dependencies..."
    uv pip install gmsh numpy scipy pyvista pyyaml click
    
    # Install development dependencies
    echo "Installing development tools..."
    uv pip install pytest pytest-cov black flake8 mypy
else
    echo "uv not found, using pip..."
    
    # Create virtual environment if not exists
    if [ ! -d "venv" ]; then
        echo "Creating virtual environment..."
        python3 -m venv venv
    fi
    
    # Activate virtual environment
    source venv/bin/activate
    
    # Upgrade pip
    pip install --upgrade pip
    
    # Install dependencies
    echo "Installing GMSH and dependencies..."
    pip install gmsh numpy scipy pyvista pyyaml click
    
    # Install development dependencies
    echo "Installing development tools..."
    pip install pytest pytest-cov black flake8 mypy
fi

# Verify GMSH installation
echo "Verifying GMSH installation..."
python3 -c "import gmsh; print(f'GMSH version: {gmsh.__version__ if hasattr(gmsh, \"__version__\") else \"Unknown\"}')" 2>/dev/null

if [ $? -eq 0 ]; then
    echo "GMSH successfully installed!"
else
    echo "Warning: GMSH installation verification failed"
    echo "You may need to install GMSH system libraries"
    echo "Visit: https://gmsh.info/#Download"
fi

# Create necessary directories
echo "Creating directory structure..."
mkdir -p workflows templates utilities tests/unit tests/integration

# Copy environment file if not exists
if [ ! -f ".env" ]; then
    echo "Creating .env file from template..."
    cp .env.example .env
fi

# Create test file
echo "Creating test structure..."
cat > tests/test_gmsh_agent.py << 'EOF'
"""Basic tests for GMSH Agent"""
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from run_gmsh_agent import GMSHAgent

def test_agent_initialization():
    """Test agent can be initialized"""
    agent = GMSHAgent()
    assert agent is not None
    assert agent.config is not None

def test_version():
    """Test version retrieval"""
    agent = GMSHAgent()
    version = agent.get_version()
    assert version is not None
    agent.cleanup_gmsh()

def test_capabilities():
    """Test capabilities loading"""
    agent = GMSHAgent()
    caps = agent.show_capabilities()
    assert caps is not None
    assert 'name' in caps or 'status' in caps
EOF

echo "Setup complete!"
echo ""
echo "To activate the environment:"
echo "  source venv/bin/activate  (if using pip)"
echo ""
echo "To run the agent:"
echo "  python run_gmsh_agent.py --help"
echo ""
echo "To run tests:"
echo "  pytest tests/"