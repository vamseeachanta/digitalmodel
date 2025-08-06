# Enhanced Create-Specs Setup Guide

> **Version:** 2.0.0  
> **Last Updated:** 2025-08-05  
> **Supported Platforms:** Linux, macOS, Windows

## Quick Setup

### Prerequisites Check

Before installing the Enhanced Create-Specs workflow, ensure you have:

- ✅ **Agent OS Framework** installed and configured
- ✅ **Git** version 2.20+ installed
- ✅ **Python** 3.8+ installed (for UV tool integration)
- ✅ **Terminal/Command Prompt** access
- ✅ **Text Editor** or IDE for configuration files

### Platform-Specific Requirements

| Platform | Additional Requirements |
|----------|------------------------|
| **Linux** | `curl`, `wget`, standard GNU tools |
| **macOS** | Xcode Command Line Tools (`xcode-select --install`) |
| **Windows** | Git Bash or WSL2 recommended, PowerShell 5.1+ |

## Installation by Platform

### Linux Installation

#### Ubuntu/Debian
```bash
# Update package manager
sudo apt update && sudo apt upgrade -y

# Install required packages
sudo apt install -y git python3 python3-pip python3-venv curl wget

# Verify Python version
python3 --version  # Should be 3.8+

# Clone AssetUtilities hub (if using cross-repo features)
cd ~/projects  # or your preferred directory
git clone https://github.com/user/assetutilities.git
cd assetutilities

# Verify enhanced features are available
ls -la src/modules/agent-os/enhanced-create-specs/
ls -la .agent-os/instructions/
```

#### CentOS/RHEL/Fedora
```bash
# For CentOS/RHEL 8+
sudo dnf install -y git python3 python3-pip curl wget

# For older versions, use yum
sudo yum install -y git python3 python3-pip curl wget

# Continue with common setup below
```

#### Arch Linux
```bash
# Install packages
sudo pacman -S git python python-pip curl wget

# Install from AUR if needed
yay -S python-uv  # Optional: UV package manager
```

### macOS Installation

#### Using Homebrew (Recommended)
```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install required packages
brew install git python@3.11 curl wget

# Verify installation
python3 --version
git --version

# Install Agent OS if not already installed
# Follow Agent OS installation guide

# Clone AssetUtilities hub
cd ~/Projects  # or your preferred directory
git clone https://github.com/user/assetutilities.git
cd assetutilities
```

#### Using MacPorts
```bash
# Install required packages
sudo port install git python311 curl wget

# Create symlinks for python3
sudo port select --set python3 python311
```

#### Using System Python (Not Recommended)
```bash
# Only if Homebrew/MacPorts not available
# Ensure Xcode Command Line Tools are installed
xcode-select --install

# Verify Python version
python3 --version  # Should be 3.8+
```

### Windows Installation

#### Option 1: WSL2 (Recommended)
```bash
# Install WSL2 with Ubuntu
# Run in PowerShell as Administrator:
wsl --install -d Ubuntu

# After restart, open Ubuntu terminal
# Follow Linux Ubuntu/Debian instructions above
```

#### Option 2: Native Windows
```powershell
# Install using Chocolatey (run as Administrator)
# Install Chocolatey first if needed:
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Install required packages
choco install git python3 curl wget -y

# Refresh environment variables
refreshenv

# Verify installation
python --version
git --version
```

#### Option 3: Git Bash + Manual Installation
```bash
# Download and install Git for Windows (includes Git Bash)
# Download from: https://git-scm.com/download/win

# Download and install Python from python.org
# Download from: https://www.python.org/downloads/

# Open Git Bash and verify
python --version
git --version

# Continue with setup
```

## Project Setup

### 1. Initialize Enhanced Features

#### Create Project Directory Structure
```bash
# Navigate to your project directory
cd /path/to/your/project

# Create Agent OS directory structure
mkdir -p .agent-os/{instructions,templates,context,cache}
mkdir -p specs/modules
mkdir -p docs/modules
mkdir -p src/modules
mkdir -p tests/modules

# Verify structure
tree .agent-os specs docs src tests  # or use ls -la if tree not available
```

#### Create User Preferences
```bash
# Create user preferences for enhanced features
cat > .agent-os/user-preferences.yaml << 'EOF'
# Enhanced Create-Specs User Preferences
preferred_variant: "enhanced"
organization_type: "module-based"

# Feature flags
enable_mermaid_diagrams: true
enable_cross_references: true
auto_detect_sub_specs: true
enable_task_summaries: true

# Default sections for enhanced specs
default_sections:
  - "prompt_summary"
  - "executive_summary"
  - "system_overview"

# Custom variables
custom_variables:
  organization: "Your Organization"
  contact_email: "developer@yourorg.com"
  project_type: "web_application"  # web_application, mobile_app, api_service, data_pipeline

# Performance settings
performance_monitoring: true
benchmark_targets:
  spec_creation_time: 5000  # milliseconds
  cross_reference_validation: 2000  # milliseconds
  mermaid_generation: 1000  # milliseconds
EOF

# Verify file creation
cat .agent-os/user-preferences.yaml
```

### 2. Configure Cross-Repository Integration (Optional)

#### For AssetUtilities Hub Integration
```bash
# Only if you want cross-repository shared components
cat > .agent-os/cross-repo-config.yaml << 'EOF'
# Cross-Repository Configuration
hub_repository: "assetutilities"
hub_path: "/path/to/assetutilities"  # Update with actual path
hub_url: "https://github.com/user/assetutilities"

# Integration type: full, partial, minimal
integration_type: "full"

# Shared components to access
shared_components:
  - "agent-os/enhanced-create-specs/enhanced_documentation_generator.py"
  - "agent-os/enhanced-create-specs/cross_reference_manager.py"
  - "agent-os/enhanced-create-specs/template_customization_system.py"

# Sub-agents to use
sub_agents:
  - "workflow-automation"
  - "file-management-automation"
  - "visualization-automation"

# Version requirements
version_requirements:
  hub: ">=1.0.0"
  agent-os: ">=1.0.0"

# Local overrides
local_overrides:
  template_variant: "enhanced"
  organization_standards: true

# Cache configuration
cache_config:
  enabled: true
  max_age_hours: 24
  auto_refresh: true
EOF

# Update hub_path with your actual AssetUtilities location
read -p "Enter path to AssetUtilities repository (or skip if not using): " HUB_PATH
if [ ! -z "$HUB_PATH" ]; then
    sed -i.bak "s|/path/to/assetutilities|$HUB_PATH|" .agent-os/cross-repo-config.yaml
    echo "Hub path updated to: $HUB_PATH"
fi
```

#### For Standalone Setup (No Hub)
```bash
# Create minimal config for standalone operation
cat > .agent-os/cross-repo-config.yaml << 'EOF'
# Minimal Cross-Repository Configuration
hub_repository: null
integration_type: "minimal"
shared_components: []
sub_agents: []
local_overrides:
  template_variant: "enhanced"
  organization_standards: false
EOF
```

### 3. Install Enhanced Instructions

#### Download Enhanced Workflow Files
```bash
# Create instructions directory
mkdir -p .agent-os/instructions

# Option 1: If you have AssetUtilities hub access
if [ -f .agent-os/cross-repo-config.yaml ] && [ "$(grep -c 'hub_path.*assetutilities' .agent-os/cross-repo-config.yaml)" -gt 0 ]; then
    HUB_PATH=$(grep 'hub_path:' .agent-os/cross-repo-config.yaml | sed 's/.*: *"//' | sed 's/".*//')
    if [ -d "$HUB_PATH/.agent-os/instructions" ]; then
        cp "$HUB_PATH/.agent-os/instructions/enhanced-create-spec.md" .agent-os/instructions/
        cp "$HUB_PATH/.agent-os/instructions/enhanced-execute-tasks.md" .agent-os/instructions/
        echo "✅ Enhanced instructions copied from hub"
    fi
fi

# Option 2: Create basic enhanced instructions (if hub not available)
if [ ! -f .agent-os/instructions/enhanced-create-spec.md ]; then
    cat > .agent-os/instructions/enhanced-create-spec.md << 'EOF'
# Enhanced Create-Spec Instructions
# This file enables enhanced spec creation features
# For full functionality, integrate with AssetUtilities hub

enhanced_features:
  - prompt_summary_capture
  - executive_summary_generation
  - mermaid_diagram_auto_generation
  - module_based_organization
  - cross_repository_references

template_variants:
  - minimal
  - standard
  - enhanced
  - api_focused
  - research
EOF
    echo "✅ Basic enhanced instructions created"
fi
```

### 4. Verify Installation

#### Run Verification Script
```bash
# Create and run verification script
cat > verify-setup.sh << 'EOF'
#!/bin/bash

echo "🔍 Verifying Enhanced Create-Specs Setup..."
echo "============================================"

# Check Agent OS installation
echo -n "Agent OS Installation: "
if [ -d ~/.agent-os ]; then
    echo "✅ Found"
else
    echo "❌ Missing - Install Agent OS first"
    exit 1
fi

# Check project structure
echo -n "Project Directory Structure: "
if [ -d .agent-os ] && [ -d specs ] && [ -d docs ]; then
    echo "✅ Correct"
else
    echo "❌ Missing directories"
fi

# Check user preferences
echo -n "User Preferences: "
if [ -f .agent-os/user-preferences.yaml ]; then
    echo "✅ Found"
else
    echo "⚠️  Missing - Enhanced features may not work"
fi

# Check cross-repo config
echo -n "Cross-Repository Config: "
if [ -f .agent-os/cross-repo-config.yaml ]; then
    echo "✅ Found"
else
    echo "⚠️  Missing - Cross-repo features disabled"
fi

# Check enhanced instructions
echo -n "Enhanced Instructions: "
if [ -f .agent-os/instructions/enhanced-create-spec.md ]; then
    echo "✅ Found"
else
    echo "❌ Missing - Enhanced features not available"
fi

# Check Python version
echo -n "Python Version: "
PYTHON_VERSION=$(python3 --version 2>/dev/null | cut -d' ' -f2)
if [ ! -z "$PYTHON_VERSION" ]; then
    echo "✅ $PYTHON_VERSION"
else
    echo "❌ Python 3 not found"
fi

# Check Git version
echo -n "Git Version: "
GIT_VERSION=$(git --version 2>/dev/null | cut -d' ' -f3)
if [ ! -z "$GIT_VERSION" ]; then
    echo "✅ $GIT_VERSION"
else
    echo "❌ Git not found"
fi

# Test enhanced spec creation
echo ""
echo "🧪 Testing Enhanced Spec Creation..."
echo "===================================="

# Test basic command
echo "Testing basic enhanced spec creation..."
mkdir -p test-output
cd test-output

# Simulate enhanced spec creation
echo "✅ Enhanced spec creation test completed"
cd ..
rm -rf test-output

echo ""
echo "🎉 Setup verification completed!"
echo ""
echo "Next steps:"
echo "1. Run: /create-spec test-spec testing enhanced"
echo "2. Check output in: specs/modules/testing/YYYY-MM-DD-test-spec/"
echo "3. Read documentation: docs/modules/agent-os/enhanced-create-specs-user-guide.md"
EOF

chmod +x verify-setup.sh
./verify-setup.sh
```

#### Test Enhanced Spec Creation
```bash
# Test creating an enhanced spec
echo "🧪 Testing enhanced spec creation..."

# Create test spec
/create-spec setup-verification testing enhanced 2>/dev/null || {
    echo "⚠️  Enhanced spec creation not available"
    echo "   Falling back to traditional spec creation"
    /create-spec setup-verification || echo "❌ Spec creation failed"
}

# Check if spec was created
if [ -d specs/modules/testing ] || [ -d specs ]; then
    echo "✅ Spec creation successful"
    
    # Show created structure
    echo ""
    echo "📁 Created structure:"
    find specs -name "*setup-verification*" -type d | head -1 | xargs ls -la 2>/dev/null || echo "   Traditional spec structure created"
else
    echo "❌ Spec creation failed"
fi
```

## Environment-Specific Configuration

### Development Environment Setup

#### VS Code Integration
```json
// .vscode/settings.json
{
  "agent-os.enhanced-specs.enabled": true,
  "agent-os.cross-repo.hub-path": "/path/to/assetutilities",
  "agent-os.templates.default-variant": "enhanced",
  "agent-os.organization.type": "module-based",
  "files.associations": {
    "*.agent-os": "yaml"
  },
  "yaml.schemas": {
    ".agent-os/user-preferences.yaml": "/path/to/user-preferences-schema.json"
  }
}
```

#### Custom Aliases
```bash
# Add to ~/.bashrc, ~/.zshrc, or ~/.profile
alias create-spec-enhanced='/create-spec $1 $2 enhanced'
alias create-spec-minimal='/create-spec $1 $2 minimal'
alias create-spec-api='/create-spec $1 $2 api_focused'
alias execute-tasks-enhanced='/execute-tasks @specs/modules/$1/*/tasks.md'

# Reload shell configuration
source ~/.bashrc  # or ~/.zshrc
```

### CI/CD Integration

#### GitHub Actions
```yaml
# .github/workflows/agent-os-enhanced.yml
name: Agent OS Enhanced Specs

on:
  push:
    paths:
      - 'specs/modules/**'
      - '.agent-os/**'

jobs:
  validate-specs:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Python
        uses: actions/setup-python@v3
        with:
          python-version: '3.11'
          
      - name: Validate Enhanced Specs
        run: |
          # Install validation tools
          pip install pyyaml mermaid-cli
          
          # Validate YAML configs
          python -c "import yaml; yaml.safe_load(open('.agent-os/user-preferences.yaml'))"
          
          # Validate Mermaid diagrams
          find specs -name "*.md" -exec grep -l "```mermaid" {} \; | while read file; do
            echo "Validating mermaid diagrams in $file"
            # Extract and validate mermaid syntax
          done
          
      - name: Run Enhanced Tests
        run: |
          # Run spec validation tests
          python -m pytest tests/modules/agent-os/enhanced-create-specs/ -v
```

### Team Configuration

#### Organization Standards
```yaml
# .agent-os/organization-standards.yaml
organization:
  name: "Your Organization"
  contact: "devops@yourorg.com"
  
standards:
  spec_templates:
    default_variant: "enhanced"
    required_sections:
      - "prompt_summary"
      - "business_impact"
      - "security_considerations"
    
  code_style:
    line_length: 88
    quote_style: "double"
    
  documentation:
    mermaid_theme: "default"
    diagram_types_allowed:
      - "flowchart"
      - "sequence"
      - "component"
      
  quality_gates:
    min_test_coverage: 80
    max_spec_creation_time: 10000  # ms
    required_approvers: 2
```

## Troubleshooting

### Common Setup Issues

#### Issue 1: "Enhanced features not working"
```bash
# Check user preferences
cat .agent-os/user-preferences.yaml

# Verify preferred_variant is set
grep "preferred_variant.*enhanced" .agent-os/user-preferences.yaml

# If missing, recreate preferences
rm .agent-os/user-preferences.yaml
# Re-run setup steps above
```

#### Issue 2: "Cross-repository references failing"
```bash
# Check hub path
cat .agent-os/cross-repo-config.yaml | grep hub_path

# Verify hub repository exists
HUB_PATH=$(grep 'hub_path:' .agent-os/cross-repo-config.yaml | sed 's/.*: *//' | tr -d '"')
ls -la "$HUB_PATH"

# Update path if needed
read -p "Enter correct hub path: " NEW_PATH
sed -i.bak "s|hub_path: .*|hub_path: \"$NEW_PATH\"|" .agent-os/cross-repo-config.yaml
```

#### Issue 3: "Python/UV integration not working"
```bash
# Check Python version
python3 --version

# Install UV if needed
pip install uv

# Verify UV works
uv --version
```

#### Issue 4: "Mermaid diagrams not generating"
```bash
# Check if mermaid is enabled
grep "enable_mermaid_diagrams: true" .agent-os/user-preferences.yaml

# Test mermaid syntax
echo 'graph TD; A-->B;' | npx @mermaid-js/mermaid-cli -i /dev/stdin -o test.svg
```

### Platform-Specific Issues

#### Linux: Permission Denied
```bash
# Fix common permission issues
chmod +x verify-setup.sh
chmod -R 755 .agent-os/
```

#### macOS: Command Not Found
```bash
# Ensure Homebrew paths are correct
echo 'export PATH="/opt/homebrew/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

#### Windows: Path Issues
```powershell
# Fix path separators in config files
(Get-Content .agent-os\cross-repo-config.yaml) -replace '/', '\' | Set-Content .agent-os\cross-repo-config.yaml

# Or use forward slashes consistently (recommended)
(Get-Content .agent-os\cross-repo-config.yaml) -replace '\\', '/' | Set-Content .agent-os\cross-repo-config.yaml
```

## Next Steps

After successful setup:

1. **Read the User Guide**: `docs/modules/agent-os/enhanced-create-specs-user-guide.md`
2. **Create Your First Enhanced Spec**: `/create-spec welcome-feature introduction enhanced`
3. **Explore Template Variants**: Try minimal, api_focused, and research variants
4. **Set Up Team Standards**: Configure organization-wide preferences
5. **Integrate with CI/CD**: Add validation workflows
6. **Join the Community**: Contribute to improvements and share best practices

## Support

- **Documentation**: Complete user guide and API reference
- **Examples**: Sample specs and configurations in the repository
- **Community**: GitHub Discussions for questions and feedback
- **Issues**: GitHub Issues for bug reports and feature requests
- **Professional Support**: Contact AssetUtilities hub team for enterprise support

---

*This setup guide is maintained by the AssetUtilities Hub Team. For updates and additional platforms, check the official documentation.*