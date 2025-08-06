# Enhanced Agent OS Troubleshooting Guide

> Version: 1.0.0
> Last Updated: 2025-08-06
> For Enhanced Create-Specs Module

## Quick Diagnosis

### Common Issues Checklist
- [ ] **Not seeing enhanced features?** → Check variant specification and user preferences
- [ ] **Module organization not working?** → Verify command format and preferences
- [ ] **Cross-repository references broken?** → Check configuration and network connectivity
- [ ] **Performance slower than expected?** → Consider template variant and caching
- [ ] **Mermaid diagrams not rendering?** → Verify markdown renderer and syntax

## Issue Categories

## 1. Enhanced Features Not Available

### Symptoms
- Specs created with traditional format despite enhanced commands
- Missing prompt summary, executive summary, or mermaid diagrams
- Module-based organization not working

### Diagnosis Steps
```bash
# Check if enhanced instructions exist
ls ~/.agent-os/instructions/enhanced-*

# Check current directory for enhanced workflow files  
ls .agent-os/instructions/enhanced-*

# Check user preferences
cat ~/.agent-os/user-preferences.yaml 2>/dev/null || echo "No user preferences found"

# Verify command format
echo "Last command used: [check your command history]"
```

### Solutions

#### Solution 1A: Enhanced Instructions Not Found
```bash
# Check if enhanced instructions are in the current project
ls .agent-os/instructions/enhanced-create-spec.md

# If not found, check installation
find . -name "enhanced-create-spec.md" -type f
```

**Fix**: Ensure enhanced instructions are available in `.agent-os/instructions/`

#### Solution 1B: Incorrect Command Format
```bash
# Wrong: /create-spec my-feature
# Right: /create-spec my-feature my-module enhanced

# Or set user preferences:
cat > ~/.agent-os/user-preferences.yaml << EOF
preferred_variant: "enhanced"
organization_type: "module-based"
enable_mermaid_diagrams: true
EOF
```

#### Solution 1C: Variant Not Specified
```bash
# Explicitly specify enhanced variant
/create-spec feature-name module-name enhanced

# Available variants: minimal, standard, enhanced, api_focused, research
```

## 2. Module Organization Issues

### Symptoms
- Specs still created in date-based folders (`specs/2025-08-06-feature/`)
- Module folders not being created (`specs/modules/module-name/`)
- Inconsistent module naming

### Diagnosis Steps
```bash
# Check current spec organization
find specs/ -type d -name "*2025*" | head -5
find specs/modules/ -type d 2>/dev/null | head -5

# Check user preferences for organization type
grep "organization_type" ~/.agent-os/user-preferences.yaml 2>/dev/null

# Check command history
history | grep "/create-spec" | tail -3
```

### Solutions

#### Solution 2A: Force Module Organization
```bash
# Explicitly specify module name in command
/create-spec feature-name module-name enhanced

# Set preference for future specs
echo "organization_type: module-based" >> ~/.agent-os/user-preferences.yaml
```

#### Solution 2B: Module Name Validation
```bash
# Valid module names (kebab-case)
# ✓ user-authentication
# ✓ payment-processing  
# ✓ api-gateway
# ✗ UserAuthentication (PascalCase)
# ✗ user_authentication (snake_case)
```

#### Solution 2C: Directory Structure Repair
```bash
# Manual migration if needed
mkdir -p specs/modules/my-module
mv specs/2025-08-06-my-feature specs/modules/my-module/
```

## 3. Cross-Repository Reference Issues

### Symptoms
- `@assetutilities:` references showing as broken or unresolved
- Cross-repository components not loading
- Version compatibility warnings

### Diagnosis Steps
```bash
# Check cross-repository configuration
ls .agent-os/cross-repo-config.yaml
cat .agent-os/cross-repo-config.yaml

# Test network connectivity to hub
ping -c 1 github.com 2>/dev/null && echo "Network OK" || echo "Network issue"

# Check if AssetUtilities hub is accessible
ls -la ../assetutilities 2>/dev/null && echo "Local hub found" || echo "Hub not found locally"
```

### Solutions

#### Solution 3A: Missing Configuration
```bash
# Create basic cross-repository configuration
cat > .agent-os/cross-repo-config.yaml << EOF
repositories:
  assetutilities:
    url: "https://github.com/vamseeachanta/assetutilities"
    local_path: "../assetutilities"
    version: "main"
    
cache:
  enabled: true
  ttl: 3600
  
offline_mode: true
EOF
```

#### Solution 3B: Network/Access Issues
```bash
# Enable offline mode
echo "offline_mode: true" >> .agent-os/cross-repo-config.yaml

# Use cached components
mkdir -p .agent-os/cache/assetutilities
# Manually copy needed components if available locally
```

#### Solution 3C: Version Compatibility
```bash
# Check version requirements in spec files
grep -r "@assetutilities:" specs/modules/*/

# Update version requirements if needed
# Edit cross-repo-config.yaml to specify compatible versions
```

## 4. Performance Issues

### Symptoms
- Spec creation taking longer than expected (>5 seconds)
- High memory usage during spec generation
- Timeouts during cross-repository operations

### Diagnosis Steps
```bash
# Measure spec creation time
time /create-spec test-perf test-module minimal

# Check system resources
free -h
df -h .

# Check for large files in .agent-os/cache/
du -sh .agent-os/cache/* 2>/dev/null
```

### Solutions

#### Solution 4A: Use Lighter Template Variants
```bash
# For simple specs, use minimal variant
/create-spec feature-name module-name minimal

# Template performance ranking (fastest to slowest):
# 1. minimal
# 2. standard  
# 3. api_focused
# 4. research
# 5. enhanced
```

#### Solution 4B: Clear Cache
```bash
# Clear cross-repository cache
rm -rf .agent-os/cache/
mkdir .agent-os/cache/

# Clear temporary files
rm -rf /tmp/agent-os-*
```

#### Solution 4C: Disable Expensive Features
```bash
# Temporarily disable mermaid generation
cat > ~/.agent-os/user-preferences.yaml << EOF
enable_mermaid_diagrams: false
enable_cross_references: false
auto_detect_sub_specs: false
EOF
```

## 5. Visual Documentation Issues

### Symptoms
- Mermaid diagrams not rendering in markdown viewers
- Diagram syntax errors
- Missing or incorrect visual documentation

### Diagnosis Steps
```bash
# Check if mermaid syntax is valid
grep -A 10 "```mermaid" specs/modules/*/spec.md

# Test mermaid rendering online
echo "Copy diagram code and test at: https://mermaid.live/"

# Check markdown renderer support
echo "Verify your markdown viewer supports mermaid"
```

### Solutions

#### Solution 5A: Syntax Validation
```bash
# Common mermaid syntax issues:
# ✗ graph TD (should be: graph TB or graph TD)
# ✗ Missing semicolons in sequence diagrams
# ✗ Invalid characters in node names

# Test diagram online before using
# Visit: https://mermaid.live/
```

#### Solution 5B: Renderer Compatibility
```bash
# For GitHub: Mermaid is natively supported
# For VS Code: Install Mermaid Preview extension
# For other editors: Check mermaid support

# Alternative: Include mermaid as images
# Generate image: mmdc -i diagram.mmd -o diagram.png
```

#### Solution 5C: Disable Problematic Diagrams
```bash
# Temporarily disable mermaid generation
echo "enable_mermaid_diagrams: false" >> ~/.agent-os/user-preferences.yaml

# Manually add diagrams later after troubleshooting
```

## 6. Task Summary Integration Issues

### Symptoms
- `task_summary.md` not created automatically
- Task tracking not working during execution
- Missing implementation details in task summaries

### Diagnosis Steps
```bash
# Check if task_summary.md template exists
ls specs/modules/*/task_summary.md

# Check if enhanced execute-tasks is available
ls .agent-os/instructions/enhanced-execute-tasks.md

# Verify spec was created with enhanced variant
grep -l "Prompt Summary" specs/modules/*/spec.md
```

### Solutions

#### Solution 6A: Manual Template Creation
```bash
# Create task summary template manually
cat > specs/modules/module-name/spec-name/task_summary.md << EOF
# Task Summary

> Created: $(date +%Y-%m-%d)
> Module: module-name
> Spec: spec-name

## Implementation Tracking

_This file tracks implementation details, technical decisions, and performance metrics during task execution._

## Technical Decisions

## Performance Metrics

## Lessons Learned

## Next Steps
EOF
```

#### Solution 6B: Enable Enhanced Execution
```bash
# Use enhanced execute-tasks workflow
/execute-tasks @specs/modules/module-name/spec-name/tasks.md

# Check if enhanced task tracking is working
grep "task_summary.md" .agent-os/instructions/enhanced-execute-tasks.md
```

## 7. Template Customization Issues

### Symptoms
- Template variants not working as expected
- Custom fields not appearing in generated specs
- Template inheritance issues

### Diagnosis Steps
```bash
# List available template variants
grep -r "variant" .agent-os/instructions/enhanced-create-spec.md

# Check template selection logic
echo "Command used: /create-spec name module variant"

# Verify template files exist
find . -name "*template*" -type f
```

### Solutions

#### Solution 7A: Explicit Variant Selection
```bash
# Always specify variant explicitly
/create-spec feature-name module-name enhanced

# Available variants:
# - minimal: Basic spec with core sections
# - standard: Traditional Agent OS format  
# - enhanced: Full features with summaries and diagrams
# - api_focused: API development specialized
# - research: Exploratory work format
```

#### Solution 7B: Template Validation
```bash
# Check if template is being applied correctly
grep -A 5 "## Prompt Summary" specs/modules/*/spec.md

# Enhanced templates should include:
# - Prompt Summary section
# - Executive Summary section  
# - System Overview with mermaid diagrams
```

## 8. Backward Compatibility Issues

### Symptoms
- Existing specs not working with new workflow
- Traditional commands failing
- CLAUDE.md template conflicts

### Diagnosis Steps
```bash
# Check existing spec structure
find specs/ -name "spec.md" -exec head -5 {} \;

# Verify traditional commands still work
/create-spec test-traditional

# Check CLAUDE.md template
grep -A 10 "Agent OS" CLAUDE.md
```

### Solutions

#### Solution 8A: Maintain Traditional Workflow
```bash
# Traditional Agent OS continues to work exactly as before
/create-spec feature-name

# This creates traditional date-based structure
# No migration required for existing specs
```

#### Solution 8B: Gradual Migration
```bash
# Keep existing specs unchanged
# Use enhanced workflow for new specs only
# Migrate individual specs as needed (optional)
```

## Emergency Rollback Procedures

### Immediate Rollback (< 5 minutes)
```bash
# 1. Stop using enhanced commands
# Use: /create-spec feature-name
# Instead of: /create-spec feature-name module-name enhanced

# 2. Remove user preferences (if any)
rm ~/.agent-os/user-preferences.yaml

# 3. Use traditional workflow
# Everything continues to work as before
```

### Selective Rollback (Specific Features)
```bash
# Disable specific enhanced features
cat > ~/.agent-os/user-preferences.yaml << EOF
preferred_variant: "standard"
organization_type: "traditional"
enable_mermaid_diagrams: false
enable_cross_references: false
EOF
```

### Complete Rollback (Nuclear Option)
```bash
# Backup enhanced files
mkdir backup-enhanced-$(date +%Y%m%d)
cp -r .agent-os/instructions/enhanced-* backup-enhanced-*/

# Remove enhanced instructions
rm .agent-os/instructions/enhanced-*

# Remove configuration
rm .agent-os/cross-repo-config.yaml
rm ~/.agent-os/user-preferences.yaml

# All existing specs (traditional and enhanced) continue to work
# New specs will use traditional workflow only
```

## Getting Additional Help

### Self-Service Resources
1. **Migration Guide**: @docs/modules/agent-os/enhanced-create-specs-migration-guide.md
2. **User Guide**: @docs/modules/agent-os/enhanced-create-specs-user-guide.md
3. **Technical Specification**: @specs/modules/agent-os/enhanced-create-specs/sub-specs/technical-spec.md

### Escalation Path
1. **Level 1**: Check this troubleshooting guide
2. **Level 2**: Consult team lead or experienced Agent OS users
3. **Level 3**: Create GitHub issue with reproduction steps
4. **Level 4**: Emergency rollback if productivity is affected

### Issue Reporting Template
```markdown
## Issue Description
[Brief description of the problem]

## Environment
- OS: [Windows/macOS/Linux]
- Python Version: [version]
- Command Used: [exact command that failed]

## Steps to Reproduce
1. [First step]
2. [Second step]
3. [etc.]

## Expected Behavior
[What you expected to happen]

## Actual Behavior  
[What actually happened]

## Error Messages
```
[Any error messages or logs]
```

## Diagnostic Information
```bash
# Include output of these commands:
ls -la .agent-os/
cat ~/.agent-os/user-preferences.yaml 2>/dev/null || echo "No preferences"
python --version
```

### Contact Information
- **Documentation Issues**: Update this troubleshooting guide
- **Bug Reports**: GitHub issues preferred
- **Feature Requests**: GitHub discussions or team retrospectives
- **Urgent Production Issues**: Follow standard escalation procedures

## Prevention Tips

### Best Practices
1. **Test First**: Try commands in a test directory before production use
2. **Start Simple**: Begin with minimal variant before using enhanced features  
3. **Check Documentation**: Read migration guide before first use
4. **Keep Backups**: Use git branches for experimental specs
5. **Monitor Performance**: Track spec creation time and file sizes

### Regular Maintenance
```bash
# Weekly cleanup
rm -rf .agent-os/cache/temp-*
du -sh .agent-os/cache/

# Monthly review
# Check for unused modules: find specs/modules/ -type d -empty
# Update user preferences based on usage patterns
# Clean up test specs and experiments
```

### Team Coordination
- Establish team conventions for module naming
- Share troubleshooting discoveries with team
- Regular review of enhanced features adoption
- Coordinate cross-repository integration setup

Remember: Enhanced Agent OS is designed to be additive and optional. Traditional workflows continue to work unchanged, providing a safe fallback at all times.