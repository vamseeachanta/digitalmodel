# OrcaFlex Module

This module contains documentation, utilities, and best practices for working with OrcaFlex in the digitalmodel project.

## Contents

- **[FILE_REQUIREMENTS.md](FILE_REQUIREMENTS.md)** - Critical file format requirements and encoding specifications for OrcaFlex YAML files
- **utils/** - Utility scripts for OrcaFlex file operations
- **examples/** - Example files and templates

## Quick Start

### Before Creating Any OrcaFlex Files

**READ THIS FIRST:** [FILE_REQUIREMENTS.md](FILE_REQUIREMENTS.md)

OrcaFlex is extremely picky about file encoding. All YAML files must:
- Use UTF-8 encoding with BOM (Byte Order Mark)
- Have exactly ONE BOM at file start
- Follow strict YAML formatting

### Common Issues

- ❌ **Double BOM** causes: "Did not find expected <document start> at Line 2, Column 1"
- ❌ **Missing BOM** causes: File won't load
- ❌ **Wrong encoding** causes: Parse errors

### Verification

```bash
# Check for proper BOM (should show: ef bb bf)
od -An -tx1 -N10 filename.yml

# Check encoding
file -i filename.yml
```

## Related Resources

- **Agents:** `/agents/orcaflex/` - OrcaFlex automation agents
- **Commands:** `.agent-os/commands/orcaflex-*.py` - Command-line tools
- **Examples:** `/docs/examples/orcaflex/` - Usage examples

## Support

For issues or questions about OrcaFlex file requirements, refer to [FILE_REQUIREMENTS.md](FILE_REQUIREMENTS.md) which includes:
- Troubleshooting guide
- Error message reference
- Fix scripts
- Python templates
- Historical issues and resolutions
