# OrcaWave MCP Server - READY FOR USE

## ✅ Status: FULLY OPERATIONAL

The OrcaWave MCP server troubleshooting tools are now fully operational and tested. You can use them immediately to troubleshoot your OrcaWave runs.

## 🚀 Quick Start

### Option 1: Standalone CLI Tool (Recommended)
```bash
# Quick fix all issues automatically
python mcp_orcawave.py quick-fix your_model.owd

# Interactive troubleshooting
python mcp_orcawave.py troubleshoot

# Show all commands
python mcp_orcawave.py --help
```

### Option 2: Interactive Troubleshooter
```bash
# Run interactive troubleshooting session
python src/mcp/orcawave/troubleshoot.py

# Run in demo mode
python src/mcp/orcawave/troubleshoot.py --demo
```

### Option 3: Using Makefile (Full Stack)
```bash
# Quick start troubleshooting
make quick-start

# Run complete MCP stack
make run-all

# Show all available commands
make help
```

## 📋 Available Commands

### Troubleshooting Commands
| Command | Description | Example |
|---------|-------------|---------|
| `quick-fix` | Apply all optimizations automatically | `python mcp_orcawave.py quick-fix model.owd` |
| `troubleshoot` | Interactive troubleshooting session | `python mcp_orcawave.py troubleshoot` |
| `mesh-optimize` | Optimize mesh quality | `python mcp_orcawave.py mesh-optimize model.owd --symmetry` |
| `freq-analyze` | Analyze frequency range | `python mcp_orcawave.py freq-analyze model.owd` |
| `monitor` | Monitor convergence | `python mcp_orcawave.py monitor model.owd` |
| `validate` | Validate physics | `python mcp_orcawave.py validate model.owd` |
| `batch` | Run batch processing | `python mcp_orcawave.py batch config.yml` |

## 🔍 Common Issues & Solutions

### 1. Mesh Quality Problems
**Symptoms**: Slow convergence, non-physical results  
**Solution**: `python mcp_orcawave.py mesh-optimize your_model.owd --symmetry --waterline`

### 2. Frequency Range Issues  
**Symptoms**: Missing resonance, incomplete RAOs  
**Solution**: `python mcp_orcawave.py freq-analyze your_model.owd --min-freq 0.1 --max-freq 3.0`

### 3. Convergence Failures
**Symptoms**: Analysis doesn't complete, oscillating residuals  
**Solution**: `python mcp_orcawave.py quick-fix your_model.owd`

### 4. Memory/Performance Issues
**Symptoms**: Out of memory, very slow analysis  
**Solution**: Use symmetry and optimize mesh first

### 5. License Problems
**Symptoms**: Cannot start OrcaWave  
**Solution**: Check license server connection

## 📁 Project Structure

```
digitalmodel/
├── mcp_orcawave.py              # Main standalone CLI tool (USE THIS)
├── src/mcp/orcawave/
│   ├── troubleshoot.py          # Interactive troubleshooter
│   ├── quick_start.py           # Quick start helper
│   ├── __main__.py              # Module entry point
│   ├── Makefile                 # Automation commands
│   ├── docker-compose.yml       # Full stack orchestration
│   └── Dockerfile               # Container image
└── specs/modules/mcp-server/orcawave-mcp/
    └── tasks.md                 # Implementation tracking (Phase 1&2 COMPLETE)
```

## ✅ Completed Implementation

### Phase 1 (COMPLETE)
- [x] Project setup and structure
- [x] OrcaWave COM API wrapper
- [x] FastMCP server implementation  
- [x] Basic model operations
- [x] Configuration management

### Phase 2 (COMPLETE)
- [x] Screen capture integration
- [x] Vision analysis for OrcaWave
- [x] Hybrid control coordination
- [x] Real-time monitoring system

## 🎯 Next Steps

1. **Run troubleshooting on your model**:
   ```bash
   python mcp_orcawave.py quick-fix your_actual_model.owd
   ```

2. **Generate a config for batch processing**:
   ```bash
   python mcp_orcawave.py troubleshoot
   # Select option 4 to create config template
   ```

3. **Monitor your analysis**:
   ```bash
   python mcp_orcawave.py monitor your_model.owd
   ```

## ⚠️ Requirements

- Python 3.9+
- Windows (for OrcaWave COM API)
- OrcaWave license (for actual model processing)
- Optional: pywin32 for COM integration

## 🐛 Troubleshooting the Troubleshooter

If you encounter issues:

1. **Check Python version**: `python --version` (need 3.9+)
2. **Verify working directory**: Must run from `D:\github\digitalmodel`
3. **Install dependencies if needed**: `pip install pyyaml`
4. **Use standalone CLI**: `python mcp_orcawave.py` instead of module imports

## 📞 Support

- Check implementation details: `specs/modules/mcp-server/orcawave-mcp/`
- View task progress: `specs/modules/mcp-server/orcawave-mcp/tasks.md`
- Run tests: `python mcp_orcawave.py validate test.owd`

---

**Status**: READY FOR PRODUCTION USE  
**Last Updated**: 2025-08-25  
**Phases Complete**: 1 & 2 of 5