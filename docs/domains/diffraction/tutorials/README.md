# Diffraction Analysis Tutorials

Complete step-by-step guides for using the diffraction analysis module.

## Tutorial Series

### 1. [Getting Started](01_getting_started.md)
**Duration**: 10 minutes | **Difficulty**: Beginner

Your first conversion from AQWA to OrcaFlex format.

**You'll learn:**
- How to install and verify the CLI
- Convert AQWA results with one command
- Understand the output files
- Interpret validation reports

**Prerequisites:**
- Python 3.10+
- AQWA .LIS file

---

### 2. [AQWA Conversion - Complete Example](02_aqwa_conversion_example.md)
**Duration**: 15 minutes | **Difficulty**: Intermediate

Detailed walkthrough of a real-world AQWA conversion.

**You'll learn:**
- How to inspect AQWA .LIS files before conversion
- Validate and troubleshoot results
- Load converted data in OrcaFlex
- Best practices for production use

**Prerequisites:**
- Tutorial 1 completed
- Access to OrcaFlex (for final steps)

---

### 3. [Batch Processing Multiple Vessels](03_batch_processing.md)
**Duration**: 20 minutes | **Difficulty**: Intermediate

Efficiently process multiple vessels or configurations in parallel.

**You'll learn:**
- Create batch configuration files
- Process multiple vessels in parallel
- Handle errors gracefully
- Optimize performance for large batches

**Prerequisites:**
- Tutorial 1 or 2 completed
- Multiple vessel analyses available

---

## Quick Reference

### Common Commands

```bash
# Single vessel conversion
diffraction convert-aqwa <folder> <name> -d <depth> -o <output>

# Batch processing
diffraction batch <config.json>

# Get help
diffraction --help
diffraction convert-aqwa --help
```

### Typical Workflow

1. **Inspect** AQWA .LIS file
2. **Convert** using CLI or Python API
3. **Validate** results (automatic)
4. **Review** validation report
5. **Load** in OrcaFlex
6. **Verify** data in OrcaFlex

## Learning Path

**For Beginners:**
1. Start with [Getting Started](01_getting_started.md)
2. Try the examples with your own data
3. Review validation reports

**For Production Use:**
1. Complete [AQWA Conversion Example](02_aqwa_conversion_example.md)
2. Learn [Batch Processing](03_batch_processing.md)
3. Integrate into your workflow

**For Advanced Users:**
- Read [CLI Guide](../CLI_GUIDE.md) for all options
- Check [AQWA Parser](../AQWA_PARSER.md) for technical details
- Review [API Reference](../README.md#api-reference) for programmatic use

## Sample Data

Tutorial examples use generic vessel data. To practice with real data:

1. **Use your own AQWA analyses** (recommended)
2. **Check example files** in `docs/domains/aqwa/examples/`
3. **Generate mock data** using test utilities (see API docs)

## Troubleshooting

Common issues and solutions:

| Issue | Solution | Tutorial |
|-------|----------|----------|
| No .LIS file found | Check folder path | Tutorial 1 |
| Validation warnings | Review coefficient magnitudes | Tutorial 2 |
| Batch processing slow | Adjust `max_workers` | Tutorial 3 |
| OrcFxAPI not available | Use AQWA conversion | Tutorial 1 |

See individual tutorials for detailed troubleshooting.

## Additional Resources

- **[CLI Guide](../CLI_GUIDE.md)** - Complete command reference
- **[AQWA Parser](../AQWA_PARSER.md)** - Technical details
- **[Module README](../README.md)** - Overview and API reference

## Support

For issues or questions:
1. Check tutorial troubleshooting sections
2. Review validation reports for diagnostics
3. Use `--verbose` flag for detailed output
4. Consult API documentation

## Contributing

Found an error or have a suggestion?
- Open an issue on GitHub
- Submit a pull request with improvements

---

**Module**: digitalmodel.diffraction
**Version**: 3.0.0
**Status**: Production Ready

Happy analyzing! 🚀
