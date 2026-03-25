# Create-Module-Agent v3.1 - Conditional Phasing

## 🔄 What's New in v3.1

### Conditional Phased Processing
The phased approach is now **intelligently applied** based on estimated compute time:

- **Compute Time ≤ 2 minutes**: Fast direct processing
- **Compute Time > 2 minutes**: Full 6-phase processing

### Compute Time Estimates by File Type
| File Type | Processing Time |
|-----------|----------------|
| PDF | 0.5 min/MB (OCR overhead) |
| Excel/Word | 0.3 min/MB |
| CSV | 0.2 min/MB |
| HTML/XML | 0.2-0.25 min/MB |
| Markdown/Text | 0.1 min/MB |
| JSON/YAML | 0.15 min/MB |

### Auto-Refresh Update
- Changed from 24 hours to **7 days**
- Reduces unnecessary processing overhead
- Still maintains knowledge currency

## Usage Examples

### Automatic Phasing Decision
```bash
# System automatically decides based on compute estimate
python agent_os/commands/create_module_agent.py my-module \
  --mode update \
  --process-docs "./docs/*.md"
```

### Force Phased Processing
```bash
# Override automatic decision
python agent_os/commands/create_module_agent.py my-module \
  --mode update \
  --process-docs "./docs/*.md" \
  --phased
```

## When Phased Processing Activates

The system uses phased processing when:
1. Estimated compute time > 2 minutes
2. Processing large PDFs (>10MB)
3. Multiple complex documents
4. User explicitly requests with --phased flag

## Benefits of Conditional Phasing

- **Efficiency**: Small document sets process quickly
- **Scalability**: Large sets get proper phased handling
- **Intelligence**: Automatic decision based on workload
- **Flexibility**: Manual override when needed

---
*v3.1 - Smarter processing with conditional phasing*
