# PandasAI

> Chat with your data using natural language. Ask questions about DataFrames and get insights, visualizations, and explanations powered by LLMs.

## Overview

PandasAI enables conversational data analysis by letting you query pandas DataFrames using natural language. It generates Python code, executes it, and returns results including visualizations.

## Quick Start

```bash
# Install
pip install pandasai openai

# Set API key
export OPENAI_API_KEY="your-key"
```

```python
import pandas as pd
from pandasai import SmartDataframe

# Load your data
df = pd.DataFrame({
    "product": ["A", "B", "C"],
    "sales": [100, 200, 150],
    "region": ["North", "South", "East"]
})

# Create SmartDataframe
smart_df = SmartDataframe(df)

# Ask questions in natural language
result = smart_df.chat("What is the total sales by region?")
print(result)
```

## Key Features

| Feature | Description |
|---------|-------------|
| Natural Language Queries | Ask questions about data in plain English |
| Chart Generation | Create visualizations from descriptions |
| Code Explanation | Understand generated analysis code |
| Multiple LLM Backends | OpenAI, Anthropic, Google, local models |
| Multi-DataFrame | Query across related DataFrames |
| Privacy Modes | Anonymize sensitive data |

## When to Use

**USE when:**
- Exploring unfamiliar datasets quickly
- Creating ad-hoc visualizations
- Building conversational data interfaces
- Prototyping data analysis workflows
- Explaining data to non-technical stakeholders

**DON'T USE when:**
- Production pipelines needing deterministic outputs
- Processing highly sensitive data
- Performance-critical applications
- Simple queries better handled by pandas directly

## Common Patterns

### Basic Query
```python
smart_df = SmartDataframe(df)
result = smart_df.chat("What is the average revenue?")
```

### Generate Chart
```python
smart_df.chat("Create a bar chart of sales by category")
```

### Multiple DataFrames
```python
from pandasai import SmartDatalake

datalake = SmartDatalake([customers_df, orders_df])
result = datalake.chat("What is the total revenue by customer segment?")
```

### Get Generated Code
```python
result = smart_df.chat("Calculate year-over-year growth")
print(smart_df.last_code_generated)
```

## Related Skills

- [langchain](../langchain/SKILL.md) - LLM application framework
- [pandas-data-processing](../../programming/pandas-data-processing/SKILL.md) - Direct pandas operations
- [streamlit](../../data-analysis/streamlit/SKILL.md) - Interactive dashboards

## Resources

- [PandasAI Docs](https://docs.pandas-ai.com/)
- [GitHub Repository](https://github.com/gventuri/pandas-ai)
- [Examples](https://github.com/gventuri/pandas-ai/tree/main/examples)

---

**Version**: 1.0.0
**Category**: ai-prompting
