# AI Prompting Skills Library

> LLM applications, prompt optimization, and AI-powered data analysis patterns
> Version: 1.0.0 | Last Updated: 2026-01-17

## Overview

This library contains 5 production-ready skills for building LLM-powered applications, optimizing prompts, and integrating AI into data workflows. Each skill covers a specific aspect of AI application development with patterns for reliability, evaluation, and scaling. Skills follow the Anthropic Skills format with practical examples from real-world AI implementations.

## Quick Start

```bash
# Browse available skills
ls skills/ai-prompting/

# Read a skill
cat skills/ai-prompting/langchain/SKILL.md

# Skills are documentation - implement patterns in your AI applications
```

## Available Skills

| Skill | Description | Key Features |
|-------|-------------|--------------|
| [langchain](./langchain/SKILL.md) | LLM application framework | Chains, agents, memory, retrievers |
| [dspy](./dspy/SKILL.md) | Programmatic prompt optimization | Signatures, modules, optimizers |
| [prompt-engineering](./prompt-engineering/SKILL.md) | Prompt design patterns | Templates, few-shot, chain-of-thought |
| [pandasai](./pandasai/SKILL.md) | AI-powered DataFrame queries | Natural language to pandas/SQL |
| [agenta](./agenta/SKILL.md) | LLM app development platform | Prompt management, evaluation, deployment |

## Skill Categories

### Application Frameworks
- **langchain** - Comprehensive framework for LLM applications
- **dspy** - Declarative programming for LLM pipelines

### Prompt Design & Management
- **prompt-engineering** - Core patterns for effective prompts
- **agenta** - Platform for prompt versioning and evaluation

### AI-Powered Data Analysis
- **pandasai** - Natural language interface for data analysis

## Skill Selection Guide

### Choose langchain when:
- Building complex LLM applications with multiple components
- Need agents that can use tools and make decisions
- Implementing RAG (Retrieval Augmented Generation)
- Integrating with various LLM providers and vector stores

### Choose dspy when:
- Optimizing prompts programmatically rather than manually
- Building pipelines where prompt quality is critical
- Need reproducible, testable prompt engineering
- Working with complex multi-step reasoning tasks

### Choose prompt-engineering when:
- Designing prompts from scratch for any use case
- Learning core principles applicable across all LLMs
- Need portable patterns not tied to specific frameworks
- Building simple LLM integrations without heavy frameworks

### Choose pandasai when:
- Enabling non-technical users to query data with natural language
- Building data analysis chatbots or assistants
- Need quick insights from DataFrames without writing code
- Prototyping AI-powered data exploration tools

### Choose agenta when:
- Managing prompt versions across development lifecycle
- Running systematic prompt evaluations and A/B tests
- Need collaboration between engineers and domain experts
- Deploying and monitoring LLM applications in production

## Quick Examples

### LangChain RAG Pipeline
```python
from langchain.document_loaders import DirectoryLoader
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain.embeddings import OpenAIEmbeddings
from langchain.vectorstores import Chroma
from langchain.chains import RetrievalQA
from langchain.chat_models import ChatOpenAI

# Load and split documents
loader = DirectoryLoader("./docs", glob="**/*.md")
documents = loader.load()
splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=200)
chunks = splitter.split_documents(documents)

# Create vector store
embeddings = OpenAIEmbeddings()
vectorstore = Chroma.from_documents(chunks, embeddings)

# Build QA chain
llm = ChatOpenAI(model="gpt-4", temperature=0)
qa_chain = RetrievalQA.from_chain_type(
    llm=llm,
    chain_type="stuff",
    retriever=vectorstore.as_retriever(search_kwargs={"k": 3})
)

# Query
response = qa_chain.run("What are the main features?")
```

### DSPy Optimized Pipeline
```python
import dspy
from dspy.teleprompt import BootstrapFewShot

# Define signature
class QASignature(dspy.Signature):
    """Answer questions based on context."""
    context = dspy.InputField(desc="Relevant context")
    question = dspy.InputField(desc="Question to answer")
    answer = dspy.OutputField(desc="Concise answer")

# Define module
class RAGModule(dspy.Module):
    def __init__(self):
        super().__init__()
        self.retrieve = dspy.Retrieve(k=3)
        self.generate = dspy.ChainOfThought(QASignature)

    def forward(self, question):
        context = self.retrieve(question).passages
        return self.generate(context=context, question=question)

# Optimize with examples
trainset = [
    dspy.Example(question="What is X?", answer="X is..."),
    # More examples...
]
optimizer = BootstrapFewShot(metric=answer_accuracy)
optimized = optimizer.compile(RAGModule(), trainset=trainset)
```

### Prompt Engineering Patterns
```python
# Chain-of-Thought Prompting
COT_TEMPLATE = """
Solve this step by step:

Problem: {problem}

Let's think through this carefully:
1. First, I'll identify the key information...
2. Next, I'll determine the approach...
3. Finally, I'll calculate the answer...

Answer:
"""

# Few-Shot Template
FEW_SHOT_TEMPLATE = """
Here are some examples:

Example 1:
Input: {example1_input}
Output: {example1_output}

Example 2:
Input: {example2_input}
Output: {example2_output}

Now, process this:
Input: {user_input}
Output:
"""

# System + User Pattern
SYSTEM_PROMPT = """
You are an expert {role}. Your task is to {task}.
Always follow these guidelines:
- {guideline1}
- {guideline2}
- {guideline3}
"""
```

### PandasAI Data Querying
```python
import pandas as pd
from pandasai import SmartDataframe
from pandasai.llm import OpenAI

# Load data
df = pd.read_csv("sales_data.csv")

# Create AI-enabled dataframe
llm = OpenAI(api_token="...")
smart_df = SmartDataframe(df, config={"llm": llm})

# Query with natural language
result = smart_df.chat("What were the top 5 products by revenue last quarter?")
print(result)

# Complex analysis
analysis = smart_df.chat(
    "Show me the trend of monthly sales with a line chart, "
    "highlighting months that exceeded the average"
)
```

### Agenta Prompt Management
```python
from agenta import Agenta

# Initialize
ag = Agenta()

# Define prompt variant
@ag.variant
def summarize_text(text: str, style: str = "concise"):
    prompt = f"""
    Summarize the following text in a {style} manner:

    {text}

    Summary:
    """
    return ag.llm.complete(prompt)

# Evaluate variants
results = ag.evaluate(
    variants=["concise", "detailed", "bullet-points"],
    test_cases=test_dataset,
    metrics=["relevance", "coherence", "length"]
)

# Deploy best variant
ag.deploy(variant="concise", environment="production")
```

## Integration Patterns

### RAG Architecture
```
User Query --> Embedding --> Vector Search --> Context Assembly --> LLM --> Response
     |                            |                   |               |
     +-- Query expansion         +-- Reranking       +-- Chunking    +-- Citations
     +-- Intent detection        +-- Filtering       +-- Templates   +-- Validation
```

### Prompt Optimization Pipeline
```
Initial Prompt --> Generate Outputs --> Evaluate --> Optimize --> Deploy
      |                  |                 |            |            |
      +-- Template      +-- Test cases   +-- Metrics  +-- Search   +-- Monitor
      +-- Variables     +-- Edge cases   +-- Human    +-- Iterate  +-- A/B test
```

### Agent Architecture
```
User Request --> Plan --> Tool Selection --> Execution --> Reflection --> Response
      |           |             |                |              |             |
      +-- Parse  +-- Decompose +-- Available    +-- Retry     +-- Verify    +-- Format
      +-- Intent +-- Prioritize+-- Constraints  +-- Timeout   +-- Correct   +-- Cite
```

## Common Patterns Across Skills

### Structured Output
```python
from pydantic import BaseModel

class OutputSchema(BaseModel):
    summary: str
    key_points: list[str]
    confidence: float

# Force structured output
response = llm.complete(
    prompt,
    response_format={"type": "json_object"},
    schema=OutputSchema.schema()
)
```

### Error Handling and Fallbacks
```python
def robust_llm_call(prompt, fallback_response=None):
    try:
        response = llm.complete(prompt, timeout=30)
        if not validate_response(response):
            raise ValueError("Invalid response format")
        return response
    except RateLimitError:
        time.sleep(60)
        return robust_llm_call(prompt, fallback_response)
    except Exception as e:
        logger.error(f"LLM call failed: {e}")
        return fallback_response
```

### Caching and Cost Optimization
```python
import hashlib
from functools import lru_cache

@lru_cache(maxsize=1000)
def cached_embedding(text: str) -> list[float]:
    return embedding_model.embed(text)

def cache_key(prompt, model, temperature):
    content = f"{prompt}|{model}|{temperature}"
    return hashlib.sha256(content.encode()).hexdigest()
```

## Integration with Workspace-Hub

These skills power AI features across the workspace-hub ecosystem:

```
workspace-hub/
├── ai/
│   ├── chains/              # Uses: langchain
│   │   ├── qa_chain.py
│   │   └── summarize_chain.py
│   ├── prompts/             # Uses: prompt-engineering
│   │   ├── templates/
│   │   └── optimized/
│   ├── pipelines/           # Uses: dspy
│   │   └── optimized_qa.py
│   └── data/                # Uses: pandasai
│       └── smart_analysis.py
├── evaluation/              # Uses: agenta
│   ├── test_cases/
│   └── metrics/
└── config/
    └── llm_config.yaml
```

## Best Practices

### 1. Version Control Prompts
```yaml
# prompts/summarize_v2.yaml
version: 2.0.0
model: gpt-4
temperature: 0.3
template: |
  Summarize the following text...
metrics:
  avg_quality: 0.87
  latency_p95: 2.3s
```

### 2. Evaluation-Driven Development
```python
def evaluate_prompt(prompt_template, test_cases):
    results = []
    for case in test_cases:
        output = generate(prompt_template.format(**case.input))
        score = evaluate_output(output, case.expected)
        results.append(score)
    return {
        "mean": sum(results) / len(results),
        "min": min(results),
        "failed_cases": [c for c, s in zip(test_cases, results) if s < 0.7]
    }
```

### 3. Cost Monitoring
```python
class CostTracker:
    def __init__(self, budget_limit=100.0):
        self.total_cost = 0
        self.budget_limit = budget_limit

    def track(self, tokens_in, tokens_out, model):
        cost = calculate_cost(tokens_in, tokens_out, model)
        self.total_cost += cost
        if self.total_cost > self.budget_limit * 0.8:
            logger.warning(f"Approaching budget limit: ${self.total_cost:.2f}")
```

### 4. Graceful Degradation
```python
def get_response(query, context):
    try:
        return advanced_rag_pipeline(query, context)
    except ModelOverloadError:
        return simpler_model_fallback(query, context)
    except Exception:
        return cached_similar_response(query)
```

## Testing AI Applications

```python
import pytest
from unittest.mock import Mock

def test_prompt_produces_valid_output():
    """Test prompt template produces expected format."""
    response = generate_with_template(
        template=SUMMARY_TEMPLATE,
        input_text="Sample text for testing..."
    )
    assert len(response) < len(input_text)
    assert response.strip()

def test_chain_handles_empty_context():
    """Test chain gracefully handles edge cases."""
    result = qa_chain.run(context="", question="What is X?")
    assert "cannot answer" in result.lower() or "no information" in result.lower()

def test_embedding_consistency():
    """Test embeddings are deterministic."""
    text = "Test sentence"
    emb1 = get_embedding(text)
    emb2 = get_embedding(text)
    assert emb1 == emb2
```

## Related Resources

- [LangChain Documentation](https://python.langchain.com/docs/)
- [DSPy Documentation](https://dspy-docs.vercel.app/)
- [OpenAI Prompt Engineering Guide](https://platform.openai.com/docs/guides/prompt-engineering)
- [PandasAI Documentation](https://pandas-ai.com/docs)
- [Agenta Documentation](https://docs.agenta.ai/)

## Version History

- **1.0.0** (2026-01-17): Initial release with 5 AI prompting skills

---

*These skills represent patterns refined across production LLM applications processing millions of queries with optimized cost and quality.*
