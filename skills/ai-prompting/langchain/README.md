# LangChain

> Build production-ready LLM applications with chains, agents, memory, and RAG pipelines.

## Overview

LangChain is a comprehensive framework for building LLM-powered applications. It provides modular components for chains, agents, memory, tools, and retrieval-augmented generation (RAG).

## Quick Start

```bash
# Install
pip install langchain langchain-openai langchain-community chromadb

# Set API key
export OPENAI_API_KEY="your-key"
```

```python
from langchain_openai import ChatOpenAI
from langchain_core.prompts import ChatPromptTemplate

# Simple chain
prompt = ChatPromptTemplate.from_template("Explain: {topic}")
llm = ChatOpenAI(model="gpt-4")
chain = prompt | llm

response = chain.invoke({"topic": "mooring systems"})
print(response.content)
```

## Key Features

| Feature | Description |
|---------|-------------|
| Chains | Compose LLM calls with prompts and output parsers |
| Agents | Autonomous systems that use tools to accomplish tasks |
| Memory | Conversation history and context management |
| RAG | Retrieval-augmented generation with vector stores |
| Tools | Extend LLM capabilities with custom functions |
| Streaming | Real-time token-by-token output |

## When to Use

**USE when:**
- Building complex multi-step LLM applications
- Need agents with tool-using capabilities
- Implementing document Q&A systems
- Require conversation memory
- Integrating multiple LLM providers

**DON'T USE when:**
- Simple single-prompt API calls
- Need minimal dependencies
- Programmatic prompt optimization (use DSPy)

## Common Patterns

### RAG Pipeline
```python
from langchain_community.vectorstores import Chroma
from langchain_openai import OpenAIEmbeddings

# Create vector store
vectorstore = Chroma.from_documents(docs, OpenAIEmbeddings())
retriever = vectorstore.as_retriever(search_kwargs={"k": 5})

# Query
docs = retriever.get_relevant_documents("your question")
```

### Agent with Tools
```python
from langchain.agents import create_react_agent, AgentExecutor
from langchain_core.tools import tool

@tool
def calculate(expression: str) -> str:
    """Evaluate a math expression."""
    return str(eval(expression))

agent = create_react_agent(llm, [calculate], prompt)
executor = AgentExecutor(agent=agent, tools=[calculate])
```

### Conversation Memory
```python
from langchain_core.runnables.history import RunnableWithMessageHistory

chain_with_memory = RunnableWithMessageHistory(
    chain,
    get_session_history,
    input_messages_key="input",
    history_messages_key="history"
)
```

## Related Skills

- [prompt-engineering](../prompt-engineering/SKILL.md) - Core prompting patterns
- [dspy](../dspy/SKILL.md) - Programmatic prompt optimization

## Resources

- [LangChain Docs](https://python.langchain.com/docs/)
- [LangSmith](https://smith.langchain.com/)
- [LangServe](https://python.langchain.com/docs/langserve/)

---

**Version**: 1.0.0
**Category**: ai-prompting
