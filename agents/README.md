# AI Agents Library

This folder contains specialized AI agents for various development tasks.
Agents are sourced from AITmpl and Claude Code Templates.

## Folder Structure

```
agents/
├── security/          # Security audit and penetration testing
├── performance/       # Performance optimization agents
├── testing/          # Test generation and E2E testing
├── documentation/    # API and code documentation
├── devops/          # CI/CD and infrastructure
├── data/            # Data engineering and ETL
└── code_quality/    # Code review and refactoring
```

## Usage

### Automatic Agent Selection

Agents are automatically selected based on your current task:

```bash
# During spec creation
/spec create api-gateway  # Automatically uses API Security Agent

# During testing
/test generate  # Uses Test Generation Agent

# During optimization
/project optimize  # Uses Performance Optimization Agents
```

### Manual Agent Usage

```bash
# List all available agents
/ai-agent list

# Get recommendations for current context
/ai-agent recommend

# Use specific agent
/ai-agent use "API Security Audit Agent"

# Get agent information
/ai-agent info "Test Generation Agent"
```

## Available Agents by Category

### 🔒 Security
- API Security Audit Agent
- Penetration Testing Agent
- Vulnerability Scanner Agent

### ⚡ Performance
- React Performance Optimization Agent
- Database Optimization Agent
- Bundle Size Analyzer Agent

### 🧪 Testing
- Test Generation Agent
- E2E Testing Agent
- Coverage Analysis Agent

### 📚 Documentation
- API Documentation Agent
- Code Documentation Agent
- README Generator Agent

### 🔧 DevOps
- CI/CD Pipeline Agent
- Infrastructure as Code Agent
- Docker Configuration Agent

### 📊 Data
- ETL Pipeline Agent
- Data Analysis Agent
- Data Validation Agent

### ✨ Code Quality
- Code Review Agent
- Refactoring Agent
- Best Practices Agent

## Integration Points

Agents integrate with these commands:
- `/spec create` - Automatic agent recommendations
- `/task execute` - Agent assistance during implementation
- `/test run` - Testing agents for quality assurance
- `/project optimize` - Performance agents
- `/git commit` - Code review agents

## Best Practices

1. **Let agents recommend themselves** - The system knows which agents to use
2. **Chain agents for better results** - Use multiple agents in sequence
3. **Review agent suggestions** - Don't apply blindly
4. **Customize for your project** - Adapt agent outputs to your standards
5. **Share improvements** - Contribute agent enhancements back

## Adding Custom Agents

To add your own agents:

1. Create agent file in appropriate category folder
2. Follow the agent template structure
3. Update this README with agent details
4. Test agent integration

## Resources

- AITmpl: https://www.aitmpl.com/
- Claude Code Templates: https://github.com/davila7/claude-code-templates
- Agent Catalog: .agent-os/resources/aitmpl_agents_catalog.yaml
