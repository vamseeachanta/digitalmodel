# Agent Template System

> Created: 2025-08-03
> Version: 1.0.0

## Overview

The agent template system provides predefined configurations and patterns for common agent types. Templates accelerate agent creation by providing domain-specific context, prompts, and response patterns.

## Template Structure

### Base Template Format
```yaml
# template.yaml
name: template_name
version: 1.0.0
description: Template description
category: engineering|analysis|infrastructure|documentation

capabilities:
  core:
    - capability_1
    - capability_2
  specialized:
    - domain_specific_capability

context_sources:
  repositories:
    - repo_name: purpose
  external:
    - url: description
    
prompts:
  - name: prompt_name
    content: |
      Prompt template content
      
responses:
  - name: response_template
    format: markdown|json|xml
    content: |
      Response template structure
      
workflow_hooks:
  - hook_name: configuration
```

## Predefined Templates

### 1. General Purpose Template
```yaml
name: general-purpose
version: 1.0.0
description: Basic agent with standard capabilities

capabilities:
  core:
    - text_generation
    - question_answering
    - task_decomposition
    - documentation_reading
  specialized:
    - basic_code_understanding
    - simple_data_analysis

context_sources:
  repositories:
    - assetutilities: "Core utilities and patterns"
  external:
    - "https://docs.python.org": "Python documentation"

prompts:
  - name: standard_task
    content: |
      Given the task: {task_description}
      
      Please:
      1. Analyze the requirements
      2. Break down into subtasks
      3. Execute systematically
      4. Provide clear output

responses:
  - name: task_completion
    format: markdown
    content: |
      ## Task Completed
      
      ### Summary
      {summary}
      
      ### Actions Taken
      {actions}
      
      ### Results
      {results}
```

### 2. Engineering Template
```yaml
name: engineering
version: 1.0.0
description: Engineering-focused agent with technical capabilities

capabilities:
  core:
    - code_generation
    - code_review
    - debugging
    - testing
  specialized:
    - architecture_design
    - performance_optimization
    - security_analysis
    - api_development

context_sources:
  repositories:
    - aceengineercode: "Engineering patterns and utilities"
    - digitalmodel: "Digital modeling frameworks"
    - pyproject-starter: "Project templates"
  external:
    - "https://engineering.best-practices.com": "Engineering standards"
    - "https://docs.api.com": "API documentation"

prompts:
  - name: code_review
    content: |
      Review the following code for:
      - Code quality and style
      - Performance implications
      - Security concerns
      - Best practices adherence
      
      Code: {code}
      
  - name: architecture_design
    content: |
      Design a system architecture for:
      Requirements: {requirements}
      Constraints: {constraints}
      
      Include:
      - Component diagram
      - Data flow
      - Technology choices
      - Scalability considerations

responses:
  - name: code_review_result
    format: markdown
    content: |
      ## Code Review Results
      
      ### Quality Score: {score}/10
      
      ### Issues Found
      {issues}
      
      ### Recommendations
      {recommendations}
      
      ### Refactored Code
      ```{language}
      {refactored_code}
      ```
```

### 3. Analysis Template
```yaml
name: analysis
version: 1.0.0
description: Data analysis and visualization agent

capabilities:
  core:
    - data_analysis
    - statistical_analysis
    - visualization_generation
    - report_creation
  specialized:
    - predictive_modeling
    - anomaly_detection
    - trend_analysis
    - data_cleaning

context_sources:
  repositories:
    - assetutilities: "Data processing utilities"
    - worldenergydata: "Energy data patterns"
  external:
    - "https://pandas.pydata.org": "Pandas documentation"
    - "https://plotly.com/python": "Plotly visualization"

prompts:
  - name: data_analysis
    content: |
      Analyze the dataset with focus on:
      - Data quality assessment
      - Statistical summary
      - Key patterns and trends
      - Anomalies or outliers
      - Actionable insights
      
  - name: visualization_request
    content: |
      Create visualizations for:
      Data: {data_description}
      Purpose: {purpose}
      Audience: {audience}
      
      Include appropriate chart types and interactivity

responses:
  - name: analysis_report
    format: markdown
    content: |
      ## Data Analysis Report
      
      ### Executive Summary
      {executive_summary}
      
      ### Key Findings
      {findings}
      
      ### Statistical Metrics
      {metrics}
      
      ### Visualizations
      {visualizations}
      
      ### Recommendations
      {recommendations}
```

### 4. Infrastructure Template
```yaml
name: infrastructure
version: 1.0.0
description: DevOps and infrastructure management agent

capabilities:
  core:
    - deployment_automation
    - ci_cd_pipeline
    - monitoring_setup
    - infrastructure_as_code
  specialized:
    - kubernetes_management
    - cloud_architecture
    - security_hardening
    - performance_tuning

context_sources:
  repositories:
    - assethold: "Infrastructure patterns"
    - pyproject-starter: "Deployment templates"
  external:
    - "https://kubernetes.io/docs": "Kubernetes documentation"
    - "https://docs.aws.amazon.com": "AWS documentation"

prompts:
  - name: deployment_setup
    content: |
      Setup deployment for:
      Application: {app_name}
      Environment: {environment}
      Requirements: {requirements}
      
      Provide:
      - Infrastructure configuration
      - CI/CD pipeline
      - Monitoring setup
      - Security measures

responses:
  - name: infrastructure_config
    format: yaml
    content: |
      # Infrastructure Configuration
      
      infrastructure:
        provider: {provider}
        region: {region}
        
      resources:
        {resources}
        
      deployment:
        {deployment_config}
        
      monitoring:
        {monitoring_setup}
```

### 5. Documentation Template
```yaml
name: documentation
version: 1.0.0
description: Documentation generation and management agent

capabilities:
  core:
    - documentation_generation
    - api_documentation
    - user_guides
    - technical_writing
  specialized:
    - diagram_generation
    - changelog_management
    - knowledge_base_creation
    - tutorial_creation

context_sources:
  repositories:
    - frontierdeepwater: "Technical documentation patterns"
    - OGManufacturing: "Industry documentation standards"
  external:
    - "https://www.writethedocs.org": "Documentation best practices"
    - "https://mermaid-js.github.io": "Diagram documentation"

prompts:
  - name: generate_docs
    content: |
      Generate documentation for:
      Component: {component}
      Audience: {audience}
      
      Include:
      - Overview
      - Installation/Setup
      - Usage examples
      - API reference
      - Troubleshooting

responses:
  - name: documentation
    format: markdown
    content: |
      # {title}
      
      ## Overview
      {overview}
      
      ## Installation
      {installation}
      
      ## Usage
      {usage_examples}
      
      ## API Reference
      {api_reference}
      
      ## Troubleshooting
      {troubleshooting}
```

## Template Application Process

### 1. Template Selection
```python
def select_template(agent_type: str, module_context: Dict) -> Template:
    """Select the most appropriate template based on agent type and context"""
    
    if agent_type == "auto":
        # Analyze module context to determine best template
        agent_type = infer_agent_type(module_context)
    
    template = load_template(agent_type)
    return template
```

### 2. Template Customization
```python
def customize_template(template: Template, module_name: str, options: Dict) -> Template:
    """Customize template for specific module requirements"""
    
    # Add module-specific context sources
    if module_name in REPOSITORY_MAPPING:
        template.context_sources.repositories.append(
            {module_name: f"Module-specific patterns for {module_name}"}
        )
    
    # Merge custom capabilities
    if options.get('additional_capabilities'):
        template.capabilities.specialized.extend(
            options['additional_capabilities']
        )
    
    # Add custom prompts
    if options.get('custom_prompts'):
        template.prompts.extend(options['custom_prompts'])
    
    return template
```

### 3. Template Instantiation
```python
def instantiate_template(template: Template, agent_path: Path):
    """Create agent files from template"""
    
    # Generate agent.yaml from template
    agent_config = generate_agent_config_from_template(template)
    save_yaml(agent_path / "agent.yaml", agent_config)
    
    # Create prompt files
    for prompt in template.prompts:
        prompt_path = agent_path / "templates" / "prompts" / f"{prompt.name}.md"
        prompt_path.write_text(prompt.content)
    
    # Create response templates
    for response in template.responses:
        response_path = agent_path / "templates" / "responses" / f"{response.name}.md"
        response_path.write_text(response.content)
    
    # Setup workflow hooks
    if template.workflow_hooks:
        setup_workflow_hooks(agent_path, template.workflow_hooks)
```

## Template Composition

### Combining Multiple Templates
```python
def compose_templates(primary: str, mixins: List[str]) -> Template:
    """Combine multiple templates into a composite agent"""
    
    composite = load_template(primary)
    
    for mixin_name in mixins:
        mixin = load_template(mixin_name)
        
        # Merge capabilities
        composite.capabilities.core.extend(mixin.capabilities.core)
        composite.capabilities.specialized.extend(mixin.capabilities.specialized)
        
        # Merge context sources
        composite.context_sources.repositories.extend(
            mixin.context_sources.repositories
        )
        
        # Add prompts and responses
        composite.prompts.extend(mixin.prompts)
        composite.responses.extend(mixin.responses)
    
    # Remove duplicates
    composite = deduplicate_template_items(composite)
    
    return composite
```

### Example: Engineering + Documentation Composite
```bash
/create-module-agent api-service --templates engineering,documentation
```

This creates an agent with both engineering capabilities (code generation, review) and documentation capabilities (API docs, user guides), perfect for API service development.

## Custom Template Creation

### Creating New Templates
```yaml
# custom_templates/domain_specific.yaml
name: domain_specific
version: 1.0.0
description: Custom template for specific domain

capabilities:
  core:
    - domain_knowledge
    - specialized_processing
  specialized:
    - industry_specific_analysis
    - regulatory_compliance

# ... rest of template configuration
```

### Registering Custom Templates
```python
def register_custom_template(template_path: Path):
    """Register a custom template for use with /create-module-agent"""
    
    template = load_yaml(template_path)
    validate_template_schema(template)
    
    # Copy to templates directory
    dest = TEMPLATES_DIR / f"{template.name}.yaml"
    shutil.copy(template_path, dest)
    
    # Update template registry
    update_template_registry(template.name, template.description)
```