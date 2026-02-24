# Test Specification: Modular Agent Management System

## Test Strategy Overview

### Testing Scope
- **Unit Tests**: Individual component testing (agent creation, refresh, discovery)
- **Integration Tests**: Cross-component interaction testing
- **Performance Tests**: Context optimization and response time validation
- **End-to-End Tests**: Complete workflow testing with real specifications
- **Validation Tests**: Agent accuracy and knowledge currency testing

### Test Environment Setup
```python
# Test environment configuration
TEST_AGENTS_DIR = "test_agents/"
TEST_SPECS_DIR = "test_specifications/"
TEST_CONFIG = {
    "agent_refresh_timeout": 30,
    "performance_thresholds": {
        "context_reduction": 0.5,
        "response_time_ms": 2000,
        "accuracy_score": 0.95
    }
}
```

## Unit Tests

### 1. Agent Template System Tests

#### Test: Agent Creation from Template
```python
def test_agent_creation_from_template():
    """Test agent creation using template system"""
    # Setup
    template = AgentTemplate(load_test_template())
    test_module_path = "test_specifications/simple_module"
    
    # Execute
    agent = template.create_agent(test_module_path)
    
    # Validate
    assert agent is not None
    assert agent.config['metadata']['name'] == 'simple_module'
    assert len(agent.knowledge_base['specifications']) > 0
    assert agent.capabilities['context_optimization'] is True
    assert agent.prompt_templates is not None

def test_agent_template_validation():
    """Test agent template schema validation"""
    # Test valid template
    valid_template = create_valid_template()
    validator = AgentTemplateValidator()
    result = validator.validate(valid_template)
    assert result.valid is True
    
    # Test invalid template
    invalid_template = create_invalid_template()
    result = validator.validate(invalid_template)
    assert result.valid is False
    assert len(result.errors) > 0

def test_knowledge_extraction_accuracy():
    """Test knowledge extraction from specifications"""
    processor = KnowledgeProcessor()
    test_spec_content = load_test_specification()
    
    knowledge_base = processor.process_specifications(test_spec_content)
    
    # Validate extracted knowledge
    assert 'specifications' in knowledge_base
    assert len(knowledge_base['specifications']) == 1
    
    spec = knowledge_base['specifications'][0]
    assert 'overview' in spec['processed_sections']
    assert 'user_stories' in spec['processed_sections']
    assert len(spec['key_concepts']) > 0
    assert spec['content_hash'] is not None
```

#### Test: Agent Configuration Management
```python
def test_agent_config_creation():
    """Test agent configuration creation and validation"""
    config_generator = AgentConfigGenerator()
    
    test_data = {
        'module_name': 'test_module',
        'domain': 'Test Domain',
        'specs_path': 'test_specs/',
        'expertise_areas': ['testing', 'validation']
    }
    
    config = config_generator.generate_config(test_data)
    
    # Validate configuration structure
    assert 'metadata' in config
    assert config['metadata']['name'] == 'test_module'
    assert config['specialization']['domain'] == 'Test Domain'
    assert len(config['specialization']['expertise_areas']) == 2
    assert config['refresh_config']['auto_refresh'] is True

def test_agent_config_validation():
    """Test agent configuration schema validation"""
    validator = AgentConfigValidator()
    
    # Valid configuration
    valid_config = create_valid_agent_config()
    result = validator.validate(valid_config)
    assert result.valid is True
    
    # Invalid configuration - missing required fields
    invalid_config = {'metadata': {'name': 'test'}}
    result = validator.validate(invalid_config)
    assert result.valid is False
    assert 'specialization' in result.missing_fields
```

### 2. Agent Refresh System Tests

#### Test: Agent Refresh Functionality
```python
def test_agent_refresh_success():
    """Test successful agent refresh with updated specifications"""
    # Setup
    refresh_system = AgentRefreshSystem(TEST_AGENTS_DIR)
    agent_id = "test_agent"
    
    # Create initial agent
    create_test_agent(agent_id)
    
    # Update specifications
    update_test_specifications(agent_id)
    
    # Execute refresh
    result = refresh_system.refresh_agent(agent_id, force=False)
    
    # Validate
    assert result.success is True
    assert result.agent_id == agent_id
    assert result.rollback_performed is False
    
    # Validate updated knowledge
    updated_agent = load_agent(agent_id)
    assert updated_agent.knowledge_base['last_updated'] > original_timestamp

def test_agent_refresh_rollback():
    """Test agent refresh rollback on validation failure"""
    refresh_system = AgentRefreshSystem(TEST_AGENTS_DIR)
    agent_id = "test_agent"
    
    # Create agent with valid configuration
    create_test_agent(agent_id)
    original_config = load_agent_config(agent_id)
    
    # Simulate refresh that would fail validation
    with patch.object(refresh_system, 'validate_agent') as mock_validate:
        mock_validate.return_value = ValidationResult(success=False, error="Test failure")
        
        result = refresh_system.refresh_agent(agent_id, force=True)
    
    # Validate rollback occurred
    assert result.success is False
    assert result.rollback_performed is True
    
    # Validate original configuration restored
    current_config = load_agent_config(agent_id)
    assert current_config == original_config

def test_agent_refresh_scheduling():
    """Test automated agent refresh scheduling"""
    scheduler = RefreshScheduler()
    
    # Schedule refresh for test agent
    agent_id = "test_agent"
    schedule = "0 2 * * 0"  # Weekly at 2 AM Sunday
    
    scheduler.schedule_refresh(agent_id, schedule)
    
    # Validate scheduling
    scheduled_jobs = scheduler.get_scheduled_jobs()
    assert len(scheduled_jobs) == 1
    assert scheduled_jobs[0].agent_id == agent_id
    assert scheduled_jobs[0].schedule == schedule
```

#### Test: Agent Refresh Triggers
```python
def test_specification_update_trigger():
    """Test automatic refresh trigger on specification update"""
    trigger_system = RefreshTriggerSystem()
    agent_id = "test_agent"
    
    # Setup file monitor
    file_monitor = trigger_system.get_file_monitor(agent_id)
    
    # Simulate specification file update
    test_spec_file = f"specs/modules/{agent_id}/spec.md"
    simulate_file_update(test_spec_file)
    
    # Wait for trigger processing
    time.sleep(2)
    
    # Validate refresh was triggered
    refresh_history = trigger_system.get_refresh_history(agent_id)
    assert len(refresh_history) == 1
    assert refresh_history[0].trigger_type == "specification_update"
    assert refresh_history[0].source_file == test_spec_file

def test_manual_refresh_trigger():
    """Test manual refresh trigger via command"""
    command = RefreshAgentCommand()
    
    result = command.execute(["test_agent", "--force"])
    
    assert result.success is True
    assert "refreshed successfully" in result.message.lower()
```

### 3. Agent Discovery Tests

#### Test: Agent Discovery System
```python
def test_agent_discovery():
    """Test automatic agent discovery"""
    # Setup test agents
    create_test_agents(['agent1', 'agent2', 'agent3'])
    
    discovery = AgentDiscovery(TEST_AGENTS_DIR)
    agents = discovery.discover_agents()
    
    # Validate discovery results
    assert len(agents) == 3
    agent_ids = [a.id for a in agents]
    assert 'agent1' in agent_ids
    assert 'agent2' in agent_ids
    assert 'agent3' in agent_ids
    
    # Validate agent information
    for agent in agents:
        assert agent.health_status is not None
        assert agent.config is not None
        assert agent.path.exists()

def test_agent_discovery_by_domain():
    """Test agent discovery by domain expertise"""
    discovery = AgentDiscovery(TEST_AGENTS_DIR)
    
    # Create agents with different domains
    create_test_agent('marine_agent', domain='Marine Engineering')
    create_test_agent('infra_agent', domain='Infrastructure')
    
    # Test exact domain match
    agent = discovery.get_agent_by_domain('Marine Engineering')
    assert agent is not None
    assert agent.id == 'marine_agent'
    
    # Test partial domain match
    agent = discovery.get_agent_by_domain('marine')
    assert agent is not None
    assert agent.id == 'marine_agent'
    
    # Test no match
    agent = discovery.get_agent_by_domain('nonexistent')
    assert agent is None

def test_agent_health_monitoring():
    """Test agent health monitoring functionality"""
    health_monitor = AgentHealthMonitor()
    
    # Test healthy agent
    healthy_agent_path = create_healthy_test_agent()
    health_status = health_monitor.check_agent_health(healthy_agent_path)
    assert health_status.status == HealthStatus.HEALTHY
    assert health_status.last_check is not None
    
    # Test unhealthy agent (corrupted config)
    unhealthy_agent_path = create_unhealthy_test_agent()
    health_status = health_monitor.check_agent_health(unhealthy_agent_path)
    assert health_status.status == HealthStatus.ERROR
    assert health_status.error_message is not None
```

## Integration Tests

### 1. Command Integration Tests

#### Test: /refresh-agent Command Integration
```python
def test_refresh_agent_command_integration():
    """Test complete /refresh-agent command workflow"""
    # Setup test environment
    setup_test_agents_and_specs()
    
    # Execute command
    command_processor = SlashCommandProcessor()
    result = command_processor.execute("/refresh-agent test_module")
    
    # Validate command execution
    assert result.success is True
    assert "successfully refreshed" in result.message
    
    # Validate agent was actually refreshed
    agent = load_agent("test_module")
    assert agent.knowledge_base['last_updated'] > initial_timestamp

def test_refresh_all_agents_command():
    """Test refresh all agents functionality"""
    setup_multiple_test_agents()
    
    command_processor = SlashCommandProcessor()
    result = command_processor.execute("/refresh-agent --all")
    
    assert result.success is True
    
    # Validate all agents were refreshed
    for agent_id in ['agent1', 'agent2', 'agent3']:
        agent = load_agent(agent_id)
        assert agent.knowledge_base['last_updated'] > initial_timestamp
```

#### Test: Enhanced /create-spec Integration
```python
def test_create_spec_with_agent_assignment():
    """Test /create-spec command with automatic agent assignment"""
    # Setup agents
    create_test_agent('marine_agent', domain='Marine Engineering')
    create_test_agent('infra_agent', domain='Infrastructure')
    
    # Execute create-spec command
    spec_generator = EnhancedSpecGenerator()
    
    spec_requirements = {
        'name': 'marine_analysis_feature',
        'domain': 'Marine Engineering',
        'description': 'New marine analysis capability'
    }
    
    spec_doc = spec_generator.generate_spec_with_agents(spec_requirements)
    
    # Validate agent assignments
    assert len(spec_doc.recommended_agents) >= 1
    assert any(a.id == 'marine_agent' for a in spec_doc.recommended_agents)
    
    # Validate task assignments
    for task in spec_doc.tasks:
        if 'assigned_agent' in task.metadata:
            assert task.metadata['assigned_agent'] in ['marine_agent', 'infra_agent']

def test_multi_domain_spec_generation():
    """Test spec generation for multi-domain features"""
    setup_multiple_domain_agents()
    
    spec_generator = EnhancedSpecGenerator()
    
    multi_domain_requirements = {
        'name': 'cross_platform_marine_tools',
        'primary_domain': 'Marine Engineering',
        'secondary_domains': ['Infrastructure', 'Development Tools'],
        'description': 'Cross-platform marine engineering tools'
    }
    
    spec_doc = spec_generator.generate_spec_with_agents(multi_domain_requirements)
    
    # Validate multiple agent assignments
    assert len(spec_doc.recommended_agents) >= 2
    assert spec_doc.collaboration_patterns is not None
    
    # Validate collaboration section
    assert 'Agent Collaboration' in spec_doc.content
    assert 'cross-module communication' in spec_doc.content.lower()
```

### 2. Cross-Agent Collaboration Tests

#### Test: Agent Collaboration Protocols
```python
def test_agent_collaboration_handoff():
    """Test agent handoff in multi-domain scenarios"""
    # Setup collaborating agents
    marine_agent = create_test_agent('marine_agent')
    infra_agent = create_test_agent('infra_agent')
    
    collaboration_manager = AgentCollaborationManager()
    
    # Test handoff scenario
    initial_context = {"domain": "marine", "task": "performance_optimization"}
    
    result = collaboration_manager.handle_collaboration(
        primary_agent=marine_agent,
        secondary_agent=infra_agent,
        context=initial_context
    )
    
    assert result.success is True
    assert result.handoff_occurred is True
    assert result.final_agent.id == 'infra_agent'  # Handed off to infrastructure

def test_agent_consensus_resolution():
    """Test agent consensus and conflict resolution"""
    agents = [
        create_test_agent('agent1'),
        create_test_agent('agent2'),
        create_test_agent('agent3')
    ]
    
    consensus_system = AgentConsensusSystem()
    
    # Simulate conflicting recommendations
    recommendations = [
        {"agent_id": "agent1", "recommendation": "approach_a", "confidence": 0.8},
        {"agent_id": "agent2", "recommendation": "approach_b", "confidence": 0.7},
        {"agent_id": "agent3", "recommendation": "approach_a", "confidence": 0.9}
    ]
    
    consensus = consensus_system.resolve_consensus(recommendations)
    
    assert consensus.final_recommendation == "approach_a"
    assert consensus.confidence_score > 0.8
    assert len(consensus.supporting_agents) >= 2
```

## Performance Tests

### 1. Context Optimization Tests

#### Test: Context Size Reduction
```python
def test_context_compression_performance():
    """Test context compression achieves target reduction"""
    compressor = ContextCompressor()
    
    # Create large knowledge base
    large_knowledge_base = create_large_test_knowledge_base(size_mb=10)
    
    # Test compression with sample query
    query_context = "How do I implement OrcaFlex integration?"
    
    compressed_context = compressor.compress_agent_context(
        {'knowledge_base': large_knowledge_base}, 
        query_context
    )
    
    # Validate compression ratio
    assert compressed_context.compression_ratio >= 0.5  # 50% reduction target
    assert len(compressed_context.relevant_content) > 0
    assert len(compressed_context.summarized_content) > 0

@pytest.mark.performance
def test_agent_response_time():
    """Test agent response time meets performance targets"""
    agent = load_test_agent('performance_test_agent')
    
    # Test multiple queries
    queries = [
        "How do I set up Agent OS?",
        "What are the marine engineering standards?", 
        "How do I refresh agent knowledge?",
        "What is the test automation strategy?"
    ]
    
    response_times = []
    for query in queries:
        start_time = time.time()
        response = agent.process_query(query)
        end_time = time.time()
        
        response_time_ms = (end_time - start_time) * 1000
        response_times.append(response_time_ms)
        
        # Validate individual response time
        assert response_time_ms < 2000  # 2 second target
        assert response is not None
    
    # Validate average response time
    avg_response_time = sum(response_times) / len(response_times)
    assert avg_response_time < 1500  # 1.5 second average target
```

#### Test: Memory Usage Optimization
```python
@pytest.mark.performance
def test_agent_memory_usage():
    """Test agent memory usage optimization"""
    import psutil
    import os
    
    process = psutil.Process(os.getpid())
    initial_memory = process.memory_info().rss
    
    # Load multiple agents
    agents = []
    for i in range(10):
        agent = create_and_load_test_agent(f'memory_test_agent_{i}')
        agents.append(agent)
    
    current_memory = process.memory_info().rss
    memory_increase = current_memory - initial_memory
    
    # Validate memory usage per agent
    memory_per_agent = memory_increase / len(agents)
    assert memory_per_agent < 50 * 1024 * 1024  # Less than 50MB per agent
    
    # Test memory cleanup
    del agents
    gc.collect()
    
    final_memory = process.memory_info().rss
    memory_freed = current_memory - final_memory
    assert memory_freed > memory_increase * 0.8  # 80% memory freed
```

### 2. Scalability Tests

#### Test: Large-Scale Agent Management
```python
@pytest.mark.scalability
def test_large_scale_agent_discovery():
    """Test agent discovery with large number of agents"""
    # Create many test agents
    num_agents = 100
    agent_ids = [f'scale_test_agent_{i}' for i in range(num_agents)]
    
    for agent_id in agent_ids:
        create_test_agent(agent_id)
    
    discovery = AgentDiscovery(TEST_AGENTS_DIR)
    
    # Measure discovery time
    start_time = time.time()
    agents = discovery.discover_agents()
    discovery_time = time.time() - start_time
    
    # Validate results
    assert len(agents) == num_agents
    assert discovery_time < 10.0  # Less than 10 seconds
    
    # Test domain lookup performance
    start_time = time.time()
    agent = discovery.get_agent_by_domain('test_domain')
    lookup_time = time.time() - start_time
    
    assert lookup_time < 1.0  # Less than 1 second
```

## End-to-End Tests

### 1. Complete Workflow Tests

#### Test: Full Agent Lifecycle
```python
@pytest.mark.e2e
def test_complete_agent_lifecycle():
    """Test complete agent lifecycle from creation to refresh"""
    # Step 1: Create agent from specifications
    spec_path = create_test_specification_module()
    template = AgentTemplate(load_default_template())
    
    agent = template.create_agent(spec_path)
    assert agent is not None
    
    # Step 2: Validate initial agent functionality
    response = agent.process_query("What is this module about?")
    assert response is not None
    assert len(response) > 0
    
    # Step 3: Update specifications
    update_test_specifications(spec_path)
    
    # Step 4: Refresh agent knowledge
    refresh_system = AgentRefreshSystem(TEST_AGENTS_DIR)
    refresh_result = refresh_system.refresh_agent(agent.id)
    assert refresh_result.success is True
    
    # Step 5: Validate updated knowledge
    updated_response = agent.process_query("What are the new features?")
    assert updated_response != response  # Response should be different
    assert "new" in updated_response.lower() or "updated" in updated_response.lower()

@pytest.mark.e2e 
def test_spec_generation_with_agents():
    """Test end-to-end spec generation with agent assignments"""
    # Setup: Create domain agents
    setup_comprehensive_agent_environment()
    
    # Step 1: Generate specification
    spec_requirements = {
        'name': 'advanced_marine_simulation',
        'domain': 'Marine Engineering',
        'complexity': 'high',
        'cross_domain_dependencies': ['Infrastructure', 'Test Automation']
    }
    
    spec_generator = EnhancedSpecGenerator()
    spec_doc = spec_generator.generate_spec_with_agents(spec_requirements)
    
    # Step 2: Validate spec generation
    assert spec_doc is not None
    assert len(spec_doc.recommended_agents) >= 2
    assert spec_doc.collaboration_patterns is not None
    
    # Step 3: Validate task assignments
    assert all(hasattr(task, 'metadata') for task in spec_doc.tasks)
    agent_assigned_tasks = [t for t in spec_doc.tasks if 'assigned_agent' in t.metadata]
    assert len(agent_assigned_tasks) > 0
    
    # Step 4: Save and validate file structure
    spec_dir = save_specification(spec_doc)
    assert (spec_dir / 'spec.md').exists()
    assert (spec_dir / 'tasks.md').exists()
    
    # Step 5: Validate agent references in tasks
    tasks_content = (spec_dir / 'tasks.md').read_text()
    assert 'Assigned Agent:' in tasks_content
    assert any(agent.id in tasks_content for agent in spec_doc.recommended_agents)
```

### 2. Integration with Existing Systems

#### Test: Integration with Existing Slash Commands
```python
@pytest.mark.e2e
def test_slash_command_ecosystem_integration():
    """Test integration with existing slash command ecosystem"""
    # Setup complete command environment
    command_processor = SlashCommandProcessor()
    
    # Test /create-spec with agent integration
    result1 = command_processor.execute("/create-spec test_feature marine-engineering")
    assert result1.success is True
    
    # Test /refresh-agent on newly referenced agents  
    result2 = command_processor.execute("/refresh-agent marine-engineering")
    assert result2.success is True
    
    # Test agent discovery integration
    result3 = command_processor.execute("/refresh-agent --all --dry-run")
    assert result3.success is True
    assert "marine-engineering" in result3.message
    
    # Validate command history and logs
    history = command_processor.get_command_history()
    assert len(history) >= 3
    assert all('success' in entry for entry in history)
```

## Test Data Management

### Test Fixtures and Utilities

```python
# Test data fixtures
@pytest.fixture
def test_agents_environment():
    """Create clean test environment with sample agents"""
    cleanup_test_environment()
    
    agents = {
        'marine_agent': create_test_agent('marine_agent', 'Marine Engineering'),
        'infra_agent': create_test_agent('infra_agent', 'Infrastructure'),
        'test_agent': create_test_agent('test_agent', 'Test Automation')
    }
    
    yield agents
    
    cleanup_test_environment()

@pytest.fixture  
def sample_specifications():
    """Create sample specification content for testing"""
    specs = {
        'simple_spec': create_simple_test_spec(),
        'complex_spec': create_complex_test_spec(),
        'multi_domain_spec': create_multi_domain_test_spec()
    }
    
    yield specs

# Test utilities
def create_test_agent(agent_id: str, domain: str = "Test Domain") -> Agent:
    """Create a test agent with specified configuration"""
    config = {
        'metadata': {'name': agent_id, 'version': '1.0.0'},
        'specialization': {'domain': domain, 'expertise_areas': ['testing']},
        'refresh_config': {'auto_refresh': True}
    }
    
    knowledge_base = {'specifications': [], 'expertise_map': {}}
    
    return Agent(config=config, knowledge_base=knowledge_base)

def simulate_file_update(file_path: str):
    """Simulate file system update for testing triggers"""
    Path(file_path).touch()

def cleanup_test_environment():
    """Clean up test environment"""
    if Path(TEST_AGENTS_DIR).exists():
        shutil.rmtree(TEST_AGENTS_DIR)
    if Path(TEST_SPECS_DIR).exists():
        shutil.rmtree(TEST_SPECS_DIR)
```

## Test Execution and CI Integration

### Test Execution Strategy
```bash
# Run all tests
pytest tests/ -v

# Run specific test categories
pytest tests/ -m "unit" -v              # Unit tests only
pytest tests/ -m "integration" -v       # Integration tests only  
pytest tests/ -m "performance" -v       # Performance tests only
pytest tests/ -m "e2e" -v              # End-to-end tests only

# Run with coverage
pytest tests/ --cov=agents --cov-report=html

# Run scalability tests (separate execution)
pytest tests/ -m "scalability" -v --timeout=300
```

### Success Criteria
- **Unit Tests**: >95% pass rate, >90% code coverage
- **Integration Tests**: >90% pass rate, all critical workflows validated
- **Performance Tests**: All performance targets met (context reduction, response time)
- **End-to-End Tests**: Complete workflows function correctly
- **Scalability Tests**: System handles target load (100 agents, <10s discovery)

---

*This comprehensive test specification ensures the modular agent management system is thoroughly validated across all functional, performance, and integration requirements.*