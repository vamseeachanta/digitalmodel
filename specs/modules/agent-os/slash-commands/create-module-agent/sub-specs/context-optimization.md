# Context Optimization and Caching

> Created: 2025-08-03
> Version: 1.0.0

## Overview

Context optimization ensures agents have fast access to relevant documentation and knowledge without processing overhead. The system creates optimized, searchable indices and caches processed context locally for immediate retrieval.

## Optimization Pipeline

### 1. Context Collection Phase
```python
def collect_context(module_name: str, repositories: List[str]) -> RawContext:
    """Collect raw context from all sources"""
    
    context = RawContext()
    
    # Repository documentation
    for repo in repositories:
        context.add_repository(
            name=repo,
            readme=fetch_file(f"@github:{repo}/README.md"),
            docs=fetch_directory(f"@github:{repo}/docs/"),
            specs=fetch_directory(f"@github:{repo}/specs/"),
            examples=fetch_directory(f"@github:{repo}/examples/")
        )
    
    # External documentation
    context.add_external(
        web_sources=fetch_web_documentation(module_name),
        api_docs=fetch_api_documentation(module_name),
        standards=fetch_industry_standards(module_name)
    )
    
    # Module-specific context
    context.add_module(
        specs=fetch_directory(f"specs/modules/{module_name}/"),
        docs=fetch_directory(f"docs/modules/{module_name}/"),
        src=analyze_source_code(f"src/modules/{module_name}/")
    )
    
    return context
```

### 2. Processing and Analysis
```python
def process_context(raw_context: RawContext) -> ProcessedContext:
    """Process raw context into structured knowledge"""
    
    processed = ProcessedContext()
    
    # Extract patterns and concepts
    processed.patterns = extract_patterns(raw_context)
    processed.concepts = identify_concepts(raw_context)
    processed.apis = extract_api_signatures(raw_context)
    processed.workflows = identify_workflows(raw_context)
    
    # Build relationships
    processed.knowledge_graph = build_knowledge_graph(
        patterns=processed.patterns,
        concepts=processed.concepts,
        apis=processed.apis
    )
    
    # Create summaries
    processed.summaries = generate_summaries(raw_context)
    
    return processed
```

### 3. Embedding Generation
```python
def generate_embeddings(processed_context: ProcessedContext) -> Embeddings:
    """Generate vector embeddings for semantic search"""
    
    embeddings = Embeddings()
    
    # Document embeddings
    for doc in processed_context.documents:
        doc_chunks = chunk_document(doc, chunk_size=512)
        doc_embeddings = embed_chunks(doc_chunks)
        embeddings.add_document(doc.id, doc_embeddings)
    
    # Concept embeddings
    for concept in processed_context.concepts:
        concept_embedding = embed_text(concept.description)
        embeddings.add_concept(concept.name, concept_embedding)
    
    # API embeddings
    for api in processed_context.apis:
        api_embedding = embed_text(api.signature + api.description)
        embeddings.add_api(api.name, api_embedding)
    
    return embeddings
```

### 4. Index Creation
```python
def create_search_index(processed_context: ProcessedContext) -> SearchIndex:
    """Create efficient search indices"""
    
    index = SearchIndex()
    
    # Full-text search index
    index.text_index = create_text_index(processed_context.documents)
    
    # Semantic search index (using embeddings)
    index.semantic_index = create_vector_index(processed_context.embeddings)
    
    # Code search index
    index.code_index = create_code_index(processed_context.apis)
    
    # Metadata index
    index.metadata_index = create_metadata_index(processed_context.metadata)
    
    return index
```

## Caching Strategy

### Cache Structure
```json
{
  "version": "1.0.0",
  "created": "2025-08-03T10:00:00Z",
  "last_updated": "2025-08-03T10:00:00Z",
  "module": "module_name",
  
  "patterns": {
    "design_patterns": [...],
    "code_patterns": [...],
    "workflow_patterns": [...]
  },
  
  "concepts": {
    "domain_concepts": [...],
    "technical_concepts": [...],
    "business_concepts": [...]
  },
  
  "apis": {
    "internal": [...],
    "external": [...],
    "interfaces": [...]
  },
  
  "summaries": {
    "executive": "...",
    "technical": "...",
    "domain": "..."
  },
  
  "knowledge_graph": {
    "nodes": [...],
    "edges": [...],
    "clusters": [...]
  },
  
  "search_indices": {
    "text_index": "path/to/text.idx",
    "semantic_index": "path/to/semantic.idx",
    "code_index": "path/to/code.idx"
  }
}
```

### Cache Operations
```python
class ContextCache:
    """Manages optimized context caching"""
    
    def __init__(self, agent_path: Path):
        self.cache_dir = agent_path / "context" / "optimized"
        self.cache_file = self.cache_dir / "cache.json"
        self.embeddings_file = self.cache_dir / "embeddings.bin"
        
    def save(self, optimized_context: OptimizedContext):
        """Save optimized context to cache"""
        
        # Save JSON cache
        cache_data = optimized_context.to_dict()
        with open(self.cache_file, 'w') as f:
            json.dump(cache_data, f, indent=2)
        
        # Save binary embeddings
        self.save_embeddings(optimized_context.embeddings)
        
        # Save search indices
        self.save_indices(optimized_context.indices)
        
    def load(self) -> OptimizedContext:
        """Load optimized context from cache"""
        
        # Load JSON cache
        with open(self.cache_file, 'r') as f:
            cache_data = json.load(f)
        
        # Load embeddings
        embeddings = self.load_embeddings()
        
        # Load indices
        indices = self.load_indices()
        
        return OptimizedContext.from_cache(cache_data, embeddings, indices)
    
    def is_valid(self, max_age_hours: int = 24) -> bool:
        """Check if cache is still valid"""
        
        if not self.cache_file.exists():
            return False
        
        # Check age
        cache_age = datetime.now() - datetime.fromtimestamp(
            self.cache_file.stat().st_mtime
        )
        
        if cache_age.total_seconds() > max_age_hours * 3600:
            return False
        
        # Verify integrity
        return self.verify_cache_integrity()
```

## Semantic Search Implementation

### Vector Database Integration
```python
class SemanticSearch:
    """Semantic search using vector embeddings"""
    
    def __init__(self, embeddings: Embeddings):
        self.embeddings = embeddings
        self.index = self.build_faiss_index()
        
    def build_faiss_index(self):
        """Build FAISS index for fast similarity search"""
        import faiss
        
        # Stack all embeddings
        vectors = np.vstack([
            e.vector for e in self.embeddings.all()
        ])
        
        # Create index
        dimension = vectors.shape[1]
        index = faiss.IndexFlatL2(dimension)
        index.add(vectors)
        
        return index
    
    def search(self, query: str, k: int = 10) -> List[SearchResult]:
        """Search for similar content"""
        
        # Embed query
        query_embedding = embed_text(query)
        
        # Search index
        distances, indices = self.index.search(
            query_embedding.reshape(1, -1), k
        )
        
        # Build results
        results = []
        for idx, distance in zip(indices[0], distances[0]):
            result = SearchResult(
                content=self.embeddings.get_content(idx),
                score=1.0 / (1.0 + distance),  # Convert distance to similarity
                metadata=self.embeddings.get_metadata(idx)
            )
            results.append(result)
        
        return results
```

### Contextual Retrieval
```python
def retrieve_context(query: str, cache: ContextCache) -> RetrievedContext:
    """Retrieve relevant context for a query"""
    
    context = cache.load()
    
    # Semantic search
    semantic_results = context.semantic_search(query)
    
    # Pattern matching
    pattern_matches = context.match_patterns(query)
    
    # API search
    api_matches = context.search_apis(query)
    
    # Combine and rank results
    combined = combine_results(
        semantic_results,
        pattern_matches,
        api_matches
    )
    
    # Build retrieved context
    retrieved = RetrievedContext(
        documents=combined.top_documents(5),
        patterns=combined.relevant_patterns(),
        apis=combined.relevant_apis(),
        examples=combined.relevant_examples()
    )
    
    return retrieved
```

## Performance Optimization

### Lazy Loading
```python
class LazyContext:
    """Lazy loading of context components"""
    
    def __init__(self, cache_path: Path):
        self.cache_path = cache_path
        self._cache = None
        self._embeddings = None
        self._indices = None
    
    @property
    def cache(self):
        if self._cache is None:
            self._cache = load_json_cache(self.cache_path)
        return self._cache
    
    @property
    def embeddings(self):
        if self._embeddings is None:
            self._embeddings = load_embeddings(self.cache_path)
        return self._embeddings
    
    @property
    def indices(self):
        if self._indices is None:
            self._indices = load_indices(self.cache_path)
        return self._indices
```

### Incremental Updates
```python
def update_cache_incrementally(cache: ContextCache, changes: List[Change]):
    """Update cache incrementally without full rebuild"""
    
    context = cache.load()
    
    for change in changes:
        if change.type == "add_document":
            # Process new document
            processed = process_document(change.document)
            context.add_document(processed)
            
        elif change.type == "update_document":
            # Update existing document
            context.update_document(change.document_id, change.content)
            
        elif change.type == "remove_document":
            # Remove document
            context.remove_document(change.document_id)
    
    # Rebuild affected indices
    context.rebuild_partial_indices(changes)
    
    # Save updated cache
    cache.save(context)
```

### Memory Management
```python
class MemoryEfficientCache:
    """Memory-efficient cache implementation"""
    
    def __init__(self, max_memory_mb: int = 500):
        self.max_memory = max_memory_mb * 1024 * 1024
        self.lru_cache = LRUCache(maxsize=100)
        
    def get_context_chunk(self, chunk_id: str) -> ContextChunk:
        """Get a specific chunk with LRU caching"""
        
        if chunk_id in self.lru_cache:
            return self.lru_cache[chunk_id]
        
        # Load from disk
        chunk = load_chunk_from_disk(chunk_id)
        
        # Check memory usage
        if self.get_memory_usage() > self.max_memory:
            self.evict_least_used()
        
        # Cache and return
        self.lru_cache[chunk_id] = chunk
        return chunk
```

## Cache Refresh Strategy

### Automatic Refresh Triggers
```python
def setup_auto_refresh(agent_path: Path):
    """Setup automatic cache refresh"""
    
    config = {
        "refresh_triggers": [
            {
                "type": "time_based",
                "interval": "24h"
            },
            {
                "type": "file_change",
                "watch_paths": [
                    "specs/modules/",
                    "docs/modules/"
                ]
            },
            {
                "type": "repository_update",
                "repositories": ["assetutilities", "..."]
            }
        ],
        "refresh_strategy": "incremental",
        "fallback": "full_rebuild"
    }
    
    save_refresh_config(agent_path, config)
```

### Manual Refresh Command
```python
def refresh_agent_context(module_name: str, full: bool = False):
    """Manually refresh agent context"""
    
    agent_path = Path(f"agents/{module_name}")
    cache = ContextCache(agent_path)
    
    if full or not cache.is_valid():
        # Full rebuild
        context = collect_and_optimize_context(module_name)
        cache.save(context)
    else:
        # Incremental update
        changes = detect_changes(agent_path)
        update_cache_incrementally(cache, changes)
    
    return cache.get_status()
```

## Integration with Agent Runtime

### Context Loading at Runtime
```python
class AgentRuntime:
    """Agent runtime with optimized context loading"""
    
    def __init__(self, module_name: str):
        self.module_name = module_name
        self.agent_path = Path(f"agents/{module_name}")
        self.cache = ContextCache(self.agent_path)
        self.context = None
        
    def initialize(self):
        """Initialize agent with optimized context"""
        
        # Check cache validity
        if not self.cache.is_valid():
            self.refresh_context()
        
        # Load optimized context
        self.context = self.cache.load()
        
        # Warm up search indices
        self.context.warm_up_indices()
        
    def query_context(self, query: str) -> RetrievedContext:
        """Query context for relevant information"""
        
        if self.context is None:
            self.initialize()
        
        return retrieve_context(query, self.cache)
    
    def refresh_context(self):
        """Refresh context cache"""
        
        refresh_agent_context(self.module_name)
        self.context = self.cache.load()
```