# Task-Specific Context Extraction Agents Specification

## Executive Summary
This specification defines a suite of specialized AI agents for extracting context, calculations, and formulas from various file types (HTML, PDF, Excel) and converting them to Python code. Each agent is optimized for its specific file type, providing efficient context extraction and formula translation capabilities.

## Overview
The context extraction system provides task-specific agents that:
- Extract structured content from HTML files
- Parse and analyze PDF documents for text, calculations, and formulas
- Convert Excel calculations and formulas to Python equivalents
- Provide optimal tool selection for each file type
- Enable parallel processing for batch operations

## Architecture

### System Components

```
context-extraction/
├── agents/
│   ├── html_context_agent.py      # HTML file context extraction
│   ├── pdf_context_agent.py       # PDF text and structure extraction
│   ├── excel_to_python_agent.py   # Excel formula conversion
│   ├── pdf_calc_agent.py          # PDF calculation extraction
│   └── pdf_formula_agent.py       # PDF formula extraction
├── core/
│   ├── base_agent.py              # Base context agent class
│   ├── extraction_engine.py       # Core extraction logic
│   └── conversion_engine.py       # Formula conversion engine
├── tools/
│   ├── html_tools.py              # HTML parsing utilities
│   ├── pdf_tools.py               # PDF processing utilities
│   ├── excel_tools.py             # Excel parsing utilities
│   └── formula_parser.py          # Formula parsing engine
└── utils/
    ├── parallel_processor.py      # Parallel processing utilities
    └── validation.py              # Output validation
```

## Agent Specifications

### 1. HTML Context Extraction Agent

#### Purpose
Extract structured content, metadata, and context from HTML files for AI processing.

#### Best Tools & Libraries
- **BeautifulSoup4**: Primary HTML parsing (`pip install beautifulsoup4`)
- **lxml**: Fast XML/HTML parser (`pip install lxml`)
- **html5lib**: Standards-compliant HTML parser (`pip install html5lib`)
- **Readability**: Content extraction from web pages (`pip install readability-lxml`)
- **trafilatura**: Web scraping and text extraction (`pip install trafilatura`)

#### Capabilities
- DOM tree traversal and element extraction
- Metadata extraction (title, description, keywords)
- Structured data extraction (tables, lists, forms)
- CSS selector and XPath queries
- JavaScript-rendered content handling (with Selenium)
- Content cleaning and normalization

#### Implementation Strategy
```python
class HTMLContextAgent:
    def __init__(self):
        self.parser = BeautifulSoup
        self.content_extractor = Readability
        
    def extract_context(self, html_path):
        # Parse HTML structure
        # Extract main content
        # Identify data tables
        # Extract metadata
        # Return structured context
```

### 2. PDF Context Extraction Agent

#### Purpose
Extract text, structure, tables with surrounding context, and embedded content from PDF documents for reuse in testing and analysis.

#### Best Tools & Libraries
- **PyPDF2/pypdf**: Basic PDF operations (`pip install pypdf`)
- **pdfplumber**: Advanced text extraction with layout (`pip install pdfplumber`)
- **PyMuPDF (fitz)**: Fast PDF rendering and extraction (`pip install PyMuPDF`)
- **Camelot**: Table extraction from PDFs (`pip install camelot-py[cv]`)
- **Tabula-py**: Java-based table extraction (`pip install tabula-py`)
- **pdf2image**: Convert PDF pages to images (`pip install pdf2image`)

#### Capabilities
- Text extraction with formatting preservation
- Table detection and extraction with context preservation
- Contextual table metadata (headers, captions, references)
- Image and diagram extraction
- Metadata and annotation extraction
- Multi-column layout handling
- OCR for scanned PDFs (with Tesseract)
- Table relationship mapping

#### Implementation Strategy
```python
class PDFContextAgent:
    def __init__(self):
        self.text_extractor = pdfplumber
        self.table_extractor = Camelot
        self.ocr_engine = pytesseract
        self.context_analyzer = TableContextAnalyzer()
        
    def extract_context(self, pdf_path):
        # Extract text with layout
        # Identify and extract tables with context
        # Capture table surroundings (±3 paragraphs)
        # Extract table metadata and references
        # Parse document structure
        # Return comprehensive context with reusable tables
        
    def extract_tables_with_context(self, pdf_path):
        """Extract tables with rich contextual information"""
        tables = []
        with pdfplumber.open(pdf_path) as pdf:
            for page_num, page in enumerate(pdf.pages):
                # Extract tables from page
                page_tables = page.extract_tables()
                
                for table_idx, table in enumerate(page_tables):
                    # Get surrounding text context
                    bbox = self._get_table_bbox(page, table)
                    context_before = self._extract_text_before(page, bbox)
                    context_after = self._extract_text_after(page, bbox)
                    
                    # Identify table caption and headers
                    caption = self._extract_caption(context_before, context_after)
                    headers = self._identify_headers(table)
                    
                    # Create enriched table object
                    enriched_table = {
                        'table_id': f'page_{page_num}_table_{table_idx}',
                        'page_number': page_num + 1,
                        'data': table,
                        'headers': headers,
                        'caption': caption,
                        'context_before': context_before,
                        'context_after': context_after,
                        'references': self._find_references(pdf, page_num, table_idx),
                        'metadata': {
                            'rows': len(table),
                            'columns': len(table[0]) if table else 0,
                            'has_headers': bool(headers),
                            'extraction_confidence': self._calculate_confidence(table)
                        }
                    }
                    tables.append(enriched_table)
        
        return tables
```

### 3. Excel-to-Python Conversion Agent

#### Purpose
Convert Excel calculations and formulas to executable Python code.

#### Best Tools & Libraries
- **openpyxl**: Read/write Excel files (`pip install openpyxl`)
- **xlwings**: Excel automation and UDF (`pip install xlwings`)
- **pandas**: Data manipulation (`pip install pandas`)
- **formulas**: Excel formula parser (`pip install formulas`)
- **pycel**: Excel formula compiler (`pip install pycel`)
- **xlcalculator**: Excel formula calculator (`pip install xlcalculator`)

#### Capabilities
- Formula parsing and tokenization
- Function mapping (Excel → Python)
- Cell reference resolution
- Array formula handling
- Conditional formatting logic extraction
- VBA macro translation (basic)

#### Implementation Strategy
```python
class ExcelToPythonAgent:
    def __init__(self):
        self.workbook_parser = openpyxl
        self.formula_parser = formulas
        self.converter = FormulaConverter()
        
    def convert_to_python(self, excel_path):
        # Load workbook
        # Parse formulas
        # Map Excel functions to Python
        # Generate Python code
        # Validate calculations
```

#### Excel Function Mapping
```python
EXCEL_TO_PYTHON_MAP = {
    'SUM': 'np.sum',
    'AVERAGE': 'np.mean',
    'IF': 'np.where',
    'VLOOKUP': 'pd.merge',
    'INDEX/MATCH': 'df.loc',
    'SUMIF': 'df[condition].sum()',
    # ... comprehensive mapping
}
```

### 4. PDF Calculation Extraction Agent

#### Purpose
Extract mathematical calculations from PDF documents and convert to Python.

#### Best Tools & Libraries
- **SymPy**: Symbolic mathematics (`pip install sympy`)
- **mathpix**: OCR for mathematical notation (API)
- **pdf2image + OCR**: For handwritten calculations
- **LaTeX parser**: For LaTeX-formatted equations
- **NumPy**: Numerical computations (`pip install numpy`)

#### Capabilities
- Mathematical expression recognition
- Equation parsing and solving
- Unit conversion handling
- Statistical calculation extraction
- Engineering calculation patterns
- Step-by-step solution extraction

#### Implementation Strategy
```python
class PDFCalculationAgent:
    def __init__(self):
        self.pdf_extractor = PDFContextAgent()
        self.math_parser = SymPy
        self.ocr_math = MathpixAPI()
        
    def extract_calculations(self, pdf_path):
        # Extract text containing calculations
        # Identify mathematical expressions
        # Parse equations
        # Convert to Python/NumPy
        # Generate executable code
```

### 5. PDF Formula Extraction Agent

#### Purpose
Extract and convert mathematical formulas from PDFs to Python implementations.

#### Best Tools & Libraries
- **SymPy**: Formula manipulation (`pip install sympy`)
- **LaTeX2SymPy**: LaTeX to SymPy conversion (`pip install latex2sympy2`)
- **pdfplumber**: Extract formula regions
- **matplotlib**: Formula visualization (`pip install matplotlib`)
- **SciPy**: Scientific computing (`pip install scipy`)

#### Capabilities
- LaTeX formula extraction
- Image-based formula recognition
- Formula standardization
- Variable mapping
- Dimensional analysis
- Formula validation

#### Implementation Strategy
```python
class PDFFormulaAgent:
    def __init__(self):
        self.formula_extractor = FormulaExtractor()
        self.latex_parser = LaTeX2SymPy
        self.validator = FormulaValidator()
        
    def extract_formulas(self, pdf_path):
        # Identify formula regions
        # Extract LaTeX or image formulas
        # Parse to symbolic representation
        # Convert to Python functions
        # Validate with test cases
```

## Table Context Preservation & Reuse

### Purpose
Preserve complete context around extracted tables to enable accurate reuse in testing, validation, and downstream analysis.

### Table Context Components

#### 1. Structural Context
```python
class TableContext:
    def __init__(self):
        self.caption = None          # Table title/caption
        self.headers = []            # Column headers
        self.row_labels = []         # Row identifiers
        self.units = {}              # Units for each column
        self.data_types = {}         # Data type per column
        self.footnotes = []          # Table footnotes
        self.source = None           # Data source reference
```

#### 2. Semantic Context
```python
class SemanticTableContext:
    def __init__(self):
        self.purpose = None          # What the table represents
        self.domain = None           # Engineering/financial/scientific
        self.relationships = []      # Links to other tables
        self.formulas = []          # Associated calculations
        self.constraints = []        # Data constraints/rules
        self.validation_rules = []   # Business rules
```

#### 3. Surrounding Context
```python
class SurroundingContext:
    def __init__(self):
        self.preceding_text = ""     # Text before table (3 paragraphs)
        self.following_text = ""     # Text after table (3 paragraphs)
        self.section_title = ""      # Document section containing table
        self.page_number = 0         # Original page location
        self.references = []         # In-text references to table
        self.related_figures = []    # Associated figures/charts
```

### Table Extraction Pipeline

```python
class ContextualTableExtractor:
    def extract_with_full_context(self, pdf_path):
        """Complete table extraction with context preservation"""
        
        # Step 1: Detect all tables in document
        tables = self._detect_tables(pdf_path)
        
        # Step 2: Extract structural context
        for table in tables:
            table.structure = self._extract_structure(table)
            table.headers = self._identify_headers(table)
            table.units = self._extract_units(table)
            
        # Step 3: Extract semantic context
        for table in tables:
            table.semantic = self._analyze_semantics(table)
            table.domain = self._classify_domain(table)
            table.relationships = self._find_relationships(tables)
            
        # Step 4: Extract surrounding context
        for table in tables:
            table.context = self._extract_surroundings(table)
            table.references = self._find_references(table)
            
        # Step 5: Generate reusable format
        return self._format_for_reuse(tables)
```

### Table Storage Format

```python
class ReusableTable:
    """Standardized format for table storage and reuse"""
    
    def to_dict(self):
        return {
            'id': self.unique_id,
            'source_document': self.source_path,
            'extraction_timestamp': self.timestamp,
            'table_data': {
                'raw': self.raw_data,
                'cleaned': self.cleaned_data,
                'dataframe': self.to_dataframe()
            },
            'context': {
                'structural': self.structural_context,
                'semantic': self.semantic_context,
                'surrounding': self.surrounding_context
            },
            'metadata': {
                'quality_score': self.quality_score,
                'extraction_method': self.method,
                'validation_status': self.validated
            },
            'reuse_hints': {
                'test_data': self.suitable_for_testing,
                'calculation_inputs': self.calculation_ready,
                'reference_data': self.is_reference_table
            }
        }
    
    def to_test_fixture(self):
        """Convert to test fixture format"""
        return {
            'name': self.caption or f'table_{self.id}',
            'data': self.cleaned_data,
            'expected_columns': self.headers,
            'constraints': self.validation_rules,
            'test_cases': self._generate_test_cases()
        }
```

### Table Reuse Patterns

#### 1. Testing Data Generation
```python
class TableTestGenerator:
    def generate_test_data(self, extracted_table):
        """Generate test data from extracted tables"""
        
        # Create parametrized test cases
        test_cases = []
        for row in extracted_table.data:
            test_case = {
                'input': row[:-1],  # All columns except result
                'expected': row[-1], # Last column as expected result
                'context': extracted_table.context,
                'source': extracted_table.source_document
            }
            test_cases.append(test_case)
        
        return test_cases
```

#### 2. Validation Rules Extraction
```python
class ValidationExtractor:
    def extract_rules(self, table_with_context):
        """Extract validation rules from table context"""
        
        rules = []
        
        # Extract from footnotes
        for footnote in table_with_context.footnotes:
            if 'must be' in footnote or 'should be' in footnote:
                rules.append(self._parse_rule(footnote))
        
        # Extract from surrounding text
        context_text = table_with_context.surrounding_text
        rules.extend(self._extract_constraints(context_text))
        
        # Infer from data patterns
        rules.extend(self._infer_patterns(table_with_context.data))
        
        return rules
```

#### 3. Cross-Reference Mapping
```python
class TableReferenceMapper:
    def map_references(self, all_tables):
        """Create reference map between related tables"""
        
        reference_graph = {}
        
        for table in all_tables:
            # Find explicit references
            refs = self._find_explicit_refs(table.surrounding_text)
            
            # Find implicit relationships
            related = self._find_related_by_content(table, all_tables)
            
            reference_graph[table.id] = {
                'references': refs,
                'referenced_by': [],
                'related_tables': related,
                'dependency_order': None
            }
        
        return self._resolve_dependencies(reference_graph)
```

## Agent Delegation Matrix

| Task Type | Primary Agent | Secondary Agents | Tools |
|-----------|--------------|------------------|-------|
| HTML Table Extraction | HTML Context Agent | - | BeautifulSoup4, pandas |
| PDF Table with Context | PDF Context Agent | Table Context Analyzer | Camelot, pdfplumber |
| PDF Engineering Calculations | PDF Calculation Agent | PDF Formula Agent | SymPy, pdfplumber |
| Excel Financial Models | Excel-to-Python Agent | - | openpyxl, formulas |
| Scientific Paper Formulas | PDF Formula Agent | PDF Context Agent | LaTeX2SymPy, SymPy |
| Web Scraping | HTML Context Agent | - | BeautifulSoup4, Selenium |
| Report Data Extraction | PDF Context Agent | PDF Calculation Agent | pdfplumber, Camelot |
| Table Context Analysis | Table Context Analyzer | PDF Context Agent | NLP tools, pandas |

## Integration Patterns

### 1. Unified Context Interface
```python
class ContextExtractionManager:
    def __init__(self):
        self.agents = {
            'html': HTMLContextAgent(),
            'pdf': PDFContextAgent(),
            'excel': ExcelToPythonAgent(),
            'pdf_calc': PDFCalculationAgent(),
            'pdf_formula': PDFFormulaAgent()
        }
    
    def extract(self, file_path, task_type):
        file_type = self._identify_file_type(file_path)
        agent = self._select_agent(file_type, task_type)
        return agent.process(file_path)
```

### 2. Parallel Processing
```python
class ParallelContextExtractor:
    def process_batch(self, files, max_workers=5):
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = []
            for file in files:
                future = executor.submit(self.extract_context, file)
                futures.append(future)
            
            results = []
            for future in as_completed(futures):
                results.append(future.result())
        return results
```

### 3. Chain Processing
```python
class ChainedExtraction:
    def extract_and_convert(self, pdf_path):
        # Step 1: Extract context
        context = self.pdf_agent.extract_context(pdf_path)
        
        # Step 2: Extract formulas
        formulas = self.formula_agent.extract_formulas(context)
        
        # Step 3: Convert to Python
        python_code = self.converter.formulas_to_python(formulas)
        
        return python_code
```

## Performance Optimization

### 1. Caching Strategy
- Cache parsed documents for repeated access
- Store extracted formulas in database
- Maintain conversion mapping cache

### 2. Parallel Execution
- Process multiple files concurrently
- Distribute formula parsing across cores
- Async I/O for file operations

### 3. Memory Management
- Stream large PDFs instead of loading entirely
- Chunk processing for Excel files
- Garbage collection after extraction

## Table Testing & Validation Strategies

### 1. Extracted Table Testing
```python
class TableTestSuite:
    """Comprehensive testing for extracted tables"""
    
    def test_extraction_accuracy(self, original_pdf, extracted_tables):
        """Validate extraction accuracy"""
        tests = {
            'structure_preserved': self._test_structure(extracted_tables),
            'data_integrity': self._test_data_integrity(extracted_tables),
            'context_captured': self._test_context_capture(extracted_tables),
            'relationships_mapped': self._test_relationships(extracted_tables)
        }
        return tests
    
    def test_reusability(self, extracted_table):
        """Test table reusability in different contexts"""
        return {
            'as_test_data': self._test_as_fixture(extracted_table),
            'as_calculation_input': self._test_calculations(extracted_table),
            'as_validation_source': self._test_validation(extracted_table),
            'as_reference_data': self._test_reference(extracted_table)
        }
```

### 2. Context Validation
```python
class ContextValidator:
    def validate_context(self, table_with_context):
        """Validate extracted context completeness"""
        
        validations = []
        
        # Check caption extraction
        if table_with_context.caption:
            validations.append(('caption', True, 'Caption extracted'))
        else:
            validations.append(('caption', False, 'No caption found'))
        
        # Check header identification
        header_quality = self._assess_headers(table_with_context.headers)
        validations.append(('headers', header_quality > 0.8, f'Header quality: {header_quality}'))
        
        # Check surrounding context
        context_score = self._score_context(table_with_context.surrounding_text)
        validations.append(('context', context_score > 0.7, f'Context score: {context_score}'))
        
        return validations
```

### 3. Table Reuse Examples

#### Engineering Calculations
```python
def use_table_for_calculations(extracted_table):
    """Use extracted engineering table for calculations"""
    
    # Example: Load factors from extracted table
    load_factors = extracted_table.to_dataframe()
    
    # Apply calculations using table data
    for index, row in load_factors.iterrows():
        wind_load = row['wind_speed'] ** 2 * row['pressure_coefficient']
        safety_factor = row['safety_factor']
        design_load = wind_load * safety_factor
        
    return design_load
```

#### Test Data Generation
```python
def generate_test_fixtures(extracted_tables):
    """Generate test fixtures from extracted tables"""
    
    fixtures = {}
    for table in extracted_tables:
        fixture_name = table.caption.replace(' ', '_').lower()
        fixtures[fixture_name] = {
            'description': table.context.surrounding_text[:200],
            'data': table.data,
            'validation_rules': table.validation_rules,
            'source': f"{table.source_document}:page_{table.page_number}"
        }
    
    # Save as JSON for test use
    with open('test_fixtures.json', 'w') as f:
        json.dump(fixtures, f, indent=2)
    
    return fixtures
```

#### Data Validation
```python
def validate_against_reference(data, reference_table):
    """Validate data against extracted reference table"""
    
    violations = []
    
    for key, value in data.items():
        # Check against reference table constraints
        if key in reference_table.headers:
            col_idx = reference_table.headers.index(key)
            valid_values = [row[col_idx] for row in reference_table.data]
            
            if value not in valid_values:
                violations.append({
                    'field': key,
                    'value': value,
                    'valid_values': valid_values,
                    'source': reference_table.source_document
                })
    
    return violations
```

## Quality Assurance

### 1. Validation Methods
- Formula syntax validation
- Calculation accuracy testing
- Output format verification
- Cross-reference checking
- Table extraction accuracy validation
- Context completeness verification

### 2. Testing Strategy
- Unit tests for each agent
- Integration tests for workflows
- Performance benchmarks
- Edge case handling
- Table extraction regression tests
- Context preservation tests

### 3. Error Handling
- Graceful degradation for unsupported formats
- Fallback extraction methods
- Comprehensive error reporting
- Recovery mechanisms
- Table extraction fallback strategies
- Context inference for missing data

## Security Considerations

1. **Input Validation**: Sanitize file inputs
2. **Sandboxing**: Execute conversions in isolated environment
3. **Resource Limits**: Prevent DoS from large files
4. **Formula Injection**: Validate generated Python code

## Success Metrics

1. **Extraction Accuracy**: >95% text extraction accuracy
2. **Formula Conversion**: >90% successful conversions
3. **Processing Speed**: <2 seconds per page
4. **Parallel Efficiency**: 3-5x speedup with parallelization

## Implementation Priority

1. **Phase 1**: Core agent framework and base classes
2. **Phase 2**: HTML and PDF context agents
3. **Phase 3**: Excel-to-Python conversion
4. **Phase 4**: PDF calculation/formula extraction
5. **Phase 5**: Integration and optimization

## Dependencies

```toml
[dependencies]
beautifulsoup4 = "^4.12.0"
lxml = "^4.9.0"
pdfplumber = "^0.10.0"
pypdf = "^3.17.0"
openpyxl = "^3.1.0"
pandas = "^2.0.0"
numpy = "^1.24.0"
sympy = "^1.12"
formulas = "^1.2.0"
camelot-py = "^0.11.0"
```

## Next Steps

1. Create detailed implementation tasks
2. Set up development environment
3. Implement base agent framework
4. Develop file-specific agents
5. Create integration tests
6. Deploy and monitor performance