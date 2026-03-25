"""
Shared CSS for OrcaFlex Analysis Reports.
Matches the OrcaWave report style with a dark header (#2c3e50).
"""

REPORT_CSS = """
:root {
    --primary-color: #2c3e50;
    --secondary-color: #34495e;
    --accent-color: #3498db;
    --success-color: #27ae60;
    --warning-color: #f39c12;
    --danger-color: #e74c3c;
    --bg-color: #f8f9fa;
    --card-bg: #ffffff;
    --text-color: #2c3e50;
    --text-muted: #7f8c8d;
    --border-color: #dee2e6;
}

* {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
}

body {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    line-height: 1.6;
    color: var(--text-color);
    background-color: var(--bg-color);
    padding-top: 60px; /* Space for sticky header */
}

/* Header & Navigation */
header {
    background-color: var(--primary-color);
    color: white;
    padding: 0.75rem 2rem;
    position: fixed;
    top: 0;
    width: 100%;
    z-index: 1000;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.header-content {
    display: flex;
    justify-content: space-between;
    align-items: center;
    max-width: 1400px;
    margin: 0 auto;
}

.header-main {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
}

.header-title {
    font-size: 1.25rem;
    font-weight: bold;
}

.header-meta {
    font-size: 0.8rem;
    color: rgba(255,255,255,0.7);
}

.nav-links {
    display: flex;
    gap: 0.75rem;
    flex-wrap: wrap;
    justify-content: flex-end;
}

.nav-links a {
    color: rgba(255,255,255,0.8);
    text-decoration: none;
    font-size: 0.9rem;
    transition: color 0.3s;
}

.nav-links a:hover {
    color: white;
}

/* Container */
.container {
    max-width: 1200px;
    margin: 2rem auto;
    padding: 0 1rem;
}

/* Section Cards */
.section-card {
    background-color: var(--card-bg);
    border-radius: 8px;
    box-shadow: 0 4px 6px rgba(0,0,0,0.05);
    margin-bottom: 2rem;
    padding: 2rem;
    border: 1px solid var(--border-color);
}

.section-empty {
    text-align: center;
    color: var(--text-muted);
    border-style: dashed;
}

.section-title {
    color: var(--primary-color);
    border-bottom: 2px solid var(--accent-color);
    padding-bottom: 0.5rem;
    margin-bottom: 1.5rem;
    font-size: 1.75rem;
}

.section-body h3 {
    margin: 1.5rem 0 1rem;
    font-size: 1.25rem;
    color: var(--secondary-color);
}

/* Badges */
.badge {
    display: inline-block;
    padding: 0.25rem 0.5rem;
    border-radius: 4px;
    font-size: 0.85rem;
    font-weight: bold;
    text-transform: uppercase;
}

.badge-pass { background-color: var(--success-color); color: white; }
.badge-fail { background-color: var(--danger-color); color: white; }
.badge-warning { background-color: var(--warning-color); color: white; }
.badge-info { background-color: var(--accent-color); color: white; }

/* Tables */
table {
    width: 100%;
    border-collapse: collapse;
    margin-bottom: 1rem;
}

th, td {
    padding: 0.75rem;
    text-align: left;
    border-bottom: 1px solid var(--border-color);
}

th {
    background-color: #f8f9fa;
    font-weight: 600;
}

tr:hover {
    background-color: #fcfcfc;
}

/* Plotly Responsive */
.plotly-chart {
    width: 100%;
    min-height: 400px;
    margin-bottom: 1rem;
}

/* Responsive Grid */
.grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 1.5rem;
    margin-bottom: 2rem;
}

.stat-card {
    background-color: #f8f9fa;
    border-radius: 6px;
    padding: 1.5rem;
    text-align: center;
    border: 1px solid var(--border-color);
}

.stat-label {
    font-size: 0.85rem;
    color: var(--text-muted);
    text-transform: uppercase;
    letter-spacing: 1px;
    margin-bottom: 0.5rem;
}

.stat-value {
    font-size: 2rem;
    font-weight: bold;
    color: var(--primary-color);
}

/* Footer */
footer {
    text-align: center;
    padding: 2rem;
    color: var(--text-muted);
    font-size: 0.9rem;
}

.no-data {
    font-style: italic;
    color: var(--text-muted);
    padding: 1rem;
}
"""
