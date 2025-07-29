### Under the Hood

This dashboard was built to simulate a scalable business intelligence workflow using a small but structured dataset. While the Chinook dataset contains only hundreds of records, the app is designed to handle much larger volumes with reactive filters, modular architecture, and optimized SQL queries.

#### Data Pipeline & ETL

- **Source:** A static DuckDB database hosted on GitHub in read-only mode.
- **ETL Logic:** SQL queries use joins, CTEs, and window functions to compute KPIs like revenue, customer retention, and genre performance.
- **Filter Validation:** All filters (date range, genre, artist, country, metric) are reactive and validated. No selection defaults to the full dataset.
- **Filter Staging:**  
  - Services in `services/sql_filters.py` build a temporary `filtered_invoices` table (filtered by genre, artist, country but not date) to shrink downstream query scopes.  
  - A date-range filter is applied later in each query to preserve temporal integrity.  
- **Catalog Mapping:** A pre-joined reference table of the full genre/artist catalog is used to annotate filtered results with metadata like track counts and artist scope.
- **Optimization:** Filters are debounced and cached where appropriate to reduce recomputation and improve responsiveness.

#### KPI & Visualization Engine

- **KPI Cards:** Each module begins with a row of KPI cards, built using reusable helper functions and styled dynamically via a theme system.
- **Plots:** Visualizations include time-series trends, stacked bar plots, choropleths, and retention curves, all rendered with ggplot2 or Plotly.
- **Tables:** Scrollable data tables are paired with download buttons for CSV export, enabling deeper exploration and reporting.

#### Modular Architecture

- Each tab is a self-contained module with its own `module_ui.R`, `module_server.R`, and `module_helpers.R`.
- Modules follow a consistent layout: KPI Cards → Plot → Table → Download.
- This structure supports scalability and reuse across future projects.

#### Theming & UX

- The app supports light/dark mode switching using `bslib`, with reactive styling applied to all plots and UI elements.
- Plotly charts are restyled dynamically to match the selected theme.
- All UI components are responsive and optimized for readability.

#### Deployment & Hosting

- Hosted on Posit Connect with GitHub integration.
- App uses a standard `global.R`, `ui.R`, `server.R` structure for clarity and portability.
- Designed to be lightweight, fast-loading, and easy to demo in interviews or walkthroughs.

#### Ideas for Further Development

This dashboard was designed to be lightweight, responsive, and modular, but there are several areas where deeper functionality could be added if needed:

- **Caching and Performance:** Plots currently re-render on filter changes. Future optimization could include caching visuals and integrating Plotly proxies for theme-aware restyling.
- **UI Experience:** Enhancements like animated transitions, richer hover tooltips, and refined mobile responsiveness could elevate user engagement.
- **Deeper Granularity:** Cross-cutting analysis (e.g. artist performance by country, genre trends within market segments) could yield more actionable insights in enterprise-scale deployments.
- **Narrative Generator:** A summary module that interprets filtered KPIs and visual trends in plain text could help analysts surface patterns automatically.
- **Alerting System:** In larger datasets, automated triggers could flag anomalies such as sudden revenue drops or retention outliers.
- **Expand staging logic**: Dynamically partition data by time or region to support concurrent views or multi-user access at scale.

While this project is scoped for portfolio clarity, it’s designed with scalability in mind, offering room to grow if new datasets, use cases, or audience needs arise.

---

This dashboard was built by a healthcare researcher with a background in bioinformatics and nursing, applying analytical rigor and storytelling to a business context. It’s a demonstration of SQL fluency, ETL design, and interactive data visualization, with a focus on clarity, performance, and real-world relevance.
