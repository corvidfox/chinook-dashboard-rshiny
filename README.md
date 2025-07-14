# Chinook Dashboard — SQL-Driven Business Intelligence in Shiny

This interactive dashboard analyzes the [Chinook](https://github.com/lerocha/chinook-database) music store dataset using SQL, R, and Shiny to surface business insights on revenue performance, customer retention, and purchase behavior.

**Why This Project?**  
Designed as a compact but scalable demonstration of real-world business analytics workflows. While the dataset is small, the app simulates BI tooling for datasets in the hundreds of millions.

---

## Key Features

- **SQL-powered KPIs:** Efficient joins, CTEs, and windows in DuckDB.
- **Interactive filtering:** Genre, country, artist, and date range.
- **Query Staging with Temp Tables:** Reduces compute load for downstream joins and CTEs.
- **Metadata enrichment via catalog mapping:** Supports informative summaries and top-N analyses.
- **Modular architecture:** Each panel is a self-contained Shiny module.
- **Dynamic visualizations:** ggplot2 + Plotly, with light/dark theming.
- **Retention analysis:** Cohort logic and decay curves built from first principles.
- **Narrative insights:** Executive summaries and strategic bullet points.

---

## Tech Stack

| Tool        | Purpose                     |
|-------------|-----------------------------|
| R + Shiny   | Frontend UI and server logic|
| DuckDB      | SQL backend (read-only mode)|
| ggplot2     | Static visualizations       |
| Plotly      | Interactive charts          |
| bslib       | Theme switching (light/dark)|
| cachem      | Optimized reactivity        |

---

## Who Built This?

I'm a healthcare researcher and bioinformatician with a background in epigenomics and nursing, transitioning into data-focused roles across industries. This portfolio piece is aimed at demonstrating storytelling, ETL logic, dashboard engineering, and analyst workflows.

See my personal porfolio site and browse more projects at [CorvidFox.github.io](https://corvidfox.github.io/)!

---

##  How to Run

This app is structured using `global.R`, `ui.R`, and `server.R`. Hosted on Posit Connect with GitHub integration, it is designed for walkthroughs, demos, or recruiter reviews.

```r
shiny::runApp("chinook-dashboard-rshiny")
