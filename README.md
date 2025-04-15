# Contract Staffing in Nursing Homes

This repository contains data and code for visualizing monthly and geographic trends in nursing home contract staffing, and summarizing facility characteristics by levels of contract staffing use.

## Project overview

This project generates:
1. A **figure** showing monthly trends in nursing home contract staffing over time in a small sample of facilities
2. A few **tables** summarizing nursing home characteristics by contract staffing patterns
3. A **map** showing county-level contract staffing prevalence in the Northeast

## Data sources

I used the following CMS datasets:
- [**Payroll-Based Journal (PBJ) Daily Nurse Staffing Data**](https://data.cms.gov/quality-of-care/payroll-based-journal-daily-nurse-staffing): Retrieved via API (no key needed)
- [**Nursing Home Compare Provider Information**](https://data.cms.gov/provider-data/dataset/4pq5-n9py): Downloaded from webpage

## How to reproduce the analysis

### 1. Clone the repository

```{sh}
git clone https://github.com/taliabenheim/nursing-home-staffing.git
```

### 2. Restore dependencies

I used `renv` to store the required R packages. To restore the required packages, run:

```{r}
renv::restore()
```

### 3. Run the Master code file

In your R console, run the following script to execute the full code. Please note that the code to retrieve the PBJ data from the API (which is included in this Master file) can take **several hours** to run.

```{r}
source("master.R")
```

If you instead prefer to run the code from an intermediate version of the PBJ data, run:

```{r}
source("master_intermediate.R")
```

The resulting tables and figures will be stored in the output folder.