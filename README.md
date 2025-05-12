# Propofol-analysis
This repository contains the full code for modeling ICU transitions in relation to propofol use, using multi-state models and sensitivity analyses.

Folder Structure
Before running any code, make sure your folder structure looks like this:
Propofol-analysis/
├── data/
│   ├── ICU.Rds
│   ├── daily.Rds
│   ├── daily_all.Rds
│   ├── excluded.csv
│   ├── excluded_ids.Rds
│   ├── mergedAndCleanedData.Rds
│   └── patient.Rds
├── models/
│   ├── models-main.Rmd
│   ├── models-age-subgroup.Rmd
│   ├── Sensitivity Analysis.Rmd
│   ├── MultiStateAnalysis.R
│   └── Patientenstatus.R
├── preproc-data.R
├── preproc-helpers.R
├── Propofol_Analyse_Bericht.pdf
├── sessionInfo.txt
└── README.md

## ▶️ Run Order

Run the following scripts **in this exact order** to reproduce the full analysis:

1. `models/Patientenstatus.R`  
   Descriptive analysis

2. `models/models-main.Rmd`  
   Fits the primary multi-state model and plots the baseline transition probabilities.

3. `models/models-age-subgroup.Rmd`  
   Stratifies the model by age groups (≤ 65 vs. > 65).

4. `models/MultiStateAnalysis.R`  
   Alternative implementation and model comparison code.

5. `models/Sensitivity Analysis.Rmd`  
   Runs the sensitivity analysis excluding palliative care deaths (1–2 days after propofol change).

## 💡 Notes

- All models use `pammtools` and `mgcv::bam()` for time-to-event modeling in the PED framework.
- Data must be cleaned and pre-processed before model fitting (provided in `mergedAndCleanedData.Rds`).
- Figures are saved/embedded within each Rmd script.
- Sensitivity analysis accounts for lead/lag bias due to end-of-life care interventions.

---

For any issues, feel free to open an issue or contact the repository maintainer.
