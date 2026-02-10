# Prestige over Merit: An Adapted Audit of LLM Bias in Peer Review

## Overview

This repository contains the replication code for:

> Howell, A., Wang, J., Du, L., Melkers, J., & Shah, V. "Prestige over Merit: An Adapted Audit of LLM Bias in Peer Review."

We adapt a résumé-style audit to scientific publishing using a multi-role LLM simulation (editor/reviewer) that evaluates high-quality manuscripts across the physical, biological, and social sciences under randomized author identities (institutional prestige, gender, race). The audit reveals a strong and consistent institutional-prestige bias: identical papers attributed to low-prestige affiliations face a significantly higher risk of rejection, despite only modest differences in LLM-assessed quality.

## Repository Structure

```
├── DataAnalysis.R          # Main analysis script (all figures, tables, and results)
├── AuthorAttributes.csv    # Author identity attributes used in the audit
├── df_final.csv            # Processed analysis-ready dataset
└── README.md
```

## Data

The full dataset, including the large combined LLM review results file, is hosted on Zenodo:

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18598214.svg)](https://zenodo.org/records/18598214)

**Zenodo repository:** [https://zenodo.org/records/18598214](https://zenodo.org/records/18598214)

Download `LLM_AI_Bias_review_combined_results_allPapers.csv` from Zenodo and place it in the root directory of this repository to reproduce all analyses.


### Requirements

- R (≥ 4.0)
- Required R packages are loaded at the top of `DataAnalysis.R`

## Citation

If you use this code or data, please cite:

> Howell, A., Wang, J., Du, L., Melkers, J., & Shah, V. "Prestige over Merit: An Adapted Audit of LLM Bias in Peer Review."

## Contact

Anthony Howell — [Anthony.Howell@asu.edu](mailto:Anthony.Howell@asu.edu)  
School of Public Affairs, Arizona State University

## License

This project is provided for academic replication purposes. Please contact the authors for reuse inquiries.
