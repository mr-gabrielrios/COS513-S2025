The directory tree of the `scripts` folder is presented below. Note that the models used (BSTS and GP regression) are contained in separate subdirectories from the data processing workflow.

``` bash

scripts
├── bsts
│   ├── bsts_prelim
│   │   ├── ALL_US_monthly_dataset.csv
│   │   └── bsts_prelim.R
│   └── bsts_test
│       ├── bsts.R
│       └── ercot_test_dataset.csv
├── data_processing
│   ├── cds_scraper.ipynb
│   ├── consolidate-CMIP6.ipynb
│   ├── consolidate-EIA.ipynb
│   ├── consolidate-ERA5.ipynb
│   └── data_processing.ipynb
└── gp_regression
    ├── ALL_US_monthly_dataset.csv
    ├── gpr_complete_prediction.png
    ├── gpr_model.ipynb
    └── gpr_test_prediction.png

```
