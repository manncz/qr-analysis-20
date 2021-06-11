# Applied Qualifying Exam Analysis 2020



### Contents
**figures**
* all figures included in report

**input**
 * pop_mort_csv: data provided by faculty from the CDC/NCHS and Census

**paper**
* main.tex: LaTex file for submitted report.
* ref.bib: bibliography file.

**scripts**
* 01_read_data.R: read in provided data and do minor cleaning.
* 02_eda.R: plots of trends in mortality
* 03_mort_model.R: model selection for a model to conduct inference on the association between age, gender, and month and mortality
* 05_mort_pred.R: model fitting for a linear, random forest, and gradient boosted model to predict mortality
* 05.01_mort_pred_compare.R: visualizing comparisons between models in terms of prediction error

**temp**

* tidy_mort_dat.Rdata: cleaned mortality data, outputted from 01_read_data.R.


Applied_QR_2020.pdf: instructions provided for the exam
Mann_Applied_QR.pdf: submitted report

### To recreate

Run all R files in order as the names suggest.
