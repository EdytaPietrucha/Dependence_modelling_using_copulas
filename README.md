# Dependence modelling using copulas functions
The analysis is about modelling dependence structure between financial indexes using copulas functions, especially C-vine and D-vine copula classes.

### Objective: 
Modelling dependency structure between stock indexes from three sectors:
- Information technology: Apple (AAPL), Microsoft (MSFT), Nvidia (NVDA),
- Finance: Citigroup Inc (C), Goldman Sachs Group Inc (GS), JPMorgan Chase \& Co (JPM),
- Services: The Walt Disney Company (DIS), Netflix (NFLX), Amazon.com (AMZN).

### Data: 
Daily values on close from 2014/08/25 to 2022/08/23.

### Metrics: 
AIC, BIC, Log-likelihood.
### Conclusion: 
- Marginals - best metrics scores achived for t-Student distribution.
- Joint distribution - best metrics scores achived for D-vine copula.

Conclusion together with results of the analysis can be found in file Dependence_modelling_using_copula.pdf

Figure 1: Level data for investigated stock indexes.
<img src = "https://github.com/EdytaPietrucha/Dependence_modelling_using_copulas/assets/115400582/bc6246c1-28a2-40d0-85b4-89a7de607e58" width="700" height="400">

Figure 2: Spearman matrix on empirical data vs Spearman matrix on simulated data from fitted benchmark copula. 

<img src = "https://github.com/EdytaPietrucha/Dependence_modelling_using_copulas/assets/115400582/eae33af7-21b9-4cc5-aba5-7f8920933e44" width="600" height="300"> | <img src = "https://github.com/EdytaPietrucha/Dependence_modelling_using_copulas/assets/115400582/8e88040d-648e-4de2-b27f-38b7fa43e965" width="600" height="300">
:-------------------------:|:-------------------------:
