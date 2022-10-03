# accessibility_paris
This is the R code allowing to reproduce the results of the paper "Can Public Transport Improve Accessibility for the Poor Over the Long Term? Empirical Evidence in Paris, 1968-2010", by Vincent Viguié, Charlotte Liotta, Basile Pfeiffer and Nicolas Coulombel, submitted to the Journal of Transport Geography and available at SSRN: https://ssrn.com/abstract=4049765 or http://dx.doi.org/10.2139/ssrn.4049765

### Data
Required inputs (average transport time between municipalities by public transit for the Census years between 1968 and 2019 and by car for the year 2010, and number of employed workers, by place of residence or employment, at the municipality level) are available here: https://doi.org/10.5281/zenodo.7139257.

### Decomposition of accessibility
main.R allows to compute the evolution of the weighted accessibility to jobs by CSP (socio-professional categories) and to decompose the evolution of job accessibility between different factors (public transport evolution, growth of the number of jobs, jobs displacement, households displacement). robustness_no_match.R allow to run the same analysis, but without matching skills (i.e. without accounting for CSPs and considering that all workers can work in all jobs). robustness_gravity.R allow to run the same analysis, but with another common accessibility measure based on an exponentially decreasing impedance function. More details can be found in the research paper.

### Charts
The Figures of the research paper can be reproduced by running charts.R, using the outputs from main.R, robustness_no_match.R or robustness_gravity.R.
