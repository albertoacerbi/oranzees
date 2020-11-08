# Oranzees model

This repository contains the code and documents the development of what we have called here "oranzees model". A description of the model is available [here](model-description.pdf). A manuscript is avialable on the preprint server [bioRxiv](https://www.biorxiv.org/content/10.1101/2020.03.25.008177v1).

The rest of the material is organised as follows. Notice this may not be always updated, because we are keep working on the model and on the outputs, but it should be sufficient to understand and reproduce the results described in the manuscript:

* [01-oranzees.Rmd](01-oranzees.Rmd) to [06-oranzees.Rmd](06-oranzees.Rmd): R Markdown files documenting the model implementation details, mainly for "internal" use. They record the history of the development of the model.

* [model-description.Rmd](model-description.Rmd): a shorter description of the model when first completed (now mostly incorporated in the main manuscript and in SI).

* [main.R](main.R): code to run the model

* [test.R](test.R): code to run tests

* [test-results.Rmd](test-results.Rmd): results of test after first stage of development

* [sims-for-ms.Rmd](sims-for-ms.Rmd): results and visualisations to use in the main manuscript

* [extra-results.Rmd](extra-results.Rmd): possible extension of the main results

Plus four folders:

* [draft/](draft): folder with material for the manuscript (written with R Markdown)

* [material/](material): folder with media

* [output/](output): folder with outputs from simulations used in the process of development

* [output_test/](output_test): folder with outputs from simulations used in the tests

* [sims_for_ms](sims_for_ms): folder with outputs to use in the manuscript

* [extra_results](extra_results): folder with output for possible extensions of the main results
