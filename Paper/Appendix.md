# Appendix
Xavier Marquez
2026-07-09

This appendix is rendered from the project’s `targets` pipeline. The
code chunks below read precomputed objects from the targets store
(`_targets/`) using `tar_read_raw()`. If you change the pipeline
configuration (e.g., `config/pipeline.yml`) or rerun the pipeline with
different settings, rerun `targets::tar_make()` before rendering this
document.

Throughout, `decade` refers to the left endpoint of each 10-year time
slice (e.g., `1700` denotes 1700-1709).

## Sample Characteristics

![](Appendix_files/figure-commonmark/unnamed-chunk-1-1.png)

Our sample includes up to 40 volumes per decade.

![](Appendix_files/figure-commonmark/unnamed-chunk-3-1.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-4-1.png)

The larger sample includes up to 500 volumes per decade.

These represent varying proportions of the total sample, though volumes
that mention democracy more often are more likely to be in the sample.
(The probability of selection into the sample is weighted by the number
of mentions of ‘democracy’ in the volume).

![](Appendix_files/figure-commonmark/unnamed-chunk-5-1.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-6-1.png)

## Graphs with all weights

![](Appendix_files/figure-commonmark/unnamed-chunk-7-1.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-1.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-2.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-3.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-4.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-5.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-6.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-7.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-8.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-9.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-10.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-11.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-12.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-13.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-14.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-15.png)

![](Appendix_files/figure-commonmark/unnamed-chunk-9-16.png)

## Graphs for Individual Models

    _Graphs for individual models are not yet available (the file `Paper/graph_appendix.rmd` needs to be regenerated from the current targets pipeline)._

## Model performance graphs

<div id="fig-combined_performance_graph_classification_auc">

![](Appendix_files/figure-commonmark/fig-combined_performance_graph_classification_auc-1.png)

Figure 1: Predictive performance of classification models, AUC measure.

</div>

<div id="fig-combined_performance_graph_classification_acc">

![](Appendix_files/figure-commonmark/fig-combined_performance_graph_classification_acc-1.png)

Figure 2: Predictive performance of classification models, accuracy
measure.

</div>

<div id="fig-combined_performance_graph_classification_kap">

![](Appendix_files/figure-commonmark/fig-combined_performance_graph_classification_kap-1.png)

Figure 3: Predictive performance of classification models, Cohen’s Kappa
measure.

</div>

<div id="fig-combined_performance_graph_classification_log_loss">

![](Appendix_files/figure-commonmark/fig-combined_performance_graph_classification_log_loss-1.png)

Figure 4: Predictive performance of classification models, log loss
measure.

</div>
