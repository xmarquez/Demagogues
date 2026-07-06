# Pipeline Configuration

The default pipeline now uses split configuration files:

- `corpus.yml`: shared corpus, sampling, DFM, model, graph, and output defaults.
- `features/*.yml`: one feature dictionary/search definition per concept.
- `runs/*.yml`: one experiment/run spec that selects features and overrides defaults.

Select a run with `TARGET_RUN`:

```powershell
$env:TARGET_RUN = "auth_glmnet_40"
Rscript -e "targets::tar_manifest()"
```

Set `TARGET_CONFIG=config/pipeline.yml` to force the legacy monolithic config.

Run specs can set `worksets.method`:

- `auto`: try the legacy HTRC Solr workset builder, then fall back to Bookworm.
- `solr`: require the legacy HTRC Solr workset builder.
- `bookworm`: use HathiTrust+Bookworm `search_results` directly.

Feature specs can set report-only graph metadata:

- `graphs.tracked_terms`: named terms to include in the report's term-weight
  trajectory section. These are not part of the feature dictionary, so changing
  them should only regenerate the run-level QMD/HTML report.
- `graphs.tracked_top_n`: number of automatically selected top adjective, verb,
  and noun terms per predictive model to plot in the run-level graph report.

Current authoritarianism experiments:

- `auth_glmnet_40`: original combined `authoritarianism`/`authoritarian` pilot,
  40 restricted volumes per decade where available.
- `authoritarianism_glmnet_100`: `authoritarianism` only, up to 100 restricted
  volumes per decade, using 1910-2010 because earlier decades had fewer than 40
  usable catalog-filtered volumes; fits glmnet, LiblineaR, naive Bayes, and
  xgboost classifiers.
  Standalone SVD and PPMI graph targets are disabled for this run.
- `authoritarian_glmnet_100`: `authoritarian` only, up to 100 restricted
  volumes per decade, using 1890-2010 because earlier decades had fewer than 40
  usable catalog-filtered volumes; fits glmnet, LiblineaR, naive Bayes, and
  xgboost classifiers.
  Standalone SVD and PPMI graph targets are disabled for this run.
- `authority_glmnet_100`: `authority` only, up to 100 restricted
  volumes per decade for 1700-2010; fits glmnet, LiblineaR, naive Bayes, and
  xgboost classifiers.
  Standalone SVD and PPMI graph targets are disabled for this run.
- `legitimacy_glmnet_100`: `legitimacy` only, up to 100 restricted
  volumes per decade for 1700-2010; fits glmnet, LiblineaR, naive Bayes, and
  xgboost classifiers.
  Standalone SVD and PPMI graph targets are disabled for this run.
- `legitimate_glmnet_100`: `legitimate` only, up to 100 restricted
  volumes per decade for 1700-2010; fits glmnet, LiblineaR, naive Bayes, and
  xgboost classifiers.
  Standalone SVD and PPMI graph targets are disabled for this run.

## Latin / Perseus prototype

The Latin prototype is intentionally separate from the Hathi/Bookworm target
graph. It uses `_targets_latin.R`, `config/latin_corpus.yml`,
`config/latin_features/*.yml`, and `config/latin_runs/*.yml`.

Run the initial Perseus-only `auctoritas` pilot with:

```powershell
$env:LATIN_RUN = "auctoritas_perseus"
Rscript -e "targets::tar_make(script = '_targets_latin.R')"
```

The prototype clones `PerseusDL/canonical-latinLit` into
`data/perseus/canonical-latinLit` when needed, extracts form-based occurrence
windows around the configured lemma forms, computes context-term PPMI against
full-corpus baselines, fits a PPMI-weighted SVD over the context-window DFM, and
writes a report to `Paper/latin_auctoritas_perseus.qmd` / `.html`.

Ancient composition dates are not reliably encoded in the Perseus TEI headers,
so broad periods are assigned through the editable `metadata.author_periods`
rules in `config/latin_corpus.yml`.

## Marx/Engels prototype

The Marx/Engels prototype is part of the main targets pipeline, but it uses its
own corpus settings in `config/marxism_corpus.yml` and
`config/marxism_runs/*.yml`. The first run is
`authority_marxism_glmnet_100`, which builds POS-tagged segment DFMs from the
migrated CoreNLP RDS annotations under `D:/ResearchData/corpora/marxism`.

Run the audit and graph pipeline with:

```powershell
$env:TARGET_RUN = "authority_marxism_glmnet_100"
Rscript -e "targets::tar_make(names = marxism_graph_html)"
```

The `marxism_feature_audit` target reports feature support by decade before
model fitting. The generated report is written to
`Paper/marxism_graphs_authority_marxism_glmnet_100.qmd` / `.html`.
