# Improvement Plan: The Vector Space of Democracy

*Prepared 2026-07-05 by Claude (Fable 5) after a full review of the pipeline, methods code, and paper draft.*
*Tasks are labeled (M = methods, B = bug, H = HPC, P = parameterization, A = architecture/hygiene, W = writing) and grouped in four phases. Each task states the files to touch and an acceptance criterion so that it can be handed to an implementation agent as-is.*

---

## Executive summary

The project is a methodologically interesting, mostly-built pipeline (targets + HathiTrust Extracted Features) supporting a paper on the conceptual history of "democracy," with a distinctive "models as readers" framing. The main problems, in order of importance:

1. **A weight-normalization bug** that contaminates every entropy/KL/JSD result in the paper (M1/B1).
2. **Inconsistent normalization across the information-theoretic measures** — entropy/JSD use one scheme, KL another (M2).
3. **The HPC execution path is dead**: `_targets.R` uses a local `crew` controller while all the `tar_resources(future = ...)` / `batchtools` Slurm plumbing is silently ignored, and the Slurm scripts load R 4.0.2 against an renv lockfile pinned to R 4.5.1 (H1–H4).
4. **Hyperparameters are hard-coded** in `R/modeling_functions.R` rather than exposed in the YAML config (P1–P3).
5. **The paper draft is ~60% complete**, with truncated sentences, stub sections, stale Rmarkdown cross-references, and claims that no longer match the pipeline configuration (W1–W6).

None of these are fatal. Recommended order: Phase 1 (correctness) → Phase 2 (HPC) → Phase 3 (parameters + uncertainty) → Phase 4 (paper). Phases 1 and 3 change results, so do them **before** any expensive full cluster run.

---

## Progress log

### 2026-07-06 — Phase 1 essentially complete (two delegated batches, reviewed and verified)

**Batch 1** (M1, M2, B2, B3, B6a–c/e/g/h): `normalize_weights()` helper added with `positive`/`shift`/`softmax` methods and degenerate-case guards; all six buggy normalization sites replaced; `kl_simple()`/`jsd_simple()` now consume `normalized_value` (this also removed a latent `group_by(!!var)` string-constant bug in `kl_simple`); LiblineaR class weights corrected to inverse-frequency; `most_similar_downsample()` de-hard-coded; `downsampled=`/`strata=` call-site fixes; `kl_sparse()` deleted; duplicate `sentiments_afinn` block removed from the paper; NA imprint-year drop made explicit; stale root files moved to `attic/`. Review fix on top of the agent's work: in `model_weights.cv.glmnet` the intercept is now filtered **before** normalization (it was previously absorbing probability mass).

**Batch 2** (B4, B5, B6d, M1-config, M3.2, KL flag — plus two newly discovered bugs):

- **B8 (new bug, found in review): performance evaluation never received the model's weighting scheme.** `model_performance()` was called without `weight`, so `match.arg()` silently evaluated **every** model on PPMI-weighted predictors, including models trained on tfidf/none. Fixed: `weight = predictive_model_dfm_weight` is now passed. This alone means all historical performance numbers (incl. the paper's weighting-scheme table) need regeneration.
- **B7 (new bug, confirmed): "testing - OOD" never used the unrestricted DFM.** The OOD parameter rows carried a `testing_dfm_object` column that the single tar_eval command never referenced — OOD metrics were actually computed on the model's own balanced DFM. Fixed by splitting into two tar_eval blocks; the OOD block evaluates on `testing_dfm_object` with `reference_dfm = get_training_sample(dfm_object, split_object)`.
- **B4 done:** `dfm_tfidf_apply()` / `dfm_ppmi_apply()` (train-statistics weighting; `dfm_ppmi()` refactored onto a shared `dfm_ppmi_worker()`), new internal `get_x_eval()` used by all `model_performance*` methods, `reference_dfm` argument throughout. Output lives in the reference's feature space; `dfm_match` to model features downstream restores zeros.
- **B5 done (design per Xavier):** duplicates/editions are **kept by default** (multiplicity = popularity signal); `samples.dedupe: none|bib_key|author_title` config flag for robustness runs. Review fix: dedupe operates **within period**, so reprints in other decades still count.
- **B6d done:** stable `workset_volume_count_<feature>` / `workset_meta_volume_count_<feature>` targets replace the hashed target names in the paper (line ~93 updated).
- **M1-config done:** `weights.normalization: positive` in `config/corpus.yml`, plumbed through `object_parameters.R` → `model_weights(method=)` / `normalize_weights(method=)` at all call sites; changing it invalidates correctly via targets' global tracking.
- **M3.2 done:** `rank_agreement_by_period()` (Spearman on common vocab + top-100 overlap) + `rank_agreements` target, mirroring `weight_correlations`.
- **KL vocab (per Xavier): union + epsilon stays the headline measure; `kl.common_vocab_robustness: false` flag adds intersection-based KL targets (`…_commonvocab`) for the appendix when enabled.**
- Flag slugs are dropped when at default values, so **default-config target names are unchanged** (verified by sourcing `object_parameters.R` under `TARGET_RUN=full_democracy`: no flag leakage in ids; 144 standard + 36 OOD performance rows build).
- Tests: `tests/testthat/` now covers normalize_weights, LiblineaR weights, reference weighting (`*_apply(x, x)` ≡ self-weighting), and rank agreement — **74 pass / 0 fail** (testthat installed in renv).

**Still open in Phase 1:** M3.1 (paper caveat text), M3.3 partially (JSD casting still conflates absent vs zero — moot under `positive` normalization, note in paper), M4 (paper caveats + OCR-noise target + topic-model decision). These fold naturally into Phase 4 writing work.

**Consequence:** the existing `_targets` store is now stale for all weight, performance, entropy/KL/JSD, and graph targets. Do not regenerate paper figures until the next full run (ideally after Phase 2 so it happens on Rāpoi).

### 2026-07-06 — Phase 2 complete (delegated, reviewed and verified; cluster-side steps untested until first run)

Design decisions (Xavier): container image built by **GitHub Actions → GHCR** and `singularity pull`ed on Rāpoi (Rāpoi has Singularity 3.10.2, not Apptainer — same SIF/CLI); code deployed **via git push + ssh pull**; results returned as a **curated bundle**; SSH key setup checked/bootstrapped by the deploy script.

What was built:

- **Container:** `docker/Dockerfile` (rocker/r-ver:4.5.1 + system libs + Quarto CLI + openssh-client + `renv::restore()` into the site library, `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE` so the project .Rprofile can't hijack `.libPaths()`); `.github/workflows/container.yml` pushes `ghcr.io/xmarquez/demagogues:{latest,sha-*}` on renv.lock/docker changes. **One-time step: flip the GHCR package to Public after the first Actions build.**
- **Backend:** `_targets.R` now uses a crew controller **group** named `std`/`bigmem` on both backends (`TARGETS_BACKEND=local|slurm`, local default → Windows dev unchanged); all 8 dead `tar_resources(future=...)` blocks removed; heavy DFM targets route to `bigmem`. `cluster_resources.R` rewritten: env-driven config, Slurm controllers via crew.cluster 0.4.0, and the worker **container re-exec pattern** (crew's generated script calls bare `Rscript` from PATH — confirmed from crew.cluster source — so `script_lines` re-exec the script inside the container via `singularity exec ... /bin/bash "$0"`). Worker scripts/logs on scratch (crew's tempdir default is node-local and invisible to compute nodes).
- **Cluster scripts:** `slurm/launch.sh` (coordinator sbatch job; tar_make inside the container; Slurm client commands proxied by `slurm/shims/{sbatch,squeue,scancel,sacct}` — ssh back to the login node, since no Slurm client exists inside the container), `slurm/smoke.sh` (quicktest + `auth_glmnet_40`/explore), `slurm/setup_raapoi.sh` (idempotent one-time setup incl. intra-cluster ssh key + image pull), `slurm/pull_image.sh`. `.gitattributes` forces LF on all shell files.
- **Windows scripts:** `slurm/deploy.ps1` (clean/pushed checks, SSH probe with first-time key guidance, detached checkout of HEAD sha on cluster, sbatch submit; `-Smoke`, `-Walltime`, `-HostName` params) and `slurm/fetch_results.ps1` (scp newest bundle, extract into local `_targets/`).
- **Results bundle:** `R/export_functions.R` — `headline_target_names()` (all names verified against `_targets.R`) + `make_results_bundle()` tarring `_targets/meta/meta` + headline objects to `exports/`; called by launch.sh even after partial runs; unit-tested.
- **Legacy retired:** `batchtools.slurm.tmpl`, `run.sh`, `run.R`, `interactive_R_on_quicktest_partition.sh` → `attic/`; zero remaining batchtools/future references; AGENTS.md updated.
- **Docs:** `HPC.md` runbook (architecture, one-time setup, everyday workflow, troubleshooting, explicit untested list).

⚠️ **renv.lock incident:** during the work, a `renv::snapshot(packages=...)` call destroyed the working-tree lockfile (HEAD's copy was an ancient R 4.2 stub). It was rebuilt from the intact project library via `renv::snapshot(type="all")`: 224 packages, R 4.5.1, all spot-checked versions match the destroyed file (targets 1.11.4, crew 1.3.0, xgboost 3.1.2.1, hathiTools/wordVectors GitHub remotes intact) + crew.cluster 0.4.0. Independently re-verified: 21 critical packages all present. Possibly a slight superset of the original (harmless). If paranoid, Dropbox version history for `renv.lock` can recover the pre-incident file for diffing. **renv.lock must be committed** — the Docker build copies it.

Verified on Windows: `_targets.R` parses; `tar_validate()` passes on both backends; 91/91 tests pass; all shell files bash-syntax-clean with LF endings; both .ps1 parse.

### 2026-07-07 — First cluster shakedown: ✅ GREEN SMOKE RUN

`auth_glmnet_40`/explore completed end-to-end on Rāpoi: **420 targets in 12 min** across 4 std + 1 bigmem crew workers; bundle fetched and `tar_read()` locally (1.48M weight rows incl. the new `rank_agreements`). Twelve real bugs were found and fixed during the shakedown, each committed individually:

1. Singularity module needs its toolchain prerequisites → later mooted by (5).
2. Non-interactive shells have a bare MODULEPATH → mooted by (5).
3. Batch shells lack the Lmod `module` function entirely → mooted by (5).
4. `lmod.sh` is not `set -u`-safe; unbound-var errors are fatal even in `||` chains (silent zero-output death) → mooted by (5).
5. **Singularity now invoked by absolute EasyBuild path** (`DEMAGOGUES_SINGULARITY`); Lmod demoted to fallback.
6. `deploy.ps1 -Smoke` exported full-run parameters over smoke.sh's defaults (would have run full_democracy in quicktest's 5 h window).
7. **Quarto's deno/V8 needs ~66 GB of virtual address space** (V8 sandbox cages); Slurm sets `RLIMIT_AS` = `--mem` (soft AND hard, unliftable; same on Quarto 1.6). Coordinator jobs request 96G — do not "optimize" down.
8. **Windows git strips exec bits**: every cluster checkout de-executabled the shims (`Permission denied` on crew's sbatch). Modes now recorded via `git update-index --chmod=+x`.
9. **Worker container entry**: `$0` under sbatch is the spooled script on node-local `/var/lib/slurm` — invisible in-container. Replaced re-exec with an `Rscript()` shell function that shadows PATH lookup. Also `seconds_launch = 1800` (crew's cluster default waits hours before retrying a failed launch).
10. **Worker CWD**: ssh-shim submission starts jobs in `$HOME`; targets' relative store paths broke dependency retrieval. The function now `cd`s to the project root.
11. **Workers never source `_targets.R`** — its `options(hathiTools.ef.dir=)` was coordinator-only, so workers looked in `./hathi-ef`, found nothing, couldn't rsync (parallel nodes lack outbound network), and "completed" files targets with zero paths, poisoning downstream. `cache_ef_files()` now derives the dir from `RESEARCH_DATA_ROOT` itself, tolerates rsync failure (pre-staged cache hits need no network), and errors loudly on empty results. Worker scripts bake in `RESEARCH_DATA_ROOT`.
12. `ssh` without `-n` blocks on stdin when deploy/fetch scripts run non-interactively.

Robustness added along the way: `restricted_dfm_from_json()` returns NULL for empty period slices, and the branch-heavy model chain (files/dfm/split/model/svd/weights/topic/kl/entropy/graph) uses `error = "null"` so a failed branch is recorded in `tar_meta()` (shipped home in the bundle via `meta`) instead of aborting a multi-day run.

Still to observe on the first **full** run: memory/walltime adequacy at 500 vols/decade (bigmem DFM builds — *validated by the P2 tuning runs 2026-07-07*), worker `free(): invalid pointer` at teardown (cosmetic so far — results unaffected), and quarto report renders on the coordinator (the smoke run's graph reports rendered? verify on full run).

EF-cache attrition (observed on the tuning runs 2026-07-07; **root-caused the same day — the paragraph below supersedes shakedown item 11's "parallel nodes lack outbound network" claim, which was WRONG**). Diagnostic (delegated, verified on the cluster): parallel compute nodes have full outbound network (test job on spj01: DNS, HTTPS, and the HTRC rsync module all reachable). The 23 missing volumes (999/1000, 978/1000, 1000/1000 coverage) were all fetchable; none dropped out of HathiTrust. Two real bugs:
1. **hathiTools 0.2.0 pairtree-encoding bug**: `stubby_url_to_rsync()` uses `id_clean()` (no `.`→`,`) instead of `id_encode()`, so every htid with dots in its local id (miun/miua Michigan ids) 404s against the comma-encoded HTRC tree. All 23 missing volumes fit this fingerprint. *Worth reporting upstream.*
2. **No `rsync` binary in the container** — the in-pipeline top-up never ran anywhere; `rsync_from_hathi()` returns exit 127 without raising, and the old `cache_ef_files()` ignored the return value.

Fixes (2026-07-07): `rsync` added to docker/Dockerfile (rebuild via Actions; **run `slurm/pull_image.sh` on the cluster before the full run**); `cache_ef_files()` now checks the rsync exit status, warns on partial coverage with counts, and falls back to `rsync_ef_comma_encoded()` — a corrected-path fetcher (`ef_remote_path()`, parity-tested against hathiTools for undotted ids, live-tree-verified for dotted ones) that stores fetched files at the dot-encoded local path `find_cached_htids()` expects. The 23 tuning-run volumes were also rsynced into the cluster cache directly, so re-runs get 1000/1000. No pre-stage step needed.

---

### 2026-07-07 — Phase 3 P1 complete (delegated, reviewed and verified)

Config-driven hyperparameters, per the P1 spec (decision: Phase 3 P1–P4 run **before** the first full democracy run so the expensive run happens once with final settings).

- **Config:** `predictive_models.engines` now accepts scalar form (`[glmnet, …]`, historical defaults) and mapping form (`- name: xgboost` / `params: {max_depth: 6, …}`), mixable, in both the legacy (`pipeline.yml`) and split (`corpus.yml` + runs) config paths; profile overrides still replace the engine list wholesale. `normalize_engines()` canonicalizes both forms to `name`+`params` immediately after the merge.
- **Grid/ids:** `params` is a list-column paired with the engine name; an 8-char `xxhash32` of the name-sorted params enters `predictive_model_id` **only when params are non-empty** — verified `tar_manifest()` is byte-identical to HEAD under the default config (383 targets), and that a `max_depth` override invalidates only xgboost targets + their downstream closure. Duplicate engine entries with different params are allowed (this is what the P2 tuning run will use).
- **Engines:** each method merges user params over the exact former hard-coded defaults via `modifyList()`. `lambda_rule` (`lambda.min`/`lambda.1se`) is a glmnet pseudo-param stored as `attr(model, "lambda_rule")` at fit time and read back by `model_weights.cv.glmnet()` and both glmnet performance methods (three formerly hard-coded `s = "lambda.min"` sites) — no target-signature changes. xgboost `early_stopping_rounds` is accepted-but-warned (no held-out set at fit time; the tuning study uses `xgb.cv` instead).
- **Pre-existing bug found and fixed (important): under xgboost 3.x the high-level `xgboost(data=, label=, params=)` interface silently drops the `params` list.** Every xgboost fit since the 3.x upgrade ignored ALL hyperparameters (including the old hard-coded `max_depth=12, eta=0.1, …`) and used xgboost's built-in defaults (`max_depth=6, eta=0.3`, no subsampling, no `scale_pos_weight`). Never caught because the explore profile only runs glmnet+naivebayes. Fits migrated to `xgb.train()`/`xgb.DMatrix()`, which honors params — so historical xgboost results reflect xgboost defaults, not the documented settings, and will legitimately change on the next run. (Objective now also travels as a fit-time attribute since `xgb.train` doesn't populate `$params`.)
- Tests: `tests/testthat/test-model-params.R` adds 20 assertions (parse equivalence, wholesale override, hash semantics/order-invariance, default-vs-override fits for xgboost/glmnet, `lambda_rule` respected by weights). Suite: **111 pass / 0 fail.**

Next in Phase 3: P2 tuning run config (3 decades, xgboost depth×eta grid, glmnet α grid) on Rāpoi → tuned defaults into config; then P3 `n_repeats` and P4 `sampled_htids`.

### 2026-07-07 — Phase 3 P2 tuning study configured (runs submitted to Rāpoi)

- **Three run configs** `config/runs/tuning_democracy_{1790,1860,1940}.yml` (periods are a single contiguous range in config, so non-contiguous decades = separate runs; they must run **sequentially** — the combined tables are static target names in the shared store, and each run's bundle is separately timestamped in `exports/`). Each: democracy, one decade, the full run's model-bearing DFM config (500 restricted vols, feature pages, lowercased), weights `[ppmi]`, engines = scalar baselines (glmnet α=1@lambda.min; xgboost depth=12/eta=0.1) + glmnet {α=0, α=0.5, α=1@lambda.1se} + xgboost depth {4,6,8} × eta {0.05,0.1,0.3} at nrounds=120 (no in-pipeline early stopping; if a winner lands on a grid edge, follow up with xgb.cv). 152 targets/run, 14 model variants/decade. These runs double as the first 500-vol bigmem DFM test.
- **P1 gap found by the tuning grid and fixed:** `weight_type` (which seeds kl/entropy target names and labels the combined tables) was `paste(engine, sample_max_vols, to_lower)` — duplicate engines with different params collided (“duplicated target names: kl_…”). Now an engine *label* carries the params hash when non-empty (`xgboost_<hash8>`); empty-hash configs keep bare engine names, so the default manifest is still byte-identical (re-verified, 383 targets) and the suite still passes 111/0.
- Analysis once bundles are back: per-variant test AUC/kappa from `combined_performance`, weight sparsity (share of exactly-zero / near-zero weights) from `all_model_weights` keyed by the hashed `weight_type` label → pick defaults, write them into `config/corpus.yml` as mapping-form engines, record the table for the paper appendix (makes the “recommended defaults” claim true).

### 2026-07-07 — Corpus stats per-feature; rendered docs in bundle; methods appendix drafted (delegated + reviewed)

- **Corpus stats are now feature-configurable** (Xavier's request): the six hard-coded democracy targets in `corpus_stats_targets` are replaced by `tar_eval` blocks over `corpus_stats_df` (one row per run feature), emitting `<feature>_word_counts` / `_words_per_million` / `_text_counts` / `_text_percent` with bookworm terms from `bookworm_terms` → `search_terms` → feature-name fallback, and `corpus_stats.lims` config (default 1700–2020). Translations are per-feature-optional (`translations_file:` in the feature config → `<feature>_translations` / `<feature>_trans`); `config/features/democracy.yml` declares the existing xlsx. `headline_target_names()` derives the per-feature stats names from its `features` arg. **Verified: `full_democracy` manifest names AND commands byte-identical** (agent: 0 command diffs; re-verified names independently), non-democracy feature produces `<feature>_*` names with multi-term queries; tests 123 pass / 0 fail (12 new for the bundle helpers).
- **Results bundle now ships rendered documents home**: `make_results_bundle()` includes `Paper/*.md|html|pdf` + `*_files/` companion dirs (root-relative, silently omitted when absent) — so a `document`-output run returns the cluster-rendered paper in the same tarball. Previously the render stayed on the cluster.
- **`Paper/Appendix_Methods.qmd` drafted** ("Appendix B: Methodological Choices") and registered as `tar_quarto` target `Appendix_Methods` (manifest 383→384, only that name added). Documents: config-driven hyperparameters + defaults table + **the xgboost 3.x params-dropping disclosure**, tuning-study design, weight normalization, keep-duplicates decision, union-vocab KL, train-statistics eval weighting, planned n_repeats (P3), reproducibility (P4 hooks). Tuning figures read committed snapshots `data/tuning_study_{performance,sparsity}.csv` (self-activating `eval=has_tuning` chunks) because the store's combined tables are overwritten by the most recent run; maintenance instructions are in the document's final section.
- Ops note recorded: pushes to master are safe while cluster jobs run (the scratch clone is pinned to a detached SHA at deploy time); only `deploy.ps1` moves the cluster checkout — don't deploy until the tuning chain finishes.

### 2026-07-07 — P2 complete: tuning study ran green; defaults adopted

All three tuning runs completed on Rāpoi with **zero errored targets** (~20 min each; the 500-vol restricted DFM builds ran fine on bigmem — retiring that open risk for the full run). Bundles fetched by scp, each extracted to an isolated store, per-variant results snapshotted to `data/tuning_study_performance.csv` / `data/tuning_study_sparsity.csv` (committed; the appendix figures now activate).

**Results (mean held-out, across 6 decades: 1790s/1800s, 1860s/70s, 1940s/50s):**
- glmnet: α=0.5 ≈ α=1 default (AUC .816 vs .814, within noise); λ.1se trades a little kappa for sparsity (.990 vs .979 zero-share); ridge α=0 clearly worse (AUC .748, dense weights). **Kept default α=1 @ λ.min.**
- xgboost: eta=0.3 uniformly worst. Top group spans AUC .812–.820. Old default (12, 0.1) ties best AUC but worst calibration of the group (log loss 1.53). **Adopted depth=6, eta=0.05** (AUC .819, kappa .490, log loss 1.16) per Xavier — near-best discrimination, much better calibrated, conventional depth for sparse text. No adopted setting on a worrying grid edge (depth flattens 8→12; eta .05 undertraining at 120 rounds did not materialize) → no xgb.cv follow-up needed.

Written into `config/corpus.yml` (mapping-form xgboost entry) + adopted-defaults paragraph and updated defaults table in `Paper/Appendix_Methods.qmd` in the same commit. Verified: exactly the 73-target xgboost closure changes names in the full_democracy manifest (checked changed names ⊆ xgboost-chain object names from the grid; 0 outside); tests 123/0.

**Full-run impact:** xgboost model targets and their downstream closure will (correctly) rebuild with the new params; glmnet/naivebayes/LiblineaR chains keep their names.

### 2026-07-07 — P3 + P4 complete (delegated, reviewed and verified); EF-cache root cause fixed

**P3 (sampling repeats):** `samples.n_repeats` (default 1) + `samples.repeats_scope` (`headline` = restricted chain at max volume cap only, `all`); `full_democracy.yml` sets `n_repeats: 5`. Design: `sample_rep` column with `sample_rep_label` empty for rep 1 (byte-identical rep-1 target names — same convention as dedupe/common-vocab flags); no explicit seed plumbing (targets derives per-target RNG seeds from the rep-labelled names); OOD evaluation pairs every rep's models with the rep-1 unrestricted testing DFM; `sample_rep_label` added to kl/entropy name columns (the semantic-label collision class found twice now — tuning grid, then reps). Verified independently: n=1 manifest = prior + exactly `sampled_htids`; n=5 manifest 853 targets (385 + 468 rep-2..5 closure, 0 drops, 0 duplicates); rep flows into all info tables for grouping. Tests 156 pass / 0 fail (25 new for repeats, 8 for EF paths).
**P4:** static `sampled_htids` target (htid × period × sample_id × rep × type × cap × feature) in the ingest group; included in `headline_target_names()` so every bundle carries the exact sample. Appendix B's "Sampling uncertainty" and "Reproducibility" sections updated to describe what shipped (replicate-range framing, not CIs; pooled-noise rationale; OOD rep-1 pairing).
**Full-run cost note:** manifest grows ×2.22 (853 targets); the extra load is 4×31 bigmem DFM builds + the downstream model chain — estimated +3–4 h wall with 4 bigmem workers.
**Prerequisite before deploying the full run:** fresh container image on the cluster (`slurm/pull_image.sh`) — the EF-cache fix (rsync binary + encoding workaround, see the corrected attrition note above) landed in the image today.

---

## Phase 1 — Correctness fixes (do these before re-running anything expensive)

> **Status: ✅ done 2026-07-06** except the paper-text items (M3.1, M4 caveats) — see Progress log above for what was actually implemented, including two additional bugs (B7, B8) discovered and fixed during review.

### M1/B1. Fix the weight normalization formula ⚠️ HIGHEST PRIORITY — ✅ DONE

**Where:** `R/model_weights.R` lines 45, 69, 89, 114; `_targets.R` lines ~371–374 (`weight_ppmi_object`) and ~446–449 (`weight_svd_object`).

All six sites use:

```r
normalized_value = value - min(value) + abs(min(value)),
normalized_value = normalized_value / sum(normalized_value)
```

When `min(value) < 0` (glmnet, LiblineaR, naive Bayes diff, SVD similarities — i.e., most models), `value - min + |min| = value + 2|min|`, so the *floor* of the distribution is `|min|`, not 0. Consequences:

- The distribution is flattened by an arbitrary, model- and period-specific additive constant.
- Entropy is inflated by an amount that depends on `|min(value)|`, which varies by decade and engine — so the entropy *trends* in the paper partly track the magnitude of the most negative coefficient, not conceptual diversity.
- KL/JSD between periods and engines are similarly distorted.
- For xgboost (all-positive gain), the formula is a no-op, so cross-engine comparisons mix two effectively different normalizations.

**Fix:** Introduce one shared helper in `R/model_weights.R`:

```r
#' Normalize signed model weights into a probability-like vector.
#' method = "shift"    : (value - min(value)) / sum(...)      — floor at 0
#' method = "positive" : pmax(value, 0) / sum(pmax(value, 0)) — only positive evidence
#' method = "softmax"  : exp(value / temperature) / sum(...)  — order-preserving, dense
normalize_weights <- function(value, method = c("positive", "shift", "softmax"), temperature = 1) { ... }
```

Replace all six inline computations with calls to this helper. Which method is *right* is a substantive choice (see M3); make it a config key (`weights.normalization: positive` in `config/corpus.yml`, plumbed through `object_parameters.R`) with `"positive"` as default — for the "distribution over topics associated with democracy" interpretation in the paper, only positive association weights make sense as probability mass; negative glmnet coefficients are evidence *against* democracy and should not receive positive probability.

**Acceptance:** all six sites use the helper; a testthat unit test verifies (a) output sums to 1, (b) `method="shift"` maps min to 0, (c) `method="positive"` zeroes negative weights; `tar_outdated()` shows only weight/entropy/KL/graph targets invalidated.

### M2. Use ONE normalization for all information-theoretic measures

**Where:** `R/entropy_functions.R` (`entropy()` line 160, `jsd_simple()` line 196–200, `kl_simple()` line 82–93), `R/correlation_functions.R` (`jsd_weights_by_period()`).

Currently `entropy()` and `jsd_weights_by_period()` consume the (buggy) `normalized_value` column, while `kl_simple()` and `jsd_simple()` ignore it and re-normalize with `value - min(value)` per group. So the paper's entropy, KL, and JSD panels are not computed on the same distributions.

**Fix:** after M1, make every information-theoretic function consume `normalized_value` (produced by the single helper) and delete the local re-normalizations in `kl_simple()`/`jsd_simple()`. Note in roxygen that inputs must already be normalized.

**Acceptance:** `grep -n "value - min(value)" R/*.R` returns nothing outside `normalize_weights()`.

### M3. Decide and document what "weights as a probability distribution" means (paper + code)

The cross-engine comparability problem is deeper than the formula: glmnet-lasso weights are sparse (most exactly 0), NB diffs are dense, xgboost gain is sparse-positive and biased toward frequently-split features. Entropy differences between engines therefore partly measure *sparsity of the regularizer*, not interpretation diversity. Concrete mitigations to implement and report:

1. Within-engine, across-time comparisons only for entropy (never compare entropy levels across engines; only shapes/trends). Add this caveat in the paper.
2. For cross-engine agreement, prefer rank-based measures on the top-k features (Spearman ρ or rank-biased overlap on top 100) alongside the existing Pearson correlations of normalized weights. Add a `rank_agreement_by_period()` function to `R/correlation_functions.R` mirroring `correlate_weights_by_period()`.
3. When casting to DFMs for JSD (`cast_dfm`), absent words and zero-weight words are currently conflated; restrict to the union of vocabularies with explicit zeros, or to the intersection (there is already `vocab_intersection()` — use it with `common_vocab_only = TRUE` as default in `kl_simple`).

**Acceptance:** paper text (Modeling section) states the normalization method and its rationale; JSD/KL computed on common vocabulary; a rank-agreement figure exists in the appendix.

### B2. LiblineaR class weights are inverted

**Where:** `R/modeling_functions.R` line 349–352.

```r
wi <- table(y_train)
svm_model <- LiblineaR::LiblineaR(..., wi = wi)
```

`wi` sets the per-class misclassification cost. Passing raw class *counts* gives the **majority** class the larger cost — the opposite of the intended imbalance correction. Fix:

```r
tab <- table(y_train)
wi <- setNames(as.numeric(sum(tab) / (length(tab) * tab)), names(tab))
```

**Acceptance:** unit test on a synthetic 90/10 imbalanced dfm shows the minority class weight > majority class weight; LiblineaR performance targets invalidated.

### B3. `most_similar_downsample()` dead code / fragility

**Where:** `R/modeling_functions.R` lines 144–149. `dplyr::arrange(-2)` sorts by the constant −2 (a no-op — presumably meant `arrange(desc(2))` on the second column, itself redundant given the following `slice_max`), and `slice_max(feat1, ...)` hard-codes a column name that only exists when the dictionary key happens to produce `feat1`. Since `split_downsample = FALSE` everywhere in `object_parameters.R` (line ~607), this path is currently unreachable — either delete the similarity-downsample path or fix it (`slice_max(.data[[colnames(sims)[2]]], ...)`) and add a test. Deleting is fine; the class balancing actually used is the page-level `multiplier` in `restricted_json_to_dfm()`.

### B4. Train/test weighting inconsistency (leakage-adjacent)

**Where:** `R/performance_functions.R` — every `model_performance.*` method calls `get_x(testing_dfm, weight = ...)`, which computes PPMI or TF-IDF **from the test set's own document frequencies**. The transformation applied at test time therefore differs from the one the model was trained on. This mostly attenuates measured performance rather than inflating it, but it makes the weighting-scheme comparison (Table `tbl-performanceByWeightingScheme`) unclean.

**Fix (pragmatic):** compute IDF/PPMI statistics on the training partition and apply them to the test partition. Implement `dfm_tfidf_apply(test_dfm, train_dfm)` and equivalent for `dfm_ppmi` in `R/dfm_functions.R`; thread `train_dfm` through `get_x()` via an optional `reference_dfm` argument. Also note in the paper that vocabulary trimming (top 30k) is computed on the full decade slice before splitting (minor, but should be stated).

**Acceptance:** performance methods pass `reference_dfm = training_dfm`; appendix sentence documents both choices.

### B5. Duplicate volumes / multiple editions inflate associations

**Where:** `_targets.R` `workset_meta_object` (lines 114–148) filters but never deduplicates: the corpus-stats targets themselves show multiple HTIDs per `ht_bib_key` and per author-title. Popular works (multiple editions/copies) get sampled more often, which the paper's own discussion of Pechenick et al. flags as a bias.

**Fix:** add `group_by(ht_bib_key) %>% slice_sample(n = 1)` (or dedupe on normalized author+title) before the period sampling in `sample_object`, controlled by a config flag `samples.dedupe: bib_key` so the old behavior remains reproducible. Report both in the appendix if the results shift.

### B6. Assorted small code fixes

| ID | Where | Problem | Fix |
|----|-------|---------|-----|
| B6a | `R/modeling_functions.R:18` + `_targets.R:248` | Generic formal is `downsampled`; call site passes `downsample =` (works only via partial matching) | Rename call-site arg to `downsampled` |
| B6b | `R/modeling_functions.R:72` | `strata = names(feat)` is `NULL` for character features → unstratified split | `strata = feat` (the column name) |
| B6c | `R/entropy_functions.R:16` (`kl_sparse`) | O(n²·nnz) triple-scan loops; unused by pipeline | Delete (kl_simple is the used path) or replace with vectorized version |
| B6d | Paper `.qmd:93` | Hard-coded hashed target names `workset_4ef79d…`, `workset_meta_22b87…` break on any config change | Create stable named summary targets (e.g. `n_democracy_workset_volumes`, `n_democracy_english_volumes`) in `_targets.R` and reference those |
| B6e | Paper `.qmd:562–568` | `sentiments_afinn` block duplicated verbatim | Delete one |
| B6f | `run.R` | `tar_prune()` unconditionally destroys stale targets before every run; `tar_make_future()` is deprecated in targets ≥ 1.4 | Replaced entirely in Phase 2 (H4) |
| B6g | `_targets.R:138–144` | `rights_date_used2` filter silently drops volumes whose `imprint` lacks a 4-digit year (`NA` fails the `<=` filter) | Make explicit: `is.na(rights_date_used2) \| rights_date_used2 <= rights_date_used` — or document the drop as intended metadata hygiene |
| B6h | root | `file964c1e2a1b85.txt`, `graph_document.html`, `_targets_old.R`, `_targets.R.bak_modular`, `object_parameters.R.bak_modular` clutter | Delete or move to `attic/` (git-tracked ones: `git rm`) |

### M4. Methods caveats to add (no code, paper text)

- **NB + PPMI weights:** multinomial NB assumes count data; fitting it on PPMI-weighted pseudo-counts is a known hack that works but should be stated.
- **xgboost gain importance** is biased toward high-frequency features; consider reporting permutation importance for one decade as a robustness check (cheap: one extra target).
- **Sentiment lexicons (bing/afinn/nrc/loughran) are anachronistic** for 1700–1900 text. Either caveat prominently, restrict the sentiment figure to post-1900, or validate against a historical lexicon for a subsample.
- **OCR quality pre-1820** (long-s → f confusion, etc.) differentially degrades the earliest decades, exactly where the paper claims the sharpest valence findings. At minimum quantify: share of tokens failing `include_pattern` by decade (one small target + appendix figure).
- **Topic models:** `stm::stm(K = 20)` on DFMs reduced to ~50 terms/period is unlikely to be meaningful, and topic-model outputs don't currently appear in the paper. Either drop `topic_models` from the default run's `outputs` (saves cluster time) or make K and vocab honest (K ≈ 10–60 via `stm::searchK` on the full reduced vocabulary).

---

## Phase 2 — HPC: make Rāpoi a first-class, one-command backend

> **Status: ✅ implemented 2026-07-06** (see Progress log). Implementation differs from the sketch below in one deliberate way: everything runs in a **Singularity container** (no R modules at all, per Xavier), which required ssh-proxy shims for Slurm commands and a worker re-exec pattern. See HPC.md for the authoritative runbook; cluster-side steps remain untested until the first real run.

**Current state (all confirmed):** `_targets.R` sets `crew::crew_controller_local` (line 37), so every `resources = tar_resources(future = tar_resources_future(plan = future.batchtools::batchtools_slurm, ...))` block (lines 175–181, 210–216, 228–235, 287–294, 350–356, 378–384, 421–427) is **silently ignored** — those legacy blocks reference `cluster_resources.R` and `batchtools.slurm.tmpl`, which load `R/4.0.2` while `renv.lock` pins **R 4.5.1**, targets 1.11.4, crew 1.3.0. `run.R` calls deprecated `tar_make_future(workers = 100)`. In short: the pipeline currently only actually runs locally.

**Rāpoi facts (from the docs, July 2026):** Slurm; partitions `quicktest` (≤64 CPU, 128 GB, 5 h), `parallel` (≤256 CPU, 512 GB, 10 days), `bigmem` (≤128 CPU, 1 TB, 10 days), `longrun` (30 days), `gpu` (A100s, 24 h). Defaults if unspecified: quicktest, 2 CPU, 2 GB, 1 h.

### H1. Replace the future/batchtools plumbing with `crew.cluster` controllers

Add `crew.cluster` to renv. Rewrite the controller setup in `_targets.R`:

```r
controller <- if (Sys.getenv("TARGETS_BACKEND", "local") == "slurm") {
  crew_group <- crew::crew_controller_group(
    crew.cluster::crew_controller_slurm(
      name = "std", workers = 25, seconds_idle = 300,
      options_cluster = crew.cluster::crew_options_slurm(
        partition = "parallel", memory_gigabytes_per_cpu = 6, cpus_per_task = 2,
        time_minutes = 600, script_lines = slurm_script_lines)),
    crew.cluster::crew_controller_slurm(
      name = "bigmem", workers = 4, seconds_idle = 300,
      options_cluster = crew.cluster::crew_options_slurm(
        partition = "bigmem", memory_gigabytes_per_cpu = 40, cpus_per_task = 2,
        time_minutes = 600, script_lines = slurm_script_lines)),
    crew.cluster::crew_controller_slurm(
      name = "cpu10", workers = 10, seconds_idle = 300,
      options_cluster = crew.cluster::crew_options_slurm(
        partition = "parallel", memory_gigabytes_per_cpu = 1, cpus_per_task = 10,
        time_minutes = 540, script_lines = slurm_script_lines))
  )
} else {
  crew::crew_controller_local(workers = target_workers, seconds_idle = 120, crashes_max = 10)
}
tar_option_set(controller = controller, resources = tar_resources(
  crew = tar_resources_crew(controller = "std")))
```

Then per heavy target, replace each `resources = tar_resources(future = ...)` block with `resources = tar_resources(crew = tar_resources_crew(controller = "bigmem"))` (DFM builds) or `"cpu10"` (model fits, SVD). Targets with `deployment = "main"` are untouched. **Delete** `batchtools.slurm.tmpl`, `cluster_resources.R` sourcing, and the `run.R` future path. On Windows, `TARGETS_BACKEND` is unset → local controller, so nothing changes for local dev.

**Acceptance:** `Rscript -e 'targets::tar_validate()'` passes on Windows (local) and on Rāpoi with `TARGETS_BACKEND=slurm`; a smoke run (`TARGET_RUN=auth_glmnet_40 TARGET_PROFILE=explore`) completes on the cluster with workers visible in `squeue`.

### H2. Rewrite `cluster_resources.R` as a partition/controller map

Keep the file, but have it define (a) `slurm_script_lines` — the module loads / renv activation lines injected into worker scripts, and (b) the controller definitions used in H1, so all Slurm-tuning lives in one file. Suggested contents of `slurm_script_lines`:

```r
slurm_script_lines <- c(
  "module purge",
  paste("module load", Sys.getenv("RAAPOI_R_MODULE", "R")),  # confirm exact name with `module avail R`
  "export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1"
)
```

### H3. Reproducible R environment on the cluster

The renv lockfile is R 4.5.1; the cluster module inventory must be checked (`module avail R` / `module spider R`). Two options, in preference order:

1. **Apptainer container** (Rāpoi supports containers): build once from `rocker/r-ver:4.5.1`, `renv::restore()` inside, store the `.sif` in `/nfs/scratch/marquexa/containers/`. Worker `script_lines` become `apptainer exec demagogues.sif Rscript ...` (crew.cluster supports a custom `r_command`... use `crew_options_slurm(script_lines=...)` + `r_arguments`, or wrap via a launcher script). This decouples you from cluster module churn permanently.
2. **Native module + renv restore** on scratch if a close-enough R module (≥ 4.4) exists. Faster to set up; brittle across cluster upgrades.

Document the chosen path in a new `HPC.md`. Also: the repo currently commits `renv/library/**` artifacts for R-4.2/Windows (visible in globs); ensure `.gitignore` has `renv/library/` and `git rm -r --cached renv/library` if tracked.

### H4. One-command launch + sync workflow

Create `slurm/` with:

- `slurm/launch.sh` — sbatch script for the *coordinator* job (small: 4 CPU/8 GB/`parallel` or `longrun`), which runs `Rscript -e 'targets::tar_make()'` with `TARGETS_BACKEND=slurm`, `TARGET_RUN`, `TARGET_PROFILE`, `RESEARCH_DATA_ROOT=/nfs/scratch/marquexa/corpora` exported. crew submits/reaps worker jobs itself — no 2-day monolithic job, no `--mem=60G` coordinator.
- `slurm/smoke.sh` — same but `quicktest` partition + `TARGET_PROFILE=explore` + small run, for validating changes in <1 h.
- `slurm/sync.md` (or a `justfile`): the store lives on scratch (`tar_config_set(store = "/nfs/scratch/marquexa/Demagogues/_targets")` via `TAR_CONFIG`/`custom.yaml` cluster profile); after a run, pull only the small result objects back with `rsync`/`scp` or `rclone` to Dropbox (replaces the hand-listed `dbxcli put` block in the old `run.sh`, which will drift out of date — script it from `tar_manifest()` names tagged `deployment == "main"` summary targets).
- Delete old `run.sh`, `run.R`, `interactive_R_on_quicktest_partition.sh` (superseded; note quicktest interactive: `srun --pty --partition=quicktest ... R`).

**Acceptance:** `sbatch slurm/smoke.sh` → completed pipeline + results synced, documented in `HPC.md` with expected wall time.

### H5. Data staging

`RESEARCH_DATA_ROOT` already parameterizes the Hathi catalog and EF cache (`_targets.R:20–29`) — good design, keep it. Add to `HPC.md`: one-time `rsync` of `raw-hathifiles/` and incremental sync of `hathi-ef/` cache to scratch; note that `cache_ef_files()` downloads what's missing, so the cluster can also populate its own cache (network from compute nodes permitting — verify; if blocked, download on the login node first via a `tar_make(files_...)` pass with `deployment = "main"`).

---

## Phase 3 — Model parameterization & statistical robustness

> **Phase 3 complete 2026-07-07.** P1 ✅ (config-driven hyperparameters; incl. the xgboost-3.x params-dropping bug). P2 ✅ (tuning study on Rāpoi; xgboost depth=6/eta=0.05 adopted per Xavier, glmnet unchanged). P3 ✅ (`samples.n_repeats: 5`, headline scope, rep-1 names byte-identical). P4 ✅ (`sampled_htids` export in the results bundle). See Progress log.

### P1. Move all hyperparameters from code to config

Currently hard-coded in `R/modeling_functions.R`: xgboost `eta=0.1, max_depth=12, nrounds=120, colsample_bytree=0.5, lambda=0.5, subsample=0.5`; glmnet defaults (`alpha=1`, `nfolds=10`, `s="lambda.min"`); LiblineaR `type=1`, cost heuristic; NB `laplace=1`. The parameter grid already carries a `predictive_model_params` column — but it is fixed at `NA` (`object_parameters.R:620`) and never passed.

**Fix:**
1. In `config/corpus.yml` (and overridable per run), allow the mapping form:

```yaml
predictive_models:
  engines:
    - name: glmnet
      params: { alpha: 0.5, nfolds: 10, lambda_rule: lambda.1se }
    - name: xgboost
      params: { nrounds: 200, eta: 0.1, max_depth: 6, early_stopping_rounds: 20 }
    - name: naivebayes
      params: { laplace: 1 }
    - name: LiblineaR
      params: { type: 1 }
```

2. In `object_parameters.R`, parse both scalar (`engines: [glmnet]`) and mapping forms; store `params` as a list-column; include a short hash of params in `predictive_model_id` so param changes invalidate the right targets.
3. In `_targets.R`, pass `!!!predictive_model_params` (or `params = predictive_model_params`) into `predictive_model()`; in `R/modeling_functions.R`, have each engine merge user params over defaults (the xgboost method already half-does this — generalize).
4. `lambda_rule` ("lambda.min" vs "lambda.1se") must also thread into `model_weights.cv.glmnet()` and both glmnet performance methods (currently hard-coded `s = "lambda.min"` in three places).

**Acceptance:** changing `max_depth` in the run YAML invalidates only xgboost model targets; `tar_manifest()` unchanged for other engines.

### P2. Principled defaults (one-off tuning study)

`max_depth = 12` is very deep for sparse 30k-dim text (typical: 4–8); `subsample/colsample = 0.5` with only 120 rounds and no early stopping is arbitrary; lasso (`alpha=1`) vs ridge materially changes weight sparsity and therefore all downstream entropy measures (see M3). Add a **tuning run config** (`config/runs/tuning_democracy.yml`) that, on 3 representative decades (e.g., 1790, 1860, 1940), grids: xgboost `max_depth {4,6,8} × eta {0.05,0.1,0.3}` with early stopping; glmnet `alpha {0, 0.5, 1}`; and records test AUC/kappa + weight-sparsity. Pick defaults from this, write them into `config/corpus.yml`, and report the table in the appendix ("recommended defaults" claim in the paper then becomes true and documented).

### P3. Uncertainty quantification

Everything downstream (weights, entropy, KL) is currently a **single realization**: one volume sample per decade, one train/test split, one model fit. Add:

1. `samples.n_repeats: 5` in config → replicate the `sample_object` → dfm → model → weights chain with different `tar_seed`-derived seeds (implement as an extra `rep` column in the parameter grids; expensive targets multiply ×5, so gate behind the config and run on Rāpoi).
2. Report mean ± range bands on the entropy/KL/JSD time-series figures, and on the valence figures.
3. This directly addresses the most likely reviewer objection ("are these decade-to-decade wiggles just sampling noise?").

**Acceptance:** entropy figure shows uncertainty bands; the 1810s and 1900s–1920s "conceptual innovation" claims survive (or the paper's claims are adjusted).

### P4. Reproducibility of the sample

`sample_n()` inside `sample_object` depends on targets' per-target RNG seeding — fine, but record `tar_option_get("seed")` and sampled HTIDs: add a lightweight `sampled_htids` target (tibble of htid × period × sample_id) exported to `data/` so the exact corpus sample can be published as supplementary material (reviewers/replicators cannot re-derive it otherwise).

---

## Phase 4 — Paper completion & repo hygiene

### W1. Finish/repair the draft (`Paper/The_Vector_Space_of_Democracy.qmd`)

Specific defects, by line (current file):

| Line | Problem |
|------|---------|
| 385 | Sentence truncates: "…the similarities between the PPMI-weighted matrix and" |
| 620 | Sentence truncates: "Despite the differences in feature" |
| 622–628 | "Quantifying Conceptual Relatedness" is a stub (dangling "…calculate the positive pointwise mutual information"), and duplicates content from "Models as Readers" — merge into the Modeling section or complete |
| 630–648 | "Word Vector Embeddings" describes **GLoVe models and a 5,000-books/decade sample** — the current pipeline uses 40/500 volumes per decade and SVD only (no GLoVe targets exist). Either re-add a GLoVe target (there's a leftover `glove_word_vectors_resources` in `cluster_resources.R`) or rewrite the section to match what is actually computed |
| 640, 652 | Legacy Rmarkdown refs `\@ref(fig:freqDemocracyInDemocracyBooks)`, `\@ref(fig:similarityToDemocracyGeneral)` — figures don't exist in this document; convert to Quarto `@fig-` syntax and create the figures or cut |
| 650–653 | "The language of democracy" — one paragraph, no figures |
| 655 | "Conceptual Innovation" — empty, though `combined_kl` / `combined_entropy` / `jsd_distances` targets exist and are the paper's advertised payoff. Wire them in: novelty/transience/resonance figure from `kl_matrix_to_df()`, entropy-over-time figure, plus the Barron et al. resonance framing already cited |
| 93 | Hard-coded hashed target names (see B6d) |
| 30 | Commented-out `tar_config_set` — decide store strategy once (AGENTS.md says use `custom.yaml`) |
| abstract/intro | Promise "measures of novelty and influence of particular books" — either add the per-volume analysis (a `performance_per_volume` grid exists in `object_parameters.R` but has **no corresponding target** in `_targets.R` — wire it or cut the claim) |

### W2. Reconcile paper claims with config

- "minimal adjustments to the regularization parameter α and tree depth" (line 270) vs. code: no α adjustment exists anywhere. After P1/P2 this sentence can be made true — update it with the tuned values.
- "restricted to the top 30,000 words" — matches config ✔, but also state trimming happens pre-split (B4).
- Sample sizes: appendix should show volumes-per-decade actually obtained (from `sampled_htids`, P4) versus the 500 cap.

### W3. Appendix (`Paper/Appendix.qmd`)

Verify every `tar_read()`/`tar_read_raw()` call against `tar_manifest()` after the Phase 1–3 target-name changes (AGENTS.md's own warning). Add: tuning table (P2), OCR-noise-by-decade figure (M4), dedupe robustness (B5), rank-agreement figure (M3).

### A1. Repo hygiene

- Add a root `README.md`: project purpose, quickstart (`TARGET_RUN=... Rscript -e 'targets::tar_make()'`), config layout (`corpus.yml` / `features/` / `runs/`), HPC pointer to `HPC.md`, store location convention.
- Add `tests/testthat/` with unit tests for: `normalize_weights()` (M1), `train_test_splits()` (B6a/b), LiblineaR `wi` (B2), `slugify()`/`add_target()` naming stability, `resolve_feature_placeholder()`. Run via `Rscript -e 'testthat::test_dir("tests/testthat")'` (this is not a package; no DESCRIPTION needed, just `library` calls in a helper).
- `.gitignore`: `renv/library/`, `Paper/*_files/`, `Paper/*.html` (rendered), `pipeline.out|err`, `*.bak*`. `git rm --cached` anything already tracked.
- Delete or attic: `_targets_old.R`, `_targets_latin.R` (if the Latin/Perseus strand is dormant), `*.bak_modular`, `file964c1e2a1b85.txt`, `graph_document.html` (generated), stale `Paper/graphs_*` HTML bundles that are regenerable.
- The gitStatus shows ~50 modified `Paper/figure/*.png` — these are render byproducts; decide once whether figures are tracked (recommended: track only the figures the .qmd actually includes; gitignore the rest).
- **Scope decision**: the JSTOR, Marxism, and Latin/Perseus sub-pipelines (~4,300 lines of R, half the codebase) serve a *different* paper (authority/authoritarianism). Consider splitting them into a separate repo/branch so this repo is only the democracy paper — big cognitive-load win, and it shrinks `tar_manifest()` for every run.

### A2. Performance niceties (optional)

- `restricted_json_to_dfm()` parses each EF JSON with `jsonlite::read_json` (fully recursive lists) — switching to `RcppSimdJson::fload` or caching per-volume page-count tibbles as parquet would cut DFM build time substantially (the dominant cluster cost after downloads).
- `error = "null"` on `performance_object` silences real failures — after the Phase 1 fixes, switch to `error = "continue"` and add a `performance_errors` audit target (mirroring `marxism_model_errors`, which already does this correctly).

---

## Suggested execution order for agents

| # | Task bundle | Depends on | Invalidates targets? | Status |
|---|-------------|-----------|----------------------|--------|
| 1 | M1+M2 (normalization) + tests | — | weights/entropy/KL/JSD/graphs | ✅ done 2026-07-06 |
| 2 | B2, B3, B4, B5(flag), B6a–h, B7, B8, M3.2 | — | models, performance | ✅ done 2026-07-06 |
| 3 | P1 (config-driven params) | — | none until values change | pending |
| 4 | H1–H5 (crew.cluster + Singularity backend, deploy automation) | 1–3 merged | none | ✅ done 2026-07-06 (cluster-side untested) |
| 5 | P2 tuning + P3 repeats (+ dedupe robustness run) | 4 (needs cluster) | full re-run on Rāpoi | pending |
| 6 | W1–W3 paper (incl. M3.1/M4 caveats), A1 hygiene | 5 (final numbers) | — | pending |

Steps 1–4 are safe to implement and validate locally with `TARGET_RUN=auth_glmnet_40 TARGET_PROFILE=explore`. Step 5 is one coordinated Rāpoi campaign. Keep the current `_targets` store until the new full run completes, then archive it.
