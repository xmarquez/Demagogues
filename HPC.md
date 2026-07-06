# HPC Runbook: Running the Pipeline on Rāpoi

How to run the full targets pipeline on VUW's Rāpoi cluster from this Windows
machine, using a Singularity container built from `renv.lock` and a
`crew.cluster` Slurm backend.

> **Status:** the local backend, `tar_validate()` on both backends, and the
> bundle/test machinery are verified on Windows. Everything that touches the
> actual cluster is **UNTESTED until the first real run** — see the checklist
> at the bottom.

## Architecture

```
WINDOWS (Dropbox project)                GITHUB                       GHCR
  edit -> commit -> git push  ───────►  Actions: container.yml  ───► ghcr.io/xmarquez/demagogues
        │                                (on renv.lock / docker/ change)      │
        │ .\slurm\deploy.ps1                                                  │ singularity pull
        ▼                                                                     ▼
RĀPOI login node (ssh, VPN off-campus)                    /nfs/scratch/marquexa/containers/demagogues.sif
  git checkout <sha> in /nfs/scratch/marquexa/Demagogues
  sbatch slurm/launch.sh
        │
        ▼
  COORDINATOR job (partition parallel, 4 CPU/16G)
    singularity exec demagogues.sif Rscript -e 'targets::tar_make()'
    TARGETS_BACKEND=slurm -> crew controller group (std / bigmem)
        │  sbatch via slurm/shims/* (ssh back to login node)
        ▼
  WORKER jobs (containers; std: parallel 2cpu×6G, bigmem: bigmem 2cpu×40G)
    each worker re-execs itself inside demagogues.sif, then runs crew_worker
        │
        ▼
  _targets/ store on scratch ──► make_results_bundle() ──► exports/results_<run>_<ts>.tar.gz
        │
        ▼
WINDOWS: .\slurm\fetch_results.ps1  (scp + tar -x into local _targets/)
```

Key design points:

- **One container everywhere.** The image (rocker/r-ver:4.5.1 + system libs +
  Quarto + full `renv::restore()` into the site library) is built by GitHub
  Actions and pulled onto scratch. Coordinator and workers all run inside it,
  so cluster R-module churn is irrelevant. `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE`
  is baked in so the project `.Rprofile` cannot re-activate renv inside the
  container. **`renv.lock` must be committed** — the Docker build copies it.
- **Worker container re-exec.** `crew.cluster` generates a worker script of
  `#SBATCH` directives + `script_lines` + `Rscript -e '<crew_worker call>'`
  (bare `Rscript`, PATH-resolved). Our `script_lines` (in
  `cluster_resources.R`) load Singularity and `exec singularity exec ... /bin/bash "$0"`
  unless already inside the container; on re-exec the `#SBATCH` lines are inert
  comments and the final `Rscript` line resolves inside the container.
- **Slurm-from-inside-the-container.** The coordinator has no Slurm client;
  `SINGULARITYENV_PREPEND_PATH` points at `slurm/shims/`, whose
  `sbatch`/`squeue`/`scancel`/`sacct` are two-line ssh proxies to the login
  node (`openssh-client` is installed in the image; `$HOME/.ssh` is visible
  because Singularity binds `$HOME`). Requires the intra-cluster key created
  by `setup_raapoi.sh`.
- **Store stays on scratch.** The cluster clone uses the default `_targets/`
  store: `custom.yaml` (which points at the Windows Dropbox path) is gitignored
  and never reaches the cluster, and `launch.sh` does not set `TAR_CONFIG`.
  Only the small headline objects + `meta` come back, as a tar.gz bundle.

## One-time setup

1. **VPN** (off-campus only): connect to the VUW VPN.
2. **SSH key to Rāpoi** — run this in **PowerShell**, not bash/WSL
   (`deploy.ps1` uses the *Windows* OpenSSH client, so the Windows key in
   `C:\Users\<you>\.ssh\` is the one that must be authorized on the cluster):
   ```powershell
   ssh-keygen -t ed25519        # if you have no key yet
   type $env:USERPROFILE\.ssh\id_ed25519.pub | ssh marquexa@raapoi.vuw.ac.nz "mkdir -p ~/.ssh && chmod 700 ~/.ssh && cat >> ~/.ssh/authorized_keys && chmod 600 ~/.ssh/authorized_keys"
   ssh -o BatchMode=yes marquexa@raapoi.vuw.ac.nz true    # should return silently
   ```
   The `chmod`s matter: sshd's `StrictModes` silently ignores an
   `authorized_keys` file with loose permissions, leaving you stuck at password
   prompts with no explanation. If you prefer WSL, pipe the **Windows** key
   (WSL has its own separate `~/.ssh`):
   `cat /mnt/c/Users/<you>/.ssh/id_ed25519.pub | ssh marquexa@raapoi.vuw.ac.nz "mkdir -p ~/.ssh && chmod 700 ~/.ssh && cat >> ~/.ssh/authorized_keys && chmod 600 ~/.ssh/authorized_keys"`
   (`deploy.ps1` prints this guidance automatically if its SSH check fails.)
3. **First container build**: push any commit touching `renv.lock`, `docker/**`
   or the workflow to `master` (or run the *Build container image* workflow
   manually from the Actions tab). **Then make the GHCR package public once**:
   github.com → your profile → *Packages* → *demagogues* → *Package settings* →
   *Change visibility* → Public. Until you do, `singularity pull` on the
   cluster fails with 403/unauthorized.
4. **Cluster-side setup** (on Rāpoi):
   ```bash
   ssh marquexa@raapoi.vuw.ac.nz
   git clone https://github.com/xmarquez/Demagogues.git /nfs/scratch/marquexa/Demagogues  # first time only
   bash /nfs/scratch/marquexa/Demagogues/slurm/setup_raapoi.sh
   ```
   Idempotent: creates scratch dirs, pulls/updates the repo, generates and
   authorizes the intra-cluster ssh key (for the shims), pulls the SIF, and
   chmods the scripts.
5. **Data staging.** If the data already lives on scratch from earlier runs
   (e.g. `/nfs/scratch/marquexa/hathi-ef` and `/nfs/scratch/marquexa/raw-hathifiles`
   from the 2023 campaign), do NOT re-transfer — symlink it into the expected
   layout instead (on Rāpoi):
   ```bash
   rmdir /nfs/scratch/marquexa/corpora/hathi/hathi-ef 2>/dev/null
   rmdir /nfs/scratch/marquexa/corpora/hathi/raw-hathifiles 2>/dev/null
   ln -sfn /nfs/scratch/marquexa/hathi-ef       /nfs/scratch/marquexa/corpora/hathi/hathi-ef
   ln -sfn /nfs/scratch/marquexa/raw-hathifiles /nfs/scratch/marquexa/corpora/hathi/raw-hathifiles
   ls -lh /nfs/scratch/marquexa/corpora/hathi/raw-hathifiles/hathi_full_20230101.txt.gz  # must exist
   ```
   (Symlinks resolve inside the container because `--bind /nfs/scratch` mounts
   the whole tree.) Otherwise rsync from Windows (Git Bash paths shown):
   ```bash
   # Hathi catalog snapshot (one-time)
   rsync -avP "/d/ResearchData/corpora/hathi/raw-hathifiles/" \
     marquexa@raapoi.vuw.ac.nz:/nfs/scratch/marquexa/corpora/hathi/raw-hathifiles/
   # EF cache (incremental; re-run to top up)
   rsync -avP "/d/ResearchData/corpora/hathi/hathi-ef/" \
     marquexa@raapoi.vuw.ac.nz:/nfs/scratch/marquexa/corpora/hathi/hathi-ef/
   ```
   `RESEARCH_DATA_ROOT=/nfs/scratch/marquexa/corpora` is set by `launch.sh`;
   the pipeline resolves the catalog and EF cache under it exactly as it does
   under `D:/ResearchData/corpora` locally. `cache_ef_files()` downloads any
   missing EF volumes itself **if compute nodes have outbound network** —
   verify on the smoke run; if blocked, pre-stage the full cache.

## Everyday workflow

```powershell
# 1. edit -> commit -> push
git push origin master

# 2. deploy + submit (from the project root)
.\slurm\deploy.ps1 -Run legitimacy_glmnet_100            # full run
.\slurm\deploy.ps1 -Run full_democracy -Walltime 3-00:00:00

# 3. monitor
ssh marquexa@raapoi.vuw.ac.nz "squeue -u marquexa"
ssh marquexa@raapoi.vuw.ac.nz "tail -f /nfs/scratch/marquexa/Demagogues/logs/coordinator-<jobid>.log"
# crew worker jobs appear as crew-worker-std-* / crew-worker-bigmem-* array jobs

# 4. fetch results when the coordinator finishes
.\slurm\fetch_results.ps1
```

`deploy.ps1` refuses to run with a dirty tree or unpushed commits, checks out
the exact local HEAD sha (detached) in the scratch clone, and submits with
`--export=ALL,TARGET_RUN=...,TARGET_PROFILE=...`.

`fetch_results.ps1` downloads the newest `exports/results_*.tar.gz` and
extracts it into the local project — **this replaces the local
`_targets/meta/meta` with the cluster's metadata** (intended: the cluster run
becomes the source of truth for the bundled targets).

## Smoke test

```powershell
.\slurm\deploy.ps1 -Smoke     # submits slurm/smoke.sh
```

Runs `TARGET_RUN=auth_glmnet_40 TARGET_PROFILE=explore` on the `quicktest`
partition (4 h cap) with a small worker fan-out (4 std / 1 bigmem). Use this
to validate the whole chain (image, shims, worker re-exec, data paths, bundle)
before any multi-day run. Expect workers in `squeue` within a couple of
minutes of the coordinator starting.

## Changing resources

All Slurm tuning lives in `cluster_resources.R`:

| Knob | Where | Default |
|---|---|---|
| Worker fan-out (std / bigmem) | env `TARGET_WORKERS_STD` / `TARGET_WORKERS_BIGMEM` | 25 / 4 |
| Partitions, mem/cpu, walltime per controller | `build_slurm_controller_group()` | std: parallel, 2 cpu × 6 GB, 600 min; bigmem: bigmem, 2 cpu × 40 GB, 600 min |
| Extra controller (e.g. 10-cpu model fits) | commented `cpu10` block | — |
| Container image path | env `DEMAGOGUES_SIF` | `/nfs/scratch/marquexa/containers/demagogues.sif` |
| Scratch clone | env `DEMAGOGUES_SCRATCH` | `/nfs/scratch/marquexa/Demagogues` |
| Coordinator walltime | `deploy.ps1 -Walltime` (sbatch `--time`) | 2-00:00:00 |

Per-target routing lives in `_targets.R`:
`resources = tar_resources(crew = tar_resources_crew(controller = "bigmem"))`
on the DFM builds; everything else defaults to `"std"`. Targets with
`deployment = "main"` run in the coordinator.

Rāpoi partitions (July 2026): quicktest ≤64 CPU/128 G/5 h (default), parallel
≤256/512 G/10 d, bigmem ≤128/1 TB/10 d, longrun 30 d, gpu. Unspecified jobs
default to quicktest, 2 CPU, 2 GB, 1 h.

## Troubleshooting

| Symptom | Likely cause | Fix |
|---|---|---|
| `singularity pull` → 403 / unauthorized | GHCR package still private (first publish defaults to private) | Make the package public in GitHub package settings (one-time) |
| Workers stuck `PD` in squeue | Partition busy or per-user CPU limits | Lower `TARGET_WORKERS_STD`; check `squeue -u marquexa --start`; try off-peak |
| Coordinator log: `sbatch: command not found` or shim auth failure | Intra-cluster ssh key missing / not authorized, or shims not executable | Re-run `slurm/setup_raapoi.sh`; test on a compute node: `ssh raapoi.vuw.ac.nz sbatch --version` |
| R inside container can't find packages / renv activation messages | renv autoloader hijacked `.libPaths()` | Image sets `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE`; make sure you're not overriding it, and the image is current (`slurm/pull_image.sh`) |
| Workers launch then die instantly, `logs/worker-*.err` empty | Worker script not readable from compute node | Worker scripts must be on scratch: check `logs/crew/` exists under `DEMAGOGUES_SCRATCH` (created by launch.sh) |
| `Rscript: command not found` in worker logs | Re-exec into container didn't happen (module load failed?) | Check `module load Singularity/3.10.2` works in a fresh shell; check `DEMAGOGUES_SIF` path in `logs/worker-*.out` |
| EF download failures on workers | Compute nodes have no outbound network | Pre-stage `hathi-ef/` via rsync (see Data staging) |
| `deploy.ps1` SSH check fails | VPN down / no key | Follow the printed key-setup guidance |
| Container build fails on GitHub rate limits (GitHub-remote pkgs) | Unauthenticated API limit | Workflow already passes `GITHUB_TOKEN` as `GITHUB_PAT` build-arg; re-run the job |

## Untested until the first real cluster run

1. `singularity pull` of the GHCR image on Rāpoi (incl. package-visibility flip).
2. The worker re-exec pattern (`script_lines` in `cluster_resources.R`) under
   Singularity 3.10.2 on Rāpoi's compute nodes.
3. The ssh shims: intra-cluster passwordless ssh from a compute-node container
   back to the login node, and whether login-node policy allows it.
4. Whether Rāpoi compute nodes have outbound network for EF downloads.
5. Memory/walltime adequacy of the std (6 GB/cpu) and bigmem (40 GB/cpu)
   controller settings for the full 500-vols-per-decade run.
6. Quarto renders (`run_graph_html`, Paper/Appendix) inside the container on
   the coordinator.
7. End-to-end `deploy.ps1` → `sbatch` → bundle → `fetch_results.ps1` round trip.
