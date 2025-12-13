# Agent Notes (Codex)

- Shell is PowerShell: don’t chain commands with `&&` (use separate commands or `;`).
- When committing only specific files: stage explicitly, verify with `git diff --cached --name-only`, and unstage accidental additions with `git restore --staged <path>`.
- Documentation convention for `R/`: add roxygen2 blocks for each function (and for S3 generics/methods) including `@param`/`@return` and any important assumptions (expected columns, required docvars, etc.).
- Quick sanity checks after edits:
  - R files: `Rscript -e "source('R/<file>.R')"`
  - YAML config: `Rscript -e "yaml::read_yaml('config/pipeline.yml')"`

