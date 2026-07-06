# syntax=docker/dockerfile:1
#
# Reproducible R environment for the "Vector Space of Democracy" pipeline.
#
# Built by .github/workflows/container.yml and pushed to
# ghcr.io/xmarquez/demagogues:latest. On Raapoi we `singularity pull` this
# image and run the whole targets pipeline (coordinator + crew workers) inside
# it. See HPC.md for the full architecture.
#
# The R version here MUST match renv.lock ("R" -> "Version").
FROM rocker/r-ver:4.5.1

LABEL org.opencontainers.image.source="https://github.com/xmarquez/Demagogues"
LABEL org.opencontainers.image.description="R + renv environment for the Vector Space of Democracy pipeline"
LABEL org.opencontainers.image.licenses="MIT"

# --- System libraries -------------------------------------------------------
# Covers the compiled dependencies of the stack: quanteda/stringi (libicu),
# xml2 (libxml2), curl/openssl (libcurl, libssl), ragg/textshaping
# (fontconfig, harfbuzz, fribidi, freetype, png, tiff, jpeg), igraph (glpk),
# and general build needs (zlib, git, ca-certificates).
RUN apt-get update && apt-get install -y --no-install-recommends \
      libxml2-dev \
      libcurl4-openssl-dev \
      libssl-dev \
      libfontconfig1-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libfreetype6-dev \
      libpng-dev \
      libtiff5-dev \
      libjpeg-dev \
      libglpk-dev \
      zlib1g-dev \
      libicu-dev \
      cmake \
      libmbedtls-dev \
      libx11-dev \
      git \
      ca-certificates \
      curl \
      gdebi-core \
      openssh-client \
    && rm -rf /var/lib/apt/lists/*

# --- Quarto CLI -------------------------------------------------------------
# Needed by the run_graph_html / Appendix / Paper render targets.
# `quarto.org/download/latest` redirects to the newest amd64 .deb; to pin a
# specific version instead use e.g.
#   https://github.com/quarto-dev/quarto-cli/releases/download/v1.5.57/quarto-1.5.57-linux-amd64.deb
RUN curl -fsSL -o /tmp/quarto.deb https://quarto.org/download/latest/quarto-linux-amd64.deb \
    && gdebi --non-interactive /tmp/quarto.deb \
    && rm -f /tmp/quarto.deb \
    && quarto --version

# --- renv restore -----------------------------------------------------------
# CRITICAL: disable the renv autoloader. The project .Rprofile sources
# renv/activate.R; inside the container we must NOT let renv hijack
# .libPaths() away from the baked /usr/local/lib/R/site-library.
ENV RENV_CONFIG_AUTOLOADER_ENABLED=FALSE
# Restore into the default site library so plain `Rscript` / `R` find everything
# with no project-level renv activation.
ENV RENV_PATHS_LIBRARY=/usr/local/lib/R/site-library

COPY renv.lock /tmp/renv.lock

# renv::restore resolves GitHub-remote packages (e.g. wordVectors from
# bmschmidt/wordVectors, hathiTools from xmarquez/hathiTools). Public repos need
# no PAT, but unauthenticated GitHub API calls are rate-limited; pass
# --build-arg GITHUB_PAT=... to raise the limit if a build ever hits 403.
ARG GITHUB_PAT=""
ENV GITHUB_PAT=${GITHUB_PAT}
RUN Rscript -e "install.packages('renv', repos = 'https://cloud.r-project.org')" \
    && Rscript -e "renv::restore(lockfile = '/tmp/renv.lock', library = '/usr/local/lib/R/site-library', prompt = FALSE)" \
    && Rscript -e "stopifnot(requireNamespace('targets', quietly = TRUE), requireNamespace('crew', quietly = TRUE), requireNamespace('crew.cluster', quietly = TRUE), requireNamespace('quanteda', quietly = TRUE))"

WORKDIR /project
CMD ["R"]
