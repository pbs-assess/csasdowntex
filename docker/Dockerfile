# Docker script for csasdowntex containers
#
# This script was based loosely on this Dockerfile:
# https://github.com/rocker-org/rocker-versioned/blob/master/verse/3.6.3.Dockerfile

# Base rocker/tidyverse image https://hub.docker.com/u/rocker/
FROM rocker/tidyverse:latest
LABEL maintainer = "chris.grandin@gmail.com"

# Install Linux packages needed for various R packages to run
RUN apt-get update -qq && apt-get -y --no-install-recommends install libxt6

# Add LaTeX, rticles and bookdown support
#
# Working versions of install-unx.sh, install-base.sh, pkgs-custom.txt, and tinytex.profile
# are located in this directory but not used. They are there as a backup in case the URLs
# become broken in the future or change enough to break this script. Note that install-unx.sh
# calls install-base.sh and pkgs-custom.txt. install-base.sh installs tinytex.profile.
RUN wget -O- \
    "https://raw.githubusercontent.com/yihui/tinytex/main/tools/install-unx.sh" | \
    sh -s - --admin --no-path \
  && mv ~/.TinyTeX /opt/TinyTeX \
  && /opt/TinyTeX/bin/*/tlmgr path add \
  && tlmgr install tabu varwidth nowidow bookmark ncctools lineno \
    fancyhdr caption tocloft ragged2e everysel microtype \
    everypage pdflscape textpos enumitem algorithms algorithmicx \
    linegoal etex-pkg zref cite import xifthen ifmtarg colortbl \
    arydshln parskip oberdiek subfig relsize multirow \
    fp ms pgf placeins eso-pic pdfpages lastpage \
    soul pdfcomment datetime2 tracklang marginnote soulpos \
    ulem makecell scalerel was setspace sectsty tocbibind \
    hanging babel-english babel-french titlecaps ifnextok \
    textcase doi appendix soulpos chngcntr amsfonts soulutf8 \
    dvips booktabs wrapfig threeparttable threeparttablex draftwatermark \
    footmisc listings \
  && tlmgr path add \
  && Rscript -e "tinytex::r_texmf()" \
  && tlmgr init-usertree \
  && chown -R root:staff /opt/TinyTeX \
  && chmod -R g+w /opt/TinyTeX \
  && chmod -R g+wx /opt/TinyTeX/bin

COPY install_packages.R install_packages.R
RUN Rscript install_packages.R

ENV USER=rstudio PASSWORD=qwerty

# To run the container, go into the directory your document is in/will be in and run
# this (set project_name in --name=project_name to unique for each project):
#
# docker run -d -p 8787:8787 --name=project_name --mount type=bind,source="$(pwd)",target=/home/rstudio cgrandin/csasdowntex
#
# If prompted for the username and password, they are rstudio and qwerty
