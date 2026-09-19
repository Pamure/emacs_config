#!/usr/bin/env bash
# setup-arch.sh — rebuild Pamure's Doom Emacs environment on Arch Linux.
#
#   ./setup-arch.sh                 # do everything
#   ./setup-arch.sh --help          # list flags
#
# It is idempotent: every step checks first and skips work that is already done.
# Nothing is deleted, and no repository file is overwritten.
#
# Package names/versions below were verified against the Arch repos and AUR on
# 2026-09-19. See README.md for the full requirement table and the "not
# verified" caveats (notably: Arch now ships Emacs 31.1, this config was
# developed on Emacs 30.2).

set -euo pipefail

# ── Config ─────────────────────────────────────────────────────────────────
DOOM_REPO="https://github.com/doomemacs/doomemacs"
DOOM_COMMIT="7e98f188f3ff686ba82e138dcaebfd7bb22af9a0"        # doom v2.2.0, 2026-07-15
DOOM_PLUS_COMMIT="c8f828c08230c015e0ed55e317487f57639cfaa6"   # doom+ v26.07.0, 2026-07-10
CONFIG_REPO="https://github.com/Pamure/emacs_config"
DRAG_STUFF_REPO="https://github.com/rejeep/drag-stuff.el"

EMACSDIR="${HOME}/.config/emacs"
DOOMDIR="${HOME}/.config/doom"
CONDA_HOME="${HOME}/miniconda3"
CONDA_ENV="torchgpu"
FLUTTER_DIR="${HOME}/downloads/dev/flutter"
DB_USER="${DB_USER:-abbas}"
DB_PASS="${DB_PASS:-}"   # empty = what the old machine used; see README

SKIP_PACMAN=0; SKIP_AUR=0; SKIP_NODE=0; SKIP_ML=0; SKIP_FLUTTER=0
SKIP_SERVICES=0; SKIP_DOOM=0; SET_SHELL=0

usage() {
  cat <<'USAGE'
setup-arch.sh — rebuild Pamure's Doom Emacs environment on Arch Linux.

  ./setup-arch.sh                 # do everything
  ./setup-arch.sh --help          # this text

It is idempotent: every step checks first and skips work that is already done.
Nothing is deleted, and no repository file is overwritten.
USAGE
  cat <<'FLAGS'

Flags:
  --skip-pacman     don't install/upgrade system packages
  --skip-aur        don't install AUR packages (kotlin-language-server)
  --skip-node       don't install the npm language servers
  --skip-ml         don't install miniconda + the torchgpu env (big download)
  --skip-flutter    don't clone flutter (~3 GB)
  --skip-services   don't touch mariadb/docker services
  --skip-doom       don't clone/install Doom itself
  --set-shell       also run `chsh -s /usr/bin/zsh`
  --yes             non-interactive confirmations
FLAGS
}

for arg in "$@"; do
  case "$arg" in
    --skip-pacman)   SKIP_PACMAN=1 ;;
    --skip-aur)      SKIP_AUR=1 ;;
    --skip-node)     SKIP_NODE=1 ;;
    --skip-ml)       SKIP_ML=1 ;;
    --skip-flutter)  SKIP_FLUTTER=1 ;;
    --skip-services) SKIP_SERVICES=1 ;;
    --skip-doom)     SKIP_DOOM=1 ;;
    --set-shell)     SET_SHELL=1 ;;
    --yes)           export ASSUME_YES=1 ;;
    -h|--help)       usage; exit 0 ;;
    *) echo "unknown flag: $arg (try --help)" >&2; exit 2 ;;
  esac
done

# ── Helpers ────────────────────────────────────────────────────────────────
log()  { printf '\n\033[1;34m==>\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m[!]\033[0m %s\n' "$*" >&2; }
have() { command -v "$1" >/dev/null 2>&1; }

REPO_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

if [ -r /etc/os-release ]; then
  # shellcheck disable=SC1091
  . /etc/os-release
  log "host: ${PRETTY_NAME:-unknown}"
  case "${ID:-}" in
    arch|archarm|endeavouros|manjaro|garuda|cachyos) : ;;
    *) warn "this script assumes an Arch-based distro (pacman); package step may fail" ;;
  esac
fi

# ── 1. System packages ─────────────────────────────────────────────────────
# One -Syu transaction: on Arch, syncing without upgrading is unsupported, and
# --needed makes re-runs no-ops. Everything here is a hard dependency of the
# Emacs config except poppler/zip/unzip (pdf-tools / tooling conveniences).
PACMAN_PKGS=(
  base-devel git curl wget unzip zip
  ripgrep fd zsh                       # doom search + vterm shell
  emacs                                # 31.1, built --with-modules --with-native-compilation=aot --with-tree-sitter --with-pgtk
  clang cmake gopls go                 # C/C++ (clangd ships inside clang), vterm build, Go LSP
  libvterm                             # vterm-module.so needs the header+lib
  sqlite poppler                       # emacs sqlite support, pdf-tools
  python python-pip python-pygments    # pygments = minted's syntax highlighter for org-export
  jdk21-openjdk                        # config verified against Java 21 (jdtls)
  nodejs npm                           # every JS-based language server
  docker docker-compose
  mariadb mariadb-clients              # `mysql' client binary comes from mariadb-clients
  texlive-xetex texlive-latexextra     # xelatex + minted.sty (verified: texlive-latexextra ships it)
  ttf-jetbrains-mono-nerd              # doom-font
  ttf-nerd-fonts-symbols               # doom-modeline / nerd-icons glyphs
  wl-clipboard xclip                   # clipboard for the :os tty module
  kotlin
)

step_pacman() {
  [ "$SKIP_PACMAN" -eq 1 ] && { log "skipping pacman step"; return 0; }
  log "installing system packages (pacman -Syu --needed)"
  sudo pacman -Syu --needed --noconfirm "${PACMAN_PKGS[@]}"
}

# ── 2. AUR ─────────────────────────────────────────────────────────────────
# kotlin-language-server has no repo package (kotlin itself is in extra, above).
# jdtls is optional: Doom's lsp-java downloads its own copy on first .java file.
step_aur() {
  [ "$SKIP_AUR" -eq 1 ] && { log "skipping AUR step"; return 0; }
  local helper=""
  have paru && helper="paru"
  [ -z "$helper" ] && have yay && helper="yay"
  if [ -z "$helper" ]; then
    warn "no AUR helper found (install paru or yay), skipping:"
    warn "  kotlin-language-server   (kotlin +lsp)"
    return 0
  fi
  log "installing AUR packages via $helper"
  "$helper" -S --needed --noconfirm kotlin-language-server
}

# ── 3. Node language servers ───────────────────────────────────────────────
NPM_GLOBALS=(
  pyright                                   # python (+pyright) — see also the
                                            # pip copy inside the torchgpu env
  typescript typescript-language-server     # javascript (+lsp)
  bash-language-server                      # sh (+lsp)
  vscode-langservers-extracted              # json / html / css / eslint (+lsp, web)
  graphql-language-service-cli              # graphql (+lsp)
  sql-language-server                       # sql (+lsp)
  dockerfile-language-server-nodejs         # docker module LSP (optional)
)

step_node() {
  [ "$SKIP_NODE" -eq 1 ] && { log "skipping npm step"; return 0; }
  log "installing npm language servers globally"
  # Arch's npm prefix is /usr (needs sudo); an nvm-managed node uses ~/.nvm (no sudo).
  local prefix; prefix="$(npm config get prefix)"
  if [ "$prefix" = "/usr" ]; then
    sudo npm install -g "${NPM_GLOBALS[@]}"
  else
    npm install -g "${NPM_GLOBALS[@]}"
  fi
}

# ── 4. Miniconda + the torchgpu env (jupyter / org-babel) ──────────────────
# config.el pins `jupyter-executable' to
# ~/miniconda3/envs/torchgpu/bin/jupyter and the kernel name `torchgpu', so
# both the env name and the location are load-bearing.
step_ml() {
  [ "$SKIP_ML" -eq 1 ] && { log "skipping miniconda/torchgpu step"; return 0; }
  if [ ! -x "${CONDA_HOME}/bin/conda" ]; then
    log "installing miniconda into ${CONDA_HOME}"
    local tmp; tmp="$(mktemp -d)"
    curl -fsSL "https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh" -o "$tmp/miniconda.sh"
    bash "$tmp/miniconda.sh" -b -p "$CONDA_HOME"
    rm -rf "$tmp"
    "${CONDA_HOME}/bin/conda" init zsh  || warn "conda init zsh failed"
    "${CONDA_HOME}/bin/conda" init bash || warn "conda init bash failed"
  else
    log "miniconda already present at ${CONDA_HOME}"
  fi
  if [ ! -d "${CONDA_HOME}/envs/${CONDA_ENV}" ]; then
    log "creating conda env '${CONDA_ENV}' (python 3.11)"
    "${CONDA_HOME}/bin/conda" create -y -n "$CONDA_ENV" python=3.11
  else
    log "conda env '${CONDA_ENV}' already exists"
  fi
  log "installing jupyter / torch / pyright into '${CONDA_ENV}'"
  "${CONDA_HOME}/envs/${CONDA_ENV}/bin/pip" install --upgrade pip
  "${CONDA_HOME}/envs/${CONDA_ENV}/bin/pip" install jupyter jupyterlab torch torchvision pyright
  # org-babel-jupyter resolves kernels through `jupyter kernelspec list', which
  # reads the *user* kernel dir — this is what registers "torchgpu".
  log "registering the 'torchgpu' kernelspec (user-level)"
  "${CONDA_HOME}/envs/${CONDA_ENV}/bin/python" -m ipykernel install --user \
    --name "$CONDA_ENV" --display-name "Python (${CONDA_ENV})"
}

# ── 5. Flutter (dart +lsp) ─────────────────────────────────────────────────
# Cloned to the exact path config.el points lsp-dart at; `flutter precache'
# is what produces bin/cache/dart-sdk on a fresh checkout.
step_flutter() {
  [ "$SKIP_FLUTTER" -eq 1 ] && { log "skipping flutter step"; return 0; }
  if [ ! -x "${FLUTTER_DIR}/bin/flutter" ]; then
    log "cloning flutter (stable) into ${FLUTTER_DIR}"
    mkdir -p "$(dirname -- "$FLUTTER_DIR")"
    git clone --branch stable https://github.com/flutter/flutter.git "$FLUTTER_DIR"
  fi
  log "flutter precache (downloads bin/cache/dart-sdk)"
  "${FLUTTER_DIR}/bin/flutter" precache
  "${FLUTTER_DIR}/bin/flutter" --version || warn "flutter --version failed; check 'flutter doctor'"
}

# ── 6. Services: mariadb + docker ──────────────────────────────────────────
step_services() {
  [ "$SKIP_SERVICES" -eq 1 ] && { log "skipping services step"; return 0; }

  if have mariadb-install-db && [ ! -d /var/lib/mysql/mysql ]; then
    log "initializing the MariaDB data directory (Arch requires this before first start)"
    sudo mariadb-install-db --user=mysql --basedir=/usr --datadir=/var/lib/mysql
  fi
  if have systemctl; then
    sudo systemctl enable --now mariadb.service || warn "could not start mariadb.service"
    sudo systemctl enable --now docker.service  || warn "could not start docker.service"
  fi
  if [ -z "$DB_PASS" ]; then
    warn "creating mariadb user '${DB_USER}'@localhost with an EMPTY password"
    warn "  (this is what the old machine used; set DB_PASS=... to change it and"
    warn "   mirror it in config.local.el as (sql-password ...))"
  fi
  sudo mariadb -e "CREATE USER IF NOT EXISTS '${DB_USER}'@'localhost' IDENTIFIED BY '${DB_PASS}';" || true
  sudo mariadb -e "CREATE DATABASE IF NOT EXISTS twentysix;" || true
  sudo mariadb -e "CREATE DATABASE IF NOT EXISTS DBMSproj;"  || true
  sudo mariadb -e "GRANT ALL PRIVILEGES ON twentysix.* TO '${DB_USER}'@'localhost';" || true
  sudo mariadb -e "GRANT ALL PRIVILEGES ON DBMSproj.*  TO '${DB_USER}'@'localhost';" || true
  sudo mariadb -e "FLUSH PRIVILEGES;" || true

  if ! id -nG "$USER" | tr ' ' '\n' | grep -qx docker; then
    log "adding ${USER} to the docker group (log out/in for it to apply)"
    sudo usermod -aG docker "$USER"
  fi
}

# ── 7. Shell environment ───────────────────────────────────────────────────
step_shell() {
  log "installing the zsh environment block (idempotent)"
  touch "${HOME}/.zshrc"
  if ! grep -q 'Pamure Doom/dev environment' "${HOME}/.zshrc"; then
    {
      printf '\n# ── Pamure Doom/dev environment ──\n'
      grep -v '^#' "${REPO_DIR}/dotfiles/zshrc.snippet" | grep -v '^$'
    } >> "${HOME}/.zshrc"
    log "appended PATH/JAVA_HOME/FLUTTER_HOME lines to ~/.zshrc"
  else
    log "$HOME/.zshrc already carries the block"
  fi
  if [ "$SET_SHELL" -eq 1 ] && [ "$(getent passwd "$USER" | cut -d: -f7)" != "/usr/bin/zsh" ]; then
    log "setting the login shell to zsh"
    chsh -s /usr/bin/zsh "$USER"
  fi
}

# ── 8. Doom Emacs + this config ────────────────────────────────────────────
step_doom() {
  [ "$SKIP_DOOM" -eq 1 ] && { log "skipping Doom step"; return 0; }

  if [ ! -x "${EMACSDIR}/bin/doom" ]; then
    log "cloning Doom core into ${EMACSDIR} @ ${DOOM_COMMIT:0:9}"
    git clone "$DOOM_REPO" "$EMACSDIR"
    git -C "$EMACSDIR" checkout --quiet "$DOOM_COMMIT"
  else
    log "Doom core already installed: $("${EMACSDIR}/bin/doom" version | head -2 | tr '\n' ' ')"
  fi

  # modules live in a submodule introduced by the pinned core commit
  if [ ! -d "${EMACSDIR}/sources/doom+/modules" ]; then
    log "initializing Doom's module source (sources/doom+)"
    git -C "$EMACSDIR" submodule update --init --recursive
  fi
  local got_plus
  got_plus="$(git -C "${EMACSDIR}/sources/doom+" rev-parse HEAD 2>/dev/null || true)"
  if [ "$got_plus" = "$DOOM_PLUS_COMMIT" ]; then
    log "doom+ modules pinned at ${DOOM_PLUS_COMMIT:0:9} (exact match)"
  else
    warn "doom+ modules are at ${got_plus:0:9}, expected ${DOOM_PLUS_COMMIT:0:9}"
    warn "  run: git -C '${EMACSDIR}' submodule update --init --recursive"
  fi

  if [ ! -d "$DOOMDIR" ]; then
    log "cloning the private config into ${DOOMDIR}"
    git clone "$CONFIG_REPO" "$DOOMDIR"
  else
    warn "$DOOMDIR already exists — leaving it untouched."
    warn "  compare with: diff -r '${REPO_DIR}' '${DOOMDIR}'  (ignoring .git)"
  fi

  [ -f "${DOOMDIR}/config.local.el" ] || {
    cp "${DOOMDIR}/config.local.el.example" "${DOOMDIR}/config.local.el"
    warn "created ${DOOMDIR}/config.local.el from the template — FILL IT IN"
    warn "  (habitica token, mail address, sql-connection-alist)"
  }
  [ -f "${DOOMDIR}/custom.el" ] || printf '(put (quote customize-group) (quote disabled) nil)\n' > "${DOOMDIR}/custom.el"

  mkdir -p "${DOOMDIR}/lisp"
  if [ ! -d "${DOOMDIR}/lisp/drag-stuff/.git" ]; then
    log "cloning drag-stuff.el (config.el adds it to load-path)"
    git clone --depth 1 "$DRAG_STUFF_REPO" "${DOOMDIR}/lisp/drag-stuff"
  fi

  log "doom install (installs ~190 packages; --no-config keeps our config)"
  "${EMACSDIR}/bin/doom" install --no-config
}

# ── 9. Secrets ─────────────────────────────────────────────────────────────
step_secrets() {
  if [ ! -f "${HOME}/.authinfo" ]; then
    log "creating ~/.authinfo from the template (currently a placeholder key)"
    cp "${REPO_DIR}/dotfiles/authinfo.example" "${HOME}/.authinfo"
    warn "put your real OpenRouter key in ~/.authinfo — see README §5"
  else
    log "$HOME/.authinfo already exists — not touching it"
  fi
  chmod 600 "${HOME}/.authinfo" 2>/dev/null || true
}

# ── 10. Misc dirs the config reads ─────────────────────────────────────────
step_dirs() {
  mkdir -p "${HOME}/f/notes-org-mode"
  [ -f "${HOME}/f/notes-org-mode/todo.org" ]  || : > "${HOME}/f/notes-org-mode/todo.org"
  [ -f "${HOME}/f/notes-org-mode/notes.org" ] || : > "${HOME}/f/notes-org-mode/notes.org"
  fc-cache -f >/dev/null 2>&1 || true
}

# ── Run ────────────────────────────────────────────────────────────────────
step_pacman
step_aur
step_node
step_ml
step_flutter
step_services
step_shell
step_doom
step_secrets
step_dirs

cat <<'NEXT'

────────────────────────────────────────────────────────────────────────────
Done. Now verify (in a NEW zsh login shell, so PATH/conda/docker group apply):

  doom doctor                                  # should end with "Everything seems fine"
  doom sync                                    # after any init.el/packages.el edit
  emacs --version                              # Emacs 31.1 (config was built on 30.2)
  fc-match "JetBrainsMono Nerd Font Mono"      # -> JetBrainsMonoNerdFontMono-Regular.ttf
  kpsewhich minted.sty                         # -> .../tex/latex/minted/minted.sty
  jupyter kernelspec list                      # -> torchgpu
  mysql -u abbas -e 'SHOW DATABASES;'          # -> twentysix, DBMSproj
  getent passwd "$USER" | cut -d: -f7          # -> /usr/bin/zsh (if you used --set-shell)

Then open Emacs and check:
  * SPC o l l (gptel chat) — proves the OpenRouter key in ~/.authinfo works
  * open a .py file — pyright from the torchgpu env must be on PATH
  * open a .c file  — clangd
  * an org file with a #+begin_src jupyter-python block, C-c C-c — kernel starts
  * M-x nerd-icons-install-fonts (only if modeline/sidebar glyphs look wrong)
────────────────────────────────────────────────────────────────────────────
NEXT
