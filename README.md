# emacs_config — my Doom Emacs setup, rebuilt for Arch Linux

This repo **is** the Doom private config (`$DOOMDIR`). Clone it to
`~/.config/doom` and it becomes live — there is no copy step.

Migrated from WSL Ubuntu → Arch Linux. Everything below was checked against the
Arch repos / AUR / npm on **2026-09-19**; every version string in the tables is
what those sources returned that day, and anything I could not verify is marked
**[not verified]**.

---

## 0. TL;DR

```sh
# 1. on the fresh Arch install, as your normal user:
git clone https://github.com/Pamure/emacs_config ~/emacs_config
cd ~/emacs_config
./setup-arch.sh                 # system pkgs → LSP servers → conda → flutter → doom → config
# 2. put your OpenRouter key in ~/.authinfo (see §5)
# 3. verify (§7)
```

`setup-arch.sh --help` lists skip flags for the heavy parts (miniconda, flutter,
npm, AUR). It is idempotent and never deletes anything.

---

## 1. What's in here

| path | what it is |
|---|---|
| `init.el` | module list (`doom!` block) — the source of truth for features |
| `config.el` | all configuration; loads `config.local.el` **last** |
| `config.local.el.example` | template for per-machine credentials (copied to `config.local.el`, which is gitignored) |
| `packages.el` | extra packages: `corfu-terminal conda habitica jupyter code-cells org-alert org-pomodoro minuet` |
| `custom.el`, `.dir-locals.el` | Doom/Emacs boilerplate |
| `snippets/` | yasnippet snippets: C/C++ `main`/`dsatmp`, Java `main`, org `jpy` jupyter block |
| `dashboard.el` | custom dashboard (ASCII banner, org TODOs, projects, keybind cheatsheet) — **not loaded by default**, see §8 |
| `habiticasync.el` | Habitica → `todo.org` sync helpers — **not loaded by default**, see §8 |
| `setup-arch.sh` | the installer |
| `dotfiles/` | `authinfo.example`, `zshrc.snippet`, `clangd` (goes to `~/.clangd`) |
| `omp/models.yml` | portable copy of `~/.omp/agent/models.yml` — see `omp/README.md` |
| `legacy/` | the previous Ubuntu-era `setup.sh`, non-Doom `init.el`, `early-init.el` (reference only) |
| `lisp/drag-stuff/` | **not committed** — cloned by `setup-arch.sh` from `rejeep/drag-stuff.el` |

Enabled modules (from `init.el`): corfu+orderless, vertico, doom, dashboard,
emoji, indent-guides, modeline, treemacs, tabs, unicode, vi-tilde-fringe,
window-select, evil, fold, rotate-text, snippets, electric, undo, vc, vterm,
syntax, debugger+lsp, docker, eval+overlay, lookup, **llm**, **lsp**, magit,
pdf, tree-sitter, tty, cc+lsp, data, dart+flutter+lsp, emacs-lisp, go+lsp,
graphql+lsp, json, java+lsp, javascript+lsp+tree-sitter, kotlin, markdown,
org+pretty, python+lsp+pyright, rest+restclient, sh, web+lsp, yaml, sql+lsp+mysql.

---

## 2. Old machine → new machine: the full requirement list

Versions left of the arrow are what the WSL Ubuntu box actually had
(`doom doctor` + `command -v`); right side is what Arch provides today.

| purpose | old (Ubuntu/WSL) | Arch package (verified 2026-09-19) |
|---|---|---|
| Emacs | 30.2 | `emacs` **31.1** — built `--with-modules --with-native-compilation=aot --with-tree-sitter --with-pgtk` (from the PKGBUILD) |
| Doom core | v2.2.0 `7e98f188f` (2026-07-15) | same commit, pinned (§4.4) |
| Doom modules (`sources/doom+`) | v26.07.0 `c8f828c08` (2026-07-10) | same commit, submodule of the above |
| search / grep | ripgrep 15.1.0 | `ripgrep` 15.2.0 |
| file finder (`fd`) | fd 10.4.2 (binary `fdfind`) | `fd` 10.5.0 (binary is plain `fd`) |
| C/C++ LSP | clangd 18.1.3 | `clang` 22.1.8 — **ships `/usr/bin/clangd`** (checked in the file list) |
| Go LSP | gopls 0.21.1 (`go install`) | `gopls` 0.23.0 (also `go` 1.27.1) |
| Python LSP | pyright 1.1.408 **inside the torchgpu conda env** | same: `pip install pyright` in `torchgpu` (§4.6) |
| node runtime | nvm, node 20.20.2 / 24.11.0 | `nodejs` 26.9.0 + `npm` 12.0.2 (nvm optional) |
| TS/JS LSP | typescript-language-server (nvm v24) | `typescript` 7.0.2 + `typescript-language-server` 6.0.0 (npm) |
| json/html/css LSP | ✗ never installed | `vscode-langservers-extracted` 4.10.0 (npm) |
| GraphQL LSP | ✗ never installed | `graphql-language-service-cli` 3.5.0 (npm) |
| SQL LSP | ✗ never installed | `sql-language-server` 1.7.1 (npm) |
| shell LSP | ✗ never installed | `bash-language-server` 5.8.0 (npm) |
| Kotlin LSP | ✗ never installed | `kotlin` 2.4.20 (extra) + `kotlin-language-server` 1.3.13 (**AUR**) |
| Java LSP | jdtls (auto-downloaded by `lsp-java`) | same mechanism; JDK pinned to 21 (§4.1) |
| JVM | openjdk 21.0.12 | `jdk21-openjdk` 21.0.12.1.u1 |
| Dart/Flutter | flutter git checkout, dart 3.13.1 | git checkout of `stable` at `~/downloads/dev/flutter` (§4.7); AUR `flutter-bin` 3.47.4 / `flutter` 3.41.2 as an alternative |
| zsh (vterm shell) | zsh 5.9 | `zsh` 5.9.2 |
| vterm native module | libvterm + cmake + gcc | `libvterm` 0.3.3, `cmake` 4.4.3, `base-devel` |
| Jupyter/conda | miniconda 26.1.1, env `torchgpu`, kernels `torchgpu`/`aiml`/`sih` | same miniconda flow, `torchgpu` kernel re-registered (§4.6) |
| LaTeX export (xelatex + minted) | texlive 2023, pygments | `texlive-xetex` + `texlive-latexextra` (**ships `tex/latex/minted/minted.sty`**, checked) + `python-pygments` 2.21.0 |
| MySQL | mysql-server 8.0.46 | `mariadb` 13.0.2 + `mariadb-clients` (provides `/usr/bin/mysql`) |
| containers | docker ✗ missing | `docker` 29.8.1 + `docker-compose` 5.5.1 |
| fonts | JetBrainsMono Nerd Font (zip) + Symbols Nerd Font | `ttf-jetbrains-mono-nerd` 3.5.1 + `ttf-nerd-fonts-symbols` 3.5.1 |
| clipboard (tty Emacs) | xclip/xsel | `wl-clipboard` 2.3.0 (+ `xclip` 0.13) |
| misc | python3 3.12, sqlite3 | `python` 3.14, `python-pip` 26.2.1, `sqlite` 3.53.4, `poppler` 26.08 (pdf-tools) |
| tree-sitter CLI | ✗ missing (optional — Emacs builds grammars itself) | `tree-sitter` 0.26.9 optional |

Two consequences worth knowing:

* **`docker` and the `sh`/`sql`/`graphql`/`json`/`html`/`css`/`kotlin` LSP servers
  never worked on the old box** — the modules/flags were enabled but the binaries
  were absent. On Arch they get installed, so those modes will actually start
  servers the first time you open such a file.
* Arch's clang/gcc versions are far newer than Ubuntu's; the old `.clangd` file
  hardcoded Ubuntu GCC-13 include paths. `dotfiles/clangd` drops them (clangd
  asks the driver for the correct paths itself).

---

## 3. Secrets and machine state — what is deliberately missing

Nothing in this repo is secret. These were left behind and must be re-created:

| not in repo | where it lives on the old machine | how to bring it over |
|---|---|---|
| **OpenRouter API key** | `~/.authinfo` (+ a copy of the env var in `~/.config/emacs/env`) | §5 — retype the key from the old machine, or copy the line and `chmod 600` |
| **Habitica user id + API token** | `config.el` on the old machine (now removed from the tracked file) | fill `config.local.el`; token from Habitica → Settings → API |
| e-mail address | `config.el` | `config.local.el` |
| mysql users/hosts | `config.el` (`sql-connection-alist`) | `config.local.el` |
| `~/.config/emacs/` (Doom core + `env` + `.local/` packages & eln cache) | generated | re-cloned / regenerated (§4.4) |
| `~/.omp/agent/{agent,history,models}.db`, `sessions/`, `blobs/`, `cache/`, `tools/`, `terminal-sessions/` | ~560 MB of machine state | don't copy — see `omp/README.md` |
| `~/miniconda3`, `~/downloads/dev/flutter` | downloads | reinstalled by `setup-arch.sh` |
| `~/f/notes-org-mode/` | your notes (**its own git repo**) | `git clone`/`rsync` it separately — only `todo.org` + `notes.org` are needed for `org-agenda-files` |

> ⚠️ The private config's old git history contains that Habitica token. This repo
> (the sanitized one) has no such history, and the old repo has no remote. If you
> ever publish the old history, revoke the token in Habitica first.

---

## 4. Manual install (what `setup-arch.sh` automates)

### 4.1 System packages

```sh
sudo pacman -Syu --needed base-devel git curl wget unzip zip \
  ripgrep fd zsh \
  emacs clang cmake gopls go libvterm \
  sqlite poppler \
  python python-pip python-pygments \
  jdk21-openjdk \
  nodejs npm \
  docker docker-compose \
  mariadb mariadb-clients \
  texlive-xetex texlive-latexextra \
  ttf-jetbrains-mono-nerd ttf-nerd-fonts-symbols \
  wl-clipboard xclip kotlin
sudo archlinux-java set java-21-openjdk     # keep jdtls on Java 21 (default is 26)
```

Rationale for the non-obvious ones: `libvterm`+`cmake` are what the `vterm`
module compiles `vterm-module.so` against; `python-pygments` is what
`minted` shells out to; `ttf-nerd-fonts-symbols` is the "Symbols Nerd Font Mono"
that `doom doctor` expects; `mariadb-clients` is what gives the `mysql` binary
`sql-mode` calls.

### 4.2 AUR (Kotlin LSP)

```sh
paru -S --needed kotlin-language-server kotlin   # kotlin is in extra; LS is AUR-only
```
No AUR helper? Skip it — `:lang kotlin` still highlights, only the LSP stays cold.
`jdtls` (AUR 1.61.0) is **not** required: Doom's `lsp-java` downloads its own
Eclipse JDT LS the first time you open a `.java` file.

### 4.3 Node language servers

```sh
sudo npm install -g pyright typescript typescript-language-server \
  bash-language-server vscode-langservers-extracted \
  graphql-language-service-cli sql-language-server \
  dockerfile-language-server-nodejs
```
(`sudo` only because Arch's npm prefix is `/usr`; under nvm it isn't needed.)

### 4.4 Doom Emacs at the exact version

```sh
git clone https://github.com/doomemacs/doomemacs ~/.config/emacs
git -C ~/.config/emacs checkout -b doom-pinned 7e98f188f3ff686ba82e138dcaebfd7bb22af9a0  # v2.2.0, 2026-07-15
git -C ~/.config/emacs submodule update --init --recursive                 # sources/doom+ @ c8f828c08 (26.07.0)
git clone https://github.com/Pamure/emacs_config ~/.config/doom            # this repo == $DOOMDIR
cp ~/.config/doom/config.local.el.example ~/.config/doom/config.local.el   # then edit it
git clone --depth 1 https://github.com/rejeep/drag-stuff.el ~/.config/doom/lisp/drag-stuff
~/.config/emacs/bin/doom install --no-config
```

Both pinned commits were confirmed to exist in the upstream repos; `7e98f188f`
is the commit that carries the `sources/doom+` submodule, which is why the
submodule checkout lands on `c8f828c08` (v26.07.0). Doom core needs Emacs ≥27.1
and modules need ≥29.1 — Arch's 31.1 satisfies both. See §8 for the
30.2 → 31.1 caveat.

`doom install` also runs `doom env`, which snapshots your shell environment
(incl. any exported API keys) into `~/.config/emacs/env`. That file is
machine-local, not part of any repo — it exists so Emacs started from the
desktop launcher still sees your PATH. Run `doom env` again **from a fresh login
shell** once zsh/conda/nvm are set up, otherwise the snapshot misses those PATH
entries (`jupyter-executable` is pinned absolutely and `conda-env-activate`
fixes pyright, so only nvm-installed servers are affected).

### 4.5 The OpenRouter key (gptel + minuet)

```sh
cp ~/emacs_config/dotfiles/authinfo.example ~/.authinfo
chmod 600 ~/.authinfo
$EDITOR ~/.authinfo     # replace REPLACE_WITH_YOUR_KEY with your sk-or-v1-... key
```
`config.el` points gptel at `gptel-api-key-from-auth-source` and minuet reuses
the same key, so this one line powers both (`SPC o l l` = chat, ghost-text
completion in prog-mode). Optional hardening: `gpg -c ~/.authinfo`, delete the
plaintext, auth-source reads `.authinfo.gpg` transparently.

### 4.6 Miniconda + the `torchgpu` env (this is what makes jupyter blocks work)

The env name, its location, and the kernel name are all hardcoded in
`config.el` (`conda-env-activate "torchgpu"`,
`jupyter-executable ~/miniconda3/envs/torchgpu/bin/jupyter`, `:kernel torchgpu`):

```sh
curl -fsSL https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -o /tmp/miniconda.sh
bash /tmp/miniconda.sh -b -p ~/miniconda3
~/miniconda3/bin/conda init zsh
~/miniconda3/bin/conda create -y -n torchgpu python=3.11
~/miniconda3/envs/torchgpu/bin/pip install jupyter jupyterlab torch torchvision pyright
~/miniconda3/envs/torchgpu/bin/python -m ipykernel install --user --name torchgpu --display-name "Python (torchgpu)"
```
The last line is the one that's easy to miss: `org-babel-jupyter` resolves
kernels through `jupyter kernelspec list`, which reads the **user** kernel dir.

### 4.7 Flutter / dart-sdk

```sh
mkdir -p ~/downloads/dev && git clone --branch stable https://github.com/flutter/flutter.git ~/downloads/dev/flutter
~/downloads/dev/flutter/bin/flutter precache     # creates bin/cache/dart-sdk — the path config.el pins
```
AUR `flutter-bin` works too, but then fix `lsp-dart-*`/`flutter-sdk-path` in
`config.local.el` to point at `/opt/flutter`.

### 4.8 MariaDB + docker

```sh
sudo mariadb-install-db --user=mysql --basedir=/usr --datadir=/var/lib/mysql   # required before first start on Arch
sudo systemctl enable --now mariadb.service docker.service
sudo mariadb -e "CREATE USER IF NOT EXISTS 'abbas'@'localhost' IDENTIFIED BY '';"
sudo mariadb -e "CREATE DATABASE IF NOT EXISTS twentysix; CREATE DATABASE IF NOT EXISTS DBMSproj;"
sudo mariadb -e "GRANT ALL PRIVILEGES ON twentysix.* TO 'abbas'@'localhost'; GRANT ALL PRIVILEGES ON DBMSproj.* TO 'abbas'@'localhost'; FLUSH PRIVILEGES;"
sudo usermod -aG docker "$USER"      # log out/in
```
The empty-password `abbas` user is exactly what the old machine had; set
`DB_PASS=…` when running `setup-arch.sh` to give it a password and mirror it in
`config.local.el` as `(sql-password "…")`.

### 4.9 Shell + fonts

```sh
cat dotfiles/zshrc.snippet >> ~/.zshrc     # PATH, JAVA_HOME, GOPATH, FLUTTER_HOME
chsh -s /usr/bin/zsh                        # optional
fc-cache -f && fc-match "JetBrainsMono Nerd Font Mono"
```
`vterm-shell` is `/usr/bin/zsh`, so zsh must exist even if your login shell isn't zsh.

---

## 5. What still needs a human (from the old box)

1. The OpenRouter key (§4.5) — it is a secret; retype it from the old machine.
2. Habitica uid + token → `config.local.el`.
3. `sql-connection-alist` → `config.local.el` (template already contains your
   `muzaffarpur` / `mario` entries).
4. `~/f/notes-org-mode/{todo.org,notes.org}` — either copy them or let Emacs
   create them (`setup-arch.sh` touches empty ones so org-agenda does not error).
5. The optional helper `~/f/projects/godmode/mcp/org/godmode-org-helpers.el`:
   `config.el` loads it **only if it exists**, so its absence is a no-op.
6. `dotfiles/clangd` → `cp dotfiles/clangd ~/.clangd` if you want the global
   clangd fallback (no system include paths hardcoded anymore).

---

## 6. Verification

```sh
doom doctor                                  # expect "Everything seems fine"
emacs --version                              # 31.1
fc-match "JetBrainsMono Nerd Font Mono"      # -> JetBrainsMonoNerdFontMono-Regular.ttf
kpsewhich minted.sty                         # -> .../tex/latex/minted/minted.sty
jupyter kernelspec list                      # -> torchgpu
mysql -u abbas -e 'SHOW DATABASES;'           # -> twentysix, DBMSproj
which clangd gopls pyright typescript-language-server bash-language-server sql-language-server
```
In Emacs: `SPC o l l` (gptel — proves the key), open a `.py` (pyright),
`.c` (clangd), `.java` (jdtls download on first use), a `.md`/`.org` file, and an
`org` buffer with a `#+begin_src jupyter-python` block + `C-c C-c` (kernel
`torchgpu` starts).

---

## 7. Day-to-day

The repo **is** `~/.config/doom`, so config changes are one commit away:

```sh
cd ~/.config/doom
git add -A && git commit -m "…" && git push        # after editing init.el/config.el
~/.config/emacs/bin/doom sync                       # only needed after init.el / packages.el edits
```
`config.local.el`, `lisp/` (the drag-stuff clone) and byte-code are gitignored.

---

## 8. Caveats — read before trusting a green run

* **Emacs 31.1 vs 30.2.** Arch ships 31.1; this config was developed and is
  verified on 30.2. Doom core only requires ≥27.1 and Arch's build has every
  feature this config uses (modules/native-comp/tree-sitter/pgtk). There is **no
  `emacs30` package in the repos or the AUR** (checked) — if something in the
  jupyter/ZMQ advice chain misbehaves under 31.1, the fallback is building Emacs
  30 from source. Not verified either way until you run it. **[not verified]**
* `dashboard.el` is **not loaded**: nothing references it, and on the old box it
  could not have loaded — it had two defects, both fixed in this repo:
  1. an extra `)` closing `pam/dash-org-todos` (Emacs aborted the whole file at
     that point);
  2. `(format "%-*s" width str)` in `pam/dash-keybind-table` — Emacs Lisp's
     `format` has no `*` width (that is C `printf`); it is now
     `(format (format "%%-%ds" width) str)`.
  Both fixes are verified: the file loads and the banner, section rules,
  keybind table and footer render (18 lines of output) in batch Emacs, and the
  two widgets that depend on org/projectile correctly no-op when their
  preconditions are false.
  To enable the custom dashboard, add `(load! "dashboard")` to the end of
  `config.el`. Same for `habiticasync.el` (`(load! "habiticasync")` re-enables
  the `abbas/habitica-sync-to-org` binding that used to be commented out at
  `config.el:328`).
* Model names in `config.el` (gptel/minuet) are free-tier OpenRouter ids as of
  September 2026; free pools come and go — swap them if you get 404/429.
* `~/f/projects/godmode/...` and the `OMP`-side files are out of scope for this
  repo (see `omp/README.md` for the one portable file).
* The 4 MB `read-process-output-max`, the 0.5 s jupyter completion wait, and the
  corfu/vertico TAB fix in `config.el` were tuned on the old box; they are
  performance settings, not correctness requirements.

---

## 9. `legacy/`

`setup-ubuntu.sh` (the apt-based installer that built the WSL machine),
`init.el.ubuntu` (the pre-Doom plain-Emacs config) and `early-init.el`. Kept for
reference — `early-init.el`'s GC tuning is unnecessary under Doom (Doom does it),
and its `undecorated` frame settings already exist in `config.el`. The archived
installer is verbatim except for the e-mail address it used to carry, which was
replaced with a placeholder.
