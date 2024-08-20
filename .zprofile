source "$HOME/Projects/dotfiles/env_functions"

# Where `/usr/local` is assumed to be equivalent to `$(brew --prefix)`.
# Use GNU coreutils.
if [ -d "/usr/local/opt/coreutils/libexec/gnubin" ]; then
  path_prepend "/usr/local/opt/coreutils/libexec/gnubin" PATH
fi

# Use GNU grep, which is not part of coreutils.
if [ -d "/usr/local/opt/grep/libexec/gnubin" ]; then
  path_prepend "/usr/local/opt/grep/libexec/gnubin" PATH
fi

# Use GNU sed, which is not part of coreutils.
if [ -d "/usr/local/opt/gnu-sed/libexec/gnubin" ]; then
  path_prepend "/usr/local/opt/gnu-sed/libexec/gnubin" PATH
fi

# Use GNU awk, which is not part of coreutils.
if [ -d "/usr/local/opt/gawk/libexec/gnubin" ]; then
  path_prepend "/usr/local/opt/gawk/libexec/gnubin" PATH
fi

# Use GNU gnu-indent, which is not part of coreutils.
if [ -d "/usr/local/opt/gnu-indent/libexec/gnubin" ]; then
  path_prepend "/usr/local/opt/gnu-indent/libexec/gnubin" PATH
fi

# Use GNU findutils, which is not part of coreutils.
if [ -d "/usr/local/opt/findutils/libexec/gnubin" ]; then
  path_prepend "/usr/local/opt/findutils/libexec/gnubin" PATH
fi

# Rye: Add shims to path, as per https://rye.astral.sh/guide/installation
if [ -d "${HOME}/.rye" ]; then
  . "${HOME}/.rye/env"
fi
