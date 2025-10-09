source "$HOME/Projects/dotfiles/env_functions"

# Use GNU coreutils.
if [ -d "$(brew --prefix)/opt/coreutils/libexec/gnubin" ]; then
  path_prepend "$(brew --prefix)/opt/coreutils/libexec/gnubin" PATH
fi

# Use GNU grep, which is not part of coreutils.
if [ -d "$(brew --prefix)/opt/grep/libexec/gnubin" ]; then
  path_prepend "$(brew --prefix)/opt/grep/libexec/gnubin" PATH
fi

# Use GNU sed, which is not part of coreutils.
if [ -d "$(brew --prefix)/opt/gnu-sed/libexec/gnubin" ]; then
  path_prepend "$(brew --prefix)/opt/gnu-sed/libexec/gnubin" PATH
fi

# Use GNU awk, which is not part of coreutils.
if [ -d "$(brew --prefix)/opt/gawk/libexec/gnubin" ]; then
  path_prepend "$(brew --prefix)/opt/gawk/libexec/gnubin" PATH
fi

# Use GNU gnu-indent, which is not part of coreutils.
if [ -d "$(brew --prefix)/opt/gnu-indent/libexec/gnubin" ]; then
  path_prepend "$(brew --prefix)/opt/gnu-indent/libexec/gnubin" PATH
fi

# Use GNU findutils, which is not part of coreutils.
if [ -d "$(brew --prefix)/opt/findutils/libexec/gnubin" ]; then
  path_prepend "$(brew --prefix)/opt/findutils/libexec/gnubin" PATH
fi

# Rye: Add shims to path, as per https://rye.astral.sh/guide/installation
if [ -d "${HOME}/.rye" ]; then
  . "${HOME}/.rye/env"
fi
