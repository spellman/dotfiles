# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# ibus fix for Android Studio until ibus is updated to 1.5.11
export IBUS_ENABLE_SYNC_MODE=1
# Aliases for symlinks to respective JDK home directories, as per
# https://stackoverflow.com/questions/23318109/is-it-possible-to-use-java-8-for-android-development
export STUDIO_JDK="/usr/java/default"
export ANDROID_HOME="/home/cort/Android/Sdk"
export JAVA_HOME="/usr/java/default"
export JAVA8_HOME="/usr/java/java8"
export JAVA7_HOME="/usr/java/java7"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
