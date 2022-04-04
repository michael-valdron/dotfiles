# Created by newuser for 5.7.1

if [[ -f ~/.zsh_aliases ]];
then
    source ~/.zsh_aliases
fi

if [[ -f ~/.zsh_myconfig ]];
then
    source ~/.zsh_myconfig
fi

autoload -U +X bashcompinit && bashcompinit

if [[ -f /usr/local/bin/odo && -n $(which compdef 2> /dev/null) ]];
then
    complete -o nospace -C /usr/local/bin/odo odo
fi

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
