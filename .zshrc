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
complete -o nospace -C /usr/local/bin/odo odo
