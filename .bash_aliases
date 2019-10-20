#!/bin/bash
# helper values
TZ=$(timedatectl status | grep -Po "(?<=Time zone: )(.+)(?= \()")
IP_ADDR=$(ip addr show bond0 | grep 'inet ' | cut -d: -f2 | awk '{print $2}' | grep -Po "(.+)(?=/24)")

# aliases

## tmux ##
# if tmux colour is not working..
# alias tmux='tmux -2'

## openssl ##
alias encrypt='bash ~/.scripts/enc.sh'
alias decrypt='bash ~/.scripts/dec.sh'

## rclone ##
# GDrive
alias gdrive-mount='rclone mount gdrive:/ "${HOME}/GDrives/Google Drive Stream/" &'
alias gdrive-umount='fusermount -u "${HOME}/GDrives/Google Drive Stream"'
alias gdrive-push='rclone sync "${PWD}" "gdrive:${1}" -v -u --checkers 1 --transfers 1'
alias gdrive-pull='rclone sync "gdrive:${1}" "${PWD}" -v -u --checkers 1 --transfers 1'

# UOIT Drive
alias udrive-mount='rclone mount udrive:/ "${HOME}/GDrives/UOIT Drive Stream/" &'
alias udrive-umount='fusermount -u "${HOME}/GDrives/UOIT Drive Stream"'
alias udrive-push='rclone sync "${PWD}" "udrive:${1}" -v -u --checkers 1 --transfers 1'
alias udrive-pull='rclone sync "udrive:${1}" "${PWD}" -v -u --checkers 1 --transfers 1'

# OneDrive
alias odrive-mount='rclone mount onedrive:/ "${HOME}/GDrives/OneDrive Stream/" &'
alias odrive-umount='fusermount -u "${HOME}/GDrives/OneDrive Stream"'
alias odrive-push='rclone sync "${PWD}" "onedrive:${1}" -v -u --checkers 1 --transfers 1'
alias odrive-pull='rclone sync "onedrive:${1}" "${PWD}" -v -u --checkers 1 --transfers 1'

## VPN ##
# alias home-vpn-connect='sudo openvpn --config ~/client.ovpn --daemon'
# alias home-vpn-disconnect='sudo killall openvpn'

## SSH ##
# Home NAS
alias ssh-nas='ssh home-nas'

# dbsci
alias ssh-dbsci-sat='ssh dbsci-sat'
alias ssh-dbsci-twitter='ssh dbsci-twitter'

# rtrsci
alias ssh-rtrsci-dev='ssh rtrsci-dev'

# GCP
alias gce='gcloud compute'
# alias od-dev-start='gcloud compute instances start $OD_DEV_HOST --zone=us-central1-c'
# alias od-dev-ssh='gcloud compute ssh $OD_DEV_USER@$OD_DEV_HOST --zone=us-central1-c'
# alias od-dev-stop='gcloud compute instances stop $OD_DEV_HOST --zone=us-central1-c'

## SSHFS ##

# Home NAS
## Mount
alias sshfs-nas-home='sshfs home-nas:/data/home/mvaldron /mnt/mvaldron/home_nas/home'
alias sshfs-nas-apps='sshfs home-nas:/data/software /mnt/mvaldron/home_nas/software'
alias sshfs-nas-videos='sshfs home-nas:/data/videos /mnt/mvaldron/home_nas/videos'
alias sshfs-nas-shares='sshfs home-nas:/data/shares /mnt/mvaldron/home_nas/shares'
alias sshfs-nas='sshfs-nas-home && sshfs-nas-apps && sshfs-nas-videos && sshfs-nas-shares'

## Unmount
alias um-nas-home='fusermount3 -u /mnt/mvaldron/home_nas/home'
alias um-nas-apps='fusermount3 -u /mnt/mvaldron/home_nas/software'
alias um-nas-videos='fusermount3 -u /mnt/mvaldron/home_nas/videos'
alias um-nas-shares='fusermount3 -u /mnt/mvaldron/home_nas/shares'
alias um-nas='um-nas-home && um-nas-apps && um-nas-videos && um-nas-shares'

## Anaconda ##
# envs
alias conda='~/anaconda3/bin/conda'
alias source='~/anaconda3/bin/source'
alias activate='~/anaconda3/bin/activate'
alias deactivate='~/anaconda3/bin/deactivate'

## Clojure ##
alias clj='clojure'

## Docker ##
if [ -f "${HOME}/.bash_servconfig" ]; then
    . $HOME/.bash_servconfig
    alias docker-create-plex="docker run \
-d \
--name plex-server \
-p 32400:32400/tcp \
-p 3005:3005/tcp \
-p 8324:8324/tcp \
-p 32469:32469/tcp \
-p 1900:1900/udp \
-p 32410:32410/udp \
-p 32412:32412/udp \
-p 32413:32413/udp \
-p 32414:32414/udp \
-e PLEX_CLAIM=\"${PLEX_CLAIM_TOK}\" \
-e TZ=\"${TZ}\" \
-e ADVERTISE_IP=\"http://${IP_ADDR}:32400/\" \
-h $HOSTNAME-plex \
-v $PLEX_DB_PATH:/config \
-v $PLEX_TRANS_PATH:/transcode \
-v $PLEX_MEDIA_PATH:/data \
plexinc/pms-docker"
fi

