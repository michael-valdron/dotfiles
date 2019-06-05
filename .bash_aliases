# aliases

## tmux ##
# if tmux colour is not working..
# alias tmux='tmux -2'

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
alias home-vpn-connect='sudo openvpn --config ~/client.ovpn --daemon'
alias home-vpn-disconnect='sudo killall openvpn'

## SSH ##
# Home NAS
alias ssh-nas='ssh home-nas'

# dbsci
alias ssh-dbsci-sat='ssh dbsci-sat'
alias ssh-dbsci-twitter='ssh dbsci-twitter'

# GCP
alias gce='gcloud compute'
alias od-dev-start='gcloud compute instances start $OD_DEV_HOST --zone=us-central1-c'
alias od-dev-ssh='gcloud compute ssh $OD_DEV_USER@$OD_DEV_HOST --zone=us-central1-c'
alias od-dev-stop='gcloud compute instances stop $OD_DEV_HOST --zone=us-central1-c'

## SSHFS ##

# Home NAS
## Mount
alias sshfs-nas-home='sshfs home-nas:/data/home/mvaldron /mnt/mvaldron/home_nas/home'
alias sshfs-nas-apps='sshfs home-nas:/data/software /mnt/mvaldron/home_nas/software'
alias sshfs-nas-videos='sshfs home-nas:/data/videos /mnt/mvaldron/home_nas/videos'
alias sshfs-nas='sshfs-nas-home && sshfs-nas-apps && sshfs-nas-videos'

## Unmount
alias um-nas-home='fusermount3 -u /mnt/mvaldron/home_nas/home'
alias um-nas-apps='fusermount3 -u /mnt/mvaldron/home_nas/software'
alias um-nas-videos='fusermount3 -u /mnt/mvaldron/home_nas/videos'
alias um-nas='um-nas-home && um-nas-apps && um-nas-videos'

## Anaconda ##
# envs
# alias conda='~/anaconda3/bin/conda'
# alias source='~/anaconda3/bin/source'
# alias activate='~/anaconda3/bin/activate'
# alias deactivate='~/anaconda3/bin/deactivate'

## Clojure ##
alias clj='clojure'
