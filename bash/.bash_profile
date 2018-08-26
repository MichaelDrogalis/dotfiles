alias l="ls -lG"
alias e="emacs -nw"
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'
alias gb='git branch -v'
alias gc='git commit -m'
alias gl='git log'
alias gp='git push'
alias gco='git checkout'
alias gm='git merge --no-ff'

export GPG_TTY=$(tty)

function mvn-run() {
    mvn exec:java -Dexec.mainClass="$1"
}
