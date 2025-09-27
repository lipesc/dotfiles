# 🖼️ Carrega prompt Powerlevel10k
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
export QT_STYLE_OVERRIDE=Adwaita-Dark
export LSP_USE_PLISTS=true
export PATH="$HOME/.cargo/bin:$PATH"


# ⚡ Powerlevel10k InstantInstant Prompt# ⚡ Powerlevel10k Instant Prompt
# ⚡ Powerlevel10k Instant PromptPromptPromptPrompt
command -v kubectl >/dev/null 2>&1 && kubectl completion zsh > /dev/null
command -v gsettings >/dev/null 2>&1 && gsettings set org.gnome.desktop.interface gtk-theme Adwaita-dark > /dev/null 

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then 
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" 
fi

# 🧩 Manjaro config (ok deixar) 

# 🧩 Manjaro config (ok deixar) 


if [[ -e /usr/share/zsh/manjaro-zsh-config ]]; then
  source /usr/share/zsh/manjaro-zsh-config
fi
source <(kubectl completion zsh)

# 🎨 Powerlevel10k Theme 
source ~/.powerlevel10k/powerlevel10k.zsh-theme


plugins=(mise)

export EDITOR="vim"
export SUDO_EDITOR="$EDITOR"
alias s="source ~/.zshrc"
alias video=" mpv --gpu-api=opengl"
# ✅ Ativar mise corretamente
eval "$(mise activate zsh --shims)"

# 🧠 Angular CLI Autocomplete  
source <(ng completion script)



alias conda-use='source ~/miniconda3/etc/profile.d/conda.sh'
alias conda-dsfs="source ~/miniconda3/etc/profile.d/conda.sh && conda activate dsfs"
alias conda-deactivate="conda deactivate"
alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"

alias cht-shell="cht.sh --shell"
alias swaycfg='nvim ~/.config/sway/config'
alias waybarcfg='nvim ~/.config/waybar/config'
alias swayreload='swaymsg reload'
#GTK
#GTK
#export GDK_BACKEND=wayland
#export CLUTTER_BACKEND=wayland

#Qt (should use wayland by default)
#export QT_QPA_PLATFORM=xcb
#export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
export LIBVA_DRIVER_NAME=nvidia


export QT_QPA_PLATFORMTHEME="qt6ct"
export GTK_THEME=Adwaita:dark
export GTK2_RC_FILES=/usr/share/themes/Adwaita-dark/gtk-2.0/gtkrc
export QT_STYLE_OVERRIDE=Adwaita-Dark
gsettings set org.gnome.desktop.interface gtk-theme Adwaita-dark

alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'


