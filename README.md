<p align="center">
  <img src="https://github.com/erebe/greenclip/raw/master/logo.png" alt="logo"/>
</p>

## Description

Recyle your clipboard selections with greenclip and don't waste your time anymore
to reselect things other and other.

**Purpose :**
Keeps track of your history of selections to quickly switch between them

**Demo :** 
<video controls>
  <source src="https://github.com/erebe/greenclip/releases/download/1.1/demo.webm" type="video/webm">
 <a href="https://www.youtube.com/watch?v=Utk-9Gy8H3w">Video Link</a>
</video>

**Installation :**

1. It's a static binary so drop it anywhere in your $PATH env 

```wget https://github.com/erebe/greenclip/releases/download/1.3/greenclip```

Alternatively if you are using Archlinux you can install the package from AUR

``pacman -S greenclip``

PS: If you want, you can add a permanent list of selections to be added to your current history. Go see the config file


**Usage :**

Greenclip is intended to be used with [rofi](https://github.com/DaveDavenport/rofi)

1. Spawn the daemon ``` greenclip daemon ```
2. When ever you need to get your selections history ``` rofi -modi "clipboard:greenclip print" -show clipboard ```
3. The entry that you have selected will be in your clipboard now
4. Configuration file can be found in ```.config/greenclip.cfg```

**Compilation :**

1. Get [stack](https://docs.haskellstack.org/en/stable/README/) for Haskell
2. stack init && stack install

