<p align="center">
  <img src="https://github.com/erebe/greenclip/raw/master/logo.png" alt="logo"/>
</p>

## Description

Recyle your clipboard selections with greenclip and don't waste your time anymore
to reselect things other and other.

**Purpose :**
Keeps track of your history of selections to quickly switch between them


**Installation :**

1. It's a static binary so drop it anywhere in your $PATH env 
   ```wget https://raw.githubusercontent.com/erebe/greenclip/master/greenclip ```

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

