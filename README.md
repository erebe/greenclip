<p align="center">
  <img src="https://github.com/erebe/greenclip/raw/master/logo.png" alt="logo"/>
</p>

## Description

Recyle your clipboard selections with greenclip and don't waste your time anymore
to reselect things other and other.

**Installation :**

1. It's a static binary so drop it anywhere in your $PATH env

2. Nothing more to do, go see how to use it


**Usage :**

Greenclip is intended to be use with [rofi](https://github.com/DaveDavenport/rofi)

1. Spawn the daemon
```
greenclip daemon
```

2. When ever you need to get your selections history
```
rofi -modi "clipboard:greenclip print" -show clipboard
```

3. The entry that you have selected will be in your clipboard now



**Compilation :**

1. Get [stack](https://docs.haskellstack.org/en/stable/README/) for Haskell

2. stack init && stack install

