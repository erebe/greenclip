<p align="center">
  <img src="https://github.com/erebe/greenclip/raw/master/logo.png" alt="logo"/>
</p>

## Description

Recyle your clipboard selections with greenclip and don't waste your time anymore
to reselect things other and other.

**Purpose:**
Keeps track of your history of selections to quickly switch between them

**Demo:**  <a href="https://www.youtube.com/watch?v=Utk-9Gy8H3w">Video Link</a>
<br/>
<a href="https://www.youtube.com/watch?v=4IycORAdW9M">Video made by @gotbletu</a>

**Features:**
  + Integrated with [rofi](https://github.com/DaveDavenport/rofi)
  + Permanently set some selections to added at the end (set `staticHistoryPath = your/file/with/static/entries` in the config file)
  + Merge X Primary selection with clipboard selection (set `usePrimarySelectionAsInput = True` in the config file)

## Installation

1. It's a static binary so drop it anywhere in your $PATH env

```wget https://github.com/erebe/greenclip/releases/download/2.1/greenclip```

Alternatively if you are using Archlinux you can install the package from AUR

``pacman -S rofi-greenclip``

PS: If you want, you can add a permanent list of selections to be added to your current history. Go see the config file


## Usage

Greenclip is intended to be used with [rofi](https://github.com/DaveDavenport/rofi)

1. Spawn the daemon ``` greenclip daemon ```
2. When ever you need to get your selections history ``` rofi -modi "clipboard:greenclip print" -show clipboard -run-command '{cmd}' ```
3. The entry that you have selected will be in your clipboard now
4. Configuration file can be found in ```.config/greenclip.cfg```

## Building the project

1. Get [stack](https://docs.haskellstack.org/en/stable/README/) for Haskell
2. stack init && stack install

or you can look at the .travis.yml file
