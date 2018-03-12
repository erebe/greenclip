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


## FAQ

Q. Greenclip does not copy selection !

A. Greenclip cannot run alongside with other clipboard manager. If you have already one activated (there is one by default in KDE), you have first to disable it before trying to run greenclip

---------

Q. I cannot paste images !

A. You can use the 3.0-beta of greenclip in order to get support for selection of small images (~ <500 kb)

----------

Q. I want to blacklist some applications !

A. Use the 3.0-beta of greenclip and you will find in your config file a section for blacklisting applications.
   You can run greenclip daemon in a terminal to find what is the name of the application who has the current selection.
   Be aware that a lot of application does not name their clipboard process/window, so it will be empty most of the time.
   Be aware also, that if you use an electron application (like slack i.e) you will get a generic name like "Chromium clipboard"
   
----------

Q. I want to paste selection directly after selecting them !

A. This is not possible as when you invoke rofi, you lose focus of your current window and there is no way to find it back (from greenclip point of view)
   Nonetheless, you can emulate the feature with xdotool `rofi -modi "clipboard:greenclip print" -show clipboard -run-command '{cmd}' ; sleep 0.5; xdotool type (xclip -o -selection clipboard)`

----------

Q. I want to thank you for greenclip !

A. Give a star to this repository
