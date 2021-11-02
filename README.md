<p align="center">
  <img src="https://github.com/erebe/greenclip/raw/master/logo.png" alt="logo"/>
</p>

## Description

Recycle your clipboard selections with greenclip and don't waste your time anymore
to reselect things over and over.

**Purpose:**
Keeps track of your history of selections to quickly switch between them

**Demo:**  <a href="https://www.youtube.com/watch?v=Utk-9Gy8H3w">Video Link</a>
<br/>
<a href="https://www.youtube.com/watch?v=4IycORAdW9M">Video made by @gotbletu</a>

**Features:**
  + Integrated with [rofi](https://github.com/DaveDavenport/rofi)
  + Permanently set some selections to added at the end (set `static_history = []` in the config file)
  + Merge X Primary selection with clipboard selection (set `use_primary_selection_as_input = true` in the config file)
  + Blacklist some applications (see `I want to blacklist some applications !` in the FAQ section)
  + Copy small images (you can disable it in the config)

## Installation

- It's a static binary so drop it anywhere in your $PATH env

```wget https://github.com/erebe/greenclip/releases/download/v4.2/greenclip```


- Alternatively if you are using Archlinux you can install the package from AUR

``yay rofi-greenclip``

PS: If you want, you can add a permanent list of selections to be added to your current history. Go see the config file

Configuration file can be found at: 
```
❯ cat ~/.config/greenclip.toml

[greenclip]
  history_file = "/home/erebe/.cache/greenclip.history"
  max_history_length = 50
  max_selection_size_bytes = 0
  trim_space_from_selection = true
  use_primary_selection_as_input = false
  blacklisted_applications = []
  enable_image_support = true
  image_cache_directory = "/tmp/greenclip"
  static_history = [
 '''¯\_(ツ)_/¯''',
]
```

## Usage

Greenclip is intended to be used with [rofi](https://github.com/DaveDavenport/rofi)

1. Spawn the daemon ``` greenclip daemon ```
2. When ever you need to get your selections history ``` rofi -modi "clipboard:greenclip print" -show clipboard -run-command '{cmd}' ```
3. The entry that you have selected will be in your clipboard now
4. Configuration file can be found in ```.config/greenclip.cfg```

## Migrating from 2.x version to 3.x one

1. Kill all greenclip process already running
2. Delete your old config file and old cache file (in ~/.cache/greenclip*)
3. Start the new version of greenclip

## Building the project

1. Get [stack](https://docs.haskellstack.org/en/stable/README/) for Haskell
2. stack init && stack install

or you can look at the .travis.yml file

## FAQ

Q. **Greenclip does not copy selection !**

A. Greenclip cannot run alongside other clipboard manager. If you have already one activated (there is one by default in KDE), you have first to disable it before trying to run greenclip

---------

Q. **I cannot paste images !**

A. You can only keep in your history small images  (~ <500 kb). I disabled the support for bigger images as it will turn your CPU into a heater for winter.

----------

Q. **I want to blacklist some applications !**

A. There is in your config file a section for blacklisting applications.
   You can run greenclip daemon in a terminal to find what is the name of the application who has the current selection.
   Be aware that a lot of application does not name their clipboard process/window, so it will be empty most of the time.
   Be aware also, that if you use an electron application (like slack i.e) you will get a generic name like "Chromium clipboard"

----------

Q. **I want to paste selection directly after selecting them !**

A. This is not possible as when you invoke rofi, you lose focus of your current window and there is no way to find it back (from greenclip point of view)

   Nonetheless, you can emulate the feature with xdotool `rofi -modi "clipboard:greenclip print" -show clipboard -run-command '{cmd}' ; sleep 0.5; xdotool type $(xclip -o -selection clipboard)`

   or look at this issue for a more complete solution https://github.com/erebe/greenclip/issues/27

----------

Q. **I got a strange behavior when clearing the clipboard - greenclip clear**

A. The command is not doing what it is supposed to do, if greenclip's daemon is running. You need to stop it first.

   Use this command to clear the history `pkill greenclip && greenclip clear && greenclip daemon &`
   
   For more information regarding why, refer to https://github.com/erebe/greenclip/issues/34 

----------

Q. **I need multi-line text in staticHistory**

A. https://github.com/erebe/greenclip/issues/78

----------

Q. **I don't want to use rofi !**

A. You can also use greenclip with [dmenu](http://tools.suckless.org/dmenu) or [fzf](https://github.com/junegunn/fzf). Example usage:

   `greenclip print | grep . | dmenu -i -l 10 -p clipboard | xargs -r -d'\n' -I '{}' greenclip print '{}'`

   `greenclip print | grep . | fzf -e | xargs -r -d'\n' -I '{}' greenclip print '{}'`

----------

Q. **I want to thank you for greenclip !**

A. Give a star to this repository
