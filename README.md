# pasfetch
A System fetch program written in Pascal.

## Installation
pasfetch is on the [AUR](https://aur.archlinux.org/packages/pasfetch). You can install it using yay: `yay -S pasfetch` <br>
Alternatively you can clone the AUR package and install it manually using makepkg.

## Building
pasfetch is built using the Free Pascal Compiler of version 3.2.2 or higher. When executed, the `build.sh` compiles all the required sources into a binary.

## Configuration
pasfetch stores its configuration ins $XDG_CONFIG_HOME/pasfetch/config.ini. If it does not exist, it will be created by pasfetch.
The configuration has the following options:
* useratmachine = 1 or 0: Formats the `USER` point like `username@machinename`
* color = 1 or 0: Sets if pasfetch outputs with color
* INFOS: Sets what point should be displayed in the same order as given
* infolabelstyle: The Text Style for the Information Labels (e.g. CPU, KERNEL, SHELL)
* infotextstyle: The Text Style for the Information texts
* logostyle: The Text Style for the Logo
* overridecolor: What color to override the default color with.

*Put 0 or leave out if you wish to have the default color or styles*

### Possible Information Points
* Any ENV variable
* fl:MEM Memory usage (used/total kB)
* fl:OS The Operating System name
* fl:KERNEL The Kernel Version
* fl:UPTIME The systems uptime
* fl:CPU The CPU modelname

*To specify an ENV variable add `env:` followed by the name of the variable. `fl` stands for file. Any point which has the `fl` prefix is fetched from a file or a shell command, this is only important for the program to know how to get the information.*

### Colors
#### Foreground Colors
```
* Black         = 30
* Red           = 31
* Green         = 32
* Brown         = 33
* Blue          = 34
* Magenta       = 35
* Cyan          = 36
* Light Gray    = 37
* Default       = 39
* Dark Gray     = 90
* LightRed      = 91
* Light Green   = 92
* Yellow        = 93
* Light Blue    = 94
* Light Magenta = 95
* Light Cyan    = 96
* White         = 97
```

#### Background Colors
```
Black         = 40
Red           = 41
Green         = 42
Brown         = 43
Blue          = 44
Magenta       = 45
Cyan          = 46
Light Gray    = 47
Default       = 49
Dark Gray     = 100
Light Red     = 101
Light Green   = 102
Yellow        = 103
Light Blue    = 104
Light Magenta = 105
Light Cyan    = 106
White         = 107
```

### Styles
```
ResetAll     = 0
Bold         = 1 
Dim          = 2
Italic       = 3
Underline    = 4
Blink        = 5
Overline     = 6
Invert       = 7
Hidden       = 8
Strike       = 9
Default      = 20
NoBold       = 21
NoDim        = 22
NoUnderline  = 24
NoBlink      = 25
NoOverline   = 26
NoInvert     = 27
NoHidden     = 28
NoStrike     = 29
```

## Third Party Software
pasfetch utilises the uAnsiCrt unit by [Mr Bee aka @pak_lebah](https://github.com/pakLebah). Find it [here](https://gist.github.com/pakLebah/c5e2bbd0b93c863b2122660111db68d1).

## TODO
* Find ways to reduce binary size. I feel like 1 MB is a little to large even with smart linking and such
