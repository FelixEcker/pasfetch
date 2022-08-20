# pasfetch
A System fetch program written in Pascal.

## Installation
pasfetch is on the [AUR](https://aur.archlinux.org/packages/pasfetch). You can install it using yay: `yay -S pasfetch` <br>
Alternatively you can clone the AUR package and install it manually using makepkg.

## Building
pasfetch is built using the Free Pascal Compiler of version 3.2.2 or higher. When executed, the `build.sh` compiles all the required sources into a binary.

## Configuration
pasfetch stores its configuration ins $HOME/.config/pasfetch/config.ini. If it does not exist, it will be created by pasfetch.
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
```
fcBlack        = 30;
  fcRed          = 31;
  fcGreen        = 32;
  fcBrown        = 33;
  fcBlue         = 34;
  fcMagenta      = 35;
  fcCyan         = 36;
  fcLightGray    = 37;
  fcRichColors   = 38;  // requires additional parameter(s)
  fcDefault      = 39;
  fcDarkGray     = 90;
  fcLightRed     = 91;
  fcLightGreen   = 92;
  fcYellow       = 93;
  fcLightBlue    = 94;
  fcLightMagenta = 95;
  fcLightCyan    = 96;
  fcWhite        = 97;
  // background colors
  bcBlack        = 40;
  bcRed          = 41;
  bcGreen        = 42;
  bcBrown        = 43;
  bcBlue         = 44;
  bcMagenta      = 45;
  bcCyan         = 46;
  bcLightGray    = 47;
  bcRichColors   = 48;  // requires additional parameter(s)
  bcDefault      = 49;
  bcDarkGray     = 100;
  bcLightRed     = 101;
  bcLightGreen   = 102;
  bcYellow       = 103;
  bcLightBlue    = 104;
  bcLightMagenta = 105;
  bcLightCyan    = 106;
  bcWhite        = 107;
```


## Third Party Software
pasfetch utilises the uAnsiCrt unit by [Mr Bee aka @pak_lebah](https://github.com/pakLebah). Find it [here](https://gist.github.com/pakLebah/c5e2bbd0b93c863b2122660111db68d1).

## TODO
* Find ways to reduce binary size. I feel like 1 MB is a little to large even with smart linking and such