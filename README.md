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

### Possible Information Points
* Any ENV variable
* fl:MEM Memory usage (used/total kB)
* fl:OS The Operating System name
* fl:KERNEL The Kernel Version
* fl:UPTIME The systems uptime

*To specify an ENV variable add `env:` followed by the name of the variable. `fl` stands for file. Any point which has the `fl` prefix is fetched from a file or a shell command, this is only important for the program to know how to get the information.*