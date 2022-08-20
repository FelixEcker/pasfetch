unit Logos;

interface
    uses uAnsiCrt;

    function GetLogo(const os: String): String;
    function GetColor(const os: String): Byte;

    // LOGOS ARE FIXE WIDTH OF 
    const
        ALPINE = '    /\ /\     ' + LineEnding +
			     '   /  \  \    ' + LineEnding +
			     '  /    \  \   ' + LineEnding +
			     ' /      \  \  ' + LineEnding +
			     '/        \  \ ' + LineEnding +
			     '          \  \';
        ALPINE_COLOR = BLUE;
                 {**************}
        ARCH =   '      /\      ' + LineEnding +
			     '     /  \     ' + LineEnding + 
			     '    /\   \    ' + LineEnding + 
			     '   /      \   ' + LineEnding +
			     '  /   ,,   \  ' + LineEnding +
			     ' /   |  |  -\ ' + LineEnding +
			     '/_-''''    ''''-_\';
        ARCH_COLOR = CYAN;
                    {***************}
        ARCH_BANG = '          ____ ' + LineEnding +
			        '      /\ /   /' + LineEnding +
			        '     /  /   / ' + LineEnding +
			        '    /   / /   ' + LineEnding +
			        '   /   /_/\   ' + LineEnding +
			        '  /   __   \  ' + LineEnding +
			        ' /   /_/\   \ ' + LineEnding +
			        '/_-''''    ''''-_\';
        ARCH_BANG_COLOR = CYAN;
                    {**************}
        ARCO =      '      /\      ' + LineEnding +
			        '     /  \     ' + LineEnding +
			        '    / /\ \    ' + LineEnding +
			        '   / /  \ \   ' + LineEnding +
			        '  / /    \ \  ' + LineEnding +
			        ' / / _____\ \ ' + LineEnding +
			        '/_/  `----.\_\';
        ARCO_COLOR = BLUE;
                    {**************}
        ARTIX = 
			        '      /\       ' + LineEnding +
			        '     /  \      ' + LineEnding +
			        '    /`''.,\    ' + LineEnding +
			        '   /     '',   ' + LineEnding +
			        '  /      ,`\   ' + LineEnding +
			        ' /   ,.''`.  \ ' + LineEnding +
			        '/.,''`     `''.\ ';
        ARTIX_COLOR = CYAN;
                    {***************}
        DEBIAN =    '  _____  ' + LineEnding +
                    ' /  __ \ ' + LineEnding +
                    '|  /    |' + LineEnding +
                    '|  \___- ' + LineEnding +
                    '-_       ' + LineEnding +
                    '  --_    ' + LineEnding +
                    '         ';
        DEBIAN_COLOR = RED;
                    {****************}
        ARCH7 =     ' _______        ' + LineEnding +
                    '|____   \ \     ' + LineEnding +
                    '    / /  \      ' + LineEnding +
                    '   / /__\ \     ' + LineEnding +
                    '  / /____\ \    ' + LineEnding +
                    ' /_/      \_\   ';
        ARCH7_COLOR = CYAN;
                    {****************}
        ELEMENTARY =
                    '  _______       ' + LineEnding +
                    ' / ____  \      ' + LineEnding +
                    '/  |  /  /\     ' + LineEnding +
                    '|__\ /  / |     ' + LineEnding +
                    '\   /__/  /     ' + LineEnding +
                    ' \_______/      ';
        ELEMENTARY_COLOR = CYAN;
                    {****************}
        GENTOO =    '   _-----_      ' + LineEnding +
                    '  (       \     ' + LineEnding +
                    '  \    0   \    ' + LineEnding +
                    '   \        )   ' + LineEnding +
                    '   /      _/    ' + LineEnding +
                    '  (     _-      ' + LineEnding +
                    '  \____-        ';
        GENTOO_COLOR = MAGENTA;
                    {****************}
        MINT =      '  _____________ ' + LineEnding +
                    ' |_            \' + LineEnding +
                    '  |  | _____  | ' + LineEnding +
                    '  |  | | | |  | ' + LineEnding +
                    '  |  | | | |  | ' + LineEnding +
                    '  |  \_____/  | ' + LineEnding +
                    '  \___________/ ';
        MINT_COLOR = GREEN;
                    {****************}
        UBUNTU =    '          _     ' + LineEnding +
                    '      ---(_)    ' + LineEnding +
                    '  _/  ---  \    ' + LineEnding +
                    ' (_) |   |      ' + LineEnding +
                    '   \  --- _/    ' + LineEnding +
                    '      ---(_)    ';
        UBUNTU_COLOR = RED;

    var
        FOverrideColor: byte;  // 0 = do not override color.

implementation

function GetLogo(const os: String): String;
begin
    case os of
        '"Arch Linux"':       exit(ARCH);
        '"Alpine Linux"':     exit(ALPINE);
        '"Arch bang Linux"':  exit(ARCH_BANG);
        '"ArcoLinux"':        exit(ARCO);
        '"Artix Linux"':      exit(ARTIX);
        '"Debian GNU/Linux"': exit(DEBIAN);
        '"Arch7"':            exit(ARCH7);
        '"elementary OS"':    exit(ELEMENTARY);
        '"Gentoo"':           exit(GENTOO);
        '"Linux Mint"':       exit(MINT);
        '"Ubuntu"':           exit(UBUNTU);
    end;
end;

function GetColor(const os: String): Byte;
begin
    if (FOverrideColor <> 0) then exit(FOverrideColor);
    case os of
        '"Arch Linux"':       exit(ARCH_COLOR);
        '"Alpine Linux"':     exit(ALPINE_COLOR);
        '"Arch bang Linux"':  exit(ARCH_BANG_COLOR);
        '"ArcoLinux"':        exit(ARCO_COLOR);
        '"Artix Linux"':      exit(ARTIX_COLOR);
        '"Debian GNU/Linux"': exit(DEBIAN_COLOR);
        '"Arch7"':            exit(ARCH7_COLOR);
        '"elementary OS"':    exit(ELEMENTARY_COLOR);
        '"Gentoo"':           exit(GENTOO_COLOR);
        '"Linux Mint"':       exit(MINT_COLOR);
        '"Ubuntu"':           exit(UBUNTU_COLOR);
    end;
end;

end.