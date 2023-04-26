{$mode delphi}
program PasFetch;

uses Classes, Dos, IniFiles, Logos, Math, Process, StrUtils, SysUtils, Types, uAnsiCrt;

var
    FConfig: TIniFile;
    FSpacing: String;
    FPrintColor: boolean;
    FInfoLabelStyle: byte; // default tsBold
    FInfoTextStyle: byte;  // default tsResetAll
    FLogoStyle: byte;      // default tsResetAll
    FUserAtMachine: boolean;
    FOSName: String;
    FWantedInfos: TStringDynArray;
    FInfos: TStringDynArray;
    FLongest: Integer;
    FTmpFile: TextFile;
    FLogo: TStringDynArray;
    FFormatString: String;
    tmp: TStringDynArray;
    i: Integer;

function Uptime: String;
var
    res: String;
begin
    result := 'Not Found';
    if (RunCommand('uptime', ['-p'], res)) then
        exit(StringReplace(SplitString(res, 'up ')[1], sLineBreak, '', [rfReplaceAll]));
end;

function PkgCount: String;
var
    res: String;
begin
    result := 'Not Found';

    // Any pkg count query that requries a pipe isnt added because i cant figure out
    // how to do pipes in Pascal.
    if (FOSName = '"Arch Linux"') or (FOSName = '"Arch bang Linux"') 
    or (FOSName = '"ArcoLinux"') or (FOSName = '"Artix Linux"') or (FOSName = '"Arch7"') then
    begin
        if (RunCommand('pacman', ['-Qq'], res)) then
            exit(IntToStr(Length(SplitString(res, sLineBreak))-1));
    end
    else if (FOSName = '"Alpine Linux"') then
    begin
        if (RunCommand('grep', ['''P:''', '/lib/apk/db/installed'], res)) then
            exit(IntToStr(Length(SplitString(res, sLineBreak))-1));
    end
    else if (FOSName = '"Gentoo"') then
    begin
        if (RunCommand('qlist', ['-IRv'], res)) then
            exit(IntToStr(Length(SplitString(res, sLineBreak))-1));
    end;
end;

function UName(const param: String): String;
var
    res: String;
begin
    result := 'Not Found';
    if (RunCommand('uname', [param], res)) then
        exit(StringReplace(res, sLineBreak, '', [rfReplaceAll]));
end;

function CPUString: String;
{$IF defined(LINUX)}
var
    s: String;
    split: TStringDynArray;
{$ENDIF}
begin
  {$IF defined(LINUX)}
    AssignFile(FTmpFile, '/proc/cpuinfo');
    Reset(FTmpFile);
    while not eof(FTmpFile) do
    begin
        readln(FTmpFile, s);
        split := SplitString(SplitString(s, ':')[0], ' ');
        if (split[0]= 'model') then
        begin
            s := SplitString(s, ':')[1];
            exit(Copy(s, 2, Length(s)-1)); // Strip Leading space
        end;
    end;

    CloseFile(FTmpFile);
    exit('Not Found');
  {$ELSEIF defined(DARWIN)}
    if not (RunCommand('sysctl', ['-n', 'machdep.cpu.brand_string'], result)) then
      exit('Not Found');

    result := StringReplace(result, sLineBreak, '', [rfReplaceAll]);
  {$ENDIF}
end;

function MemoryUsageString: String;
{$IF defined(LINUX)}
var
    s, total: String;
    free: Integer;
    split: TStringDynArray;
{$ENDIF}
begin
  {$IF defined(LINUX)}
    AssignFile(FTmpFile, '/proc/meminfo');
    Reset(FTmpFile);

    total := '0';
    free := 0;

    while not eof(FTmpFile) do
    begin
        readln(FTmpFile, s);
        split := SplitString(s, ':');
        if (split[0] = 'MemTotal') then
        begin
            total := SplitString(StringReplace(split[1], ' ', '', [rfReplaceAll]), 'k')[0];
        end
        else if (split[0] = 'MemFree') then 
        begin
            free := StrToInt(SplitString(StringReplace(split[1], ' ', '', [rfReplaceAll]), 'k')[0]);
        end;
    end;

    CloseFile(FTmpFile);
    exit(IntToStr(StrToInt(total)-free)+' / '+total+' kB');
  {$ELSEIF defined(DARWIN)}
    exit('coming soon to macos');
  {$ENDIF}
end;

function GetOsName: String;
{$IF defined(LINUX)}
var
    s, res: String;
    split: TStringDynArray;
{$ENDIF}
begin
  {$IF defined(LINUX)}
    if FileExists('/etc/os-release') then
        AssignFile(FTmpFile, '/etc/os-release')
    else
        AssignFile(FTmpFile, '/etc/release');

    Reset(FTmpFile);

    res := '';
    while not eof(FTmpFile) do
    begin
        readln(FTmpFile, s);
        split := SplitString(s, '=');
        if (split[0] = 'NAME') then res := split[1];
    end;

    CloseFile(FTmpFile);
    exit(res);
  {$ELSEIF defined(DARWIN)}
    exit('MacOS');
  {$ENDIF}
end;

begin
    if (ParamStr(1) = '-h') or (ParamStr(1) = '--help') then
    begin
        writeln('pasfetch - System Information Fetcher written in Pascal.');
        writeln();
        writeln('Usage: pasfetch');
        writeln('Config: $XDG_CONFIG_HOME/pasfetch/config.ini (created on first run)');
        writeln('Supported Infos:');
        writeln('- All Environment Variables');
        writeln('- fl:OS     Operating System');
        writeln('- fl:KERNEL Kernel Version');
        writeln('- fl:UPTIME Systems Uptime');
        writeln('- fl:MEM    Memory Usage');
        writeln('- fl:CPU    CPU Model Name');
        writeln('- fl:PKGS   Package Count');
        halt;
    end;

    FSpacing := '              ';
    FOSName := GetOsName();
    FLogo := SplitString(Logos.GetLogo(FOSName), sLineBreak);

    FConfig := TIniFile.Create(GetEnv('HOME')+'/.config/pasfetch/config.ini');
    if not FileExists(GetEnv('HOME')+'/.config/pasfetch/config.ini') then
    begin
        writeln('No config File present, creating...');
        FConfig.WriteBool('PASFETCH', 'color', True);
        FConfig.WriteBool('PASFETCH', 'useratmachine', True);
        FConfig.WriteString('PASFETCH', 'INFOS', 'fl:OS fl:KERNEL env:SHELL');
        writeln('Created '+GetEnv('HOME')+'/.config/pasfetch/config.ini');
    end;

    FUserAtMachine := FConfig.ReadBool('PASFETCH', 'useratmachine', True);
    FWantedInfos := SplitString(FConfig.ReadString('PASFETCH', 'INFOS', 'fl:OS fl:KERNEL env:SHELL'), ' ');

    FPrintColor := FConfig.ReadBool('PASFETCH', 'color', True);
    Logos.FOverrideColor := FConfig.ReadInteger('PASFETCH', 'overridecolor', 0);
    FInfoLabelStyle := FConfig.ReadInteger('PASFETCH', 'infolabelstyle', tsBold);
    FInfoTextStyle := FConfig.ReadInteger('PASFETCH', 'infotextstyle', tsResetAll);
    FLogoStyle := FConfig.ReadInteger('PASFETCH', 'logostyle', tsResetAll);

    FConfig.Free;
    //['fl:OS', 'fl:KERNEL', 'env:SHELL', 'env:USER', 'fl:UPTIME', 'fl:MEM'];
    SetLength(FInfos, Length(FWantedInfos));

    // Get Information
    FLongest := 0;
    for i := 0 to Length(FInfos)-1 do
    begin    
        tmp := SplitString(FWantedInfos[i], ':');
        if (tmp[0] = 'env') then // Environment Variables
        begin
            FInfos[i] := GetEnv(tmp[1]);
            if (tmp[1] = 'USER') and FUserAtMachine then FInfos[i] := FInfos[i] + '@' + UName('-n');
        end
        else if (tmp[0] = 'fl') then // Information to be grabbed from files
        begin
            // Apparently cant do a "case of" with strings in delphi mode, sucks
            if (tmp[1] = 'MEM') then FInfos[i] := MemoryUsageString
            else if (tmp[1] = 'CPU') then FInfos[i] := CPUString
            else if (tmp[1] = 'OS') then FInfos[i] := FOSName
            else if (tmp[1] = 'UPTIME') then FInfos[i] := Uptime
            else if (tmp[1] = 'PKGS') then FInfos[i] := PkgCount
            else if (tmp[1] = 'KERNEL') then FInfos[i] := UName('-r'); 
        end;

       if (Length(tmp[1]) > FLongest) then FLongest := Length(FWantedInfos[i]);
    end;

    FFormatString := '%'+IntToStr(FLongest)+'s';

    // Write Information to console
    for i := 0 to max(Length(FInfos), Length(FLogo))-1 do
    begin
        // OS Art
        if (i < Length(FLogo)) then
        begin
            TextMode(FLogoStyle);
            if FPrintColor then TextColor(Logos.GetColor(FOsName));
            write(FLogo[i]);
            if FPrintColor then AReset; // NormVideo resets the Color back to default
        end
        else
            write(FSpacing);

        // Information
        if (i < Length(FWantedInfos)) then
        begin
            TextMode(FInfoLabelStyle);
            if FPrintColor then TextColor(Logos.GetColor(FOsName));
            write('     '+Format(FFormatString, 
                [SplitString(FWantedInfos[i]+':', ':')[1]]));
            if FPrintColor then AReset;

            TextMode(FInfoTextStyle);
            write(' '+FInfos[i]);
        end;
        
        TextMode(tsResetAll);
        writeln();
    end;

    writeln();
end.
