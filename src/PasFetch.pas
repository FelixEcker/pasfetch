{$mode delphi}
program PasFetch;

{
    REFERENCES:
    https://forum.lazarus.freepascal.org/index.php?topic=54110.0
    https://www.freepascal.org/docs-html/rtl/dos/getenv.html

    (i do not claim copyright. all rights go to the original owners)
}

uses Classes, Dos, IniFiles, Logos, Math, Process, StrUtils, SysUtils, Types, uAnsiCrt;

var
    FConfig: TIniFile;
    FSpacing: String;
    FPrintColor: boolean;
    FUserAtMachine: boolean;
    FOSName: String;
    FWantedInfos: TStringDynArray;
    FInfos: TStringDynArray;
    FLongest: Integer;
    FTmpFile: TextFile;
    FLogo: TStringDynArray;
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

function UName(const param: String): String;
var
    res: String;
begin
    result := 'Not Found';
    if (RunCommand('uname', [param], res)) then
        exit(StringReplace(res, sLineBreak, '', [rfReplaceAll]));
end;

function CPUString: String;
var
    s: String;
    split: TStringDynArray;
begin
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
end;

function MemoryUsageString: String;
var
    s, total: String;
    free: Integer;
    split: TStringDynArray;
begin
    AssignFile(FTmpFile, '/proc/meminfo');
    Reset(FTmpFile);

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
end;

function GetOsName: String;
var
    s, res: String;
    split: TStringDynArray;
begin
    AssignFile(FTmpFile, '/etc/os-release');
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
end;

{ Pads a String to given length using spaces on the right }
function PadToLength(input: String; const len: Integer): String;
var
    padding: String;
    i: Integer;
begin
    padding := '';
    for i := 0 to len-Length(input)-1 do
        padding := padding + ' ';

    exit(input+padding);
end;

begin
    if (ParamStr(1) = '-h') or (ParamStr(1) = '--help') then
    begin
        writeln('pasfetch - System Information Fetcher written in Pascal.');
        writeln();
        writeln('Usage: pasfetch');
        writeln('Config: $HOME/.config/pasfetch/config.ini (created on first run)');
        writeln('Supported Infos:');
        writeln('- All Environment Variables');
        writeln('- fl:OS     Operating System');
        writeln('- fl:KERNEL Kernel Version');
        writeln('- fl:UPTIME Systems Uptime');
        writeln('- fl:MEM    Memory Usage');
        halt;
    end;

    FSpacing := '                   ';
    FOSName := GetOsName();
    FLogo := SplitString(Logos.GetLogo(FOSName), sLineBreak);

    // All this should be loaded from a config    
    FConfig := TIniFile.Create(GetEnv('HOME')+'/.config/pasfetch/config.ini');
    if not FileExists(GetEnv('HOME')+'/.config/pasfetch/config.ini') then
    begin
        writeln('No config File present, creating...');
        FConfig.WriteBool('PASFETCH', 'color', True);
        FConfig.WriteBool('PASFETCH', 'useratmachine', True);
        FConfig.WriteString('PASFETCH', 'INFOS', 'fl:OS fl:KERNEL env:SHELL');
        writeln('Created '+GetEnv('HOME')+'/.config/pasfetch/config.ini');
    end;

    FPrintColor := FConfig.ReadBool('PASFETCH', 'color', True);
    FUserAtMachine := FConfig.ReadBool('PASFETCH', 'useratmachine', True);
    FWantedInfos := SplitString(FConfig.ReadString('PASFETCH', 'INFOS', 'fl:OS fl:KERNEL env:SHELL'), ' ');
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
            else if (tmp[1] = 'KERNEL') then FInfos[i] := UName('-r'); 
        end;

       if (Length(tmp[1]) > FLongest) then FLongest := Length(FWantedInfos[i]);
    end;

    // Write Information to console
    for i := 0 to max(Length(FInfos), Length(FLogo))-1 do
    begin
        // OS Art
        if (i < Length(FLogo)) then
        begin
            if FPrintColor then TextColor(Logos.GetColor(FOsName));
            write(FLogo[i]);
            if FPrintColor then AReset; // NormVideo resets the Color back to default
        end
        else
            write(FSpacing);

        // Information
        if (i < Length(FWantedInfos)) then
        begin
            if FPrintColor then TextColor(Logos.GetColor(FOsName));
            write('     '+PadToLength(
                SplitString(FWantedInfos[i]+':', ':')[1], FLongest));
            if FPrintColor then AReset;

            write(' '+FInfos[i]);
        end;
        
        writeln();
    end;

    writeln();
end.