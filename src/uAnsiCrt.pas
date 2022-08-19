unit uAnsiCrt;

(******************************************************************************
  This unit is emulating classic Pascal's CRT unit text color management and
  cursor movement using ANSI Escape Code sequence. Keyboard input handling &
  screen windowing is not possible. For keyboard input, better use the FPC's
  Keyboard unit.

  ANSI Escape Code list taken from:
  https://en.wikipedia.org/wiki/ANSI_escape_code

  Inspired by an article at:
  http://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html

  Some more info from:
  - https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
  - https://misc.flogisoft.com/bash/tip_colors_and_formatting

  Todo list:
  • Non-blocking standard i/o reading to obtain return value of esc commands,
    so the WhereXY and ScreenSize procedures would work correctly.
  • Cursor styling and coloring (most consoles today ignores them though).

  (c) v.0.2 by Mr Bee aka @pak_lebah – 2 December 2018
 ******************************************************************************)

{$MODE OBJFPC}{$H+}{$J-}

interface

const
  // text styles
  tsResetAll     = 0;
  tsBold         = 1;  // bright
  tsDim          = 2;
  tsItalic       = 3;
  tsUnderline    = 4;
  tsBlink        = 5;
  tsOverline     = 6;
  tsInvert       = 7;
  tsHidden       = 8;
  tsStrike       = 9;
  tsDefault      = 20;
  tsNoBold       = 21;
  tsNoDim        = 22;
  tsNoItalic     = 0;  // 23; not working?
  tsNoUnderline  = 24;
  tsNoBlink      = 25;
  tsNoOverline   = 26;
  tsNoInvert     = 27;
  tsNoHidden     = 28;
  tsNoStrike     = 29;
  // foreground colors
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
  // CRT unit's colors
  Black          = 30; // 0;
  Blue           = 34; // 1;
  Green          = 32; // 2;
  Cyan           = 36; // 3;
  Red            = 31; // 4;
  Magenta        = 35; // 5;
  Brown          = 33; // 6;
  LightGray      = 37; // 7;
  DarkGray       = 90; // 8;
  LightBlue      = 94; // 9;
  LightGreen     = 92; // 10;
  LightCyan      = 96; // 11;
  LightRed       = 91; // 12;
  LightMagenta   = 95; // 13;
  Yellow         = 93; // 14;
  White          = 97; // 15;
  // ANSI table drawing chars
  tdTopLeft      = #27'(0l'#27'(B';  // ┌
  tdTopCenter    = #27'(0w'#27'(B';  // ┬
  tdTopRight     = #27'(0k'#27'(B';  // ┐
  tdMidLeft      = #27'(0t'#27'(B';  // ├
  tdMidCenter    = #27'(0n'#27'(B';  // ┼
  tdMidRight     = #27'(0u'#27'(B';  // ┤
  tdBottomLeft   = #27'(0m'#27'(B';  // └
  tdBottomCenter = #27'(0v'#27'(B';  // ┴
  tdBottomRight  = #27'(0j'#27'(B';  // ┘
  tdHorzLine     = #27'(0q'#27'(B';  // ─
  tdVertLine     = #27'(0x'#27'(B';  // │

{$IFDEF WINDOWS}
function EnableANSIMode: boolean;
{$ENDIF}

// CRT's screen clearance
procedure ClrScr;
procedure ClrEol;
procedure ClrLine;

// CRT's cursor mode and positioning
procedure CursorOn;
procedure CursorOff;
procedure BlinkOn;
procedure BlinkOff;
procedure GotoXY(const X, Y: integer);
// function  WhereX: integer;
// function  WhereY: integer;
// function  ScreenWidth: integer;
// function  ScreenHeight: integer;

// ANSI's cursor positioning
procedure MoveUp(const Lines: word = 1);
procedure MoveDown(const Lines: word = 1);
procedure MoveRight(const Cols: word = 1);
procedure MoveLeft(const Cols: word = 1);
procedure MoveLineUp(const Lines: word = 1);
procedure MoveLineDown(const Lines: word = 1);
procedure GotoLine(const Line: word = 1);
procedure GotoColumn(const Col: word = 1);
// procedure WhereXY(var X,Y: integer);
// procedure ScreenSize(var X,Y: integer);
procedure StoreCursorPosition(const useAlt: boolean = {$IFDEF DARWIN}true{$ELSE}false{$ENDIF});
procedure RestoreCursorPosition(const useAlt: boolean = {$IFDEF DARWIN}true{$ELSE}false{$ENDIF});

// ANSI's text styling and coloring
procedure SetAttribute(const Attr: word; const Text: string = '');
procedure SetColor(const Color: word; const Text: string = '');
procedure SetStyle(const Style: word; const Text: string = '');
procedure SetColors(const ForeColor, BackColor: word; const Text: string = '');
procedure SetAttributes(const Attrs: array of word; const Text: string = '');
procedure AReset(const ColorOnly: boolean = false);

// ANSI's rich coloring (not all console support rich colors)
procedure SetForeColor256(const Color: word; const Text: string = '');
procedure SetBackColor256(const Color: word; const Text: string = '');
procedure SetForeColorRGB(const R,G,B: word; const Text: string = '');
procedure SetBackColorRGB(const R,G,B: word; const Text: string = '');

// CRT's text styling and coloring
procedure TextMode(const Style: word; const Text: string = '');
procedure TextColor(const Color: word; const Text: string = '');
procedure TextBackground(const Color: word; const Text: string = '');

function ansiText(const Text: string; const Color: word): string;

// helper functions
function int2Str(const int: integer): string;
function str2Int(const txt: string): integer;

implementation

{$IFDEF WINDOWS}
uses
  Windows;

function EnableANSIMode: boolean;
begin
  result := false;
  // To Windows programmers, please complete this function... :)
  // 1. handle := GetStdHandle(STD_OUTPUT_HANDLE);
  // 2. GetConsoleMode(handle, mode);
  // 3. mode := mode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  // 4. result := SetConsoleMode(handle, mode);
end;
{$ENDIF}

function int2Str(const int: integer): string;
begin
  Str(int, result);
end;

function str2Int(const txt: string): integer;
var
  i,c: integer;
begin
  Val(txt,i,c);
  if c <> 0 then result := -0 else result := i;
end;

function readStdIn(var txt: string): boolean;
begin
  result := false;
  // this should be replaced by non-blocking i/o reading
  read(txt);
end;

procedure ClrScr;
begin
  write(#27'[2J'#27'[H');
end;

procedure ClrEol;
begin
  write(#27'[0K');
end;

procedure ClrLine;
begin
  write(#27'[2K');
end;

procedure CursorOn;
begin
  write(#27'[?25h');
end;

procedure CursorOff;
begin
  write(#27'[?25l');
end;

procedure BlinkOn;
begin
  write(#27'[?12h');
end;

procedure BlinkOff;
begin
  write(#27'[?12l');
end;

procedure MoveUp(const Lines: word = 1);
begin
  write(#27'[',lines,'A');
end;

procedure MoveDown(const Lines: word = 1);
begin
  write(#27'[',lines,'B');
end;

procedure MoveRight(const Cols: word = 1);
begin
  write(#27'[',cols,'C');
end;

procedure MoveLeft(const Cols: word = 1);
begin
  write(#27'[',cols,'D');
end;

procedure MoveLineDown(const Lines: word = 1);
begin
  write(#27'[',lines,'E');
end;

procedure MoveLineUp(const Lines: word = 1);
begin
  write(#27'[',lines,'F');
end;

procedure GotoColumn(const Col: word = 1);
begin
  write(#27'[',col,'G');
end;

procedure GotoXY(const X, Y: integer);
begin
  write(#27'[',y,';',x,'H');
end;

procedure GotoLine(const Line: word = 1);
begin
  write(#27'[',line,'d');
end;

procedure WhereXY(var X,Y: integer);
var
  s: string = '';
  p,l: integer;
begin
  x := -1; y := -1;  // –1 means 'unknown'
  write(#27'[6n');   // returns: \e[line;columnR (note the last 'R')
  readStdIn(s);
  if s = '' then exit;

  l := Length(s);
  p := Pos(';',s);
  if p <> 0 then
  begin
    y := str2Int(Copy(s,3,p-3));
    x := str2Int(Copy(s,p+1,l-p-1));
  end;
end;

procedure ScreenSize(var X,Y: integer);
var
  s: string = '';
  p,l: integer;
begin
  x := -1; y := -1;  // –1 means 'unknown'
  write(#27'[18t');  // returns: \e[8;line;columnt (note the last 't')
  readStdIn(s);
  if s = '' then exit;

  Delete(s,3,2);  // remove the '8;' value prefix
  l := Length(s);
  p := Pos(';',s);
  if p <> 0 then
  begin
    y := str2Int(Copy(s,3,p-3));
    x := str2Int(Copy(s,p+1,l-p-1));
  end;
end;

procedure StoreCursorPosition(const useAlt: boolean = {$IFDEF DARWIN}true{$ELSE}false{$ENDIF});
begin
  if useAlt then write(#27'7')
    else write(#27'[s');
end;

procedure RestoreCursorPosition(const useAlt: boolean = {$IFDEF DARWIN}true{$ELSE}false{$ENDIF});
begin
  if useAlt then write(#27'8')
    else write(#27'[u');
end;

procedure SetAttribute(const Attr: word; const Text: string = '');
begin
  write(#27'[',attr,'m');
  if text <> '' then
    // undo the attribute after the text
    case attr of
      tsBold         : write(text,#27'[',tsNoBold     ,'m');
      tsDim          : write(text,#27'[',tsNoDim      ,'m');
      tsItalic       : write(text,#27'[',tsNoItalic   ,'m');
      tsUnderline    : write(text,#27'[',tsNoUnderline,'m');
      tsBlink        : write(text,#27'[',tsNoBlink    ,'m');
      tsOverline     : write(text,#27'[',tsNoOverline ,'m');
      tsInvert       : write(text,#27'[',tsNoInvert   ,'m');
      tsHidden       : write(text,#27'[',tsNoHidden   ,'m');
      tsStrike       : write(text,#27'[',tsNoStrike   ,'m');
      30..37, 90.. 97: write(text,#27'[',fcDefault    ,'m');
      40..47,100..107: write(text,#27'[',bcDefault    ,'m');
      else             write(text,#27'[',tsDefault    ,'m');
    end;
end;

procedure SetColor(const Color: word; const Text: string = '');
begin
  SetAttribute(Color, Text);
end;

procedure SetStyle(const Style: word; const Text: string = '');
begin
  SetAttribute(Style, Text);
end;

procedure SetColors(const ForeColor, BackColor: word; const Text: string = '');
begin
  write(#27'[',foreColor,';',backColor,'m');
  if text <> '' then write(text,#27'[',fcDefault,';',bcDefault,'m');
end;

procedure SetAttributes(const Attrs: array of word; const Text: string = '');
var
  i,j: integer;
  s: string;
begin
  s := #27'[';
  j := High(attrs);
  for i := 0 to j-1 do s += int2Str(attrs[i])+';';
  write(s,attrs[j],'m');
  if text <> '' then write(text,#27'[',tsResetAll,'m');
end;

procedure AReset(const ColorOnly: boolean = false);
begin
  if ColorOnly then
    write(#27'[',fcDefault,';',bcDefault,'m')
  else
    write(#27'[0m');
end;

// ANSI's rich coloring
procedure SetForeColor256(const Color: word; const Text: string = '');
begin
  write(#27'[',fcRichColors,';5;',color,'m');
  if text <> '' then write(text,#27'[',fcDefault,'m');
end;

procedure SetBackColor256(const Color: word; const Text: string = '');
begin
  write(#27'[',bcRichColors,';5;',color,'m');
  if text <> '' then write(text,#27'[',bcDefault,'m');
end;

procedure SetForeColorRGB(const R,G,B: word; const Text: string = '');
begin
  write(#27'[',fcRichColors,';2;',r,';',g,';',b,'m');
  if text <> '' then write(text,#27'[',fcDefault,'m');
end;

procedure SetBackColorRGB(const R,G,B: word; const Text: string = '');
begin
  write(#27'[',bcRichColors,';2;',r,';',g,';',b,'m');
  if text <> '' then write(text,#27'[',bcDefault,'m');
end;

procedure TextMode(const Style: word; const Text: string = '');
begin
  if style in [0..29] then SetAttribute(style, text);
end;

procedure TextColor(const Color: word; const Text: string = '');
begin
  if color in [40..47, 100..107] then  // bc to fc
    SetAttribute(color-10, text)
  else
    SetAttribute(color, text);
end;

procedure TextBackground(const Color: word; const Text: string = '');
begin
  if color in [30..37, 90..97] then  // fc to bc
    SetAttribute(color+10, text)
  else
    SetAttribute(color, text);
end;

function ansiText(const Text: string; const Color: word): string;
begin
  result := #27'['+int2Str(Color)+'m';
  case Color of
    tsBold         : result += text+#27'['+int2str(tsNoBold)     +'m';
    tsDim          : result += text+#27'['+int2str(tsNoDim)      +'m';
    tsItalic       : result += text+#27'['+int2str(tsNoItalic)   +'m';
    tsUnderline    : result += text+#27'['+int2str(tsNoUnderline)+'m';
    tsBlink        : result += text+#27'['+int2str(tsNoBlink)    +'m';
    tsOverline     : result += text+#27'['+int2str(tsNoOverline) +'m';
    tsInvert       : result += text+#27'['+int2str(tsNoInvert)   +'m';
    tsHidden       : result += text+#27'['+int2str(tsNoHidden)   +'m';
    tsStrike       : result += text+#27'['+int2str(tsNoStrike)   +'m';
    30..37, 90.. 97: result += text+#27'['+int2str(fcDefault)    +'m';
    40..47,100..107: result += text+#27'['+int2str(bcDefault)    +'m';
    else             result += text+#27'['+int2str(tsDefault)    +'m';
  end;
end;

end.
