{**
 *  Copyright 2011 by Benjamin J. Land (a.k.a. BenLand100)
 *
 *  This file is part of ScratchCode.
 *
 *  ScratchCode is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  ScratchCode is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with ScratchCode. If not, see <http://www.gnu.org/licenses/>.
 *}
 
{$mode objfpc}

program maze_solver;

type Tpoint = record
  x,y:integer;
end;

type TpointArray = array of TPoint;

type TpointArrayArray = array of TpointArray;

type TboolGrid = array of array of boolean;

type TghettoStack = record
  passed,my_path: TpointArray;
  x,y:integer;
end;

type Tmaze = record
  tiles: TboolGrid;
  width,height: integer;
  start: Tpoint;
  finish: Tpoint;
end;

function readString(var f: file of char): string;
var
  c: char;
begin
  result:= '';
  Read(f,c);
  while c <> chr(10) do
  begin
    result:= result + c;
    Read(f,c);
  end;
end;

function ParseMaze(filename: string): Tmaze;
var
  tiles: TboolGrid;
  f: file of char;
  c: char;
  i,w,h,x,y: integer;
begin
  Assign(f,filename);
  Reset(f);
  val(readString(f),w,i);
  val(readString(f),h,i);
  SetLength(tiles,w,h);
  x:= 0;
  y:= 0;
  while y < h do
  begin
    Read(f,c);
    case c of
      '0': begin
        tiles[x][y]:= false;
        inc(x);
      end;
      '1': begin
        tiles[x][y]:= true;
        inc(x);
      end;
      '3': begin
        result.start.x:= x;
        result.start.y:= y;
        tiles[x][y]:= true;
        inc(x);
      end;
      '4': begin
        result.finish.x:= x;
        result.finish.y:= y;
        tiles[x][y]:= true;
        inc(x);
      end;
    end;
    if x = w then begin x:= 0; inc(y); end;
  end;
  result.width:= w;
  result.height:= h;
  result.tiles:= tiles;
end;

function notPassed(passed: TpointArray; x,y: integer): boolean;
var
  i: integer;
begin
  result:= true;
  for i:= length(passed) - 1 downto 0 do
    if (passed[i].x = x) and (passed[i].y = y) then begin result:= false; exit; end;
end;

function flood(input: TboolGrid; sx,sy,dx,dy,w,h: integer): TpointArrayArray;
var
  i,off,top:integer;
  stack: array of TghettoStack;
  my_path,passed: TpointArray;
  x,y: integer;
begin
  SetLength(stack,1);
  SetLength(stack[0].passed,0);
  stack[0].x:= sx;
  stack[0].y:= sy;
  off:= 0;
  top:= 1;
  while length(stack) > 0 do
  begin
    dec(top);
    my_path:= stack[top].my_path;
    passed:= stack[top].passed;
    x:= stack[top].x;
    y:= stack[top].y;
    SetLength(stack,top);
    SetLength(my_path,Length(passed)+1);
    
    //write('following ('); write(x); write(','); write(y); write(') '); writeln(length(my_path));
    for i:= 0 to Length(passed) - 1 do
      my_path[i]:= passed[i];
    my_path[Length(passed)].x:= x;
    my_path[Length(passed)].y:= y;
    if (x = dx) and (y = dy) then
    begin
      write('found path! ');
      writeln(off);
      {for d:= 0 to length(my_path) do
      begin
        write(my_path[d].x);
        write(',');
        writeln(my_path[d].y);
      end;}
      SetLength(result,off+1);
      result[off]:= my_path;
      inc(off);
    end;
    if (y-1 >= 0) then if input[x][y-1] then if notPassed(my_path,x,y-1) then
    begin
      SetLength(stack,top+1);
      stack[top].passed:= my_path;
      stack[top].x:= x;
      stack[top].y:= y-1;
      inc(top);
    end;
    if (y+1 < h) then if input[x][y+1] then if notPassed(my_path,x,y+1) then
    begin
      SetLength(stack,top+1);
      stack[top].passed:= my_path;
      stack[top].x:= x;
      stack[top].y:= y+1;
      inc(top);
    end;
    if (x-1 >= 0) then if input[x-1][y] then if notPassed(my_path,x-1,y) then
    begin
      SetLength(stack,top+1);
      stack[top].passed:= my_path;
      stack[top].x:= x-1;
      stack[top].y:= y;
      inc(top);
    end;
    if (x+1 < w) then if input[x+1][y] then if notPassed(my_path,x+1,y) then
    begin
      SetLength(stack,top+1);
      stack[top].passed:= my_path;
      stack[top].x:= x+1;
      stack[top].y:= y;
      inc(top);
    end;
  end;
end;


procedure prune(var input: TboolGrid; x,y,w,h: integer);
var
  nx,ny,c: integer;
begin
  nx:= x;
  ny:= y;
  repeat
    x:= nx;
    y:= ny;
    input[x][y]:= false;
    c:= 0;
    if (y-1 >= 0) then if input[x][y-1] then
    begin
      c:= c + 1;
      ny:= y - 1;
    end;
    if (y+1 < h) then if input[x][y+1] then
    begin
      c:= c + 1;
      ny:= y + 1;
    end;
    if (x-1 >= 0) then if input[x-1][y] then
    begin
      c:= c + 1;
      nx:= x - 1;
    end;
    if (x+1 < w) then if input[x+1][y] then
    begin
      c:= c + 1;
      nx:= x + 1;
    end;
  until c <> 1;
  input[x][y]:= true;
end;

procedure thin(var input: TboolGrid; sx,sy,fx,fy,w,h: integer);
var
  x,y,c: integer;
begin
  for x:= 0 to w-1 do
    for y:= 0 to h-1 do
      if input[x][y] then
      begin
        c:= 0;
        if (y-1 >= 0) then if input[x][y-1] then c:= c + 1;
        if (y+1 < h) then if input[x][y+1] then c:= c + 1;
        if (x-1 >= 0) then if input[x-1][y] then  c:= c + 1;
        if (x+1 < w) then if input[x+1][y] then c:= c + 1;
        if c <= 1 then if ((x<>sx) or (y<>sy)) and ((x<>fx) or (y<>fy)) then prune(input,x,y,w,h);
      end;
end;

procedure benland100_solver(var jacks_path: TPointArray; var jills_routes: Integer; input: TboolGrid; start, finish: TPoint);
var
   all_paths: TpointArrayArray;
   i,max_pos,max_len: integer;
begin
  thin(input,start.x,start.y,finish.x,finish.y,length(input),length(input[0]));
  all_paths:= flood(input,start.x,start.y,finish.x,finish.y,length(input),length(input[0]));
  jills_routes:= Length(all_paths) - 1;
  max_pos:= 0;
  max_len:= length(all_paths[0]);
  for i:= 1 to jills_routes do
  begin
    if length(all_paths[i]) > max_len then
    begin
      max_len:= length(all_paths[i]);
      max_pos:= i;
    end;
  end;
  jacks_path:= all_paths[max_pos];
end;

var
  maze: Tmaze;
  jack: TPointArray;
  jills: integer;
begin
  maze:= ParseMaze('tester.dat');
  benland100_solver(jack,jills,maze.tiles,maze.start,maze.finish);
  writeln(length(jack));
  writeln(jills);
end.
