program blobber;
{$mode objfpc}{$H+} 

uses Classes,SysUtils,Math,bmp;

type
  TGradientCalc = function(color1,color2:tRGB): integer;
  TBlobberData = record
    w,h,numblobs: integer;
    blobbed: array of integer;
    blobcount: array of integer;
    blobcolor: tRGBArray;
  end;
  tLab = record
    L,a,b: real;
  end;

function AvgColors(color1:tRGB; weight1: integer; color2: tRGB; weight2: integer): tRGB;
begin
    result.r:= (color1.r * weight1 + color2.r * weight2) div (weight1 + weight2);
    result.g:= (color1.g * weight1 + color2.g * weight2) div (weight1 + weight2);
    result.b:= (color1.b * weight1 + color2.b * weight2) div (weight1 + weight2);
end;

function blobber(colors: tRGBArray; w,h: integer; func: TGradientCalc; GradientMax: integer): TBlobberData;
var
  up, left: boolean;
  len,nextblob,thisblob,lastblob,i,j,used: integer;
  blobbed,blobcount,stack: array of integer;
  blobcolor: tRGBArray;
begin
  len:= Length(colors);
  SetLength(blobbed,len);
  SetLength(blobcount,len);
  SetLength(blobcolor,len);
  SetLength(stack,len);
  nextblob:= 0;
  for i:= 0 to len-1 do
  begin
    if i >= w then
      up:= func(colors[i],colors[i-w]) <= GradientMax
    else
      up:= false;
    if i mod w <> 0 then
      left:= func(colors[i],colors[i-1]) <= GradientMax
    else
      left:= false;
    if left and up then
    begin
      thisblob:= blobbed[i-w];
      blobbed[i]:= thisblob;
      blobcolor[thisblob]:= AvgColors(blobcolor[thisblob],blobcount[thisblob],colors[i],1);
      blobcount[thisblob]:= blobcount[thisblob] + 1;
      lastblob:= blobbed[i-1];
      if lastblob <> thisblob then
      begin
        used:= 1;
        stack[0]:= i-1;
        while used > 0 do
        begin
          used:= used - 1;
          j:= stack[used];
          if blobbed[j] = lastblob then
          begin
            blobbed[j]:= thisblob;
            if j >= w then if blobbed[j-w] = lastblob then begin stack[used]:= j-w; used:= used + 1; end;
            if j mod w <> 0 then if blobbed[j-1] = lastblob then begin stack[used]:= j-1; used:= used + 1; end;
            if (j+1) mod w <> 0 then if blobbed[j+1] = lastblob then  begin stack[used]:= j+1; used:= used + 1; end;
            if j < w*h-w then if blobbed[j+w] = lastblob then  begin stack[used]:= j+w; used:= used + 1; end;
          end;
        end;
        blobcolor[thisblob]:= AvgColors(blobcolor[thisblob],blobcount[thisblob],blobcolor[lastblob],blobcount[lastblob]);
        blobcount[thisblob]:= blobcount[thisblob] + blobcount[lastblob];
        blobcount[lastblob]:= 0;
        blobcolor[lastblob].r:= 0;
        blobcolor[lastblob].g:= 0;
        blobcolor[lastblob].b:= 0;
      end;
    end else if left then
    begin
      thisblob:= blobbed[i-1];
      blobbed[i]:= thisblob;
      blobcolor[thisblob]:= AvgColors(blobcolor[thisblob],blobcount[thisblob],colors[i],1);
      blobcount[thisblob]:= blobcount[thisblob] + 1;
    end else if up then
    begin
      thisblob:= blobbed[i-w];
      blobbed[i]:= thisblob;
      blobcolor[thisblob]:= AvgColors(blobcolor[thisblob],blobcount[thisblob],colors[i],1);
      blobcount[thisblob]:= blobcount[thisblob] + 1;
    end else
    begin
      blobbed[i]:= nextblob;
      blobcount[nextblob]:= 1;
      blobcolor[nextblob]:= colors[i];
      nextblob:= nextblob + 1;
    end;
  end;
  SetLength(blobcount,nextblob);
  SetLength(blobcolor,nextblob);
  result.w:= w;
  result.h:= h;
  result.blobbed:= blobbed;
  result.numblobs:= nextblob;
  result.blobcount:= blobcount;
  result.blobcolor:= blobcolor;
end;

function blobImage(data: TBlobberData): tRGBArray;
var
  i,len: integer;
begin
  len:= data.w*data.h;
  SetLength(result,len);
  for i:= 0 to len-1 do
    result[i]:= data.blobcolor[data.blobbed[i]];
end;

procedure RGBtoXYZ(color: tRGB; var X, Y, Z: real); inline;
var
    nr,ng,nb: real;
begin
    nr:= color.r / 255.0;
    ng:= color.g / 255.0;
    nb:= color.b / 255.0;
    if nr <= 0.04045 then nr:= nr / 12.92 else nr:= power((nr + 0.055)/1.055,2.4);
    if ng <= 0.04045 then ng:= ng / 12.92 else ng:= power((ng + 0.055)/1.055,2.4);
    if nb <= 0.04045 then nr:= nb / 12.92 else nb:= power((nb + 0.055)/1.055,2.4);
    X:= 0.4124*nr + 0.3576*ng + 0.1805*nb;
    Y:= 0.2126*nr + 0.7152*ng + 0.0722*nb;
    Z:= 0.0193*nr + 0.1192*ng + 0.9505*nb;
end;

function labmod(i: real): real; inline;
begin
    if i > power(0.206896552,3) then
        result:= power(i,0.333333333) 
    else 
        result:= 7.787037037*i + 0.137931034; 
end;

function ColortoLab(c: tRGB): tLab; inline;
var
    X,Y,Z,sum,Xn,Yn,Zn: real;
begin
    RGBtoXYZ(c,X,Y,Z);
    sum:= X + Y + Z;
    if sum = 0 then
    begin
        result.L:= 0;
        result.a:= 0;
        result.b:= 0;
        exit;
    end;
    Xn:= X / sum;
    Yn:= Y / sum;
    Zn:= Z / sum;
    result.L:= 116.0*labmod(y/yn) - 16.0;
    result.a:= 500.0*(labmod(x/xn)-labmod(y/yn));
    result.b:= 500.0*(labmod(y/yn)-labmod(z/zn));
end;

function LABContrast(rgb1,rgb2: tRGB): integer;
var
  Lab1, Lab2: tLab;
begin
  Lab1:= ColorToLab(rgb1);
  Lab2:= ColorToLab(rgb2);
  result:= round(abs(Lab1.L-Lab2.L)+abs(Lab1.a-Lab2.a)+abs(Lab1.b-Lab2.b));
end;

function RGBContrast(a,b: tRGB): integer;
begin
    result:= (a.r-b.r)*(a.r-b.r)+(a.g-b.g)*(a.g-b.g)+(a.b-b.b)*(a.b-b.b);
end;

{***DEMO CODE***}

var
    blobdata: TBlobberData;
    i:integer;
    tick: TDateTime;
    time,tol: integer;
    bitmap: tBMP;
begin
    if paramcount <> 3 then
    begin 
        Writeln('Usage: blobber infile outfile tolerance');
        exit;
    end;
    bitmap:= readbmp(paramstr(1));
    tol:= strtoint(paramstr(3));
    {writeln('Running time test...');
    tick:= now;
    for i:= 1 to 10 do
    begin
        blobdata:= blobber(bitmap.data,bitmap.width,bitmap.height,@RGBContrast,tol);
        blobImage(blobdata);
    end;
    time:= trunc((now - tick) * 24 * 60 * 60 * 1000);
    write('Blobbing took: ');
    writeln(time / 10.0);}
    blobdata:= blobber(bitmap.data,bitmap.width,bitmap.height,@LABContrast,tol);
    bitmap.data:= blobImage(blobdata);
    writebmp(paramstr(2),bitmap);
end.
