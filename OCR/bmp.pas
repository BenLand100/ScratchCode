unit bmp;
{$mode objfpc}{$H+}

interface
    uses Classes,Interfaces,SysUtils,FPImgCanv,FPImage,graphtype,IntfGraphics,graphics;
    type 
        tRGB = packed record
            B, G, R, A: Byte;
        end;
        tRGBArray = array of tRGB;
        Tbmp = record
            data: array of tRGB;
            width,height: integer;
        end;
    function ReadBMP(path: string): Tbmp;
    
implementation

    function ReadBMP(path: string): Tbmp;
    var
      LazIntf : TLazIntfImage;
      RawImageDesc : TRawImageDescription;
      data: array of tRGB;
    begin
      if FileExists(path) then
      begin;
        LazIntf := TLazIntfImage.Create(0,0);
        RawImageDesc.Init_BPP32_B8G8R8_BIO_TTB(LazIntf.Width,LazIntf.Height);
        LazIntf.DataDescription := RawImageDesc;
        LazIntf.LoadFromFile(path);
        Result.width := LazIntf.Width;
        Result.height := LazIntf.Height;
        SetLength(data,LazIntf.Width*LazIntf.Height);
        Move(LazIntf.PixelData[0],data[0],LazIntf.Width*LazIntf.Height*sizeOf(tRGB));
        Result.data:= data;
        LazIntf.Free;
      end;
    end;
    
end.

{

    function Read8(var f: file of byte): integer;
    var
        b: byte;
    begin
        read(f,b);
        result:= b;
    end;

    function Read16(var f: file of byte): integer;
    var
        b: byte;
    begin
        read(f,b);
        result:= b;
        read(f,b);
        result:= result or (b shl 8);
    end;

    function Read24(var f: file of byte): integer;
    var
        b: byte;
    begin
        read(f,b);
        result:= b;
        read(f,b);
        result:= result or (b shl 8);
        read(f,b);
        result:= result or (b shl 16);
    end;

    function Read32(var f: file of byte): integer;
    var
        b: byte;
    begin
        read(f,b);
        result:= b;
        read(f,b);
        result:= result or (b shl 8);
        read(f,b);
        result:= result or (b shl 16);
        read(f,b);
        result:= result or (b shl 24);
    end;
    
function ReadBmp(path: string): Tbmp;
var
    offset,bits,i,c,px,len: integer;
    f: file of byte;
begin
    Assign(f,path);
    Reset(f);
    seek(f,10);
    offset:= read32(f);
    read32(f);
    result.width:= read32(f);
    result.height:= read32(f);
    read16(f);
    bits:= read16(f) div 8;
    seek(f,	offset);
    i:= 0;
    len:= result.width * result.height;
    SetLength(result.data,len);
    if result.height < 0 then
    begin
        while i < len do
        begin
            px:= 0;
            for c:= 1 to bits do
            begin
                px:= px + read8(f);
            end;
            if bits = 4 then
                px:= px div 3
            else
                px:= px div bits;
            if px <> 0 then
                result.data[i] := 1;
            inc(i);
        end;
    end else
    begin
        while i < len do
        begin
            px:= 0;
            for c:= 1 to bits do
            begin
                px:= px + read8(f);
            end;
            if bits = 4 then
                px:= px div 3
            else
                px:= px div bits;
            if px <> 0 then
                result.data[i mod result.width + (result.height - (i div result.width) - 1) * result.width] := 1;
             inc(i);
        end;
    end;
    Close(f);
end;
}
