////////////////////////////////////////////////////////////////////////////////
// File:       PegtopBufferStreams.pas
// Version:    1.00
// Date:       18 Apr 2005 created 1.00
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopBufferStream allows to read from and write to a memory buffer already
// existing when the stream is created. The size of the stream / buffer cannot
// be changed, Write cannot write data past the end of the buffer.
// TPegtopGlobalMemoryStream is a TPegtopBufferStream which is initialized by
// a handle to a global memory object, and allows to read from and write to such
// a global memory object.
// TPegtopMemorySliceStream is a  TPegtopBufferStream which is initialized by
// another memory stream and an offset, so it represents a slice of another
// memory stream without much overhead (no additional copying of data). When
// used as a slice of a TPegtopMemoryStream (which allows changing its size
// dynamically) it can become invalid, when memory is reallocated (when the
// size exceeds the capacity).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopBufferStreams;

interface

uses
  Windows, Classes;

type
  TPegtopBufferStream = class(TCustomMemoryStream)
  public
    constructor Create(var Buffer; const ASize: Longint);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TPegtopGlobalMemoryStream = class(TPegtopBufferStream)
  private
    FHandle: THandle;
  public
    constructor Create(AHandle: THandle);
    destructor Destroy; override;
    property Handle: THandle read FHandle;
  end;

  TPegtopMemorySliceStream = class(TCustomMemoryStream)
  public
    constructor Create(const AStream: TCustomMemoryStream; const APosition, ASize: Longint);
  end;

  EPegtopStreamLockFailed = class(EStreamError);

implementation

resourcestring
  PegtopStreamLockFailedMessage = 'Cannot unlock global memory object.';

////////////////////////////////////////////////////////////////////////////////
// TPegtopBufferStream
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopBufferStream.Create(var Buffer; const ASize: Longint);
begin
  SetPointer(Addr(Buffer), ASize);
end;

function TPegtopBufferStream.Write(const Buffer; Count: Longint): Longint;
var
  StartPos: Longint;
  EndPos: Longint;
  MaxPos: LongInt;
  P: ^Byte;
begin
  StartPos := Position;
  MaxPos := Size;
  if (StartPos >= 0) and (StartPos < MaxPos) then begin
    EndPos := Position + Count;
    if EndPos > MaxPos then Dec(Count, EndPos - MaxPos);
    P := Memory;
    Inc(P, StartPos);
    Move(Buffer, P^, Count);
    Result := Count;
  end
  else begin
    Result := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopGlobalMemoryStream
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopGlobalMemoryStream.Create(AHandle: THandle);
var
  Ptr: Pointer;
begin
  Ptr := GlobalLock(AHandle);
  if Ptr = NIL then raise EPegtopStreamLockFailed.Create(PegtopStreamLockFailedMessage);
  inherited Create(Ptr^, GlobalSize(AHandle));
  FHandle := AHandle;
end;

destructor TPegtopGlobalMemoryStream.Destroy;
begin
  if FHandle <> 0 then GlobalUnlock(FHandle);
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopMemorySliceStream
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopMemorySliceStream.Create(const AStream: TCustomMemoryStream; const APosition, ASize: Longint);
var
  StartAddr: ^Byte;
begin
  StartAddr := AStream.Memory;
  Inc(StartAddr, APosition);
  SetPointer(StartAddr, ASize);
end;

end.

(*
{== Unit ROMemStream
==================================================}
{: Implements a TStream descendent that gives read-only access to a
   memory block referenced by a pointer.
@author Dr. Peter Below
@desc   Version 1.0 created 20 November 2000<BR>
        Current revision 1.0<BR>
        Last modified       20 November 2000<P>
        Last review         17.04.2001<P>
   }
{======================================================================
}
Unit ROMemStream;
Interface

Uses Classes, Sysutils;

Type
  {: A class giving stream-style access (read-only) to a memory block
     referenced by a pointer. }
  TReadOnlyMemoryStream = Class( TStream )
  private
    FMemory: PChar;      { address of memory block }
    FPosition: Integer;  { current offset inside block, 0-based }
    FSize: Integer;      { size of memory block, in bytes }
  protected
    Procedure SetSize(NewSize: Longint); override;
  public
    Function Read(var Buffer; Count: Longint): Longint; override;
    Function Write(const Buffer; Count: Longint): Longint; override;
    Function Seek(Offset: Longint; Origin: Word): Longint; override;

    Constructor Create( aPointer: Pointer; aSize: Integer );

    property Memory: PChar read FMemory;
    property Size: Integer read FSize;
  End; { TReadOnlyMemoryStream }

  EROMemStreamError = class( EStreamError );

Implementation

ResourceString
{$IFDEF GERMAN}
  eStreamIsNotResizable =
    'TReadOnlyMemoryStream.SetSize: Die Größe dieses Streams kann '+
    'nicht geändert werden.';
  eStreamIsReadonly =
    'TReadOnlyMemoryStream.Write: Aus diesem Stream kann nur gelesen '+
    'werden.';
{$ELSE}
  eStreamIsNotResizable =
    'TReadOnlyMemoryStream.SetSize: The size of this stream cannot be
'+
    'changed.';
  eStreamIsReadonly =
    'TReadOnlyMemoryStream.Write: This stream is read-only.';
{$ENDIF}

{-- TReadOnlyMemoryStream.Create
--------------------------------------}
{: Stores the passed parameters
@Param aPointer is the address of the memory block to use
@Param aSize is the size of the memory block
@Precondition  aPointer <> nil, aSize > 0
@Desc The passed pointer remains property and responsibility of the
  caller, the object is only reading from the memory and will not free
  or reallocate it. The caller has to make sure the pointer stays
  valid for the lifetime of the object!
}{ Created 20.11.2000 by P. Below
-----------------------------------------------------------------------
}
Constructor TReadOnlyMemoryStream.Create( aPointer: Pointer; aSize:
Integer );
  Begin { Create }
    Assert( Assigned( aPointer ));
    Assert( aSize > 0 );

    inherited Create;
    FMemory   := aPointer;
    FSize     := aSize;
    FPosition := 0;
  End; { TReadOnlyMemoryStream.Create }

{-- TReadOnlyMemoryStream.SetSize
-------------------------------------}
{: Accessor for inherited property Size (write)
@Raises EROMemStreamError since the streams size cannot be changed
}{ Created 20.11.2000 by P. Below
-----------------------------------------------------------------------
}
Procedure TReadOnlyMemoryStream.SetSize( NewSize: Longint );
  Begin { SetSize }
    raise EROMemStreamError.Create( eStreamIsNotResizable );
  End; { TReadOnlyMemoryStream.SetSize }

{-- TReadOnlyMemoryStream.Read
----------------------------------------}
{: Reads bytes from the stream.
@Param Buffer is the receptacle for the read bytes
@Param Count is the number of bytes to read
@Returns the actual number of bytes read, which may be less than count
@Desc Copies the requested number of bytes into Buffer and increments
  the internal position accordingly. If less than count bytes remain
  until the end of stream only the bytes available are transferred.
}{ Created 20.11.2000 by P. Below
-----------------------------------------------------------------------
}
Function TReadOnlyMemoryStream.Read( var Buffer; Count: Longint ):
Longint;
  Var
    pCurrent: PChar;
  Begin { Read }
    If ( FPosition + count ) > FSize Then
      Result := FSize - FPosition
    Else
      Result := count;
    pCurrent := FMemory + FPosition;
    Move( pCurrent^, Buffer, Result );
    Seek( Result, soFromCurrent );
  End; { TReadOnlyMemoryStream.Read }

{-- TReadOnlyMemoryStream.Write
---------------------------------------}
{: Raises an exception since the stream is read-only.
@Raises EROMemStreamError
}{ Created 20.11.2000 by P. Below
-----------------------------------------------------------------------
}
Function TReadOnlyMemoryStream.Write( const Buffer; Count: Longint ):
Longint;
  Begin { Write }
    raise EROMemStreamError.Create( eStreamIsReadonly );
  End; { TReadOnlyMemoryStream.Write }

{-- TReadOnlyMemoryStream.Seek
----------------------------------------}
{: Moves the internal stream position
@Param Offset is the amount to move in the stream
@Param Origin defines where to start
@Returns the new stream position
@Desc Calculates and returns the new stream position. The position will
  be clipped to the available range, trying to seek beyond the end of
  stream will not raise an exception but leave the position at the end
  of stream.
}{ Created 20.11.2000 by P. Below
-----------------------------------------------------------------------
}
Function TReadOnlyMemoryStream.Seek( Offset: Longint; Origin: Word ):
Longint;
  Begin { Seek }
    Case Origin Of
      soFromBeginning: FPosition := Offset;
      soFromCurrent     : Inc( FPosition, Offset );
      soFromEnd      : FPosition := FSize + offset;
    Else
      Assert(False, 'TReadOnlyMemoryStream.Seek: invalid Origin!' );
    End; { Case }
    If FPosition < 0 Then
      FPosition := 0
    Else If FPosition > FSize Then
      FPosition := FSize;
    Result := FPosition;
  End; { TReadOnlyMemoryStream.Seek }

End { ROMemStream }.
*)