////////////////////////////////////////////////////////////////////////////////
// File:       PegtopWaveFileDialogs.pas
// Classes:    TPegtopWaveOpenDialog, TPegtopWaveSaveDialog
// Version:    1.00
// Date:       13 Mar 2005
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopWaveOpenDialog and TPegtopWaveSaveDialog are file dialogs for
// selecting wave files, having a play button.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopWaveFileDialogs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs,
  StdCtrls, ExtCtrls, PegtopFileDialogs, MMSystem;

type
  TPegtopWaveOpenDialog = class(TPegtopCustomExtendedFileDialog)
  private
    FInfoLabel: TLabel;
    FPlayButton: TButton;
    procedure PlayButtonClick(Sender: TObject);
    function IsFilterStored: Boolean;
  protected
    procedure DoShow; override;
    procedure DoClose; override;
    procedure DoAlign; override;
    procedure DoSelectionChange; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Filter stored IsFilterStored;
  end;

  TPegtopWaveSaveDialog = class(TPegtopWaveOpenDialog)
  protected
    function IsSaveDialog: Boolean; override;
  published
    property AutoAdjustExtension;
  end;

implementation

type
  TPegtopChunkId = packed array[0..3] of AnsiChar;

type
  TWaveFileHeader = packed record
    FileID: TPegtopChunkId;
    FileLen: Longword;
    WaveID: TPegtopChunkId;
    FormatID: TPegtopChunkId;
    FormatLen: Longword;
    Format: TWaveFormatEx;
  end;

function UpperChunkId(const Id: TPegtopChunkId): TPegtopChunkId;
var
  I: Integer;
begin
  Result := Id;
  for I := 0 to 3 do begin
    if (Result[I] >= 'a') and (Result[I] <= 'z') then
      Result[I] := Chr(Ord(Result[I]) xor 32);
  end;
end;

function GetWaveFormatName(FormatTag: Word): String;
begin
  case FormatTag of
    $0001: Result := 'PCM';
    $0002: Result := 'ADPCM';
    $0003: Result := 'IEEE float';
    $0004: Result := 'VSELP (Compaq)';
    $0005: Result := 'IBM CVSD';
    $0006: Result := 'A-law';
    $0007: Result := 'Mu-law';
    $0008: Result := 'DTS';
    $0009: Result := 'DRM (digital rights management)';
    $0010: Result := 'OKI ADPCM';
    $0011: Result := 'DVI ADPCM (Intel)';
    $0012: Result := 'Mediaspace ADPCM (Videologic)';
    $0013: Result := 'Sierra ADPCM';
    $0014: Result := 'G723 ADPCM';
    $0015: Result := 'DIGISTD (DSP Solutions)';
    $0016: Result := 'DIGIFIX (DSP Solutions)';
    $0017: Result := 'Dialogic OKI ADPCM';
    $0018: Result := 'Media Vision ADPCM';
    $0019: Result := 'CU Codec (Hewlett-Packard)';
    $0020: Result := 'Yamaha ADPCM';
    $0021: Result := 'SONARC Speech Compression';
    $0022: Result := 'DSP Group Truespeech';
    $0023: Result := 'Echo Speech Corporation';
    $0024: Result := 'Audiofile AF36 (Virtual Music)';
    $0025: Result := 'APTX (Audio Processing Technology)';
    $0026: Result := 'Audiofile AF10 (Virtual Music)';
    $0027: Result := 'PROSODY 1612 (Aculab)';
    $0028: Result := 'LRC (Merging Technologies)';
    $0030: Result := 'Dolby AC2';
    $0031: Result := 'GSM 610';
    $0032: Result := 'MSN Audio';
    $0033: Result := 'Antex ADPCME';
    $0034: Result := 'VQLPC (Control Resources)';
    $0035: Result := 'DIGIREAL (DSP Solutions)';
    $0036: Result := 'DIGIADPCM (DSP Solutions)';
    $0037: Result := 'CR10 (Control Resources)';
    $0038: Result := 'NMS VBXADPCM (Natural MicroSystems)';
    $0039: Result := 'CS IMA ADPCM (Crystal Semiconductor)';
    $0050: Result := 'MPEG';
    $0052: Result := 'RT24 (InSoft)';
    $0053: Result := 'PAC (InSoft)';
    $0055: Result := 'MPEG layer 3';
    $FFFE: Result := 'extensible';
    $FFFF: Result := 'unofficial (development)';
    else result := 'unknown';
  end;
end;

{#define  WAVE_FORMAT_ECHOSC3                    0x003A /* Echo Speech Corporation */
#define  WAVE_FORMAT_ROCKWELL_ADPCM             0x003B /* Rockwell International */
#define  WAVE_FORMAT_ROCKWELL_DIGITALK          0x003C /* Rockwell International */
#define  WAVE_FORMAT_XEBEC                      0x003D /* Xebec Multimedia Solutions Limited */
#define  WAVE_FORMAT_G721_ADPCM                 0x0040 /* Antex Electronics Corporation */
#define  WAVE_FORMAT_G728_CELP                  0x0041 /* Antex Electronics Corporation */
#define  WAVE_FORMAT_MSG723                     0x0042 /* Microsoft Corporation */
#define  WAVE_FORMAT_MPEG                       0x0050 /* Microsoft Corporation */
#define  WAVE_FORMAT_RT24                       0x0052 /* InSoft, Inc. */
#define  WAVE_FORMAT_PAC                        0x0053 /* InSoft, Inc. */
#define  WAVE_FORMAT_MPEGLAYER3                 0x0055 /* ISO/MPEG Layer3 Format Tag */
#define  WAVE_FORMAT_LUCENT_G723                0x0059 /* Lucent Technologies */
#define  WAVE_FORMAT_CIRRUS                     0x0060 /* Cirrus Logic */
#define  WAVE_FORMAT_ESPCM                      0x0061 /* ESS Technology */
#define  WAVE_FORMAT_VOXWARE                    0x0062 /* Voxware Inc */
#define  WAVE_FORMAT_CANOPUS_ATRAC              0x0063 /* Canopus, co., Ltd. */
#define  WAVE_FORMAT_G726_ADPCM                 0x0064 /* APICOM */
#define  WAVE_FORMAT_G722_ADPCM                 0x0065 /* APICOM */
#define  WAVE_FORMAT_DSAT_DISPLAY               0x0067 /* Microsoft Corporation */
#define  WAVE_FORMAT_VOXWARE_BYTE_ALIGNED       0x0069 /* Voxware Inc */
#define  WAVE_FORMAT_VOXWARE_AC8                0x0070 /* Voxware Inc */
#define  WAVE_FORMAT_VOXWARE_AC10               0x0071 /* Voxware Inc */
#define  WAVE_FORMAT_VOXWARE_AC16               0x0072 /* Voxware Inc */
#define  WAVE_FORMAT_VOXWARE_AC20               0x0073 /* Voxware Inc */
#define  WAVE_FORMAT_VOXWARE_RT24               0x0074 /* Voxware Inc */
#define  WAVE_FORMAT_VOXWARE_RT29               0x0075 /* Voxware Inc */
#define  WAVE_FORMAT_VOXWARE_RT29HW             0x0076 /* Voxware Inc */
#define  WAVE_FORMAT_VOXWARE_VR12               0x0077 /* Voxware Inc */
#define  WAVE_FORMAT_VOXWARE_VR18               0x0078 /* Voxware Inc */
#define  WAVE_FORMAT_VOXWARE_TQ40               0x0079 /* Voxware Inc */
#define  WAVE_FORMAT_SOFTSOUND                  0x0080 /* Softsound, Ltd. */
#define  WAVE_FORMAT_VOXWARE_TQ60               0x0081 /* Voxware Inc */
#define  WAVE_FORMAT_MSRT24                     0x0082 /* Microsoft Corporation */
#define  WAVE_FORMAT_G729A                      0x0083 /* AT&T Labs, Inc. */
#define  WAVE_FORMAT_MVI_MVI2                   0x0084 /* Motion Pixels */
#define  WAVE_FORMAT_DF_G726                    0x0085 /* DataFusion Systems (Pty) (Ltd) */
#define  WAVE_FORMAT_DF_GSM610                  0x0086 /* DataFusion Systems (Pty) (Ltd) */
#define  WAVE_FORMAT_ISIAUDIO                   0x0088 /* Iterated Systems, Inc. */
#define  WAVE_FORMAT_ONLIVE                     0x0089 /* OnLive! Technologies, Inc. */
#define  WAVE_FORMAT_SBC24                      0x0091 /* Siemens Business Communications Sys */
#define  WAVE_FORMAT_DOLBY_AC3_SPDIF            0x0092 /* Sonic Foundry */
#define  WAVE_FORMAT_MEDIASONIC_G723            0x0093 /* MediaSonic */
#define  WAVE_FORMAT_PROSODY_8KBPS              0x0094 /* Aculab plc */
#define  WAVE_FORMAT_ZYXEL_ADPCM                0x0097 /* ZyXEL Communications, Inc. */
#define  WAVE_FORMAT_PHILIPS_LPCBB              0x0098 /* Philips Speech Processing */
#define  WAVE_FORMAT_PACKED                     0x0099 /* Studer Professional Audio AG */
#define  WAVE_FORMAT_MALDEN_PHONYTALK           0x00A0 /* Malden Electronics Ltd. */
#define  WAVE_FORMAT_RHETOREX_ADPCM             0x0100 /* Rhetorex Inc. */
#define  WAVE_FORMAT_IRAT                       0x0101 /* BeCubed Software Inc. */
#define  WAVE_FORMAT_VIVO_G723                  0x0111 /* Vivo Software */
#define  WAVE_FORMAT_VIVO_SIREN                 0x0112 /* Vivo Software */
#define  WAVE_FORMAT_DIGITAL_G723               0x0123 /* Digital Equipment Corporation */
#define  WAVE_FORMAT_SANYO_LD_ADPCM             0x0125 /* Sanyo Electric Co., Ltd. */
#define  WAVE_FORMAT_SIPROLAB_ACEPLNET          0x0130 /* Sipro Lab Telecom Inc. */
#define  WAVE_FORMAT_SIPROLAB_ACELP4800         0x0131 /* Sipro Lab Telecom Inc. */
#define  WAVE_FORMAT_SIPROLAB_ACELP8V3          0x0132 /* Sipro Lab Telecom Inc. */
#define  WAVE_FORMAT_SIPROLAB_G729              0x0133 /* Sipro Lab Telecom Inc. */
#define  WAVE_FORMAT_SIPROLAB_G729A             0x0134 /* Sipro Lab Telecom Inc. */
#define  WAVE_FORMAT_SIPROLAB_KELVIN            0x0135 /* Sipro Lab Telecom Inc. */
#define  WAVE_FORMAT_G726ADPCM                  0x0140 /* Dictaphone Corporation */
#define  WAVE_FORMAT_QUALCOMM_PUREVOICE         0x0150 /* Qualcomm, Inc. */
#define  WAVE_FORMAT_QUALCOMM_HALFRATE          0x0151 /* Qualcomm, Inc. */
#define  WAVE_FORMAT_TUBGSM                     0x0155 /* Ring Zero Systems, Inc. */
#define  WAVE_FORMAT_MSAUDIO1                   0x0160 /* Microsoft Corporation */
#define  WAVE_FORMAT_CREATIVE_ADPCM             0x0200 /* Creative Labs, Inc */
#define  WAVE_FORMAT_CREATIVE_FASTSPEECH8       0x0202 /* Creative Labs, Inc */
#define  WAVE_FORMAT_CREATIVE_FASTSPEECH10      0x0203 /* Creative Labs, Inc */
#define  WAVE_FORMAT_UHER_ADPCM                 0x0210 /* UHER informatic GmbH */
#define  WAVE_FORMAT_QUARTERDECK                0x0220 /* Quarterdeck Corporation */
#define  WAVE_FORMAT_ILINK_VC                   0x0230 /* I-link Worldwide */
#define  WAVE_FORMAT_RAW_SPORT                  0x0240 /* Aureal Semiconductor */
#define  WAVE_FORMAT_IPI_HSX                    0x0250 /* Interactive Products, Inc. */
#define  WAVE_FORMAT_IPI_RPELP                  0x0251 /* Interactive Products, Inc. */
#define  WAVE_FORMAT_CS2                        0x0260 /* Consistent Software */
#define  WAVE_FORMAT_SONY_SCX                   0x0270 /* Sony Corp. */
#define  WAVE_FORMAT_FM_TOWNS_SND               0x0300 /* Fujitsu Corp. */
#define  WAVE_FORMAT_BTV_DIGITAL                0x0400 /* Brooktree Corporation */
#define  WAVE_FORMAT_QDESIGN_MUSIC              0x0450 /* QDesign Corporation */
#define  WAVE_FORMAT_VME_VMPCM                  0x0680 /* AT&T Labs, Inc. */
#define  WAVE_FORMAT_TPC                        0x0681 /* AT&T Labs, Inc. */
#define  WAVE_FORMAT_OLIGSM                     0x1000 /* Ing C. Olivetti & C., S.p.A. */
#define  WAVE_FORMAT_OLIADPCM                   0x1001 /* Ing C. Olivetti & C., S.p.A. */
#define  WAVE_FORMAT_OLICELP                    0x1002 /* Ing C. Olivetti & C., S.p.A. */
#define  WAVE_FORMAT_OLISBC                     0x1003 /* Ing C. Olivetti & C., S.p.A. */
#define  WAVE_FORMAT_OLIOPR                     0x1004 /* Ing C. Olivetti & C., S.p.A. */
#define  WAVE_FORMAT_LH_CODEC                   0x1100 /* Lernout & Hauspie */
#define  WAVE_FORMAT_NORRIS                     0x1400 /* Norris Communications, Inc. */
#define  WAVE_FORMAT_SOUNDSPACE_MUSICOMPRESS    0x1500 /* AT&T Labs, Inc. */
#define  WAVE_FORMAT_DVM                        0x2000 /* FAST Multimedia AG */ }

resourcestring
  PegtopWaveDialogPlay = 'Play';
  PegtopWaveFilterDef = 'Wave files (*.wav)|*.wav|All files (*.*)|*.*';

////////////////////////////////////////////////////////////////////////////////
// TPegtopGraphicOpenDialog
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopWaveOpenDialog.Create(AOwner: TComponent);
begin
  inherited;
  FPlayButton := TButton.Create(Self);
  FPlayButton.Enabled := False;
  FPlayButton.Parent := ExtendedContainer;
  FPlayButton.OnClick := PlayButtonClick;
  FInfoLabel := TLabel.Create(Self);
  FInfoLabel.AutoSize := False;
  FInfoLabel.Layout := tlCenter;
  FInfoLabel.Transparent := True;
  FInfoLabel.Parent := ExtendedContainer;
  ExtendedAlignment := peaBottom;
  ExtendedSize := 32;
  DefaultExt := 'wav';
  Filter := PegtopWaveFilterDef;
end;

procedure TPegtopWaveOpenDialog.DoShow;
begin
  FPlayButton.Caption := PegtopWaveDialogPlay;
  FPlayButton.Enabled := False;
  FInfoLabel.Caption := '';
  inherited;
end;

procedure TPegtopWaveOpenDialog.DoClose;
begin
  SndPlaySound(NIL, SND_ASYNC or SND_NODEFAULT);
  inherited;
end;

procedure TPegtopWaveOpenDialog.DoAlign;
var
  R: TRect;
begin
  FInfoLabel.SetBounds(0, 0, ExtendedContainer.Width - 85, ExtendedContainer.Height);
  R := ItemRects[pfiOkButton];
  FPlayButton.SetBounds(R.Left - ExtendedContainer.Left, 0, R.Right - R.Left, 23);
  inherited;
end;

procedure TPegtopWaveOpenDialog.DoSelectionChange;
var
  Valid: Boolean;
  Header: TWaveFileHeader;
  Stream: TStream;
  Seconds: Double;
  S: String;
begin
  Seconds := 0; // avoid compiler warning
  Valid := FileExists(FileName) and (GetFileAttributes(PChar(FileName)) <> $FFFFFFFF);
  if Valid then begin
    try
      Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        Valid := Stream.Read(Header, SizeOf(Header)) >= 36;
        if Valid then begin
          if (UpperChunkId(Header.FileID) <> 'RIFF')
          or (UpperChunkId(Header.WaveID) <> 'WAVE')
          or (UpperChunkId(Header.FormatID) <> 'FMT ')
          or (Header.FormatLen < 16) then Valid := False;
          if Valid then Seconds := (Stream.Size - 44) / Header.Format.nAvgBytesPerSec;
        end;
      finally
        Stream.Free;
      end;
    except
      Valid := False;
    end;
  end;
  if Valid then begin
    S := FormatDateTime('hh:nn:ss', Seconds * (1/86400))
    + ' (' + FloatToStrF(Header.Format.nSamplesPerSec * 0.001, ffNumber, 15, 1) + ' KHz '
    + IntToStr(Header.Format.wBitsPerSample) + ' bit ';
    if Header.Format.nChannels = 1 then
      S := S + 'mono'
    else if Header.Format.nChannels = 2 then
      S := S + 'stereo'
    else
      S := S + FInfoLabel.Caption + IntToStr(Header.Format.nChannels) + ' channels';
    S := S + ')' + #13#10 + GetWaveFormatName(Header.Format.wFormatTag);
    FInfoLabel.Caption := S;
  end
  else begin
    FInfoLabel.Caption := '';
  end;
  FPlayButton.Enabled := Valid;
  inherited;
end;

procedure TPegtopWaveOpenDialog.PlayButtonClick(Sender: TObject);
begin
  SndPlaySound(PChar(FileName), SND_ASYNC or SND_NODEFAULT);
end;

function TPegtopWaveOpenDialog.IsFilterStored: Boolean;
begin
  Result := Filter <> PegtopWaveFilterDef;
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopWaveSaveDialog
////////////////////////////////////////////////////////////////////////////////

function TPegtopWaveSaveDialog.IsSaveDialog: Boolean;
begin
  Result := True;
end;

end.
