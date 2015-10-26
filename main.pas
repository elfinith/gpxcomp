unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ComObj, Buttons, pngimage, ExtCtrls;

type

  TTrkPt = record
    Lat, Lon : String;
    Time : TDatetime;
  end;

  TRoute = array of TTrkPt;


  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit3: TEdit;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Label3: TLabel;
    Image1: TImage;
    procedure LoadGpxDoc(const FileName: string; var Route : TRoute);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    OriginalRoute, TargetRoute : TRoute;
    strOrgFilename, strTgtFilename : string;
    TimeShift : TDatetime;
    bIsLater : boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function getParseError(Error: Variant): string;
/// Вспомогательная функция. Формирует текст сообщения об ошибке
/// Вход : Error: TXMLDOMParseError
/// Выход : Отчет об ошибке
begin
  Result := 'Ошибка при загрузке документа ["' + Error.url + ' ]'#13#10
    + Error.reason + #13#10#13#10;
  if (Error.line > 0) then Result := Result + 'строка ' + Error.line
    + ', символ ' + error.linepos + #13#10 + Error.srcText;
end;

function getISOtoDateTime(strDT: string) : TDateTime;
var
  // Delphi settings save vars
  ShortDF, ShortTF : string;
  TS, DS : char;
  // conversion vars
  dd, tt, ddtt: TDateTime;
begin
  // save Delphi settings
  DS := DateSeparator;
  TS := TimeSeparator;
  ShortDF := ShortDateFormat;
  ShortTF := ShortTimeFormat;
  // set Delphi settings for string to date/time
  DateSeparator := '-';
  ShortDateFormat := 'yyyy-mm-dd';
  TimeSeparator := ':';
  ShortTimeFormat := 'hh:mm:ss';
  // convert test string to datetime
  try
    dd := StrToDate( Copy(strDT, 1, Pos('T',strDT)-1) );
    tt := StrToTime( Copy(strDT, Pos('T',strDT)+1, 8) );
    ddtt := trunc(dd) + frac(tt);
  except
    on EConvertError do
      ShowMessage('Error in converting : ' + strDT);
  end;
  // restore Delphi settings
  DateSeparator := DS;
  ShortDateFormat := ShortDF;
  TimeSeparator := TS;
  ShortTimeFormat := ShortTF;
  // return
  getISOtoDateTime := ddtt;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  SpeedButton3.Enabled := true;
end;

procedure TForm1.LoadGpxDoc(const FileName: string; var Route : TRoute);
/// Загружает файл в Microsoft.XMLDOM
var
  XML, Attributes, parseError: variant;
  Node, mainNode, childNodes: variant;
  i, l: integer;
  TreeNode, TN1: TTreeNode;

  procedure LoadItems(Node: variant);
  // рекурсивно обходит дерево элементов
  var
    i: integer;
    strTime : string;
  begin
    if Node.nodeName = 'trkpt' then begin
      strTime := '';
      for i := 0 to Node.childNodes.length - 1 do begin
        if Node.childNodes.item[i].nodeName = 'time'
        then strTime := Node.childNodes.item[i].childNodes.item[0].nodeValue;
      end;

      l := Length(Route) + 1;
      SetLength(Route, l);
      Route[l - 1].Lat := Node.Attributes.getNamedItem('lat').text;
      Route[l - 1].Lon := Node.Attributes.getNamedItem('lon').text;
      Route[l - 1].Time := getISOtoDateTime(strTime);
    end;
    // вот она сраная рекурсия
    for i := 0 to Node.childNodes.length - 1 do LoadItems(Node.childNodes.item[i]);
  end;

begin
  XML := CreateOleObject('Microsoft.XMLDOM');

  // async
  // Свойство, доступное для записи и чтения,
  // идентифицирующее текущий режим обработки (синхронный или асинхронный)
  XML.Async := false;

  // Загружает документ, адрес которого задан параметром url.
  // В случае успеха возвращает логическое значение true. Необходимо иметь в виду,
  // что вызов этого метода сразу же обнуляет содержимое текущего документа
  XML.load(FileName);

  // Можно загружать из строки:
  // loadXML(xmlString)
  // Загружает XML - фрагмент, определенный в передаваемой строке

  // parseError
  // Возвращает ссылку на объект XMLDOMParseError, при помощи которого
  // можно получить всю необходимую информацию о последней ошибке анализатора.
  // Только для чтения.
  if XML.parseError.errorCode <> 0 then begin
    ShowMessage(getParseError(XML.parseError));
  end
  else begin
    mainNode := XML.documentElement;
    { Загрузка DOM в TreeView }
    LoadItems(mainNode);
  end;
end;


procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  i : integer;
begin
  if OpenDialog1.Execute then begin
    ListBox1.Items.Clear;
    strOrgFilename := OpenDialog1.FileName;
    SetLength(OriginalRoute, 0);
    LoadGpxDoc(strOrgFilename, OriginalRoute);
    for i := 0 to length(OriginalRoute) - 1 do begin
      ListBox1.Items.Add(OriginalRoute[i].Lat + ' : ' + OriginalRoute[i].Lon + ' : '
        + FormatDateTime('mm/dd/yyyy hh:mm:ss', OriginalRoute[i].Time));
    end;
    Label1.Caption := IntToStr(length(OriginalRoute)) + ' точек';
  end;
end;


procedure TForm1.SpeedButton2Click(Sender: TObject);
var
  i : integer;
begin
  if OpenDialog1.Execute then begin
    ListBox2.Items.Clear;
    strTgtFilename := OpenDialog1.FileName;
    SetLength(TargetRoute, 0);
    LoadGpxDoc(strTgtFilename, TargetRoute);
    for i := 0 to length(TargetRoute) - 1 do begin
      ListBox2.Items.Add(TargetRoute[i].Lat + ' : ' + TargetRoute[i].Lon + ' : '
        + FormatDateTime('mm/dd/yyyy hh:mm:ss', TargetRoute[i].Time));
    end;
    Label2.Caption := IntToStr(length(TargetRoute)) + ' точек';
  end;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
var
  i, j : integer;
  str : string;
  rOrgLat, rOrgLon, rTgtLat, rTgtLon, rShift, rMinShift : real;
begin
  if (Length(OriginalRoute) > 0) and (Length(TargetRoute) > 0) then begin
    rMinShift := 0;
    str := StringReplace(OriginalRoute[ListBox1.ItemIndex].Lat, '.', DecimalSeparator, [rfReplaceAll]);
    rOrgLat := StrToFloat(str);
    str := StringReplace(OriginalRoute[ListBox1.ItemIndex].Lon, '.', DecimalSeparator, [rfReplaceAll]);
    rOrgLon := StrToFloat(str);
    for i := 0 to Length(TargetRoute) - 1 do begin
      str := StringReplace(TargetRoute[i].Lat, '.', DecimalSeparator, [rfReplaceAll]);
      rTgtLat := StrToFloat(str);
      str := StringReplace(TargetRoute[i].Lon, '.', DecimalSeparator, [rfReplaceAll]);
      rTgtLon := StrToFloat(str);
      rShift := abs(rOrgLat - rTgtLat) + abs(rOrgLon - rTgtLon);
      if (rMinShift = 0) or (rShift < rMinShift) then begin
        rMinShift := rShift;
        j := i;
      end;
    end;
    ListBox2.ItemIndex := j;
    TimeShift := TargetRoute[j].Time - OriginalRoute[ListBox1.ItemIndex].Time;
    bIsLater := TargetRoute[j].Time > OriginalRoute[ListBox1.ItemIndex].Time;
    if bIsLater then  Edit3.Text := '+' + FormatDateTime('hh:mm:ss', TimeShift)
    else Edit3.Text := '-' + FormatDateTime('hh:mm:ss', TimeShift);
    SpeedButton4.Show;
    Label3.Show;
    Edit3.Show;
  end;
end;


procedure TForm1.SpeedButton4Click(Sender: TObject);
var
  slBuffer, slResult : TStringList;
  strNewTime : string;
  i : integer;
begin
  try
    slResult := TStringList.Create;
    slBuffer := TStringList.Create;
    if SaveDialog1.Execute then begin
      slBuffer.LoadFromFile(strTgtFilename);
      for i := 0 to slBuffer.Count - 1 do begin
        if AnsiPos('<time>', slBuffer[i]) > 0 then begin
          strNewTime := FormatDateTime('hh:mm:ss',
            StrToTime(Copy(slBuffer[i], Pos('T', slBuffer[i]) + 1, 8)) - TimeShift);
          slResult.Add(StringReplace(slBuffer[i], Copy(slBuffer[i], Pos('T', slBuffer[i]) + 1, 8),
            strNewTime, [rfReplaceAll]));
        end
        else slResult.Add(slBuffer[i]);
      end;
    end;
    slResult.SaveToFile(SaveDialog1.FileName + '.gpx');
  finally
    slBuffer.Free;
    slResult.Free;
  end;
end;

end.
