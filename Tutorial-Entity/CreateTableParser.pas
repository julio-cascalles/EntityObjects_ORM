unit CreateTableParser;

interface

uses Classes, SysUtils, DB;

type
  TTableTokenID = (ttiNone, ttiTable,
                   ttiField,ttiFieldType,ttiSize,
                   ttiAttr,ttiForeign, ttiEND);
  TDatabaseStyle = (dsANSI, dsMySql,dsSqlServer);
  TFieldAttribs = set of (ffPrimaryKey,ffNotNull,ffUnique);


  TCreateTableParser = class
  private
    LastToken,Token: string;
    LastChar: char;
    Ptr, PToken: PChar;
    TableTokenID: TTableTokenID;
    comment, otherSyntax: Boolean;
    Position,
    CharCount: integer;
    f_Source: string;
    procedure Clear;
    procedure AddChar;
    procedure NewToken;
    function StrToFieldType(const S: string): TFieldType;
    procedure NextChar;
    procedure IgnoreParentesis;
    procedure setClassList;
  public
    Table,FieldName, classList, ForeignTable: string;
    FieldType: TFieldType;
    FieldAttribs: TFieldAttribs;
    Size: integer;
    DatabaseStyle: TDatabaseStyle;
    constructor Create(const FileName: string);
    destructor Destroy; override;
    function FindNextField: boolean;
  end;
  

implementation


procedure TCreateTableParser.AddChar;
begin
  if PToken = nil then
    PToken := Ptr;
  {}
  Inc(CharCount);
end;

procedure TCreateTableParser.Clear;
begin
    FieldType     := ftUnknown;
    Size          := 0;
    FieldName     := '';
    Token         := '';
    FieldAttribs  := [];
    otherSyntax   := False;
    ForeignTable  := '';
    classList     := '';
end;

constructor TCreateTableParser.Create(const FileName: string);
var
  List: TStringList;
begin
  if not FileExists(FileName) then
    raise Exception.Create('Invalid FileName');
  List := TStringList.Create;
  List.LoadFromFile(FileName);
  f_Source := List.Text;
  Ptr := Pchar(f_Source);
  List.Free;
  Position := 0;
  Clear;
  Table  := '';
  comment := False;
  TableTokenID := ttiNone;
  DatabaseStyle := dsANSI;
end;

destructor TCreateTableParser.Destroy;
begin
  {...}
  inherited;
end;

procedure TCreateTableParser.NextChar;
begin
    LastChar := Ptr^;
    Inc(Ptr);
    Inc(Position);
    if (Position >= Length(f_Source)) then begin
        TableTokenID := ttiEND;
    end;
end;

function ExtraiString(var Str: PChar; sep_ini,sep_fim: char; const defaultValue: string = '???'): string;
var
  pIni,pFim: PChar;
begin
  Result := defaultValue;
  pIni := StrScan(Str,sep_ini);
  if pIni = nil then Exit;
  {}
  Inc(pIni);
  pFim := StrScan(pIni,sep_fim);
  if pFim = nil then Exit;
  {}
  SetString(Result,pIni,pFim-pIni);
  Str := pFim;
end;

procedure TCreateTableParser.setClassList;
var
  newClass: string;
begin
  newClass := ExtraiString(Ptr,'[',']','');
  if newClass = '' then Exit;
  {---}
  if (classList <> '') then
    classList := classList + ',';
  classList := classList + newClass;
  {---}
end;

function TCreateTableParser.FindNextField: boolean;
begin
  Clear;
  PToken := nil;
  CharCount := 0;
  while TableTokenID <> ttiEND do begin
    if not comment
    and (LastChar+Ptr^ = '/*') then begin
      comment := True;
      NextChar;
    end;
    if comment then begin
      NextChar;
      if LastChar+Ptr^ = '*/' then
        comment := False
      else
        setClassList();
      Continue;
    end;
    case Ptr^ of
    '0'..'9','A'..'Z','a'..'z','_':
      AddChar;
    ' ',#9,#13:
      NewToken;
    ',':
      begin
          if (TableTokenID = ttiForeign) then
             TableTokenID := ttiField
          else if (FieldType = ftUnknown) then
             TableTokenID := ttiFieldType;
        NewToken;
        TableTokenID := ttiField;
        NextChar;
        if (FieldName = '') and (DatabaseStyle = dsMySql) then begin
            Continue;
        end;
        Break;
      end;
    '(':
      begin
        NewToken;
        if (not otherSyntax) and (TableTokenID = ttiAttr) then begin
          FieldType := StrToFieldType(Token);
          Token := '';
          TableTokenID := ttiSize
        end; // if..else
      end;
    ')':
        if (TableTokenID = ttiAttr) then begin
            TableTokenID := ttiNone;
            Break
        end Else if TableTokenID = ttiSize then begin
          NewToken;
          Size := StrToIntDef(Token,0);
          Token := '';
          TableTokenID := ttiAttr;
        end; // if..else
    end;  // case
    NextChar;
  end;  // while
  Result := (FieldName <> '');
end;


procedure TCreateTableParser.NewToken;

begin
  if CharCount = 0 then Exit;
  {*----------------------------
    Extrai uma palavra e a identifica com TableTokenID :
  -----------------------------}
  LastToken := Token;
  SetString(Token,pToken,CharCount);
  CharCount := 0;
  PToken := nil;
  case TableTokenID of
  ttiNone:
    if SameText(LastToken,'Create') then begin
      if SameText(Token,'Index') then begin
         // ignora INDEX ...
        TableTokenID := ttiNone;
        IgnoreParentesis();
      end
      else if SameText(Token,'Table') then
        TableTokenID := ttiTable;
    end;
  ttiTable:
    begin
      Table := Token;
      TableTokenID := ttiField;
    end;
  ttiField:
      begin
        if SameText(Token,'Foreign') then begin
            IgnoreParentesis();
            TableTokenID := ttiForeign;
            otherSyntax := True;
            DatabaseStyle := dsMySql;
        end
        else begin
          TableTokenID := ttiFieldType;
        end;
        FieldName := Token;
      end;
  ttiFieldType:
    begin
      FieldType := StrToFieldType(Token);
      TableTokenID := ttiAttr;
    end;
  ttiForeign:
    if not SameText(Token,'References') then begin
        ForeignTable := Token;
        if (DatabaseStyle = dsMySql) then begin
            FieldName := ExtraiString(Ptr,'(',')');
        end;
        TableTokenID := ttiAttr;
    end;
  ttiAttr:
    if SameText(Token,'Unique') then
      FieldAttribs := FieldAttribs + [ffUnique]
    else if SameText(LastToken,'Not')
    and SameText(Token,'Null') then
      FieldAttribs := FieldAttribs + [ffNotNull]
    else if SameText(Token,'Key') then begin
        if SameText(LastToken,'Primary') then begin
            FieldAttribs := FieldAttribs + [ffPrimaryKey]
        end Else if SameText('Foreign',LastToken) then begin
            TableTokenID := ttiForeign;
        end; // if..else
    end
    else if SameText(Token,'references') then begin
        TableTokenID := ttiForeign;
    end
    else if SameText(Token,'IDENTITY') then begin
        IgnoreParentesis();
        otherSyntax := True;
        DatabaseStyle := dsSqlServer;
    end;
  end;  // case
end;

procedure TCreateTableParser.IgnoreParentesis;
begin
    LastChar := #0;
    while (LastChar <> ')') do begin
          NextChar();
    end; // while
end;

function TCreateTableParser.StrToFieldType(const S: string): TFieldType;
begin
  if SameText(S,'Char') or SameText(S,'VarChar') then
    Result := ftString
  else if SameText(S,'Integer') or SameText(S,'Int') then
    Result := ftInteger
  else if SameText(S,'Float') then
    Result := ftFloat
  else if SameText(S,'Date') then
    Result := ftDate
  else if SameText(S,'Bit') then
    Result := ftBoolean
  else
    Result := ftUnknown;
end;


end.
