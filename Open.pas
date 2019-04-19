unit Open; // vim:sw=2:

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Index;

{ TOpenForm }

type
  TIndexEntry = Index.TIndexEntry;

  TOpenForm = class(TForm)
    ButtonPanel: TPanel;
      OpenButton: TButton;
      CancelButton: TButton;
    MRUHeading: TLabel;
    ScoreHeading: TLabel;
    MRUListBox: TListBox;
    ScoreListBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure MRUListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ScoreListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MRUListBoxEnter(Sender: TObject);
    procedure ScoreListBoxEnter(Sender: TObject);
    procedure MRUListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ScoreListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    selected: TIndexEntry;
    first, mruActive: Boolean;
  public
    { public declarations }
    function GetSelectedEntry: TIndexEntry;
    function GetMostRecent: TIndexEntry;
    procedure UpdateMostRecent( entry: TIndexEntry );
  end;

var
  OpenForm: TOpenForm;

implementation

{$R *.lfm}

uses LCLType;

{ TODO - Setlist (playlist) }

procedure TOpenForm.FormCreate(Sender: TObject);
begin Sender := Sender;
  selected := NIL;
  mruActive := FALSE;
  ScoreListBox.Items.Assign(Index.Build);
  first := TRUE
end;

procedure TOpenForm.FormShow(Sender: TObject);
var
  s: String;
  w, maxw: Integer;
begin Sender := Sender;
  (* Compute required width of main index. *)
  if first then begin
    ScoreListBox.Canvas.Font := ScoreListBox.Font;
    maxw := 0;
    for s in ScoreListBox.Items do begin
      w := ScoreListBox.Canvas.TextWidth(s);
      if w > maxw then maxw := w
    end;
    if maxw > 0 then
      ScoreListBox.ScrollWidth := maxw + ScoreListBox.Width
      				       - ScoreListBox.ClientWidth;
    first := FALSE
  end;

  (* Always start with most recently selected score. *)
  MRUListBox.Items.Assign(Index.GetMRU);
  if MRUListBox.Items.Count > 0 then begin
    MRUListBox.ItemIndex := 0;
    (* Compute required width of MRU list. *)
    MRUListBox.Canvas.Font := MRUListBox.Font;
    maxw := 0;
    for s in MRUListBox.Items do begin
      w := MRUListBox.Canvas.TextWidth(s);
      if w > maxw then maxw := w
    end;
    if maxw > 0 then
      MRUListBox.ScrollWidth := maxw + MRUListBox.Width
				     - MRUListBox.ClientWidth
  end;
  MRUListBox.SetFocus
end;

procedure TOpenForm.OpenButtonClick(Sender: TObject);
begin Sender := Sender;
  (* Also invoked when double-clicking or pressing Enter in the list boxes. *)
  if ScoreListBox.ItemIndex >= 0 then begin
    selected := TIndexEntry(ScoreListBox.Items.Objects[ScoreListBox.ItemIndex]);
    ModalResult := mrOk
  end
  else
    ModalResult := mrNone
end;

procedure TOpenForm.MRUListBoxKeyDown(Sender: TObject; var Key: Word;
				      Shift: TShiftState);
begin Shift := Shift;
  case Key of
  13:
    OpenButtonClick(Sender);
  VK_RIGHT:
    ScoreListBox.SetFocus;
  end
end;

procedure TOpenForm.ScoreListBoxKeyDown(Sender: TObject; var Key: Word;
					Shift: TShiftState);
begin Shift := Shift;
  case Key of
  13:
    OpenButtonClick(Sender);
  VK_LEFT:
    MRUListBox.SetFocus;
  end
end;

procedure TOpenForm.MRUListBoxEnter(Sender: TObject);
begin
  ScoreListBox.Color := Color;
  MRUListBox.Color := clDefault;
  MRUListBoxSelectionChange(Sender,FALSE);
  mruActive := TRUE
end;

procedure TOpenForm.ScoreListBoxEnter(Sender: TObject);
begin
  MRUListBox.Color := Color;
  ScoreListBox.Color := clDefault;
  ScoreListBoxSelectionChange(Sender,FALSE);
  mruActive := FALSE
end;

procedure TOpenForm.MRUListBoxSelectionChange(Sender: TObject; User: boolean);
var
  i: Integer;
  s: String;
begin Sender := Sender; User := User;
  (* Select the corresponding item in the overall index. *)
  if MRUListBox.ItemIndex >= 0 then begin
    s := MRUListBox.Items.Strings[MRUListBox.ItemIndex];
    i := ScoreListBox.Items.IndexOf(s);
    if i >= 0 then ScoreListBox.ItemIndex := i
  end
end;

procedure TOpenForm.ScoreListBoxSelectionChange(Sender: TObject; User: boolean);
var
  i: Integer;
  s: String;
begin Sender := Sender; User := User;
  (* Select the corresponding item in the MRU list if present.. *)
  if ScoreListBox.ItemIndex >= 0 then begin
    s := ScoreListBox.Items.Strings[ScoreListBox.ItemIndex];
    i := MRUListBox.Items.IndexOf(s);
    if i >= 0 then MRUListBox.ItemIndex := i
  end
end;

function TOpenForm.GetSelectedEntry: TIndexEntry;
begin
  GetSelectedEntry := selected
end;

function TOpenForm.GetMostRecent: TIndexEntry;
var
  mru: TStringList;
  i: Integer;
  s: String;
begin
  GetMostRecent := NIL;
  (* Fetch the up to date MRU list. We can't use MRUListBox.Items here because
     it is only updated each time the Open dialog is shown. *)
  mru := Index.GetMRU;
  if mru.Count > 0 then begin
    s := mru[0];
    i := ScoreListBox.Items.IndexOf(s);
    if i >= 0 then GetMostRecent := TIndexEntry(ScoreListBox.Items.Objects[i])
  end
end;

procedure TOpenForm.UpdateMostRecent( entry: TIndexEntry );
begin
  Index.UpdateMRU(entry)
end;

end.
