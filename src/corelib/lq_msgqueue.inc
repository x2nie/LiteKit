{%mainunit lq_main.pas}

// LiteKit internal message queue implementation

type
  // a simlpe linked list implementation
  TMessageListElement = class(TObject)
  protected
    Next: TMessageListElement;
    Prev: TMessageListElement;
  public
    msg: TlqMessageRec;
  end;

var
  // to make things easier we have a few global variable that will get set
  UsedFirstMessage: TMessageListElement;
  UsedLastMessage: TMessageListElement;
  FreeFirstMessage: TMessageListElement;
  FreeLastMessage: TMessageListElement;

procedure MsgListInsertElement(Elem: TMessageListElement; var First: TMessageListElement; var Last: TMessageListElement);
begin
  Elem.Prev := nil;
  Elem.Next := nil;

  if First = nil then
  begin
    First := Elem;
    Last  := Elem;
  end
  else
  begin
    Last.Next := Elem;
    Elem.Prev := Last;
    Last      := Elem;
  end;
end;

procedure MsgListRemoveElement(Elem: TMessageListElement; var First: TMessageListElement; var Last: TMessageListElement);
begin
  if Elem = First then
    First := Elem.Next;

  if Elem = Last  then
    Last  := Elem.Prev;

  if Elem.Next <> nil then
    Elem.Next.Prev := Elem.Prev;

  if Elem.Prev <> nil then
    Elem.Prev.Next := Elem.Next;
end;

function lqAllocateMessage: PfpgMessageRec;
var
  e: TMessageListElement;
begin
  e := FreeFirstMessage;
  if e <> nil then
  begin
    MsgListRemoveElement(e, FreeFirstMessage, FreeLastMessage);
    MsgListInsertElement(e, UsedFirstMessage, UsedLastMessage);
    result := @(e.msg);
  end
  else
    result := nil;
end;

procedure lqDeleteFirstMessage;
var
  e: TMessageListElement;
begin
  lqApplication.Lock;
  try
    e := UsedFirstMessage;
    if e <> nil then
    begin
      MsgListRemoveElement(e, UsedFirstMessage, UsedLastMessage);
      MsgListInsertElement(e, FreeFirstMessage, FreeLastMessage);
    end;
  finally
    lqApplication.Unlock;
  end;
end;

function lqGetFirstMessage: PfpgMessageRec;
begin
  lqApplication.Lock;
  try
    if UsedFirstMessage <> nil then
      Result := @(UsedFirstMessage.msg)
    else
      Result := nil;
  finally
    lqApplication.Unlock;
  end;
end;

procedure lqInitMsgQueue;
var
  n: integer;
  e: TMessageListElement;
begin
  UsedFirstMessage := nil;
  UsedLastMessage  := nil;
  FreeFirstMessage := nil;
  FreeLastMessage  := nil;
  
  uMsgQueueList := TList.Create;

  for n := 1 to cMessageQueueSize do
  begin
    e := TMessageListElement.Create;
    uMsgQueueList.Add(e); // so we can free it off later
    MsgListInsertElement(e,FreeFirstMessage,FreeLastMessage);
  end;
end;

procedure lqPostMessage(Sender, Dest: TObject; MsgCode: integer; var aparams: TlqMessageParams); overload;
var
  p: PfpgMessageRec;
begin
  if Dest = nil then
    Exit; //==>
  lqApplication.Lock;
  try
    p := lqAllocateMessage;
    if p <> nil then
    begin
      p^.MsgCode := MsgCode;
      p^.Sender  := Sender;
      p^.Dest    := Dest;
      p^.Params  := aparams;
    end
    else
      raise Exception.Create('THE LiteKit MESSAGE QUEUE IS FULL.');
  finally
    lqApplication.Unlock;
  end;
end;

procedure lqPostMessage(Sender, Dest: TObject; MsgCode: integer); overload;
var
  p: PfpgMessageRec;
begin
  if Dest = nil then
    Exit; //==>

  lqApplication.Lock;
  try
    p := lqAllocateMessage;
    if p <> nil then
    begin
      p^.MsgCode := MsgCode;
      p^.Sender  := Sender;
      p^.Dest    := Dest;
      p^.Stop    := False;
    end
    else
      raise Exception.Create('THE LiteKit MESSAGE QUEUE IS FULL.');
  finally
    lqApplication.Unlock;
  end;
end;

procedure lqSendMessage(Sender, Dest: TObject; MsgCode: integer; var aparams: TlqMessageParams); overload;
var
  m: TlqMessageRec;
begin
  if Dest = nil then
    Exit; //==>

  m.MsgCode := MsgCode;
  m.Sender  := Sender;
  m.Dest    := Dest;
  m.Params  := aparams;

  lqDeliverMessage(m);
end;

procedure lqSendMessage(Sender, Dest: TObject; MsgCode: integer); overload;
var
  m: TlqMessageRec;
begin
  if Dest = nil then
    Exit; //==>

  m.MsgCode := MsgCode;
  m.Sender  := Sender;
  m.Dest    := Dest;

  lqDeliverMessage(m);
end;

procedure lqDeliverMessage(var msg: TlqMessageRec);
var
  oItem: TMsgHookItem;
  i: integer;
begin
  if msg.MsgCode = LQM_KILLME then
    msg.Dest.Free
  else
  begin
    msg.Dest.Dispatch(msg);
    if lqApplication.FMessageHookList.Count > 0 then
    begin
      for i := 0 to lqApplication.FMessageHookList.Count - 1 do
      begin
        oItem := TMsgHookItem(lqApplication.FMessageHookList.Items[i]);
        if (msg.Dest = oItem.Dest) and (msg.MsgCode = oItem.MsgCode) then
          oItem.Listener.Dispatch(msg);
      end;
    end;
  end;
end;

procedure lqDeliverMessages;
var
  mp: PfpgMessageRec;
  m: TlqMessageRec;
begin
  repeat
    mp := lqGetFirstMessage;
    if mp <> nil then
    begin
      m := mp^;
      lqDeleteFirstMessage;
      lqDeliverMessage(m);
    end;
  until mp = nil;
end;

