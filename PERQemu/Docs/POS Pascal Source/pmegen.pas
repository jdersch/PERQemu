{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program pmegen;
type msg = string[63];
     rec = record
             n: integer;
             m: msg;
           end;
var r: rec;
    f: file of rec;
    t: text;
begin
 reset(t,'prqmic.err.text');
 rewrite(f,'prqmic.error');
 while not eof(t) do
  begin
   readln(t,r.n,r.m);
   writeln(r.n,' ',r.m);
   f^ := r;
   put(f)
  end;
 close(t);
 close(f)
end.
