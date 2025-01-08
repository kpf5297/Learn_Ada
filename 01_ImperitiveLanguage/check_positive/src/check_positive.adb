with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Check_Positive is
   N : Integer;
begin
   Put("Enter a number: ");

   Get(N);

   Put(N);

   if N > 0 then
      Put_Line(" is a postive number");
   else
      Put_Line(" is a negative number");
   end if;

end Check_Positive;
