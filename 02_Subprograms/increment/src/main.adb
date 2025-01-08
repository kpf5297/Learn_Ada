with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Increment;

procedure Main is
   A, B : Integer;
begin
   A := 5;
   B := 0;

   Put("B is ");
   Put(B);
   Put_Line (".");

   while B < 100 loop
      B := Increment(B);
      
      Put("B is ");
      Put(B);
      Put_Line ("");
   end loop;

end Main;