with Ada.Text_IO; use Ada.Text_IO;

procedure Greet_5c is
   I : Integer := 1;
begin
   while I <= 5 loop
      Put_Line ("Hello World!"
                  & Integer'Image(I));

      I := I + 1;        
   end loop;
end Greet_5c;
