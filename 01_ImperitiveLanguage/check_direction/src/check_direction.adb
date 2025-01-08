with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Check_Direction is
   N : Integer;
begin
   Put_Line("Enter an integer value for direction (0-360)");

   Get(N);

   Put(N);

   if N = 0 or N = 360 then
      Put_Line(" is due North.");
   elsif N in 1 .. 89 then
      Put_Line(" is in the NorthEast quadrant.");
   elsif N in 90 .. 179 then 
      Put_Line(" is in the SouthEast quadrant.");
   elsif N in 180 .. 269 then
      Put_Line(" is in the SouthWest quadrant.");
   elsif N in 270 .. 359 then
      Put_Line(" is in the NorthWest quadrant.");
   else  
      Put_Line(" is not in range.");
   end if;
   
end Check_Direction;
