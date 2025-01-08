with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Check_Direction_2 is
   X : Integer;
begin
   loop
      Put("Enter and Interger value: ");
      Get(X);

      Put(X);

      case X is
         when 0 | 360 =>
            Put_Line (" is due North");
         when 1 .. 89 =>
            Put_Line (" is in the NorthEast quadrant");
         when 90 =>
            Put_Line (" is due East");
         when others =>
            Put_Line (" is not a valid direction");
            Put_Line ("Au revoir");
            exit;
      end case;
   end loop;
end Check_Direction_2;
