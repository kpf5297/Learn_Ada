with Ada.Real_Time; use Ada.Real_Time;
-- tasks.adb (Define the task behavior)
package body Tasks is
   task body Simple_Task is
      Period : constant Time_Span := Milliseconds(100);  -- 10Hz cycle
      Next_Time : Time := Clock;
   begin
      loop
         -- Your task logic here
         Next_Time := Next_Time + Period;
         delay until Next_Time;
      end loop;
   end Simple_Task;
end Tasks;
