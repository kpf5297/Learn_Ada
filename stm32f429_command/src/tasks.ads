-- tasks.ads (Declare the task type)
pragma Profile (Ravenscar);
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
pragma Locking_Policy (Ceiling_Locking);

with Ada.Real_Time; use Ada.Real_Time;
with STM32.Board; use STM32.Board;
package Tasks is
   task Simple_Task with Priority => 10; -- Library-level task declaration
   task Another_Task with Priority => 15; -- New task declaration
end Tasks;
