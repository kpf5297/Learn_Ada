-- tasks.ads (Declare the task type)
pragma Profile (Ravenscar);
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
pragma Locking_Policy (Ceiling_Locking);

package Tasks is
   task Simple_Task with Priority => 10; -- Library-level task declaration
end Tasks;
