with STM32.GPIO;
with STM32.Timers;
with STM32.Device;
with STM32.PWM;
with HAL; use HAL;
with STM32.Board;
with Drive_Motor;
with Steering_Motor;
with Ada.Text_IO;

procedure Main is
   -- Assigning timers for PWM output
   Drive_Timer    : STM32.Timers.Timer renames STM32.Device.Timer_3;
   Steering_Timer : STM32.Timers.Timer renames STM32.Device.Timer_4;

   -- PWM Modulators
   Drive_Modulator    : STM32.PWM.PWM_Modulator;
   Steering_Modulator : STM32.PWM.PWM_Modulator;

   -- Motor instances
   Drive    : Drive_Motor.Concrete_Drive_Motor;
   Steering : Steering_Motor.Concrete_Steering_Motor;

   -- PWM signal parameters
   Drive_Frequency    : STM32.PWM.Hertz := 1000;
   Steering_Frequency : STM32.PWM.Hertz := 50;
   
   Drive_Duty_Cycle    : STM32.PWM.Microseconds := 1500;
   Steering_Duty_Cycle : STM32.PWM.Microseconds := 1200;

begin
   -- Initialize STM32 board
   STM32.Board.Initialize_LEDs;
   STM32.Board.Configure_User_Button_GPIO;

   -- Turn on LED to indicate initialization
   STM32.Board.Turn_On(STM32.Board.Red_LED);
   delay 0.5;
   STM32.Board.Turn_Off(STM32.Board.Red_LED);

   -- Adjust frequency and duty cycle due to timer frequency doubling
   Drive_Frequency := Drive_Frequency * 2;
   Drive_Duty_Cycle := Drive_Duty_Cycle / 2;

   Steering_Frequency := Steering_Frequency * 2;
   Steering_Duty_Cycle := Steering_Duty_Cycle / 2;

   -- Configure PWM Timers
   STM32.PWM.Configure_PWM_Timer(Drive_Timer'Access, Drive_Frequency);
   STM32.PWM.Configure_PWM_Timer(Steering_Timer'Access, Steering_Frequency);

   -- Attach PWM channels to GPIO pins
   Drive_Modulator.Attach_PWM_Channel
     (Drive_Timer'Access,
      STM32.Timers.Channel_1,
      STM32.Device.PA6,  -- Adjust for actual drive motor pin
      STM32.Device.GPIO_AF_TIM3_1);

   Steering_Modulator.Attach_PWM_Channel
     (Steering_Timer'Access,
      STM32.Timers.Channel_1,
      STM32.Device.PA7,  -- Adjust for actual steering motor pin
      STM32.Device.GPIO_AF_TIM4_1);

   -- Set initial duty cycles
   Drive_Modulator.Set_Duty_Time(Drive_Duty_Cycle);
   Steering_Modulator.Set_Duty_Time(Steering_Duty_Cycle);

   -- Enable PWM output
   Drive_Modulator.Enable_Output;
   Steering_Modulator.Enable_Output;

   -- Debug output
   Ada.Text_IO.Put_Line("Motors Initialized!");

   loop
      -- Keep running indefinitely
      delay 0.1;
   end loop;

exception
   when others =>
      -- Error handling, optional blinking LED
      STM32.Board.Turn_On(STM32.Board.Red_LED);
end Main;
